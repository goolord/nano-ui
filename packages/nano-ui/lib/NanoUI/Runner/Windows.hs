-- | A session's further windows ('NanoUI.nativeWindow'), for a backend that
-- runs its first window with 'NanoUI.Runner.runSessionLoop': opening and
-- closing them as views declare them, and running their frames after each
-- pass of the first window's loop. The backend supplies 'WindowOps', hands
-- 'loopWindows' to the loop ('NanoUI.Runner.sdWindows'), and routes each
-- further window's events to it ('openWindows').
--
-- A further window runs its view when it has input, when its last frame
-- asked for another, an animation or timed wake is due, a 'NanoUI.Var'
-- changed since its view last ran ('varChangeCount'), or the window that
-- declared it ran a frame, whose view the declaration's body may close over.
-- A session whose views declare none does none of this beyond finding the
-- list empty.
module NanoUI.Runner.Windows
  ( WindowOps (..)
  , Windows
  , newWindows
  , openWindows
  , hasWindows
  , loopWindows
  , afterMainPass
  , closeWindows
  ) where

import Control.Concurrent (myThreadId)
import Control.Monad (filterM, forM, forM_, unless, when)
import Data.Foldable (foldlM)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)
import NanoUI.Internal.ChildWindow (ChildWindow (..), childView, childWindows)
import NanoUI.Internal.Context
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Input
import NanoUI.Internal.Monad (NanoUI, unlessM)
import NanoUI.Internal.NativeWindow (quitRequested, requestWindowClose)
import NanoUI.Internal.NativeWindow.Types (WindowSettings (..))
import NanoUI.Internal.Var (readVarIO, writeVarIO)
import NanoUI.Internal.Var.Cell (varChangeCount)
import NanoUI.Runner (ClickTracker, LoopWindows (..), maxFrameDt, newClickTracker, shorterWait, shouldRedrawFrame, stampClicks, wakeTimeoutMs)

-- | How a backend opens, draws and closes a further window @w@ with events
-- @ev@.
data WindowOps w ev = WindowOps
  { woOpen :: Context -> WindowSettings -> IO (w, Context)
  -- ^ Open a window for a declaration in the view of the given context.
  -- Answer the window and the context its view runs on, with the backend's
  -- session and wake installed.
  , woClose :: w -> IO ()
  -- ^ Close it, cancelling its view's background jobs.
  , woApplyEvent :: Input -> ev -> Input
  , woIsButtonEdge :: ev -> Bool
  , woIsClose :: ev -> Bool
  -- ^ The desktop asked to close the window.
  , woSync :: w -> Context -> Input -> IO (Context, Input)
  -- ^ As 'NanoUI.Runner.sdSyncDisplay', for this window.
  , woDraw :: w -> Context -> NanoUI () -> Input -> Bool -> IO Bool
  -- ^ Draw a frame of the view, forcing a full repaint with the flag, and
  -- answer whether the context needs another.
  , woCursor :: w -> Context -> Input -> IO ()
  -- ^ Sync the cursor, after every pass of the window.
  , woPacingMs :: w -> Int
  -- ^ The wait between frames while the window animates.
  }

data Windows w ev = Windows
  { wOps :: !(WindowOps w ev)
  , wTop :: !(IORef [Child w ev])
  , wFirstSeen :: !(IORef Word64)
  -- ^ 'varChangeCount' when the first window's view last ran.
  }

-- | One further window.
data Child w ev = Child
  { chKey :: !WidgetId
  , chWin :: !w
  , chParent :: !Context
  -- ^ The declaring window's context, which a close asks for a frame.
  , chDecl :: !(IORef ChildWindow)
  -- ^ The latest declaration, whose view and 'NanoUI.Var' are current.
  , chClosed :: !(IORef Bool)
  -- ^ Closed, though a sweep that began before may still hold it.
  , chSiblings :: !(IORef [Child w ev])
  , chChildren :: !(IORef [Child w ev])
  , chQueue :: !(IORef [ev])
  -- ^ Events not yet taken, newest first.
  , chClicks :: !ClickTracker
  , chLoop :: !(IORef ChildLoop)
  }

-- | A further window's loop state between its passes.
data ChildLoop = ChildLoop
  { clCtx :: !Context
  , clInput :: !Input
  , clLastT :: !Double
  , clWasAnim :: !Bool
  , clDraw :: !Bool
  -- ^ Draw next pass: the last frame asked for another, or the declaring
  -- window drew.
  , clSettle :: !Int
  -- ^ Opening frames still to draw: the first lays out against no previous
  -- rectangles, as the first window's opening frames do.
  , clSeen :: !Word64
  -- ^ 'varChangeCount' when its view last ran.
  , clParentFrame :: !Int
  -- ^ The declaring window's frame number at the last sync.
  }

newWindows :: WindowOps w ev -> IO (Windows w ev)
newWindows ops = Windows ops <$> newIORef [] <*> (newIORef =<< varChangeCount)

allChildren :: IORef [Child w ev] -> IO [Child w ev]
allChildren ref = readIORef ref >>= fmap concat . mapM (\c -> (c :) <$> allChildren (chChildren c))

-- | Every open further window, each before those it opened, with an action
-- that queues an event for it.
openWindows :: Windows w ev -> IO [(w, ev -> IO ())]
openWindows w = map (\c -> (chWin c, \ev -> modifyIORef' (chQueue c) (ev :))) <$> allChildren (wTop w)

-- | Whether any further window is open.
hasWindows :: Windows w ev -> IO Bool
hasWindows w = not . null <$> readIORef (wTop w)

-- | The windows for 'NanoUI.Runner.sdWindows'. The first window runs its view
-- when a 'NanoUI.Var' changed since it last did, which matters only while
-- further windows are open.
loopWindows :: Windows w ev -> LoopWindows
loopWindows w =
  LoopWindows
    { lwWait = allChildren (wTop w) >>= foldlM (\acc c -> shorterWait acc <$> childWait w c) (-1)
    , lwFirstDue = do
        open <- hasWindows w
        if open then (/=) <$> varChangeCount <*> readIORef (wFirstSeen w) else pure False
    , lwDrew = writeIORef (wFirstSeen w) =<< varChangeCount
    , lwAfterPass = afterMainPass w
    }

childWait :: Windows w ev -> Child w ev -> IO Int
childWait w c = do
  cl <- readIORef (chLoop c)
  due <- childDue c cl
  if due
    then pure 0
    else do
      animating <- (clWasAnim cl ||) <$> anyAnimating (clCtx cl)
      if animating
        then pure (woPacingMs (wOps w) (chWin c))
        else wakeTimeoutMs <$> getWakeAt (clCtx cl) <*> getMonotonicTime

-- | Whether a window must draw next pass, whatever its input.
mustDraw :: ChildLoop -> IO Bool
mustDraw cl = (\gen -> clSettle cl > 0 || clDraw cl || clSeen cl /= gen) <$> varChangeCount

-- | Whether a window has something to do now, animations and timed wakes
-- aside.
childDue :: Child w ev -> ChildLoop -> IO Bool
childDue c cl = do
  queued <- not . null <$> readIORef (chQueue c)
  forced <- mustDraw cl
  woken <- readIORef (ctxWoken (clCtx cl))
  ctxDirty <- isDirty (clCtx cl)
  pure (queued || forced || woken || ctxDirty)

-- | After a pass of the first window's loop: open and close windows for what
-- its view declared and run the further windows' passes.
afterMainPass :: Windows w ev -> Context -> IO ()
afterMainPass w ctx = do
  syncChildren w ctx (wTop w)
  kids <- allChildren (wTop w)
  forM_ kids $ \c -> unlessM (readIORef (chClosed c)) (stepChild w c)

-- | Open a window for each new declaration of @parent@'s view and close
-- those it no longer declares, or declares closed. A parent that drew a
-- frame since the last sync makes each of its windows draw.
syncChildren :: Windows w ev -> Context -> IORef [Child w ev] -> IO ()
syncChildren w parent ref = do
  decls <- childWindows parent
  old <- readIORef ref
  unless (null decls && null old) $ do
    frame <- frameNumber parent
    open <- filterM (readVarIO . childOpen) decls
    new <- forM open $ \d -> case find ((== childKey d) . chKey) old of
      Just c -> do
        writeIORef (chDecl c) d
        modifyIORef' (chLoop c) $ \cl ->
          if clParentFrame cl == frame then cl else cl {clParentFrame = frame, clDraw = True}
        pure c
      Nothing -> openChild w parent ref frame d
    forM_ old $ \c -> unless (any ((== chKey c) . chKey) new) (closeChild w c)
    writeIORef ref new

openChild :: Windows w ev -> Context -> IORef [Child w ev] -> Int -> ChildWindow -> IO (Child w ev)
openChild w parent siblings frame decl = do
  (win, ctx) <- woOpen (wOps w) parent (childSettings decl)
  -- This thread runs its frames, so its own dirty marks need no wake.
  myThreadId >>= writeIORef (ctxLoopThread ctx) . Just
  now <- getMonotonicTime
  let inp = applyPointerLeave emptyInput {inputWindowSize = wsSize (childSettings decl)}
  Child (childKey decl) win parent
    <$> newIORef decl
    <*> newIORef False
    <*> pure siblings
    <*> newIORef []
    <*> newIORef []
    <*> newClickTracker
    <*> newIORef (ChildLoop ctx inp now False False 2 0 frame)

-- | Close a window and those it declared.
closeChild :: Windows w ev -> Child w ev -> IO ()
closeChild w c = unlessM (readIORef (chClosed c)) $ do
  writeIORef (chClosed c) True
  mapM_ (closeChild w) =<< readIORef (chChildren c)
  writeIORef (chChildren c) []
  woClose (wOps w) (chWin c)

-- | Close every further window, when the session ends.
closeWindows :: Windows w ev -> IO ()
closeWindows w = do
  mapM_ (closeChild w) =<< readIORef (wTop w)
  writeIORef (wTop w) []

-- | The window asked to close (its close button, or 'NanoUI.quitUi' in its
-- view): write 'False' to its 'NanoUI.Var', close it now, and ask the
-- declaring window for the frame that shows it closed, which nothing else
-- would ask for once no further window is open. A read-only 'NanoUI.Var'
-- keeps it open.
closeRequested :: Windows w ev -> Child w ev -> IO ()
closeRequested w c = do
  open <- childOpen <$> readIORef (chDecl c)
  writeVarIO open False
  unlessM (readVarIO open) $ do
    closeChild w c
    modifyIORef' (chSiblings c) (filter ((/= chKey c) . chKey))
    markDirty (chParent c)

-- | One pass of a further window's loop: take its events into a frame's
-- input, as the first window's loop does, and draw when anything calls for
-- it. Nothing to do costs a few reads.
stepChild :: Windows w ev -> Child w ev -> IO ()
stepChild w c = do
  cl <- readIORef (chLoop c)
  let ctx = clCtx cl
  due <- childDue c cl
  animating <- anyAnimating ctx
  wakeAt <- getWakeAt ctx
  now <- getMonotonicTime
  let wakeDue = wakeAt > 0 && now >= wakeAt
  when wakeDue (clearWakeAt ctx)
  when (due || clWasAnim cl || animating || wakeDue) $ do
    takeWakes ctx
    evs <- reverse <$> readIORef (chQueue c)
    let dt = min maxFrameDt (realToFrac (now - clLastT cl))
        (frameInp, group, rest) = takeFrame (woApplyEvent ops) (woIsButtonEdge ops) (clearEphemeral (clInput cl) {inputDeltaTime = dt}) evs
    writeIORef (chQueue c) (reverse rest)
    closing <- if any (woIsClose ops) group then requestWindowClose ctx else pure False
    if closing
      then closeRequested w c
      else do
        stamped <- stampClicks (chClicks c) frameInp
        (ctx', synced) <- woSync ops win ctx stamped
        forced <- mustDraw cl
        draw <- if forced then pure True else shouldRedrawFrame ctx' (clInput cl) synced (clWasAnim cl) False wakeDue
        animNow <- anyAnimating ctx'
        cl' <-
          if draw
            then do
              view <- childView <$> readIORef (chDecl c)
              more <- woDraw ops win ctx' view synced (clSettle cl > 0 || (clWasAnim cl && not animNow))
              seen <- varChangeCount
              pure cl {clDraw = more, clSettle = max 0 (clSettle cl - 1), clSeen = seen}
            else pure cl
        writeIORef (chLoop c) cl' {clCtx = ctx', clInput = synced, clLastT = now, clWasAnim = animNow}
        woCursor ops win ctx' synced
        quit <- quitRequested ctx'
        if quit then closeRequested w c else syncChildren w ctx' (chChildren c)
  where
    ops = wOps w
    win = chWin c
