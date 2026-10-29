-- | A session's further windows ('NanoUI.nativeWindow'), for a backend that
-- runs its first window with 'NanoUI.Runner.runSessionLoop': opening and
-- closing them as views declare them, and running their frames after each
-- pass of the first window's loop. The backend supplies 'WindowOps' and
-- calls 'afterMainPass' after every pass (from 'NanoUI.Runner.sdOnCursor'),
-- folds 'windowsWait' into its event wait, and runs the first window's view
-- when 'mainNeedsFrame' says so, noting each run with 'noteMainFrame'.
--
-- A further window runs its view when it has input, when its last frame
-- asked for another, an animation or timed wake is due, a 'NanoUI.Var'
-- changed since its view last began ('varChangeCount'), or the window that
-- declared it ran a frame, whose view the declaration's body may close over.
-- A session whose views declare none does none of this beyond finding the
-- list empty.
module NanoUI.Runner.Windows
  ( WindowOps (..)
  , Windows
  , newWindows
  , openWindows
  , windowsWait
  , mainNeedsFrame
  , noteMainFrame
  , afterMainPass
  , closeWindows
  ) where

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
import NanoUI.Internal.Monad (NanoUI, unlessM, whenM)
import NanoUI.Internal.NativeWindow (quitRequested, requestWindowClose)
import NanoUI.Internal.NativeWindow.Types (WindowSettings (..))
import NanoUI.Internal.Types (V2 (..))
import NanoUI.Internal.Var (readVarIO, writeVarIO)
import NanoUI.Internal.Var.Cell (varChangeCount)
import NanoUI.Runner (shouldRedrawFrame, stampClicks)

-- | How a backend opens, feeds, draws and closes a further window @w@ with
-- events @ev@.
data WindowOps w ev = WindowOps
  { woOpen :: Maybe w -> Context -> WindowSettings -> IO (w, Context)
  -- ^ Open a window for a declaration in the view of @Maybe w@ ('Nothing'
  -- for the first window), whose context is given. Answer the window and the
  -- context its view runs on, with the backend's session and wake installed.
  , woClose :: w -> IO ()
  -- ^ Close it, cancelling its view's background jobs.
  , woPoll :: w -> IO [ev]
  -- ^ Events the backend has for the window since the last call, beyond
  -- those it queued with 'openWindows'' action.
  , woApplyEvent :: Input -> ev -> Input
  , woIsButtonEdge :: ev -> Bool
  , woIsClose :: ev -> Bool
  -- ^ The desktop asked to close the window.
  , woSync :: w -> Context -> Input -> IO (Context, Input)
  -- ^ As 'NanoUI.Runner.sdSyncDisplay', for this window.
  , woDraw :: w -> Context -> NanoUI () -> Input -> Bool -> IO Bool
  -- ^ Draw a frame of the view, forcing a full repaint with the flag, and
  -- answer whether the context needs another.
  , woPointerIn :: w -> IO Bool
  , woCursor :: w -> Context -> Input -> IO ()
  , woPacingMs :: w -> Int
  -- ^ The wait between frames while the window animates.
  }

data Windows w ev = Windows
  { wOps :: !(WindowOps w ev)
  , wTop :: !(IORef [Child w ev])
  , wFirstSeen :: !(IORef Word64)
  -- ^ 'varChangeCount' when the first window's view last began.
  }

-- | One further window and its own loop state.
data Child w ev = Child
  { chKey :: !WidgetId
  , chWin :: !w
  , chDecl :: !(IORef ChildWindow)
  -- ^ The latest declaration, whose view and 'NanoUI.Var' are current.
  , chParent :: !Context
  -- ^ The declaring window's context, which a close asks for a frame.
  , chClosed :: !(IORef Bool)
  -- ^ Closed, though a sweep that began before may still hold it.
  , chSiblings :: !(IORef [Child w ev])
  , chChildren :: !(IORef [Child w ev])
  , chCtx :: !(IORef Context)
  , chInput :: !(IORef Input)
  , chQueue :: !(IORef [ev])
  -- ^ Events not yet taken, newest first.
  , chLastT :: !(IORef Double)
  , chWasAnim :: !(IORef Bool)
  , chDirty :: !(IORef Bool)
  -- ^ The last frame asked for another, or the declaring window drew.
  , chSettle :: !(IORef Int)
  -- ^ Opening frames still to draw: the first lays out against no previous
  -- rectangles, as the first window's opening frames do.
  , chSeen :: !(IORef Word64)
  , chParentFrame :: !(IORef Int)
  -- ^ The declaring window's frame number when this one last drew.
  , chClicks :: !(IORef (Double, V2, MouseButtons, Int))
  }

newWindows :: WindowOps w ev -> IO (Windows w ev)
newWindows ops = Windows ops <$> newIORef [] <*> (newIORef =<< varChangeCount)

allChildren :: IORef [Child w ev] -> IO [Child w ev]
allChildren ref = readIORef ref >>= fmap concat . mapM (\c -> (c :) <$> allChildren (chChildren c))

-- | Every open further window, each before those it opened, with an action
-- that queues an event for it.
openWindows :: Windows w ev -> IO [(w, ev -> IO ())]
openWindows w = map (\c -> (chWin c, \ev -> modifyIORef' (chQueue c) (ev :))) <$> allChildren (wTop w)

-- | The longest wait, in milliseconds, the further windows allow before the
-- loop runs again; -1 for no limit, as with none open.
windowsWait :: Windows w ev -> IO Int
windowsWait w = do
  kids <- allChildren (wTop w)
  if null kids
    then pure (-1)
    else do
      firstDue <- mainNeedsFrame w
      if firstDue then pure 0 else foldlM (\acc c -> shorter acc <$> childWait w c) (-1) kids
  where
    shorter a b
      | a < 0 = b
      | b < 0 = a
      | otherwise = min a b

childWait :: Windows w ev -> Child w ev -> IO Int
childWait w c = do
  due <- childDue c
  if due
    then pure 0
    else do
      ctx <- readIORef (chCtx c)
      animating <- (||) <$> readIORef (chWasAnim c) <*> anyAnimating ctx
      if animating
        then pure (woPacingMs (wOps w) (chWin c))
        else do
          wakeAt <- getWakeAt ctx
          if wakeAt <= 0
            then pure (-1)
            else (\now -> max 0 (ceiling (min 3600 (wakeAt - now) * 1000) + 2)) <$> getMonotonicTime

-- | Whether a window has something to draw now, animations and timed wakes
-- aside.
childDue :: Child w ev -> IO Bool
childDue c = do
  ctx <- readIORef (chCtx c)
  queued <- not . null <$> readIORef (chQueue c)
  settle <- readIORef (chSettle c)
  dirty <- readIORef (chDirty c)
  seen <- readIORef (chSeen c)
  gen <- varChangeCount
  woken <- readIORef (ctxWoken ctx)
  ctxDirty <- isDirty ctx
  pure (queued || settle > 0 || dirty || seen /= gen || woken || ctxDirty)

-- | Whether the first window should run its view because a 'NanoUI.Var'
-- changed since it last did, which matters only while further windows are
-- open.
mainNeedsFrame :: Windows w ev -> IO Bool
mainNeedsFrame w = do
  kids <- readIORef (wTop w)
  if null kids then pure False else (/=) <$> varChangeCount <*> readIORef (wFirstSeen w)

-- | Note that the first window's view is about to run.
noteMainFrame :: Windows w ev -> IO ()
noteMainFrame w = writeIORef (wFirstSeen w) =<< varChangeCount

-- | After a pass of the first window's loop: open and close windows for what
-- its view declared and run the further windows' frames. Answers whether
-- any are open, when the backend syncs the cursor only from the window the
-- pointer is over.
afterMainPass :: Windows w ev -> Context -> IO Bool
afterMainPass w ctx = do
  syncChildren w Nothing ctx (wTop w)
  kids <- allChildren (wTop w)
  forM_ kids $ \c -> unlessM (readIORef (chClosed c)) (stepChild w c)
  pure (not (null kids))

-- | Open a window for each new declaration of @parent@'s view and close
-- those it no longer declares, or declares closed. A parent that drew a
-- frame since a window last did makes it due.
syncChildren :: Windows w ev -> Maybe w -> Context -> IORef [Child w ev] -> IO ()
syncChildren w parentWin parent ref = do
  decls <- childWindows parent
  old <- readIORef ref
  unless (null decls && null old) $ do
    frame <- frameNumber parent
    open <- filterM (readVarIO . childOpen) decls
    new <- forM open $ \d -> case find ((== childKey d) . chKey) old of
      Just c -> do
        writeIORef (chDecl c) d
        drewSince <- (/= frame) <$> readIORef (chParentFrame c)
        when drewSince $ writeIORef (chParentFrame c) frame >> writeIORef (chDirty c) True
        pure c
      Nothing -> openChild w parentWin parent ref frame d
    forM_ old $ \c -> unless (any ((== chKey c) . chKey) new) (closeChild w c)
    writeIORef ref new

openChild :: Windows w ev -> Maybe w -> Context -> IORef [Child w ev] -> Int -> ChildWindow -> IO (Child w ev)
openChild w parentWin parent siblings frame decl = do
  (win, ctx) <- woOpen (wOps w) parentWin parent (childSettings decl)
  now <- getMonotonicTime
  Child (childKey decl) win
    <$> newIORef decl
    <*> pure parent
    <*> newIORef False
    <*> pure siblings
    <*> newIORef []
    <*> newIORef ctx
    <*> newIORef (applyPointerLeave emptyInput {inputWindowSize = wsSize (childSettings decl)})
    <*> newIORef []
    <*> newIORef now
    <*> newIORef False
    <*> newIORef False
    <*> newIORef 2
    <*> newIORef 0
    <*> newIORef frame
    <*> newIORef (0, V2 (-999) (-999), noButtons, 0)

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
  polled <- woPoll ops win
  unless (null polled) $ modifyIORef' (chQueue c) (reverse polled ++)
  ctx <- readIORef (chCtx c)
  due <- childDue c
  wasAnim <- readIORef (chWasAnim c)
  animating <- anyAnimating ctx
  wakeAt <- getWakeAt ctx
  now <- getMonotonicTime
  let wakeDue = wakeAt > 0 && now >= wakeAt
  when wakeDue (clearWakeAt ctx)
  when (due || wasAnim || animating || wakeDue) $ do
    takeWakes ctx
    evs <- reverse <$> readIORef (chQueue c)
    inp <- readIORef (chInput c)
    lastT <- readIORef (chLastT c)
    let dt = min 0.05 (realToFrac (now - lastT))
        (frameInp, group, rest) = takeFrame (woApplyEvent ops) (woIsButtonEdge ops) (clearEphemeral inp {inputDeltaTime = dt}) evs
    writeIORef (chQueue c) (reverse rest)
    writeIORef (chLastT c) now
    closing <- if any (woIsClose ops) group then requestWindowClose ctx else pure False
    if closing
      then closeRequested w c
      else do
        stamped <- stampClicks (chClicks c) frameInp
        (ctx', synced) <- woSync ops win ctx stamped
        writeIORef (chCtx c) ctx'
        settle <- readIORef (chSettle c)
        dirty <- readIORef (chDirty c)
        seen <- readIORef (chSeen c)
        gen <- varChangeCount
        draw <-
          if settle > 0 || dirty || seen /= gen
            then pure True
            else shouldRedrawFrame ctx' inp synced wasAnim False wakeDue
        animNow <- anyAnimating ctx'
        when draw $ do
          writeIORef (chSeen c) gen
          view <- childView <$> readIORef (chDecl c)
          writeIORef (chDirty c) =<< woDraw ops win ctx' view synced (settle > 0 || (wasAnim && not animNow))
          when (settle > 0) (writeIORef (chSettle c) (settle - 1))
        writeIORef (chInput c) synced
        writeIORef (chWasAnim c) animNow
        whenM (woPointerIn ops win) $ woCursor ops win ctx' synced
        quit <- quitRequested ctx'
        if quit then closeRequested w c else syncChildren w (Just win) ctx' (chChildren c)
  where
    ops = wOps w
    win = chWin c
