-- | A session's further windows ('NanoUI.nativeWindow'): opening and closing
-- them as views declare them, routing SDL's events to them, and running their
-- frames after each pass of the first window's loop. A session whose views
-- declare none does none of this beyond finding the list empty.
module NanoUI.Sdl.Internal.Children
  ( Windows (..)
  , newWindows
  , routeEvents
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
import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTime)
import NanoUI (V2 (..), WindowSettings (..), readVarIO, unlessM, whenM, writeVarIO)
import NanoUI.Backend
  ( ChildWindow (..)
  , Context
  , Input (..)
  , MouseButtons
  , childView
  , childWindows
  , clearEphemeral
  , emptyInput
  , getWakeAt
  , isDirty
  , noButtons
  , quitRequested
  , requestWindowClose
  , varChangeCount
  )
import NanoUI.Internal.Context (Context (..), anyAnimating, clearWakeAt, takeWakes)
import NanoUI.Internal.Input (takeFrame)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Runner (shouldRedrawFrame, stampClicks)
import NanoUI.Sdl.Internal.Cursor (syncPointerCursor)
import NanoUI.Sdl.Internal.Display (pointerInWindow)
import NanoUI.Sdl.Internal.Input
import NanoUI.Sdl.Internal.Runner (resetRenderTextures, sdlDrawFrame)
import NanoUI.Sdl.Internal.Window (SdlEnv (..), SdlOptions, openChildWindow, syncDisplay, windowZoom)
import SDL3.Sys.Bindgen.Stdinc (Uint32 (..))
import SDL3.Sys.Bindgen.Video (SDL_WindowID (..))
import SDL3.Sys.Video (getWindowID)

-- | The session's windows beyond the first: those its view declared, each
-- with those it declared in turn.
data Windows = Windows
  { wOptions :: !SdlOptions
  , wFirst :: !SdlEnv
  , wFirstId :: !Word32
  , wTop :: !(IORef [Child])
  , wFirstSeen :: !(IORef Word64)
  -- ^ 'varChangeCount' when the first window's view last began.
  }

-- | One further window and its own loop state.
data Child = Child
  { chKey :: !WidgetId
  , chId :: !Word32
  , chEnv :: !SdlEnv
  , chClose :: IO ()
  , chClosed :: !(IORef Bool)
  -- ^ Closed, though a sweep that began before may still hold it.
  , chDecl :: !(IORef ChildWindow)
  -- ^ The latest declaration, whose view and 'Var' are current.
  , chSiblings :: !(IORef [Child])
  , chChildren :: !(IORef [Child])
  , chCtx :: !(IORef Context)
  , chInput :: !(IORef Input)
  , chQueue :: !(IORef [SdlEvent])
  -- ^ Routed events not yet taken, newest first.
  , chLastT :: !(IORef Double)
  , chWasAnim :: !(IORef Bool)
  , chDirty :: !(IORef Bool)
  -- ^ The last frame asked for another.
  , chSettle :: !(IORef Int)
  -- ^ Opening frames still to draw: the first lays out against no previous
  -- rectangles, as the first window's opening frames do.
  , chSeen :: !(IORef Word64)
  , chClicks :: !(IORef (Double, V2, MouseButtons, Int))
  }

newWindows :: SdlOptions -> SdlEnv -> IO Windows
newWindows opts first = do
  firstId <- windowId first
  Windows opts first firstId <$> newIORef [] <*> (newIORef =<< varChangeCount)

windowId :: SdlEnv -> IO Word32
windowId env = (\(SDL_WindowID (Uint32 w)) -> w) <$> getWindowID (sdlWindow env)

-- | Every further window, each before those it declared.
allChildren :: IORef [Child] -> IO [Child]
allChildren ref = readIORef ref >>= fmap concat . mapM (\c -> (c :) <$> allChildren (chChildren c))

-- | Keep the first window's events and queue the others' on their windows.
-- With none open, every event is the first window's, as in a session of
-- one; a close request there is followed by SDL's quit. With some open, the
-- first window's close request ends the session (SDL quits only after the
-- last window), events for every window go to each, and those for a window
-- just closed go nowhere.
routeEvents :: Windows -> [(Word32, SdlEvent)] -> IO [SdlEvent]
routeEvents w evs = do
  kids <- allChildren (wTop w)
  if null kids
    then pure [ev | (_, ev) <- evs, ev /= EvCloseRequested]
    else concat <$> mapM (route kids) evs
  where
    route kids (wid, ev)
      | wid == 0 = do
          unless (ev == EvQuit || ev == EvRefresh) $ forM_ kids (`queue` ev)
          pure [ev]
      | wid == wFirstId w = pure [if ev == EvCloseRequested then EvQuit else ev]
      | otherwise = [] <$ forM_ (find ((== wid) . chId) kids) (`queue` ev)
    queue c ev = modifyIORef' (chQueue c) (ev :)

-- | The longest wait, in milliseconds, the further windows allow before the
-- loop runs again; -1 for no limit. A session of one window answers -1.
windowsWait :: Windows -> IO Int
windowsWait w = do
  kids <- allChildren (wTop w)
  if null kids
    then pure (-1)
    else do
      firstDue <- mainNeedsFrame w
      if firstDue then pure 0 else foldlM (\acc c -> shorter acc <$> childWait c) (-1) kids
  where
    shorter a b
      | a < 0 = b
      | b < 0 = a
      | otherwise = min a b

childWait :: Child -> IO Int
childWait c = do
  due <- childDue c
  if due
    then pure 0
    else do
      ctx <- readIORef (chCtx c)
      animating <- (||) <$> readIORef (chWasAnim c) <*> anyAnimating ctx
      if animating
        then pure (max 1 (floor (sdlRefreshPeriod (chEnv c) * 1000)))
        else do
          wakeAt <- getWakeAt ctx
          if wakeAt <= 0
            then pure (-1)
            else (\now -> max 0 (ceiling (min 3600 (wakeAt - now) * 1000) + 2)) <$> getMonotonicTime

-- | Whether a window has something to draw now, animations and timed wakes
-- aside.
childDue :: Child -> IO Bool
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

-- | Whether the first window should run its view because a 'Var' changed
-- since it last did, which matters only while further windows are open.
mainNeedsFrame :: Windows -> IO Bool
mainNeedsFrame w = do
  kids <- readIORef (wTop w)
  if null kids then pure False else (/=) <$> varChangeCount <*> readIORef (wFirstSeen w)

-- | Note that the first window's view is about to run.
noteMainFrame :: Windows -> IO ()
noteMainFrame w = writeIORef (wFirstSeen w) =<< varChangeCount

-- | After a pass of the first window's loop: open and close windows for what
-- its view declared, sync the pointer's cursor from the window it is over,
-- and run the further windows' frames.
afterMainPass :: Windows -> Context -> Input -> IO ()
afterMainPass w ctx inp = do
  syncChildren w ctx (wTop w)
  kids <- allChildren (wTop w)
  if null kids
    then syncPointerCursor (sdlCursors first) ctx inp
    else do
      whenM ((/= 0) <$> pointerInWindow (sdlWindow first)) $ syncPointerCursor (sdlCursors first) ctx inp
      forM_ kids $ \c -> unlessM (readIORef (chClosed c)) (stepChild w c)
  where
    first = wFirst w

-- | Open a window for each new declaration of @parent@'s view and close
-- those it no longer declares, or declares closed.
syncChildren :: Windows -> Context -> IORef [Child] -> IO ()
syncChildren w parent ref = do
  decls <- childWindows parent
  old <- readIORef ref
  unless (null decls && null old) $ do
    open <- filterM (readVarIO . childOpen) decls
    new <- forM open $ \d -> case find ((== childKey d) . chKey) old of
      Just c -> c <$ writeIORef (chDecl c) d
      Nothing -> openChild w parent ref d
    forM_ old $ \c -> unless (any ((== chKey c) . chKey) new) (closeChild c)
    writeIORef ref new

openChild :: Windows -> Context -> IORef [Child] -> ChildWindow -> IO Child
openChild w parent siblings decl = do
  (ctx, env, close) <- openChildWindow (wOptions w) (wFirst w) parent (childSettings decl)
  now <- getMonotonicTime
  Child (childKey decl)
    <$> windowId env
    <*> pure env
    <*> pure close
    <*> newIORef False
    <*> newIORef decl
    <*> pure siblings
    <*> newIORef []
    <*> newIORef ctx
    <*> newIORef emptyInput {inputWindowSize = wsSize (childSettings decl)}
    <*> newIORef []
    <*> newIORef now
    <*> newIORef False
    <*> newIORef False
    <*> newIORef 2
    <*> newIORef 0
    <*> newIORef (0, V2 (-999) (-999), noButtons, 0)

-- | Close a window and those it declared.
closeChild :: Child -> IO ()
closeChild c = unlessM (readIORef (chClosed c)) $ do
  writeIORef (chClosed c) True
  mapM_ closeChild =<< readIORef (chChildren c)
  writeIORef (chChildren c) []
  chClose c

-- | Close every further window, when the session ends.
closeWindows :: Windows -> IO ()
closeWindows w = do
  mapM_ closeChild =<< readIORef (wTop w)
  writeIORef (wTop w) []

-- | The window asked to close (its close button, or 'NanoUI.quitUi' in its
-- view): write 'False' to its 'Var', which shows in the declaring view's
-- next frame, and close it now. A read-only 'Var' keeps it open.
closeRequested :: Child -> IO ()
closeRequested c = do
  open <- childOpen <$> readIORef (chDecl c)
  writeVarIO open False
  unlessM (readVarIO open) $ do
    closeChild c
    modifyIORef' (chSiblings c) (filter ((/= chKey c) . chKey))

-- | One pass of a further window's loop: take its queued events into a
-- frame's input, as the first window's loop does, and draw when anything
-- calls for it. Nothing to do costs a few reads.
stepChild :: Windows -> Child -> IO ()
stepChild w c = do
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
    zoom <- windowZoom env
    evs <- scaleEventPositions zoom . reverse <$> readIORef (chQueue c)
    when (hasPointerEvent evs) (writeIORef (sdlPointerSeen env) True)
    when (EvRenderDeviceReset `elem` evs) (resetRenderTextures env)
    inp <- readIORef (chInput c)
    lastT <- readIORef (chLastT c)
    let dt = min 0.05 (realToFrac (now - lastT))
        (frameInp, group, rest) = takeFrame applyEvent isButtonEdge (clearEphemeral inp {inputDeltaTime = dt}) evs
    writeIORef (chQueue c) (reverse rest)
    writeIORef (chLastT c) now
    closing <- if EvCloseRequested `elem` group then requestWindowClose ctx else pure False
    if closing
      then closeRequested c
      else do
        stamped <- stampClicks (chClicks c) frameInp
        (ctx', synced) <- syncDisplay ctx env stamped
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
          writeIORef (chDirty c) =<< sdlDrawFrame ctx' view env synced (settle > 0 || (wasAnim && not animNow))
          when (settle > 0) (writeIORef (chSettle c) (settle - 1))
        writeIORef (chInput c) synced
        writeIORef (chWasAnim c) animNow
        whenM ((/= 0) <$> pointerInWindow (sdlWindow env)) $ syncPointerCursor (sdlCursors env) ctx' synced
        quit <- quitRequested ctx'
        if quit then closeRequested c else syncChildren w ctx' (chChildren c)
  where
    env = chEnv c
