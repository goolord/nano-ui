-- | SDL window session loop: event poll, resize sync, frame present.
--
-- 'runSdlSession' opens the window, draws its opening frames
-- ('openingFrames') and runs "NanoUI.Runner"'s loop with 'sessionDriver'.
-- While Windows' modal size loop holds the event loop, the window's resize
-- watch draws with 'resizeFrame' instead. All three read and write the
-- 'Session' record.
module NanoUI.Sdl.Internal.Session
  ( runSdlSession
  ) where

import Control.Exception (bracket, finally)
import Control.Monad (forM_, unless, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isNothing)
import Foreign.Marshal.Utils (fromBool)
import GHC.Clock (getMonotonicTime)
import NanoUI (Size, WindowSettings (..), whenM)
import NanoUI.Backend (Context, Input (..), clearEphemeral, emptyInput, followSystemThemeIO, newPixelContext, requestWindowClose, setExplainLayout, withTheme)
import NanoUI.Runner
import NanoUI.Runner.Windows (closeWindows)
import NanoUI.Sdl.Internal.Children
import NanoUI.Sdl.Internal.Input
import NanoUI.Sdl.Internal.Display (ResizeWatches (..), newResizeWatches, pushRefreshEvent, rethrowResizeFailure, scaleMoved, watchResizes)
import NanoUI.Sdl.Internal.Window
  (SdlEnv (..), SdlOptions (..), presentPaces, registerAppImages, restartPresentProbe, sdlLivePacingMs, sdlPacingMs, syncDisplay, withSdl)
import SDL3.Sys.Bindgen.Blendmode (sDL_BLENDMODE_BLEND)
import SDL3.Sys.Render (setRenderDrawBlendModeSafe, setRenderVSyncSafe)

-- | Draws a frame and answers whether another is needed; its flag forces a
-- full repaint.
type DrawFn = Context -> SdlEnv -> Input -> Bool -> IO Bool

-- | What the session's loop driver, its opening frames and its resize watch
-- share: the window and the state each leaves for the others.
data Session = Session
  { sOptions :: !SdlOptions
  , sEnv :: !SdlEnv
  , sWindows :: !SdlWindows
  -- ^ Windows the view opens beside this one ('NanoUI.nativeWindow').
  , sWatches :: !ResizeWatches
  -- ^ Each window's resize watch draws it while Windows' modal size loop
  -- holds the event loop. They share the loop's drawing lock.
  , sDraw :: DrawFn
  , sPrev :: !(IORef Input)
  -- ^ The input the last pass, opening frame or resize frame synced, which
  -- the next resize frame starts from.
  , sVsyncPaused :: !(IORef Bool)
  -- ^ Whether the resize watch turned vsync off ('pauseVsync').
  , sResizePresented :: !(IORef (Maybe Size))
  -- ^ Window size the resize watch presented since the main loop last
  -- decided whether to draw. That frame already covers the size change and
  -- expose events the loop is about to see.
  , sWatchDrawnAt :: !(IORef Double)
  -- ^ When the watch last drew, or 0 when the loop has drawn since. A watch
  -- frame after another ticks animations by the time between them, not by
  -- the loop's last delta, and the loop's next frame ticks only the time
  -- after the watch's.
  , sWake :: !(IORef Bool)
  -- ^ A wake (a background thread changed what the view reads, a file
  -- dialog finished) that no frame has answered yet. What that frame
  -- presents is up to its damage: a wake that changed nothing on screen
  -- costs the UI pass and no repaint or present. Code may push the wake
  -- event without going through 'NanoUI.Internal.Context.ctxWoken', so this
  -- is not a copy of it.
  , sQuitTaken :: !(IORef Bool)
  -- ^ A close request 'settle' took off the queue during the opening frames.
  -- SDL asks to quit once, so it is kept for the loop.
  , sPassStart :: !(IORef Double)
  -- ^ When the loop's current pass began, for pacing the next by time
  -- ('pacedWait').
  }

-- | Open a window for the options, with their theme and images, and run the
-- event loop until it closes or 'sdlAppShouldQuit' says so. @drawFn@ draws a
-- frame and answers whether another is needed; its flag forces a full
-- repaint.
runSdlSession :: SdlOptions -> DrawFn -> IO ()
runSdlSession options drawFn = do
  base <- newPixelContext
  ctx <- maybe (pure base) (withTheme base) (sdlAppTheme options)
  forM_ (sdlAppThemeFor options) (followSystemThemeIO ctx)
  setExplainLayout ctx (sdlExplainLayout options)
  registerAppImages options ctx
  withSdl options ctx $ \ctx0 env -> do
    watches <- newResizeWatches
    windows <- newSdlWindows options env watches
    -- Every exit closes the further windows, those the opening frames
    -- opened among them, and while SDL is still running.
    flip finally (closeWindows (swWindows windows)) $ do
      void $ setRenderDrawBlendModeSafe (sdlRenderer env) (maybe (fromIntegral sDL_BLENDMODE_BLEND) fst (sdlTransparent env))
      s <-
        Session options env windows watches drawFn
          <$> newIORef emptyInput
          <*> newIORef False
          <*> newIORef Nothing
          <*> newIORef 0
          <*> newIORef False
          <*> newIORef False
          <*> newIORef 0
      (ctx2, inp1) <- openingFrames s ctx0
      -- A close request the opening frames took, which the loop would not see:
      -- it ends the session here, or with exits off the loop's first frame
      -- reports it.
      quit <- readIORef (sQuitTaken s)
      closeNow <- if quit then requestWindowClose ctx2 else pure False
      unless closeNow $
        bracket (watchResizes watches (sdlWindow env) (resizeFrame s)) id $ \_ ->
          runSessionLoop (sessionDriver s) ctx2 inp1

-- | Draw the opening frames, before the loop starts, and answer the context
-- and input the loop starts from.
--
-- SDL draws two or three opening frames where the RGFW and web backends
-- draw one. A window's first present can apply the display's pixel density,
-- so the first frame may be drawn at a scale the window does not keep, with
-- no previous rects to repaint from. A second frame draws at the settled
-- scale; without it a page stays stretched until the first pointer move. A
-- third runs when the size or scale moved again after the second frame, or
-- when events queued meanwhile carry one-shot input (presses, keys, text),
-- which the loop's first pass would clear unseen. RGFW sizes its hidden
-- window at the monitor's scale before showing it and drawing, and the web
-- backend reads the canvas's pixel ratio before its first frame, so their
-- one opening frame has the scale it keeps; both leave every queued event to
-- the loop.
--
-- The frames are drawn outside the loop, so what one asks for beyond itself
-- has to be carried into the loop by hand. A frame answers the wakes that
-- came before it, and leaves the context dirty when it needs another.
openingFrames :: Session -> Context -> IO (Context, Input)
openingFrames s ctx0 = do
  (ctx1, inp0, scale0) <- settleScaled ctx0 emptyInput {inputWindowSize = wsSize (sdlWindowSettings (sOptions s))}
  startupFrame ctx1 inp0
  -- Each frame's one-shot input (presses, keys, text) is only what came
  -- after the frame before.
  (ctx1b, inp0b, scaleSettle) <- settleScaled ctx1 (clearEphemeral inp0)
  startupFrame ctx1b inp0b
  (ctx2, inp1, scale1) <- settleScaled ctx1b (clearEphemeral inp0b)
  -- Once more for a size or scale the second frame did not have, or for
  -- one-shot input it did not see, which the loop's first pass clears.
  when (inputWindowSize inp1 /= inputWindowSize inp0b || scaleMoved scale1 scaleSettle || scaleMoved scaleSettle scale0 || inp1 /= clearEphemeral inp1) $
    startupFrame ctx2 inp1
  writeIORef (sPrev s) inp1
  -- Open the windows the opening frames declared, which the loop would
  -- otherwise leave for its first pass, after waiting for input.
  afterFirstPass (sWindows s) ctx2
  -- The last opening frame may have asked for another: it marked the
  -- context dirty, which the loop sees by itself, or a wake arrived after
  -- it began, which the drains above took off the queue. Queue that wake
  -- again, or the loop would block on a view waiting to be drawn until some
  -- input happened along.
  whenM (readIORef (sWake s)) pushRefreshEvent
  pure (ctx2, inp1)
  where
    startupFrame c inp = do
      writeIORef (sWake s) False
      void (sDraw s c (sEnv s) inp True)
    settleScaled c inp = do
      (c', inp') <- settle s c inp
      scale <- readIORef (sdlScaleRef (sEnv s))
      pure (c', inp', scale)

-- | Take every queued event into the input, syncing the display after each
-- batch, until none is left. A close request taken here is kept in
-- 'sQuitTaken'.
settle :: Session -> Context -> Input -> IO (Context, Input)
settle s c inp = do
  pending <- pollBatch s
  when (EvQuit `elem` pending) (writeIORef (sQuitTaken s) True)
  (c', inp') <- syncDisplay c (sEnv s) (foldl' applyEvent inp pending)
  if null pending then pure (c', inp') else settle s c' inp'

-- | This window's share of a batch of events, the rest routed to the
-- windows they are for. A wake among them is noted in 'sWake'.
decodeBatch :: Session -> [SdlEvent] -> IO [SdlEvent]
decodeBatch s evs = do
  when (EvRefresh `elem` evs) (writeIORef (sWake s) True)
  decodeForWindow (sEnv s) evs

-- | Take every queued event.
pollBatch :: Session -> IO [SdlEvent]
pollBatch s = pollWindowEvents >>= routeEvents (sWindows s) >>= decodeBatch s

-- | Wait up to @t@ ms for an event (-1 for no limit), then take the rest of
-- the queue with it, so one pass sees a whole burst (a resize queues several
-- window events at once).
waitBatch :: Session -> Int -> IO [SdlEvent]
waitBatch s t =
  waitWindowEvent t >>= maybe (pure []) (\ev -> routeEvents (sWindows s) . (ev :) =<< pollWindowEvents) >>= decodeBatch s

-- | The loop's 'sdPacedWait': the wait between the frames of an animation
-- whose presents do not wait for the display. It follows the display the
-- window is on now ('sdlRefreshPeriodRef') rather than the one it opened on:
-- the wait is that display's, and a wait that times out starts the frame a
-- refresh after the pass began ('sPassStart'). The loop's own alignment
-- ('sdAlignSec') winds onto a fixed period, which cannot follow the window
-- to another display.
pacedWait :: Session -> IO [SdlEvent]
pacedWait s = do
  evs <- waitBatch s =<< sdlLivePacingMs env
  when (null evs) $ do
    period <- readIORef (sdlRefreshPeriodRef env)
    waitUntil . (+ period) =<< readIORef (sPassStart s)
  pure evs
  where
    env = sEnv s

-- | Turn vsync off while the resize watch draws, and on again for the
-- loop's frames. The watch presents with vsync off: Windows' modal size loop
-- cannot take the next drag step while a present waits for vblank.
pauseVsync :: Session -> Bool -> IO ()
pauseVsync s paused =
  when (sdlVsync env) $ whenM ((/= paused) <$> readIORef (sVsyncPaused s)) $ do
    void $ setRenderVSyncSafe (sdlRenderer env) (fromBool (not paused))
    writeIORef (sVsyncPaused s) paused
  where
    env = sEnv s

-- | The resize watch's frame, drawn while Windows' modal size loop holds the
-- event loop, when the window's size or scale moved. 'watchResizes' runs it
-- under the drawing lock. The live context is the session's: 'syncDisplay'
-- answers it.
resizeFrame :: Session -> IO ()
resizeFrame s = do
  liveCtx <- readIORef (sdlCachedCtx env)
  inp <- readIORef (sPrev s)
  scale0 <- readIORef (sdlScaleRef env)
  (ctx', inpSynced) <- syncDisplay liveCtx env (clearEphemeral inp)
  writeIORef (sPrev s) inpSynced
  scale1 <- readIORef (sdlScaleRef env)
  unless (inputWindowSize inpSynced == inputWindowSize inp && scale1 == scale0) $ do
    pauseVsync s True
    now <- getMonotonicTime
    t <- readIORef (sWatchDrawnAt s)
    writeIORef (sWatchDrawnAt s) now
    -- At most the 50 ms the loop ticks a frame by.
    let dt = if t > 0 then min 0.05 (realToFrac (now - t)) else inputDeltaTime inpSynced
    _ <- sDraw s ctx' env inpSynced {inputDeltaTime = dt} True
    -- Its present ran with vsync off on purpose: it says nothing of
    -- whether the driver honours vsync.
    restartPresentProbe env
    writeIORef (sResizePresented s) (Just (inputWindowSize inpSynced))
  where
    env = sEnv s

-- | The loop's view of the session.
sessionDriver :: Session -> SessionDriver SdlEvent
sessionDriver s =
  SessionDriver
    { sdPollEvents    = pollBatch s
    , sdWaitEvents    = waitBatch s
    , sdPacedWait     = Just (\_ -> pacedWait s)
    , sdApplyEvent    = applyEvent
    , sdIsButtonEdge  = isButtonEdge
    , sdIsSessionQuit = (== EvQuit)
    , sdSyncDisplay   = \c inp -> do
        writeIORef (sPassStart s) =<< getMonotonicTime
        rethrowResizeFailure (sWatches s)
        pauseVsync s False
        synced@(_, inp') <- syncDisplay c env inp
        synced <$ writeIORef (sPrev s) inp'
    , sdDebug         = sdlDebug env
    , sdContinuous    = sdlContinuous env
    , sdVsyncOff      = not (sdlVsync env)
    , sdPacingMs      = sdlPacingMs env
      -- A frame that skipped (empty damage, e.g. an animation scrolled
      -- out of view) did not wait for vblank, so it must not loop
      -- without waiting; nor may one whose driver turned vsync off
      -- ('NanoUI.Sdl.Internal.Window.notePresent').
    , sdPresentPaces  = presentPaces env
    , sdShouldDraw    = \c prevInp inpSynced wasAnim refreshDue -> do
        presented <- readIORef (sResizePresented s)
        writeIORef (sResizePresented s) Nothing
        wakeDue <- readIORef (sWake s)
        let !(prevInp', inpSynced') = case presented of
              Just size
                | size == inputWindowSize inpSynced ->
                    (prevInp {inputWindowSize = size}, inpSynced {inputWindowRedraw = False})
              _ -> (prevInp, inpSynced)
        shouldRedrawFrame c prevInp' inpSynced' wasAnim (sdlContinuous env) (refreshDue || wakeDue)
    , sdDraw          = \c inpSynced forceFull -> do
        writeIORef (sResizePresented s) Nothing
        -- Only a frame that runs answers a wake.
        drawn <- tryWithDrawingLock (rwDrawing (sWatches s)) $ do
          writeIORef (sWake s) False
          t <- readIORef (sWatchDrawnAt s)
          if t > 0
            then do
              now <- getMonotonicTime
              writeIORef (sWatchDrawnAt s) 0
              sDraw s c env inpSynced {inputDeltaTime = min (inputDeltaTime inpSynced) (realToFrac (now - t))} forceFull
            else sDraw s c env inpSynced forceFull
        -- A frame the lock turned away leaves its wake's event off
        -- the queue: queue it again for the pass after the lock is
        -- free.
        when (isNothing drawn) $ whenM (readIORef (sWake s)) pushRefreshEvent
        pure (fromMaybe False drawn)
    , sdOnCursor      = syncFirstCursor (sWindows s)
      -- Unused: 'sdPacedWait' aligns frames itself.
    , sdAlignSec      = 0
    , sdShouldQuit    = sdlAppShouldQuit (sOptions s)
    , sdWindows       = Just (sdlLoopWindows (sWindows s))
    }
  where
    env = sEnv s
