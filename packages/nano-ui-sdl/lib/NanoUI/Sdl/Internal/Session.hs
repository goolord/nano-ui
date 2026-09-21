{-# LANGUAGE DataKinds #-}

-- | SDL window session loop: event poll, resize sync, frame present.
module NanoUI.Sdl.Internal.Session
  ( runSdlSession
  ) where

import Control.Exception (bracket)
import Control.Monad (void, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import NanoUI.Backend (Input (..), clearEphemeral, emptyInput)
import NanoUI.Sdl.Internal.Debug (SdlDebugSampler (..))
import NanoUI.Runner
  ( SessionDriver (..)
  , newDrawingLock
  , runSessionLoop
  , shouldRedrawFrame
  , tryWithDrawingLock
  )
import NanoUI.Testing (Context)
import NanoUI.Sdl.Internal.Cursor (syncPointerCursor)
import NanoUI.Sdl.Internal.Input
  ( SdlEvent (..)
  , applyEvent
  , isButtonEdge
  , isHardQuit
  , pollEvents
  , waitEvent
  , waitEventTimeout
  )
import NanoUI.Sdl.Internal.Display (installResizeWatch, pushRefreshEvent)
import NanoUI.Sdl.Internal.Window (SdlEnv (..), SdlOptions (..), syncDisplay, withSdl)
import SDL3.Sys.Bindgen.Blendmode (sDL_BLENDMODE_BLEND)
import SDL3.Sys.Render (setRenderDrawBlendModeSafe, setRenderVSync)


runSdlSession ::
  SdlOptions ->
  Context ->
  (SdlEnv -> IO ()) ->
  (Input -> Bool) ->
  (Context -> SdlEnv -> Input -> Bool -> IO (Bool, Input)) ->
  IO ()
runSdlSession options ctx setup shouldQuit drawFn =
  withSdl options ctx $ \ctx0 env -> do
    setup env
    void $ setRenderDrawBlendModeSafe (sdlRenderer env) (fromIntegral sDL_BLENDMODE_BLEND)
    ctxRef <- newIORef ctx0
    prev <- newIORef emptyInput
    drawing <- newDrawingLock
    startupDone <- newIORef False
    startupCatchup <- newIORef False
    -- The resize watch presents with vsync off: Windows' modal size loop
    -- cannot take the next drag step while a present waits for vblank. The
    -- main loop turns vsync back on before its own frames.
    vsyncPaused <- newIORef False
    -- Window size the resize watch presented since the main loop last
    -- decided whether to draw. That frame already covers the size change and
    -- expose events the loop is about to see.
    resizePresented <- newIORef Nothing
    let onResize = do
          void $
            tryWithDrawingLock drawing $ do
              liveCtx <- readIORef ctxRef
              inp <- readIORef prev
              scale0 <- readIORef (sdlScaleRef env)
              (ctx', inpSynced) <- syncDisplay liveCtx env (clearEphemeral inp)
              writeIORef ctxRef ctx'
              done <- readIORef startupDone
              if not done
                then do
                  writeIORef prev inpSynced
                  writeIORef startupCatchup True
                else do
                  scale1 <- readIORef (sdlScaleRef env)
                  if inputWindowSize inpSynced == inputWindowSize inp && scale1 == scale0
                    then writeIORef prev inpSynced
                    else do
                      paused <- readIORef vsyncPaused
                      when (sdlVsync env && not paused) $ do
                        void $ setRenderVSync (sdlRenderer env) 0
                        writeIORef vsyncPaused True
                      (_, s) <- drawFn ctx' env inpSynced True
                      writeIORef prev s
                      writeIORef resizePresented (Just (inputWindowSize s))
    -- A wake (a background thread changed what the view reads, a file dialog
    -- finished) asks for a frame. What that frame presents is up to its
    -- damage: a wake that changed nothing on screen costs the UI pass and no
    -- repaint or present.
    wakeRef <- newIORef False
    -- A wake may postdate the watch's frame, so it voids that frame's cover.
    let noteWake evs = do
          when (EvRefresh `elem` evs) $ do
            writeIORef resizePresented Nothing
            writeIORef wakeRef True
          pure evs
    let drainUntilQuiet c inp = do
          pending <- pollEvents >>= noteWake
          (c', inp') <- syncDisplay c env (foldl' applyEvent inp pending)
          if null pending
            then pure (c', inp')
            else drainUntilQuiet c' inp'
    -- The opening frames are drawn here, outside the loop, so what one asks
    -- for beyond itself has to be carried into the loop by hand. A frame
    -- answers the wakes that came before it, and leaves the context dirty
    -- when it needs another.
    let startupFrame c inp = do
          writeIORef wakeRef False
          drawFn c env inp True
    let inpSeed = emptyInput {inputWindowSize = sdlWindowSize options}
    (ctx1, inp0) <- drainUntilQuiet ctx0 inpSeed
    writeIORef ctxRef ctx1
    scale0 <- readIORef (sdlScaleRef env)
    (_, synced0) <- startupFrame ctx1 inp0
    -- First present can apply DPI. Prev rects are empty on that frame.
    -- Draw once more before idle or the Controls page stays stretched
    -- until the first mouse move.
    (ctx1b, inp0b) <- drainUntilQuiet ctx1 synced0
    writeIORef ctxRef ctx1b
    scaleSettle <- readIORef (sdlScaleRef env)
    let paintedSize = inputWindowSize inp0b
    (_, synced0b) <- startupFrame ctx1b inp0b
    (ctx2, inp1) <- drainUntilQuiet ctx1b synced0b
    writeIORef ctxRef ctx2
    scale1 <- readIORef (sdlScaleRef env)
    catchup <- readIORef startupCatchup
    synced1 <-
      if catchup || inputWindowSize inp1 /= paintedSize || abs (scale1 - scaleSettle) > 0.001 || abs (scaleSettle - scale0) > 0.001
        then snd <$> startupFrame ctx2 inp1
        else pure inp1
    writeIORef startupCatchup False
    writeIORef startupDone True
    writeIORef prev synced1
    -- The last opening frame may have asked for another: it marked the
    -- context dirty, which the loop sees by itself, or a wake arrived after
    -- it began, which the drains above took off the queue. Queue that wake
    -- again, or the loop would block on a view waiting to be drawn until some
    -- input happened along.
    wokeSinceLastFrame <- readIORef wakeRef
    when wokeSinceLastFrame pushRefreshEvent
    let drv =
          SessionDriver
            { sdPollEvents    = pollEvents >>= noteWake
            , sdWaitEvents    = \t -> do
                -- Take the rest of the queue with the event that ended the
                -- wait, so one pass sees a whole burst (a resize queues
                -- several window events at once).
                woke <- if t < 0 then waitEvent else waitEventTimeout t
                case woke of
                  Nothing -> pure []
                  Just ev -> noteWake . (ev :) =<< pollEvents
            , sdApplyEvent    = applyEvent
            , sdIsButtonEdge  = isButtonEdge
            , sdIsHardQuit    = isHardQuit
            , sdIsSessionQuit = (== EvQuit)
            , sdSyncDisplay   = \c inp -> do
                paused <- readIORef vsyncPaused
                when paused $ do
                  void $ setRenderVSync (sdlRenderer env) 1
                  writeIORef vsyncPaused False
                (c', inp') <- syncDisplay c env inp
                writeIORef ctxRef c'
                writeIORef prev inp'
                pure (c', inp')
            , sdDebug         = sdsSampler (sdlDebug env)
            , sdContinuous    = sdlContinuous env
              -- With vsync on, presents throttle the loop. With vsync off a
              -- live animation would spin at max speed, so wait ~2 ms short
              -- of the frame period, leaving the slack for alignFrameStart:
              -- SDL_WaitEventTimeout overruns by ~1 ms, and a wait that
              -- returns past the boundary makes the frame late.
            , sdPacingMs      = if sdlVsync env then 16 else max 1 (floor (sdlRefreshPeriod env * 1000) - 2)
              -- A frame that skipped (empty damage, e.g. an animation scrolled
              -- out of view) did not wait for vblank, so it must not loop
              -- without waiting.
            , sdPresentPaces  = if sdlVsync env then readIORef (sdlLastPresented env) else pure False
            , sdShouldDraw    = \c prevInp inpSynced wasAnim refreshDue -> do
                presented <- readIORef resizePresented
                writeIORef resizePresented Nothing
                wakeDue <- readIORef wakeRef
                let (prevInp', inpSynced') = case presented of
                      Just size
                        | size == inputWindowSize inpSynced ->
                            (prevInp {inputWindowSize = size}, inpSynced {inputWindowRedraw = False})
                      _ -> (prevInp, inpSynced)
                shouldRedrawFrame c prevInp' inpSynced' wasAnim (sdlContinuous env) (refreshDue || wakeDue)
            , sdDraw          = \c inpSynced forceFull -> do
                writeIORef resizePresented Nothing
                -- Only a frame that runs answers a wake.
                ms <- tryWithDrawingLock drawing $ do
                  writeIORef wakeRef False
                  drawFn c env inpSynced (forceFull || sdlContinuous env)
                case ms of
                  Just (dirtyOut, s) -> do
                    writeIORef prev s
                    pure (dirtyOut, s)
                  Nothing -> do
                    -- The wake's event is already off the queue: queue it
                    -- again for the pass after the lock is free.
                    stillDue <- readIORef wakeRef
                    when stillDue pushRefreshEvent
                    pure (False, inpSynced)
            , sdOnCursor      = syncPointerCursor (sdlCursors env)
            , sdAlignSec      = sdlRefreshPeriod env
            , sdShouldQuit    = shouldQuit
            }
    bracket (installResizeWatch onResize) id $ \_ ->
      runSessionLoop drv ctx2 synced1
