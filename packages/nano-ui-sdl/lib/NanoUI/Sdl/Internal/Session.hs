-- | SDL window session loop: event poll, resize sync, frame present.
module NanoUI.Sdl.Internal.Session
  ( runSdlSession
  ) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless, void, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isNothing)
import NanoUI.Backend (Input (..), clearEphemeral, emptyInput)
import NanoUI.Runner
  ( SessionDriver (..)
  , newDrawingLock
  , runSessionLoop
  , shouldRedrawFrame
  , tryWithDrawingLock
  )
import NanoUI.Testing (Context, newPixelContext, registerImage, withTheme)
import NanoUI.Sdl.Internal.Cursor (syncPointerCursor)
import NanoUI.Sdl.Internal.Input
  ( SdlEvent (..)
  , applyEvent
  , isButtonEdge
  , pollEvents
  , waitEvent
  )
import NanoUI.Sdl.Internal.Display (installResizeWatch, pushRefreshEvent)
import NanoUI.Sdl.Internal.Window (RgbaImage (..), SdlEnv (..), SdlOptions (..), syncDisplay, withSdl)
import SDL3.Sys.Bindgen.Blendmode (sDL_BLENDMODE_BLEND)
import SDL3.Sys.Render (setRenderDrawBlendModeSafe, setRenderVSync)

-- | Open a window for the options, with their theme and images, and run the
-- event loop until it closes or 'sdlAppShouldQuit' says so. @drawFn@ draws a
-- frame and answers whether another is needed; its flag forces a full
-- repaint.
runSdlSession :: SdlOptions -> (Context -> SdlEnv -> Input -> Bool -> IO Bool) -> IO ()
runSdlSession options drawFn = do
  base <- newPixelContext
  ctx <- maybe (pure base) (withTheme base) (sdlAppTheme options)
  forM_ (sdlAppImages options) $ \(RgbaImage image w h pixels) ->
    registerImage ctx image w h pixels >>= (`unless` fail "registerImage failed")
  withSdl options ctx $ \ctx0 env -> do
    void $ setRenderDrawBlendModeSafe (sdlRenderer env) (fromIntegral sDL_BLENDMODE_BLEND)
    prev <- newIORef emptyInput
    drawing <- newDrawingLock
    -- The resize watch presents with vsync off: Windows' modal size loop
    -- cannot take the next drag step while a present waits for vblank. The
    -- main loop turns vsync back on before its own frames.
    vsyncPaused <- newIORef False
    -- Window size the resize watch presented since the main loop last
    -- decided whether to draw. That frame already covers the size change and
    -- expose events the loop is about to see.
    resizePresented <- newIORef Nothing
    -- The live context is the session's: 'syncDisplay' answers it.
    let onResize = void $ tryWithDrawingLock drawing $ do
          liveCtx <- readIORef (sdlCachedCtx env)
          inp <- readIORef prev
          scale0 <- readIORef (sdlScaleRef env)
          (ctx', inpSynced) <- syncDisplay liveCtx env (clearEphemeral inp)
          writeIORef prev inpSynced
          scale1 <- readIORef (sdlScaleRef env)
          unless (inputWindowSize inpSynced == inputWindowSize inp && scale1 == scale0) $ do
            paused <- readIORef vsyncPaused
            when (sdlVsync env && not paused) $ do
              void $ setRenderVSync (sdlRenderer env) 0
              writeIORef vsyncPaused True
            _ <- drawFn ctx' env inpSynced True
            writeIORef resizePresented (Just (inputWindowSize inpSynced))
    -- A wake (a background thread changed what the view reads, a file dialog
    -- finished) asks for a frame. What that frame presents is up to its
    -- damage: a wake that changed nothing on screen costs the UI pass and no
    -- repaint or present.
    wakeRef <- newIORef False
    let noteWake evs = evs <$ when (EvRefresh `elem` evs) (writeIORef wakeRef True)
    -- Take every queued event into the input, syncing the display after
    -- each batch, until none is left.
    let settle c inp = do
          pending <- pollEvents >>= noteWake
          (c', inp') <- syncDisplay c env (foldl' applyEvent inp pending)
          if null pending
            then pure (c', inp')
            else settle c' inp'
    -- The opening frames are drawn here, outside the loop, so what one asks
    -- for beyond itself has to be carried into the loop by hand. A frame
    -- answers the wakes that came before it, and leaves the context dirty
    -- when it needs another.
    let startupFrame c inp = do
          writeIORef wakeRef False
          void (drawFn c env inp True)
    (ctx1, inp0) <- settle ctx0 emptyInput {inputWindowSize = sdlWindowSize options}
    scale0 <- readIORef (sdlScaleRef env)
    startupFrame ctx1 inp0
    -- First present can apply DPI. Prev rects are empty on that frame.
    -- Draw once more before idle or the Controls page stays stretched
    -- until the first mouse move.
    (ctx1b, inp0b) <- settle ctx1 inp0
    scaleSettle <- readIORef (sdlScaleRef env)
    startupFrame ctx1b inp0b
    (ctx2, inp1) <- settle ctx1b inp0b
    scale1 <- readIORef (sdlScaleRef env)
    when (inputWindowSize inp1 /= inputWindowSize inp0b || abs (scale1 - scaleSettle) > 0.001 || abs (scaleSettle - scale0) > 0.001) $
      startupFrame ctx2 inp1
    writeIORef prev inp1
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
            , sdWaitEvents    = \t ->
                -- Take the rest of the queue with the event that ended the
                -- wait, so one pass sees a whole burst (a resize queues
                -- several window events at once).
                waitEvent t >>= maybe (pure []) (\ev -> noteWake . (ev :) =<< pollEvents)
            , sdApplyEvent    = applyEvent
            , sdIsButtonEdge  = isButtonEdge
            , sdIsSessionQuit = (== EvQuit)
            , sdSyncDisplay   = \c inp -> do
                paused <- readIORef vsyncPaused
                when paused $ do
                  void $ setRenderVSync (sdlRenderer env) 1
                  writeIORef vsyncPaused False
                synced@(_, inp') <- syncDisplay c env inp
                synced <$ writeIORef prev inp'
            , sdDebug         = sdlDebug env
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
                drawn <- tryWithDrawingLock drawing $ do
                  writeIORef wakeRef False
                  drawFn c env inpSynced (forceFull || sdlContinuous env)
                -- A frame the lock turned away leaves its wake's event off
                -- the queue: queue it again for the pass after the lock is
                -- free.
                when (isNothing drawn) $ readIORef wakeRef >>= (`when` pushRefreshEvent)
                pure (fromMaybe False drawn)
            , sdOnCursor      = syncPointerCursor (sdlCursors env)
            , sdAlignSec      = sdlRefreshPeriod env
            , sdShouldQuit    = sdlAppShouldQuit options
            }
    bracket (installResizeWatch onResize) id $ \_ ->
      runSessionLoop drv ctx2 inp1
