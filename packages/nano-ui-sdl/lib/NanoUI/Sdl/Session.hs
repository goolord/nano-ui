{-# LANGUAGE DataKinds #-}

-- | SDL window session loop: event poll, resize sync, frame present.
module NanoUI.Sdl.Session
  ( runSdlSession
  ) where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Primitive.SmallArray (sizeofSmallArray)
import NanoUI
  ( Input (..)
  , V2 (..)
  , emptyInput
  , inputMousePressed
  , inputMouseReleased
  , inputMouseRightPressed
  , inputMouseRightReleased
  , inputScroll
  , inputWindowSize
  )
import NanoUI.Debug (debugRefreshSec)
import NanoUI.Runner
  ( SessionDriver (..)
  , newDrawingLock
  , runSessionLoop
  , tryWithDrawingLock
  )
import NanoUI.Testing
  ( Context
  , anyAnimating
  , clearDirty
  , debugPanelOpen
  , isDirty
  , needsRedraw
  , textFieldActive
  )
import NanoUI.Sdl.Debug (isDebugActive, noteLoop, noteSkip, takeDebugLive)
import NanoUI.Sdl.Cursor (syncPointerCursor)
import NanoUI.Sdl.Input
  ( SdlEvent (..)
  , applyEvent
  , clearEphemeral
  , isButtonEdge
  , isHardQuit
  , pollEvents
  , waitEvent
  , waitEventTimeout
  )
import NanoUI.Sdl.Display (installResizeWatch)
import NanoUI.Sdl.Window (SdlEnv (..), SdlOptions (..), syncDisplay, withSdl)
import SDL3.Sys.Bindgen.Blendmode (sDL_BLENDMODE_BLEND)
import SDL3.Sys.Render (setRenderDrawBlendModeSafe)

animateTimeout :: Int
animateTimeout = 16

debugHudTimeout :: Int
debugHudTimeout = max animateTimeout (round (debugRefreshSec * 1000))


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
                      (_, s) <- drawFn ctx' env inpSynced True
                      writeIORef prev s
    let drainUntilQuiet c inp = do
          pending <- pollEvents
          let inp' = foldl' applyEvent inp pending
          (c', inp'') <- syncDisplay c env inp'
          if sizeofSmallArray pending == 0
            then pure (c', inp'')
            else drainUntilQuiet c' inp''
    let inpSeed = emptyInput {inputWindowSize = sdlWindowSize options}
    (ctx1, inp0) <- drainUntilQuiet ctx0 inpSeed
    writeIORef ctxRef ctx1
    scale0 <- readIORef (sdlScaleRef env)
    (_, synced0) <- drawFn ctx1 env inp0 True
    -- First present can apply DPI. Prev rects are empty on that frame.
    -- Draw once more before idle or the Controls page stays stretched
    -- until the first mouse move.
    (ctx1b, inp0b) <- drainUntilQuiet ctx1 synced0
    writeIORef ctxRef ctx1b
    scaleSettle <- readIORef (sdlScaleRef env)
    let paintedSize = inputWindowSize inp0b
    (_, synced0b) <- drawFn ctx1b env inp0b True
    clearDirty ctx1b
    (ctx2, inp1) <- drainUntilQuiet ctx1b synced0b
    writeIORef ctxRef ctx2
    scale1 <- readIORef (sdlScaleRef env)
    catchup <- readIORef startupCatchup
    synced1 <-
      if catchup || inputWindowSize inp1 /= paintedSize || abs (scale1 - scaleSettle) > 0.001 || abs (scaleSettle - scale0) > 0.001
        then do
          (_, s) <- drawFn ctx2 env inp1 True
          clearDirty ctx2
          pure s
        else pure inp1
    writeIORef startupCatchup False
    writeIORef startupDone True
    writeIORef prev synced1
    let drv =
          SessionDriver
            { sdPollEvents    = foldr (:) [] <$> pollEvents
            , sdWaitEvents    = \t ->
                if t < 0
                  then foldr (:) [] <$> waitEvent
                  else foldr (:) [] <$> waitEventTimeout t
            , sdApplyEvent    = applyEvent
            , sdIsButtonEdge  = isButtonEdge
            , sdIsHardQuit    = isHardQuit
            , sdIsSessionQuit = (== EvQuit)
            , sdSyncDisplay   = \c inp -> do
                (c', inp') <- syncDisplay c env inp
                writeIORef ctxRef c'
                writeIORef prev inp'
                pure (c', inp')
            , sdWaitTimeout   = \c wasAnim -> do
                debugWinOpen <- debugPanelOpen c
                debugActive <- isDebugActive (sdlDebug env) debugWinOpen
                wantDebug <- takeDebugLive (sdlDebug env) debugActive
                animating <- anyAnimating c
                editing <- textFieldActive c
                dirtyWait <- isDirty c
                if sdlContinuous env || wantDebug || animating || dirtyWait
                  then pure 0
                  else if wasAnim || editing
                    then pure animateTimeout
                    else if debugActive
                      then pure debugHudTimeout
                      else pure (-1)
            , sdShouldDraw    = \c prevInp inpSynced wasAnim -> do
                debugWinOpen <- debugPanelOpen c
                debugActive <- isDebugActive (sdlDebug env) debugWinOpen
                wantDebug <- takeDebugLive (sdlDebug env) debugActive
                need <- needsRedraw c prevInp inpSynced
                dirtyNow <- isDirty c
                anim <- anyAnimating c
                editing <- textFieldActive c
                let forceFinal = wasAnim && not anim
                    pointerEdge =
                      inputMousePressed inpSynced
                        || inputMouseReleased inpSynced
                        || inputMouseRightPressed inpSynced
                        || inputMouseRightReleased inpSynced
                    scrollEdge = inputScroll inpSynced /= V2 0 0
                pure (sdlContinuous env || wantDebug || need || anim || forceFinal || dirtyNow || editing || pointerEdge || scrollEdge)
            , sdDraw          = \c inpSynced forceFull -> do
                ms <- tryWithDrawingLock drawing (drawFn c env inpSynced (forceFull || sdlContinuous env))
                case ms of
                  Just (dirtyOut, s) -> do
                    writeIORef prev s
                    pure (dirtyOut, s)
                  Nothing -> pure (False, inpSynced)
            , sdSkip          = \_ _ -> noteSkip (sdlDebug env)
            , sdOnCursor      = \c inpSynced -> syncPointerCursor (sdlCursors env) c inpSynced
            , sdNoteLoop      = noteLoop (sdlDebug env)
            , sdShouldQuit    = shouldQuit
            , sdClickDistance = 5.0
            , sdClickTime     = 0.4
            }
    bracket (installResizeWatch onResize) id $ \_ ->
      runSessionLoop drv ctx2 synced1
