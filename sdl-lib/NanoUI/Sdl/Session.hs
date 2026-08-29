{-# LANGUAGE DataKinds #-}

-- | SDL window session loop: event poll, resize sync, frame present.
module NanoUI.Sdl.Session
  ( runSdlSession
  ) where

import Control.Exception (bracket, finally)
import Control.Monad (void, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Input (..)
  , emptyInput
  , inputInteracted
  , inputWindowSize
  )
import NanoUI.Testing
  ( Context
  , anyAnimating
  , clearDirty
  , debugPanelOpen
  , isDirty
  , needsRedraw
  , overlayConsumesQuit
  , textFieldActive
  , textInputEditActive
  )
import NanoUI.Sdl.Debug (noteLoop, noteSkip, takeDebugLive)
import NanoUI.Sdl.Event (SdlEvent (..))
import NanoUI.Sdl.Input
  ( applyEvent
  , clearEphemeral
  , isHardQuit
  , isHardQuitInput
  , pollEvents
  , splitFrame
  , waitEvent
  , waitEventTimeout
  )
import NanoUI.Sdl.Display (installResizeWatch)
import NanoUI.Sdl.Window (SdlEnv (..), defaultWindowSize, syncDisplay, withSdl)
import SDL3.Sys.Bindgen.Blendmode (sDL_BLENDMODE_BLEND)
import SDL3.Sys.Render (setRenderDrawBlendModeSafe)

animateTimeout :: Int
animateTimeout = 16

maxFrameDt :: Float
maxFrameDt = 0.05

runSdlSession ::
  Bool ->
  Context ->
  (SdlEnv -> IO ()) ->
  (Input -> Bool) ->
  (Context -> SdlEnv -> Input -> Bool -> IO (Bool, Input)) ->
  IO ()
runSdlSession vsync ctx setup shouldQuit drawFn =
  withSdl vsync ctx "nano-ui" defaultWindowSize $ \ctx0 env -> do
    setup env
    void $ setRenderDrawBlendModeSafe (sdlRenderer env) (fromIntegral sDL_BLENDMODE_BLEND)
    ctxRef <- newIORef ctx0
    prev <- newIORef emptyInput
    pendingRedraw <- newIORef False
    wasAnimating <- newIORef False
    drawing <- newIORef False
    startupDone <- newIORef False
    startupCatchup <- newIORef False
    startupGrace <- newIORef (2 :: Int)
    startupFull <- newIORef (2 :: Int)
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
          if null pending
            then pure (c', inp'')
            else drainUntilQuiet c' inp''
    let inpSeed = emptyInput {inputWindowSize = defaultWindowSize}
    (ctx1, inp0) <- drainUntilQuiet ctx0 inpSeed
    writeIORef ctxRef ctx1
    scale0 <- readIORef (sdlScaleRef env)
    let paintedSize = inputWindowSize inp0
    (_, synced0) <- drawFn ctx1 env inp0 True
    clearDirty ctx1
    (ctx2, inp1) <- drainUntilQuiet ctx1 synced0
    writeIORef ctxRef ctx2
    scale1 <- readIORef (sdlScaleRef env)
    catchup <- readIORef startupCatchup
    synced1 <-
      if catchup || inputWindowSize inp1 /= paintedSize || abs (scale1 - scale0) > 0.001
        then do
          (_, s) <- drawFn ctx2 env inp1 True
          clearDirty ctx2
          pure s
        else pure inp1
    writeIORef startupCatchup False
    writeIORef startupDone True
    writeIORef pendingRedraw False
    writeIORef prev synced1
    now <- getMonotonicTime
    bracket (installResizeWatch onResize) id $ \_ ->
      loop ctxRef drawFn env prev pendingRedraw wasAnimating drawing startupGrace startupFull shouldQuit synced1 [] now

loop ::
  IORef Context ->
  (Context -> SdlEnv -> Input -> Bool -> IO (Bool, Input)) ->
  SdlEnv ->
  IORef Input ->
  IORef Bool ->
  IORef Bool ->
  IORef Bool ->
  IORef Int ->
  IORef Int ->
  (Input -> Bool) ->
  Input ->
  [SdlEvent] ->
  Double ->
  IO ()
loop ctxRef drawFn env prev pendingRedraw wasAnimating drawing startupGrace startupFull shouldQuit inp queued lastT = do
  ctx <- readIORef ctxRef
  debugOpen <- debugPanelOpen ctx
  wantDebug <- takeDebugLive (sdlDebug env) debugOpen
  pending <-
    if null queued
      then do
        polled <- pollEvents
        if not (null polled)
          then pure polled
          else do
            animating <- anyAnimating ctx
            editing <- textFieldActive ctx
            wasAnimWait <- readIORef wasAnimating
            nFullWait <- readIORef startupFull
            if animating
              then pure []
              else
                if wasAnimWait || nFullWait > 0 || wantDebug || editing || debugOpen
                  then waitEventTimeout animateTimeout
                  else waitEvent
      else pure queued
  let (group, rest) = splitFrame pending
  editActive <- textInputEditActive ctx
  if any (== EvQuit) group || (any isHardQuit group && not editActive)
    then pure ()
    else do
      now <- getMonotonicTime
      let dt = min maxFrameDt (realToFrac (now - lastT))
      noteLoop (sdlDebug env) dt
      let inp' =
            foldl'
              applyEvent
              (clearEphemeral inp {inputDeltaTime = dt})
              group
      (ctx', inpSynced) <- syncDisplay ctx env inp'
      writeIORef ctxRef ctx'
      editActive' <- textInputEditActive ctx'
      if isHardQuitInput inpSynced && not editActive'
        then pure ()
        else do
          prevInp <- readIORef prev
          pendingDirty <- readIORef pendingRedraw
          wasAnim <- readIORef wasAnimating
          need <- needsRedraw ctx' prevInp inpSynced
          dirtyNow <- isDirty ctx'
          anim <- anyAnimating ctx'
          editing <- textFieldActive ctx'
          grace <- readIORef startupGrace
          nFull <- readIORef startupFull
          let sizeChanged = inputWindowSize prevInp /= inputWindowSize inpSynced
              interacted = inputInteracted prevInp inpSynced
              graceAllow =
                grace <= 0 || sizeChanged || interacted || anim || editing || dirtyNow || pendingDirty || need || nFull > 0
              forceFinal = wasAnim && not anim
              shouldDraw =
                graceAllow
                  && ( need
                         || anim
                         || forceFinal
                         || pendingDirty
                         || dirtyNow
                         || debugOpen
                         || wantDebug
                         || editing
                         || nFull > 0
                     )
          when (grace > 0) $ writeIORef startupGrace (grace - 1)
          synced <-
            if shouldDraw
              then do
                ms <-
                  tryWithDrawingLock drawing $ do
                    (_, s) <- drawFn ctx' env inpSynced (debugOpen || wantDebug || nFull > 0)
                    when (nFull > 0) $ writeIORef startupFull (nFull - 1)
                    writeIORef pendingRedraw False
                    writeIORef prev s
                    pure s
                maybe (pure inpSynced) pure ms
              else do
                noteSkip (sdlDebug env)
                writeIORef prev inpSynced
                pure inpSynced
          animAfter <- anyAnimating ctx'
          writeIORef wasAnimating (anim || animAfter)
          overlayQuit <- overlayConsumesQuit ctx' inpSynced
          if shouldQuit inpSynced && not overlayQuit
            then pure ()
            else
              loop
                ctxRef
                drawFn
                env
                prev
                pendingRedraw
                wasAnimating
                drawing
                startupGrace
                startupFull
                shouldQuit
                synced
                (if null rest then [] else rest)
                now

tryWithDrawingLock :: IORef Bool -> IO a -> IO (Maybe a)
tryWithDrawingLock ref act = do
  ok <- atomicModifyIORef' ref $ \busy -> if busy then (True, False) else (True, True)
  if ok
    then Just <$> (act `finally` writeIORef ref False)
    else pure Nothing
