{-# LANGUAGE DataKinds #-}

-- | SDL window session loop: event poll, resize sync, frame present.
module NanoUI.Sdl.Session
  ( runSdlSession
  ) where

import Control.Exception (bracket, finally)
import Control.Monad (void, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Primitive.SmallArray (SmallArray, emptySmallArray, sizeofSmallArray)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Input (..)
  , V2 (..)
  , emptyInput
  , inputInteracted
  , inputMousePressed
  , inputMouseReleased
  , inputMouseRightPressed
  , inputMouseRightReleased
  , inputWindowSize
  )
import NanoUI.Debug (debugRefreshSec)
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
import NanoUI.Sdl.Cursor (syncPointerCursor)
import NanoUI.Sdl.Input
  ( SdlEvent (..)
  , applyEvent
  , clearEphemeral
  , isHardQuit
  , isHardQuitInput
  , pollEvents
  , splitFrame
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

maxFrameDt :: Float
maxFrameDt = 0.05

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
    pendingRedraw <- newIORef False
    wasAnimating <- newIORef False
    drawing <- newIORef False
    startupDone <- newIORef False
    startupCatchup <- newIORef False
    startupGrace <- newIORef (2 :: Int)
    startupFull <- newIORef (2 :: Int)
    firstPointerFull <- newIORef True
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
    writeIORef pendingRedraw False
    writeIORef prev synced1
    now <- getMonotonicTime
    bracket (installResizeWatch onResize) id $ \_ ->
      loop ctxRef drawFn env prev pendingRedraw wasAnimating drawing startupGrace startupFull firstPointerFull shouldQuit synced1 emptySmallArray now

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
  IORef Bool ->
  (Input -> Bool) ->
  Input ->
  SmallArray SdlEvent ->
  Double ->
  IO ()
loop ctxRef drawFn env prev pendingRedraw wasAnimating drawing startupGrace startupFull firstPointerFull shouldQuit inp queued lastT = do
  ctx <- readIORef ctxRef
  debugOpen <- debugPanelOpen ctx
  pending <-
    if sizeofSmallArray queued == 0
      then do
        polled <- pollEvents
        if sizeofSmallArray polled /= 0
          then pure polled
          else do
            animating <- anyAnimating ctx
            editing <- textFieldActive ctx
            wasAnimWait <- readIORef wasAnimating
            nFullWait <- readIORef startupFull
            pendingDirtyWait <- readIORef pendingRedraw
            dirtyWait <- isDirty ctx
            if animating || pendingDirtyWait || dirtyWait
              then pure emptySmallArray
              else
                if wasAnimWait || nFullWait > 0 || editing
                  then waitEventTimeout animateTimeout
                  else
                    if debugOpen
                      then waitEventTimeout debugHudTimeout
                      else waitEvent
      else pure queued
  wantDebug <- takeDebugLive (sdlDebug env) debugOpen
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
          wantFirstFull <- readIORef firstPointerFull
          let sizeChanged = inputWindowSize prevInp /= inputWindowSize inpSynced
              interacted = inputInteracted prevInp inpSynced
              displayScale = any (== EvDisplayScale) group
              userEvent = any isUserPresentEvent group
              firstUserFull = wantFirstFull && userEvent
              pointerEdge =
                inputMousePressed inpSynced
                  || inputMouseReleased inpSynced
                  || inputMouseRightPressed inpSynced
                  || inputMouseRightReleased inpSynced
              scrollEdge = inputScroll inpSynced /= V2 0 0
              graceAllow =
                grace <= 0 || sizeChanged || interacted || anim || editing || dirtyNow || pendingDirty || need || nFull > 0 || displayScale
              forceFinal = wasAnim && not anim
              shouldDraw =
                graceAllow
                  && ( need
                         || anim
                         || forceFinal
                         || pendingDirty
                         || dirtyNow
                         || wantDebug
                         || editing
                         || nFull > 0
                         || displayScale
                         || firstUserFull
                         || pointerEdge
                         || scrollEdge
                     )
          when (grace > 0) $ writeIORef startupGrace (grace - 1)
          let runDraw = do
                when firstUserFull $ writeIORef firstPointerFull False
                (dirtyOut, s) <-
                  drawFn
                    ctx'
                    env
                    inpSynced
                    (debugOpen || wantDebug || nFull > 0 || displayScale || firstUserFull)
                when (nFull > 0) $ writeIORef startupFull (nFull - 1)
                writeIORef pendingRedraw dirtyOut
                writeIORef prev s
                pure s
          synced <-
            if shouldDraw
              then do
                ms <- tryWithDrawingLock drawing runDraw
                case ms of
                  Just s -> pure s
                  Nothing
                    | pointerEdge || scrollEdge -> runDraw
                    | otherwise -> do
                        syncPointerCursor (sdlCursors env) ctx' inpSynced
                        pure inpSynced
              else do
                noteSkip (sdlDebug env)
                syncPointerCursor (sdlCursors env) ctx' inpSynced
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
                firstPointerFull
                shouldQuit
                synced
                rest
                now

isUserPresentEvent :: SdlEvent -> Bool
isUserPresentEvent ev =
  case ev of
    EvMouseMotion {} -> True
    EvMousePress {} -> True
    EvMouseRelease {} -> True
    EvMouseRightPress {} -> True
    EvMouseRightRelease {} -> True
    EvDisplayScale -> True
    EvResize {} -> True
    EvKey {} -> True
    EvText {} -> True
    EvScroll {} -> True
    _ -> False

tryWithDrawingLock :: IORef Bool -> IO a -> IO (Maybe a)
tryWithDrawingLock ref act = do
  ok <- atomicModifyIORef' ref $ \busy -> if busy then (True, False) else (True, True)
  if ok
    then Just <$> (act `finally` writeIORef ref False)
    else pure Nothing
