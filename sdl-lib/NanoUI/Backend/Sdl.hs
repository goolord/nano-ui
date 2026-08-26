-- | SDL3 backend: consumes geometry ('DrawData') from the core frame loop and
-- maps SDL events into 'Input'. Idle frames skip 'runFrame' until input,
-- 'markDirty', or an active animation demands a redraw (damage tracking; see design doc).
module NanoUI.Backend.Sdl
  ( SdlEnv (..)
  , runSdlApp
  , runSdlAppWithQuit
  , runSdlAppWith
  , registerRgbaImage
  ) where

import Control.Exception (bracket, finally)
import Control.Monad (unless, void)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Context
  , ImageId
  , Input (..)
  , UI
  , anyAnimating
  , collectTextSpans
  , collectOverlayTextSpans
  , ctxTheme
  , emptyInput
  , inputMousePos
  , isDirty
  , needsRedraw
  , overlayConsumesQuit
  , registerImage
  , runFrame
  , textInputEditActive
  , themeWindow
  )
import NanoUI.Sdl.Cursor (syncPointerCursor)
import NanoUI.Sdl.Event (SdlEvent (..))
import NanoUI.Sdl.Font (renderTextSpans)
import NanoUI.Sdl.Input
  ( applyEvent
  , clearEphemeral
  , isHardQuit
  , isHardQuitInput
  , pollEvents
  , splitFrame
  , waitEventTimeout
  )
import Data.ByteString (ByteString)
import qualified NanoUI.Sdl.Image as SdlImage
import NanoUI.Sdl.Render (renderDrawDataPass)
import NanoUI.Sdl.Display (installResizeWatch)
import NanoUI.Sdl.Window (SdlEnv (..), defaultWindowSize, syncDisplay, withSdl)
import SDL3.Sys.Bindgen.Blendmode (sDL_BLENDMODE_BLEND)
import SDL3.Sys.Render (renderPresentSafe, setRenderDrawBlendModeSafe)

animateTimeout :: Int
animateTimeout = 16

idleTimeout :: Int
idleTimeout = 250

runSdlApp :: Context -> UI () -> IO ()
runSdlApp ctx ui = runSdlAppWithQuit ctx (const False) ui

runSdlAppWithQuit :: Context -> (Input -> Bool) -> UI () -> IO ()
runSdlAppWithQuit ctx shouldQuit ui = runSdlAppWith ctx (const (pure ())) shouldQuit ui

registerRgbaImage :: Context -> ImageId -> Int -> Int -> ByteString -> IO Bool
registerRgbaImage = registerImage

runSdlAppWith :: Context -> (SdlEnv -> IO ()) -> (Input -> Bool) -> UI () -> IO ()
runSdlAppWith ctx setup shouldQuit ui =
  withSdl ctx "nano-ui" defaultWindowSize $ \ctx0 env -> do
    setup env
    void $ setRenderDrawBlendModeSafe (sdlRenderer env) (fromIntegral sDL_BLENDMODE_BLEND)
    now <- getMonotonicTime
    (ctx1, inp0) <- syncDisplay ctx0 env emptyInput
    prev <- newIORef inp0
    pendingRedraw <- newIORef False
    wasAnimating <- newIORef False
    (_, synced0) <- draw ctx1 ui env inp0
    writeIORef pendingRedraw False
    writeIORef prev synced0
    ctxRef <- newIORef ctx1
    drawing <- newIORef False
    let onResize = do
          void $
            tryWithDrawingLock drawing $ do
              liveCtx <- readIORef ctxRef
              inp <- readIORef prev
              (ctx', inpSynced) <- syncDisplay liveCtx env (clearEphemeral inp)
              (_, s) <- draw ctx' ui env inpSynced
              writeIORef ctxRef ctx'
              writeIORef prev s
    bracket (installResizeWatch onResize) id $ \_ ->
      loop ctxRef ui env prev pendingRedraw wasAnimating drawing shouldQuit synced0 [] now

loop ::
  IORef Context ->
  UI () ->
  SdlEnv ->
  IORef Input ->
  IORef Bool ->
  IORef Bool ->
  IORef Bool ->
  (Input -> Bool) ->
  Input ->
  [SdlEvent] ->
  Double ->
  IO ()
loop ctxRef ui env prev pendingRedraw wasAnimating drawing shouldQuit inp queued lastT = do
  ctx <- readIORef ctxRef
  pending <-
    if null queued
      then do
        polled <- pollEvents
        if not (null polled)
          then pure polled
          else do
            animating <- anyAnimating ctx
            if animating
              then waitEventTimeout animateTimeout
              else waitEventTimeout idleTimeout
      else pure queued
  let (group, rest) = splitFrame pending
  editActive <- textInputEditActive ctx
  if any (== EvQuit) group || (any isHardQuit group && not editActive)
    then pure ()
    else do
      now <- getMonotonicTime
      let dt = realToFrac (now - lastT)
          inp' =
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
          let forceFinal = wasAnim && not anim
              mouseMoved = inputMousePos prevInp /= inputMousePos inpSynced
              shouldDraw =
                need
                  || anim
                  || forceFinal
                  || pendingDirty
                  || dirtyNow
                  || not (null group)
              cursorOnly = mouseMoved && not shouldDraw
          writeIORef wasAnimating anim
          synced <-
            if shouldDraw
              then do
                ms <-
                  tryWithDrawingLock drawing $ do
                    (_, s) <- draw ctx' ui env inpSynced
                    writeIORef pendingRedraw False
                    writeIORef prev s
                    pure s
                maybe (pure inpSynced) pure ms
              else if cursorOnly
                then do
                  s <- syncCursorFrame ctx' ui env inpSynced
                  writeIORef prev s
                  pure s
                else pure inpSynced
          overlayQuit <- overlayConsumesQuit ctx' inpSynced
          unless (shouldQuit inpSynced && not overlayQuit) $
            if null rest
              then loop ctxRef ui env prev pendingRedraw wasAnimating drawing shouldQuit synced [] now
              else loop ctxRef ui env prev pendingRedraw wasAnimating drawing shouldQuit synced rest now

tryWithDrawingLock :: IORef Bool -> IO a -> IO (Maybe a)
tryWithDrawingLock ref act = do
  ok <- atomicModifyIORef' ref $ \busy -> if busy then (True, False) else (True, True)
  if ok
    then Just <$> (act `finally` writeIORef ref False)
    else pure Nothing

draw :: Context -> UI () -> SdlEnv -> Input -> IO (Bool, Input)
draw ctx ui env inp = do
  scale <- readIORef (sdlScaleRef env)
  SdlImage.syncImageAtlas (sdlRenderer env) (sdlImages env) ctx
  (_, _, drawData, dirtyAfterUi) <- runFrame ctx inp ui
  syncPointerCursor (sdlCursors env) ctx inp
  baseSpans <- collectTextSpans ctx
  overlaySpans <- collectOverlayTextSpans ctx inp
  font <- readIORef (sdlFontRef env)
  let clear = themeWindow (ctxTheme ctx)
  renderDrawDataPass (sdlRenderer env) scale (Just clear) drawData False (sdlImages env)
  renderTextSpans (sdlRenderer env) scale font (sdlTextCache env) baseSpans
  renderDrawDataPass (sdlRenderer env) scale Nothing drawData True (sdlImages env)
  renderTextSpans (sdlRenderer env) scale font (sdlTextCache env) overlaySpans
  void $ renderPresentSafe (sdlRenderer env)
  pure (dirtyAfterUi, inp)

syncCursorFrame :: Context -> UI () -> SdlEnv -> Input -> IO Input
syncCursorFrame ctx ui env inp = do
  _ <- runFrame ctx inp ui
  syncPointerCursor (sdlCursors env) ctx inp
  pure inp
