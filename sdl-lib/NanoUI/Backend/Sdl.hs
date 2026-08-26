-- | SDL3 backend: consumes geometry ('DrawData') from the core frame loop and
-- maps SDL events into 'Input'. Idle frames skip 'runFrame' until a command,
-- hover target change, 'markDirty', or an active animation demands a redraw.
-- Mouse motion on the same widget is ignored. Presents clip to a dirty rect
-- when only hover or animation changed (see design doc damage tracking).
module NanoUI.Backend.Sdl
  ( SdlEnv (..)
  , runSdlApp
  , runSdlAppWithQuit
  , runSdlAppWith
  , registerRgbaImage
  , SdlDebugSnapshot (..)
  , emptySdlDebug
  , readSdlDebugEnv
  ) where

import Control.Exception (bracket, finally)
import Control.Monad (unless, void, when)
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
  , Damage (..)
  , damageIsEmpty
  , emptyInput
  , isDirty
  , needsRedraw
  , overlayConsumesQuit
  , registerImage
  , Rect (..)
  , rectIntersect
  , runFrame
  , Size (..)
  , takeDamage
  , textInputEditActive
  , themeWindow
  , V2 (..)
  )
import NanoUI.Sdl.Debug
  ( SdlDebugSnapshot (..)
  , emptySdlDebug
  , noteLoop
  , notePresent
  , noteSkip
  , readSdlDebug
  , takeDebugLive
  )
import NanoUI.Sdl.Cursor (syncPointerCursor)
import NanoUI.Sdl.Display
  ( installResizeWatch
  , queryMouseWindowPos
  , queryRendererName
  , queryWindowLogicalSize
  , retainBegin
  , retainBlit
  , retainBlitRect
  , retainCreate
  , retainDestroy
  , windowToLogicalCoords
  )
import NanoUI.Sdl.Event (SdlEvent (..))
import NanoUI.Sdl.Font (renderTextSpans)
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
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Foreign.Ptr (Ptr, nullPtr)
import qualified NanoUI.Sdl.Image as SdlImage
import NanoUI.Sdl.Render (renderDrawDataPass)
import NanoUI.Sdl.Window (SdlEnv (..), defaultWindowSize, syncDisplay, withSdl)
import SDL3.Sys.Bindgen.Blendmode (sDL_BLENDMODE_BLEND)
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import SDL3.Sys.Render (renderPresentSafe, setRenderDrawBlendModeSafe)

animateTimeout :: Int
animateTimeout = 16

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
    (_, synced0) <- draw ctx1 ui env inp0 True
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
              (_, s) <- draw ctx' ui env inpSynced True
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
  wantDebug <- takeDebugLive (sdlDebug env)
  pending <-
    if null queued
      then do
        polled <- pollEvents
        if not (null polled)
          then pure polled
          else do
            animating <- anyAnimating ctx
            if animating || wantDebug
              then waitEventTimeout animateTimeout
              else waitEvent
      else pure queued
  let (group, rest) = splitFrame pending
  editActive <- textInputEditActive ctx
  if any (== EvQuit) group || (any isHardQuit group && not editActive)
    then pure ()
    else do
      now <- getMonotonicTime
      let dt = realToFrac (now - lastT)
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
          let forceFinal = wasAnim && not anim
              shouldDraw =
                need
                  || anim
                  || forceFinal
                  || pendingDirty
                  || dirtyNow
                  || wantDebug
          writeIORef wasAnimating anim
          synced <-
            if shouldDraw
              then do
                ms <-
                  tryWithDrawingLock drawing $ do
                    (_, s) <- draw ctx' ui env inpSynced wantDebug
                    writeIORef pendingRedraw False
                    writeIORef prev s
                    pure s
                maybe (pure inpSynced) pure ms
              else do
                noteSkip (sdlDebug env)
                writeIORef prev inpSynced
                pure inpSynced
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

draw :: Context -> UI () -> SdlEnv -> Input -> Bool -> IO (Bool, Input)
draw ctx ui env inp forceFull = do
  scale <- readIORef (sdlScaleRef env)
  SdlImage.syncImageAtlas (sdlRenderer env) (sdlImages env) ctx
  t0 <- getMonotonicTime
  (_, _, drawData, dirtyAfterUi) <- runFrame ctx inp ui
  t1 <- getMonotonicTime
  syncPointerCursor (sdlCursors env) ctx inp
  dmg0 <- takeDamage ctx
  let damage = if forceFull then DamageFull else dmg0
  if damageIsEmpty damage
    then do
      notePresent (sdlDebug env) ((t1 - t0) * 1000) drawData
      pure (dirtyAfterUi, inp)
    else do
      let Size lw lh = inputWindowSize inp
          pw = max 1 (round (lw * scale))
          ph = max 1 (round (lh * scale))
      tex <- ensureRetain env pw ph
      okBegin <- retainBegin (sdlRenderer env) tex
      unless okBegin $ fail "SDL_SetRenderTarget(retain) failed"
      baseSpans <- collectTextSpans ctx
      overlaySpans <- collectOverlayTextSpans ctx inp
      font <- readIORef (sdlFontRef env)
      let clear = themeWindow (ctxTheme ctx)
          spansIn = filterSpans damage
      renderDrawDataPass (sdlRenderer env) scale (Just clear) drawData False (sdlImages env) damage
      renderTextSpans (sdlRenderer env) scale font (sdlTextCache env) (spansIn baseSpans)
      renderDrawDataPass (sdlRenderer env) scale Nothing drawData True (sdlImages env) damage
      renderTextSpans (sdlRenderer env) scale font (sdlTextCache env) (spansIn overlaySpans)
      okBlit <- presentRetain (sdlRenderer env) tex scale damage
      unless okBlit $ fail "SDL_RenderTexture(retain) failed"
      void $ renderPresentSafe (sdlRenderer env)
      notePresent (sdlDebug env) ((t1 - t0) * 1000) drawData
      pure (dirtyAfterUi, inp)

ensureRetain :: SdlEnv -> Int -> Int -> IO (Ptr ())
ensureRetain env w h = do
  (tex, ow, oh) <- readIORef (sdlRetain env)
  if tex /= nullPtr && ow == w && oh == h
    then pure tex
    else do
      retainDestroy tex
      tex' <- retainCreate (sdlRenderer env) w h
      when (tex' == nullPtr) $ fail "SDL_CreateTexture(retain) failed"
      writeIORef (sdlRetain env) (tex', w, h)
      pure tex'

filterSpans :: Damage -> [(Rect, a, b, c, Rect)] -> [(Rect, a, b, c, Rect)]
filterSpans DamageFull spans = spans
filterSpans (DamageClip clip) spans =
  filter (\(box, _, _, _, _) -> isJust (rectIntersect clip box)) spans

presentRetain :: Ptr SDL_Renderer -> Ptr () -> Float -> Damage -> IO Bool
presentRetain ren tex scale damage =
  case damage of
    DamageFull -> retainBlit ren tex
    DamageClip (Rect x y w h) -> do
      let px = x * scale
          py = y * scale
          pw = w * scale
          ph = h * scale
      retainBlitRect ren tex px py pw ph px py

readSdlDebugEnv :: SdlEnv -> IO SdlDebugSnapshot
readSdlDebugEnv env = do
  scale <- readIORef (sdlScaleRef env)
  name <- queryRendererName (sdlRenderer env)
  size <- queryWindowLogicalSize (sdlWindow env) scale
  mouse <- queryMouseWindowPos
  let pos = maybe (V2 0 0) (windowToLogicalCoords scale) mouse
  readSdlDebug (sdlDebug env) size pos (sdlFontPath env) scale name

