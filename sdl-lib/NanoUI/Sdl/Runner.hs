{-# LANGUAGE DataKinds #-}

-- | SDL3 draw path: frame execution and retain-texture present.
module NanoUI.Sdl.Runner
  ( newSdlContext
  , runSdlSession
  , sdlDrawFrame
  , drawEff
  , drawReduceEff
  , askSdlEnv
  , askSdlDebug
  , readSdlDebugEnv
  ) where

import Control.Monad (unless, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Typeable (Typeable)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Input (..)
  , NanoUI
  , Size (..)
  , themeWindow
  )
import NanoUI.Types (Rect (..), V2 (..), rectIntersect)
import Effectful (Eff, IOE, type (:>))
import NanoUI.Testing
  ( Context
  , Damage (..)
  , DrawData
  , Layer (..)
  , Ui
  , anyAnimating
  , collectRasterSpans
  , ctxTheme
  , damageIsEmpty
  , runEff
  , runFrameEff
  , runFrameReduceEff
  , takeDamage
  , askHost
  , uiIO
  )
import NanoUI.Sdl.Context (newSdlContext)
import NanoUI.Sdl.Session (runSdlSession)
import NanoUI.Sdl.Debug
  ( SdlDebugSnapshot (..)
  , emptySdlDebug
  , notePresent
  , readSdlDebug
  )
import NanoUI.Sdl.Cursor (syncPointerCursor)
import NanoUI.Sdl.Display
  ( queryMouseWindowPos
  , queryRendererName
  , queryWindowLogicalSize
  , retainBegin
  , retainBlit
  , retainBlitRect
  , retainCreate
  , retainDestroy
  , windowToLogicalCoords
  )
import NanoUI.Sdl.Batch (withRenderBatch)
import NanoUI.Sdl.Font (renderTextSpans)
import NanoUI.Sdl.Window (SdlEnv (..))
import Foreign.Ptr (Ptr, nullPtr)
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import qualified NanoUI.Sdl.Image as SdlImage
import NanoUI.Sdl.Render (renderDrawDataPass, snapDamage, clipPixelRect)
import Data.Maybe (isJust)
import SDL3.Sys.Render (renderPresentSafe)

sdlDrawFrame :: Context -> NanoUI () -> SdlEnv -> Input -> Bool -> IO (Bool, Input)
sdlDrawFrame ctx ui env inp forceFull = drawEff runEff ctx ui env inp forceFull

drawEff ::
  IOE :> es =>
  (forall x. Eff es x -> IO x) ->
  Context ->
  Eff (Ui : es) () ->
  SdlEnv ->
  Input ->
  Bool ->
  IO (Bool, Input)
drawEff unlift ctx ui env inp forceFull = do
  SdlImage.syncImageAtlas (sdlRenderer env) (sdlImages env) ctx
  t0 <- getMonotonicTime
  (_, _, drawData, dirtyAfterUi) <- runFrameEff unlift ctx inp ui
  finishDraw ctx env inp forceFull t0 drawData dirtyAfterUi

drawReduceEff ::
  (IOE :> es, Typeable msg, Eq model) =>
  (forall x. Eff es x -> IO x) ->
  (msg -> model -> model) ->
  IORef model ->
  (model -> Eff (Ui : es) ()) ->
  Context ->
  SdlEnv ->
  Input ->
  Bool ->
  IO (Bool, Input)
drawReduceEff unlift update modelRef view ctx env inp forceFull = do
  SdlImage.syncImageAtlas (sdlRenderer env) (sdlImages env) ctx
  t0 <- getMonotonicTime
  m <- readIORef modelRef
  (_, m', _, drawData, dirtyAfterUi) <- runFrameReduceEff unlift update ctx inp m view
  writeIORef modelRef m'
  finishDraw ctx env inp forceFull t0 drawData dirtyAfterUi

finishDraw :: Context -> SdlEnv -> Input -> Bool -> Double -> DrawData -> Bool -> IO (Bool, Input)
finishDraw ctx env inp forceFull t0 drawData dirtyAfterUi = do
  scale <- readIORef (sdlScaleRef env)
  t1 <- getMonotonicTime
  syncPointerCursor (sdlCursors env) ctx inp
  dmg0 <- takeDamage ctx
  let Size lw lh = inputWindowSize inp
      pw = max 1 (round (lw * scale))
      ph = max 1 (round (lh * scale))
  (tex, retainNew) <- ensureRetain env pw ph
  animating <- anyAnimating ctx
  let damage0 = if forceFull || retainNew then DamageFull else snapDamage scale dmg0
      damage =
        if damageIsEmpty damage0 && animating
          then DamageFull
          else damage0
  if damageIsEmpty damage || lw <= 0 || lh <= 0
    then do
      notePresent (sdlDebug env) ((t1 - t0) * 1000) drawData
      pure (dirtyAfterUi, inp)
    else do
      okBegin <- retainBegin (sdlRenderer env) tex
      unless okBegin $ fail "SDL_SetRenderTarget(retain) failed"
      (baseSpans, overlaySpans) <- collectRasterSpans ctx inp
      font <- readIORef (sdlFontRef env)
      monoFont <- readIORef (sdlMonoFontRef env)
      let clear = themeWindow (ctxTheme ctx)
          spansIn = filterSpans damage
      withRenderBatch (sdlRenderer env) $ \batch -> do
        renderDrawDataPass batch (sdlRenderer env) scale (Just clear) drawData [LayerBackground] (sdlImages env) damage
        renderTextSpans batch (sdlRenderer env) scale font monoFont (sdlTextCache env) (spansIn baseSpans)
        renderDrawDataPass batch (sdlRenderer env) scale Nothing drawData [LayerContent] (sdlImages env) damage
        renderDrawDataPass batch (sdlRenderer env) scale Nothing drawData [LayerOverlay] (sdlImages env) damage
        renderTextSpans batch (sdlRenderer env) scale font monoFont (sdlTextCache env) (spansIn overlaySpans)
        renderDrawDataPass batch (sdlRenderer env) scale Nothing drawData [LayerChrome] (sdlImages env) damage
      okBlit <- blitRetain (sdlRenderer env) scale tex damage
      unless okBlit $ fail "SDL_RenderTexture(retain) failed"
      void $ renderPresentSafe (sdlRenderer env)
      notePresent (sdlDebug env) ((t1 - t0) * 1000) drawData
      pure (dirtyAfterUi, inp)

ensureRetain :: SdlEnv -> Int -> Int -> IO (Ptr (), Bool)
ensureRetain env w h = do
  (tex, ow, oh) <- readIORef (sdlRetain env)
  if tex /= nullPtr && ow == w && oh == h
    then pure (tex, False)
    else do
      retainDestroy tex
      tex' <- retainCreate (sdlRenderer env) w h
      when (tex' == nullPtr) $ fail "SDL_CreateTexture(retain) failed"
      writeIORef (sdlRetain env) (tex', w, h)
      pure (tex', True)

filterSpans :: Damage -> [(Rect, a, b, c, Rect)] -> [(Rect, a, b, c, Rect)]
filterSpans DamageFull spans = spans
filterSpans (DamageClip clip) spans =
  filter (\(box, _, _, _, _) -> isJust (rectIntersect clip box)) spans

blitRetain :: Ptr SDL_Renderer -> Float -> Ptr () -> Damage -> IO Bool
blitRetain ren scale tex damage =
  case damage of
    DamageFull -> retainBlit ren tex
    DamageClip r ->
      let (px, py, pw, ph) = clipPixelRect scale r
       in retainBlitRect ren tex px py pw ph px py

askSdlEnv :: Ui :> es => Eff es (Maybe SdlEnv)
askSdlEnv = askHost

askSdlDebug :: Ui :> es => Eff es SdlDebugSnapshot
askSdlDebug = do
  menv <- askSdlEnv
  case menv of
    Nothing -> pure emptySdlDebug
    Just env -> uiIO (readSdlDebugEnv env)

readSdlDebugEnv :: SdlEnv -> IO SdlDebugSnapshot
readSdlDebugEnv env = do
  scale <- readIORef (sdlScaleRef env)
  name <- queryRendererName (sdlRenderer env)
  size <- queryWindowLogicalSize (sdlWindow env) scale
  mouse <- queryMouseWindowPos
  let pos = maybe (V2 0 0) (windowToLogicalCoords scale) mouse
  readSdlDebug (sdlDebug env) size pos (sdlFontPath env) scale name
