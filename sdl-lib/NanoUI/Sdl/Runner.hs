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
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Primitive.SmallArray (SmallArray, smallArrayFromListN)
import Data.Typeable (Typeable)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Input (..)
  , NanoUI
  , Size (..)
  , V2 (..)
  , themeWindow
  )
import Effectful (Eff, IOE, type (:>))
import NanoUI.Testing
  ( Context
  , Damage (..)
  , DrawData
  , Layer (..)
  , Ui
  , anyAnimating
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
import NanoUI.Sdl.Font (glyphAtlasTexture)
import NanoUI.Sdl.Window (SdlEnv (..))
import Foreign.Ptr (Ptr, nullPtr)
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import qualified NanoUI.Sdl.Image as SdlImage
import NanoUI.Sdl.Render (renderDrawDataPass, snapDamage, clipPixelRect)
import SDL3.Sys.Render (renderPresentSafe)

allLayersArr :: SmallArray Layer
allLayersArr = smallArrayFromListN 4 [LayerBackground, LayerContent, LayerOverlay, LayerChrome]

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
  t1 <- getMonotonicTime
  finishDraw ctx env inp forceFull t0 t1 drawData dirtyAfterUi

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
  t1 <- getMonotonicTime
  finishDraw ctx env inp forceFull t0 t1 drawData dirtyAfterUi

finishDraw :: Context -> SdlEnv -> Input -> Bool -> Double -> Double -> DrawData -> Bool -> IO (Bool, Input)
finishDraw ctx env inp forceFull t0 t1 drawData dirtyAfterUi = do
  let uiMs = (t1 - t0) * 1000
  scale <- readIORef (sdlScaleRef env)
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
      tEnd <- getMonotonicTime
      let renderMs = 0
          presentMs = 0
          frameMs = (tEnd - t0) * 1000
      notePresent (sdlDebug env) uiMs renderMs presentMs frameMs drawData
      pure (dirtyAfterUi, inp)
    else do
      okBegin <- retainBegin (sdlRenderer env) tex scale
      unless okBegin $ fail "SDL_SetRenderTarget(retain) failed"
      let clear = themeWindow (ctxTheme ctx)
      glyphTex <- glyphAtlasTexture (sdlGlyphAtlas env)
      withRenderBatch (sdlRenderer env) $ \batch ->
        renderDrawDataPass batch (sdlRenderer env) scale (Just clear) drawData allLayersArr (sdlImages env) glyphTex damage
      t2 <- getMonotonicTime
      okBlit <- blitRetain (sdlRenderer env) scale tex damage
      unless okBlit $ fail "SDL_RenderTexture(retain) failed"
      void $ renderPresentSafe (sdlRenderer env)
      t3 <- getMonotonicTime
      let renderMs = (t2 - t1) * 1000
          presentMs = (t3 - t2) * 1000
          frameMs = (t3 - t0) * 1000
      notePresent (sdlDebug env) uiMs renderMs presentMs frameMs drawData
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
  readSdlDebug (sdlDebug env) size pos (sdlFontPath env) scale name (sdlVsync env)
