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
  , rectH
  , rectW
  , rectX
  , rectY
  , themeWindow
  )
import Effectful (Eff, IOE, type (:>))
import NanoUI.Testing
  ( Context
  , Damage (..)
  , DrawData
  , Layer (..)
  , Ui
  , ctxTheme
  , damageIsEmpty
  , runEff
  , runFrameEff
  , runFrameReduceEff
  , takeDamage
  , askHost
  , uiIO
  )
import NanoUI.Testing (newPixelContext)
import NanoUI.Sdl.Session (runSdlSession)
import NanoUI.Sdl.Debug
  ( SdlDebugSnapshot (..)
  , emptySdlDebug
  , notePresent
  , noteSkip
  , readSdlDebug
  )
import NanoUI.Sdl.Cursor (syncPointerCursor)
import NanoUI.Sdl.Display
  ( backbufferPersists
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
import NanoUI.Sdl.Render (withRenderBatch)
import NanoUI.Sdl.Font (fontSourceLabel, glyphAtlasTexture)
import NanoUI.Sdl.Window (SdlEnv (..))
import Foreign.Ptr (Ptr, nullPtr)
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import qualified NanoUI.Sdl.Image as SdlImage
import NanoUI.Sdl.Render (renderDrawDataPass, snapDamage)
import SDL3.Sys.Render (renderPresentSafe)

newSdlContext :: IO Context
newSdlContext = newPixelContext

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
  (tex, retainNew) <- ensureRetain env pw ph scale
  -- Frame damage from writeDamage is authoritative: a live animation whose
  -- key is out of view or scroll-clipped produces empty damage, and forcing
  -- DamageFull here would turn every skip frame into a full present. A
  -- window redraw event (expose/restore) is the exception: the backbuffer
  -- is gone, so the next present must be full.
  let damage0 =
        if forceFull || retainNew || sdlContinuous env || inputWindowRedraw inp
          then DamageFull
          else snapDamage scale dmg0
      damage = damage0
  writeIORef (sdlLastPresented env) False
  if damageIsEmpty damage || lw <= 0 || lh <= 0
    then do
      noteSkip (sdlDebug env)
      pure (dirtyAfterUi, inp)
    else do
      okBegin <- retainBegin (sdlRenderer env) tex scale
      unless okBegin $ fail "SDL_SetRenderTarget(retain) failed"
      theme <- readIORef (ctxTheme ctx)
      glyphTex <- glyphAtlasTexture (sdlGlyphAtlas env)
      withRenderBatch (sdlRenderer env) $ \batch ->
        -- Skip the render clear when the retain texture already holds valid
        -- content from a previous present (retainNew == False).  The draw
        -- commands overwrite every pixel of the DamageFull clip, and for
        -- DamageClip the undamaged region keeps its old content.  Clearing
        -- to themeWindow before drawing caused a visible dark flash on the
        -- software renderer because the cleared texture could briefly reach
        -- the display before the draw commands completed.
        renderDrawDataPass batch (sdlRenderer env) scale (if retainNew then Just (themeWindow theme) else Nothing) drawData allLayersArr (sdlImages env) glyphTex damage
      t2 <- getMonotonicTime
      okBlit <- blitRetain (sdlRenderer env) scale tex damage
      unless okBlit $ fail "SDL_RenderTexture(retain) failed"
      void $ renderPresentSafe (sdlRenderer env)
      t3 <- getMonotonicTime
      let renderMs = (t2 - t1) * 1000
          presentMs = (t3 - t2) * 1000
          frameMs = (t3 - t0) * 1000
      notePresent (sdlDebug env) uiMs renderMs presentMs frameMs drawData
      writeIORef (sdlLastPresented env) True
      pure (dirtyAfterUi, inp)

ensureRetain :: SdlEnv -> Int -> Int -> Float -> IO (Ptr (), Bool)
ensureRetain env w h scale = do
  (tex, ow, oh, oldScale) <- readIORef (sdlRetain env)
  let scaleChanged = abs (oldScale - scale) > 0.001
  if tex /= nullPtr && ow == w && oh == h
    then do
      when scaleChanged $ writeIORef (sdlRetain env) (tex, w, h, scale)
      -- Same pixel size after a DPI change still holds the old present.
      pure (tex, scaleChanged)
    else do
      retainDestroy tex
      tex' <- retainCreate (sdlRenderer env) w h
      when (tex' == nullPtr) $ fail "SDL_CreateTexture(retain) failed"
      writeIORef (sdlRetain env) (tex', w, h, scale)
      pure (tex', True)

blitRetain :: Ptr SDL_Renderer -> Float -> Ptr () -> Damage -> IO Bool
blitRetain ren scale tex damage = do
  persists <- backbufferPersists ren
  if not persists
    then retainBlit ren tex
    else case damage of
      DamageFull -> retainBlit ren tex
      -- Blit only the damaged region: the retain texture and the
      -- backbuffer are both device-pixel sized, so the snapped clip
      -- (outward, in device pixels) maps 1:1 from src to dst. Safe only
      -- because the software backbuffer persists across presents.
      DamageClip r ->
        let s = if scale > 0 then scale else 1
            x0 = fromIntegral (floor (rectX r * s) :: Int) :: Float
            y0 = fromIntegral (floor (rectY r * s) :: Int) :: Float
            x1 = fromIntegral (ceiling ((rectX r + rectW r) * s) :: Int) :: Float
            y1 = fromIntegral (ceiling ((rectY r + rectH r) * s) :: Int) :: Float
         in retainBlitRect ren tex x0 y0 (x1 - x0) (y1 - y0) x0 y0

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
      refreshHz = round (1 / sdlRefreshPeriod env)
  readSdlDebug (sdlDebug env) size pos (fontSourceLabel (sdlFontSource env)) scale name (sdlVsync env) refreshHz
