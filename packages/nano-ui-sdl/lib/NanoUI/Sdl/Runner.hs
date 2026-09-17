{-# LANGUAGE DataKinds #-}

-- | SDL3 draw path: retained damage updates or direct continuous presentation.
module NanoUI.Sdl.Runner
  ( sdlDrawFrame
  , drawReduceEff
  , askSdlDebug
  , setSdlUiFont
  ) where

import Control.Exception (finally, mask_)
import Control.Monad (unless, void, when)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Typeable (Typeable)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Input (..)
  , NanoUI
  , Size (..)
  , themeWindow
  )
import Effectful (Eff, IOE, type (:>))
import NanoUI.Testing
  ( Context
  , Damage (..)
  , DrawData
  , Ui
  , askHost
  , ctxPaintFull
  , ctxTheme
  , damageFull
  , damageIsEmpty
  , markDirty
  , runEff
  , runFrameEff
  , runFrameReduceEff
  , takeDamage
  , uiIO
  )
import NanoUI.Sdl.Debug
  ( SdlDebugSnapshot
  , emptySdlDebug
  , notePresent
  , noteSkip
  , readSdlDebug
  )
import NanoUI.Sdl.Cursor (syncPointerCursor)
import NanoUI.Sdl.Display (queryMouseWindowPos, queryWindowLogicalSize)
import NanoUI.Sdl.Font
  ( fontSourceLabel
  , glyphAtlasTexture
  , sdlFontCacheSource
  , prepareGlyphAtlasForFrame
  , takeGlyphAtlasResetFlag
  )
import NanoUI.Sdl.NanoUIFont (NanoUIFont)
import NanoUI.Sdl.Render (flushRenderBatch, renderDrawDataPass, snapDamage)
import NanoUI.Sdl.Window (SdlEnv (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import qualified NanoUI.Sdl.Image as SdlImage
import SDL3.Sys.Bindgen.Blendmode (sDL_BLENDMODE_NONE)
import SDL3.Sys.Bindgen.Pixels (data SDL_PIXELFORMAT_RGBA32)
import SDL3.Sys.Bindgen.Render (SDL_Texture, data SDL_TEXTUREACCESS_TARGET)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Render
  ( createTexture
  , destroyTexture
  , getRenderOutputSize
  , renderPresentSafe
  , renderTexture
  , setRenderClipRect
  , setRenderScale
  , setRenderTarget
  , setTextureBlendMode
  )

sdlDrawFrame :: Context -> NanoUI () -> SdlEnv -> Input -> Bool -> IO (Bool, Input)
sdlDrawFrame ctx ui env inp forceFull =
  drawFrameWith ctx env inp forceFull $ do
    (_, _, drawData, dirtyAfterUi) <- runFrameEff runEff ctx inp ui
    pure (drawData, dirtyAfterUi)

-- | Both application styles share atlas maintenance, retain preparation,
-- timing, and presentation. Only evaluation of the UI differs.
drawFrameWith :: Context -> SdlEnv -> Input -> Bool -> IO (DrawData, Bool) -> IO (Bool, Input)
drawFrameWith ctx env inp forceFull evaluateUi = do
  SdlImage.syncImageAtlas (sdlRenderer env) (sdlImages env) ctx
  (tex, presentFull) <- prepareRetain ctx env inp forceFull
  t0 <- getMonotonicTime
  (drawData, dirtyAfterUi) <- evaluateUi
  t1 <- getMonotonicTime
  finishDraw ctx env inp tex presentFull t0 t1 drawData dirtyAfterUi

-- | Choose the render target and whether to repaint everything. Must run
-- before the frame so paint can cull to damage for retained partial updates.
prepareRetain :: Context -> SdlEnv -> Input -> Bool -> IO (Ptr SDL_Texture, Bool)
prepareRetain ctx env inp forceFull = do
  -- Glyph-atlas maintenance before any quad is recorded: if the atlas ran
  -- out of space during the previous frame, reset it now (re-warming the
  -- base fonts) so a reset can never wipe the texture underneath
  -- already-recorded text mid-frame.
  prepareGlyphAtlasForFrame (sdlGlyphAtlas env)
  scale <- readIORef (sdlScaleRef env)
  let Size lw lh = inputWindowSize inp
      pw = max 1 (round (lw * scale))
      ph = max 1 (round (lh * scale))
  -- Continuous sessions repaint every pixel, so retaining and copying a
  -- second framebuffer only adds a target switch and a full-window blit.
  -- A null target selects the window backbuffer directly. Direct drawing is
  -- equivalent to the retained blit only at the same pixel dimensions;
  -- content scale and window pixel density can differ.
  direct <-
    if sdlContinuous env
      then
        alloca $ \wp ->
          alloca $ \hp -> do
            ok <- getRenderOutputSize (sdlRenderer env) wp hp
            ow <- peek wp
            oh <- peek hp
            pure (ok && fromIntegral ow == pw && fromIntegral oh == ph)
      else pure False
  (tex, retainNew) <-
    if direct
      then pure (nullPtr, False)
      else ensureRetain env pw ph scale
  let presentFull = forceFull || retainNew || sdlContinuous env || inputWindowRedraw inp
  writeIORef (ctxPaintFull ctx) presentFull
  pure (tex, presentFull)

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
drawReduceEff unlift update modelRef view ctx env inp forceFull =
  drawFrameWith ctx env inp forceFull $ do
    m <- readIORef modelRef
    (_, m', _, drawData, dirtyAfterUi) <- runFrameReduceEff unlift update ctx inp m view
    writeIORef modelRef m'
    pure (drawData, dirtyAfterUi)

finishDraw :: Context -> SdlEnv -> Input -> Ptr SDL_Texture -> Bool -> Double -> Double -> DrawData -> Bool -> IO (Bool, Input)
finishDraw ctx env inp tex presentFull t0 t1 drawData dirtyAfterUi = do
  let uiMs = (t1 - t0) * 1000
  scale <- readIORef (sdlScaleRef env)
  syncPointerCursor (sdlCursors env) ctx inp
  dmg0 <- takeDamage ctx
  let Size lw lh = inputWindowSize inp
  -- Frame damage from writeDamage is authoritative: a live animation whose
  -- key is out of view or scroll-clipped produces empty damage, and forcing
  -- DamageFull here would turn every skip frame into a full present. A
  -- window redraw event (expose/restore) is the exception: the backbuffer
  -- is gone, so the next present must be full.
  let damage =
        if presentFull
          then DamageFull
          else snapDamage scale dmg0
  writeIORef (sdlLastPresented env) False
  -- A glyph-atlas reset or exhaustion during the UI pass means quads
  -- recorded before that point hold stale (or unplaceable) UVs. Drop the
  -- frame instead of presenting it: the screen keeps the previous valid
  -- frame, 'damageFull' forces a full repaint, and
  -- 'prepareGlyphAtlasForFrame' resets the atlas before the next frame
  -- records any quads, so text never flickers or vanishes for a frame.
  atlasReset <- takeGlyphAtlasResetFlag (sdlGlyphAtlas env)
  if atlasReset || damageIsEmpty damage || lw <= 0 || lh <= 0
    then do
      when atlasReset $ do
        damageFull ctx
        markDirty ctx
      noteSkip (sdlDebug env)
      pure (atlasReset || dirtyAfterUi, inp)
    else do
      -- A null texture draws full-repaint sessions straight to the window.
      okBegin <- setRenderTarget (sdlRenderer env) tex
      okScale <- setRenderScale (sdlRenderer env) scale scale
      unless (okBegin && okScale) $ fail "SDL_SetRenderTarget/Scale failed"
      theme <- readIORef (ctxTheme ctx)
      glyphTex <- glyphAtlasTexture (sdlGlyphAtlas env)
      -- Persistent batch created once per session (sdlBatch): no C
      -- calloc/free pair per presented frame. Flush unconditionally so an
      -- aborted pass cannot leak pending geometry into the next frame.
      --
      -- Full repaints clear the target, including bare backdrop regions.
      -- Partial updates preserve the undamaged part of the retained texture.
      let batch = sdlBatch env
      renderDrawDataPass
        batch
        (sdlRenderer env)
        (if damage == DamageFull then Just (themeWindow theme) else Nothing)
        drawData
        (sdlImages env)
        glyphTex
        damage
        `finally` flushRenderBatch batch
      t2 <- getMonotonicTime
      -- Damage limits updates to the retained texture, not the final copy:
      -- SDL leaves the window backbuffer undefined after each present.
      -- Restore the window's pixel coordinate system before polling events.
      -- Retained sessions do this as part of their final texture copy.
      okBlit <-
        if tex == nullPtr
          then setRenderScale (sdlRenderer env) 1 1
          else do
            okTarget <- setRenderTarget (sdlRenderer env) nullPtr
            okClip <- setRenderClipRect (sdlRenderer env) (PtrConst.unsafeFromPtr nullPtr)
            void $ setRenderScale (sdlRenderer env) 1 1
            okCopy <- renderTexture (sdlRenderer env) tex (PtrConst.unsafeFromPtr nullPtr) (PtrConst.unsafeFromPtr nullPtr)
            pure (okTarget && okClip && okCopy)
      unless okBlit $ fail "SDL window presentation preparation failed"
      void $ renderPresentSafe (sdlRenderer env)
      t3 <- getMonotonicTime
      let renderMs = (t2 - t1) * 1000
          presentMs = (t3 - t2) * 1000
          frameMs = (t3 - t0) * 1000
      notePresent (sdlDebug env) uiMs renderMs presentMs frameMs drawData
      writeIORef (sdlLastPresented env) True
      pure (dirtyAfterUi, inp)

ensureRetain :: SdlEnv -> Int -> Int -> Float -> IO (Ptr SDL_Texture, Bool)
ensureRetain env w h scale = do
  (tex, ow, oh, oldScale) <- readIORef (sdlRetain env)
  let scaleChanged = abs (oldScale - scale) > 0.001
  if tex /= nullPtr && ow == w && oh == h
    then do
      when scaleChanged $ writeIORef (sdlRetain env) (tex, w, h, scale)
      -- Same pixel size after a DPI change still holds the old present.
      pure (tex, scaleChanged)
    else mask_ $ do
      -- Allocate before replacing: failure leaves the owned texture valid.
      tex' <- createTexture (sdlRenderer env) SDL_PIXELFORMAT_RGBA32 SDL_TEXTUREACCESS_TARGET (fromIntegral w) (fromIntegral h)
      when (tex' == nullPtr) $ fail "SDL_CreateTexture(retain) failed"
      void $ setTextureBlendMode tex' (fromIntegral sDL_BLENDMODE_NONE)
      writeIORef (sdlRetain env) (tex', w, h, scale)
      unless (tex == nullPtr) $ destroyTexture tex
      pure (tex', True)

askSdlDebug :: Ui :> es => Eff es SdlDebugSnapshot
askSdlDebug = do
  menv <- askHost @SdlEnv
  case menv of
    Nothing -> pure emptySdlDebug
    Just env -> uiIO (readSdlDebugEnv env)

-- | Request a UI font family. The SDL display thread resolves and applies it
-- before the next frame (see 'NanoUI.Sdl.Window.syncDisplay'), rebuilding the
-- glyph atlas and text resolver. A no-op on non-SDL hosts.
setSdlUiFont :: Ui :> es => NanoUIFont -> Eff es ()
setSdlUiFont font = do
  menv <- askHost @SdlEnv
  case menv of
    Nothing -> pure ()
    Just env -> uiIO $ do
      cur <- readIORef (sdlFontRequestRef env)
      when (cur /= font) $ writeIORef (sdlFontRequestRef env) font

readSdlDebugEnv :: SdlEnv -> IO SdlDebugSnapshot
readSdlDebugEnv env = do
  scale <- readIORef (sdlScaleRef env)
  fontSource <- sdlFontCacheSource (sdlFontCache env)
  size <- queryWindowLogicalSize (sdlWindow env)
  pos <- queryMouseWindowPos
  let refreshHz = round (1 / sdlRefreshPeriod env)
  readSdlDebug (sdlDebug env) size pos (fontSourceLabel fontSource) scale (sdlRendererName env) (sdlVsync env) refreshHz
