-- | SDL3 draw path: retained damage updates or direct continuous presentation.
module NanoUI.Sdl.Internal.Runner
  ( sdlDrawFrame
  , drawFrameWith
  , askSdlDebug
  , setSdlUiFont
  , setSdlUiScale
  ) where

import Control.Exception (finally, mask_)
import Control.Monad (unless, void, when)
import Data.Foldable (traverse_)
import Data.IORef (readIORef, writeIORef)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Input (..)
  , NanoUI
  , Size (..)
  , V2 (..)
  , themeWindow
  )
import Effectful (Eff, type (:>))
import NanoUI.Testing
  ( Context
  , Damage (..)
  , DrawData (..)
  , Ui
  , askHost
  , ctxPaintFull
  , ctxTheme
  , damageFull
  , damageIsEmpty
  , drawCmdCount
  , markDirty
  , runFrame
  , takeDamage
  , uiIO
  )
import NanoUI.Internal.Debug (CoreDebugSnapshot (..), noteDebugPresent, noteDebugSkip, refreshDebugSnapshot)
import NanoUI.Sdl.Internal.Debug
  ( SdlDebugSampler (..)
  , SdlDebugSnapshot (..)
  , emptySdlDebug
  , traceFrame
  )
import NanoUI.Sdl.Internal.Display (outPair, pushRefreshEvent, queryMouseWindowPos, queryWindowLogicalSize)
import NanoUI.Sdl.Internal.Font
  ( fontSourceLabel
  , glyphAtlasTextures
  , sdlFontCacheSource
  , prepareGlyphAtlasForFrame
  , glyphAtlasFull
  )
import NanoUI.Sdl.Internal.NanoUIFont (NanoUIFont)
import NanoUI.Sdl.Internal.Render (flushRenderBatch, renderDrawDataPass, snapDamage)
import NanoUI.Sdl.Internal.Window (Retain (..), SdlEnv (..))
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)
import qualified NanoUI.Sdl.Internal.Image as SdlImage
import SDL3.Sys.Bindgen.Blendmode (sDL_BLENDMODE_NONE)
import SDL3.Sys.Bindgen.Pixels (data SDL_PIXELFORMAT_RGBA32)
import SDL3.Sys.Bindgen.Rect (SDL_FRect (..))
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

-- | Build, draw, and present one frame on the display thread. The final flag
-- forces a full repaint. Returns whether another frame is needed. Use the
-- context/input returned by @syncDisplay@.
sdlDrawFrame :: Context -> NanoUI () -> SdlEnv -> Input -> Bool -> IO Bool
sdlDrawFrame ctx ui env inp forceFull =
  drawFrameWith ctx env inp forceFull $ do
    (_, _, drawData, dirtyAfterUi) <- runFrame ctx inp ui
    pure (drawData, dirtyAfterUi)

-- | Draw and present a frame whose UI pass is the given action, which answers
-- the draw data and whether the context needs another frame. Both
-- application styles share atlas maintenance, retain preparation, timing,
-- and presentation; only evaluation of the UI differs.
drawFrameWith :: Context -> SdlEnv -> Input -> Bool -> IO (DrawData, Bool) -> IO Bool
drawFrameWith ctx env inp forceFull evaluateUi = do
  SdlImage.syncImageAtlas ren (sdlImages env) ctx
  -- Glyph-atlas maintenance before any quad is recorded: if the atlas ran
  -- out of space during the previous frame, reset it now (re-warming the
  -- base fonts) so a reset can never wipe the texture underneath
  -- already-recorded text mid-frame.
  prepareGlyphAtlasForFrame (sdlFontCache env)
  scale <- readIORef (sdlScaleRef env)
  let Size lw lh = inputWindowSize inp
      pw = max 1 (round (lw * scale))
      ph = max 1 (round (lh * scale))
  -- The render target and whether to repaint everything are chosen before
  -- the UI pass, so paint can cull to damage for retained partial updates.
  --
  -- Continuous sessions repaint every pixel, so retaining and copying a
  -- second framebuffer only adds a target switch and a full-window blit.
  -- A null target selects the window backbuffer directly. Direct drawing is
  -- equivalent to the retained blit only at the same pixel dimensions;
  -- content scale and window pixel density can differ.
  direct <-
    if sdlContinuous env
      then do
        (ok, ow, oh) <- outPair (getRenderOutputSize ren)
        pure (ok && fromIntegral ow == pw && fromIntegral oh == ph)
      else pure False
  (tex, retainNew) <-
    if direct
      then pure (nullPtr, False)
      else ensureRetain env pw ph scale
  let presentFull = forceFull || retainNew || sdlContinuous env || inputWindowRedraw inp
  writeIORef (ctxPaintFull ctx) presentFull
  t0 <- getMonotonicTime
  (drawData, dirtyAfterUi) <- evaluateUi
  t1 <- getMonotonicTime
  dmg0 <- takeDamage ctx
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
  -- A glyph-atlas exhaustion during the UI pass means text quads the frame
  -- could not place. Drop the frame instead of presenting it: the screen
  -- keeps the previous valid frame, 'damageFull' forces a full repaint, and
  -- 'prepareGlyphAtlasForFrame' resets the atlas before the next frame
  -- records any quads, so text never flickers or vanishes for a frame.
  atlasReset <- glyphAtlasFull (sdlGlyphAtlas env)
  if atlasReset || damageIsEmpty damage || lw <= 0 || lh <= 0
    then do
      when atlasReset $ do
        damageFull ctx
        markDirty ctx
      noteDebugSkip (sdsSampler (sdlDebug env))
      pure (atlasReset || dirtyAfterUi)
    else do
      -- A null texture draws full-repaint sessions straight to the window.
      okBegin <- setRenderTarget ren tex
      okScale <- setRenderScale ren scale scale
      unless (okBegin && okScale) $ fail "SDL_SetRenderTarget/Scale failed"
      theme <- readIORef (ctxTheme ctx)
      glyphTex <- glyphAtlasTextures (sdlGlyphAtlas env)
      -- Persistent batch created once per session (sdlBatch): no C
      -- calloc/free pair per presented frame. Flush unconditionally so an
      -- aborted pass cannot leak pending geometry into the next frame.
      --
      -- Full repaints clear the target, including bare backdrop regions.
      -- Partial updates preserve the undamaged part of the retained texture.
      let batch = sdlBatch env
      renderDrawDataPass
        batch
        ren
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
          then setRenderScale ren 1 1
          else do
            okTarget <- setRenderTarget ren nullPtr
            okClip <- setRenderClipRect ren (PtrConst.unsafeFromPtr nullPtr)
            void $ setRenderScale ren 1 1
            -- The texture is larger than the window: copy only the used area.
            r <- readIORef (sdlRetain env)
            let src = SDL_FRect 0 0 (fromIntegral (retainW r)) (fromIntegral (retainH r))
            okCopy <- with src $ \srcP ->
              renderTexture ren tex (PtrConst.unsafeFromPtr srcP) (PtrConst.unsafeFromPtr nullPtr)
            pure (okTarget && okClip && okCopy)
      unless okBlit $ fail "SDL window presentation preparation failed"
      void $ renderPresentSafe ren
      t3 <- getMonotonicTime
      let ms a b = (b - a) * 1000
      noteDebugPresent (sdsSampler (sdlDebug env)) (ms t0 t1) (ms t1 t2) (ms t2 t3) (ms t0 t3)
        (drawVertexCount drawData) (drawIndexCount drawData) (drawCmdCount drawData)
      writeIORef (sdlLastPresented env) True
      pure dirtyAfterUi
  where
    ren = sdlRenderer env

-- | Pixels a retained texture is rounded up to, in each dimension.
retainBlock :: Int
retainBlock = 256

ensureRetain :: SdlEnv -> Int -> Int -> Float -> IO (Ptr SDL_Texture, Bool)
ensureRetain env w h scale = do
  r <- readIORef (sdlRetain env)
  let tex = retainTexture r
      fits = w <= retainCapW r && h <= retainCapH r
      -- Give memory back once the window is well inside the texture, but not
      -- for a shrink of a block or so, which a drag back out would undo.
      roomy = retainCapW r - w > 2 * retainBlock || retainCapH r - h > 2 * retainBlock
      -- A new used size or density holds none of the frame to be drawn.
      stale = w /= retainW r || h /= retainH r || abs (retainScale r - scale) > 0.001
  if tex /= nullPtr && fits && not roomy
    then do
      when stale $ writeIORef (sdlRetain env) r {retainW = w, retainH = h, retainScale = scale}
      pure (tex, stale)
    else mask_ $ do
      let cw = roundUp w
          ch = roundUp h
      -- Allocate before replacing: failure leaves the owned texture valid.
      tex' <- createTexture (sdlRenderer env) SDL_PIXELFORMAT_RGBA32 SDL_TEXTUREACCESS_TARGET (fromIntegral cw) (fromIntegral ch)
      when (tex' == nullPtr) $ fail "SDL_CreateTexture(retain) failed"
      void $ setTextureBlendMode tex' (fromIntegral sDL_BLENDMODE_NONE)
      writeIORef (sdlRetain env) (Retain tex' cw ch w h scale)
      unless (tex == nullPtr) $ destroyTexture tex
      pure (tex', True)
  where
    roundUp n = max retainBlock (((n + retainBlock - 1) `div` retainBlock) * retainBlock)

-- | Read debug information, refreshing at most four times per second.
-- Returns the empty snapshot outside an SDL session. Repeated queries keep
-- the debug sampler active and can schedule periodic frames.
askSdlDebug :: Ui :> es => Eff es SdlDebugSnapshot
askSdlDebug = askHost @SdlEnv >>= maybe (pure emptySdlDebug) (uiIO . sample)
  where
    sample env = do
      let sampler = sdlDebug env
      -- The display is queried only when the snapshot refreshes.
      refreshDebugSnapshot (sdsSampler sampler) (sdsSnapshot sampler) $ \core -> do
        scale <- readIORef (sdlScaleRef env)
        fontSource <- sdlFontCacheSource (sdlFontCache env)
        Size ww wh <- queryWindowLogicalSize (sdlWindow env)
        V2 mx my <- queryMouseWindowPos
        let snap =
              SdlDebugSnapshot
                { dbgCore = core {dbgWinW = ww, dbgWinH = wh, dbgMouseX = mx, dbgMouseY = my}
                , dbgScale = scale
                , dbgFontPath = fontSourceLabel fontSource
                , dbgRenderer = sdlRendererName env
                , dbgVsync = sdlVsync env
                , dbgRefreshHz = round (1 / sdlRefreshPeriod env)
                }
        when (sdsTrace sampler) (traceFrame snap)
        pure snap

-- | Request a UI font family. The SDL display thread resolves and applies it
-- before the next frame (see 'NanoUI.Sdl.Internal.Window.syncDisplay'), rebuilding the
-- glyph atlas and text resolver. A no-op on non-SDL hosts.
setSdlUiFont :: Ui :> es => NanoUIFont -> Eff es ()
setSdlUiFont font = askHost >>= traverse_ (uiIO . (`writeIORef` font) . sdlFontRequestRef)

-- | Set the UI scale (see 'NanoUI.Sdl.Internal.Window.sdlAppUiScale'): a zoom on top
-- of the pixel density, or zero or less to follow the display. The display
-- thread applies it before the next frame, which this wakes. A no-op on
-- non-SDL hosts.
setSdlUiScale :: Ui :> es => Float -> Eff es ()
setSdlUiScale s = askHost @SdlEnv >>= traverse_ (uiIO . request)
  where
    request env = do
      cur <- readIORef (sdlUiScaleRef env)
      when (cur /= s) $ do
        writeIORef (sdlUiScaleRef env) s
        pushRefreshEvent
