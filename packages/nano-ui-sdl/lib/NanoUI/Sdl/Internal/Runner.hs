-- | SDL3 draw path: retained damage updates or direct continuous presentation.
module NanoUI.Sdl.Internal.Runner
  ( sdlDrawFrame
  , drawFrameWith
  , askSdlDebug
  , sdlDebugSnapshot
  , setSdlUiFont
  , setSdlUiScale
  ) where

import Control.Exception (finally, mask_)
import Control.Monad (unless, void, when)
import Data.Foldable (for_)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import GHC.Clock (getMonotonicTime)
import NanoUI
import NanoUI.Backend (answerScreenshots, answerScreenshotsAfter)
import NanoUI.Testing
import NanoUI.Internal.Context (Context (ctxDamageWanted), markDirtyCovered)
import NanoUI.Internal.Debug (CoreDebugSnapshot (..), noteDebugPresent, noteDebugSkip, refreshDebugSnapshot)
import NanoUI.Sdl.Internal.Debug
import NanoUI.Sdl.Internal.Display (outPair, queryMouseWindowPos, queryWindowLogicalSize, scaleMoved, sendWaylandSizeLimits)
import NanoUI.Sdl.Internal.Font
import NanoUI.Sdl.Internal.Input (syncTextInput)
import NanoUI.Sdl.Internal.NanoUIFont (NanoUIFont)
import NanoUI.Sdl.Internal.Render (flushRenderBatch, glyphPageTexture, renderDrawDataPass, snapDamage)
import NanoUI.Sdl.Internal.Window (Retain (..), SdlEnv (..), captureFrame, windowZoom, withSdlEnv)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)
import qualified NanoUI.Sdl.Internal.Image as SdlImage
import SDL3.Sys.Bindgen.Blendmode (sDL_BLENDMODE_NONE)
import SDL3.Sys.Bindgen.Pixels qualified as Pixels
import SDL3.Sys.Bindgen.Rect (SDL_FRect (..))
import SDL3.Sys.Bindgen.Render (SDL_Texture)
import SDL3.Sys.Bindgen.Render qualified as Render
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
  drawFrameWith ctx env inp forceFull ((\(_, drawData, dirty) -> (drawData, dirty)) <$> runFrame ctx inp ui)

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
      -- A transparent window repaints in full on any change: a clip frame
      -- blends its backdrop over the old pixels, which would show through a
      -- translucent window colour.
      transparent = isJust (sdlTransparent env)
  -- The render target and whether to repaint everything are chosen before
  -- the UI pass, so paint can cull to damage for retained partial updates.
  --
  -- Continuous sessions repaint every pixel, so retaining and copying a
  -- second framebuffer only adds a target switch and a full-window blit.
  -- A null target selects the window backbuffer directly. Direct drawing is
  -- equivalent to the retained blit only at the same pixel dimensions;
  -- content scale and window pixel density can differ. A transparent window
  -- always draws retained: the copy to the window premultiplies its alpha.
  direct <-
    if sdlContinuous env && not transparent
      then do
        (ok, ow, oh) <- outPair (getRenderOutputSize ren)
        pure (ok && fromIntegral ow == pw && fromIntegral oh == ph)
      else pure False
  (tex, retainNew) <-
    if direct
      then pure (nullPtr, False)
      else ensureRetain env pw ph scale
  let presentFull = forceFull || retainNew || sdlContinuous env || inputWindowRedraw inp
  writeIORef (ctxPaintFull ctx) (presentFull || transparent)
  -- A full present ignores the frame's damage (below), so the frame need not
  -- work it out beyond what reusing its last draw takes. Frames run outside
  -- a draw still do, even when the pass throws.
  writeIORef (ctxDamageWanted ctx) (not presentFull)
  t0 <- getMonotonicTime
  (drawData, dirtyAfterUi) <- evaluateUi `finally` writeIORef (ctxDamageWanted ctx) True
  t1 <- getMonotonicTime
  -- Sync SDL text input with the focus and caret every frame. A skipped
  -- frame would look like a focus change mid-composition and drop it.
  zoom <- windowZoom env
  _ <- syncTextInput (sdlTextInput env) (sdlWindow env) zoom ctx inp
  dmg0 <- takeDamage ctx
  -- Frame damage from writeDamage is authoritative: a live animation whose
  -- key is out of view or scroll-clipped produces empty damage, and forcing
  -- DamageFull here would turn every skip frame into a full present. A
  -- window redraw event (expose/restore) is the exception: the backbuffer
  -- is gone, so the next present must be full.
  let damage
        | presentFull = DamageFull
        | transparent = if damageIsEmpty dmg0 then dmg0 else DamageFull
        | otherwise = snapDamage scale dmg0
  writeIORef (sdlLastPresented env) False
  -- A glyph-atlas exhaustion during the UI pass means text quads the frame
  -- could not place. Drop the frame instead of presenting it: the screen
  -- keeps the previous valid frame, 'damageFull' forces a full repaint, and
  -- 'prepareGlyphAtlasForFrame' resets the atlas before the next frame
  -- records any quads, so text never flickers or vanishes for a frame.
  atlasReset <- glyphAtlasFull (sdlFontCache env)
  if atlasReset || damageIsEmpty damage || lw <= 0 || lh <= 0
    then do
      when atlasReset $ do
        damageFull ctx
        markDirty ctx
      noteDebugSkip (sdlDebug env)
      -- With nothing to repaint, the retained frame on screen is this one.
      -- A dropped frame's screenshots wait for the next frame, which the
      -- reset requested.
      when (not atlasReset && damageIsEmpty damage && not direct) $
        answerScreenshots ctx (captureFrame env tex)
      pure (atlasReset || dirtyAfterUi)
    else do
      -- A null texture draws full-repaint sessions straight to the window.
      unlessM ((&&) <$> setRenderTarget ren tex <*> setRenderScale ren scale scale) $
        fail "SDL_SetRenderTarget/Scale failed"
      theme <- readIORef (ctxTheme ctx)
      let glyphs = glyphAtlasHandle (sdlFontCache env)
      -- A transparent window draws atlas textures with its own blend mode.
      -- Set it after the UI pass, which can create atlas textures.
      for_ (sdlTransparent env) $ \(blend, _) -> do
        textures <- (:) <$> SdlImage.lookupImage (sdlImages env) atlasTextureId <*> traverse (glyphPageTexture glyphs) [0 .. glyphAtlasPages - 1]
        for_ (filter (/= nullPtr) textures) (`setTextureBlendMode` blend)
      -- Full repaints clear the target, including bare backdrop regions.
      -- Partial updates preserve the undamaged part of the retained texture.
      -- The batch is flushed even when the pass throws, so no geometry leaks
      -- into the next frame.
      renderDrawDataPass (sdlBatch env) ren (themeWindow theme) drawData (sdlImages env) glyphs damage
        `finally` flushRenderBatch (sdlBatch env)
      t2 <- getMonotonicTime
      unlessM (if direct then setRenderScale ren 1 1 else copyRetained env tex) $
        fail "SDL window presentation preparation failed"
      sendSizeLimits env
      -- Read the direct backbuffer before present, but invoke view callbacks
      -- only afterwards, just as for retained frames.
      if direct
        then answerScreenshotsAfter ctx (captureFrame env tex) (void (renderPresentSafe ren))
        else void (renderPresentSafe ren)
      t3 <- getMonotonicTime
      let ms a b = (b - a) * 1000
      noteDebugPresent (sdlDebug env) (ms t0 t1) (ms t1 t2) (ms t2 t3) (ms t0 t3)
        (drawVertexCount drawData) (drawIndexCount drawData) (drawCmdCount drawData)
      writeIORef (sdlLastPresented env) True
      unless direct $ answerScreenshots ctx (captureFrame env tex)
      pure dirtyAfterUi
  where
    ren = sdlRenderer env

-- | Copy the used area of the retained texture to the window, back in the
-- window's pixel coordinates for the events polled next. Damage limits
-- updates to the retained texture, not to this copy: SDL leaves the window
-- backbuffer undefined after each present.
copyRetained :: SdlEnv -> Ptr SDL_Texture -> IO Bool
copyRetained env tex = do
  okTarget <- setRenderTarget ren nullPtr
  okClip <- setRenderClipRect ren (PtrConst.unsafeFromPtr nullPtr)
  void $ setRenderScale ren 1 1
  -- The texture is larger than the window: copy only the used area.
  r <- readIORef (sdlRetain env)
  okCopy <- with (SDL_FRect 0 0 (fromIntegral (retainW r)) (fromIntegral (retainH r))) $ \src ->
    renderTexture ren tex (PtrConst.unsafeFromPtr src) (PtrConst.unsafeFromPtr nullPtr)
  pure (okTarget && okClip && okCopy)
  where
    ren = sdlRenderer env

-- | SDL replaces a Wayland toplevel's size limits with its own at every
-- configure, so they go again with each present, which commits them. SDL's
-- are none, so no limits are sent only to clear the ones sent before.
sendSizeLimits :: SdlEnv -> IO ()
sendSizeLimits env = for_ (sdlSizeLimits env) $ \ref -> do
  limits@(nw, nh, xw, xh) <- readIORef ref
  let some = limits /= (0, 0, 0, 0)
  whenM ((some ||) <$> readIORef (sdlSizeLimitsSent env)) $ do
    sendWaylandSizeLimits (sdlWindow env) nw nh xw xh
    writeIORef (sdlSizeLimitsSent env) some

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
      stale = w /= retainW r || h /= retainH r || scaleMoved (retainScale r) scale
  if tex /= nullPtr && fits && not roomy
    then do
      when stale $ writeIORef (sdlRetain env) r {retainW = w, retainH = h, retainScale = scale}
      pure (tex, stale)
    else mask_ $ do
      let cw = roundUp w
          ch = roundUp h
      -- Allocate before replacing: failure leaves the owned texture valid.
      tex' <- createTexture (sdlRenderer env) Pixels.SDL_PIXELFORMAT_RGBA32 Render.SDL_TEXTUREACCESS_TARGET (fromIntegral cw) (fromIntegral ch)
      when (tex' == nullPtr) $ fail "SDL_CreateTexture(retain) failed"
      void $ setTextureBlendMode tex' (maybe (fromIntegral sDL_BLENDMODE_NONE) snd (sdlTransparent env))
      writeIORef (sdlRetain env) (Retain tex' cw ch w h scale)
      unless (tex == nullPtr) $ destroyTexture tex
      pure (tex', True)
  where
    roundUp n = max retainBlock (((n + retainBlock - 1) `div` retainBlock) * retainBlock)

-- | Read the open session's debug information, refreshing at most four
-- times per second; 'Nothing' outside a session. Repeated queries keep the
-- debug sampler active and can schedule periodic frames.
askSdlDebug :: NanoUI (Maybe SdlDebugSnapshot)
askSdlDebug = withSdlEnv Nothing (fmap Just . sdlDebugSnapshot)

-- | 'askSdlDebug' for an explicit session, from IO.
sdlDebugSnapshot :: SdlEnv -> IO SdlDebugSnapshot
sdlDebugSnapshot env = do
  -- The display is queried only when the snapshot refreshes.
  refreshDebugSnapshot (sdlDebug env) (sdlDebugPublished env) $ \core -> do
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
    when (sdlFrameTrace env) (traceFrame snap)
    pure snap

-- | Request a UI font family. The SDL display thread resolves and applies it
-- before the next frame, which this asks for (see
-- 'NanoUI.Sdl.Internal.Window.syncDisplay'), rebuilding the glyph atlas and
-- text resolver.
setSdlUiFont :: NanoUIFont -> NanoUI ()
setSdlUiFont = requestDisplay sdlFontRequestRef

-- | Set the UI scale (see 'NanoUI.Sdl.Internal.Window.sdlAppUiScale'): a zoom on top
-- of the pixel density, or zero or less to follow the display. The display
-- thread applies it before the next frame, which this asks for.
setSdlUiScale :: Float -> NanoUI ()
setSdlUiScale = requestDisplay sdlUiScaleRef

-- | Hand the display thread a new setting and ask for the frame it is
-- applied before. Asking for the setting already asked for asks for nothing.
requestDisplay :: Eq a => (SdlEnv -> IORef a) -> a -> NanoUI ()
requestDisplay field new = do
  ctx <- askContext
  withSdlEnv () $ \env -> do
    cur <- readIORef (field env)
    when (cur /= new) $ writeIORef (field env) new >> markDirtyCovered ctx
