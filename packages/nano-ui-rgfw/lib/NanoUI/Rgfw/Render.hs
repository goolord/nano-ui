{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Rgfw.Render
  ( renderArena
  , renderTextEditMenuOverlay
  ) where

import Control.Monad (forM_, when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8, Word32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff, peekElemOff, pokeElemOff)
import NanoUI (Color (..), Rect (..), V2 (..), colorRGBA, rectContains)
import NanoUI.Context
  ( Context
  , TextInputMenu (..)
  , ctxFontMetrics
  , ctxHostProfile
  , getTextInputMenu
  )
import NanoUI.Frame.Hit (widgetOverlayAllowed)
import NanoUI.Frame.TextEdit
  ( TextEditMenuRow (..)
  , textFieldMenuActionEnabled
  , textEditMenuContentRect
  , textEditMenuLayout
  )
import NanoUI.Rgfw.Font.Cozette (CozetteFont)
import NanoUI.Rgfw.Surface
  ( RgfwSurface (..)
  , drawRectOutline
  , drawTextScaled
  , fillRect
  , packColor
  , popClip
  , pushClip
  , toPhysRect
  )
import NanoUI.Rgfw.Theme (RgfwTheme (..))
import NanoUI.Testing
  ( DrawCmd (..)
  , DrawData (..)
  , Layer (..)
  , backdropDimTextureId
  , forDrawCmdsInLayer_
  , indexSize
  , vertexSize
  )

-- | One vertex colour, straight out of the shared draw buffer.
type RGBA = (Float, Float, Float, Float)

-- | Rasterize a frame the way the terminal backend does, but into pixels:
-- walk the core's 'DrawData' in canonical layer order filling every quad,
-- then stamp the core's collected text spans with the embedded Cozette
-- bitmap font. Widget drawing (windows, popups, buttons, fields, scroll
-- chrome, ...) comes from the core draw path, so RGFW gets theme parity and
-- core drawing behavior for free.
--
-- Layer and span order matches the terminal rasterizer: background quads,
-- content quads, base spans, overlay quads, chrome quads, overlay spans.
-- Quads whose texture id is 'glyphAtlasTextureId' are always plain colored
-- quads on this host (the Cozette metrics expose no glyph atlas, so core
-- text lowers to per-character advance rects, exactly like the terminal's
-- @applyCmd@ assumption), which is why every quad is filled rather than
-- sampled; glyphs themselves are stamped afterwards from the span lists.
renderArena ::
  RgfwSurface ->
  CozetteFont ->
  Float -> -- logical (layout) -> physical (pixel) scale
  DrawData ->
  [(Rect, Text, Color, Color, Rect)] -> -- base spans (rect, text, fg, bg, clip)
  [(Rect, Text, Color, Color, Rect)] -> -- overlay spans
  IO ()
renderArena surf font !scale drawData baseSpans overlaySpans = do
  forDrawCmdsInLayer_ LayerBackground drawData (applyCmd surf scale drawData)
  forDrawCmdsInLayer_ LayerContent drawData (applyCmd surf scale drawData)
  -- Spans after content quads so scroll tracks do not erase box rules.
  mapM_ (stampSpan surf font scale) baseSpans
  forDrawCmdsInLayer_ LayerOverlay drawData (applyCmd surf scale drawData)
  -- Floating chrome (window scrollbars) before overlay text, like Term.
  forDrawCmdsInLayer_ LayerChrome drawData (applyCmd surf scale drawData)
  mapM_ (stampSpan surf font scale) overlaySpans

applyCmd :: RgfwSurface -> Float -> DrawData -> DrawCmd -> IO ()
applyCmd surf !scale dd cmd
  | count < 3 = pure ()
  | otherwise =
      withForeignPtr (drawVertices dd) $ \vp ->
        withForeignPtr (drawIndices dd) $ \ip ->
          case physClip surf scale (Rect (cmdClipX cmd) (cmdClipY cmd) (cmdClipW cmd) (cmdClipH cmd)) of
            Nothing -> pure ()
            Just clip -> walkPrims surf scale dd vp ip isDim clip start (start + count)
  where
    start = fromIntegral (cmdIndexOffset cmd)
    count = fromIntegral (cmdIndexCount cmd)
    isDim = cmdTextureId cmd == backdropDimTextureId

-- | Walk a cmd's index range. The draw buffer holds two primitive shapes:
-- quads (6 indices, @a b c a c d@, vertices @a..d@ contiguous) and filled
-- triangles (3 fresh indices). A group whose 4th and 5th indices repeat the
-- 1st and 3rd is a quad; anything else is a triangle.
walkPrims ::
  RgfwSurface ->
  Float ->
  DrawData ->
  Ptr Word8 ->
  Ptr Word8 ->
  Bool ->
  (Int, Int, Int, Int) ->
  Int ->
  Int ->
  IO ()
walkPrims surf scale dd vp ip isDim clip !i !end
  | i + 3 > end = pure ()
  | otherwise = do
      ia <- indexAt dd ip i
      ib <- indexAt dd ip (i + 1)
      ic <- indexAt dd ip (i + 2)
      isQuad <-
        if i + 6 <= end
          then do
            ia' <- indexAt dd ip (i + 3)
            ic' <- indexAt dd ip (i + 4)
            pure (ia' == ia && ic' == ic)
          else pure False
      if isQuad
        then do
          idv <- indexAt dd ip (i + 5)
          stampQuad surf scale dd vp isDim clip ia ib ic idv
          walkPrims surf scale dd vp ip isDim clip (i + 6) end
        else do
          stampTriangle surf scale dd vp clip ia ib ic
          walkPrims surf scale dd vp ip isDim clip (i + 3) end

-- | Fill a quad's bounding box with its colour, like the terminal
-- rasterizer's @stampQuad@. Two flat-fill approximations apply:
--
--  * 4-corner gradient quads (no gradient support on the software surface)
--    are filled with the average of their corner colours; the terminal
--    flattens them to the top-left corner instead. For the equal-corner
--    quads the core emits for plain rects this is the exact colour.
--  * Quads with any corner alpha < 32 are coverage-AA fringes of strokes and
--    rounded corners and are skipped, so those edges lose their 1px soft
--    gradient but never smear a half-transparent flat colour.
--
-- Translucent fills (drop shadows, modal backdrop dims) alpha-blend instead
-- of overwriting; 'backdropDimTextureId' cmds blend uniformly like the
-- terminal's @stampBackdropDim@ (mix amount taken from the vertex alpha).
stampQuad ::
  RgfwSurface ->
  Float ->
  DrawData ->
  Ptr Word8 ->
  Bool ->
  (Int, Int, Int, Int) ->
  Int ->
  Int ->
  Int ->
  Int ->
  IO ()
stampQuad surf scale dd vp isDim clip ia ib ic idv = do
  mvs <- mapM (vertexAt dd vp) [ia, ib, ic, idv]
  case mvs of
    [Just (x0, y0, c0), Just (x1, y1, c1), Just (x2, y2, c2), Just (x3, y3, c3)] ->
      when (minimum (map alpha8 [c0, c1, c2, c3]) >= 32) $ do
        let !minX = minimum [x0, x1, x2, x3]
            !maxX = maximum [x0, x1, x2, x3]
            !minY = minimum [y0, y1, y2, y3]
            !maxY = maximum [y0, y1, y2, y3]
            !col = avgRGBA [c0, c1, c2, c3]
            (!px, !py, !pw, !ph) = toPhysRect scale minX minY (maxX - minX) (maxY - minY)
        case clipRect clip px py pw ph of
          Nothing -> pure ()
          Just (fx, fy, fw, fh) ->
            if isDim || alpha8 col < 255
              then blendRectPx surf fx fy fw fh col
              else fillRect surf fx fy fw fh (surfaceWord col)
    _ -> pure ()

-- | Fill a triangle (sort arrows, select chevrons, rounded-corner fans) with
-- its average corner colour under the same alpha rules as 'stampQuad'.
stampTriangle ::
  RgfwSurface ->
  Float ->
  DrawData ->
  Ptr Word8 ->
  (Int, Int, Int, Int) ->
  Int ->
  Int ->
  Int ->
  IO ()
stampTriangle surf scale dd vp clip ia ib ic = do
  mvs <- mapM (vertexAt dd vp) [ia, ib, ic]
  case mvs of
    [Just (x0, y0, c0), Just (x1, y1, c1), Just (x2, y2, c2)] ->
      when (minimum (map alpha8 [c0, c1, c2]) >= 32) $ do
        let !col = avgRGBA [c0, c1, c2]
            (!ax, !ay) = physPt scale x0 y0
            (!bx, !by) = physPt scale x1 y1
            (!cx, !cy) = physPt scale x2 y2
        fillTrianglePx surf clip ax ay bx by cx cy col
    _ -> pure ()

-- | Flat triangle fill on pixel centers, clipped to the cmd clip.
fillTrianglePx ::
  RgfwSurface ->
  (Int, Int, Int, Int) ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  RGBA ->
  IO ()
fillTrianglePx surf (cx0, cy0, cx1, cy1) ax ay bx by cx cy col
  | area == 0 = pure ()
  | otherwise =
      forM_ [max cy0 (min3 ay by cy) .. min (cy1 - 1) (max3 ay by cy)] $ \py ->
        forM_ [max cx0 (min3 ax bx cx) .. min (cx1 - 1) (max3 ax bx cx)] $ \px -> do
          let !pxc = fromIntegral px + (0.5 :: Float)
              !pyc = fromIntegral py + (0.5 :: Float)
              edge x0 y0 x1 y1 =
                (f x1 - f x0) * (pyc - f y0) - (f y1 - f y0) * (pxc - f x0)
              !w0 = edge bx by cx cy
              !w1 = edge cx cy ax ay
              !w2 = edge ax ay bx by
              !s = if area < 0 then -1 else 1
          when (w0 * s >= 0 && w1 * s >= 0 && w2 * s >= 0) $
            pokePixel surf px py col
  where
    f :: Int -> Float
    f = fromIntegral
    area = (f bx - f ax) * (f cy - f ay) - (f cx - f ax) * (f by - f ay)
    min3 p q r = min p (min q r)
    max3 p q r = max p (max q r)

-- | Stamp one core text span with the Cozette bitmap font. The span carries
-- its own clip rect (logical pixels), pushed onto the surface clip stack for
-- the blit. The span background is only painted when it is opaque enough to
-- matter; translucent backgrounds already exist as quads in the DrawData.
stampSpan ::
  RgfwSurface ->
  CozetteFont ->
  Float ->
  (Rect, Text, Color, Color, Rect) ->
  IO ()
stampSpan surf font !scale (Rect rx ry rw rh, txt, fg, bg, clip)
  | T.null txt = pure ()
  | otherwise = case physClip surf scale clip of
      Nothing -> pure ()
      Just (cx0, cy0, cx1, cy1) -> do
        let !bgA = colorAlphaOf bg
        when (bgA >= 32) $ do
          let (!px, !py, !pw, !ph) = toPhysRect scale rx ry rw rh
          case clipRect (cx0, cy0, cx1, cy1) px py pw ph of
            Just (fx, fy, fw, fh) ->
              if bgA >= 255
                then fillRect surf fx fy fw fh (packColor bg)
                else blendRectPx surf fx fy fw fh (colorRGBAf bg)
            Nothing -> pure ()
        pushClip surf cx0 cy0 (cx1 - cx0) (cy1 - cy0)
        drawTextScaled surf font scale rx ry txt (packColor fg)
        popClip surf

-- | Flat alpha-blended fill. x/y/w/h must already be clipped to the surface.
blendRectPx :: RgfwSurface -> Int -> Int -> Int -> Int -> RGBA -> IO ()
blendRectPx surf !x !y !w !h col
  | w <= 0 || h <= 0 = pure ()
  | otherwise = loopY y
  where
    buf = sBuffer surf
    stride = sWidth surf
    loopY !py
      | py >= y + h = pure ()
      | otherwise = loopX py x >> loopY (py + 1)
    loopX !py !px
      | px >= x + w = pure ()
      | otherwise = do
          let !base = py * stride + px
          !dst <- peekElemOff buf base
          pokeElemOff buf base (blendPixelWord dst col)
          loopX py (px + 1)

pokePixel :: RgfwSurface -> Int -> Int -> RGBA -> IO ()
pokePixel surf px py col
  | alpha8 col >= 255 =
      pokeElemOff (sBuffer surf) (py * sWidth surf + px) (surfaceWord col)
  | otherwise = do
      let !base = py * sWidth surf + px
      !dst <- peekElemOff (sBuffer surf) base
      pokeElemOff (sBuffer surf) base (blendPixelWord dst col)

-- | Alpha-blend an RGBA colour over a packed BGRA surface pixel.
blendPixelWord :: Word32 -> RGBA -> Word32
blendPixelWord !dst (r, g, b, a) =
  let !t = max 0 (min 1 a)
      !inv = 1 - t
      !dr = fromIntegral ((dst `shiftR` 16) .&. 0xFF) :: Float
      !dg = fromIntegral ((dst `shiftR` 8) .&. 0xFF) :: Float
      !db = fromIntegral (dst .&. 0xFF) :: Float
      !orC = r * t + dr * inv
      !ogC = g * t + dg * inv
      !obC = b * t + db * inv
   in packColor (Color (rgbaWord (orC, ogC, obC, 1)))

indexAt :: DrawData -> Ptr Word8 -> Int -> IO Int
indexAt dd ip i
  | i < 0 || i >= drawIndexCount dd = pure (-1)
  | otherwise = do
      !w <- peekByteOff ip (i * indexSize) :: IO Word32
      pure (fromIntegral w)

vertexAt :: DrawData -> Ptr Word8 -> Int -> IO (Maybe (Float, Float, RGBA))
vertexAt dd vp vi
  | vi < 0 || vi >= drawVertexCount dd = pure Nothing
  | otherwise = do
      x <- peekFloatAt vp (vi * vertexSize)
      y <- peekFloatAt vp (vi * vertexSize + 4)
      r <- peekFloatAt vp (vi * vertexSize + 8)
      g <- peekFloatAt vp (vi * vertexSize + 12)
      b <- peekFloatAt vp (vi * vertexSize + 16)
      a <- peekFloatAt vp (vi * vertexSize + 20)
      pure (Just (x, y, (r, g, b, a)))

peekFloatAt :: Ptr Word8 -> Int -> IO Float
peekFloatAt p off = peekByteOff p off

-- | Clip rect (logical) scaled and intersected with the surface bounds.
physClip :: RgfwSurface -> Float -> Rect -> Maybe (Int, Int, Int, Int)
physClip surf !scale (Rect x y w h) =
  let (!px, !py, !pw, !ph) = toPhysRect scale x y w h
      !x0 = max 0 px
      !y0 = max 0 py
      !x1 = min (sWidth surf) (px + pw)
      !y1 = min (sHeight surf) (py + ph)
   in if x0 >= x1 || y0 >= y1 then Nothing else Just (x0, y0, x1, y1)

clipRect :: (Int, Int, Int, Int) -> Int -> Int -> Int -> Int -> Maybe (Int, Int, Int, Int)
clipRect (cx0, cy0, cx1, cy1) x y w h =
  let !x0 = max cx0 x
      !y0 = max cy0 y
      !x1 = min cx1 (x + w)
      !y1 = min cy1 (y + h)
   in if x0 >= x1 || y0 >= y1 then Nothing else Just (x0, y0, x1 - x0, y1 - y0)

physPt :: Float -> Float -> Float -> (Int, Int)
physPt !s !x !y = (round (x * s), round (y * s))

rgbaA :: RGBA -> Float
rgbaA (_, _, _, a) = a

-- Vertex alphas are 0..1 floats; the fill gates use the packed 0..255 scale.
alpha8 :: RGBA -> Int
alpha8 c = max 0 (min 255 (round (rgbaA c * 255) :: Int))

avgRGBA :: [RGBA] -> RGBA
avgRGBA cs =
  let !n = fromIntegral (length cs) :: Float
      mean f = sum (map f cs) / n
   in ( mean (\(r, _, _, _) -> r)
      , mean (\(_, g, _, _) -> g)
      , mean (\(_, _, b, _) -> b)
      , mean (\(_, _, _, a) -> a)
      )

clampByte :: Float -> Word32
clampByte f = fromIntegral (max 0 (min 255 (round (f * 255) :: Int)))

rgbaWord :: RGBA -> Word32
rgbaWord (r, g, b, a) =
  (clampByte r `shiftL` 24) .|. (clampByte g `shiftL` 16) .|. (clampByte b `shiftL` 8) .|. clampByte a

surfaceWord :: RGBA -> Word32
surfaceWord = packColor . Color . rgbaWord

colorAlphaOf :: Color -> Int
colorAlphaOf (Color w) = fromIntegral (w .&. 0xFF)

colorRGBAf :: Color -> RGBA
colorRGBAf (Color w) =
  ( fromIntegral ((w `shiftR` 24) .&. 0xFF) / 255
  , fromIntegral ((w `shiftR` 16) .&. 0xFF) / 255
  , fromIntegral ((w `shiftR` 8) .&. 0xFF) / 255
  , fromIntegral (w .&. 0xFF) / 255
  )

-- | Render the built-in text input / text area context menu overlay
renderTextEditMenuOverlay ::
  RgfwSurface ->
  CozetteFont ->
  Float -> -- Scale factor
  RgfwTheme ->
  Context ->
  V2 -> -- Mouse position for hover highlight
  IO ()
renderTextEditMenuOverlay surf font !scale theme ctx mousePos = do
  mMenu <- getTextInputMenu ctx
  case mMenu of
    Nothing -> pure ()
    Just menu -> do
      let wid = textInputMenuWidget menu
      allow <- widgetOverlayAllowed ctx wid
      when allow $ do
        let menuRect = textInputMenuRect menu
            (!mx, !my, !mw, !mh) = toPhysRect scale (rectX menuRect) (rectY menuRect) (rectW menuRect) (rectH menuRect)
            content = textEditMenuContentRect (ctxHostProfile ctx) menuRect (ctxFontMetrics ctx)
            !cx = rectX content
            !cy = rectY content
            !cw = rectW content
            !shadowOff = max 1 (round (2.0 * scale))
        -- Drop shadow
        fillRect surf (mx + shadowOff) (my + shadowOff) mw mh (packColor (colorRGBA 16 16 16 255))
        -- Background & border
        fillRect surf mx my mw mh (packColor (thPanelBg theme))
        drawRectOutline surf mx my mw mh (packColor (thBorder theme))
        -- Render menu rows
        forM_ (textEditMenuLayout (ctxHostProfile ctx)) $ \(entry, relY, h) -> do
          let (!rowX, !rowY, !rowW, !rowH) = toPhysRect scale cx (cy + relY) cw h
              rowRect = Rect cx (cy + relY) cw h
          case entry of
            TextEditMenuSep -> do
              let !lineY = rowY + max 1 (rowH `div` 2)
                  !padX = max 1 (round (2.0 * scale))
              fillRect surf (rowX + padX) lineY (max 0 (rowW - padX * 2)) 1 (packColor (thBorder theme))
            TextEditMenuItem action lbl -> do
              enabled <- textFieldMenuActionEnabled ctx wid action
              let !hovered = enabled && rectContains rowRect mousePos
              when hovered $ do
                fillRect surf rowX rowY rowW rowH (packColor (thWidgetHover theme))
                let !barW = max 1 (round (2.0 * scale))
                fillRect surf rowX (rowY + barW) barW (max 1 (rowH - barW * 2)) (packColor (thPrimary theme))
              let !textColor = if enabled then thText theme else thTextMuted theme
                  !textY = cy + relY + max 0.0 ((h - 13.0) / 2.0)
              drawTextScaled surf font scale (cx + 5.0) textY lbl (packColor textColor)
