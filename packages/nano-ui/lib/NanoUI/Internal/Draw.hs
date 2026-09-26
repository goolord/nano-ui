{-# LANGUAGE StrictData #-}

-- | Draw layer facade: data types, arena and geometry, plus the text emitters
-- (plain and synthetic-styled) and the 'DrawOp' interpreter.
module NanoUI.Internal.Draw
  ( Layer (..)
  , DrawCmd (..)
  , DrawData (..)
  , DrawArena (..)
  , DrawOp (..)
  , LineCap (..)
  , LineJoin (..)
  , Shade (..)
  , TextFont (..)
  , defaultTextFont
  , DrawingBuild
  , shiftDrawOp
  , newDrawArena
  , resetDrawArena
  , setDrawSnapScale
  , getDrawSnapScale
  , setDrawSquareGeometry
  , setDrawExternalText
  , beginLayer
  , currentLayer
  , currentClip
  , setClip
  , setClipPieces
  , getClipPieces
  , withClip
  , finishDraw
  , drawCmdCount
  , drawCmdNull
  , forDrawCmdsInLayer_
  , drawCmdElems
  , vertexSize
  , indexSize
  , backdropDimTextureId
  , glyphAtlasTextureId
  , glyphAtlasPages
  , glyphPageTextureId
  , textureGlyphPage
  , pushRect
  , pushQuadGradient
  , pushImage
  , pushImageRotated
  , pushRoundedRect
  , pushRoundedRectRaw
  , pushRoundedStroke
  , pushCircle
  , pushLine
  , pushStrokeAA
  , pushFilledTriangle
  , pushPolylineAA
  , points3
  , drawTextBox
  , pushText
  , pushPreparedTextStyled
  , emitDrawOps
  , pushImageOp
  , pushShapeOp
  , checkboxOps
  ) where

import Control.Monad (forM_, unless, when)
import Data.IORef (readIORef)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray)
import Data.Primitive.PrimArray (indexPrimArray, readPrimArray, sizeofPrimArray)
import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr)
import NanoUI.Internal.Draw.Arena
import NanoUI.Internal.Draw.Shapes
import NanoUI.Internal.Draw.Types
import NanoUI.Internal.Font
import NanoUI.Internal.SIMD (pokeQuadCornersSIMD, pokeQuadSIMD)
import NanoUI.Internal.Style (FontStyle (..), FontWeight (..), TextDecoration (..), Theme (..), styleBg)
import NanoUI.Internal.Types (Color (..), Rect (..), onGrid, rectInflate, rectIntersect)

-- | Pixel box for a 'DrawText' using host advances. diagrams text has no
-- envelope, so plot sizing uses this instead of `fontSizeL`.
drawTextBox :: FontMetrics -> Float -> Float -> Float -> Float -> T.Text -> Rect
drawTextBox fm x y ax ay t =
  let tw = lineWidth fm t
      th = fmLineHeight fm
      px = x - tw * max 0 ax
      py =
        if ay < 0
          then y - fmAscent fm
          else y - th * (1 - ay)
   in Rect px py tw th

{-# INLINE pushText #-}
pushText :: DrawArena -> FontMetrics -> Float -> Float -> T.Text -> Color -> IO ()
pushText _da _fm _x _y txt _col | T.null txt = pure ()
pushText da fm x y txt col = do
  prepared <- prepareFontMetrics fm txt
  external <- readIORef (daExternalText da)
  unless external $ pushPreparedTextQuads da prepared x y txt col

-- | Styled text with metrics already prepared for @txt@.
pushPreparedTextStyled ::
  DrawArena ->
  FontMetrics ->
  FontWeight ->
  FontStyle ->
  TextDecoration ->
  Float ->
  Float ->
  T.Text ->
  Color ->
  IO ()
pushPreparedTextStyled da prepared weight fstyle deco x y txt col = do
  external <- readIORef (daExternalText da)
  unless external $ pushPreparedTextStyledQuads da prepared weight fstyle deco x y txt col

-- Snapping the pen to the device pixel grid keeps every glyph quad on a whole
-- pixel. Advances, bearings, and ink sizes are all integer pixel counts divided
-- by the snap scale, so snapping the origin alone aligns the whole line:
-- otherwise fractional layout positions leave glyphs straddling pixel
-- boundaries, which makes nearest-sampled atlas text blurry and jitter as
-- scroll position changes.
pushPreparedTextQuads :: DrawArena -> FontMetrics -> Float -> Float -> T.Text -> Color -> IO ()
pushPreparedTextQuads da fm x y txt col = do
  let !px = onGrid (fmSnapScale fm) x
      !py = onGrid (fmSnapScale fm) y
  -- The host's shaped glyphs when it shapes, otherwise glyphs by character.
  drawShaped fm txt >>= \case
    Just glyphs -> pushShapedQuads da fm 0 px py glyphs col
    Nothing -> pushGlyphQuads da fm 0 px py txt col

-- | How far a glyph's x may fall behind a glyph before it in a line, and how
-- wide a glyph may be, in line heights. Pens only move right in visual order:
-- a glyph lands left of an earlier one only by a negative bearing or as a
-- mark over the glyph before it, and no glyph is wider than a few ems. The
-- clip skips in 'pushShapedQuads' and 'pushGlyphQuads' rely on it.
glyphSlackLines :: Float
glyphSlackLines = 4

-- | A shaped line's glyph quads from pen @(px, py)@, sheared by @slant@
-- around the baseline like 'pushGlyphQuads'. Glyphs wholly left or right of
-- the current clip emit nothing. Glyph x offsets rise in visual order, to
-- within 'glyphSlackLines', so a binary search finds the first glyph that
-- can reach the clip and the walk stops at the first one that starts well
-- past it: a long line in a narrow view costs the glyphs it shows plus a
-- search. Glyphs on another atlas page than the one before them start a new
-- draw command on that page's texture.
pushShapedQuads :: DrawArena -> FontMetrics -> Float -> Float -> Float -> ShapedGlyphs -> Color -> IO ()
pushShapedQuads da fm slant px py (ShapedGlyphs quads) col = do
  let !count = sizeofPrimArray quads `div` 8
  when (count > 0) $ do
    cx <- readPrimArray (daCurrentClip da) 0
    cw <- readPrimArray (daCurrentClip da) 2
    let -- A sheared glyph leans at most this far past its box.
        !lean = abs slant * (fmLineHeight fm + abs (fmAscent fm))
        !left = cx - lean
        !right = cx + cw + lean
        !slack = glyphSlackLines * fmLineHeight fm
        at k = indexPrimArray quads k
        -- The first glyph in [lo, hi) whose x reaches @left - slack@. Every
        -- glyph before it ends left of the clip.
        firstReaching !lo !hi
          | lo >= hi = lo
          | otherwise =
              let !mid = (lo + hi) `div` 2
               in if px + at (mid * 8) < left - slack
                    then firstReaching (mid + 1) hi
                    else firstReaching lo mid
        !start = firstReaching 0 count
        !(r, g, b, a) = unpackColorF col
        !baselineY = py + fmAscent fm
        -- The glyphs from @q0@ on that are on atlas page @page@, until one
        -- that is not.
        drawPage !q0 !page = do
          setTexture da (glyphPageTextureId page)
          let !shown = count - q0
              !pageU = fromIntegral page
          withVertsReserve da (shown * 4) (shown * 6) $ \vp ip base baseIdx commit -> do
            let go !q !m
                  | q >= count = commit (m * 4) (m * 6)
                  | otherwise = do
                      let !o = q * 8
                          !gx = px + at o
                          !gw = at (o + 2)
                          !u0 = at (o + 4)
                      if gx > right + slack
                        -- This glyph and every later one start right of the clip.
                        then commit (m * 4) (m * 6)
                        else if gx + gw < left || gx > right
                        then go (q + 1) m
                        else if u0 < pageU || u0 >= pageU + 1
                        then commit (m * 4) (m * 6) >> drawPage q (truncate u0)
                        else do
                          let !gy = py + at (o + 1)
                              !gh = at (o + 3)
                              !v0 = at (o + 5)
                              !u1 = at (o + 6)
                              !v1 = at (o + 7)
                          pokeGlyphQuad vp ip base baseIdx slant baselineY r g b a m gx gy gw gh (u0 - pageU) v0 (u1 - pageU) v1
                          go (q + 1) (m + 1)
            go q0 0
    drawPage start 0

-- | Glyph quad @q@ of a text reservation whose vertices start at @base@ and
-- indices at @baseIdx@. A non-zero @slant@ shears the quad around
-- @baselineY@. INLINE: it runs per glyph and takes more arguments than GHC
-- unboxes for a call.
{-# INLINE pokeGlyphQuad #-}
pokeGlyphQuad ::
  Ptr Word8 ->
  Ptr Word8 ->
  Int ->
  Int ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Int ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
pokeGlyphQuad vp ip base baseIdx slant baselineY r g b a q gx gy gw gh u0 v0 u1 v1 = do
  let !vb = (base + q * 4) * vertexSize
      !ib = (baseIdx + q * 6) * indexSize
      !i0 = fromIntegral (base + q * 4) :: Word32
  if slant == 0
    then pokeQuadSIMD vp vb ip ib gx gy gw gh u0 v0 u1 v1 r g b a i0
    else do
      let !gy1 = gy + gh
          !topDx = slant * (baselineY - gy)
          !botDx = slant * (baselineY - gy1)
      pokeQuadCornersSIMD vp vb ip ib (gx + topDx) gy (gx + gw + topDx) gy (gx + gw + botDx) gy1 (gx + botDx) gy1 u0 v0 u1 v1 r g b a i0

-- | Glyph quads for one line from pen @(px, py)@, used as given: synthetic bold
-- relies on its sub-pixel pass offsets. Every quad shares one arena
-- reservation. A non-zero @slant@ shears glyphs around the shared baseline for
-- synthetic oblique, so stems stay parallel and descenders lean left. A glyph
-- the font lacks draws an upright advance box on the device grid. As in
-- 'pushShapedQuads', glyphs wholly outside the clip emit nothing, and the
-- walk stops once the pen is well past it.
pushGlyphQuads :: DrawArena -> FontMetrics -> Float -> Float -> Float -> T.Text -> Color -> IO ()
pushGlyphQuads da fm slant px py txt col = do
  let !cap = T.length txt
  when (cap > 0) $ do
    scale <- readIORef (daSnapScale da)
    cx <- readPrimArray (daCurrentClip da) 0
    cw <- readPrimArray (daCurrentClip da) 2
    setTexture da glyphAtlasTextureId
    withVertsReserve da (cap * 4) (cap * 6) $ \vp ip base baseIdx commit -> do
      let !(r, g, b, a) = unpackColorF col
          !baselineY = py + fmAscent fm
          !lean = abs slant * (fmLineHeight fm + abs (fmAscent fm))
          !left = cx - lean
          !right = cx + cw + lean
          !stop = right + glyphSlackLines * fmLineHeight fm
          outside gx gw = gx + gw < left || gx > right
          walk !q !ox !prev !t =
            case T.uncons t of
              Nothing -> pure q
              Just (c, rest)
                | ox > stop -> pure q
                | otherwise -> do
                    let !adv = kernedAdvance fm prev c
                        next !q' = walk q' (ox + adv) (Just c) rest
                    case fmGlyph fm c of
                      Nothing
                        | adv > 0 && c /= ' ' && not (outside ox adv) -> do
                            pokeGlyphQuad vp ip base baseIdx 0 baselineY r g b a q (onGrid scale ox) (onGrid scale py) adv (fmLineHeight fm) whitePixel whitePixel whitePixel whitePixel
                            next (q + 1)
                        | otherwise -> next q
                      Just gq
                        | outside (ox + gqX gq) (gqW gq) -> next q
                        | otherwise -> do
                            pokeGlyphQuad vp ip base baseIdx slant baselineY r g b a q (ox + gqX gq) (py + gqY gq) (gqW gq) (gqH gq) (gqU0 gq) (gqV0 gq) (gqU1 gq) (gqV1 gq)
                            next (q + 1)
      !k <- walk 0 px Nothing txt
      commit (k * 4) (k * 6)

-- | Synthetic weight, slant and decoration over the plain text path. Upright
-- normal weight keeps the run path; every other pass walks glyphs.
pushPreparedTextStyledQuads :: DrawArena -> FontMetrics -> FontWeight -> FontStyle -> TextDecoration -> Float -> Float -> T.Text -> Color -> IO ()
pushPreparedTextStyledQuads da fm weight fstyle deco x y txt col
  | weight == WeightNormal && fstyle == FontStyleNormal && deco == DecorationNone =
      pushPreparedTextQuads da fm x y txt col
  | otherwise = do
      let !px = onGrid (fmSnapScale fm) x
          !py = onGrid (fmSnapScale fm) y
          !lh = fmLineHeight fm
          !bOff = max 1.0 (0.05 * lh)
          !slant = if fstyle == FontStyleNormal then 0 else 0.18
          -- Pen offsets, in bold steps, of the passes that synthesize a weight.
          passes = case weight of
            WeightNormal -> [0]
            WeightLight -> [0]
            WeightMedium -> [0, 0.5]
            WeightSemiBold -> [0, 0.75]
            WeightBold -> [0, 1]
            WeightExtraBold -> [0, 1, 1.5]
            WeightBlack -> [0, 1, 1.5, 2]
      shaped <- drawShaped fm txt
      forM_ passes $ \k -> case shaped of
        Just glyphs -> pushShapedQuads da fm slant (px + k * bOff) py glyphs col
        Nothing -> pushGlyphQuads da fm slant (px + k * bOff) py txt col
      when (deco /= DecorationNone) $ do
        let !textW = lineWidth fm txt
            !thick = max 1.0 (0.06 * lh)
            underline = pushRect da (Rect px (py + fmAscent fm + max 1.0 (0.1 * lh)) textW thick) col
            strike = pushRect da (Rect px (py + fmAscent fm * 0.65) textW thick) col
        case deco of
          DecorationUnderline -> underline
          DecorationStrikethrough -> strike
          DecorationUnderlineStrike -> underline >> strike
          DecorationNone -> pure ()

-- | Emit ops with @fm@ as the default font, @size@ the size it is, and
-- @resolve@ giving the font of styled text, and whether it draws its
-- weight and slant natively. @imageUv@ gives the texture and UV bounds of
-- an image id an image op names, or 'Nothing' when the id is a texture of
-- its own. A 'PushClip' clips the ops up to its 'PopClip' inside the clip
-- they are drawn in, and one left open ends with the ops.
emitDrawOps ::
  DrawArena
  -> FontMetrics
  -> Float
  -> (TextFont -> IO (FontMetrics, Bool))
  -> (Int -> IO (Maybe (Int, (Float, Float, Float, Float))))
  -> SmallArray DrawOp
  -> IO ()
emitDrawOps da fm size resolve imageUv ops = go 0 []
  where
    !n = sizeofSmallArray ops
    -- The clips the open 'PushClip's replaced, innermost first.
    go !i saved
      | i >= n = unless (null saved) (setClip da (last saved))
      | otherwise = case indexSmallArray ops i of
          PushClip r -> do
            prev <- currentClip da
            setClip da (fromMaybe (Rect 0 0 0 0) (rectIntersect prev r))
            go (i + 1) (prev : saved)
          PopClip -> case saved of
            prev : rest -> setClip da prev >> go (i + 1) rest
            [] -> go (i + 1) []
          op -> emitOne op >> go (i + 1) saved
    emitOne op@DrawImage {} = pushImageOp da imageUv op
    emitOne (DrawText x y ax ay t c) = do
      prepared <- prepareFontMetrics fm t
      let Rect px py _ _ = drawTextBox prepared x y ax ay t
      -- Drawing text has no collected text span, so it keeps its quads even
      -- when the host rasterizes widget text externally.
      pushPreparedTextQuads da prepared px py t c
    emitOne (DrawTextStyled x y font t c) = styled x y font t c
    emitOne (DrawTextAligned x y ax ay k font t c) = do
      let font'
            | k == 1 = font
            | otherwise = font {textFontSize = k * (if textFontSize font > 0 then textFontSize font else size)}
      (styledFm, _) <- resolve font'
      prepared <- prepareFontMetrics styledFm t
      let Rect px py _ _ = drawTextBox prepared x y ax ay t
      styled px py font' t c
    emitOne op = pushShapeOp da op
    -- Text in a font of its own, its line box's top left corner at (x, y).
    styled x y font t c = do
      (styledFm, native) <- resolve font
      let weight = if native then WeightNormal else textFontWeight font
          fstyle = if native then FontStyleNormal else textFontStyle font
      prepared <- prepareFontMetrics styledFm t
      pushPreparedTextStyledQuads da prepared weight fstyle (textFontDecoration font) x y t c

-- | Paint an image op ('DrawImage'), @imageUv@ giving the texture and UV
-- bounds of the image id it names, or 'Nothing' when the id is a texture of
-- its own; any other op paints nothing. The op's UVs run 0 to 1 over the
-- image. An unturned image keeps the snapped quad; only a turned one needs
-- its corners worked out.
{-# INLINE pushImageOp #-}
pushImageOp :: DrawArena -> (Int -> IO (Maybe (Int, (Float, Float, Float, Float)))) -> DrawOp -> IO ()
pushImageOp da imageUv = \case
  DrawImage r angle tex u0 v0 u1 v1 c ->
    let draw !t !a0 !b0 !a1 !b1
          | angle == 0 = pushImage da r t a0 b0 a1 b1 c
          | otherwise = pushImageRotated da r angle t a0 b0 a1 b1 c
     in imageUv tex >>= \case
          Just (atlas, (a0, b0, a1, b1)) ->
            draw atlas (a0 + u0 * (a1 - a0)) (b0 + v0 * (b1 - b0)) (a0 + u1 * (a1 - a0)) (b0 + v1 * (b1 - b0))
          Nothing -> draw tex u0 v0 u1 v1
  _ -> pure ()

-- | Paint an op that needs no font or image: a fill, stroke, line or
-- gradient. Text, image and clip ops paint nothing here ('emitDrawOps'
-- paints them). Inlined, so an op built only to be painted costs nothing.
{-# INLINE pushShapeOp #-}
pushShapeOp :: DrawArena -> DrawOp -> IO ()
pushShapeOp da = \case
  FillRect r c -> pushRect da r c
  FillRoundedRect r radius c -> pushRoundedRect da r radius c
  FillTriangle x0 y0 x1 y1 x2 y2 c -> pushFilledTriangle da x0 y0 x1 y1 x2 y2 c
  FillCircle cx cy radius c -> pushCircle da cx cy radius c
  Stroke x0 y0 x1 y1 t c -> pushStroke da x0 y0 x1 y1 t c
  StrokeRoundedRect r radius bw c -> pushRoundedStroke da r radius bw c
  StrokeCircle cx cy radius bw c -> pushCircleStroke da cx cy radius bw c
  StrokeLineAA x0 y0 x1 y1 bw c -> pushStrokeAA da x0 y0 x1 y1 bw c
  FillPolygon pts rings tris sh -> pushPolygonAA da pts rings tris sh
  StrokePolyline pts w closed cap join limit sh -> pushPolylineAA da pts w closed cap join limit sh
  FillQuadGradient r c0 c1 c2 c3 -> pushQuadGradient da r c0 c1 c2 c3
  _ -> pure ()

-- | A checkbox's box, @box@ wide with its top-left corner at @(x, y)@, as
-- the checkbox widget paints it, an op at a time through @op@: the theme's
-- accent with a check mark when @checked@, otherwise an input well outlined
-- in @border@. The widget paints the ops as they come ('pushShapeOp'); a
-- canvas collects them ('NanoUI.Widgets.Custom.drawCheckbox').
{-# INLINE checkboxOps #-}
checkboxOps :: Applicative f => (DrawOp -> f ()) -> Theme -> Color -> Float -> Float -> Float -> Bool -> f ()
checkboxOps op theme border x y box checked
  | checked =
      op (FillRoundedRect outer r accent)
        *> op (StrokeRoundedRect outer r bw accent)
        *> stroke x0 y0 x1 y1
        *> stroke x1 y1 x2 y2
        *> cap x0 y0
        *> cap x1 y1
        *> cap x2 y2
  | otherwise =
      op (FillRoundedRect (rectInflate (-bw) outer) (max 0 (r - bw)) (styleBg (themeInput theme)))
        *> op (StrokeRoundedRect outer r bw border)
  where
    outer = Rect x y box box
    r = min 6 (box / 3.5)
    bw = 1.5
    accent = themeAccent theme
    mark = themeOnAccent theme
    t = max 1.6 (box * 0.11)
    x0 = x + box * 0.22
    y0 = y + box * 0.52
    x1 = x + box * 0.42
    y1 = y + box * 0.72
    x2 = x + box * 0.78
    y2 = y + box * 0.28
    stroke ax ay bx by = op (StrokeLineAA ax ay bx by t mark)
    -- Caps snap their centres, as the strokes snap their ends; snapping a
    -- cap's corner lands it up to a pixel off the stroke at a fractional
    -- scale.
    cap cx cy = op (FillCircle cx cy (t / 2) mark)
