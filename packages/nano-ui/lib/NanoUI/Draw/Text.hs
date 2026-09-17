{-# LANGUAGE StrictData #-}

-- | Text emitters (plain and synthetic-styled) and the 'DrawOp' interpreter.
module NanoUI.Draw.Text
  ( drawTextBox
  , pushText
  , pushTextStyled
  , emitDrawOps
  ) where

import Control.Monad (forM_, unless, when)
import Data.IORef (readIORef)
import qualified Data.Text as T
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray)
import Data.Primitive.PrimArray (indexPrimArray, sizeofPrimArray)
import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr)
import NanoUI.Draw.Arena
import NanoUI.Draw.Shapes
import NanoUI.Draw.Types (DrawArena (..), DrawOp (..), TextFont (..), glyphAtlasTextureId, indexSize, vertexSize)
import NanoUI.Font
  ( FontMetrics (..)
  , GlyphQuad (..)
  , ShapedGlyphs (..)
  , drawGlyph
  , drawShaped
  , kernedAdvance
  , lineWidth
  , prepareFontMetrics
  )
import NanoUI.SIMD (pokeQuadSIMD, pokeVertexSIMD)
import NanoUI.Style (FontStyle (..), FontWeight (..), TextDecoration (..))
import NanoUI.Types (Color (..), Rect (..), onGrid)

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

{-# INLINE pushTextStyled #-}
pushTextStyled ::
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
pushTextStyled da fm weight fstyle deco x y txt col = do
  prepared <- prepareFontMetrics fm txt
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

-- | A shaped line's glyph quads from pen @(px, py)@, sheared by @slant@
-- around the baseline like 'pushGlyphQuads'.
pushShapedQuads :: DrawArena -> FontMetrics -> Float -> Float -> Float -> ShapedGlyphs -> Color -> IO ()
pushShapedQuads da fm slant px py (ShapedGlyphs quads) col = do
  let !count = sizeofPrimArray quads `div` 8
  when (count > 0) $ do
    setTexture da glyphAtlasTextureId
    withVertsReserve da (count * 4) (count * 6) $ \vp ip base baseIdx commit -> do
      let !(r, g, b, a) = unpackColorF col
          !baselineY = py + fmAscent fm
          at k = indexPrimArray quads k
          go !q
            | q >= count = pure ()
            | otherwise = do
                let !o = q * 8
                    !gx = px + at o
                    !gy = py + at (o + 1)
                    !gw = at (o + 2)
                    !gh = at (o + 3)
                    !u0 = at (o + 4)
                    !v0 = at (o + 5)
                    !u1 = at (o + 6)
                    !v1 = at (o + 7)
                pokeGlyphQuad vp ip base baseIdx slant baselineY r g b a q gx gy gw gh u0 v0 u1 v1
                go (q + 1)
      go 0
      commit (count * 4) (count * 6)

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
      pokeVertexSIMD vp vb (gx + topDx) gy r g b a u0 v0
      pokeVertexSIMD vp (vb + 32) (gx + gw + topDx) gy r g b a u1 v0
      pokeVertexSIMD vp (vb + 64) (gx + gw + botDx) gy1 r g b a u1 v1
      pokeVertexSIMD vp (vb + 96) (gx + botDx) gy1 r g b a u0 v1
      pokeQuadIndices ip ib i0 (i0 + 1) (i0 + 2) (i0 + 3)

-- | Glyph quads for one line from pen @(px, py)@, used as given: synthetic bold
-- relies on its sub-pixel pass offsets. Every quad shares one arena
-- reservation. A non-zero @slant@ shears glyphs around the shared baseline for
-- synthetic oblique, so stems stay parallel and descenders lean left. A glyph
-- the font lacks draws an upright advance box on the device grid.
pushGlyphQuads :: DrawArena -> FontMetrics -> Float -> Float -> Float -> T.Text -> Color -> IO ()
pushGlyphQuads da fm slant px py txt col = do
  let !cap = T.length txt
  when (cap > 0) $ do
    scale <- readIORef (daSnapScale da)
    setTexture da glyphAtlasTextureId
    withVertsReserve da (cap * 4) (cap * 6) $ \vp ip base baseIdx commit -> do
      let !(r, g, b, a) = unpackColorF col
          !baselineY = py + fmAscent fm
          walk !q !ox !prev !t =
            case T.uncons t of
              Nothing -> pure q
              Just (c, rest) -> do
                let !adv = kernedAdvance fm prev c
                    next !q' = walk q' (ox + adv) (Just c) rest
                drawGlyph fm c >>= \case
                  Nothing
                    | adv > 0 && c /= ' ' -> do
                        pokeGlyphQuad vp ip base baseIdx 0 baselineY r g b a q (onGrid scale ox) (onGrid scale py) adv (fmLineHeight fm) whitePixelU whitePixelV whitePixelU whitePixelV
                        next (q + 1)
                    | otherwise -> next q
                  Just gq -> do
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
      if slant == 0 && weight == WeightNormal
        then pushPreparedTextQuads da fm px py txt col
        else do
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

-- | Emit ops with @fm@ as the default font and @resolve@ giving the font of
-- styled text, and whether it draws its weight and slant natively.
emitDrawOps :: DrawArena -> FontMetrics -> (TextFont -> IO (FontMetrics, Bool)) -> SmallArray DrawOp -> IO ()
emitDrawOps da fm resolve ops = go 0
  where
    go !i
      | i >= sizeofSmallArray ops = pure ()
      | otherwise = emitOne (indexSmallArray ops i) >> go (i + 1)
    emitOne (FillRect r c) = pushRect da r c
    emitOne (FillRoundedRect r radius c) = pushRoundedRect da r radius c
    emitOne (FillTriangle x0 y0 x1 y1 x2 y2 c) = pushFilledTriangle da x0 y0 x1 y1 x2 y2 c
    emitOne (FillCircle cx cy radius c) =
      pushRoundedRect da (Rect (cx - radius) (cy - radius) (2 * radius) (2 * radius)) radius c
    emitOne (Stroke x0 y0 x1 y1 t c) = pushStroke da x0 y0 x1 y1 t c
    emitOne (StrokeRoundedRect r radius bw c) = pushRoundedStroke da r radius bw c
    emitOne (StrokeCircle cx cy radius bw c) =
      pushRoundedStroke da (Rect (cx - radius) (cy - radius) (2 * radius) (2 * radius)) radius bw c
    emitOne (StrokeLineAA x0 y0 x1 y1 bw c) = pushStrokeAA da x0 y0 x1 y1 bw c
    emitOne (FillQuadGradient r c0 c1 c2 c3) = pushQuadGradient da r c0 c1 c2 c3
    emitOne (DrawImageRect r tex u0 v0 u1 v1 c) = pushImage da r tex u0 v0 u1 v1 c
    emitOne (DrawText x y ax ay t c) = do
      prepared <- prepareFontMetrics fm t
      let Rect px py _ _ = drawTextBox prepared x y ax ay t
      -- Drawing text has no collected text span, so it keeps its quads even
      -- when the host rasterizes widget text externally.
      pushPreparedTextQuads da prepared px py t c
    emitOne (DrawTextStyled x y font t c) = do
      (styledFm, native) <- resolve font
      let weight = if native then WeightNormal else textFontWeight font
          fstyle = if native then FontStyleNormal else textFontStyle font
      prepared <- prepareFontMetrics styledFm t
      pushPreparedTextStyledQuads da prepared weight fstyle (textFontDecoration font) x y t c
