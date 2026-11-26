{-# LANGUAGE StrictData #-}

-- | Draw layer facade: data types, arena and geometry, plus the text emitters
-- (plain and synthetic-styled) and the 'DrawOp' interpreter.
module NanoUI.Internal.Draw
  ( Layer (..)
  , DrawCmd (..)
  , ShaderCall (..)
  , EffectSpan (..)
  , emptyShaderCalls
  , emptyEffectSpans
  , emptyUniformBytes
  , DrawData (..)
  , DrawFrameExtra (..)
  , DrawArena (..)
  , RecordedOp (..)
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
  , setDrawPixelScale
  , setDrawSquareGeometry
  , setDrawClear
  , getDrawClear
  , setDrawExternalText
  , setDrawRecordOps
  , getDrawRecordOps
  , withRecordOpacity
  , recordedOps
  , beginLayer
  , currentLayer
  , currentClip
  , setClip
  , setClipPieces
  , getClipPieces
  , withClip
  , pushShaderQuad
  , EffectSpanToken
  , beginEffectSpan
  , endEffectSpan
  , finishDraw
  , drawCmdCount
  , drawnVertexCount
  , drawCmdNull
  , forDrawCmdsInLayer_
  , drawCmdElems
  , cmdEffectiveClip
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
  , pushRoundedRect
  , pushRoundedRectRaw
  , pushRoundedStroke
  , pushLine
  , pushFilledTriangle
  , pushPolylineAA
  , points3
  , drawTextBox
  , glyphSlackLines
  , pushText
  , pushPreparedTextStyled
  , emitDrawOps
  , pushImageOp
  , pushShapeOp
  , checkboxOps
  ) where

import Control.Monad (forM_, unless, when)
import Data.Char (isSpace)
import Data.IORef (modifyIORef', readIORef)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import qualified Data.Text.Unsafe as TU
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray)
import Data.Primitive.PrimArray (indexPrimArray, readPrimArray, sizeofPrimArray)
import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr)
import NanoUI.Internal.Draw.Arena
import NanoUI.Internal.Draw.Shapes
import NanoUI.Internal.Draw.Types
import NanoUI.Internal.Font
import NanoUI.Internal.SIMD (pokeQuadCornersSIMD, pokeQuadSIMD)
import NanoUI.Internal.Style (FontStyle (..), FontWeight (..), TextDecoration (..), Theme (..), resolveFontSize, styleBg)
import NanoUI.Internal.Types (Color (..), Rect (..), onGrid, rectInflate, rectIntersect, spanEffectiveClip)

-- | The line box, in logical pixels, that a 'DrawText' of the text anchored
-- at @(x, y)@ by @ax@ and @ay@ occupies, measured with @fm@'s advances.
drawTextBox :: FontMetrics -> Float -> Float -> Float -> Float -> T.Text -> Rect
drawTextBox fm x y ax ay t =
  let tw = lineWidth fm t
      px = x - tw * max 0 ax
   in Rect px (textBoxTop fm y ay) tw (fmLineHeight fm)

-- | Top of a 'drawTextBox' line box anchored at @y@ by @ay@.
textBoxTop :: FontMetrics -> Float -> Float -> Float
textBoxTop fm y ay
  | ay < 0 = y - fmAscent fm
  | otherwise = y - fmLineHeight fm * (1 - ay)

-- | Intersect a command's clip rectangle with an optional damage rectangle.
-- Returns 'Nothing' when the command is culled, @Just Nothing@ when both the
-- command clip and damage are open, and @Just (Just clip)@ for a concrete clip.
{-# INLINE cmdEffectiveClip #-}
cmdEffectiveClip :: Maybe Rect -> DrawCmd -> Maybe (Maybe Rect)
cmdEffectiveClip mDamage cmd =
  spanEffectiveClip mDamage (Rect (cmdClipX cmd) (cmdClipY cmd) (cmdClipW cmd) (cmdClipH cmd))

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

-- | Horizontal clip bounds expanded by synthetic-oblique shear lean, plus the
-- conservative glyph slack in logical pixels.
{-# INLINE glyphClipBounds #-}
glyphClipBounds :: DrawArena -> FontMetrics -> Float -> IO (Float, Float, Float)
glyphClipBounds da fm slant = do
  cx <- readPrimArray (daCurrentClip da) 0
  cw <- readPrimArray (daCurrentClip da) 2
  let !lean = abs slant * (fmLineHeight fm + abs (fmAscent fm))
  pure (cx - lean, cx + cw + lean, glyphSlackLines * fmLineHeight fm)

-- | A shaped line's glyph quads from pen @(px, py)@, sheared by @slant@
-- around the baseline like 'pushGlyphQuads'. Glyphs wholly left or right of
-- the current clip emit nothing. Glyph x offsets rise in visual order, to
-- within 'glyphSlackLines', so binary searches find the first glyph that
-- can reach the clip and the first that starts well past it: a long line in
-- a narrow view costs, and reserves buffer room for, the glyphs it shows
-- plus two searches. Glyphs on another atlas page than the one before them
-- start a new draw command on that page's texture.
pushShapedQuads :: DrawArena -> FontMetrics -> Float -> Float -> Float -> ShapedGlyphs -> Color -> IO ()
pushShapedQuads da fm slant px py (ShapedGlyphs quads) col = do
  let !count = sizeofPrimArray quads `div` 8
  when (count > 0) $ do
    (!left, !right, !slack) <- glyphClipBounds da fm slant
    let at k = indexPrimArray quads k
        -- The first glyph in [lo, hi) whose x reaches @thr@.
        firstReaching !thr !lo !hi
          | lo >= hi = lo
          | otherwise =
              let !mid = (lo + hi) `div` 2
               in if px + at (mid * 8) < thr
                    then firstReaching thr (mid + 1) hi
                    else firstReaching thr lo mid
        -- Every glyph before @start@ ends left of the clip, and every glyph
        -- from @end@ on starts right of it.
        !start = firstReaching (left - slack) 0 count
        !end = firstReaching (right + slack) start count
        !(r, g, b, a) = unpackColorF col
        !baselineY = py + fmAscent fm
        -- The glyphs from @q0@ on that are on atlas page @page@, until one
        -- that is not.
        drawPage !q0 !page = do
          setTexture da (glyphPageTextureId page)
          let !shown = end - q0
              !pageU = fromIntegral page
          withVertsReserve da (shown * 4) (shown * 6) $ \vp ip base baseIdx commit -> do
            let go !q !m
                  | q >= end = commit (m * 4) (m * 6)
                  | otherwise = do
                      let !o = q * 8
                          !gx = px + at o
                          !gw = at (o + 2)
                          !u0 = at (o + 4)
                      if gx + gw < left || gx > right
                        then go (q + 1) m
                        else if u0 < pageU || u0 >= pageU + 1
                        then
                          -- A glyph whose u is on no page is skipped.
                          if u0 < 0 || u0 >= fromIntegral glyphAtlasPages
                            then go (q + 1) m
                            else commit (m * 4) (m * 6) >> drawPage q (truncate u0)
                        else do
                          let !gy = py + at (o + 1)
                              !gh = at (o + 3)
                              !v0 = at (o + 5)
                              !u1 = at (o + 6)
                              !v1 = at (o + 7)
                          pokeGlyphQuad vp ip base baseIdx slant baselineY r g b a m gx gy gw gh (u0 - pageU) v0 (u1 - pageU) v1
                          go (q + 1) (m + 1)
            go q0 0
    when (start < end) $ do
      let !uFirst = at (start * 8 + 4)
          !startPage =
            if uFirst >= 0 && uFirst < fromIntegral glyphAtlasPages
              then truncate uFirst
              else 0
      drawPage start startPage

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
-- the font lacks, other than whitespace, draws an upright advance box on the
-- device grid. As in 'pushShapedQuads', glyphs wholly outside the clip emit
-- nothing, and the walk stops once the pen is well past it.
pushGlyphQuads :: DrawArena -> FontMetrics -> Float -> Float -> Float -> T.Text -> Color -> IO ()
pushGlyphQuads da fm slant px py txt col = do
  let !byteLen = TF.lengthWord8 txt
  when (byteLen > 0) $ do
    let !cap = T.length txt
    scale <- readIORef (daSnapScale da)
    cx <- readPrimArray (daCurrentClip da) 0
    cw <- readPrimArray (daCurrentClip da) 2
    let !lh = fmLineHeight fm
        !lean = abs slant * (lh + abs (fmAscent fm))
        !left = cx - lean
        !right = cx + cw + lean
        !slack = glyphSlackLines * lh
    setTexture da glyphAtlasTextureId
    withVertsReserve da (cap * 4) (cap * 6) $ \vp ip base baseIdx commit -> do
      let !(r, g, b, a) = unpackColorF col
          !baselineY = py + fmAscent fm
          !stop = right + slack
          -- A pen this far left of the clip draws nothing, whatever its glyph.
          !before = left - slack
          -- Walk UTF-8 byte offsets with 'TU.iter' to avoid per-character
          -- 'T.uncons' Text slicing and Maybe/tuple allocation.
          walk !q !ox !first !prev !off
            | off >= byteLen || ox > stop = pure q
            | otherwise = do
                let !(TU.Iter c step) = TU.iter txt off
                    !gx = if first then ox else ox + fmKerning fm prev c
                    !adv = fmAdvance fm c
                    !ox' = gx + adv
                    !off' = off + step
                if gx < before
                  then walk q ox' False c off'
                  else case fmGlyph fm c of
                    Nothing
                      | adv > 0 && not (isSpace c) && not (gx + adv < left || gx > right) -> do
                          pokeGlyphQuad vp ip base baseIdx 0 baselineY r g b a q (onGrid scale gx) (onGrid scale py) adv lh whitePixel whitePixel whitePixel whitePixel
                          walk (q + 1) ox' False c off'
                      | otherwise -> walk q ox' False c off'
                    Just gq ->
                      let !qx = gx + gqX gq
                          !qw = gqW gq
                       in if qx + qw < left || qx > right
                            then walk q ox' False c off'
                            else do
                              pokeGlyphQuad vp ip base baseIdx slant baselineY r g b a q qx (py + gqY gq) qw (gqH gq) (gqU0 gq) (gqV0 gq) (gqU1 gq) (gqV1 gq)
                              walk (q + 1) ox' False c off'
      !k <- walk 0 px True ' ' 0
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
            !(thick, underOff, strikeOff) = textDecorationMetrics lh (fmAscent fm)
            underline = pushRect da (Rect px (py + underOff) textW thick) col
            strike = pushRect da (Rect px (py + strikeOff) textW thick) col
        case deco of
          DecorationUnderline -> underline
          DecorationStrikethrough -> strike
          DecorationUnderlineStrike -> underline >> strike
          DecorationNone -> pure ()

-- | Emit ops with @fm@ as the default font, at @size@. @resolve@ gives the
-- font for styled text and whether it renders weight and slant natively.
-- @imageUv@ maps an image op's id to an atlas texture and UV bounds, or
-- 'Nothing' when the id is itself a texture. A 'PushClip' intersects the
-- current clip until its 'PopClip'; an unclosed one ends with the ops.
-- Under external text, text ops push no quads: each line is recorded as a
-- span ('pushTextSpan') with the current clip, filed by layer.
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
    -- Clips saved by open 'PushClip's, innermost first.
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
    emitOne (DrawText x y ax ay t c) =
      unlessOffClip fm (textBoxTop fm y ay) $ do
        prepared <- prepareFontMetrics fm t
        external <- readIORef (daExternalText da)
        -- Each branch builds the box: shared, it would be allocated for the
        -- quads too.
        if external
          then pushTextSpan da (drawTextBox prepared x y ax ay t) t c
          else
            let Rect px py _ _ = drawTextBox prepared x y ax ay t
             in pushPreparedTextQuads da prepared px py t c
    -- Styled text with its line box's top-left corner at (x, y).
    emitOne (DrawTextStyled x y font t c) = do
      (styledFm, native) <- resolve font
      unlessOffClip styledFm y $ do
        prepared <- prepareFontMetrics styledFm t
        pushStyled prepared native font x y t c
    emitOne (DrawTextAligned x y ax ay k font t c) = do
      let font'
            | k == 1 = font
            | otherwise =
                let resolved = resolveFontSize size (textFontSize font)
                    baseSz = if resolved > 0 then resolved else size
                 in font {textFontSize = k * baseSz}
      (styledFm, native) <- resolve font'
      unlessOffClip styledFm (textBoxTop styledFm y ay) $ do
        prepared <- prepareFontMetrics styledFm t
        let Rect px py _ _ = drawTextBox prepared x y ax ay t
        pushStyled prepared native font' px py t c
    emitOne op = pushShapeOp da op
    -- Text in @font@, with metrics prepared for it and its line box's
    -- top-left at (x, y), synthesizing the font's weight and slant unless
    -- the host renders them natively. Inlined, or each call of
    -- 'emitDrawOps' would allocate it as a closure over @da@.
    {-# INLINE pushStyled #-}
    pushStyled prepared native font x y t c = do
      external <- readIORef (daExternalText da)
      if external
        then pushTextSpan da (Rect x y (lineWidth prepared t) (fmLineHeight prepared)) t c
        else
          let weight = if native then WeightNormal else textFontWeight font
              fstyle = if native then FontStyleNormal else textFontStyle font
           in pushPreparedTextStyledQuads da prepared weight fstyle (textFontDecoration font) x y t c
    -- Text whose line box, from @top@, lies wholly above or below the clip
    -- by more than the glyph slack draws nothing ('pushShapedQuads' culls
    -- only across), so it is skipped before it is shaped: a long paragraph
    -- or code block costs the lines in view, and under external text
    -- records only those lines' spans.
    unlessOffClip lineFm top act = do
      Rect _ cy _ ch <- currentClip da
      let !lh = fmLineHeight lineFm
          !slack = glyphSlackLines * lh
      unless (top + lh + slack < cy || top - slack > cy + ch) act

-- | Record drawing text as a host span with its line box: under external
-- text a drawing's text has no quads and no node span, so the host draws it
-- from this one ('dfeTextSpans'), in the current clip and filed by layer. An
-- empty clip records nothing, as it gets no command. Top level and out of
-- line so the local closures of 'emitDrawOps' capture nothing more for it.
{-# NOINLINE pushTextSpan #-}
pushTextSpan :: DrawArena -> Rect -> T.Text -> Color -> IO ()
pushTextSpan da !box t c = unless (T.null t) $ do
  clip@(Rect _ _ cw ch) <- currentClip da
  when (cw > 0 && ch > 0) $ do
    layer <- currentLayer da
    let entry = (box, t, c, Color 0, clip)
    modifyIORef' (daExtra da) $ \ex ->
      let (base, overlay) = dfeTextSpans ex
       in ex {dfeTextSpans = if layer >= LayerOverlay then (base, entry : overlay) else (entry : base, overlay)}

-- | Paint a 'DrawImage' op; other ops paint nothing. @imageUv@ is as for
-- 'emitDrawOps', and the op's UVs span 0 to 1 over the image. Unrotated
-- images use the snapped quad; only rotated ones compute their corners.
-- An image is recorded as the op, under its own id; a solid one (no
-- texture) records the shape it paints.
{-# INLINE pushImageOp #-}
pushImageOp :: DrawArena -> (Int -> IO (Maybe (Int, (Float, Float, Float, Float)))) -> DrawOp -> IO ()
pushImageOp da imageUv = \case
  op@(DrawImage r angle tex u0 v0 u1 v1 c) -> do
    let draw !t !a0 !b0 !a1 !b1
          | angle == 0 = pushImage da r t a0 b0 a1 b1 c
          | otherwise = pushImageRotated da r angle t a0 b0 a1 b1 c
    when (tex > 0) (recordOp da op)
    imageUv tex >>= \case
          Just (atlas, (a0, b0, a1, b1)) ->
            draw atlas (a0 + u0 * (a1 - a0)) (b0 + v0 * (b1 - b0)) (a0 + u1 * (a1 - a0)) (b0 + v1 * (b1 - b0))
          Nothing -> draw tex u0 v0 u1 v1
  _ -> pure ()

-- | Paint a fill, stroke, line or gradient op. Text, image and clip ops
-- paint nothing here ('emitDrawOps' handles them). Inlined so that an op
-- built only to be painted is optimized away.
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

-- | The checkbox widget's box, @box@ wide with its top-left corner at
-- @(x, y)@, passed op by op to @op@: the theme accent with a check mark when
-- @checked@, else an input well outlined in @border@. The widget paints the
-- ops directly ('pushShapeOp'); a canvas collects them
-- ('NanoUI.Widgets.Custom.drawCheckbox').
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
    -- Caps snap their centres, like the stroke ends. Snapping a cap's corner
    -- instead can land it a pixel off the stroke at fractional scales.
    cap cx cy = op (FillCircle cx cy (t / 2) mark)
