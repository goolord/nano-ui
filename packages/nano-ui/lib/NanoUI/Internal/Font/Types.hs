{-# LANGUAGE StrictData #-}

-- | Font metric types, pure text measurement, caret geometry, and decoration
-- offsets. Backend callbacks populate 'FontBackend' to prepare snapshots and
-- rasterise glyph runs.
module NanoUI.Internal.Font.Types
  ( GlyphQuad (..)
  , ShapedText (..)
  , ShapedGlyphs (..)
  , FontMetrics (..)
  , FontBackend (..)
  , CustomMeasureFn
  , prepareFontMetrics
  , prepareFontMetricsMany
  , measureTextIO
  , lineWidthIO
  , drawShaped
  , drawGlyph
  , caretX
  , caretXIO
  , selectionSpans
  , monospaceMetrics
  , scaleFontMetrics
  , tabularFontMetrics
  , lineWidth
  , textIndexAtX
  , centeredTextY
  , alignedTextPen
  , textInkEnd
  , textDecorationMetrics
  , revealSpanInViewport
  , isDefaultNodeFont
  ) where

import Control.Monad.ST (runST)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Primitive.PrimArray (PrimArray, imapPrimArray, indexPrimArray, mapPrimArray, newPrimArray, sizeofPrimArray, unsafeFreezePrimArray, writePrimArray)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Internal.Style (AlignX (..), FontStyle (..), FontVariant (..), FontWeight (..))
import NanoUI.Internal.TextCluster (clusterBounds)
import NanoUI.Internal.Types (clamp, onGrid)

-- | Glyph ink rectangle relative to the pen, in logical pixels, with normalised
-- atlas UV bounds. Valid only while the backend's atlas placement remains valid.
data GlyphQuad = GlyphQuad
  { gqX :: {-# UNPACK #-} !Float
  , gqY :: {-# UNPACK #-} !Float
  , gqW :: {-# UNPACK #-} !Float
  , gqH :: {-# UNPACK #-} !Float
  , gqU0 :: {-# UNPACK #-} !Float
  , gqV0 :: {-# UNPACK #-} !Float
  , gqU1 :: {-# UNPACK #-} !Float
  , gqV1 :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

-- | A line of text as the host's shaper laid it out: glyphs chosen and placed
-- with the font's kerning, ligatures and contextual forms, in fallback fonts
-- where the font lacks a character, and right-to-left runs reordered.
data ShapedText = ShapedText
  { stAdvance :: {-# UNPACK #-} !Float
  , stInkEnd :: {-# UNPACK #-} !Float
  -- ^ The right edge of the rightmost glyph's ink.
  , stCarets :: !(PrimArray Float)
  -- ^ Where the caret sits before each character, and after the last: one
  -- more entry than the text has characters. A right-to-left run's carets
  -- decrease, and the characters of a cluster share its width.
  }
  deriving (Eq, Show)

-- | The glyph quads that draw a shaped line: eight numbers a glyph (x, y,
-- width and height from the pen, then the atlas UVs u0 v0 u1 v1), in logical
-- pixels.
newtype ShapedGlyphs = ShapedGlyphs (PrimArray Float)
  deriving (Eq, Show)

-- | Logical-pixel font measurements and pure lookup snapshots. Prepare the
-- metrics for the text before pure layout. Native loading and atlas mutation
-- belong in the optional 'FontBackend' IO callbacks.
data FontMetrics = FontMetrics
  { fmLineHeight :: {-# UNPACK #-} !Float
  , fmAscent :: {-# UNPACK #-} !Float
  -- | Device pixels per logical unit used to snap glyph quads to the pixel grid.
  , fmSnapScale :: {-# UNPACK #-} !Float
  , fmAdvance :: Char -> Float
  -- ^ A character's advance. A font whose @\' \'@, @\'x\'@ and @\'M\'@ share one,
  -- with no kerning between @\'x\'@ and @\'M\'@, is taken as monospace.
  , fmKerning :: Char -> Char -> Float
  , fmShape :: Text -> Maybe ShapedText
  -- ^ Shaped line metrics when the host prepared metrics for this text.
  , fmGlyph :: Char -> Maybe GlyphQuad
  -- ^ Glyph quad for fallback unshaped rendering.
  , fmBackend :: Maybe FontBackend
  }

-- | Backend IO hooks for preparing metrics and rasterising shaped glyph runs.
data FontBackend = FontBackend
  { fbPrepare :: Text -> IO FontMetrics
  , fbDrawShaped :: Text -> IO (Maybe ShapedGlyphs)
  }

-- | Pure measurement callback used by custom widgets: takes font metrics and
-- available @(width, height)@ and returns desired @(width, height)@.
type CustomMeasureFn = FontMetrics -> (Float, Float) -> (Float, Float)

-- | Prepare an immutable measurement snapshot for text through the backend,
-- or return the supplied metrics when no backend is attached.
{-# INLINE prepareFontMetrics #-}
prepareFontMetrics :: FontMetrics -> Text -> IO FontMetrics
prepareFontMetrics fm txt = maybe (pure fm) (`fbPrepare` txt) (fmBackend fm)

-- | Prepare a finite text workspace for pure multi-label layout algorithms.
prepareFontMetricsMany :: FontMetrics -> [Text] -> IO FontMetrics
prepareFontMetricsMany fm texts = case fmBackend fm of
  Nothing -> pure fm
  Just _ -> case texts of
    [] -> pure fm
    _ -> do
      preparedList <- mapM (\t -> (t,) <$> prepareFontMetrics fm t) texts
      let !base = case preparedList of
            (_, p) : _ -> p
            [] -> fm
          !byText = Map.fromList [(t, fmShape p t) | (t, p) <- preparedList]
      pure base {fmShape = \t -> Map.findWithDefault Nothing t byText}

-- | Prepare metrics and measure a single line's advance in logical pixels.
{-# INLINE lineWidthIO #-}
lineWidthIO :: FontMetrics -> Text -> IO Float
lineWidthIO fm txt = do
  prepared <- prepareFontMetrics fm txt
  pure $! lineWidth prepared txt

-- | Prepare metrics and measure a single line's advance and the line height,
-- in logical pixels. Split text at newlines first.
{-# INLINE measureTextIO #-}
measureTextIO :: FontMetrics -> Text -> IO (Float, Float)
measureTextIO fm txt = do
  prepared <- prepareFontMetrics fm txt
  pure $! (lineWidth prepared txt, fmLineHeight prepared)

-- | The glyph quads of a shaped line, placing glyphs in the host's atlas as
-- needed; 'Nothing' when the host does not shape.
{-# INLINE drawShaped #-}
drawShaped :: FontMetrics -> Text -> IO (Maybe ShapedGlyphs)
drawShaped fm txt = maybe (pure Nothing) (`fbDrawShaped` txt) (fmBackend fm)

-- | A glyph's quad from the metrics snapshot.
{-# INLINE drawGlyph #-}
drawGlyph :: FontMetrics -> Char -> IO (Maybe GlyphQuad)
drawGlyph fm c = pure (fmGlyph fm c)

-- | Headless metrics with square cells of the given logical size, ascent 80%
-- of cell height, and no drawable glyphs or shaping backend.
monospaceMetrics :: Float -> FontMetrics
monospaceMetrics cell =
  FontMetrics
    { fmLineHeight = cell
    , fmAscent = cell * 0.8
    , fmSnapScale = 1.0
    , fmAdvance = \_ -> cell
    , fmKerning = \_ _ -> 0
    , fmShape = \_ -> Nothing
    , fmGlyph = \_ -> Nothing
    , fmBackend = Nothing
    }

-- | Scale logical measurements and glyph geometry by a positive factor.
-- Atlas UVs and display snap scale remain unchanged.
scaleFontMetrics :: Float -> FontMetrics -> FontMetrics
scaleFontMetrics s fm
  | s == 1.0 = fm
  | otherwise =
      fm
        { fmLineHeight = fmLineHeight fm * s
        , fmAscent = fmAscent fm * s
        , fmAdvance = \c -> fmAdvance fm c * s
        , fmKerning = \a b -> fmKerning fm a b * s
        , fmShape = \t -> fmap scaleShape (fmShape fm t)
        , fmGlyph = fmap scaleGlyph . fmGlyph fm
        , fmBackend = fmap scaleBackend (fmBackend fm)
        }
  where
    scaleBackend backend = FontBackend
      { fbPrepare = \t -> scaleFontMetrics s <$> fbPrepare backend t
      , fbDrawShaped = \t -> fmap (fmap scaleGlyphs) (fbDrawShaped backend t)
      }
    scaleGlyph gq = gq
      { gqX = gqX gq * s, gqY = gqY gq * s
      , gqW = gqW gq * s, gqH = gqH gq * s
      }
    scaleShape st =
      st
        { stAdvance = stAdvance st * s
        , stInkEnd = stInkEnd st * s
        , stCarets = mapPrimArray (* s) (stCarets st)
        }
    scaleGlyphs (ShapedGlyphs quads) =
      ShapedGlyphs (imapPrimArray (\i v -> if i `mod` 8 < 4 then v * s else v) quads)

-- | Give ASCII digits (@'0'@ to @'9'@) a uniform tabular advance equal to the
-- widest digit in @fm@, centring each digit's glyph ink inside that cell and
-- zeroing kerning around digits so numeric columns align and live counters
-- never jitter horizontally. Returns @fm@ unchanged when the font is already
-- monospace.
tabularFontMetrics :: FontMetrics -> FontMetrics
tabularFontMetrics fm
  | isJust (monospaceAdvance fm) = fm
  | otherwise = wrap fm
  where
    !snap = fmSnapScale fm
    !digitAdv = onGrid snap (maximum [fmAdvance fm d | d <- ['0' .. '9']])
    isTabDigit c = c >= '0' && c <= '9'
    wrap src =
      src
        { fmAdvance = \c -> if isTabDigit c then digitAdv else fmAdvance src c
        , fmKerning = \a b -> if isTabDigit a || isTabDigit b then 0 else fmKerning src a b
        , fmShape = \t ->
            if T.any isTabDigit t
              then fmap (tabularShape snap digitAdv t) (fmShape src t)
              else fmShape src t
        , fmGlyph = \c ->
            if isTabDigit c
              then fmap (\gq -> gq {gqX = gqX gq + onGrid snap ((digitAdv - fmAdvance src c) * 0.5)}) (fmGlyph src c)
              else fmGlyph src c
        , fmBackend = fmap wrapBackend (fmBackend src)
        }
    wrapBackend backend =
      FontBackend
        { fbPrepare = \t -> wrap <$> fbPrepare backend t
        , fbDrawShaped = \t ->
            if not (T.any isTabDigit t)
              then fbDrawShaped backend t
              else do
                prepared <- fbPrepare backend t
                mGlyphs <- fbDrawShaped backend t
                pure $ case (fmShape prepared t, mGlyphs) of
                  (Just st, Just gs) -> Just (tabularGlyphs snap digitAdv t st gs)
                  (_, m) -> m
        }

tabularOffsets :: Float -> Float -> Text -> PrimArray Float -> (Float, PrimArray Float, PrimArray Float)
tabularOffsets snap digitAdv txt carets = runST $ do
  let !n = T.length txt
  shifts <- newPrimArray n
  stops <- newPrimArray (n + 1)
  writePrimArray stops 0 (indexPrimArray carets 0)
  let go !j !acc
        | j >= n = pure acc
        | otherwise = do
            let c0 = indexPrimArray carets j
                c1 = indexPrimArray carets (j + 1)
                origW = c1 - c0
                c = T.index txt j
                (!acc', !gShift)
                  | c >= '0' && c <= '9' && origW >= 0 =
                      let d = digitAdv - origW in (acc + d, acc + onGrid snap (d * 0.5))
                  | otherwise = (acc, acc)
            writePrimArray shifts j gShift
            writePrimArray stops (j + 1) (c1 + acc')
            go (j + 1) acc'
  totalDelta <- go 0 0
  (totalDelta,,) <$> unsafeFreezePrimArray shifts <*> unsafeFreezePrimArray stops

tabularShape :: Float -> Float -> Text -> ShapedText -> ShapedText
tabularShape snap digitAdv txt st@(ShapedText adv ink carets)
  | T.null txt || sizeofPrimArray carets /= T.length txt + 1 = st
  | otherwise =
      let (totalDelta, _, carets') = tabularOffsets snap digitAdv txt carets
       in ShapedText (adv + totalDelta) (max 0 (ink + totalDelta)) carets'

tabularGlyphs :: Float -> Float -> Text -> ShapedText -> ShapedGlyphs -> ShapedGlyphs
tabularGlyphs snap digitAdv txt (ShapedText _ _ carets) sg@(ShapedGlyphs quads)
  | T.null txt || sizeofPrimArray carets /= T.length txt + 1 = sg
  | otherwise =
      let !n = T.length txt
          (_, shifts, _) = tabularOffsets snap digitAdv txt carets
          shiftFor midX = go 0
            where
              go !j
                | j >= n - 1 = indexPrimArray shifts (n - 1)
                | otherwise =
                    let a = indexPrimArray carets j
                        b = indexPrimArray carets (j + 1)
                        lo = min a b
                        hi = max a b
                     in if midX <= hi + 0.01 && (j == 0 || midX >= lo - 0.01)
                          then indexPrimArray shifts j
                          else go (j + 1)
          adjust i v
            | i `mod` 8 == 0 =
                let gw = indexPrimArray quads (i + 2)
                 in v + shiftFor (v + gw * 0.5)
            | otherwise = v
       in ShapedGlyphs (imapPrimArray adjust quads)

-- | Vertical pen offset inside a box of height @h@ at top @y@ for text height @th@.
{-# INLINE centeredTextY #-}
centeredTextY :: FontMetrics -> Float -> Float -> Float -> Float
centeredTextY fm y h th =
  case fmGlyph fm 'H' of
    Nothing -> y + (h - th) / 2
    Just gq -> y + onGrid (fmSnapScale fm) (h / 2 - (gqY gq + gqH gq / 2))

-- | Last glyph ink right in the same space as 'NanoUI.Internal.Draw.pushText'.
textInkEnd :: FontMetrics -> Text -> Float
textInkEnd fm txt =
  case T.unsnoc txt of
    Nothing -> 0
    Just (_, c) ->
      case fmShape fm txt of
        Just st -> stInkEnd st
        Nothing ->
          let pen = lineWidth fm txt - fmAdvance fm c
           in case fmGlyph fm c of
                Just gq -> pen + gqX gq + gqW gq
                Nothing -> pen + fmAdvance fm c

-- | Horizontal pen X and used width inside a node box of @(x, w)@ with horizontal
-- inset @ix@.
alignedTextPen :: AlignX -> Float -> Float -> Float -> FontMetrics -> Text -> (Float, Float)
alignedTextPen ax x w ix fm txt =
  let tw = lineWidth fm txt
      contentW = max 0 (w - 2 * ix)
      used = min tw contentW
      shift = if tw > contentW then used else min (textInkEnd fm txt) contentW
      tx = case ax of
        AlignStart -> x + ix
        AlignEnd -> x + w - ix - shift
        AlignCenter -> x + ix + (contentW - shift) / 2
   in (tx, used)

-- | Decoration stroke thickness, underline offset from line top, and
-- strikethrough offset from line top, given a font's line height and ascent.
{-# INLINE textDecorationMetrics #-}
textDecorationMetrics :: Float -> Float -> (Float, Float, Float)
textDecorationMetrics lineH ascent =
  (max 1 (0.06 * lineH), ascent + max 1 (0.1 * lineH), ascent * 0.65)

-- | Scroll offset that keeps @[spanStart, spanStart + spanSize]@ inside a
-- viewport of extent @viewExtent@ over @contentExtent@ of content, starting
-- from @oldScroll@.
{-# INLINE revealSpanInViewport #-}
revealSpanInViewport :: Float -> Float -> Float -> Float -> Float -> Float
revealSpanInViewport oldScroll viewExtent contentExtent spanStart spanSize
  | viewExtent <= 0 = 0
  | otherwise =
      let s0
            | spanStart < oldScroll = spanStart
            | spanStart + spanSize > oldScroll + viewExtent = spanStart + spanSize - viewExtent
            | otherwise = oldScroll
       in clamp 0 (max 0 (contentExtent - viewExtent)) s0

-- | Whether a node uses the ambient base font metrics rather than the host
-- font resolver.
{-# INLINE isDefaultNodeFont #-}
isDefaultNodeFont :: Float -> FontWeight -> FontStyle -> FontVariant -> Bool
isDefaultNodeFont size weight style variant =
  size <= 0
    && weight == WeightNormal
    && style == FontStyleNormal
    && (variant == FontRegular || variant == FontMono)

-- | Advance of @c@ plus its kerning against the previous character.
{-# INLINE kernedAdvance #-}
kernedAdvance :: FontMetrics -> Maybe Char -> Char -> Float
kernedAdvance fm prev c = case prev of
  Nothing -> fmAdvance fm c
  Just p -> fmAdvance fm c + fmKerning fm p c

-- | The character index whose caret is nearest @x@.
textIndexAtX :: FontMetrics -> Text -> Float -> Int
textIndexAtX fm txt x
  | s == e = nearestIdx
  | abs (caretX fm txt s - x) <= abs (caretX fm txt e - x) = s
  | otherwise = e
  where
    (s, e) = clusterBounds txt nearestIdx
    nearestIdx
      | T.null txt = 0
      | Just st <- fmShape fm txt =
          let carets = stCarets st
              n = sizeofPrimArray carets
              nearest !best !bestD !i
                | i >= n = best
                | otherwise =
                    let d = abs (indexPrimArray carets i - x)
                     in if d < bestD then nearest i d (i + 1) else nearest best bestD (i + 1)
           in nearest 0 (1 / 0) 0
      | x <= 0 = 0
      | Just adv <- monospaceAdvance fm, adv > 0 = clamp 0 (T.length txt) (floor (x / adv + 0.5))
      | otherwise = go 0 0.0 Nothing txt
    go !i !acc prev t =
      case T.uncons t of
        Nothing -> i
        Just (c, rest) ->
          let adv = kernedAdvance fm prev c
              mid = acc + adv * 0.5
           in if x < mid then i else go (i + 1) (acc + adv) (Just c) rest

-- | Where the caret before character @i@ of @txt@ sits.
caretX :: FontMetrics -> Text -> Int -> Float
caretX fm txt i = case fmShape fm txt of
  Just st ->
    let carets = stCarets st
     in if sizeofPrimArray carets == 0 then 0 else indexPrimArray carets (clamp 0 (sizeofPrimArray carets - 1) i)
  Nothing -> lineWidth fm (T.take i txt)

-- | Prepare metrics and compute the caret X offset before character @i@.
caretXIO :: FontMetrics -> Text -> Int -> IO Float
caretXIO fm txt i = do
  prepared <- prepareFontMetrics fm txt
  pure $! caretX prepared txt i

-- | The horizontal extents covering characters @lo@ to @hi@, splitting across
-- directional runs when a selection crosses bidi boundaries.
selectionSpans :: FontMetrics -> Text -> Int -> Int -> [(Float, Float)]
selectionSpans fm txt lo hi
  | hi <= lo = []
  | Just st <- fmShape fm txt =
      let carets = stCarets st
          n = sizeofPrimArray carets - 1
          charSpan i =
            let a = indexPrimArray carets i
                b = indexPrimArray carets (i + 1)
             in (min a b, max a b)
          merge [] = []
          merge [one] = [one]
          merge ((a0, a1) : (b0, b1) : rest)
            | b0 <= a1 + 0.5 && b1 >= a0 - 0.5 = merge ((min a0 b0, max a1 b1) : rest)
            | otherwise = (a0, a1) : merge ((b0, b1) : rest)
       in merge [charSpan i | i <- [max 0 lo .. min n hi - 1]]
  | otherwise = [(caretX fm txt lo, caretX fm txt hi)]

-- | Pure single-line advance in logical pixels. Metrics from a shaping
-- backend shape only the text they were prepared for; for any other text
-- this adds per-glyph advances, which can differ from the drawn run on a
-- long line. In a view, measure with 'NanoUI.lineWidthUi', which prepares
-- the metrics first.
lineWidth :: FontMetrics -> Text -> Float
lineWidth fm line
  | T.null line = 0
  | otherwise =
      case fmShape fm line of
        Just st -> stAdvance st
        Nothing -> case monospaceAdvance fm of
          Just adv -> fromIntegral (T.length line) * adv
          Nothing -> case T.uncons line of
            Just (c0, rest) ->
              fst (T.foldl' step (fmAdvance fm c0, c0) rest)
            Nothing -> 0
  where
    step (!w, !prev) c = (w + kernedAdvance fm (Just prev) c, c)

-- | The advance of every character of a font taken as monospace, if constant.
{-# INLINE monospaceAdvance #-}
monospaceAdvance :: FontMetrics -> Maybe Float
monospaceAdvance fm =
  let !spaceAdv = fmAdvance fm ' '
   in if spaceAdv == fmAdvance fm 'x' && spaceAdv == fmAdvance fm 'M' && fmKerning fm 'x' 'M' == 0
        then Just spaceAdv
        else Nothing
