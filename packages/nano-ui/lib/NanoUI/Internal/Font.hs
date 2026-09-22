{-# LANGUAGE StrictData #-}

-- | Font measurement, shaping snapshots, glyph access, and shared text geometry.
-- Layout uses logical pixels; backend callbacks prepare metrics and rasterise glyphs.
module NanoUI.Internal.Font
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
  , WrapResult (..)
  , wrapMeasure
  , wrapTextIO
  , wrapTextLinesIO
  , truncateTextIO
  , lineWidth
  , kernedAdvance
  , textIndexAtX
  , tableCellInset
  , widgetContentInset
  , widgetPadding
  , buttonPadding
  , selectPadding
  , menuOuterPad
  , menuItemPadX
  , menuItemRowH
  , menuSepH
  , menuMinW
  , menuAccentW
  , menuAccentInset
  , centeredTextY
  , alignedTextPen
  , textInkEnd
  , isDefaultNodeFont
  , checkboxBoxSize
  , checkboxLeading
  , treeItemPadding
  , treeRowLeading
  , treeChevronRect
  , scrollBarWidth
  , scrollBarSideGap
  , scrollBarGeomFor
  , scrollBarGap
  , scrollBarGutter
  , ScrollBarSlot (..)
  , classifyScrollBar
  , scrollLayoutGutter
  , sliderHitBounds
  , sliderTrackBounds
  , sliderTrackHeight
  , sliderHandleDiameter
  , sliderHandleSlack
  ) where

import Control.Monad (forM_, unless)
import Data.Char (isSpace)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Primitive.PrimArray (PrimArray, imapPrimArray, indexPrimArray, mapPrimArray, sizeofPrimArray)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Internal.Types (Rect (..), clamp, onGrid)
import NanoUI.Internal.Style (AlignX (..), FontStyle (..), FontVariant (..), FontWeight (..))

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
-- pixels. Valid until the host's glyph atlas next resets.
newtype ShapedGlyphs = ShapedGlyphs (PrimArray Float)
  deriving (Eq, Show)

-- | Logical-pixel font measurements and pure lookup snapshots. Prepare the
-- metrics for the text before pure layout. Native loading and atlas mutation
-- belong in the optional 'FontBackend' IO callbacks.
data FontMetrics = FontMetrics
  { fmLineHeight :: {-# UNPACK #-} !Float
  , fmAscent :: {-# UNPACK #-} !Float
  -- | Device pixels per logical unit used to snap glyph quads to the pixel
  -- grid. The SDL backend sets this to the window pixel density so text lands on
  -- whole device pixels.
  , fmSnapScale :: {-# UNPACK #-} !Float
  , fmAdvance :: Char -> Float
  , fmKerning :: Char -> Char -> Float
  , fmShape :: Text -> Maybe ShapedText
  -- ^ The shaped layout of a text the snapshot was prepared for, when the
  -- host shapes. Other texts fall back to 'fmAdvance' and 'fmKerning'.
  , fmGlyph :: Char -> Maybe GlyphQuad
  -- | Optional effectful backend. Pure callbacks above are immutable metric
  -- snapshots; they must never perform font loading or atlas mutation.
  , fmBackend :: Maybe FontBackend
  }

-- | Text preparation performs font queries in IO and returns an immutable
-- snapshot for pure layout. Rasterisation is separate and occurs during draw.
data FontBackend = FontBackend
  { fbPrepare :: Text -> IO FontMetrics
  , fbDrawShaped :: Text -> IO (Maybe ShapedGlyphs)
  }

-- | Custom node measurement: font metrics and available (width, height) to
-- the node's desired (width, height).
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
  Just _ -> do
    combined <- prepareFontMetrics fm (T.intercalate "\n" texts)
    shapes <- mapM (\t -> do
      prepared <- prepareFontMetrics fm t
      pure (t, fmShape prepared t)) texts
    let !byText = Map.fromList shapes
    pure combined {fmShape = \t -> Map.findWithDefault Nothing t byText}

-- | Prepare metrics and measure a single line's advance in logical pixels.
{-# INLINE lineWidthIO #-}
lineWidthIO :: FontMetrics -> Text -> IO Float
lineWidthIO fm txt = do
  prepared <- prepareFontMetrics fm txt
  pure $! lineWidth prepared txt

-- | Prepare metrics and measure logical width/height, accounting for newlines.
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

-- | A glyph's quad from the metrics snapshot. 'Nothing' means no drawable
-- quad is available, for example for whitespace or a host that shapes.
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
      FontMetrics
        { fmLineHeight = fmLineHeight fm * s
        , fmAscent = fmAscent fm * s
        -- Snap scale is a display property, not a font-size property.
        , fmSnapScale = fmSnapScale fm
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
    -- UVs stay in normalised atlas space; only positions and sizes scale.
    scaleGlyphs (ShapedGlyphs quads) =
      ShapedGlyphs (imapPrimArray (\i v -> if i `mod` 8 < 4 then v * s else v) quads)

-- | Horizontal text inset of a table cell. Zebra and header fills use the full cell rect.
tableCellInset :: Float
tableCellInset = 6

-- | Per-side horizontal/vertical content inset: 1.25 space advances on each axis.
{-# INLINE widgetContentInset #-}
widgetContentInset :: FontMetrics -> (Float, Float)
widgetContentInset fm =
  let pad = fmAdvance fm ' ' * 1.25
   in (pad, pad)

{-# INLINE buttonPadding #-}
buttonPadding :: FontMetrics -> (Float, Float)
buttonPadding fm =
  let adv = fmAdvance fm ' '
      lh = fmLineHeight fm
   in (adv * 2.0, lh * 0.30)

{-# INLINE selectPadding #-}
selectPadding :: FontMetrics -> (Float, Float)
selectPadding fm =
  let adv = fmAdvance fm ' '
      lh = fmLineHeight fm
   in (adv * 2.0, lh * 0.50)

-- Menu metrics shared by the text-field context menu painter, the generic
-- context-menu widgets, and the layout/paint passes, so both menus render
-- identically by construction.

-- | Blank border between the menu panel edge and its rows.
menuOuterPad :: Float
menuOuterPad = 6

-- | Extra horizontal inset of a menu row's label past 'menuOuterPad'.
menuItemPadX :: Float
menuItemPadX = 10

-- | Fixed height of one menu row.
menuItemRowH :: Float
menuItemRowH = 28

-- | Height of a separator band inside a menu.
menuSepH :: Float
menuSepH = 9

-- | Floor for the menu panel width.
menuMinW :: Float
menuMinW = 148

-- | Width of the hover accent marker painted at a menu row's left edge.
menuAccentW :: Float
menuAccentW = 2

-- | Gap between the hover accent marker and the row's top and bottom edges.
menuAccentInset :: Float
menuAccentInset = 3

{-# INLINE centeredTextY #-}
centeredTextY :: FontMetrics -> Float -> Float -> Float -> Float
centeredTextY fm y h th =
  case fmGlyph fm 'H' of
    Nothing -> y + (h - th) / 2
    Just gq -> y + onGrid (fmSnapScale fm) (h / 2 - (gqY gq + gqH gq / 2))
  where
    -- Snap the (constant) baseline offset to the device grid rather than the
    -- whole pen: pen = snap(y + offset) rounds a fractional offset with ties
    -- to even, so adjacent rows (and the same row across a sub-pixel scroll)
    -- land on alternating device pixels while the geometry beside them stays
    -- rigid. Snapping only the constant offset keeps every row fixed on the
    -- grid no matter where y falls.

-- Last glyph ink right in the same space as 'pushText' (pen + gqX + gqW).
-- Falls back to advance when 'fmGlyph' is Nothing (tests).
textInkEnd :: FontMetrics -> Text -> Float
textInkEnd fm txt =
  case T.unsnoc txt of
    Nothing -> 0
    Just (prefix, c) ->
      case fmShape fm txt of
        Just st -> stInkEnd st
        Nothing ->
          let pen = lineWidth fm prefix
           in case fmGlyph fm c of
                Just gq -> pen + gqX gq + gqW gq
                Nothing -> pen + fmAdvance fm c

-- Align using per-glyph advances (same as 'pushText'), not TTF_GetStringSize.
-- When the line fits, AlignEnd/Center shift by ink so the visual right edge
-- stays put as the last character's right bearing changes.
alignedTextPen :: AlignX -> Float -> Float -> Float -> FontMetrics -> Text -> (Float, Float)
alignedTextPen ax x w ix fm txt =
  let tw = lineWidth fm txt
      contentW = max 0 (w - 2 * ix)
      used = min tw contentW
      -- Origin inside the node box, inset on all AlignX sides.
      shift = if tw > contentW then used else min (textInkEnd fm txt) contentW
      tx = case ax of
        AlignStart -> x + ix
        AlignEnd -> x + w - ix - shift
        AlignCenter -> x + ix + (contentW - shift) / 2
   in (tx, used)

-- | Total horizontal and vertical content padding, twice 'widgetContentInset'.
{-# INLINE widgetPadding #-}
widgetPadding :: FontMetrics -> (Float, Float)
widgetPadding fm =
  let (cx, cy) = widgetContentInset fm
   in (2 * cx, 2 * cy)

{-# INLINE checkboxBoxSize #-}
checkboxBoxSize :: FontMetrics -> Float
checkboxBoxSize fm = clamp 18 22 (fmLineHeight fm * 1.15)

{-# INLINE checkboxLeading #-}
checkboxLeading :: FontMetrics -> Float
checkboxLeading fm = checkboxBoxSize fm + 8

-- | Total tree-row x/y padding: zero horizontally and at least 8 logical pixels vertically.
{-# INLINE treeItemPadding #-}
treeItemPadding :: FontMetrics -> (Float, Float)
treeItemPadding fm =
  let lh = fmLineHeight fm
   in (0, max 8 (fromIntegral (round (lh * 0.40) :: Int)))

{-# INLINE treeIndentStep #-}
treeIndentStep :: FontMetrics -> Float
treeIndentStep fm = max 12 (fmLineHeight fm * 0.85)

{-# INLINE treeChevronLeading #-}
treeChevronLeading :: FontMetrics -> Float
treeChevronLeading fm = checkboxBoxSize fm + 6

{-# INLINE treeRowLeading #-}
treeRowLeading :: FontMetrics -> Int -> Float
treeRowLeading fm depth =
  treeIndentStep fm * fromIntegral (max 0 depth) + treeChevronLeading fm

{-# INLINE treeChevronRect #-}
treeChevronRect :: FontMetrics -> Float -> Float -> Float -> Int -> Rect
treeChevronRect fm x y h depth =
  let indent = treeIndentStep fm * fromIntegral (max 0 depth)
      lead = max 1 (treeChevronLeading fm)
   in Rect (x + indent) y lead h

sliderTrackHeight :: Float
sliderTrackHeight = 10

sliderHandleDiameter :: Float
sliderHandleDiameter = 18

sliderHandleSlack :: Float
sliderHandleSlack = (sliderHandleDiameter - sliderTrackHeight) / 2

-- | A 10-pixel-high track centred vertically within x/y/width/height bounds.
{-# INLINE sliderTrackBounds #-}
sliderTrackBounds :: Float -> Float -> Float -> Float -> Rect
sliderTrackBounds x y w h =
  let trackY = y + max 0 ((h - sliderTrackHeight) / 2)
   in Rect x trackY (max 0 w) sliderTrackHeight

-- | The track grown by the handle's overhang: where a drag or a hover on the
-- handle counts. The painter and the cursor must agree on this.
{-# INLINE sliderHitBounds #-}
sliderHitBounds :: Float -> Float -> Float -> Float -> Rect
sliderHitBounds x y w h =
  let Rect tx ty tw th = sliderTrackBounds x y w h
   in Rect tx (ty - sliderHandleSlack) tw (th + 2 * sliderHandleSlack)

-- | Thickness of a list or page scrollbar.
scrollBarWidth :: Float
scrollBarWidth = 8

-- Window bodies take a slimmer bar.
scrollBarSlimWidth :: Float
scrollBarSlimWidth = 4

scrollBarMargin :: Float
scrollBarMargin = 3

-- | The sliver between a page or window bar and the outer edge, and the
-- smallest gap on either side of a list bar.
scrollBarSideGap :: Float
scrollBarSideGap = 3

-- | Bar width and end margin for a slot.
scrollBarGeomFor :: ScrollBarSlot -> (Float, Float)
scrollBarGeomFor slot =
  case slot of
    ScrollBarList -> (scrollBarWidth, scrollBarMargin)
    ScrollBarPage -> (scrollBarWidth, scrollBarMargin)
    -- Window bar: side gaps only. No end inset.
    ScrollBarWindow -> (scrollBarSlimWidth, 0)

-- | The layout arena stores a scroller's slot as its 'Enum' value, and every
-- other node reads a zero there, so 'ScrollBarList' comes first.
data ScrollBarSlot = ScrollBarList | ScrollBarPage | ScrollBarWindow
  deriving (Eq, Show, Enum)

classifyScrollBar :: Bool -> Bool -> ScrollBarSlot
classifyScrollBar isWindowBody isPageGrow
  | isWindowBody = ScrollBarWindow
  | isPageGrow = ScrollBarPage
  | otherwise = ScrollBarList

-- | Gap between the content and a bar, given the padding @trailPad@ on the
-- bar's side: the padding itself, never under 'scrollBarSideGap'.
scrollBarGap :: Float -> Float
scrollBarGap trailPad = max scrollBarSideGap trailPad

-- | Space an overflowing scroller takes from its content, beside the padding
-- @trailPad@ on the bar's side, so the content stops one gap before the bar.
-- A list bar keeps a gap to its well's edge as well. A page bar sits a side
-- gap inside the page's edge. A window body's bar sits out in the window's
-- padding, a side gap inside the window's edge, so that padding is the gap
-- and only the bar and the side gap come out of the content.
scrollBarGutter :: ScrollBarSlot -> Float -> Float
scrollBarGutter slot trailPad =
  let (barW, _) = scrollBarGeomFor slot
      gap = scrollBarGap trailPad
   in case slot of
        ScrollBarList -> barW + 2 * gap - trailPad
        ScrollBarPage -> barW + scrollBarSideGap + gap - trailPad
        ScrollBarWindow -> barW + scrollBarSideGap

scrollLayoutGutter :: ScrollBarSlot -> Float -> Float -> Float -> Float
scrollLayoutGutter slot trailPad contentSize innerMain
  | contentSize <= innerMain = 0
  | otherwise = scrollBarGutter slot trailPad

-- | The one policy for "does this node use the ambient base font, or does it
-- need the host resolver?". A zero size with a plain weight/style and the
-- regular or mono variant resolves to the pre-read base metrics; everything
-- else (heading/muted/danger, bold, italic, explicit size) defers to the host.
-- Layout, paint, span placement and hit testing all share this so they cannot
-- pick different faces for the same node.
{-# INLINE isDefaultNodeFont #-}
isDefaultNodeFont :: Float -> FontWeight -> FontStyle -> FontVariant -> Bool
isDefaultNodeFont size weight style variant =
  size <= 0
    && weight == WeightNormal
    && style == FontStyleNormal
    && (variant == FontRegular || variant == FontMono)

-- | Advance of @c@ plus its kerning against the previous character: the one
-- pen step shared by measuring, hit testing and glyph emission.
{-# INLINE kernedAdvance #-}
kernedAdvance :: FontMetrics -> Maybe Char -> Char -> Float
kernedAdvance fm prev c = case prev of
  Nothing -> fmAdvance fm c
  Just p -> fmAdvance fm c + fmKerning fm p c

-- | The character index whose caret is nearest @x@: from the shaped carets
-- when the text was prepared by a shaping host, which handles clusters and
-- right-to-left runs, and otherwise from the same advances and kerning as
-- 'NanoUI.Internal.Draw.pushText', so the caret lands where the glyph to its left was
-- drawn.
textIndexAtX :: FontMetrics -> Text -> Float -> Int
textIndexAtX fm txt x
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
  | otherwise = go 0 0.0 Nothing txt
  where
    go !i !acc prev t =
      case T.uncons t of
        Nothing -> i
        Just (c, rest) ->
          let adv = kernedAdvance fm prev c
              mid = acc + adv * 0.5
           in if x < mid then i else go (i + 1) (acc + adv) (Just c) rest

-- | Where the caret before character @i@ of @txt@ sits: a shaped caret when
-- the snapshot was prepared for @txt@, else the width of the characters
-- before it.
caretX :: FontMetrics -> Text -> Int -> Float
caretX fm txt i = case fmShape fm txt of
  Just st ->
    let carets = stCarets st
     in if sizeofPrimArray carets == 0 then 0 else indexPrimArray carets (clamp 0 (sizeofPrimArray carets - 1) i)
  Nothing -> lineWidth fm (T.take i txt)

caretXIO :: FontMetrics -> Text -> Int -> IO Float
caretXIO fm txt i = do
  prepared <- prepareFontMetrics fm txt
  pure $! caretX prepared txt i

-- | The horizontal extents covering characters @lo@ to @hi@: one span for
-- left-to-right text, and a span per direction run where a selection crosses
-- right-to-left text.
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

-- | Pure single-line advance in logical pixels. Use metrics prepared for this
-- text to include shaping; otherwise uses character advances and kerning.
lineWidth :: FontMetrics -> Text -> Float
lineWidth fm line
  | T.null line = 0
  | otherwise =
      case fmShape fm line of
        Just st -> stAdvance st
        Nothing ->
          let !spaceAdv = fmAdvance fm ' '
              !xAdv = fmAdvance fm 'x'
              !mAdv = fmAdvance fm 'M'
           in if spaceAdv == xAdv && xAdv == mAdv && fmKerning fm 'x' 'M' == 0
                then fromIntegral (T.length line) * spaceAdv
                else case T.uncons line of
                  Just (c0, rest) ->
                    fst (T.foldl' step (fmAdvance fm c0, c0) rest)
                  Nothing -> 0
  where
    step (!w, !prev) c = (w + kernedAdvance fm (Just prev) c, c)

-- | The size of text wrapped to @maxW@ into the lines of @wrap@.
wrapMeasure :: FontMetrics -> Float -> WrapResult -> (Float, Float)
wrapMeasure fm maxW wrap =
  let lineH = fmLineHeight fm
   in case wrLines wrap of
        [] -> (0, lineH)
        textLines -> (min maxW (wrWidest wrap), lineH * fromIntegral (length textLines))

-- | Text wrapped to a width, and the widths it wraps the same at.
data WrapResult = WrapResult
  { wrLines :: [Text]
  -- ^ The lines, as 'wrapTextLinesIO' gives them.
  , wrWidest :: Float
  -- ^ The widest line, which is wider than the width asked for when a
  -- single character is.
  , wrFitW :: Float
  -- ^ The widest line that fit. At any width from here up to 'wrBreakW'
  -- every such line still fits and none could take more, so the lines are
  -- the same.
  , wrBreakW :: Float
  -- ^ The narrowest width at which some line would take another word or
  -- character, or a paragraph would fit whole.
  }
  deriving (Eq, Show)

-- | Wrap each paragraph to @maxW@ using the host line measure: whole words
-- first, characters for words (or paragraphs) that cannot fit. Each line
-- starts from a guess at the paragraph's average character width, which the
-- host measure then confirms, so a paragraph costs a few host measures a line
-- rather than one a word.
wrapTextLinesIO :: (Text -> IO Float) -> Text -> Float -> IO [Text]
wrapTextLinesIO lineW txt maxW = wrLines <$> wrapTextIO lineW txt maxW

-- | 'wrapTextLinesIO' with the widths its result holds for. Each line is the
-- longest that fits, and text only gets wider as it gets longer, so a line
-- is decided by its own width and by the width of it with one more word (or
-- character). The widest of the first and the narrowest of the second bound
-- the widths that wrap the same. Measuring each line with its next word is
-- the extra cost, and the search has measured most of those already.
wrapTextIO :: (Text -> IO Float) -> Text -> Float -> IO WrapResult
wrapTextIO lineW txt maxW = do
  widestRef <- newIORef 0
  fitRef <- newIORef 0
  breakRef <- newIORef (1 / 0)
  let -- A line of the result, @w@ wide.
      lineOf w = do
        modifyIORef' widestRef (max w)
        if w <= maxW then modifyIORef' fitRef (max w) else breakAt w
      -- A width from which the result would differ.
      breakAt w = modifyIORef' breakRef (min w)
      wrapParagraph para
        | T.null para = pure [""]
        | otherwise = do
            w <- lineW para
            let perLine = charsPerWidth w para maxW
            if w <= maxW
              then [para] <$ lineOf w
              else do
                breakAt w
                if T.any (== ' ') para
                  then wrapWords perLine (singleSpaced para) []
                  else reverse <$> charLines perLine para []
      -- Lines keep one space between words, whatever separated them.
      singleSpaced para
        | T.all (\c -> c == ' ' || not (isSpace c)) para
            && not (" " `T.isPrefixOf` para || " " `T.isSuffixOf` para || "  " `T.isInfixOf` para) =
            para
        | otherwise = T.unwords (T.words para)
      -- @rest@ is the paragraph from its next word on, one space between
      -- words, so every candidate line is a slice of it and costs no copy.
      wrapWords perLine = startLine
        where
          startLine rest acc
            | T.null rest = pure (reverse acc)
            | otherwise = do
                let word = T.takeWhile (/= ' ') rest
                width <- lineW word
                if width <= maxW
                  then extend rest (T.length word) acc
                  else do
                    breakAt width
                    -- The last piece of a broken word starts the next line.
                    pieces <- charLines perLine word []
                    case pieces of
                      piece : done -> extend (T.drop (T.length word - T.length piece) rest) (T.length piece) (done ++ acc)
                      [] -> startLine (T.drop (T.length word + 1) rest) acc
          -- Append as many of the next words as fit to the line that starts
          -- @fromLine@ and is @lineLen@ characters long. The line can end at
          -- @lineLen@, before each later space, or at the paragraph's end; the
          -- search starts from the last of those within @perLine@ characters
          -- and steps a word at a time.
          extend fromLine lineLen acc = do
            let fits end = (<= maxW) <$> lineW (T.take end fromLine)
                nextEnd end
                  | T.null (T.drop end fromLine) = Nothing
                  | otherwise = Just (end + 1 + T.length (T.takeWhile (/= ' ') (T.drop (end + 1) fromLine)))
                prevEnd end = T.length (fst (T.breakOnEnd " " (T.take end fromLine))) - 1
                guess
                  | T.compareLength fromLine perLine /= GT = T.length fromLine
                  | otherwise = max lineLen (prevEnd (perLine + 1))
                up end = case nextEnd end of
                  Just end' -> fits end' >>= \ok -> if ok then up end' else pure end
                  Nothing -> pure end
                down end
                  | end' <= lineLen = pure lineLen
                  | otherwise = fits end' >>= \ok -> if ok then pure end' else down end'
                  where
                    end' = prevEnd end
            end <-
              if guess <= lineLen
                then up lineLen
                else fits guess >>= \ok -> if ok then up guess else down guess
            let line = T.take end fromLine
                rest = T.drop (end + 1) fromLine
            lineW line >>= lineOf
            forM_ (nextEnd end) $ \end' -> lineW (T.take end' fromLine) >>= breakAt
            if T.null rest then pure (reverse (line : acc)) else startLine rest (line : acc)
      charLines perLine chunk acc
        | T.null chunk = pure acc
        | otherwise = do
            (line, rest) <- takeWidth perLine lineW maxW chunk
            if T.null line
              then pure acc
              else do
                lineW line >>= lineOf
                unless (T.null rest) $
                  lineW (T.take (T.length line + 1) chunk) >>= breakAt
                charLines perLine rest (line : acc)
  if maxW <= 0
    -- Nothing fits, and no other width wraps to nothing.
    then pure (WrapResult [] 0 1 0)
    else do
      textLines <- concat <$> mapM wrapParagraph (T.lines txt)
      WrapResult textLines <$> readIORef widestRef <*> readIORef fitRef <*> readIORef breakRef

-- | About how many characters of @txt@, which the host measures @w@ wide,
-- fit in @maxW@.
charsPerWidth :: Float -> Text -> Float -> Int
charsPerWidth w txt maxW
  | w <= 0 = maxBound `div` 2
  | otherwise = floor (maxW * fromIntegral (T.length txt) / w)

-- | The largest count from @lo@ to @hi@ that @fits@, where @lo@ fits and
-- fitting is monotone. Probes @guess@ and the count after it first, then
-- widens by doubling steps and bisects, so a close guess costs two probes.
largestFitting :: (Int -> IO Bool) -> Int -> Int -> Int -> IO Int
largestFitting fits lo0 hi0 guess
  | hi0 <= lo0 = pure lo0
  | otherwise = do
      let g = clamp (lo0 + 1) hi0 guess
      ok <- fits g
      if ok then up g 1 else down g 1
  where
    -- @lo@ fits.
    up !lo !step
      | lo >= hi0 = pure lo
      | otherwise = do
          let p = min hi0 (lo + step)
          ok <- fits p
          if ok then up p (step * 2) else bisect lo p
    -- @hi@ does not fit.
    down !hi !step
      | p <= lo0 = bisect lo0 hi
      | otherwise = do
          ok <- fits p
          if ok then bisect p hi else down p (step * 2)
      where
        p = hi - step
    -- @lo@ fits and @hi@ does not.
    bisect !lo !hi
      | hi - lo <= 1 = pure lo
      | otherwise = do
          let mid = (lo + hi) `div` 2
          ok <- fits mid
          if ok then bisect mid hi else bisect lo mid

-- | Split off the longest prefix that fits @maxW@, starting the search at
-- @guess@ characters. Always consume at least one character from non-empty
-- text, even when a single glyph exceeds the available width, so wrapping
-- makes progress.
takeWidth :: Int -> (Text -> IO Float) -> Float -> Text -> IO (Text, Text)
takeWidth guess lineW maxW txt
  | T.null txt = pure (txt, T.empty)
  | otherwise = (`T.splitAt` txt) <$> largestFitting fits 1 maxBound guess
  where
    -- A count past the end does not fit, so the search needs no length.
    fits k
      | T.compareLength txt k == LT = pure False
      | otherwise = (<= maxW) <$> lineW (T.take k txt)

truncateTextIO :: (Text -> IO Float) -> Float -> Text -> IO Text
truncateTextIO lineW maxW txt
  | maxW <= 0 = pure ""
  | otherwise = do
      w <- lineW txt
      if w <= maxW
        then pure txt
        else do
          ellW <- lineW "..."
          if maxW <= ellW
            then fst <$> takeWidth (charsPerWidth w txt maxW) lineW maxW txt
            else do
              (fit, _) <- takeWidth (charsPerWidth w txt (maxW - ellW)) lineW (maxW - ellW) txt
              pure (T.dropWhileEnd (== '.') fit <> "...")
