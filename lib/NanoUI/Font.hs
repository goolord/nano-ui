{-# LANGUAGE StrictData #-}

module NanoUI.Font
  ( GlyphQuad (..)
  , FontMetrics (..)
  , monospaceMetrics
  , measureText
  , measureTextWrapped
  , measureTextWrappedIO
  , wrapTextLines
  , wrapTextLinesIO
  , lineWidth
  , textDisplayWidth
  , labelContentInset
  , widgetContentInset
  , widgetPadding
  , buttonPadding
  , centeredTextY
  , alignedTextBox
  , layoutLineHeight
  , checkboxBoxSize
  , checkboxLeading
  , layoutUnitScale
  , resolveLayoutGap
  , resolveLayoutPadding
  , monoFontMarker
  , hasMonoFontMarker
  , stripMonoFontMarker
  , headingFontMarker
  , hasHeadingMarker
  , mutedFontMarker
  , hasMutedMarker
  , stripWidgetMarkers
  , scrollBarWidth
  , scrollBarWindowWidth
  , scrollBarMargin
  , scrollBarGeom
  , scrollBarGeomFor
  , scrollBarGutter
  , ScrollBarSlot (..)
  , classifyScrollBar
  , scrollLayoutGutter
  , scrollBarOuterGap
  , scrollBarPageExtra
  , scrollBarListExtra
  , scrollBarWindowGutter
  , sliderTrackBounds
  , sliderTrackHeight
  ) where


import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Host (HostProfile, isCellHost)
import NanoUI.Style (AlignX (..), Padding (..), defaultLayout, layoutGap)
import NanoUI.Icons (terminalTextColumns)
import NanoUI.Types (Rect (..), sliderBarCells)

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

data FontMetrics = FontMetrics
  { fmLineHeight :: {-# UNPACK #-} !Float
  , fmAscent :: {-# UNPACK #-} !Float
  , fmAdvance :: Char -> Float
  , fmGlyph :: Char -> Maybe GlyphQuad
  }

{-# INLINE monospaceMetrics #-}
monospaceMetrics :: Float -> FontMetrics
monospaceMetrics cell =
  FontMetrics
    { fmLineHeight = cell
    , fmAscent = cell * 0.8
    , fmAdvance = \_ -> cell
    , fmGlyph = \_ -> Nothing
    }

-- Layout gap/pad are authored in pixel steps (see defaultLayout). Cell hosts map one cell per step.
{-# INLINE layoutUnitScale #-}
layoutUnitScale :: HostProfile -> Float
layoutUnitScale host
  | isCellHost host = 1 / layoutGap defaultLayout
  | otherwise = 1

{-# INLINE resolveLayoutGap #-}
resolveLayoutGap :: HostProfile -> FontMetrics -> Float -> Float
resolveLayoutGap host _fm g = g * layoutUnitScale host

{-# INLINE resolveLayoutPadding #-}
resolveLayoutPadding :: HostProfile -> FontMetrics -> Padding -> Padding
resolveLayoutPadding host _fm (Padding l t r b) =
  let s = layoutUnitScale host
   in Padding (l * s) (t * s) (r * s) (b * s)

monoFontMarker :: Text
monoFontMarker = T.singleton '\x02'

{-# INLINE hasMonoFontMarker #-}
hasMonoFontMarker :: Text -> Bool
hasMonoFontMarker txt = T.take 1 txt == monoFontMarker

{-# INLINE stripMonoFontMarker #-}
stripMonoFontMarker :: Text -> Text
stripMonoFontMarker txt =
  if hasMonoFontMarker txt
    then T.drop 1 txt
    else txt

headingFontMarker :: Text
headingFontMarker = T.singleton '\x03'

{-# INLINE hasHeadingMarker #-}
hasHeadingMarker :: Text -> Bool
hasHeadingMarker txt = T.take 1 txt == headingFontMarker

mutedFontMarker :: Text
mutedFontMarker = T.singleton '\x04'

{-# INLINE hasMutedMarker #-}
hasMutedMarker :: Text -> Bool
hasMutedMarker txt = T.take 1 txt == mutedFontMarker

{-# INLINE stripWidgetMarkers #-}
stripWidgetMarkers :: Text -> Text
stripWidgetMarkers txt =
  if hasMonoFontMarker txt || hasHeadingMarker txt || hasMutedMarker txt
    then T.drop 1 txt
    else txt

{-# INLINE labelContentInset #-}
labelContentInset :: HostProfile -> FontMetrics -> (Float, Float)
labelContentInset host fm
  | isCellHost host = (0, 0)
  | otherwise = (0.55 * fmAdvance fm ' ', 0.16 * layoutLineHeight host fm)

{-# INLINE widgetContentInset #-}
widgetContentInset :: HostProfile -> FontMetrics -> (Float, Float)
widgetContentInset host fm
  | isCellHost host = (fmAdvance fm ' ', 0)
  | otherwise = (fmAdvance fm ' ' * 1.25, 0.28 * layoutLineHeight host fm)

{-# INLINE buttonPadding #-}
buttonPadding :: HostProfile -> FontMetrics -> (Float, Float)
buttonPadding host fm
  | isCellHost host = (0, 0)
  | otherwise =
      let adv = fmAdvance fm ' '
          lh = layoutLineHeight host fm
       in (adv * 2.0, lh * 0.30)

{-# INLINE layoutLineHeight #-}
layoutLineHeight :: HostProfile -> FontMetrics -> Float
layoutLineHeight host fm
  | isCellHost host = fmLineHeight fm
  | otherwise = fmLineHeight fm * 0.82

{-# INLINE centeredTextY #-}
centeredTextY :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float
centeredTextY host fm y h th
  | isCellHost host = y + (h - th) / 2
  | otherwise =
      let slack = max 0 (th - fmAscent fm)
       in y + (h - th) / 2 - slack * 0.45

-- Origin and used width inside the node box, inset on all AlignX sides.
{-# INLINE alignedTextBox #-}
alignedTextBox :: AlignX -> Float -> Float -> Float -> Float -> (Float, Float)
alignedTextBox ax x w ix tw =
  let contentW = max 0 (w - 2 * ix)
      used = min tw contentW
      tx = case ax of
        AlignEnd -> x + w - ix - used
        AlignCenter -> x + ix + (contentW - used) / 2
        AlignStart -> x + ix
   in (tx, used)

{-# INLINE widgetPadding #-}
widgetPadding :: HostProfile -> FontMetrics -> (Float, Float)
widgetPadding host fm =
  let (cx, cy) = widgetContentInset host fm
   in (2 * cx, 2 * cy)

{-# INLINE checkboxBoxSize #-}
checkboxBoxSize :: HostProfile -> FontMetrics -> Float
checkboxBoxSize host fm
  | isCellHost host = fmLineHeight fm
  | otherwise = min 22 (max 18 (fmLineHeight fm * 1.15))

{-# INLINE checkboxLeading #-}
checkboxLeading :: HostProfile -> FontMetrics -> Float
checkboxLeading host fm
  | isCellHost host = 0
  | otherwise = checkboxBoxSize host fm + 8

sliderTrackHeight :: Float
sliderTrackHeight = 10

sliderTrackMargin :: Float
sliderTrackMargin = 3

-- Pixel hosts: track spans the label row insets. Cell hosts: inline [bar] cells.
{-# INLINE sliderTrackBounds #-}
sliderTrackBounds :: HostProfile -> FontMetrics -> Text -> Float -> Float -> Float -> Float -> Rect
sliderTrackBounds host fm lbl x y w h
  | isCellHost host =
      let adv = fmAdvance fm ' '
          (ix, _) = widgetContentInset host fm
          prefix = lineWidth fm (lbl <> " ")
          trackW = fromIntegral (sliderBarCells + 2) * adv
       in Rect (x + ix + prefix) y trackW h
  | otherwise =
      let (lx, _) = labelContentInset host fm
          bandH = max 4 (h * 0.18)
          bandY = y + h - bandH - sliderTrackMargin
          trackY = bandY + (bandH - sliderTrackHeight) / 2
          trackX = x + lx
          trackW = max 0 (w - 2 * lx)
       in Rect trackX trackY trackW sliderTrackHeight

scrollBarWidth :: Float
scrollBarWidth = 8

-- Thinner than list/page so the body gutter stays small.
scrollBarWindowWidth :: Float
scrollBarWindowWidth = 4

scrollBarMargin :: Float
scrollBarMargin = 3

scrollBarGeom :: HostProfile -> FontMetrics -> (Float, Float)
scrollBarGeom host fm = scrollBarGeomFor host fm ScrollBarList

scrollBarGeomFor :: HostProfile -> FontMetrics -> ScrollBarSlot -> (Float, Float)
scrollBarGeomFor host _fm slot =
  if isCellHost host
    then (1, 0)
    else
      let barW = case slot of
            ScrollBarWindow -> scrollBarWindowWidth
            _ -> scrollBarWidth
          -- Window bar: side gaps only. No end inset.
          endM = case slot of
            ScrollBarWindow -> 0
            _ -> scrollBarMargin
       in (barW, endM)

-- Bar plus end margin. List/page overflow reserves this on the cross axis.
scrollBarGutter :: HostProfile -> FontMetrics -> Float
scrollBarGutter host fm =
  let (barW, barMargin) = scrollBarGeom host fm
   in barW + barMargin

data ScrollBarSlot = ScrollBarPage | ScrollBarList | ScrollBarWindow
  deriving (Eq, Show)

classifyScrollBar :: Bool -> Bool -> ScrollBarSlot
classifyScrollBar isWindowBody isPageGrow
  | isWindowBody = ScrollBarWindow
  | isPageGrow = ScrollBarPage
  | otherwise = ScrollBarList

-- Extra inset from the page scroll's right edge. Reserved in layout.
scrollBarPageExtra :: Float
scrollBarPageExtra = 4

-- Extra inset from a list well's right edge. Reserved in layout.
scrollBarListExtra :: Float
scrollBarListExtra = 3

-- Gap on both sides of a window body bar.
scrollBarWindowSide :: Float
scrollBarWindowSide = 2

scrollLayoutGutter :: HostProfile -> FontMetrics -> ScrollBarSlot -> Float -> Float -> Float
scrollLayoutGutter host fm slot contentSize innerMain
  | contentSize <= innerMain = 0
  | otherwise =
      case slot of
        -- Window bar hangs into the parent pad. Content keeps the full inner width.
        ScrollBarWindow -> 0
        ScrollBarList -> scrollBarGutter host fm + scrollBarListExtra
        ScrollBarPage -> scrollBarGutter host fm + scrollBarPageExtra

scrollBarOuterGap :: HostProfile -> FontMetrics -> ScrollBarSlot -> Float
scrollBarOuterGap host _fm slot =
  if isCellHost host
    then 0
    else
      case slot of
        ScrollBarList -> scrollBarListExtra
        ScrollBarPage -> scrollBarPageExtra
        ScrollBarWindow -> scrollBarWindowSide

-- Width the window bar occupies in the parent pad (not taken from content).
scrollBarWindowGutter :: HostProfile -> FontMetrics -> Float
scrollBarWindowGutter host fm =
  let (barW, _) = scrollBarGeomFor host fm ScrollBarWindow
      side = scrollBarOuterGap host fm ScrollBarWindow
   in barW + 2 * side

measureText :: HostProfile -> FontMetrics -> Text -> (Float, Float)
measureText host fm txt =
  let h = fmLineHeight fm
      w =
        if isCellHost host
          then fromIntegral (terminalTextColumns txt)
          else fromIntegral (T.length txt) * fmAdvance fm ' '
   in (w, h)

-- | Line width for hit testing and centering. Cell hosts use column counts.
textDisplayWidth :: HostProfile -> FontMetrics -> Text -> Float
textDisplayWidth host fm txt =
  if isCellHost host
    then fromIntegral (terminalTextColumns txt)
    else lineWidth fm txt

measureTextWrapped :: HostProfile -> FontMetrics -> Text -> Float -> (Float, Float)
measureTextWrapped host fm txt maxW =
  let lineH = fmLineHeight fm
      textLines = wrapTextLines host fm txt maxW
   in wrappedSize lineW lineH maxW textLines
  where
    lineW = textDisplayWidth host fm

measureTextWrappedIO :: (Text -> IO Float) -> FontMetrics -> Text -> Float -> IO (Float, Float)
measureTextWrappedIO lineW fm txt maxW = do
  textLines <- wrapTextLinesIO lineW fm txt maxW
  ws <- mapM lineW textLines
  pure (wrappedSizeFrom (fmLineHeight fm) maxW textLines ws)

wrappedSize :: (Text -> Float) -> Float -> Float -> [Text] -> (Float, Float)
wrappedSize lineW lineH maxW textLines =
  wrappedSizeFrom lineH maxW textLines (map lineW textLines)

wrappedSizeFrom :: Float -> Float -> [Text] -> [Float] -> (Float, Float)
wrappedSizeFrom lineH maxW textLines ws =
  case textLines of
    [] -> (0, lineH)
    _ -> (min maxW (maximum ws), lineH * fromIntegral (length textLines))

wrapTextLines :: HostProfile -> FontMetrics -> Text -> Float -> [Text]
wrapTextLines host fm txt maxW = wrapTextLinesWith (textDisplayWidth host fm) txt maxW

wrapTextLinesWith :: (Text -> Float) -> Text -> Float -> [Text]
wrapTextLinesWith lineW txt maxW =
  concatMap (\para -> wrapParagraphWith lineW para maxW) (T.lines txt)

wrapTextLinesIO :: (Text -> IO Float) -> FontMetrics -> Text -> Float -> IO [Text]
wrapTextLinesIO lineW _ txt maxW =
  concat <$> mapM (\para -> wrapParagraphIO lineW para maxW) (T.lines txt)

wrapParagraphWith :: (Text -> Float) -> Text -> Float -> [Text]
wrapParagraphWith lineW para maxW
  | maxW <= 0 = []
  | T.null para = [""]
  | T.any (== ' ') para = wrapWordsWith lineW (T.words para) maxW []
  | otherwise = reverse (charLinesWith lineW maxW para [])

wrapParagraphIO :: (Text -> IO Float) -> Text -> Float -> IO [Text]
wrapParagraphIO lineW para maxW
  | maxW <= 0 = pure []
  | T.null para = pure [""]
  | T.any (== ' ') para = wrapWordsIO lineW (T.words para) maxW []
  | otherwise = reverse <$> charLinesIO lineW maxW para []

wrapWordsWith :: (Text -> Float) -> [Text] -> Float -> [Text] -> [Text]
wrapWordsWith _ [] _ acc = reverse acc
wrapWordsWith lineW (w : ws) maxW acc =
  case acc of
    [] ->
      if lineW w <= maxW
        then wrapWordsWith lineW ws maxW [w]
        else wrapWordsWith lineW ws maxW (charLinesWith lineW maxW w [])
    (line : rest) ->
      let candidate = line <> " " <> w
       in if lineW candidate <= maxW
            then wrapWordsWith lineW ws maxW (candidate : rest)
            else
              if lineW w <= maxW
                then wrapWordsWith lineW ws maxW (w : line : rest)
                else wrapWordsWith lineW ws maxW (charLinesWith lineW maxW w [] ++ (line : rest))

wrapWordsIO :: (Text -> IO Float) -> [Text] -> Float -> [Text] -> IO [Text]
wrapWordsIO _ [] _ acc = pure (reverse acc)
wrapWordsIO lineW (w : ws) maxW acc =
  case acc of
    [] -> do
      wW <- lineW w
      if wW <= maxW
        then wrapWordsIO lineW ws maxW [w]
        else do
          broken <- charLinesIO lineW maxW w []
          wrapWordsIO lineW ws maxW broken
    (line : rest) -> do
      let candidate = line <> " " <> w
      cW <- lineW candidate
      if cW <= maxW
        then wrapWordsIO lineW ws maxW (candidate : rest)
        else do
          wW <- lineW w
          if wW <= maxW
            then wrapWordsIO lineW ws maxW (w : line : rest)
            else do
              broken <- charLinesIO lineW maxW w []
              wrapWordsIO lineW ws maxW (broken ++ (line : rest))

charLinesWith :: (Text -> Float) -> Float -> Text -> [Text] -> [Text]
charLinesWith lineW maxW txt acc =
  if T.null txt
    then acc
    else
      let (line, rest) = takeWidthWith lineW maxW txt
       in if T.null line
            then acc
            else charLinesWith lineW maxW rest (line : acc)

charLinesIO :: (Text -> IO Float) -> Float -> Text -> [Text] -> IO [Text]
charLinesIO lineW maxW txt acc =
  if T.null txt
    then pure acc
    else do
      (line, rest) <- takeWidthIO lineW maxW txt
      if T.null line
        then pure acc
        else charLinesIO lineW maxW rest (line : acc)

lineWidth :: FontMetrics -> Text -> Float
lineWidth fm line =
  T.foldl' (\w c -> w + fmAdvance fm c) 0 line

takeWidthWith :: (Text -> Float) -> Float -> Text -> (Text, Text)
takeWidthWith lineW maxW txt = go 1
  where
    go k
      | k > T.length txt = (txt, T.empty)
      | otherwise =
          let chunk = T.take k txt
           in if lineW chunk > maxW
                then
                  if k == 1
                    then (chunk, T.drop 1 txt)
                    else (T.take (k - 1) txt, T.drop (k - 1) txt)
                else go (k + 1)

takeWidthIO :: (Text -> IO Float) -> Float -> Text -> IO (Text, Text)
takeWidthIO lineW maxW txt = go 1
  where
    go k
      | k > T.length txt = pure (txt, T.empty)
      | otherwise = do
          let chunk = T.take k txt
          w <- lineW chunk
          if w > maxW
            then
              if k == 1
                then pure (chunk, T.drop 1 txt)
                else pure (T.take (k - 1) txt, T.drop (k - 1) txt)
            else go (k + 1)
