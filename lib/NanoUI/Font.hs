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
  , labelContentInset
  , widgetContentInset
  , widgetPadding
  , buttonPadding
  , centeredTextY
  , layoutLineHeight
  , checkboxBoxSize
  , checkboxLeading
  , isTerminalFont
  , monoFontMarker
  , hasMonoFontMarker
  , stripMonoFontMarker
  , headingFontMarker
  , hasHeadingMarker
  , mutedFontMarker
  , hasMutedMarker
  , stripWidgetMarkers
  , scrollBarWidth
  , scrollBarMargin
  , scrollBarGeom
  , scrollBarGutter
  , ScrollBarSlot (..)
  , classifyScrollBar
  , scrollLayoutGutter
  , scrollBarOuterGap
  , scrollBarPageExtra
  , scrollBarListExtra
  , scrollBarWindowHang
  ) where


import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Style (padR, windowPad)

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

{-# INLINE isTerminalFont #-}
isTerminalFont :: FontMetrics -> Bool
isTerminalFont fm = fmLineHeight fm == 1 && fmAdvance fm ' ' == 1

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
labelContentInset :: FontMetrics -> (Float, Float)
labelContentInset fm
  | isTerminalFont fm = (0, 0)
  | otherwise = (0.5 * fmAdvance fm ' ', 0.12 * layoutLineHeight fm)

{-# INLINE widgetContentInset #-}
widgetContentInset :: FontMetrics -> (Float, Float)
widgetContentInset fm
  | isTerminalFont fm = (fmAdvance fm ' ', 0)
  | otherwise = (fmAdvance fm ' ' * 1.15, 0.22 * layoutLineHeight fm)

{-# INLINE buttonPadding #-}
buttonPadding :: FontMetrics -> (Float, Float)
buttonPadding fm
  | isTerminalFont fm =
      let (px, py) = widgetPadding fm
          adv = fmAdvance fm ' '
       in (px + adv * 0.4, py)
  | otherwise =
      let adv = fmAdvance fm ' '
          lh = layoutLineHeight fm
       in (adv * 3.0, lh * 0.42)

{-# INLINE layoutLineHeight #-}
layoutLineHeight :: FontMetrics -> Float
layoutLineHeight fm
  | isTerminalFont fm = fmLineHeight fm
  | otherwise = fmLineHeight fm * 0.82

{-# INLINE centeredTextY #-}
centeredTextY :: FontMetrics -> Float -> Float -> Float -> Float
centeredTextY fm y h th
  | isTerminalFont fm = y + (h - th) / 2
  | otherwise =
      let slack = max 0 (th - fmAscent fm)
       in y + (h - th) / 2 - slack * 0.45

{-# INLINE widgetPadding #-}
widgetPadding :: FontMetrics -> (Float, Float)
widgetPadding fm =
  let (cx, cy) = widgetContentInset fm
   in (2 * cx, 2 * cy)

{-# INLINE checkboxBoxSize #-}
checkboxBoxSize :: FontMetrics -> Float
checkboxBoxSize fm
  | isTerminalFont fm = min 18 (max 14 (fmLineHeight fm * 0.9))
  | otherwise = min 22 (max 18 (fmLineHeight fm * 1.15))

{-# INLINE checkboxLeading #-}
checkboxLeading :: FontMetrics -> Float
checkboxLeading fm
  | isTerminalFont fm = 0
  | otherwise = checkboxBoxSize fm + 8

scrollBarWidth :: Float
scrollBarWidth = 8

scrollBarMargin :: Float
scrollBarMargin = 3

scrollBarGeom :: FontMetrics -> (Float, Float)
scrollBarGeom fm =
  if isTerminalFont fm
    then (1, 0)
    else (scrollBarWidth, scrollBarMargin)

-- Bar plus end margin. List/page overflow reserves this on the cross axis.
scrollBarGutter :: FontMetrics -> Float
scrollBarGutter fm =
  let (barW, barMargin) = scrollBarGeom fm
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

-- Tiny gap on both sides of a hanging window bar (content and frame).
scrollBarWindowSide :: Float
scrollBarWindowSide = 4

scrollLayoutGutter :: FontMetrics -> ScrollBarSlot -> Float -> Float -> Float
scrollLayoutGutter fm slot contentSize innerMain
  | contentSize <= innerMain = 0
  | otherwise =
      case slot of
        ScrollBarWindow -> 0
        ScrollBarList -> scrollBarGutter fm + scrollBarListExtra
        ScrollBarPage -> scrollBarGutter fm + scrollBarPageExtra

scrollBarOuterGap :: FontMetrics -> ScrollBarSlot -> Float
scrollBarOuterGap fm slot =
  if isTerminalFont fm
    then 0
    else
      case slot of
        ScrollBarList -> scrollBarListExtra
        ScrollBarPage -> scrollBarPageExtra
        ScrollBarWindow -> scrollBarWindowSide

-- How far a window body bar hangs past the content edge (left of the bar).
scrollBarWindowHang :: FontMetrics -> Float
scrollBarWindowHang fm =
  if isTerminalFont fm
    then 0
    else
      let (barW, _) = scrollBarGeom fm
          side = scrollBarWindowSide
       in min side (max 0 (padR windowPad - side - barW))

measureText :: FontMetrics -> Text -> (Float, Float)
measureText fm txt =
  let adv = fmAdvance fm ' '
      len = fromIntegral (T.length txt)
      w = len * adv
      h = fmLineHeight fm
   in (w, h)

measureTextWrapped :: FontMetrics -> Text -> Float -> (Float, Float)
measureTextWrapped fm txt maxW =
  let lineH = fmLineHeight fm
      textLines = wrapTextLines fm txt maxW
   in wrappedSize lineW lineH maxW textLines
  where
    lineW = lineWidth fm

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

wrapTextLines :: FontMetrics -> Text -> Float -> [Text]
wrapTextLines fm txt maxW = wrapTextLinesWith (lineWidth fm) txt maxW

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
