{-# LANGUAGE StrictData #-}

module NanoUI.Font
  ( GlyphQuad (..)
  , RunQuad (..)
  , FontMetrics (..)
  , FontBackend (..)
  , prepareFontMetrics
  , prepareFontMetricsMany
  , measureTextIO
  , lineWidthIO
  , drawRun
  , drawGlyph
  , monospaceMetrics
  , scaleFontMetrics
  , measureText
  , measureTextWrapped
  , measureTextWrappedIO
  , wrapTextLines
  , wrapTextLinesIO
  , truncateTextAdvance
  , truncateTextWith
  , truncateTextIO
  , lineWidth
  , textDisplayWidth
  , textIndexAtX
  , labelContentInset
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
  , layoutLineHeight
  , checkboxBoxSize
  , checkboxLeading
  , treeItemPadding
  , treeIndentStep
  , treeChevronLeading
  , treeRowLeading
  , treeChevronRect
  , resolveLayoutGap
  , resolveLayoutPadding
  , tabSentinelChar
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
  , sliderTrackMargin
  , sliderHandleDiameter
  , sliderHandleSlack
  ) where


import Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Types (Rect (..), onGrid)
import NanoUI.Style (AlignX (..), Padding (..))

tabSentinelChar :: Char
tabSentinelChar = '\x2409'

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

data RunQuad = RunQuad
  { rqX :: {-# UNPACK #-} !Float
  , rqY :: {-# UNPACK #-} !Float
  , rqW :: {-# UNPACK #-} !Float
  , rqH :: {-# UNPACK #-} !Float
  , rqU0 :: {-# UNPACK #-} !Float
  , rqV0 :: {-# UNPACK #-} !Float
  , rqU1 :: {-# UNPACK #-} !Float
  , rqV1 :: {-# UNPACK #-} !Float
  , rqAdvance :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

data FontMetrics = FontMetrics
  { fmLineHeight :: {-# UNPACK #-} !Float
  , fmAscent :: {-# UNPACK #-} !Float
  -- | Device pixels per logical unit used to snap glyph quads to the pixel
  -- grid. The SDL backend sets this to the display scale so text lands on
  -- whole device pixels.
  , fmSnapScale :: {-# UNPACK #-} !Float
  , fmAdvance :: Char -> Float
  , fmKerning :: Char -> Char -> Float
  , fmRun :: Text -> Maybe RunQuad
  , fmGlyph :: Char -> Maybe GlyphQuad
  -- | Optional effectful backend. Pure callbacks above are immutable metric
  -- snapshots; they must never perform font loading or atlas mutation.
  , fmBackend :: Maybe FontBackend
  }

-- | Text preparation performs font queries in IO and returns an immutable
-- snapshot for pure layout. Rasterisation is separate and occurs during draw.
data FontBackend = FontBackend
  { fbPrepare :: Text -> IO FontMetrics
  , fbDrawRun :: Text -> IO (Maybe RunQuad)
  , fbDrawGlyph :: Char -> IO (Maybe GlyphQuad)
  }

{-# INLINE prepareFontMetrics #-}
prepareFontMetrics :: FontMetrics -> Text -> IO FontMetrics
prepareFontMetrics fm txt = case fmBackend fm of
  Nothing -> pure fm
  Just backend -> fbPrepare backend txt

-- | Prepare a finite text workspace for pure multi-label layout algorithms.
prepareFontMetricsMany :: FontMetrics -> [Text] -> IO FontMetrics
prepareFontMetricsMany fm texts = case fmBackend fm of
  Nothing -> pure fm
  Just _ -> do
    combined <- prepareFontMetrics fm (T.intercalate "\n" texts)
    runs <- mapM (\t -> do
      prepared <- prepareFontMetrics fm t
      pure (t, fmRun prepared t)) texts
    let !byText = Map.fromList runs
    pure combined {fmRun = \t -> Map.findWithDefault Nothing t byText}

{-# INLINE lineWidthIO #-}
lineWidthIO :: FontMetrics -> Text -> IO Float
lineWidthIO fm txt = do
  prepared <- prepareFontMetrics fm txt
  pure $! lineWidth prepared txt

{-# INLINE measureTextIO #-}
measureTextIO :: FontMetrics -> Text -> IO (Float, Float)
measureTextIO fm txt = do
  prepared <- prepareFontMetrics fm txt
  pure $! measureText prepared txt

{-# INLINE drawRun #-}
drawRun :: FontMetrics -> Text -> IO (Maybe RunQuad)
drawRun fm txt = case fmBackend fm of
  Nothing -> pure (fmRun fm txt)
  Just backend -> fbDrawRun backend txt

{-# INLINE drawGlyph #-}
drawGlyph :: FontMetrics -> Char -> IO (Maybe GlyphQuad)
drawGlyph fm c = case fmBackend fm of
  Nothing -> pure (fmGlyph fm c)
  Just backend -> fbDrawGlyph backend c

{-# INLINE monospaceMetrics #-}
monospaceMetrics :: Float -> FontMetrics
monospaceMetrics cell =
  FontMetrics
    { fmLineHeight = cell
    , fmAscent = cell * 0.8
    , fmSnapScale = 1.0
    , fmAdvance = \_ -> cell
    , fmKerning = \_ _ -> 0
    , fmRun = \_ -> Nothing
    , fmGlyph = \_ -> Nothing
    , fmBackend = Nothing
    }

{-# INLINE scaleFontMetrics #-}
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
        , fmRun = \t -> fmap scaleRun (fmRun fm t)
        , fmGlyph = \c -> case fmGlyph fm c of
            Nothing -> Nothing
            Just gq ->
              Just
                gq
                  { gqX = gqX gq * s
                  , gqY = gqY gq * s
                  , gqW = gqW gq * s
                  , gqH = gqH gq * s
                   }
        , fmBackend = fmap scaleBackend (fmBackend fm)
        }
  where
    scaleBackend backend = FontBackend
      { fbPrepare = \t -> scaleFontMetrics s <$> fbPrepare backend t
      , fbDrawRun = \t -> fmap (fmap scaleRun) (fbDrawRun backend t)
      , fbDrawGlyph = \c -> fmap (fmap scaleGlyph) (fbDrawGlyph backend c)
      }
    scaleGlyph gq = gq
      { gqX = gqX gq * s, gqY = gqY gq * s
      , gqW = gqW gq * s, gqH = gqH gq * s
      }
    scaleRun rq =
      rq
        { rqX = rqX rq * s
        , rqY = rqY rq * s
        , rqW = rqW rq * s
        , rqH = rqH rq * s
        , rqAdvance = rqAdvance rq * s
        -- UVs stay in normalised atlas space; do not scale them.
        }

-- Layout gap/pad are authored in pixel steps (see defaultLayout).
{-# INLINE resolveLayoutGap #-}
resolveLayoutGap :: FontMetrics -> Float -> Float
resolveLayoutGap _fm g = g

{-# INLINE resolveLayoutPadding #-}
resolveLayoutPadding :: FontMetrics -> Padding -> Padding
resolveLayoutPadding _fm (Padding l t r b) = Padding l t r b

-- Labels share the node origin with rects and images. Outer gap lives on card/panel padding.
{-# INLINE labelContentInset #-}
labelContentInset :: FontMetrics -> (Float, Float)
labelContentInset _fm = (0, 0)

-- Table text inset. Zebra and header fills use the full cell rect.
{-# INLINE tableCellInset #-}
tableCellInset :: FontMetrics -> (Float, Float)
tableCellInset _fm = (6, 0)

{-# INLINE widgetContentInset #-}
widgetContentInset :: FontMetrics -> (Float, Float)
widgetContentInset fm =
  let pad = fmAdvance fm ' ' * 1.25
   in (pad, pad)

{-# INLINE buttonPadding #-}
buttonPadding :: FontMetrics -> (Float, Float)
buttonPadding fm =
  let adv = fmAdvance fm ' '
      lh = layoutLineHeight fm
   in (adv * 2.0, lh * 0.30)

{-# INLINE selectPadding #-}
selectPadding :: FontMetrics -> (Float, Float)
selectPadding fm =
  let adv = fmAdvance fm ' '
      lh = layoutLineHeight fm
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

{-# INLINE layoutLineHeight #-}
layoutLineHeight :: FontMetrics -> Float
layoutLineHeight fm = fmLineHeight fm

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

-- Last glyph ink right in the same space as 'pushText' (pen + gqX + gqW).
-- Falls back to advance when 'fmGlyph' is Nothing (tests).
{-# INLINE textInkEnd #-}
textInkEnd :: FontMetrics -> Text -> Float
textInkEnd fm txt =
  case T.unsnoc txt of
    Nothing -> 0
    Just (prefix, c) ->
      case fmRun fm txt of
        Just rq -> rqX rq + rqW rq
        Nothing ->
          let pen = lineWidth fm prefix
           in case fmGlyph fm c of
                Just gq -> pen + gqX gq + gqW gq
                Nothing -> pen + fmAdvance fm c

-- Align using per-glyph advances (same as 'pushText'), not TTF_GetStringSize.
-- When the line fits, AlignEnd/Center shift by ink so the visual right edge
-- stays put as the last character's right bearing changes.
{-# INLINE alignedTextPen #-}
alignedTextPen :: AlignX -> Float -> Float -> Float -> FontMetrics -> Text -> (Float, Float)
alignedTextPen ax x w ix fm txt =
  let tw = lineWidth fm txt
      ink = textInkEnd fm txt
      contentW = max 0 (w - 2 * ix)
      used = min tw contentW
      shift =
        if tw > contentW
          then used
          else case ax of
            AlignStart -> used
            _ -> ink
      (tx, _) = alignedTextBox ax x w ix shift
   in (tx, used)

{-# INLINE widgetPadding #-}
widgetPadding :: FontMetrics -> (Float, Float)
widgetPadding fm =
  let (cx, cy) = widgetContentInset fm
   in (2 * cx, 2 * cy)

{-# INLINE checkboxBoxSize #-}
checkboxBoxSize :: FontMetrics -> Float
checkboxBoxSize fm = min 22 (max 18 (fmLineHeight fm * 1.15))

{-# INLINE checkboxLeading #-}
checkboxLeading :: FontMetrics -> Float
checkboxLeading fm = checkboxBoxSize fm + 8

{-# INLINE treeItemPadding #-}
treeItemPadding :: FontMetrics -> (Float, Float)
treeItemPadding fm =
  let lh = layoutLineHeight fm
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
treeChevronRect :: FontMetrics -> Float -> Float -> Float -> Float -> Int -> Rect
treeChevronRect fm x y _w h depth =
  let (ix, _) = labelContentInset fm
      indent = treeIndentStep fm * fromIntegral (max 0 depth)
      lead = max 1 (treeChevronLeading fm)
   in Rect (x + ix + indent) y lead h

sliderTrackHeight :: Float
sliderTrackHeight = 10

sliderHandleDiameter :: Float
sliderHandleDiameter = 18

sliderHandleSlack :: Float
sliderHandleSlack = (sliderHandleDiameter - sliderTrackHeight) / 2

-- Distance from label baseline to track: 4px gap to handle + handle overhang
sliderTrackMargin :: Float
sliderTrackMargin = 4 + sliderHandleSlack

{-# INLINE sliderTrackBounds #-}
sliderTrackBounds :: FontMetrics -> Float -> Float -> Float -> Float -> Rect
sliderTrackBounds fm x y w h =
  let (lx, ly) = labelContentInset fm
      trackY = y + max ly ((h - sliderTrackHeight) / 2)
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

scrollBarGeom :: FontMetrics -> (Float, Float)
scrollBarGeom fm = scrollBarGeomFor fm ScrollBarList

scrollBarGeomFor :: FontMetrics -> ScrollBarSlot -> (Float, Float)
scrollBarGeomFor _fm slot =
  let barW = case slot of
        ScrollBarWindow -> scrollBarWindowWidth
        _ -> scrollBarWidth
      -- Window bar: side gaps only. No end inset.
      endM = case slot of
        ScrollBarWindow -> 0
        _ -> scrollBarMargin
   in (barW, endM)

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

-- Gap on both sides of a window body bar.
scrollBarWindowSide :: Float
scrollBarWindowSide = 2

scrollLayoutGutter :: FontMetrics -> ScrollBarSlot -> Float -> Float -> Float
scrollLayoutGutter fm slot contentSize innerMain
  | contentSize <= innerMain = 0
  | otherwise =
      case slot of
        -- Window bar hangs into the parent pad. Content keeps the full inner width.
        ScrollBarWindow -> 0
        ScrollBarList -> scrollBarGutter fm + scrollBarListExtra
        ScrollBarPage -> scrollBarGutter fm + scrollBarPageExtra

scrollBarOuterGap :: ScrollBarSlot -> Float
scrollBarOuterGap slot =
  case slot of
    ScrollBarList -> scrollBarListExtra
    ScrollBarPage -> scrollBarPageExtra
    ScrollBarWindow -> scrollBarWindowSide

-- Width the window bar occupies in the parent pad (not taken from content).
scrollBarWindowGutter :: FontMetrics -> Float
scrollBarWindowGutter fm =
  let (barW, _) = scrollBarGeomFor fm ScrollBarWindow
      side = scrollBarOuterGap ScrollBarWindow
   in barW + 2 * side

measureText :: FontMetrics -> Text -> (Float, Float)
measureText fm txt =
  let h = fmLineHeight fm
      w = lineWidth fm txt
   in (w, h)

-- | Line width for hit testing and centering.
textDisplayWidth :: FontMetrics -> Text -> Float
textDisplayWidth fm txt = lineWidth fm txt

-- Caret and click index using the same advances and kerning as pushText,
-- so the caret lands exactly where the glyph to its left was drawn.
textIndexAtX :: FontMetrics -> Text -> Float -> Int
textIndexAtX fm txt x
  | T.null txt || x <= 0 = 0
  | otherwise = go 0 0.0 Nothing txt
  where
    go !i !acc prev t =
      case T.uncons t of
        Nothing -> i
        Just (c, rest) ->
          let adv = charW prev c
              mid = acc + adv * 0.5
           in if x < mid then i else go (i + 1) (acc + adv) (Just c) rest
    charW prev c =
      case prev of
        Nothing -> fmAdvance fm c
        Just p -> fmAdvance fm c + fmKerning fm p c

measureTextWrapped :: FontMetrics -> Text -> Float -> (Float, Float)
measureTextWrapped fm txt maxW
  | maxW <= 0 = (0, fmLineHeight fm)
  | T.null txt = (0, fmLineHeight fm)
  | not (T.any (== '\n') txt) && lineW txt <= maxW = (lineW txt, fmLineHeight fm)
  | otherwise =
      let lineH = fmLineHeight fm
          textLines = wrapTextLines fm txt maxW
       in wrappedSize lineW lineH maxW textLines
  where
    lineW = textDisplayWidth fm

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

{-# INLINE wrapTextLines #-}
wrapTextLines :: FontMetrics -> Text -> Float -> [Text]
wrapTextLines fm txt maxW =
  concatMap
    (\para -> runIdentity (wrapParagraphM fit (pure . lineWidth fm) para maxW))
    (T.lines txt)
 where
  fit width = pure . takeWidthAdvance fm width

wrapTextLinesIO :: (Text -> IO Float) -> FontMetrics -> Text -> Float -> IO [Text]
wrapTextLinesIO lineW _ txt maxW =
  concat <$> mapM (\para -> wrapParagraphM (takeWidthM lineW) lineW para maxW) (T.lines txt)

-- The layout policy is shared by pure font metrics (Identity) and host-backed
-- shaping (IO). Only measuring a line and fitting a prefix depend on the host.
{-# INLINE wrapParagraphM #-}
wrapParagraphM ::
  Monad m =>
  (Float -> Text -> m (Text, Text)) -> (Text -> m Float) -> Text -> Float -> m [Text]
wrapParagraphM fit lineW para maxW
  | maxW <= 0 = pure []
  | T.null para = pure [""]
  | otherwise = do
      w <- lineW para
      if w <= maxW
        then pure [para]
        else if T.any (== ' ') para
          then wrapWordsM lineW maxW (T.words para)
          else reverse <$> charLinesM fit maxW para []

{-# INLINE wrapWordsM #-}
wrapWordsM :: Monad m => (Text -> m Float) -> Float -> [Text] -> m [Text]
wrapWordsM lineW maxW wordsToWrap = go wordsToWrap []
 where
  go [] acc = pure (reverse acc)
  go (word : wordsLeft) acc = case acc of
    [] -> startLine word wordsLeft acc
    line : rest -> do
      let candidate = line <> " " <> word
      width <- lineW candidate
      if width <= maxW
        then go wordsLeft (candidate : rest)
        else startLine word wordsLeft acc
  startLine word wordsLeft acc = do
    width <- lineW word
    if width <= maxW
      then go wordsLeft (word : acc)
      else do
        broken <- charLinesM (takeWidthM lineW) maxW word []
        go wordsLeft (broken ++ acc)

{-# INLINE charLinesM #-}
charLinesM ::
  Monad m => (Float -> Text -> m (Text, Text)) -> Float -> Text -> [Text] -> m [Text]
charLinesM fit maxW txt acc =
  if T.null txt
    then pure acc
    else do
      (line, rest) <- fit maxW txt
      if T.null line
        then pure acc
        else charLinesM fit maxW rest (line : acc)

{-# INLINE lineWidth #-}
lineWidth :: FontMetrics -> Text -> Float
lineWidth fm line
  | T.null line = 0
  | otherwise =
      case fmRun fm line of
        Just rq -> rqAdvance rq
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
    step (!w, !prev) c = (w + fmAdvance fm c + fmKerning fm prev c, c)

takeWidthAdvance :: FontMetrics -> Float -> Text -> (Text, Text)
takeWidthAdvance fm maxW txt
  | T.null txt = (txt, T.empty)
  | maxW <= 0 = (T.empty, txt)
  | otherwise =
      let !adv = fmAdvance fm ' '
          !xAdv = fmAdvance fm 'x'
          !mAdv = fmAdvance fm 'M'
       in if adv == xAdv && xAdv == mAdv && adv > 0 && fmKerning fm 'x' 'M' == 0
            then
              let !count = floor (maxW / adv)
                  !n = T.length txt
               in if count >= n
                    then (txt, T.empty)
                    else if count <= 0
                      then (T.take 1 txt, T.drop 1 txt)
                      else (T.take count txt, T.drop count txt)
            else
              let (!len, _, _) = T.foldl' step (0, 0.0 :: Float, ' ') txt
               in if len <= 0
                    then (T.take 1 txt, T.drop 1 txt)
                    else if len >= T.length txt
                      then (txt, T.empty)
                      else (T.take len txt, T.drop len txt)
  where
    step (!len, !w, !prev) c =
      let w' = w + fmAdvance fm c + (if len == 0 then 0 else fmKerning fm prev c)
       in if w' > maxW
            then if len == 0 then (1, w', c) else (len, w, c)
            else (len + 1, w', c)

-- Always consume at least one character from non-empty text, even when a
-- single glyph exceeds the available width, so wrapping makes progress.
{-# INLINE takeWidthM #-}
takeWidthM :: Monad m => (Text -> m Float) -> Float -> Text -> m (Text, Text)
takeWidthM lineW maxW txt
  | T.null txt = pure (txt, T.empty)
  | otherwise = (`T.splitAt` txt) <$> maxFit 1 (T.length txt)
  where
    maxFit lo hi
      | lo >= hi = pure lo
      | otherwise = do
          let mid = (lo + hi + 1) `div` 2
          ok <- (<= maxW) <$> lineW (T.take mid txt)
          if ok then maxFit mid hi else maxFit lo (mid - 1)

{-# INLINE truncateTextAdvance #-}
truncateTextAdvance :: FontMetrics -> Float -> Text -> Text
truncateTextAdvance fm maxW txt =
  runIdentity $
    truncateTextM
      (\width -> pure . takeWidthAdvance fm width)
      (pure . lineWidth fm)
      maxW
      txt

{-# INLINE truncateTextWith #-}
truncateTextWith :: (Text -> Float) -> Float -> Text -> Text
truncateTextWith lineW maxW txt =
  runIdentity (truncateTextM (takeWidthM (pure . lineW)) (pure . lineW) maxW txt)

truncateTextIO :: (Text -> IO Float) -> Float -> Text -> IO Text
truncateTextIO lineW = truncateTextM (takeWidthM lineW) lineW

{-# INLINE truncateTextM #-}
truncateTextM ::
  Monad m =>
  (Float -> Text -> m (Text, Text)) -> (Text -> m Float) -> Float -> Text -> m Text
truncateTextM fitPrefix lineW maxW txt
  | maxW <= 0 = pure ""
  | otherwise = do
      w <- lineW txt
      if w <= maxW
        then pure txt
        else do
          ellW <- lineW "..."
          if maxW <= ellW
            then fst <$> fitPrefix maxW txt
            else do
              (fit, _) <- fitPrefix (maxW - ellW) txt
              pure (T.dropWhileEnd (== '.') fit <> "...")
