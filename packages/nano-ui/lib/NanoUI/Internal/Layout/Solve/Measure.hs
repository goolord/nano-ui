-- | Measure pass, custom/text/container measurement, fit-height refit, and baselines.
module NanoUI.Internal.Layout.Solve.Measure
  ( FontResolver
  , SolveEnv (..)
  , PositionLog (..)
  , Measurers (..)
  , TextWrapper
  , solveEnv
  , TextMeasurer (..)
  , nodeMeasurer
  , measureFontLine
  , measureFontWrapped
  , TextBox (..)
  , measureTextNodeAt
  , textHeightAt
  , measurePass
  , runCustomMeasure
  , textWrapCap
  , findAncestorMaxW
  , adornedButtonGroup
  , windowBodyScroller
  , foldChildDimsFromParent
  , wrapExtent
  , recomputeFitHeightAtWidth
  , loadChildrenScratch
  , flowChildSize
  , percentOr
  , childBaseline
  , scrollBarSlotOf
  , hasPanelAncestor
  , drawingHeightAt
  , measuredHeightAt
  , wrapLineCross
  , rowChildHeightAt
  , ContainerKind (..)
  , readContainerKind
  , readGridColumns
  ) where

import Control.Monad (filterM, foldM, forM_, guard, mfilter, unless, when)
import Data.IORef (IORef, modifyIORef', readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe, isJust)
import Data.Primitive.PrimArray (copyMutablePrimArray, readPrimArray, writePrimArray)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8, Word64)
import GHC.Float (castFloatToWord32)
import NanoUI.Internal.Font
import NanoUI.Internal.Layout.ScrollGeometry
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Layout.Arena.Types (newFlexScratch)
import NanoUI.Internal.Layout.Solve.Flex
import NanoUI.Internal.Style (AlignY (..), Flow (..), FontStyle (..), FontVariant (..), FontWeight (..), Padding (..), hasExplicitMin)
import NanoUI.Internal.Types (Rect (..), clamp, foldUpTo, newZeroedPrimArray, isBounded, isUnbounded, noLimit)
import NanoUI.Internal.WidgetText

-- | Resolve size, weight, slant, and variant to metrics plus logical-pixel
-- text measurement. Size zero requests the backend default.
type FontResolver = Float -> FontWeight -> FontStyle -> FontVariant -> IO (FontMetrics, Text -> IO (Float, Float))

-- | Per-solve constants threaded through the measure and position passes.
data SolveEnv = SolveEnv
  { seArena :: !NodeArena
  , seArrays :: !NodeArenaArrays
  , seMs :: !Measurers
  , seSub :: !(IOArr Word64)
  -- ^ The arena's per-node subtree hashes, written by 'computeSubtreeHashes'
  -- for this frame and read by the measure pass.
  , seMeasured :: !(IOArr Float)
  -- ^ The arena's per-node measured sizes (two floats), written by the
  -- measure pass and snapshotted at capture.
  , seCache :: !(Maybe LayoutCache)
  -- ^ A non-empty captured solve, taken under the current font metrics. A
  -- node it measured the same way gets its captured size back instead of
  -- measuring again ('restoreMeasured').
  , seMeasureLog :: !(Maybe (IORef (IntMap CustomMeasureRecord)))
  -- ^ Where 'measureCustomNode' records each custom measurement, for the
  -- layout cache: set by 'solveLayout' only, so floating placement records
  -- nothing.
  , sePositions :: !(Maybe PositionLog)
  -- ^ Set by 'solveLayout' only: what its position pass records and puts
  -- back ('positionNodeA').
  }

-- | The position pass's record of the rects it offers, and which subtrees
-- it may put back from the cache ('naOffered', 'naSpans').
data PositionLog = PositionLog
  { plOffered :: !(IOArr Float)
  , plSpans :: !(IOArr Int)
  }

-- | How text and custom widgets are measured. The solve and the placement of
-- floating nodes after it measure with the same, so a label placed in a modal
-- wraps exactly as the solve measured it for the modal's size.
data Measurers = Measurers
  { msFm :: !FontMetrics
  , msMonoFm :: !FontMetrics
  , msMeasure :: !(Text -> IO (Float, Float))
  , msResolveFont :: !FontResolver
  , msLookupMeasure :: !(WidgetId -> IO (Maybe CustomMeasureFn))
  , msWrap :: !TextWrapper
  }

-- | Wrap text in a font ('textNodeFontKey') with its line measure, as
-- 'wrapTextIO' does. The context's shares results across frames and with
-- the text spans ('NanoUI.Internal.Context.cachedWrapText').
type TextWrapper = Int -> (Text -> IO Float) -> Text -> Float -> IO WrapResult

solveEnv :: NodeArena -> Measurers -> Maybe LayoutCache -> IO SolveEnv
solveEnv na ms mCache = do
  a <- arenaArrays na
  (sub, measured) <- subtreeArrays na
  pure (SolveEnv na a ms sub measured (mfilter ((> 0) . lcCount) mCache) Nothing Nothing)

-- Keep font selection and single-line/wrapped measurement together so every
-- layout pass uses the same policy. Monospaced text uses its metrics directly;
-- proportional text uses the host's shaping-aware measurement callback.
data TextMeasurer = TextMeasurer
  { tmMetrics :: !FontMetrics
  , tmVariant :: !FontVariant
  , tmFontKey :: !Int
  , tmHostLine :: Text -> IO (Float, Float)
  }

-- | How node @idx@, of type @nt@ and style index @nodeSi@, measures its
-- text: in the font its style packs ('nodeFontBits'), else in its font size
-- in the default face, as
-- 'NanoUI.Internal.Frame.Node.resolveFontFor' draws them.
nodeMeasurer :: SolveEnv -> NodeIdx -> NodeType -> Int -> IO TextMeasurer
nodeMeasurer SolveEnv {seArena = na, seMs = ms} idx nt nodeSi = do
  let si = nodeFontBits nt nodeSi
  size <- getNodeFontSize na idx
  let variant = textNodeFontVariant si
      weight = textNodeFontWeight si
      style = textNodeFontStyle si
  (metrics, measureLine) <-
    if isDefaultNodeFont size weight style variant
      then pure (if variant == FontMono then msMonoFm ms else msFm ms, msMeasure ms)
      else msResolveFont ms size weight style variant
  pure (TextMeasurer metrics variant (textNodeFontKey size si) measureLine)

-- Keep these operations as inline functions rather than allocating two
-- closures for every resolved node, including nodes that never wrap.
{-# INLINE measureFontLine #-}
measureFontLine :: TextMeasurer -> Text -> IO (Float, Float)
measureFontLine TextMeasurer {tmMetrics = metrics, tmVariant = variant, tmHostLine = hostLine} text
  | variant == FontMono = measureTextIO metrics text
  | otherwise = hostLine text

{-# INLINE measureFontWrapped #-}
measureFontWrapped :: SolveEnv -> TextMeasurer -> Text -> Float -> IO (Float, Float)
measureFontWrapped env TextMeasurer {tmMetrics = metrics, tmVariant = variant, tmFontKey = font, tmHostLine = hostLine} text width =
  wrapMeasure metrics width <$> msWrap (seMs env) font lineW text width
  where
    lineW
      | variant == FontMono = lineWidthIO metrics
      | otherwise = fmap fst . hostLine

-- | A measured text node: whether it wrapped, its content size, and the line
-- height of its font.
data TextBox = TextBox
  { tbWrapped :: !Bool
  , tbW :: !Float
  , tbH :: !Float
  , tbLineH :: !Float
  }

-- | Measure a text node's content for the width @outerW@. The text wraps at
-- @outerW@ minus its label inset ('tableStripeInset'), as paint wraps it,
-- when it has explicit newlines, or when @shouldWrap outerW lineW@ holds for
-- its single-line width.
measureTextNodeAt :: SolveEnv -> NodeIdx -> Text -> Float -> (Float -> Float -> Bool) -> IO TextBox
measureTextNodeAt env idx txt outerW shouldWrap = do
  si <- getStyleIdx (seArena env) idx
  measurer@TextMeasurer {tmMetrics = textFm} <- nodeMeasurer env idx NodeText si
  let wrapW = max 0 (outerW - 2 * tableStripeInset si)
      lineH = fmLineHeight textFm
      na = seArena env
  if T.any (== '\n') txt
    then do
      (tw, th) <- memoizeWidth na (naWrapMemo na) idx wrapW (measureFontWrapped env measurer txt wrapW)
      pure (TextBox True tw th lineH)
    else do
      (tw0, th0) <- measureFontLine measurer txt
      if shouldWrap (max 0 outerW) tw0
        then do
          (tw, th) <- memoizeWidth na (naWrapMemo na) idx wrapW (measureFontWrapped env measurer txt wrapW)
          pure (TextBox True tw th lineH)
        else pure (TextBox False tw0 th0 lineH)

-- | Height of text node @idx@ at width @w@, clamped to @hAx@. Text wraps at
-- newlines, and at @w@ when @allowed@ and its line overflows a positive @w@.
-- Text that does not wrap returns @fallback@, unless its height is fit and
-- it may wrap at a positive @w@, where paint wraps it ('textWrapCap'): the
-- measure may have wrapped it narrower, at an ancestor's cap. Empty text,
-- which measured one line tall, returns @fallback@.
{-# INLINE textHeightAt #-}
textHeightAt :: SolveEnv -> NodeIdx -> AxisSizing -> Float -> Bool -> Float -> IO Float
textHeightAt env !idx (AxisSizing hTag _ minH maxH) !w allowed !fallback = do
  txt <- getText (seArena env) idx
  if T.null txt
    then pure fallback
    else do
      TextBox {tbWrapped, tbH, tbLineH} <-
        measureTextNodeAt env idx txt w (\wrapW lineW -> allowed && wrapW + 0.5 < lineW && wrapW > 0)
      pure (if tbWrapped || (allowed && hTag == SizingFit && w > 0) then clamp minH maxH (max tbLineH tbH) else fallback)

measurePass :: SolveEnv -> Int -> IO ()
measurePass env count = do
  -- Which nodes keep their height at any width ('markSteadyHeight'), before
  -- any is measured: a container's measure reads its own mark. Children
  -- follow their parent in the arena, so walking down from the last node
  -- reaches every child before its parent. A pass of its own: marking inside
  -- the measure loop below allocated a box per node.
  let steadyPass !i = when (i >= 0) (markSteadyHeight env i >> steadyPass (i - 1))
  steadyPass (count - 1)
  case seCache env of
    Nothing ->
      let go !idx = when (idx >= 0) (measureNode env idx >> recordMeasured env idx >> go (idx - 1))
       in go (count - 1)
    Just lc -> do
      -- Per node, 1 once some child came out a different size than the
      -- capture recorded for it, and 2 when a drawing with a custom measure
      -- is below it, so the node cannot take its captured size.
      moved <- newZeroedPrimArray count :: IO (IOArr Word8)
      let go !idx = when (idx >= 0) (measureCachedNode env lc moved idx >> go (idx - 1))
      go (count - 1)

-- | Measure node @idx@ in a solve with a layout cache, or put back its
-- captured size ('restoreMeasured'), and record for its parent whether it
-- came out its captured size. Children follow their parent in the arena,
-- so 'measurePass' walks down from the last node and measures every child
-- before its parent.
measureCachedNode :: SolveEnv -> LayoutCache -> IOArr Word8 -> NodeIdx -> IO ()
measureCachedNode env lc moved !idx = do
  hit <-
    if idx < lcCount lc
      then (==) <$> readPrimArray (seSub env) idx <*> readPrimArray (lcSub lc) idx
      else pure False
  restored <- restoreMeasured env lc moved hit idx
  unless restored (measureNode env idx)
  (w, h) <- recordMeasured env idx
  same <-
    if restored || idx >= lcCount lc
      then pure restored
      else do
        cw <- readPrimArray (lcMeasured lc) (idx * 2)
        ch <- readPrimArray (lcMeasured lc) (idx * 2 + 1)
        pure (cw == w && ch == h)
  -- 2 marks a node above a drawing with a custom measure: its measured
  -- height runs that measure at its children's widths
  -- ('recomputeFitHeightAtWidth'), which the cache does not record, so
  -- it measures again however its children came out.
  nt <- readTagEnum (seArrays env) idx TagNodeType
  aboveMeasure <-
    if nt == NodeDrawing
      then getWidgetId (seArena env) idx >>= fmap isJust . msLookupMeasure (seMs env)
      else (== 2) <$> readPrimArray moved idx
  p <- readTree (seArrays env) idx TreeParent
  when (p >= 0) $ do
    m <- readPrimArray moved p
    if aboveMeasure
      then writePrimArray moved p 2
      else when (not same && m == 0) (writePrimArray moved p 1)
  -- The captured solve placed the subtree as this one will when the
  -- node's key hit and it came out its captured size, as did every node
  -- below it, and it holds no drawing, whose custom measure may return a
  -- different size with nothing in the arena changed.
  case sePositions env of
    Nothing -> pure ()
    Just PositionLog {plSpans} -> do
      end <- if hit && same && nt /= NodeDrawing then subtreeEnd (seArrays env) plSpans idx else pure (-1)
      writePrimArray plSpans idx end

-- | Put back the measured size the captured solve recorded for a node, when
-- that solve measured it the same way: its restore key
-- ('computeSubtreeHashes', over its own, its ancestors' and its descendants'
-- inputs) matches the capture's (@hit@), and no child came out a different
-- size.
-- 'False' when the node must be measured. Drawing nodes always measure: a
-- custom measure may read state outside the arena, and losing the hook
-- changes what the size means. Scroll containers always measure too, cheaply
-- from their children: the capture holds their content extent as the position
-- pass left it, not as measured.
{-# INLINE restoreMeasured #-}
restoreMeasured :: SolveEnv -> LayoutCache -> IOArr Word8 -> Bool -> NodeIdx -> IO Bool
restoreMeasured env lc moved hit idx
  | not hit = pure False
  | otherwise = do
      childMoved <- (/= 0) <$> readPrimArray moved idx
      nt <- readTagEnum (seArrays env) idx TagNodeType
      if childMoved || nt == NodeDrawing || isScrollNode nt
        then pure False
        else do
          w <- readPrimArray (lcMeasured lc) (idx * 2)
          h <- readPrimArray (lcMeasured lc) (idx * 2 + 1)
          setRect (seArena env) idx 0 0 w h
          pure True

-- | One past the last node of @idx@'s subtree when its children's subtrees,
-- whose ends @spans@ holds, follow it one after another, and -1 when one of
-- them has none or they do not.
subtreeEnd :: NodeArenaArrays -> IOArr Int -> NodeIdx -> IO Int
subtreeEnd a spans idx = do
  -- Children are linked last added first, so the first holds the end.
  fc <- readTree a idx TreeFirstChild
  end <- if fc < 0 then pure (idx + 1) else readPrimArray spans fc
  let go !c !next
        | c < 0 = pure (if next == idx + 1 then end else -1)
        | otherwise = do
            e <- readPrimArray spans c
            if e /= next then pure (-1) else readTree a c TreeNextSibling >>= \sib -> go sib c
  if end < 0 then pure (-1) else go fc end

-- | Record the node's measured size for the next capture, whether it was
-- just measured or restored, and return it.
{-# INLINE recordMeasured #-}
recordMeasured :: SolveEnv -> NodeIdx -> IO (Float, Float)
recordMeasured env idx = do
  Rect _ _ w h <- getNodeRect (seArena env) idx
  let ma = seMeasured env
  writePrimArray ma (idx * 2) w
  writePrimArray ma (idx * 2 + 1) h
  pure (w, h)

measureNode :: SolveEnv -> NodeIdx -> IO ()
measureNode env idx = measureContent env idx >> measureAspect (seArrays env) idx

-- | Apply a node's aspect ratio ('StyleAspect') after its content is
-- measured. A fit height becomes width / ratio, and a fit width beside a
-- fixed height becomes height * ratio, both within limits.
{-# INLINE measureAspect #-}
measureAspect :: NodeArenaArrays -> NodeIdx -> IO ()
measureAspect a idx = do
  ratio <- readStyle a idx StyleAspect
  when (ratio > 0) $ do
    wAx <- readAxisSizing a idx True
    hAx <- readAxisSizing a idx False
    case (axTag wAx, axTag hAx) of
      (_, SizingFit) -> readGeom a idx GeomW >>= \w -> writeGeom a idx GeomH (sizeWithin hAx (w / ratio))
      (SizingFit, SizingFixed) -> readGeom a idx GeomH >>= \h -> writeGeom a idx GeomW (sizeWithin wAx (h * ratio))
      _ -> pure ()

measureContent :: SolveEnv -> NodeIdx -> IO ()
measureContent env@SolveEnv {seArena = na} idx = do
  nt <- readTagEnum (seArrays env) idx TagNodeType
  -- Each is called once below, so it is inlined there.
  let container = do
        measureContainer env idx
        when (nt == NodeModal) $ setSolvedValue na idx 0
      widget = measureWidget env idx
  case nt of
    NodeText -> measureTextNode env idx
    -- Non-fixed spacers reserve the default 8px extent.
    NodeSpacer -> measureImage na idx (Just (8, 8))
    NodeSeparator -> measureSeparator na idx
    NodeScrollContainer -> measureScrollContainer env idx
    NodeImage -> getImageNode na idx >>= measureImage na idx . fmap (\n -> (inWidth n, inHeight n))
    NodeBox -> measureImage na idx Nothing
    NodeDrawing -> do
      wid <- getWidgetId na idx
      msLookupMeasure (seMs env) wid >>= maybe (measureImage na idx Nothing) (\fn -> measureCustomNode env fn idx)
    NodeContainer -> container
    NodePanel -> container
    NodeModal -> container
    NodeWindow -> container
    NodePopup -> container
    NodeButton -> widget
    NodeSlider -> widget
    NodeTextInput -> widget
    NodeTextArea -> widget
    NodeSelect -> widget
    NodeColorPicker -> widget

measureCustomNode :: SolveEnv -> CustomMeasureFn -> NodeIdx -> IO ()
measureCustomNode env@SolveEnv {seArena = na} measureFn idx = do
  wAx <- getWidthSizing na idx
  hAx <- getHeightSizing na idx
  let ((mw, mh), record) = customMeasure (msFm (seMs env)) measureFn wAx hAx
  forM_ (seMeasureLog env) $ \ref -> modifyIORef' ref (IM.insert idx record)
  setRect na idx 0 0 (sizeWithin wAx mw) (sizeWithin hAx mh)

-- | Run a node's custom measure again, as the solve ran it, and return the
-- record the layout cache keeps of the call. Layout-reuse validation compares
-- it with the solve's.
runCustomMeasure :: NodeArena -> FontMetrics -> CustomMeasureFn -> NodeIdx -> IO CustomMeasureRecord
runCustomMeasure na fm measureFn idx = do
  wAx <- getWidthSizing na idx
  hAx <- getHeightSizing na idx
  pure $! snd (customMeasure fm measureFn wAx hAx)

-- | Run a custom measure on the offered space ('offeredExtent'). Returns the
-- measured size and the call record, forced with the pair so the cache holds
-- words rather than the closure.
{-# INLINE customMeasure #-}
customMeasure ::
  FontMetrics -> CustomMeasureFn -> AxisSizing -> AxisSizing -> ((Float, Float), CustomMeasureRecord)
customMeasure fm measureFn wAx hAx =
  let aw = offeredExtent wAx
      ah = offeredExtent hAx
      (mw, mh) = measureFn fm (aw, ah)
      !aw' = castFloatToWord32 aw
      !ah' = castFloatToWord32 ah
      !mw' = castFloatToWord32 mw
      !mh' = castFloatToWord32 mh
   in ((mw, mh), (aw', ah', mw', mh'))

-- | The space a custom measure is offered along an axis: its fixed size, or
-- else its maximum when that is finite ('isBounded'), and 'noLimit' otherwise.
{-# INLINE offeredExtent #-}
offeredExtent :: AxisSizing -> Float
offeredExtent (AxisSizing tag val lo hi)
  | tag == SizingFixed = clamp lo hi val
  | otherwise = if isBounded hi then hi else noLimit

-- | The axis's fixed size or else @content@, within its limits.
{-# INLINE sizeWithin #-}
sizeWithin :: AxisSizing -> Float -> Float
sizeWithin (AxisSizing tag val lo hi) content = clamp lo hi (if tag == SizingFixed then val else content)

-- | A measured drawing's height at width @w@, within its fit height's
-- limits, or @fallback@ when its widget has no custom measure.
{-# INLINE drawingHeightAt #-}
drawingHeightAt :: SolveEnv -> NodeIdx -> Float -> AxisSizing -> Float -> IO Float
drawingHeightAt SolveEnv {seArena = na, seMs = ms} idx w hAx fallback = do
  wid <- getWidgetId na idx
  msLookupMeasure ms wid >>= \case
    Just measure -> pure (measuredHeightAt (msFm ms) measure w hAx)
    Nothing -> pure fallback

-- | The height a custom measure gives a drawing laid out at width @w@,
-- clamped to @hAx@: what the position pass sets ('drawingHeightAt').
{-# INLINE measuredHeightAt #-}
measuredHeightAt :: FontMetrics -> CustomMeasureFn -> Float -> AxisSizing -> Float
measuredHeightAt fm measure w hAx = clamp (axMin hAx) (axMax hAx) (snd (measure fm (w, offeredExtent hAx)))

-- | The width a text node that is not a row's child wraps at, from its
-- effective max width, width sizing and assigned width: 1e8 or more when
-- nothing caps it (@textNodeSpanEntry@ in the text-span collector). Text
-- whose width is not fit wraps at the width it was assigned, as the solve's
-- position pass wraps it; fit text wraps where it was measured.
textWrapCap :: Float -> SizingTag -> Float -> Float
textWrapCap effMaxW wTag w
  | wTag /= SizingFit && w > 0 = w
  | otherwise = max 0 effMaxW

-- | Find the nearest ancestor's fixed or finite maximum width, subtracting
-- accumulated horizontal padding. Returns 'noLimit' when none constrains the node.
findAncestorMaxW :: NodeArena -> NodeIdx -> IO Float
findAncestorMaxW na idx = go idx 0
  where
    go cur !padAccum = do
      p <- getParent na cur
      if p < 0
        then pure noLimit
        else do
          pad <- getPadding na p
          let padAccum' = padAccum + padL pad + padR pad
          AxisSizing pwTag pwVal pMinW pMaxW <- getWidthSizing na p
          if pwTag == SizingFixed
            then pure (max 0 (clamp pMinW pMaxW pwVal - padAccum'))
            else if isBounded pMaxW
              then pure (max 0 (pMaxW - padAccum'))
              else go p padAccum'

measureTextNode :: SolveEnv -> NodeIdx -> IO ()
measureTextNode env@SolveEnv {seArena = na} idx = do
  wAx@(AxisSizing wTag wVal minW maxW) <- getWidthSizing na idx
  hAx <- getHeightSizing na idx
  parentAssigns <- growParent na idx
  txt <- getText na idx
  isRowChild <- parentIsRow na idx
  -- A fixed width wraps at itself within its limits, as placement does
  -- ('textWrapCap').
  effMaxW <-
    if wTag == SizingFixed
      then pure (clamp minW maxW wVal)
      else if isBounded maxW
        then pure maxW
        else findAncestorMaxW na idx
  let canWrap = not isRowChild && isBounded effMaxW
  TextBox {tbW = tw, tbH = th, tbLineH = lineH} <-
    measureTextNodeAt env idx txt effMaxW (\_ lineW -> canWrap && effMaxW + 0.5 < lineW)
  let reportedW = sizeWithin wAx (if wTag == SizingGrow && parentAssigns then 0 else tw)
  setRect na idx 0 0 reportedW (sizeWithin hAx (max lineH th))

-- | Whether a grow-width node's width is assigned from above rather than
-- reported: its parent grows, and the nearest ancestor that does not grow is
-- not a modal. A modal takes its width from what it holds, so a grow label
-- inside one still reports its natural width; otherwise the modal could never
-- widen for it and the label would wrap into more lines than the modal
-- measured. Windows keep their own width and truncate long lines instead.
growParent :: NodeArena -> NodeIdx -> IO Bool
growParent na = growParentOn na True

-- | 'growParent' along the width with @horizontal@, else the height.
growParentOn :: NodeArena -> Bool -> NodeIdx -> IO Bool
growParentOn na horizontal idx = getParent na idx >>= go True
  where
    go isParent p
      | p < 0 = pure (not isParent)
      | otherwise = do
          pTag <- axTag <$> (if horizontal then getWidthSizing else getHeightSizing) na p
          if pTag == SizingGrow || pTag == SizingPercent
            then getParent na p >>= go False
            else if isParent
              then pure False
              else (/= NodeModal) <$> getNodeType na p

-- | Whether a container of type @nt@ reports its minimum along an axis, with
-- @horizontal@ the width, in place of its content: it grows along the axis,
-- its minimum is explicit ('hasExplicitMin'), and its parent assigns its
-- size there ('growParentOn'). It shrinks that far in a line that is short of
-- space, so that is the least it needs. As with CSS's min-width on a flex
-- item, the explicit minimum replaces the content-based one. Otherwise a 2D
-- scroller, which lays its content out at the width it reports, scrolls
-- sideways for a long label in a cell that would have fit, and a pane's
-- content would size the pane grid. Without a minimum the content still
-- counts, so a grow wrapper around a wide table keeps its sideways scroll.
minAssignedOn :: NodeArena -> NodeType -> Bool -> NodeIdx -> AxisSizing -> IO Bool
minAssignedOn na nt horizontal idx (AxisSizing tag _ lo _)
  | tag == SizingGrow && hasExplicitMin lo && not (isFloatingNode nt) = growParentOn na horizontal idx
  | otherwise = pure False

-- | Measure a node with no measurable content: an image, a box, a spacer,
-- or a drawing without its own measure. Unless fixed, it takes @natural@
-- (an image's own size, a spacer's default), else its minimum, else 32,
-- within its limits.
measureImage :: NodeArena -> NodeIdx -> Maybe (Float, Float) -> IO ()
measureImage na idx natural = do
  wAx <- getWidthSizing na idx
  hAx <- getHeightSizing na idx
  let orMin ax = if axMin ax > 0 then axMin ax else 32
      (nw, nh) = fromMaybe (orMin wAx, orMin hAx) natural
  setRect na idx 0 0 (sizeWithin wAx nw) (sizeWithin hAx nh)

measureSeparator :: NodeArena -> NodeIdx -> IO ()
measureSeparator na idx = do
  dir <- getDirection na idx
  setRect na idx 0 0 (if dir == DirRow then 1 else 20) (if dir == DirRow then 20 else 1)

-- | A label beside a box or marker: the label, @leading@ and the padding
-- @pad@ wide, and as tall as the label or the box, plus the padding.
{-# INLINE measureMarkedWidget #-}
measureMarkedWidget ::
  FontMetrics ->
  (Text -> IO (Float, Float)) ->
  Text ->
  Float ->
  (Float, Float) ->
  IO (Float, Float)
measureMarkedWidget fm measure body leading (padX, padY) = do
  (mw, mh) <- measure (if T.null body then " " else body)
  pure (mw + padX + leading, max mh (checkboxBoxSize fm) + padY)

-- | A widget's size from its content, measured in the font paint draws it
-- in ('nodeMeasurer').
measureWidget :: SolveEnv -> NodeIdx -> IO ()
measureWidget env@SolveEnv {seArena = na, seArrays = a, seMs = Measurers {msFm = baseFm}} idx = do
  nt <- readTagEnum a idx TagNodeType
  txt <- getText na idx
  si <- readTree a idx TreeStyleIdx
  wAx <- readAxisSizing a idx True
  hAx <- readAxisSizing a idx False
  measurer@TextMeasurer {tmMetrics = fm} <- nodeMeasurer env idx nt si
  let measure = measureFontLine measurer
  -- The content with its padding and whatever sits beside the label.
  (rawW, rawH) <-
    case nt of
      NodeSlider -> pure (60, sliderHandleDiameter)
      NodeSelect -> do
        opts <- getOptions na idx
        let choices = if null opts then [""] else opts
            (padX, padY) = selectPadding fm
        (mw, mh) <-
          foldM
            (\(!mw, !mh) c -> (\(w, h) -> (max mw w, max mh h)) <$> measure (selectDisplayText txt c))
            (0, 0)
            choices
        pure (mw + padX + selectChevronReserve, mh + padY)
      -- Picker parts carry fixed layouts; the field grows to its square.
      NodeColorPicker -> pure (0, colorPickerSvH)
      NodeTextInput
        | hasFlag textInputFlagSelectable si ->
            measure (if T.null txt then " " else txt)
        -- Numeric field: a short editable box and its stepper.
        | hasFlag textInputFlagNumeric si ->
            pure (56 + numericStepperW, textInputFieldHeight fm)
        -- Caption-less search box: single row tall, icons counted in the
        -- width budget. Paint sizes the icons by the base font.
        | hasFlag textInputFlagSearch si -> do
            (lw, _) <- measure (if T.null txt then " " else txt)
            pure (max textInputMinWidth lw + searchInputReserveW baseFm, textInputFieldHeight fm)
        | otherwise -> do
            pw <- if T.null txt then pure 0 else fst <$> measure txt
            pure (max textInputMinWidth pw, textInputFieldHeight fm)
      -- A text area resolves its own font as it draws ('resolveTextAreaFont').
      NodeTextArea -> pure (textInputMinWidth, max 96 (textInputFieldHeight baseFm * 4))
      NodeButton -> case buttonKindOf si of
        ButtonChoice ->
          measureMarkedWidget fm measure txt (checkboxLeading fm) (0, 0)
        ButtonRow -> do
          let (depth, _, _) = treeDecodeStyle si
          measureMarkedWidget fm measure txt (treeRowLeading fm depth) (treeItemPadding fm)
        bkind -> do
          let body = if T.null txt then " " else txt
              (padX, padY) = case bkind of
                ButtonTable -> (tableHeaderPadX, 0)
                ButtonTab -> (2 * tabHeaderPadX, snd (buttonPadding fm))
                -- Menu rows reserve the same gutter the text-field context
                -- menu paints (outer pad + item pad on each side of the
                -- label), so the generic popup panel sizes identically.
                ButtonMenu -> (2 * (menuOuterPad + menuItemPadX), snd (buttonPadding fm))
                _ -> buttonPadding fm
          (mw, mh) <- measure body
          pure (mw + padX, mh + padY)
      -- 'measureContent' sends only widgets here, and decides per node
      -- type which ones; another widget is measured as a label with the
      -- widget padding.
      _ -> do
        let body = if T.null txt then " " else txt
            (padX, padY) = widgetPadding fm
        (mw, mh) <- measure body
        pure (mw + padX, mh + padY)
  -- A widget's children are its adornments ('measureAdorned').
  kids <- readTree a idx TreeFirstChild
  (w, h) <- if kids < 0 then pure (rawW, rawH) else measureAdorned env measurer idx nt si txt rawW rawH
  setRect na idx 0 0 (sizeWithin wAx w) (sizeWithin hAx h)

-- | The size of a button or text field with adornments ('adornRows'), from
-- its size without them, @rawW@ by @rawH@. A field widens by each row and a
-- gap. A button fits one group ('adornedButtonGroup'), padded as a label
-- except an icon alone, which the vertical padding squares.
measureAdorned :: SolveEnv -> TextMeasurer -> NodeIdx -> NodeType -> Int -> Text -> Float -> Float -> IO (Float, Float)
measureAdorned SolveEnv {seArena = na, seArrays = a} measurer@TextMeasurer {tmMetrics = fm} idx nt si txt rawW rawH = do
  gap <- readStyle a idx StyleGap
  rows@(AdornRows li lw ti tw rowH) <- adornRows na idx
  case nt of
    NodeTextInput ->
      let sides = fromIntegral (fromEnum (li >= 0) + fromEnum (ti >= 0))
       in pure (rawW + lw + tw + gap * sides, max rawH (rowH + rawH - fmLineHeight fm))
    -- A text field widens by its adornments; every other widget fits them
    -- in one group with its label.
    _ -> do
      (labelW, groupW) <- adornedButtonGroup measurer txt gap rows
      let kind = if nt == NodeButton then buttonKindOf si else ButtonPlain
          (padX, padY)
            | kind == ButtonTab = (2 * tabHeaderPadX, snd (buttonPadding fm))
            | otherwise = buttonPadding fm
          sidePad = if labelW > 0 || kind == ButtonContent then padX else padY
      pure (groupW + sidePad, max rawH (rowH + padY))

-- | A button's label width, in the font paint draws it in (0 without a
-- label), and the width of the group the label and its adornment rows make,
-- the node's gap apart.
adornedButtonGroup :: TextMeasurer -> Text -> Float -> AdornRows -> IO (Float, Float)
adornedButtonGroup measurer txt gap (AdornRows li lw ti tw _) = do
  labelW <- if T.null txt then pure 0 else fst <$> measureFontLine measurer txt
  let pieces = fromEnum (li >= 0) + fromEnum (labelW > 0) + fromEnum (ti >= 0)
  pure (labelW, lw + labelW + tw + gap * fromIntegral (max 0 (pieces - 1)))

-- | How a container lays out its flow children: in a grid, as layers, or
-- in lines along its direction ('TagDirection') that wrap or not. Grid
-- columns ('TreeGridCols' above 0, or 'StyleGridMinColW' above 0) take
-- precedence over the 'TagFlow'.
--
-- The constructors carry no fields, so choosing one allocates nothing even
-- where GHC passes it to a join point; a grid reads its columns with
-- 'readGridColumns'. The direction stays a separate case, so that each pass
-- calls its wrap or line layout from one site and GHC still inlines it.
data ContainerKind
  = KGrid
  | KLayered
  | KWrap
  | KLine

-- | Container @idx@'s 'ContainerKind'. The measure and position passes
-- branch on it, so a new layout mode is a constructor that
-- @-Wincomplete-patterns@ makes each of them handle.
{-# INLINE readContainerKind #-}
readContainerKind :: NodeArenaArrays -> NodeIdx -> IO ContainerKind
readContainerKind a idx = do
  gCols <- readTree a idx TreeGridCols
  minColW <- readStyle a idx StyleGridMinColW
  flow <- readTagEnum a idx TagFlow
  pure $
    if gCols > 0 || minColW > 0
      then KGrid
      else case flow of
        Layered -> KLayered
        Wrap -> KWrap
        Line -> KLine

-- | A grid container's column count ('TreeGridCols', 0 to pick it by width)
-- and minimum column width ('StyleGridMinColW').
{-# INLINE readGridColumns #-}
readGridColumns :: NodeArenaArrays -> NodeIdx -> IO (Int, Float)
readGridColumns a idx = (,) <$> readTree a idx TreeGridCols <*> readStyle a idx StyleGridMinColW

measureContainer :: SolveEnv -> NodeIdx -> IO ()
measureContainer env@SolveEnv {seArena = na, seArrays = a} idx = do
  (pad, gap, dir) <- containerFlow a idx
  -- Read here rather than in the grid branch: read there, they made the
  -- grow profile scene, which has no grid, allocate 0.005% more.
  (gCols, minColW) <- readGridColumns a idx
  kind <- readContainerKind a idx
  wAx@(AxisSizing wTag _ _ _) <- readAxisSizing a idx True
  hAx <- readAxisSizing a idx False
  nt <- readTagEnum a idx TagNodeType
  let chrome = isChromeColumn nt dir
      padX = padL pad + padR pad
      padY = padT pad + padB pad
      -- The room inside the fixed size, or else inside the maximum.
      inner (AxisSizing tag val lo hi) p = max 0 ((if tag == SizingFixed then clamp lo hi val else hi) - p)
      innerMaxW = inner wAx padX
      innerAvailH = inner hAx padY
  (contentW, contentH) <- case kind of
    KGrid -> measureGridScratch env idx wTag gCols minColW innerMaxW innerAvailH gap
    -- Layered children make a box as large as the largest on each axis.
    KLayered -> do
      let step (FlowAcc count maxW maxH) ci = do
            Rect _ _ w h <- getNodeRect na ci
            pure (FlowAcc (count + 1) (max maxW w) (max maxH h))
      FlowAcc _ layersW layersH <- foldFlowChildrenM na idx step (FlowAcc 0 0 0)
      pure (layersW, layersH)
    KWrap -> measureWrap env idx dir gap (if dir == DirRow then innerMaxW else innerAvailH)
    KLine
      | dir == DirColumn && chrome -> do
          n <- loadChildrenScratch na idx (flowChildSize env False innerMaxW innerAvailH)
          foldChromeColumnScratch na n gap
      | otherwise -> foldChildDimsFromParent env idx dir gap
  minAssignedW <- minAssignedOn na nt True idx wAx
  minAssignedH <- minAssignedOn na nt False idx hAx
  let h = sizeWithin hAx (if minAssignedH then 0 else contentH + padY)
  -- A window or modal that fits its width to its content leaves room for its
  -- body's scrollbar when the body will scroll, or the bar's gutter narrows
  -- the content below its measured width and clips its right edge.
  bodyGutter <-
    if chrome && wTag /= SizingFixed
      then floatingBodyGutter na idx (contentH + padY - h)
      else pure 0
  let measuredW = sizeWithin wAx (if minAssignedW then 0 else contentW + padX + bodyGutter)
  setRect na idx 0 0 measuredW h
  -- A parent lays a container out at the width it measured at without
  -- refitting its height ('flowChildSize', 'rowChildHeightAt'). Its children
  -- measured at widths of their own (an ancestor's cap, a grid cell unknown
  -- here, none for a filling label), so the measured height is the refit at
  -- the measured width, the height placement gives it there.
  steady <- readTagEnum a idx TagSteadyHeight
  when (axTag hAx == SizingFit && (nt == NodeContainer || nt == NodePanel) && not minAssignedH && not steady) $
    recomputeFitHeightAtWidth env idx measuredW >>= setRect na idx 0 0 measuredW

-- | Width the scrollbar of window or modal @idx@'s body takes when the
-- window is @overflow@ shorter than its content.
floatingBodyGutter :: NodeArena -> NodeIdx -> Float -> IO Float
floatingBodyGutter na idx overflow = do
  mBody <- windowBodyScroller na idx
  case mBody of
    Nothing -> pure 0
    Just ci -> do
      si <- getStyleIdx na ci
      dir <- getDirection na ci
      if isScrollStyle2D si || dir /= DirColumn
        then pure 0
        else do
          pad <- getPadding na ci
          Rect _ _ _ bodyH <- getNodeRect na ci
          contentH <- getNodeValue na ci
          let innerH = bodyH - padT pad - padB pad - max 0 overflow
          pure (scrollAxisGutter (scrollPolicyY (decodeScrollConfig si)) ScrollBarWindow (padR pad) contentH innerH)

-- | The scroll container holding window or modal @idx@'s body.
windowBodyScroller :: NodeArena -> NodeIdx -> IO (Maybe NodeIdx)
windowBodyScroller na idx =
  firstChildJustM na idx $ \ci -> do
    nt <- getNodeType na ci
    if nt /= NodeScrollContainer
      then pure Nothing
      else (\slot -> ci <$ guard (slot == ScrollBarWindow)) <$> scrollBarSlotOf na ci

measureScrollContainer :: SolveEnv -> NodeIdx -> IO ()
measureScrollContainer env@SolveEnv {seArena = na, seArrays = a} idx = do
  (pad, gap, dir) <- containerFlow a idx
  let padX = padL pad + padR pad
      padY = padT pad + padB pad
  si <- getStyleIdx na idx
  wAx@(AxisSizing wTag wVal wMin wMax) <- getWidthSizing na idx
  hAx@(AxisSizing hTag hVal hMin hMax) <- getHeightSizing na idx
  parent <- getParent na idx
  -- A modal's body scrolls like a window's: its bar sits just inside the
  -- panel's edge, out in the panel padding.
  isWin <-
    if parent < 0 then pure False else (`elem` [NodeWindow, NodeModal]) <$> getNodeType na parent
  inPanel <- hasPanelAncestor na parent
  let slot = classifyScrollBar isWin (wTag == SizingGrow && hTag == SizingGrow && not inPanel)
      cfg = decodeScrollConfig si
      vertical = not (isScrollStyle2D si) && dir == DirColumn
      fitsContent = wTag == SizingFit || wTag == SizingShrink
  writeTagEnum a idx TagScrollBarSlot slot
  -- A vertical scroller measures its children at the width it lays them out
  -- at, so what wraps there counts at its wrapped height. A column refits its
  -- children when it places them and grows to them; a scroller does not grow,
  -- so its measured height has to hold them already. The width is its own,
  -- or else the most its nearest bounded ancestor leaves it
  -- ('findAncestorMaxW'), and no more than its content when it fits its
  -- width to that. The bar's gutter comes off it when the content at its
  -- full width overflows the scroller's own height cap. A child with a
  -- percentage height keeps its measured height, since the scroller's height
  -- is not known yet.
  bound <-
    if not vertical
      then pure noLimit
      else if wTag == SizingFixed
        then pure (clamp wMin wMax wVal)
        else (\w -> min wMax (if wTag == SizingPercent then w * wVal / 100 else w)) <$> findAncestorMaxW na idx
  (contentW, contentH) <-
    if isUnbounded bound && not (vertical && fitsContent)
      then foldChildDimsFromParent env idx dir gap
      else do
        naturalW <- if fitsContent then fst <$> foldChildDimsFromParent env idx dir gap else pure noLimit
        let viewAt gutter = min naturalW (max 0 (bound - padX - gutter))
            stackAt viewW = do
              let step (FlowAcc count maxW sumH) ci = do
                    pct <- (== SizingPercent) . axTag <$> getHeightSizing na ci
                    (w, h) <-
                      if pct
                        then (\(Rect _ _ mw mh) -> (mw, mh)) <$> getNodeRect na ci
                        else flowChildSize env True viewW 0 ci
                    pure (FlowAcc (count + 1) (max maxW w) (sumH + h))
              FlowAcc count maxW sumH <- foldFlowChildrenM na idx step (FlowAcc 0 0 0)
              pure (maxW, sumH + gap * fromIntegral (max 0 (count - 1)))
        full@(_, fullH) <- stackAt (viewAt 0)
        let capH = (if hTag == SizingFixed then clamp hMin hMax hVal else hMax) - padY
            narrowed = viewAt (scrollAxisGutter (scrollPolicyY cfg) slot (padR pad) fullH capH)
        if narrowed /= viewAt 0 then stackAt narrowed else pure full
  -- A width fit to the content makes room for the bar when the content
  -- overflows the scroller's own height, fixed or capped.
  let h = sizeWithin hAx (contentH + padY)
      fitGutterW
        | not vertical || wTag == SizingGrow || wTag == SizingFixed = 0
        | otherwise = scrollAxisGutter (scrollPolicyY cfg) slot (padR pad) contentH (h - padY)
  if isScrollStyle2D si
    then setSolvedValue na idx contentH >> setScrollContentW na idx contentW
    else setSolvedValue na idx (if dir == DirColumn then contentH else contentW)
  setRect na idx 0 0 (sizeWithin wAx (contentW + padX + fitGutterW)) h

foldChildDimsFromParent :: SolveEnv -> NodeIdx -> DirTag -> Float -> IO (Float, Float)
foldChildDimsFromParent env@SolveEnv {seArena = na} idx dir gap = do
  -- Along the axis the children's sizes add up, and across it the largest counts.
  FlowAcc count along across <- foldFlowChildrenM na idx step (FlowAcc 0 0 0)
  let gaps = gap * fromIntegral (max 0 (count - 1))
  case dir of
    DirColumn -> pure (across, along + gaps)
    DirRow -> do
      -- A row's baseline-aligned children stand on one line, so together they
      -- are as tall as the most room any takes above it plus the most any takes
      -- below.
      FlowAcc _ above below <- foldFlowChildrenM na idx baselineStep (FlowAcc 0 0 0)
      pure (along + gaps, max across (above + below))
  where
    step (FlowAcc count along across) ci = do
      Rect _ _ w h <- getNodeRect na ci
      pure $ case dir of
        DirRow -> FlowAcc (count + 1) (along + w) (max across h)
        DirColumn -> FlowAcc (count + 1) (along + h) (max across w)
    baselineStep acc@(FlowAcc _ above below) ci = do
      ay <- getAlignY na ci
      if ay /= AlignBaseline
        then pure acc
        else do
          Rect _ _ _ h <- getNodeRect na ci
          b <- childBaseline env ci h
          pure (FlowAcc 0 (max above b) (max below (h - b)))

-- | Content size of wrapping container @idx@ ('Wrap'), with lines along
-- @dir@ at most @bound@ long ('wrapExtent').
--
-- A row with no bound of its own (1e8 or more) wraps at its nearest bounded
-- ancestor's width, like text ('findAncestorMaxW'), or stays on one line if
-- there is none. A grow row whose width comes from its parent ('growParent')
-- reports only its widest child's width and one line's height, then wraps
-- at the width it gets: a column refits its height
-- ('recomputeFitHeightAtWidth'); elsewhere it grows to its lines after
-- placement ('adjustFitHeight').
measureWrap :: SolveEnv -> NodeIdx -> DirTag -> Float -> Float -> IO (Float, Float)
measureWrap env@SolveEnv {seArena = na} idx dir gap bound = do
  limit <-
    if dir == DirRow && isUnbounded bound
      then do
        pad <- getPadding na idx
        (\w -> max 0 (w - padL pad - padR pad)) <$> findAncestorMaxW na idx
      else pure bound
  -- A percentage width is of the line length, as placement resolves it
  -- ('positionWrap').
  let sized ci = do
        Rect _ _ w h <- getNodeRect na ci
        w' <- if dir == DirRow then (\ax -> percentOr ax limit w) <$> getWidthSizing na ci else pure w
        pure (ci, (w', h))
  kids <- mapM sized =<< flowChildrenInOrder na idx
  (along, across) <- wrapExtent env idx dir gap limit kids
  wTag <- axTag <$> getWidthSizing na idx
  assigned <-
    if dir == DirRow && isUnbounded limit && wTag == SizingGrow then growParent na idx else pure False
  let widest = foldl' (\m (_, (w, _)) -> max m w) 0 kids
  pure (if dir == DirRow then (if assigned then widest else along, across) else (across, along))

-- | Extent of @kids@ broken into lines no longer than @limit@ ('wrapLines'):
-- the longest line's length, and the sum of the lines' cross sizes
-- ('wrapLineCross') plus the line gaps.
wrapExtent :: SolveEnv -> NodeIdx -> DirTag -> Float -> Float -> [(NodeIdx, (Float, Float))] -> IO (Float, Float)
wrapExtent env idx dir gap limit kids = do
  lineGap <- readStyle (seArrays env) idx StyleLineGap
  let along (_, (w, h)) = if dir == DirRow then w else h
      lns = wrapLines limit gap along kids
      lineLength l = sum (map along l) + gap * fromIntegral (length l - 1)
  crosses <- mapM (wrapLineCross env dir) lns
  pure (foldl' max 0 (map lineLength lns), sum crosses + lineGap * fromIntegral (max 0 (length lns - 1)))

-- | A wrap line's cross size: its largest child's. In a row it is at least
-- the tallest ascent plus the deepest descent among baseline-aligned
-- children, as in 'foldChildDimsFromParent'.
wrapLineCross :: SolveEnv -> DirTag -> [(NodeIdx, (Float, Float))] -> IO Float
wrapLineCross env@SolveEnv {seArena = na} dir line = do
  let across = foldl' (\m (_, (w, h)) -> max m (if dir == DirRow then h else w)) 0 line
      baselineStep acc@(!above, !below) (ci, (_, h)) = do
        ay <- getAlignY na ci
        if ay /= AlignBaseline
          then pure acc
          else (\b -> (max above b, max below (h - b))) <$> childBaseline env ci h
  if dir /= DirRow
    then pure across
    else (\(above, below) -> max across (above + below)) <$> foldM baselineStep (0, 0) line

-- | Content size of grid @idx@ with @gCols@ columns, or else as many
-- @minColW@ wide as fit in @innerMaxW@.
--
-- Without a bound, columns of @minColW@ come out one, and the grid reports
-- one column's width. When its width is assigned from above instead (a grow
-- grid whose parent grows, 'growParent', or a percentage), it reports one
-- row's height too, as a wrapping row does ('measureWrap'): the width it
-- gets decides its rows. A column refits its height at that width
-- ('recomputeFitHeightAtWidth'), and elsewhere it grows to its rows after
-- placement ('adjustFitHeight'). Every cell stacked would be the least
-- height its fit parents take ('positionNode'), a gap below a grid of
-- several columns.
measureGridScratch ::
  SolveEnv ->
  NodeIdx ->
  SizingTag ->
  Int ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO (Float, Float)
measureGridScratch env idx wTag gCols minColW innerMaxW innerAvailH gap = do
  let bounded = isBounded innerMaxW
      cols = gridColumnCount gCols minColW (if bounded then innerMaxW else 0) gap
      colW = max 0 ((innerMaxW - gap * fromIntegral (cols - 1)) / fromIntegral cols)
  n <- loadChildrenScratch (seArena env) idx (flowChildSize env bounded (if bounded then colW else innerMaxW) innerAvailH)
  if n <= 0
    then pure (0, 0)
    else do
      FlexScratch {fsW = wArr, fsH = hArr} <- readIORef (naScratch (seArena env))
      assigned <-
        if bounded || gCols > 0
          then pure False
          else if wTag == SizingGrow then growParent (seArena env) idx else pure (wTag == SizingPercent)
      let rowCols = if assigned then n else cols
          numRows = (n + rowCols - 1) `quot` rowCols
      totalH <- foldUpTo numRows (\t r -> (t +) <$> gridRowHeight hArr n rowCols r) 0
      let contentH = totalH + gap * fromIntegral (max 0 (numRows - 1))
      contentW <-
        if innerMaxW > 0 && isBounded innerMaxW
          then pure innerMaxW
          else if minColW > 0
            then pure (fromIntegral cols * minColW + gap * fromIntegral (max 0 (cols - 1)))
            else do
              maxChildW <- foldUpTo n (\m i -> max m <$> readPrimArray wArr i) 0
              pure (fromIntegral cols * maxChildW + gap * fromIntegral (max 0 (cols - 1)))
      pure (contentW, contentH)

-- | The height node @idx@ takes when @availW@ is offered to it: text and a
-- measured drawing can wrap taller when narrower, and so can a column that
-- holds them. Memoized per node and width for the frame.
recomputeFitHeightAtWidth :: SolveEnv -> NodeIdx -> Float -> IO Float
recomputeFitHeightAtWidth env@SolveEnv {seArrays = a} idx availW =
  readTagEnum a idx TagSteadyHeight >>= \case
    -- Its measured height, at any width ('markSteadyHeight').
    True -> readPrimArray (seMeasured env) (idx * 2 + 1)
    False -> recomputeUnsteady env idx availW

recomputeUnsteady :: SolveEnv -> NodeIdx -> Float -> IO Float
recomputeUnsteady env@SolveEnv {seArena = na, seArrays = a} idx availW =
  fmap snd . memoizeWidth na (naFitMemo na) idx availW . fmap ((,) 0) $ do
    nt <- getNodeType na idx
    AxisSizing wTag wVal minW maxW <- getWidthSizing na idx
    hAx@(AxisSizing hTag _ minH maxH) <- getHeightSizing na idx
    Rect _ _ oldW oldH <- getNodeRect na idx
    let -- The width a node measured @iw@ wide takes of @avail@, as a
        -- column places it ('columnChildX'): fixed, a percentage, all of it
        -- when it grows, or else its measured width, narrowed to @avail@.
        widthOf (AxisSizing tag val lo hi) iw avail = case tag of
          SizingPercent -> avail * val / 100
          SizingFixed -> clamp lo hi val
          SizingGrow -> avail
          _ -> min iw avail
        -- Every caller offers this node a percentage already resolved, as
        -- 'childH' below does, so only a fixed width replaces the offer.
        effW' = clamp minW maxW (if wTag == SizingFixed then wVal else availW)

    ratio <- if hTag == SizingFit then readStyle a idx StyleAspect else pure 0
    minAssignedH <-
      if nt == NodeContainer || nt == NodePanel then minAssignedOn na nt False idx hAx else pure False
    let -- A container's height at this width. Called from two alternatives
        -- below, both tail calls, so it compiles to a join point.
        refit
          | hTag == SizingFixed = pure oldH
          | otherwise = do
            (pad, gap, dir) <- containerFlow a idx
            kind <- readContainerKind a idx
            let innerW = max 0 (effW' - padL pad - padR pad)
                -- A child's height when @w@ is offered to it, kept to its max width.
                childH w ci = do
                  subAx <- getWidthSizing na ci
                  Rect _ _ iw _ <- getNodeRect na ci
                  let subW = widthOf subAx iw w
                  recomputeFitHeightAtWidth env ci (if isBounded (axMax subAx) then min subW (axMax subAx) else subW)
                padded h = pure (clamp minH maxH (h + padT pad + padB pad))
            case kind of
              -- A grid stacks rows, each as tall as its tallest cell at the
              -- column width, not every cell.
              KGrid -> do
                (gCols, minColW) <- readGridColumns a idx
                let cols = gridColumnCount gCols minColW innerW gap
                    colW = max 0 ((innerW - gap * fromIntegral (cols - 1)) / fromIntegral cols)
                    rows [] = []
                    rows hs = let (r, rest) = splitAt cols hs in r : rows rest
                hs <- mapM (childH colW) =<< flowChildrenInOrder na idx
                let rowHs = map (foldl' max 0) (rows hs)
                padded (if null rowHs then 0 else sum rowHs + gap * fromIntegral (length rowHs - 1))
              -- Layers are as tall as the tallest child at this width.
              KLayered -> padded . foldl' max 0 =<< mapM (childH innerW) =<< flowChildrenInOrder na idx
              -- A wrapping row is as tall as the lines this width breaks it
              -- into, with percentages resolved against the width. A
              -- wrapping column's height does not depend on its width.
              KWrap
                | dir == DirRow -> do
                    let sized ci = do
                          subAx <- getWidthSizing na ci
                          Rect _ _ w h <- getNodeRect na ci
                          pure (ci, (percentOr subAx innerW w, h))
                    kids <- mapM sized =<< flowChildrenInOrder na idx
                    lineGap <- readStyle a idx StyleLineGap
                    -- A line is as thick as its measured children
                    -- ('wrapLineCross') or what they grow to at their shares
                    -- of it, as 'positionWrap' places it.
                    let lns = wrapLines innerW gap (fst . snd) kids
                        lineCross l = do
                          measured <- wrapLineCross env dir l
                          maybe measured (max measured) <$> lineHeightAt env l innerW gap
                    crosses <- mapM lineCross lns
                    padded (sum crosses + lineGap * fromIntegral (max 0 (length lns - 1)))
                | otherwise -> pure oldH
              KLine
                | dir == DirRow -> rowHeightAt env idx innerW gap >>= maybe (pure oldH) padded
                | otherwise -> do
                    let step (FlowAcc count contentH _) ci = do
                          subH <- childH innerW ci
                          pure (FlowAcc (count + 1) (contentH + subH) 0)
                    FlowAcc count contentH _ <- foldFlowChildrenM na idx step (FlowAcc 0 0 0)
                    padded (if count <= 0 then 0 else contentH + gap * fromIntegral (count - 1))
    case nt of
      -- A fit height with an aspect ratio is width / ratio.
      _ | ratio > 0 -> pure (clamp minH maxH (effW' / ratio))

      NodeText
        | hTag /= SizingFixed -> do
            isRowChild <- parentIsRow na idx
            textHeightAt env idx hAx effW' (wTag /= SizingFit && not isRowChild) oldH
        | otherwise -> pure oldH

      -- A measured drawing, like wrapped text, can be taller when narrower.
      -- At the width it measured at, it keeps its measured height, as
      -- 'positionNode' does: its measure was offered no width limit there.
      NodeDrawing
        | hTag == SizingFit && abs (effW' - oldW) > 1.0e-3 -> drawingHeightAt env idx effW' hAx oldH
        | otherwise -> pure oldH

      -- A container that reports its minimum height ('minAssignedOn') is as
      -- tall as it is given, whatever it holds.
      _ | minAssignedH -> pure oldH

      NodeContainer -> refit
      NodePanel -> refit
      NodeSpacer -> pure oldH
      NodeSeparator -> pure oldH
      NodeButton -> pure oldH
      NodeSlider -> pure oldH
      NodeTextInput -> pure oldH
      NodeTextArea -> pure oldH
      NodeScrollContainer -> pure oldH
      NodeSelect -> pure oldH
      NodeModal -> pure oldH
      NodeImage -> pure oldH
      NodeWindow -> pure oldH
      NodeBox -> pure oldH
      NodeColorPicker -> pure oldH
      NodePopup -> pure oldH

-- | The content height of row @idx@ laid out @innerW@ wide inside its
-- padding ('lineHeightAt'), or 'Nothing' when every child keeps its
-- measured width there.
rowHeightAt :: SolveEnv -> NodeIdx -> Float -> Float -> IO (Maybe Float)
rowHeightAt env@SolveEnv {seArena = na} idx innerW gap = do
  anyFlex <- foldFlowChildrenM na idx (\acc ci -> if acc then pure True else flexesAlongRow env ci) False
  if not anyFlex
    then pure Nothing
    else do
      kids <- mapM (\ci -> (ci,) <$> flowChildSize env False innerW noLimit ci) =<< flowChildrenInOrder na idx
      lineHeightAt env kids innerW gap

-- | Whether a row's child can come out another width than it measured: it
-- grows, shrinks, takes a percentage or scrolls.
flexesAlongRow :: SolveEnv -> NodeIdx -> IO Bool
flexesAlongRow SolveEnv {seArrays = a} ci = do
  tag <- axTag <$> readAxisSizing a ci True
  nt <- readTagEnum a ci TagNodeType
  steady <- readTagEnum a ci TagSteadyHeight
  pure (not steady && ((tag /= SizingFit && tag /= SizingFixed) || nt == NodeScrollContainer))

-- | The height of a line of a row's children @kids@, each with its size
-- before the sharing, laid out @avail@ wide with @gap@ between them, or
-- 'Nothing' when every child keeps its measured width there. The line
-- shares the width out as 'positionRowFromParent' and 'positionWrap' do. A
-- child given another width than it measured at takes its height at that
-- width ('recomputeFitHeightAtWidth'), a fit container no less than it
-- measured, since 'placeRowLine' offers it that. Baseline-aligned children
-- stand on one line, as in 'foldChildDimsFromParent'.
--
-- The sharing runs in a scratch of its own: a parent can ask for this height
-- while it is filling the arena's scratch with its children's sizes.
lineHeightAt :: SolveEnv -> [(NodeIdx, (Float, Float))] -> Float -> Float -> IO (Maybe Float)
lineHeightAt env@SolveEnv {seArena = na, seArrays = a} kids avail gap = do
  anyFlex <- foldM (\acc (ci, _) -> if acc then pure True else flexesAlongRow env ci) False kids
  if not anyFlex
    then pure Nothing
    else do
      let n = length kids
      saved <- readIORef (naScratch na)
      scratch@FlexScratch {fsIdx = idxArr, fsW = wArr, fsH = hArr, fsOut = outArr} <- newFlexScratch (max 1 n)
      writeIORef (naScratch na) scratch
      forM_ (zip [0 ..] kids) $ \(i, (ci, (w, h))) ->
        writePrimArray idxArr i ci >> writePrimArray wArr i w >> writePrimArray hArr i h
      distributeScratch na n avail (gap * fromIntegral (max 0 (n - 1))) True
      shares <- mapM (\i -> (,) <$> readPrimArray idxArr i <*> readPrimArray outArr i) [0 .. n - 1]
      writeIORef (naScratch na) saved
      let heightAt (!changed, hs) (ci, share) =
            rowChildHeightAt env ci share >>= \case
              Just h -> pure (True, (ci, h) : hs)
              Nothing -> (\h -> (changed, (ci, h) : hs)) <$> readGeom a ci GeomH
      (changed, hs) <- foldM heightAt (False, []) shares
      if not changed
        then pure Nothing
        else do
          let across = foldl' (\m (_, h) -> max m h) 0 hs
              baselineStep acc@(!above, !below) (ci, h) = do
                ay <- getAlignY na ci
                if ay /= AlignBaseline
                  then pure acc
                  else (\b -> (max above b, max below (h - b))) <$> childBaseline env ci h
          (above, below) <- foldM baselineStep (0, 0) hs
          pure (Just (max across (above + below)))

-- | The height row child @ci@ comes out when its row gives it @share@ of
-- its width, or 'Nothing' when it keeps its measured height: it keeps its
-- measured width there, or its height is fixed, a percentage or grows. Its
-- height at its new width ('recomputeFitHeightAtWidth'), and for a fit
-- container no less than it measured, since 'placeRowLine' offers it that.
rowChildHeightAt :: SolveEnv -> NodeIdx -> Float -> IO (Maybe Float)
rowChildHeightAt env@SolveEnv {seArrays = a} ci share = readTagEnum a ci TagSteadyHeight >>= \case
  True -> pure Nothing
  False -> rowChildHeightAtUnsteady env ci share

rowChildHeightAtUnsteady :: SolveEnv -> NodeIdx -> Float -> IO (Maybe Float)
rowChildHeightAtUnsteady env@SolveEnv {seArrays = a} ci share = do
  AxisSizing wTag wVal lo hi <- readAxisSizing a ci True
  hTag <- axTag <$> readAxisSizing a ci False
  iw <- readGeom a ci GeomW
  -- The width 'positionNode' gives a child offered its share.
  let w = clamp lo hi $ case wTag of
        SizingFixed -> wVal
        SizingGrow -> min share hi
        SizingPercent -> min share hi
        _ -> min iw share
  if abs (w - iw) <= 1.0e-3 || (hTag /= SizingFit && hTag /= SizingShrink)
    then pure Nothing
    else do
      nt <- readTagEnum a ci TagNodeType
      r <- recomputeFitHeightAtWidth env ci w
      if nt == NodeContainer || nt == NodePanel
        then Just . max r <$> readGeom a ci GeomH
        else pure (Just r)

-- | Record whether node @idx@ is as tall at every width it can be laid out
-- at ('TagSteadyHeight'), once its children are recorded. Then the refits
-- ('recomputeFitHeightAtWidth', 'rowChildHeightAt') take its measured height
-- instead of working it out again. A fixed or percentage height is steady,
-- as is a node with no content that wraps: text wraps unless it is empty,
-- has no newline and is fit or a row's child ('textHeightAt'); a drawing
-- with a custom measure, an aspect ratio, a wrapping container and a grid
-- that picks its column count by width can all change height with width;
-- and a container is steady when its flow children all are.
--
-- A child that is not steady leaves 2 in its parent's column, so a
-- container need not walk its children: 'measurePass' marks every child
-- before its parent. A floating child is laid out apart from the flow, so
-- it leaves no mark.
markSteadyHeight :: SolveEnv -> NodeIdx -> IO ()
markSteadyHeight env@SolveEnv {seArena = na, seArrays = a} idx = do
  hTag <- readTagEnum a idx TagHSizing
  nt <- readTagEnum a idx TagNodeType
  ratio <- readStyle a idx StyleAspect
  let container = do
        kind <- readContainerKind a idx
        childMark <- readTagEnum a idx TagSteadyHeight
        -- Wrapping lines and a grid that picks its column count by width
        -- can change height with width.
        byWidth <- case kind of
          KGrid -> (> 0) <$> readStyle a idx StyleGridMinColW
          KLayered -> pure False
          KWrap -> pure True
          KLine -> pure False
        pure (not byWidth && childMark /= (2 :: Int))
  steady <-
    if hTag == SizingFixed || hTag == SizingPercent
      then pure True
      else if ratio > 0
        then pure False
        else case nt of
          NodeText -> do
            txt <- getText na idx
            wTag <- readTagEnum a idx TagWSizing
            rowChild <- parentIsRow na idx
            pure (T.null txt || (not (T.any (== '\n') txt) && (wTag == SizingFit || rowChild)))
          NodeDrawing -> not . isJust <$> (msLookupMeasure (seMs env) =<< getWidgetId na idx)
          NodeContainer -> container
          NodePanel -> container
          NodeSpacer -> pure True
          NodeSeparator -> pure True
          NodeButton -> pure True
          NodeSlider -> pure True
          NodeTextInput -> pure True
          NodeTextArea -> pure True
          NodeScrollContainer -> pure True
          NodeSelect -> pure True
          NodeModal -> pure True
          NodeImage -> pure True
          NodeWindow -> pure True
          NodeBox -> pure True
          NodeColorPicker -> pure True
          NodePopup -> pure True
  writeTagEnum a idx TagSteadyHeight steady
  unless (steady || isFloatingNode nt) $ do
    p <- readTree a idx TreeParent
    when (p >= 0) $ writeTagEnum a p TagSteadyHeight (2 :: Int)

-- | Load a parent's flow children into the flex scratch in child order, with
-- each child's (width, height) from @sizeOf@. Returns the child count.
{-# INLINE loadChildrenScratch #-}
loadChildrenScratch :: NodeArena -> NodeIdx -> (NodeIdx -> IO (Float, Float)) -> IO Int
loadChildrenScratch na parent sizeOf = do
  cc <- arenaArrays na >>= \a -> readTree a parent TreeChildCount
  FlexScratch {fsIdx = idxArr, fsW = wArr, fsH = hArr} <- ensureScratchCapacity na cc
  -- The sibling links run from the last child to the first, so the children
  -- fill the arrays from the end. Floating children leave room at the front,
  -- which the copies close.
  let write !i ci = do
        (w, h) <- sizeOf ci
        writePrimArray idxArr i ci
        writePrimArray wArr i w
        writePrimArray hArr i h
        pure (i - 1)
  lo <- (+ 1) <$> foldFlowChildrenM na parent write (cc - 1)
  when (lo > 0) $ do
    copyMutablePrimArray idxArr 0 idxArr lo (cc - lo)
    copyMutablePrimArray wArr 0 wArr lo (cc - lo)
    copyMutablePrimArray hArr 0 hArr lo (cc - lo)
  pure (cc - lo)

-- | Scratch size of a flow child: its measured box, with percent sizing
-- resolved against the parent's inner box. With @refit@, a fit-height child
-- that the parent narrows (grow or percent width, or wider than @availW@) is
-- re-measured at the assigned width.
flowChildSize :: SolveEnv -> Bool -> Float -> Float -> NodeIdx -> IO (Float, Float)
flowChildSize env refit availW availH ci = do
  let a = seArrays env
  w <- readGeom a ci GeomW
  h <- readGeom a ci GeomH
  wAx@(AxisSizing wTag _ _ _) <- readAxisSizing a ci True
  hAx@(AxisSizing hTag _ _ _) <- readAxisSizing a ci False
  let w' = percentOr wAx availW w
  h' <-
    if refit && hTag /= SizingFixed && hTag /= SizingPercent && (wTag == SizingGrow || wTag == SizingPercent || availW < w)
      then recomputeFitHeightAtWidth env ci (if wTag == SizingPercent then w' else availW)
      else pure (percentOr hAx availH h)
  pure (w', h')

-- | A percentage of @avail@ within the axis's limits, or else @size@.
-- Ignores unbounded extents (>= 1e8) so percentage children inside fit-sized
-- modals, windows, or unbounded grids keep their measured size.
{-# INLINE percentOr #-}
percentOr :: AxisSizing -> Float -> Float -> Float
percentOr (AxisSizing tag val lo hi) avail size =
  if tag == SizingPercent && isBounded avail then clamp lo hi (avail * val / 100) else size

-- | Distance from the top of node @ci@, laid out @h@ tall, to its first
-- baseline, as in CSS:
--
-- * text: its first line's, where the text-span collector puts it. One
--   line is centered in the box, and wrapped lines start at the top. Paint
--   wraps at explicit newlines, and outside a row where the line overflows
--   'textWrapCap'.
-- * a widget with a label (a button, select, checkbox): the label's, which
--   paint centers in the widget.
-- * a container: the baseline its baseline-aligned children share if it is a
--   row that has some, and otherwise its first child's, offset by where
--   that child's alignment places it in a row or among layers.
-- * anything else: its bottom edge.
childBaseline :: SolveEnv -> NodeIdx -> Float -> IO Float
childBaseline env@SolveEnv {seArena = na, seArrays = a} ci h = do
  nt <- getNodeType na ci
  si <- getStyleIdx na ci
  let -- Widget labels take the node's font size in the default face.
      label = do
        fm <- tmMetrics <$> nodeMeasurer env ci nt si
        pure (textBaseline fm h)
      container = do
        kids <- flowChildrenInOrder na ci
        case kids of
          [] -> pure h
          first : _ -> do
            (pad, _, dir) <- containerFlow a ci
            let innerH = max 0 (h - padT pad - padB pad)
                heightOf k = rectH <$> getNodeRect na k
            aligned <-
              if dir == DirRow then filterM (fmap (== AlignBaseline) . getAlignY na) kids else pure []
            grouped <- mapM (\k -> heightOf k >>= childBaseline env k) aligned
            (padT pad +) <$> case grouped of
              _ : _ -> pure (maximum grouped)
              [] -> do
                fh <- heightOf first
                flow <- getFlow na ci
                ay <- if dir == DirRow || flow == Layered then getAlignY na first else pure AlignTop
                (alignY ay 0 innerH fh +) <$> childBaseline env first fh
  case nt of
    NodeText -> do
      raw <- getText na ci
      if T.null raw
        then pure h
        else do
          measurer@TextMeasurer {tmMetrics = fm} <- nodeMeasurer env ci nt si
          rowChild <- parentIsRow na ci
          let newline = T.any (== '\n') raw
          wrapped <-
            if newline || rowChild
              then pure newline
              else do
                AxisSizing wTag _ _ maxW <- getWidthSizing na ci
                Rect _ _ w _ <- getNodeRect na ci
                -- Only fit text wraps at the ancestor cap ('textWrapCap').
                effMaxW <- if isBounded maxW || (wTag /= SizingFit && w > 0) then pure maxW else findAncestorMaxW na ci
                let cap = textWrapCap effMaxW wTag w
                (tw, _) <- measureFontLine measurer raw
                pure (isBounded cap && cap + 0.5 < tw)
          pure (textBaseline fm (if wrapped then fmLineHeight fm else h))
    -- A close button's cross is no label.
    NodeButton
      | hasFlag buttonFlagClose si -> pure h
      | otherwise -> label
    NodeSelect -> label
    NodeContainer -> container
    NodeScrollContainer -> container
    NodeModal -> container
    NodePanel -> container
    NodeWindow -> container
    NodePopup -> container
    NodeSpacer -> pure h
    NodeSeparator -> pure h
    NodeSlider -> pure h
    NodeTextInput -> pure h
    NodeTextArea -> pure h
    NodeImage -> pure h
    NodeBox -> pure h
    NodeColorPicker -> pure h
    NodeDrawing -> pure h
  where
    textBaseline fm boxH = centeredTextY fm 0 boxH (fmLineHeight fm) + fmAscent fm

-- | Where a scroll container's bar sits, as measurement stored it. Text
-- areas and other nodes read 'ScrollBarList'.
{-# INLINE scrollBarSlotOf #-}
scrollBarSlotOf :: NodeArena -> NodeIdx -> IO ScrollBarSlot
scrollBarSlotOf na idx = arenaArrays na >>= \a -> readTagEnum a idx TagScrollBarSlot

-- | Whether @p@ or an ancestor below the nearest floating node is a panel.
hasPanelAncestor :: NodeArena -> NodeIdx -> IO Bool
hasPanelAncestor na p =
  fmap (fromMaybe False) . walkAncestors na p $ \i -> do
    nt <- getNodeType na i
    pure (if nt == NodePanel then Just True else if isFloatingNode nt then Just False else Nothing)
