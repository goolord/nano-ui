-- | The layout solver: measures and places the node arena's flow tree, then
-- positions modals, windows and popups.
module NanoUI.Internal.Layout.Solve
  ( solveLayout
  , runCustomMeasure
  , Measurers (..)
  , placeFloatingNodes
  , computePopupPosition
  , placeWindowNode
  , scrollBarSlotOf
  , windowBodyScroller
  , findAncestorMaxW
  , textWrapCap
  ) where

import Control.Monad (filterM, foldM, foldM_, forM_, guard, mfilter, unless, when, zipWithM_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Primitive.PrimArray
  ( copyMutablePrimArray
  , newPrimArray
  , readPrimArray
  , setPrimArray
  , writePrimArray
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8, Word64)
import GHC.Float (castFloatToWord32)
import NanoUI.Internal.Font
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Style (AlignX (..), AlignY (..), Flow (..), FontStyle (..), FontVariant (..), FontWeight (..), Padding (..), lineAlignFraction, windowMargin)
import NanoUI.Internal.Types (PopupAnchor (..), PopupPlacement (..), Rect (..), V2 (..), clamp, foldUpTo, forUpTo_, gridSpan, onGrid)
import NanoUI.Internal.WidgetText
import NanoUI.Internal.Frame.Scroll.Geometry

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
  pure (SolveEnv na a ms sub measured (mfilter ((> 0) . lcCount) mCache) Nothing)

-- | Strict accumulator for flow-child folds: a child count and two running
-- sums or extents. The strict fields keep the folds unboxed.
data FlowAcc = FlowAcc !Int !Float !Float

-- Keep font selection and single-line/wrapped measurement together so every
-- layout pass uses the same policy. Monospaced text uses its metrics directly;
-- proportional text uses the host's shaping-aware measurement callback.
data TextMeasurer = TextMeasurer
  { tmMetrics :: !FontMetrics
  , tmVariant :: !FontVariant
  , tmFontKey :: !Int
  , tmHostLine :: Text -> IO (Float, Float)
  }

-- | How node @idx@ measures its text: a text node or text field in its own
-- font, and any other widget's label in its font size in the default face,
-- as 'NanoUI.Internal.Frame.Node.resolveFontFor' draws them.
textNodeMeasurer :: SolveEnv -> NodeIdx -> IO TextMeasurer
textNodeMeasurer env@SolveEnv {seArena = na} idx = do
  nt <- getNodeType na idx
  si <- getStyleIdx na idx
  nodeMeasurer env idx nt si

-- | 'textNodeMeasurer' for a node whose type and style index the caller has.
nodeMeasurer :: SolveEnv -> NodeIdx -> NodeType -> Int -> IO TextMeasurer
nodeMeasurer SolveEnv {seArena = na, seMs = ms} idx nt nodeSi = do
  let si = if packsNodeFont nt then nodeSi else 0
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
-- @outerW@ minus its label inset when it has explicit newlines, or when
-- @shouldWrap wrapW lineW@ holds for its single-line width.
measureTextNodeAt :: SolveEnv -> NodeIdx -> Text -> Float -> (Float -> Float -> Bool) -> IO TextBox
measureTextNodeAt env idx txt outerW shouldWrap = do
  measurer@TextMeasurer {tmMetrics = textFm} <- textNodeMeasurer env idx
  (tw0, th0) <- measureFontLine measurer txt
  let wrapW = max 0 outerW
      lineH = fmLineHeight textFm
      na = seArena env
  if T.any (== '\n') txt || shouldWrap wrapW tw0
    then do
      (tw, th) <- memoizeWidth na (naWrapMemo na) idx wrapW (measureFontWrapped env measurer txt wrapW)
      pure (TextBox True tw th lineH)
    else pure (TextBox False tw0 th0 lineH)

-- | The height text node @idx@ takes at the width @w@ within the limits of
-- @hAx@: none for no text, and else @fallback@ when its text does not wrap
-- there. It wraps at its newlines, and where it is @allowed@ to and its line
-- overflows a positive width.
{-# INLINE textHeightAt #-}
textHeightAt :: SolveEnv -> NodeIdx -> AxisSizing -> Float -> Bool -> Float -> IO Float
textHeightAt env !idx (AxisSizing _ _ minH maxH) !w allowed !fallback = do
  txt <- getText (seArena env) idx
  if T.null txt
    then pure (clamp minH maxH 0)
    else do
      TextBox {tbWrapped, tbH, tbLineH} <-
        measureTextNodeAt env idx txt w (\wrapW lineW -> allowed && wrapW + 0.5 < lineW && wrapW > 0)
      pure (if tbWrapped then clamp minH maxH (max tbLineH tbH) else fallback)

-- | Measure nodes and place the page within the supplied logical width/height.
-- The view must have finished adding nodes, and 'computeSubtreeHashes' must
-- have run for this frame. Place floating nodes separately with
-- 'placeFloatingNodes', then apply scrolling.
-- When a layout cache captured under the same font metrics is supplied, a
-- node whose inputs, ancestors' inputs and subtree are unchanged since that
-- solve, and whose children all came out their captured sizes, takes its
-- captured measured size instead of measuring again ('restoreMeasured').
-- Position and quantization run over every node regardless, so the result is
-- exactly what a full solve computes. Returns each custom-measured node's
-- 'CustomMeasureRecord', for the layout cache.
solveLayout :: NodeArena -> Measurers -> Float -> Float -> Maybe LayoutCache -> IO (IntMap CustomMeasureRecord)
solveLayout na ms rootW rootH mCache = do
  count <- arenaCount na
  if count <= 0
    then pure IM.empty
    else do
      measureLog <- newIORef IM.empty
      env0 <- solveEnv na ms mCache
      let env = env0 {seMeasureLog = Just measureLog}
      measurePass env count
      positionNodeA env 0 0 (Rect 0 0 rootW rootH)
      floatingCount <- floatingNodeCount na
      quantizeResultsA (seArrays env) count floatingCount (fmSnapScale (msFm ms))
      readIORef measureLog

-- | Snap the solved geometry of the @count@ nodes to the device pixel grid of
-- scale @s@. @floatingCount@ is the arena's floating node count.
quantizeResultsA :: NodeArenaArrays -> Int -> Int -> Float -> IO ()
quantizeResultsA a count floatingCount s
  | s <= 0 = pure ()
  -- Nothing floats, so every node snaps and none needs marking.
  | floatingCount <= 0 = forUpTo_ count snapNode
  | otherwise = do
      -- A floating node (modal, window, popup) and everything inside it is
      -- laid out by placement after the solve, which sizes the subtree from
      -- these measured sizes. Rounding them here would size a dialog and its
      -- content-sized parts off their content, so the subtree keeps them;
      -- placement overwrites its geometry anyway. A parent always precedes
      -- its children, so one pass marks each node from its parent.
      floating <- newPrimArray count :: IO (IOArr Word8)
      forUpTo_ count $ \i -> do
        nt <- readTagEnum a i TagNodeType
        parent <- readTree a i TreeParent
        inFloating <-
          if isFloatingNode nt
            then pure True
            else if parent >= 0 then (/= 0) <$> readPrimArray floating parent else pure False
        writePrimArray floating i (if inFloating then 1 else 0)
        unless inFloating (snapNode i)
 where
  snapNode i = do
    x <- readGeom a i GeomX
    y <- readGeom a i GeomY
    w <- readGeom a i GeomW
    h <- readGeom a i GeomH
    -- Snap both edges and take the size between them. Rounding the size on
    -- its own can push a node's far edge a pixel past the snapped origin of
    -- the sibling that starts there, and the node then paints over it (a
    -- table cell over the column rule beside it).
    writeGeom a i GeomX (onGrid s x)
    writeGeom a i GeomY (onGrid s y)
    writeGeom a i GeomW (max 0 (gridSpan s x (x + w)))
    writeGeom a i GeomH (max 0 (gridSpan s y (y + h)))

measurePass :: SolveEnv -> Int -> IO ()
measurePass env count = case seCache env of
  Nothing -> forDown $ \idx -> measureNode env idx >> () <$ recordMeasured env idx
  Just lc -> do
    -- Per node, 1 once some child came out a different size than the
    -- capture recorded for it, so the node cannot take its captured size.
    moved <- newPrimArray count :: IO (IOArr Word8)
    setPrimArray moved 0 count 0
    forDown $ \idx -> do
      restored <- restoreMeasured env lc moved idx
      unless restored (measureNode env idx)
      (w, h) <- recordMeasured env idx
      same <-
        if restored || idx >= lcCount lc
          then pure restored
          else do
            cw <- readPrimArray (lcMeasured lc) (idx * 2)
            ch <- readPrimArray (lcMeasured lc) (idx * 2 + 1)
            pure (cw == w && ch == h)
      unless same $ do
        p <- readTree (seArrays env) idx TreeParent
        when (p >= 0) $ writePrimArray moved p 1
  where
    -- Children follow their parent in the arena, so a descending walk
    -- measures every child before its parent.
    forDown f = let go !idx = when (idx >= 0) (f idx >> go (idx - 1)) in go (count - 1)

-- | Put back the measured size the captured solve recorded for a node, when
-- that solve measured it the same way: its restore key
-- ('computeSubtreeHashes', over its own, its ancestors' and its descendants'
-- inputs) matches the capture's, and no child came out a different size.
-- 'False' when the node must be measured. Drawing nodes always measure: a
-- custom measure may read state outside the arena, and losing the hook
-- changes what the size means. Scroll containers always measure too, cheaply
-- from their children: the capture holds their content extent as the position
-- pass left it, not as measured.
restoreMeasured :: SolveEnv -> LayoutCache -> IOArr Word8 -> NodeIdx -> IO Bool
restoreMeasured env lc moved idx
  | idx >= lcCount lc = pure False
  | otherwise = do
      hit <- (==) <$> readPrimArray (seSub env) idx <*> readPrimArray (lcSub lc) idx
      childMoved <- (/= 0) <$> readPrimArray moved idx
      nt <- readTagEnum (seArrays env) idx TagNodeType
      if not hit || childMoved || nt == NodeDrawing || isScrollNode nt
        then pure False
        else do
          w <- readPrimArray (lcMeasured lc) (idx * 2)
          h <- readPrimArray (lcMeasured lc) (idx * 2 + 1)
          setRect (seArena env) idx 0 0 w h
          pure True

-- | Record the node's measured size for the next capture, whether it was
-- just measured or restored, and return it.
recordMeasured :: SolveEnv -> NodeIdx -> IO (Float, Float)
recordMeasured env idx = do
  Rect _ _ w h <- getNodeRect (seArena env) idx
  let ma = seMeasured env
  writePrimArray ma (idx * 2) w
  writePrimArray ma (idx * 2 + 1) h
  pure (w, h)

measureNode :: SolveEnv -> NodeIdx -> IO ()
measureNode env idx = measureContent env idx >> measureAspect (seArrays env) idx

-- | Hold a node that keeps an aspect ratio ('StyleAspect') to it, once its
-- content is measured: a fit height is its width over the ratio, and a fit
-- width beside a fixed height is the height times it, each within its
-- limits.
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
  case nt of
    NodeText -> measureTextNode env idx
    NodeSpacer -> measureSpacer na idx
    NodeSeparator -> measureSeparator na idx
    NodeScrollContainer -> measureScrollContainer env idx
    NodeImage -> getImageNode na idx >>= measureImage na idx . fmap (\n -> (inWidth n, inHeight n))
    NodeBox -> measureImage na idx Nothing
    NodeDrawing -> do
      wid <- getWidgetId na idx
      mFn <- msLookupMeasure (seMs env) wid
      case mFn of
        Just fn -> measureCustomNode env fn idx
        Nothing -> measureImage na idx Nothing
    _
      | isContainerNode nt -> do
          measureContainer env idx
          when (nt == NodeModal) $ setNodeValue na idx 0
      | otherwise -> measureWidget env idx

measureCustomNode :: SolveEnv -> CustomMeasureFn -> NodeIdx -> IO ()
measureCustomNode env@SolveEnv {seArena = na} measureFn idx = do
  wAx <- getWidthSizing na idx
  hAx <- getHeightSizing na idx
  let ((mw, mh), record) = customMeasure (msFm (seMs env)) measureFn wAx hAx
  forM_ (seMeasureLog env) $ \ref -> modifyIORef' ref (IM.insert idx record)
  setRect na idx 0 0 (fixedOr wAx mw) (fixedOr hAx mh)

-- | Run a node's custom measure again, as the solve ran it, and return the
-- record the layout cache keeps of the call. Layout-reuse validation compares
-- it with the solve's.
runCustomMeasure :: NodeArena -> FontMetrics -> CustomMeasureFn -> NodeIdx -> IO CustomMeasureRecord
runCustomMeasure na fm measureFn idx = do
  wAx <- getWidthSizing na idx
  hAx <- getHeightSizing na idx
  pure $! snd (customMeasure fm measureFn wAx hAx)

-- | A custom measure run on the space it is offered ('offeredExtent').
-- Returns the size it asks for and the record of the call, forced with the
-- pair so the cache holds words rather than the closure.
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
-- else its maximum when that is finite (under 1e8), and 1e9 otherwise.
{-# INLINE offeredExtent #-}
offeredExtent :: AxisSizing -> Float
offeredExtent (AxisSizing tag val _ m)
  | tag == SizingFixed = val
  | otherwise = if m < 1e8 then m else 1e9

-- | The axis's fixed size, or else @content@ within its limits.
{-# INLINE fixedOr #-}
fixedOr :: AxisSizing -> Float -> Float
fixedOr (AxisSizing tag val lo hi) content = if tag == SizingFixed then val else clamp lo hi content

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
    Just measure -> pure (clamp (axMin hAx) (axMax hAx) (snd (measure (msFm ms) (w, offeredExtent hAx))))
    Nothing -> pure fallback

-- | The width a text node that is not a row's child wraps at, from its
-- effective max width, width sizing and assigned width: 1e8 or more when
-- nothing caps it (@textNodeSpanEntry@ in the text-span collector).
textWrapCap :: Float -> SizingTag -> Float -> Float
textWrapCap effMaxW wTag w
  | effMaxW < 1e8 = max 0 effMaxW
  | wTag == SizingGrow && w > 0 = w
  | otherwise = effMaxW

-- | Find the nearest ancestor's fixed or finite maximum width, subtracting
-- accumulated horizontal padding. Returns 1e9 when none constrains the node.
findAncestorMaxW :: NodeArena -> NodeIdx -> IO Float
findAncestorMaxW na idx = go idx 0
  where
    go cur !padAccum = do
      p <- getParent na cur
      if p < 0
        then pure 1e9
        else do
          pad <- getPadding na p
          let padAccum' = padAccum + padL pad + padR pad
          AxisSizing pwTag pwVal _ pMaxW <- getWidthSizing na p
          if pwTag == SizingFixed
            then pure (max 0 (pwVal - padAccum'))
            else if pMaxW < 1e8
              then pure (max 0 (pMaxW - padAccum'))
              else go p padAccum'

measureTextNode :: SolveEnv -> NodeIdx -> IO ()
measureTextNode env@SolveEnv {seArena = na} idx = do
  AxisSizing wTag _ minW maxW <- getWidthSizing na idx
  hAx <- getHeightSizing na idx
  parentAssigns <- growParent na idx
  txt <- getText na idx
  isRowChild <- parentIsRow na idx
  effMaxW <-
    if maxW < 1e8
      then pure maxW
      else findAncestorMaxW na idx
  let canWrap = not isRowChild && effMaxW < 1e8
  TextBox {tbW = tw, tbH = th, tbLineH = lineH} <-
    measureTextNodeAt env idx txt effMaxW (\_ lineW -> canWrap && effMaxW + 0.5 < lineW)
  let reportedW = clamp minW maxW (if wTag == SizingGrow && parentAssigns then 0 else tw)
  setRect na idx 0 0 reportedW (sizeWithin hAx (max lineH th))

-- | Whether a grow-width node's width is assigned from above rather than
-- reported: its parent grows, and the nearest ancestor that does not grow is
-- not a modal. A modal takes its width from what it holds, so a grow label
-- inside one still reports its natural width; otherwise the modal could never
-- widen for it and the label would wrap into more lines than the modal
-- measured. Windows keep their own width and truncate long lines instead.
growParent :: NodeArena -> NodeIdx -> IO Bool
growParent na idx = getParent na idx >>= go True
  where
    go isParent p
      | p < 0 = pure (not isParent)
      | otherwise = do
          pwTag <- axTag <$> getWidthSizing na p
          if pwTag == SizingGrow
            then getParent na p >>= go False
            else if isParent
              then pure False
              else (/= NodeModal) <$> getNodeType na p

-- | Measure a node with no content to measure: an image, a box, or a
-- drawing without a measure of its own. Without a fixed size it takes its
-- @natural@ size, an image's own, within its limits, or else its minimum,
-- or 32 without one.
measureImage :: NodeArena -> NodeIdx -> Maybe (Float, Float) -> IO ()
measureImage na idx natural = do
  wAx <- getWidthSizing na idx
  hAx <- getHeightSizing na idx
  let orMin ax = if axMin ax > 0 then axMin ax else 32
      (nw, nh) = fromMaybe (orMin wAx, orMin hAx) natural
  setRect na idx 0 0 (sizeWithin wAx nw) (sizeWithin hAx nh)

measureSpacer :: NodeArena -> NodeIdx -> IO ()
measureSpacer na idx = do
  AxisSizing wTag wVal _ _ <- getWidthSizing na idx
  AxisSizing hTag hVal _ _ <- getHeightSizing na idx
  -- Non-fixed spacers reserve the default 8px extent.
  let w = if wTag == SizingFixed then wVal else 8
      h = if hTag == SizingFixed then hVal else 8
  setRect na idx 0 0 w h

measureSeparator :: NodeArena -> NodeIdx -> IO ()
measureSeparator na idx = do
  dir <- getDirection na idx
  case dir of
    DirRow -> setRect na idx 0 0 1 20
    DirColumn -> setRect na idx 0 0 20 1

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
      NodeSlider -> pure (60, max sliderHandleDiameter (sliderTrackHeight + 2 * sliderHandleSlack))
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
      _
        | nt == NodeButton && hasFlag buttonFlagChoice si ->
            measureMarkedWidget fm measure txt (checkboxLeading fm) (0, 0)
        | nt == NodeButton && hasFlag buttonFlagRow si -> do
            let (depth, _, _) = treeDecodeStyle si
            measureMarkedWidget fm measure txt (treeRowLeading fm depth) (treeItemPadding fm)
        | otherwise -> do
            let body
                  | T.null txt = " "
                  | hasFlag buttonFlagTable si = tableHeaderDisplayText txt
                  | otherwise = txt
                (padX, padY)
                  | nt /= NodeButton = widgetPadding fm
                  | hasFlag buttonFlagTable si = (2 * tableCellInset, 0)
                  -- Menu rows reserve the same gutter the text-field context
                  -- menu paints (outer pad + item pad on each side of the
                  -- label), so the generic popup panel sizes identically.
                  | hasFlag buttonFlagMenu si = (2 * (menuOuterPad + menuItemPadX), snd (buttonPadding fm))
                  | otherwise = buttonPadding fm
            (mw, mh) <- measure body
            pure (mw + padX, mh + padY)
  -- A widget's children are its adornments ('measureAdorned').
  kids <- readTree a idx TreeFirstChild
  (w, h) <- if kids < 0 then pure (rawW, rawH) else measureAdorned env measurer idx nt si txt rawW rawH
  setRect na idx 0 0 (fixedOr wAx w) (fixedOr hAx h)

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
    _ -> do
      (labelW, groupW) <- adornedButtonGroup measurer txt gap rows
      let (padX, padY) = buttonPadding fm
          sidePad = if labelW > 0 || hasFlag buttonFlagContent si then padX else padY
      pure (groupW + sidePad, max rawH (rowH + padY))

-- | A button's label width, in the font paint draws it in (0 without a
-- label), and the width of the group the label and its adornment rows make,
-- the node's gap apart.
adornedButtonGroup :: TextMeasurer -> Text -> Float -> AdornRows -> IO (Float, Float)
adornedButtonGroup measurer txt gap (AdornRows li lw ti tw _) = do
  labelW <- if T.null txt then pure 0 else fst <$> measureFontLine measurer txt
  let pieces = fromEnum (li >= 0) + fromEnum (labelW > 0) + fromEnum (ti >= 0)
  pure (labelW, lw + labelW + tw + gap * fromIntegral (max 0 (pieces - 1)))

-- | Place a widget's adornment rows ('adornRows'), each centred on its
-- height: a field's at its content insets, a button's either side of its
-- label in its centred group ('adornedButtonGroup').
positionAdornments :: SolveEnv -> Int -> NodeIdx -> NodeType -> Rect -> IO ()
positionAdornments env@SolveEnv {seArena = na, seArrays = a} depth idx nt (Rect x y w h) = do
  rows@(AdornRows li lw ti tw _) <- adornRows na idx
  measurer <- textNodeMeasurer env idx
  (x0, x1) <- case nt of
    NodeTextInput -> do
      let (ix, _) = widgetContentInset (tmMetrics measurer)
      pure (x + ix, x + w - ix)
    _ -> do
      gap <- readStyle a idx StyleGap
      txt <- getText na idx
      (_, groupW) <- adornedButtonGroup measurer txt gap rows
      let x0 = alignX AlignCenter x w groupW
      pure (x0, x0 + groupW)
  let place ci cx cw = when (ci >= 0) $ do
        ch <- readGeom a ci GeomH
        positionNodeA env (depth + 1) ci (Rect cx (y + (h - ch) / 2) cw ch)
  place li x0 lw
  place ti (x1 - tw) tw

measureContainer :: SolveEnv -> NodeIdx -> IO ()
measureContainer env@SolveEnv {seArena = na, seArrays = a} idx = do
  (pad, gap, dir) <- containerFlow a idx
  gCols <- readTree a idx TreeGridCols
  minColW <- readStyle a idx StyleGridMinColW
  flow <- readTagEnum a idx TagFlow
  wAx@(AxisSizing wTag _ minW _) <- readAxisSizing a idx True
  hAx <- readAxisSizing a idx False
  nt <- readTagEnum a idx TagNodeType
  let chrome = isChromeColumn nt dir
      padX = padL pad + padR pad
      padY = padT pad + padB pad
      -- The room inside the fixed size, or else inside the maximum.
      inner (AxisSizing tag val _ hi) p = max 0 ((if tag == SizingFixed then val else hi) - p)
      innerMaxW = inner wAx padX
      innerAvailH = inner hAx padY
  (contentW, contentH) <-
    if gCols > 0 || minColW > 0
      then measureGridScratch env idx gCols minColW innerMaxW innerAvailH gap
      else case flow of
        -- Layered children make a box as large as the largest on each axis.
        Layered -> do
          let step (FlowAcc count maxW maxH) ci = do
                Rect _ _ w h <- getNodeRect na ci
                pure (FlowAcc (count + 1) (max maxW w) (max maxH h))
          FlowAcc _ layersW layersH <- foldFlowChildrenM na idx step (FlowAcc 0 0 0)
          pure (layersW, layersH)
        Wrap -> measureWrap env idx dir gap (if dir == DirRow then innerMaxW else innerAvailH)
        Line
          | dir == DirColumn && chrome -> do
              n <- loadChildrenScratch na idx (flowChildSize env False innerMaxW innerAvailH)
              foldChromeColumnScratch na n gap
          | otherwise -> foldChildDimsFromParent env idx dir gap
  -- A grow container with its own minimum width, whose width is assigned from
  -- above, reports that minimum rather than its content: it shrinks that far
  -- in a row that is short of space, so that is the least it needs. As with
  -- CSS's min-width on a flex item, the explicit minimum replaces the
  -- content-based one. Otherwise a 2D scroller, which lays its content out at
  -- the width it reports, scrolls sideways for a long label in a cell that
  -- would have fit. Without a minimum the content still counts, so a grow
  -- wrapper around a wide table keeps its sideways scroll.
  minAssigned <-
    if wTag == SizingGrow && minW > 0 && not (isFloatingNode nt)
      then growParent na idx
      else pure False
  let h = sizeWithin hAx (contentH + padY)
  -- A window or modal that fits its width to its content leaves room for its
  -- body's scrollbar when the body will scroll, or the bar's gutter narrows
  -- the content below its measured width and clips its right edge.
  bodyGutter <-
    if chrome && wTag /= SizingFixed
      then floatingBodyGutter na idx (contentH + padY - h)
      else pure 0
  setRect na idx 0 0 (sizeWithin wAx (if minAssigned then 0 else contentW + padX + bodyGutter)) h

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
  wAx@(AxisSizing wTag _ _ _) <- getWidthSizing na idx
  hAx@(AxisSizing hTag hVal _ _) <- getHeightSizing na idx
  (contentW, contentH) <- foldChildDimsFromParent env idx dir gap
  parent <- getParent na idx
  -- A modal's body scrolls like a window's: its bar sits just inside the
  -- panel's edge, out in the panel padding.
  isWin <-
    if parent < 0 then pure False else (`elem` [NodeWindow, NodeModal]) <$> getNodeType na parent
  inPanel <- hasPanelAncestor na parent
  let slot = classifyScrollBar isWin (wTag == SizingGrow && hTag == SizingGrow && not inPanel)
  writeTagEnum a idx TagScrollBarSlot slot
  let assignedInnerH =
        case hTag of
          SizingFixed -> max 0 (hVal - padY)
          _ -> contentH
      cfg = decodeScrollConfig si
      fitGutterW
        | wTag == SizingGrow || wTag == SizingFixed = 0
        | isScrollStyle2D si = 0
        | otherwise =
            case dir of
              DirColumn -> scrollAxisGutter (scrollPolicyY cfg) slot (padR pad) contentH assignedInnerH
              DirRow -> 0
  if isScrollStyle2D si
    then do
      setNodeValue na idx contentH
      setScrollContentW na idx contentW
    else setNodeValue na idx (case dir of DirColumn -> contentH; DirRow -> contentW)
  setRect na idx 0 0 (sizeWithin wAx (contentW + padX + fitGutterW)) (sizeWithin hAx (contentH + padY))

foldChildDimsFromParent :: SolveEnv -> NodeIdx -> DirTag -> Float -> IO (Float, Float)
foldChildDimsFromParent env@SolveEnv {seArena = na} idx dir gap = do
  -- Along the axis the children's sizes add up, and across it the largest counts.
  FlowAcc count along across <- foldFlowChildrenM na idx step (FlowAcc 0 0 0)
  let gaps = gap * fromIntegral (max 0 (count - 1))
  case dir of
    DirColumn -> pure (if count <= 0 then 0 else across, along + gaps)
    DirRow -> do
      -- A row's baseline-aligned children stand on one line, so together they
      -- are as tall as the most room any takes above it plus the most any takes
      -- below.
      FlowAcc _ above below <- foldFlowChildrenM na idx baselineStep (FlowAcc 0 0 0)
      pure (along + gaps, if count <= 0 then 0 else max across (above + below))
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

-- | The content size of wrapping container @idx@ ('Wrap') whose main
-- axis, along @dir@, holds @bound@ at most: its children in lines of that
-- length ('wrapExtent'). A row that nothing of its own bounds (1e8 or more)
-- wraps where its nearest bounded ancestor leaves it room, as text does
-- ('findAncestorMaxW'), and is one line when nothing does. A grow row whose
-- width is assigned from above ('growParent') then asks only for the width of
-- its widest child, and that one line's height, and wraps at whatever width
-- it is given: a column refits its height there ('recomputeFitHeightAtWidth'),
-- and anywhere else it grows to its lines once they are placed
-- ('adjustFitHeight').
measureWrap :: SolveEnv -> NodeIdx -> DirTag -> Float -> Float -> IO (Float, Float)
measureWrap env@SolveEnv {seArena = na} idx dir gap bound = do
  kids <- mapM (\ci -> (\(Rect _ _ w h) -> (ci, (w, h))) <$> getNodeRect na ci) =<< flowChildrenInOrder na idx
  limit <-
    if dir == DirRow && bound >= 1e8
      then do
        pad <- getPadding na idx
        (\w -> w - padL pad - padR pad) <$> findAncestorMaxW na idx
      else pure bound
  (along, across) <- wrapExtent env idx dir gap limit kids
  wTag <- axTag <$> getWidthSizing na idx
  assigned <-
    if dir == DirRow && limit >= 1e8 && wTag == SizingGrow then growParent na idx else pure False
  let widest = foldl' (\m (_, (w, _)) -> max m w) 0 kids
  pure (if dir == DirRow then (if assigned then widest else along, across) else (across, along))

-- | How far the children @kids@ of wrapping container @idx@, each with its
-- width and height, reach in lines no longer than @limit@ along @dir@
-- ('wrapLines'): the longest line's length, and the lines' sizes across the
-- axis ('wrapLineCross') with the container's line gap between them.
wrapExtent :: SolveEnv -> NodeIdx -> DirTag -> Float -> Float -> [(NodeIdx, (Float, Float))] -> IO (Float, Float)
wrapExtent env idx dir gap limit kids = do
  lineGap <- readStyle (seArrays env) idx StyleLineGap
  let along (_, (w, h)) = if dir == DirRow then w else h
      lns = wrapLines limit gap along kids
      lineLength l = sum (map along l) + gap * fromIntegral (length l - 1)
  crosses <- mapM (wrapLineCross env dir) lns
  pure (foldl' max 0 (map lineLength lns), sum crosses + lineGap * fromIntegral (max 0 (length lns - 1)))

-- | Break items into wrap lines no longer than @limit@, where @len@ is an
-- item's length along the line and @gap@ separates neighbours: a line takes
-- items while they fit, and an item longer than the limit takes a line to
-- itself.
wrapLines :: Float -> Float -> (a -> Float) -> [a] -> [[a]]
wrapLines limit gap len = go
  where
    go [] = []
    go (x : xs) = let (line, rest) = fill (len x) xs in (x : line) : go rest
    fill !used (y : ys)
      | used + gap + len y <= limit + 1.0e-3 =
          let (line, rest) = fill (used + gap + len y) ys in (y : line, rest)
    fill _ ys = ([], ys)

-- | A wrap line's size across its axis: that of its largest child, or in a
-- row, when it is more, the most room any of the children aligned on the
-- line's baseline take above it plus the most any take below it, as in a row
-- ('foldChildDimsFromParent').
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

isChromeColumn :: NodeType -> DirTag -> Bool
isChromeColumn nt dir =
  dir == DirColumn && (nt == NodeWindow || nt == NodeModal)

-- | Gap before child @b@ in a column; chrome columns drop it before separators.
pairColumnGap :: NodeArena -> Bool -> NodeIdx -> Float -> IO Float
pairColumnGap _ False _ gap = pure gap
pairColumnGap na True b gap = do
  ntB <- getNodeType na b
  pure (if ntB == NodeSeparator then 0 else gap)

foldChromeColumnScratch :: NodeArena -> Int -> Float -> IO (Float, Float)
foldChromeColumnScratch na n gap = do
  FlexScratch {fsW = wArr, fsH = hArr} <- readIORef (naScratch na)
  gapSum <- columnGapSumScratch na n gap
  maxW <- foldUpTo n (\m i -> max m <$> readPrimArray wArr i) 0
  totalH <- foldUpTo n (\t i -> (t +) <$> readPrimArray hArr i) 0
  pure (maxW, totalH + gapSum)

-- | Grid column count: explicit, else as many @minColW@ columns as fit in a
-- positive @availW@, else one.
{-# INLINE gridColumnCount #-}
gridColumnCount :: Int -> Float -> Float -> Float -> Int
gridColumnCount gCols minColW availW gap
  | gCols > 0 = gCols
  | minColW > 0 && availW > 0 = max 1 (floor ((availW + gap) / (minColW + gap)))
  | otherwise = 1

-- | Height of grid row @r@: its tallest child.
gridRowHeight :: IOArr Float -> Int -> Int -> Int -> IO Float
gridRowHeight hArr n cols r =
  foldUpTo (min cols (n - r * cols)) (\m j -> max m <$> readPrimArray hArr (r * cols + j)) 0

measureGridScratch ::
  SolveEnv ->
  NodeIdx ->
  Int ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO (Float, Float)
measureGridScratch env idx gCols minColW innerMaxW innerAvailH gap = do
  n <- loadChildrenScratch (seArena env) idx (flowChildSize env False innerMaxW innerAvailH)
  if n <= 0
    then pure (0, 0)
    else do
      FlexScratch {fsW = wArr, fsH = hArr} <- readIORef (naScratch (seArena env))
      let cols = gridColumnCount gCols minColW (if innerMaxW < 1e8 then innerMaxW else 0) gap
          numRows = (n + cols - 1) `quot` cols
      totalH <- foldUpTo numRows (\t r -> (t +) <$> gridRowHeight hArr n cols r) 0
      let contentH = totalH + gap * fromIntegral (max 0 (numRows - 1))
      contentW <-
        if innerMaxW > 0 && innerMaxW < 1e8
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
recomputeFitHeightAtWidth env@SolveEnv {seArena = na, seArrays = a} idx availW =
  fmap snd . memoizeWidth na (naFitMemo na) idx availW . fmap ((,) 0) $ do
    nt <- getNodeType na idx
    wAx@(AxisSizing wTag _ minW maxW) <- getWidthSizing na idx
    hAx@(AxisSizing hTag _ minH maxH) <- getHeightSizing na idx
    Rect _ _ _ oldH <- getNodeRect na idx
    let -- The width a node takes of @avail@: fixed, a percentage, or all of it.
        widthOf (AxisSizing tag val _ _) avail = case tag of
          SizingPercent -> avail * val / 100
          SizingFixed -> val
          _ -> avail
        effW' = clamp minW maxW (widthOf wAx availW)
    ratio <- if hTag == SizingFit then readStyle a idx StyleAspect else pure 0
    case nt of
      -- A fit height that keeps an aspect ratio is the width over it.
      _ | ratio > 0 -> pure (clamp minH maxH (effW' / ratio))

      NodeText
        | hTag /= SizingFixed -> do
            isRowChild <- parentIsRow na idx
            textHeightAt env idx hAx effW' (wTag /= SizingFit && not isRowChild) oldH
        | otherwise -> pure oldH

      -- A measured drawing, like wrapped text, can be taller when narrower.
      NodeDrawing
        | hTag == SizingFit -> drawingHeightAt env idx effW' hAx oldH
        | otherwise -> pure oldH

      _ | (nt == NodeContainer || nt == NodePanel), hTag /= SizingFixed -> do
            (pad, gap, dir) <- containerFlow a idx
            gCols <- readTree a idx TreeGridCols
            minColW <- readStyle a idx StyleGridMinColW
            let innerW = max 0 (effW' - padL pad - padR pad)
                -- A child's height when @w@ is offered to it, kept to its max width.
                childH w ci = do
                  subAx <- getWidthSizing na ci
                  let subW = widthOf subAx w
                  recomputeFitHeightAtWidth env ci (if axMax subAx < 1e8 then min subW (axMax subAx) else subW)
                padded h = pure (clamp minH maxH (h + padT pad + padB pad))
            if gCols > 0 || minColW > 0
              then do
                -- A grid stacks rows, each as tall as its tallest cell at the
                -- column width, not every cell.
                let cols = gridColumnCount gCols minColW innerW gap
                    colW = max 0 ((innerW - gap * fromIntegral (cols - 1)) / fromIntegral cols)
                    rows [] = []
                    rows hs = let (r, rest) = splitAt cols hs in r : rows rest
                hs <- mapM (childH colW) =<< flowChildrenInOrder na idx
                let rowHs = map (foldl' max 0) (rows hs)
                padded (if null rowHs then 0 else sum rowHs + gap * fromIntegral (length rowHs - 1))
              else do
                flow <- readTagEnum a idx TagFlow
                case flow of
                  -- Layers are as tall as the tallest at the width.
                  Layered -> padded . foldl' max 0 =<< mapM (childH innerW) =<< flowChildrenInOrder na idx
                  -- A wrapping row takes as many lines as the width breaks
                  -- it into, its children at their widths, a percentage
                  -- resolved against it. A wrapping column's height does
                  -- not follow from its width.
                  Wrap
                    | dir == DirRow -> do
                        let sized ci = do
                              subAx <- getWidthSizing na ci
                              Rect _ _ w h <- getNodeRect na ci
                              pure (ci, (percentOr subAx innerW w, h))
                        kids <- mapM sized =<< flowChildrenInOrder na idx
                        padded . snd =<< wrapExtent env idx dir gap innerW kids
                    | otherwise -> pure oldH
                  Line
                    | dir == DirRow -> pure oldH
                    | otherwise -> do
                        let step (FlowAcc count contentH _) ci = do
                              subH <- childH innerW ci
                              pure (FlowAcc (count + 1) (contentH + subH) 0)
                        FlowAcc count contentH _ <- foldFlowChildrenM na idx step (FlowAcc 0 0 0)
                        padded (if count <= 0 then 0 else contentH + gap * fromIntegral (count - 1))

      _ -> pure oldH

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
{-# INLINE percentOr #-}
percentOr :: AxisSizing -> Float -> Float -> Float
percentOr (AxisSizing tag val lo hi) avail size = if tag == SizingPercent then clamp lo hi (avail * val / 100) else size

positionNodeA ::
  SolveEnv ->
  Int ->
  NodeIdx ->
  Rect ->
  IO ()
positionNodeA env@SolveEnv {seArena = na, seArrays = a} !depth !idx (Rect x y availW availH) = do
  wAx <- readAxisSizing a idx True
  hAx@(AxisSizing hTag _ minH maxH) <- readAxisSizing a idx False
  intrinsicW <- readGeom a idx GeomW
  intrinsicH <- readGeom a idx GeomH
  nt <- readTagEnum a idx TagNodeType
  let !w = resolveSize wAx intrinsicW availW
      !resolvedH = resolveSize hAx intrinsicH availH
  ratio <- if hTag == SizingFit then readStyle a idx StyleAspect else pure 0
  h <- case nt of
    -- A fit height that keeps an aspect ratio follows the width it got.
    _ | ratio > 0 -> pure (clamp minH maxH (w / ratio))
    -- Text outside a row wraps at the width it got.
    NodeText | hTag /= SizingFixed -> do
      isRowChild <- parentIsRow na idx
      if isRowChild then pure resolvedH else textHeightAt env idx hAx w (axTag wAx /= SizingFit) resolvedH
    _ | (nt == NodeContainer || nt == NodePanel) && hTag == SizingFit -> pure (clamp minH maxH (max intrinsicH availH))
    -- A measured drawing laid out at another width than it was measured at
    -- takes its height at the width it got.
    NodeDrawing | hTag == SizingFit && w /= intrinsicW -> drawingHeightAt env idx w hAx resolvedH
    _ -> pure resolvedH
  setRect na idx x y w h
  if isContainerNode nt
    then do
      (pad, gap, dir) <- containerFlow a idx
      if isScrollNode nt
        then positionScrollChildren env depth idx dir gap pad (Rect x y w h)
        else positionChildren env depth idx dir gap pad (Rect x y w h)
    else do
      kids <- readTree a idx TreeFirstChild
      when (kids >= 0) $ positionAdornments env depth idx nt (Rect x y w h)
  when (hTag == SizingFit && ratio <= 0 && isContainerNode nt && not (isScrollNode nt)) $
    adjustFitHeight na idx minH maxH x y w

-- | A container's resolved padding and gap, and its direction.
{-# INLINE containerFlow #-}
containerFlow :: NodeArenaArrays -> NodeIdx -> IO (Padding, Float, DirTag)
containerFlow a idx = do
  pad <- Padding <$> readStyle a idx StylePadL <*> readStyle a idx StylePadR <*> readStyle a idx StylePadT <*> readStyle a idx StylePadB
  gap <- readStyle a idx StyleGap
  dir <- readTagEnum a idx TagDirection
  pure (pad, gap, dir)

adjustFitHeight :: NodeArena -> NodeIdx -> Float -> Float -> Float -> Float -> Float -> IO ()
adjustFitHeight na idx minH maxH x y w = do
  fc <- getFirstChild na idx
  when (fc >= 0) $ do
    pad <- getPadding na idx
    let step maxB ci = do
          Rect _ subY _ subH <- getNodeRect na ci
          pure (max maxB (subY + subH))
    maxB <- foldFlowChildrenM na idx step y
    let fitH = clamp minH maxH (maxB + padB pad - y)
    Rect _ _ _ curH <- getNodeRect na idx
    -- Children sit at raw positions, so only float error separates their
    -- bottom from the measured height; the epsilon keeps that from growing
    -- every nested content-sized level.
    when (fitH > curH + 1.0e-3) $
      setRect na idx x y w fitH

positionScrollChildren ::
  SolveEnv ->
  Int ->
  NodeIdx ->
  DirTag ->
  Float ->
  Padding ->
  Rect ->
  IO ()
positionScrollChildren env@SolveEnv {seArena = na} depth idx dir gap pad (Rect px py pw ph) = do
  si <- getStyleIdx na idx
  contentSize <- getNodeValue na idx
  slot <- scrollBarSlotOf na idx
  let cx = px + padL pad
      cy = py + padT pad
      innerW = pw - padL pad - padR pad
      innerH = ph - padT pad - padB pad
      cfg = decodeScrollConfig si
  if isScrollStyle2D si
    then do
      contentW <- getScrollContentW na idx
      let (gutterW, gutterH) = scrollGutters2D slot cfg pad contentW contentSize innerW innerH
          viewW = max 0 (innerW - gutterW)
          viewH = max 0 (innerH - gutterH)
          -- Keep measured content. Shrinking to the clip wraps table columns.
          layoutW = max contentW viewW
          layoutH = max contentSize viewH
      -- cx/cy and the layout box are already inside the padding.
      positionChildren env depth idx DirColumn gap (Padding 0 0 0 0) (Rect cx cy layoutW layoutH)
    else do
      let gutterCol = scrollAxisGutter (scrollPolicyY cfg) slot (padR pad) contentSize innerH
          gutterRow = scrollAxisGutter (scrollPolicyX cfg) slot (padB pad) contentSize innerW
      case dir of
        DirRow -> do
          wTag <- axTag <$> getWidthSizing na idx
          let rowMain =
                if wTag == SizingGrow
                  then max contentSize (innerW - gutterRow)
                  else contentSize
              box = Rect cx cy rowMain (innerH - gutterRow)
          positionRowFromParent env depth idx gap box
          positionLayered env depth idx False box
        DirColumn -> do
          let viewW = innerW - gutterCol
          positionColumn env depth idx gap False (Just contentSize) cx viewW (Rect cx cy viewW innerH)
          positionLayered env depth idx False (Rect cx cy viewW innerH)
  fc <- getFirstChild na idx
  when (fc >= 0) $ do
    let step (FlowAcc count maxB maxR) ci = do
          Rect subX subY subW subH <- getNodeRect na ci
          pure (FlowAcc (count + 1) (max maxB (subY + subH)) (max maxR (subX + subW)))
    -- Pinned children count too, so the scroller reaches them.
    FlowAcc _ maxB maxR <- foldPlacedChildrenM na idx step (FlowAcc 0 cy cx)
    -- Content size is measured from the content origin (px+padL, py+padT) so
    -- it compares against the padded viewport (innerW/innerH) on the same
    -- scale. Measuring from the padding-box origin double-counts the leading
    -- padding and makes a child that exactly fills the viewport look
    -- padX/padY bigger, surfacing a phantom scrollbar on padded scrollers.
    -- The trailing padding is excluded here too (so it cannot
    -- surface a bar by itself); scrollAxisRange adds it back into the
    -- reachable range once an axis genuinely overflows, so scrolling to the
    -- end still reveals it.
    let actualContentH = maxB - py - padT pad
        actualContentW = maxR - px - padL pad
        raise get set v = get na idx >>= \old -> set na idx (max old v)
    if isScrollStyle2D si
      then do
        raise getNodeValue setNodeValue actualContentH
        raise getScrollContentW setScrollContentW actualContentW
      else raise getNodeValue setNodeValue $
        case dir of DirColumn -> actualContentH; DirRow -> actualContentW

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

-- | Left edge of column child @ci@ in a column of width @cw@ at @cx@. Grow and
-- percent children already take the full width; alignment is for content
-- narrower than the column, not for shifting a full-width box past it, so a
-- child is aligned at the width it takes there: text measured on one line
-- wraps to the column.
{-# INLINE columnChildX #-}
columnChildX :: NodeArena -> NodeIdx -> Float -> Float -> IO Float
columnChildX na ci cx cw = do
  wAx <- getWidthSizing na ci
  if axTag wAx == SizingGrow || axTag wAx == SizingPercent
    then pure cx
    else do
      Rect _ _ iw _ <- getNodeRect na ci
      ax <- getAlignX na ci
      pure $! alignX ax cx cw (resolveSize wAx iw cw)

-- | The size a node takes along an axis where @avail@ is offered, from its
-- sizing, its measured size @intrinsic@ and its limits.
{-# INLINE resolveSize #-}
resolveSize :: AxisSizing -> Float -> Float -> Float
resolveSize (AxisSizing tag val lo hi) intrinsic avail =
  clamp lo hi $ case tag of
    SizingFixed -> val
    SizingGrow -> min avail hi
    SizingPercent -> min avail hi
    _ -> min intrinsic avail

positionChildren ::
  SolveEnv ->
  Int ->
  NodeIdx ->
  DirTag ->
  Float ->
  Padding ->
  Rect ->
  IO ()
positionChildren env@SolveEnv {seArrays = a} depth idx dir gap pad (Rect px py pw ph) = do
  nt <- readTagEnum a idx TagNodeType
  gCols <- readTree a idx TreeGridCols
  minColW <- readStyle a idx StyleGridMinColW
  flow <- readTagEnum a idx TagFlow
  let chrome = isChromeColumn nt dir
      cx = px + padL pad
      cy = py + padT pad
      cw = pw - padL pad - padR pad
      ch = ph - padT pad - padB pad
  if gCols > 0 || minColW > 0
    then positionGrid env depth idx gCols minColW gap (Rect cx cy cw ch)
    else case flow of
      -- Layered children are all placed with the pinned ones, below.
      Layered -> pure ()
      Wrap -> positionWrap env depth idx dir gap (Rect cx cy cw ch)
      Line -> case dir of
        DirRow -> positionRowFromParent env depth idx gap (Rect cx cy cw ch)
        DirColumn -> positionColumn env depth idx gap chrome Nothing px pw (Rect cx cy cw ch)
  positionLayered env depth idx (flow == Layered) (Rect cx cy cw ch)

childRowCrossSize :: NodeArena -> NodeIdx -> Float -> IO Float
childRowCrossSize na ci availCross = do
  hAx@(AxisSizing hTag _ minH _) <- getHeightSizing na ci
  Rect _ _ _ intrinsic <- getNodeRect na ci
  pure $
    if hTag == SizingFit || hTag == SizingShrink
      -- Fit/Shrink keep the measured box. Do not use the wrap-line
      -- or row slot as availH: that stretches every child when leftover
      -- leaks into scratch `fh`.
      then max minH intrinsic
      else resolveSize hAx intrinsic availCross

-- Column leftover must not change Fixed step height.
columnChildHeight :: NodeArena -> NodeIdx -> Float -> IO Float
columnChildHeight na ci scratchH = do
  AxisSizing hTag _ minH maxH <- getHeightSizing na ci
  case hTag of
    SizingFixed -> do
      Rect _ _ _ ih <- getNodeRect na ci
      pure (clamp minH maxH ih)
    _ -> pure (clamp minH maxH scratchH)

-- | Copy the first @n@ scratch child indices, and beside them the scratch
-- array @sizes@ picks, to the snapshot for nesting depth @depth@, and run
-- @act@ on the copies. Laying out a child reuses the scratch, so a container
-- walks its children from the snapshot.
{-# INLINE withSnapshot #-}
withSnapshot ::
  NodeArena -> Int -> Int -> (FlexScratch -> IOArr Float) ->
  (IOArr Int -> IOArr Float -> IO a) -> IO a
withSnapshot na depth n sizes act = do
  s <- readIORef (naScratch na)
  AxisSnapshot idxSnap sizeSnap <- ensureAxisSnapshot na depth n
  copyMutablePrimArray idxSnap 0 (fsIdx s) 0 n
  copyMutablePrimArray sizeSnap 0 (sizes s) 0 n
  act idxSnap sizeSnap

positionRowFromParent ::
  SolveEnv ->
  Int ->
  NodeIdx ->
  Float ->
  Rect ->
  IO ()
positionRowFromParent env@SolveEnv {seArena = na} depth parent gap (Rect cx cy cw ch) = do
  n <- loadChildrenScratch na parent (flowChildSize env False cw ch)
  distributeScratch na n cw (gap * fromIntegral (max 0 (n - 1))) True
  withSnapshot na depth n fsOut $ \idxSnap outSnap ->
    placeRowLine env depth idxSnap outSnap n gap cx cy ch

-- | Place the first @k@ children of a snapshot left to right from @x0@,
-- each as wide as its share in @outSnap@, in a line @cross@ tall at @top@.
-- Children aligned on the baseline share one, as low as the deepest among
-- them, so the child with the tallest ascent stays at the top.
placeRowLine :: SolveEnv -> Int -> IOArr Int -> IOArr Float -> Int -> Float -> Float -> Float -> Float -> IO ()
placeRowLine env@SolveEnv {seArena = na} !depth idxSnap outSnap !k !gap !x0 !top !cross = do
  let lowestBaseline acc i = do
        ci <- readPrimArray idxSnap i
        ay <- getAlignY na ci
        if ay /= AlignBaseline
          then pure acc
          else max acc <$> (childRowCrossSize na ci cross >>= childBaseline env ci)
  base <- foldUpTo k lowestBaseline 0
  -- The cursor stays in raw floats, never snapped: rounding it re-compounds
  -- error every child (1.667 -> 2.0 -> ...) so a shrink row overruns its
  -- fixed width. Each child's far edge is the next one's raw origin, so
  -- quantizeResultsA, which snaps edges, puts both on the same pixel.
  let go !i !x = when (i < k) $ do
          ci <- readPrimArray idxSnap i
          fw <- readPrimArray outSnap i
          -- Fit/fixed children keep content height. Only Grow/Percent eat `cross`.
          crossH <- childRowCrossSize na ci cross
          ay <- getAlignY na ci
          fy <-
            if ay == AlignBaseline
              then (\b -> top + base - b) <$> childBaseline env ci crossH
              else pure (alignY ay top cross crossH)
          positionNodeA env (depth + 1) ci (Rect x fy fw crossH)
          -- A grow child that its max width stopped short of its share
          -- hands the rest to the siblings after it instead of leaving a
          -- hole.
          placedW <- readGeom (seArrays env) ci GeomW
          go (i + 1) (x + min fw placedW + gap)
  go 0 x0

positionGrid ::
  SolveEnv ->
  Int ->
  NodeIdx ->
  Int ->
  Float ->
  Float ->
  Rect ->
  IO ()
positionGrid env@SolveEnv {seArena = na} depth parent gCols minColW gap (Rect cx cy cw ch) = do
  n <- loadChildrenScratch (seArena env) parent (flowChildSize env False cw ch)
  when (n > 0) $ do
    let cols = gridColumnCount gCols minColW cw gap
        colW = max 0 ((cw - gap * fromIntegral (cols - 1)) / fromIntegral cols)
        numRows = (n + cols - 1) `quot` cols
    -- Freeze child indices and their measured cross sizes before recursing.
    -- Children reuse the working scratch while this grid iterates rows and
    -- columns, so the live arrays would be clobbered by the first child.
    withSnapshot na depth n fsH $ \idxArr hArr -> do
      let goRows !r !curY = when (r < numRows) $ do
            rowH <- gridRowHeight hArr n cols r
            forUpTo_ (min cols (n - r * cols)) $ \j -> do
              ci <- readPrimArray idxArr (r * cols + j)
              wAx <- getWidthSizing na ci
              hAx <- getHeightSizing na ci
              Rect _ _ iw ih <- getNodeRect na ci
              let childW = resolveSize wAx iw colW
                  childH = resolveSize hAx ih rowH
                  itemX = cx + fromIntegral j * (colW + gap)
              ax <- getAlignX na ci
              ay <- getAlignY na ci
              let fx = alignX ax itemX colW childW
                  fy = alignY ay curY rowH childH
              positionNodeA env (depth + 1) ci (Rect fx fy colW rowH)
            goRows (r + 1) (curY + rowH + gap)
      goRows 0 cy

-- | Lay out a wrapping container's flow children in lines along @dir@ that
-- fit the content box @cx cy cw ch@ ('wrapLines'), each line after the one
-- before it with the line gap between. Within its line a child sits as in a
-- row or a column: the line's children share its spare or missing length
-- ('distributeScratch') and keep the gap apart, and a child takes its place
-- across the line by its alignment, or fills the line when it grows. A line
-- that leaves room along the axis moves along it by the container's line
-- alignment ('TagLineAlign'). A line is as thick as its thickest child
-- ('wrapLineCross'), or as whatever it placed grew to.
positionWrap :: SolveEnv -> Int -> NodeIdx -> DirTag -> Float -> Rect -> IO ()
positionWrap env@SolveEnv {seArena = na, seArrays = a} depth parent dir gap (Rect cx cy cw ch) = do
  lineGap <- readStyle a parent StyleLineGap
  align <- lineAlignFraction <$> readTagEnum a parent TagLineAlign
  let row = dir == DirRow
      limit = if row then cw else ch
      along (_, (w, h)) = if row then w else h
      gaps k = gap * fromIntegral (k - 1)
  kids <- mapM (\ci -> (ci,) <$> flowChildSize env False cw ch ci) =<< flowChildrenInOrder na parent
  let placeLine !lineAt line = do
        let k = length line
        FlexScratch {fsIdx = idxArr, fsW = wArr, fsH = hArr, fsOut = outArr} <- ensureScratchCapacity na k
        zipWithM_ (\j (ci, (w, h)) -> writePrimArray idxArr j ci >> writePrimArray wArr j w >> writePrimArray hArr j h) [0 ..] line
        distributeScratch na k limit (gaps k) row
        lineCross <- wrapLineCross env dir line
        -- The line starts its alignment's share of the room it leaves.
        shift <-
          if align <= 0
            then pure 0
            else (\used -> max 0 (limit - used) * align) <$> foldUpTo k (\t i -> (t +) <$> readPrimArray outArr i) (gaps k)
        withSnapshot na depth k fsOut $ \idxSnap outSnap ->
          if row
            then placeRowLine env depth idxSnap outSnap k gap (cx + shift) lineAt lineCross
            else placeColumnLine env depth idxSnap outSnap k gap False Nothing 0 0 (Rect lineAt (cy + shift) lineCross ch)
        -- The next line starts past the line, or past what it placed grew to.
        let reach m (ci, _) = do
              Rect x y w h <- getNodeRect na ci
              pure (max m (if row then y + h else x + w))
        (+ lineGap) <$> foldM reach (lineAt + lineCross) line
  foldM_ placeLine (if row then cy else cx) (wrapLines limit gap along kids)

-- | Place the children of node @idx@ drawn over the rest in its content box
-- @cx cy cw ch@: all of a layered container's (@allLayered@), each where its
-- alignment puts it, and pinned ones where their alignment puts them, moved
-- by their offsets, so an offset from the end edge anchors a node there. Each
-- keeps its own size: a child that grows fills the box, from a pin's offset
-- to the edge it is not aligned to, a percentage is of the box, and any other
-- keeps its measured size, among layers no wider than the box. A fit height
-- is taken again at the width the child gets.
positionLayered :: SolveEnv -> Int -> NodeIdx -> Bool -> Rect -> IO ()
positionLayered env@SolveEnv {seArena = na, seArrays = a} depth idx allLayered (Rect cx cy cw ch) = do
  pinnedBelow <- readTagEnum a idx TagPinnedBelow
  when (allLayered || pinnedBelow) $ forChildNodes_ na idx $ \ci -> do
    floating <- isFloatingNode <$> readTagEnum a ci TagNodeType
    pinned <- readTagEnum a ci TagPinned
    when ((allLayered || pinned) && not floating) $ do
      -- Only a pinned node has an offset.
      ox <- readStyle a ci StylePinX
      oy <- readStyle a ci StylePinY
      ax <- readTagEnum a ci TagAlignX
      ay <- readTagEnum a ci TagAlignY
      wAx <- readAxisSizing a ci True
      hAx@(AxisSizing hTag _ _ _) <- readAxisSizing a ci False
      iw <- readGeom a ci GeomW
      ih <- readGeom a ci GeomH
      let -- What the offset leaves a grow size of the box: past it at the
          -- start, short of it at the end, and centred between.
          room (AxisSizing tag val _ _) atStart atEnd box offset other = case tag of
            SizingGrow
              | atStart -> max 0 (box - offset)
              | atEnd -> max 0 (box + offset)
              | otherwise -> max 0 (box - 2 * abs offset)
            SizingPercent -> box * val / 100
            _ -> other
          w = resolveSize wAx iw (room wAx (ax == AlignStart) (ax == AlignEnd) cw ox (if pinned then iw else cw))
      fitH <-
        if (hTag == SizingFit || hTag == SizingShrink) && w /= iw
          then recomputeFitHeightAtWidth env ci w
          else pure ih
      let h = resolveSize hAx fitH (room hAx (ay /= AlignMiddle && ay /= AlignBottom) (ay == AlignBottom) ch oy fitH)
      positionNodeA env (depth + 1) ci (Rect (alignX ax cx cw w + ox) (alignY ay cy ch h + oy) w h)

-- | Place a column's flow children top to bottom in the box @cx cy cw ch@. A
-- scroll column shares out its content height (@scrollContent@) and gives a
-- scroll child no more than the part of the viewport below its top; any other
-- column shares out @ch@. In a window's or modal's column (@chrome@) a
-- separator spans the padding box (@px@, @pw@) and takes no gap before it.
positionColumn ::
  SolveEnv ->
  Int ->
  NodeIdx ->
  Float ->
  Bool ->
  Maybe Float ->
  Float ->
  Float ->
  Rect ->
  IO ()
positionColumn env@SolveEnv {seArena = na} !depth !parent !gap chrome scrollContent !px !pw box@(Rect _ _ cw ch) = do
  n <- loadChildrenScratch na parent (flowChildSize env True cw ch)
  -- The gaps come out of the height shared, as in a row, or grow children
  -- overflow the column by them.
  gapSum <-
    if chrome then columnGapSumScratch na n gap else pure (gap * fromIntegral (max 0 (n - 1)))
  distributeScratch na n (fromMaybe ch scrollContent) gapSum False
  withSnapshot na depth n fsOut $ \idxSnap outSnap ->
    placeColumnLine env depth idxSnap outSnap n gap chrome scrollContent px pw box

-- | Place the first @k@ children of a snapshot top to bottom in the box
-- @cx cy cw ch@, each as tall as its share in @outSnap@. The arguments after
-- @gap@ are as for 'positionColumn'.
placeColumnLine ::
  SolveEnv -> Int -> IOArr Int -> IOArr Float -> Int -> Float ->
  Bool -> Maybe Float -> Float -> Float -> Rect -> IO ()
placeColumnLine env@SolveEnv {seArena = na} !depth idxSnap outSnap !k !gap chrome scrollContent !px !pw (Rect cx cy cw ch) =
  go 0 cy
  where
    go !i !y = when (i < k) $ do
      ci <- readPrimArray idxSnap i
      fh <- readPrimArray outSnap i
      nt <- getNodeType na ci
      (fx, nodeW) <-
        if chrome && nt == NodeSeparator
          then pure (px, pw)
          else (,cw) <$> columnChildX na ci cx cw
      childH <- case scrollContent of
        Just _ -> pure (if isScrollNode nt then min fh (max 0 (ch - (y - cy))) else fh)
        Nothing -> columnChildHeight na ci fh
      positionNodeA env (depth + 1) ci (Rect fx y nodeW childH)
      Rect _ _ _ placedH <- getNodeRect na ci
      gapAfter <-
        if i + 1 >= k
          then pure 0
          else readPrimArray idxSnap (i + 1) >>= \b -> pairColumnGap na chrome b gap
      go (i + 1) (y + placedH + gapAfter)

-- | The gaps between the first @n@ scratch children of a window's or modal's
-- column, where a separator takes none before it.
columnGapSumScratch :: NodeArena -> Int -> Float -> IO Float
columnGapSumScratch na n gap = do
  FlexScratch {fsIdx = idxArr} <- readIORef (naScratch na)
  let addGap acc i = readPrimArray idxArr (i + 1) >>= \b -> (acc +) <$> pairColumnGap na True b gap
  foldUpTo (n - 1) addGap 0

-- | Share the main axis among the first @n@ scratch children: 'fsOut' gets
-- each child's size along it, starting from its content size ('fsW' or
-- 'fsH') and taking its part of the space the container has spare or lacks.
distributeScratch :: NodeArena -> Int -> Float -> Float -> Bool -> IO ()
distributeScratch na n avail gapSum horizontal = do
  FlexScratch {fsIdx = idxArr, fsW = wArr, fsH = hArr, fsOut = out, fsGrow = gfArr} <- readIORef (naScratch na)
  a <- arenaArrays na
  copyMutablePrimArray out 0 (if horizontal then wArr else hArr) 0 n
  total <- foldUpTo n (\acc i -> (acc +) <$> readPrimArray out i) 0
  let slack = avail - (total + gapSum)
      sizingAt i = readPrimArray idxArr i >>= \ci -> readAxisSizing a ci horizontal
  if slack > 0.001
    then do
      -- Grow children share the free space by factor, but no child is
      -- squeezed below its content size (a min-content floor, like CSS
      -- flex with min-width:auto): two fillW columns come out equal unless
      -- one column's content needs more, and that one then takes exactly
      -- what it needs while the rest re-share what is left.
      --
      -- 'fsGrow' holds each child's factor, 0 once it is not or no longer
      -- growing, and 'fsOut' keeps the exact content size until the shares
      -- are handed out.
      growTotal <- foldUpTo n (\acc i -> do
        AxisSizing tag val _ _ <- sizingAt i
        let gf = if tag == SizingGrow then val else 0
        writePrimArray gfArr i (if gf > 0 then gf else 0)
        pure (acc + gf)) 0
      when (growTotal > 0) $ do
        (free, gfSum) <- settleGrow out gfArr avail gapSum n 0
        forUpTo_ n $ \i -> do
          gf <- readPrimArray gfArr i
          when (gf > 0) $ writePrimArray out i (max 0 (free * gf / gfSum))
    else when (slack < -0.001) $ do
      -- Children give back the room the container lacks by factor, none
      -- below its minimum ('shrinkScratch'), with 'fsGrow' holding each
      -- child's factor.
      forUpTo_ n $ \i -> sizingAt i >>= writePrimArray gfArr i . shrinkFactor
      shrinkScratch out gfArr (fmap axMin . sizingAt) n (negate slack)

-- | Take @need@ from the first @n@ scratch children's sizes in @mainArr@ by
-- their factors in @factorArr@ (0 for a child that keeps its size), none
-- below its minimum @minAt@: a child its share would take below it stops
-- there, its factor cleared, and the others share what it could not give,
-- so a short row fits once its children do.
shrinkScratch :: IOArr Float -> IOArr Float -> (Int -> IO Float) -> Int -> Float -> IO ()
shrinkScratch mainArr factorArr minAt n !need = do
  total <- foldUpTo n (\acc i -> (acc +) <$> readPrimArray factorArr i) 0
  when (total > 0) $ do
    let stop (!given, !stopped) i = do
          f <- readPrimArray factorArr i
          if f <= 0
            then pure (given, stopped)
            else do
              lo <- minAt i
              main <- readPrimArray mainArr i
              if main - need * f / total < lo
                then do
                  writePrimArray mainArr i lo
                  writePrimArray factorArr i 0
                  pure (given + main - lo, stopped + 1)
                else pure (given, stopped)
    (given, stopped) <- foldUpTo n stop (0, 0 :: Int)
    if stopped > 0
      then shrinkScratch mainArr factorArr minAt n (need - given)
      else forUpTo_ n $ \i -> do
        f <- readPrimArray factorArr i
        when (f > 0) $ readPrimArray mainArr i >>= \main -> writePrimArray mainArr i (main - need * f / total)

{-# INLINE shrinkFactor #-}
shrinkFactor :: AxisSizing -> Float
shrinkFactor (AxisSizing tag val _ _) =
  case tag of
    SizingShrink -> val
    -- Grow also gives space back when the window is smaller than content.
    SizingGrow -> if val > 0 then val else 1
    -- Percent flexes like CSS: when siblings plus gaps overflow the axis,
    -- percent children give the overflow back so e.g. two 50% columns and a
    -- gap land exactly on the row width. Covers percent on either axis,
    -- should height percent ever be sized that way.
    SizingPercent -> 1
    -- Fit stays content-sized. A pinned header must not squash when a Grow
    -- sibling (page scroll) is taller than the window.
    _ -> 0

-- One sweep: sum content of non-grow + already-locked children (factor 0) and
-- grow factors of the still-unlocked.
{-# INLINE scanGrow #-}
scanGrow :: IOArr Float -> IOArr Float -> Int -> Int -> Float -> Float -> IO (Float, Float)
scanGrow mainArr crossArr !i !end !occupied !gfSum
  | i >= end = pure (occupied, gfSum)
  | otherwise = do
      gf <- readPrimArray crossArr i
      if gf > 0
        then scanGrow mainArr crossArr (i + 1) end occupied (gfSum + gf)
        else do
          main <- readPrimArray mainArr i
          scanGrow mainArr crossArr (i + 1) end (occupied + main) gfSum

-- Pin every grow child whose content exceeds its would-be share by clearing
-- its factor; its content stays in mainArr.
lockGrow :: IOArr Float -> IOArr Float -> Float -> Float -> Int -> IO Int
lockGrow mainArr crossArr !free !gfSum n = foldUpTo n lock 0
  where
    lock acc i = do
      gf <- readPrimArray crossArr i
      need <- readPrimArray mainArr i
      if gf > 0 && need * gfSum > gf * free
        then (acc + 1) <$ writePrimArray crossArr i 0
        else pure acc

-- Each lock shrinks the share pool, possibly locking more children; the
-- locked set only grows, so this fixpoints within n sweeps, and a row of n
-- children could take n. Rows settle in a few; one still locking after
-- 'waterFillAfter' sweeps is settled by 'waterFillGrow', which the next sweep
-- confirms, so no row takes more than a sort and that many sweeps.
settleGrow :: IOArr Float -> IOArr Float -> Float -> Float -> Int -> Int -> IO (Float, Float)
settleGrow mainArr crossArr avail gapSum n !pass = do
  (occupied, gfSum) <- scanGrow mainArr crossArr 0 n 0 0
  let free = avail - gapSum - occupied
  locked <- lockGrow mainArr crossArr free gfSum n
  if locked == 0 || pass >= n
    then pure (free, gfSum)
    else do
      when (pass + 1 == waterFillAfter) $ waterFillGrow mainArr crossArr (avail - gapSum) n
      settleGrow mainArr crossArr avail gapSum n (pass + 1)

-- | Sweeps 'settleGrow' makes before it sorts. Rows with varied content and
-- weights settle in three or four, and below this the sweeps cost less than
-- the sort.
waterFillAfter :: Int
waterFillAfter = 8

-- Lock, in one go, every grow child that 'lockGrow' sweeps would lock one
-- after another. A child locks when its content per unit of grow factor is
-- more than the share per unit left once the children above it lock; the
-- share per unit only falls as children lock, so taking children by content
-- per unit, largest first, and stopping at the first that fits locks the
-- same set.
waterFillGrow :: IOArr Float -> IOArr Float -> Float -> Int -> IO ()
waterFillGrow mainArr crossArr room n = do
  (occupied, gfSum) <- scanGrow mainArr crossArr 0 n 0 0
  let collect acc i = do
        gf <- readPrimArray crossArr i
        need <- readPrimArray mainArr i
        pure (if gf > 0 then (need / gf, need, gf, i) : acc else acc)
      lockFrom !free !g ((_, need, gf, i) : rest)
        | need * g > gf * free = do
            writePrimArray crossArr i 0
            lockFrom (free - need) (g - gf) rest
      lockFrom _ _ _ = pure ()
  growing <- foldUpTo n collect []
  lockFrom (room - occupied) gfSum (sortOn (\(perUnit, _, _, _) -> Down perUnit) growing)

alignX :: AlignX -> Float -> Float -> Float -> Float
alignX AlignStart cx _ _ = cx
alignX AlignCenter cx cw iw = cx + (cw - iw) / 2
alignX AlignEnd cx cw iw = cx + cw - iw

alignY :: AlignY -> Float -> Float -> Float -> Float
alignY AlignTop cy _ _ = cy
alignY AlignMiddle cy ch ih = cy + (ch - ih) / 2
alignY AlignBottom cy ch ih = cy + ch - ih
-- Only a row has a baseline to share; 'positionRowFromParent' places these.
alignY AlignBaseline cy _ _ = cy

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
--   row that has some, and otherwise its first child's, placed in a row or
--   among layers by its alignment.
-- * anything else: its bottom edge.
childBaseline :: SolveEnv -> NodeIdx -> Float -> IO Float
childBaseline env@SolveEnv {seArena = na, seArrays = a} ci h = do
  nt <- getNodeType na ci
  si <- getStyleIdx na ci
  case nt of
    NodeText -> do
      raw <- getText na ci
      if T.null raw
        then pure h
        else do
          measurer@TextMeasurer {tmMetrics = fm} <- textNodeMeasurer env ci
          rowChild <- parentIsRow na ci
          let newline = T.any (== '\n') raw
          wrapped <-
            if newline || rowChild
              then pure newline
              else do
                AxisSizing wTag _ _ maxW <- getWidthSizing na ci
                Rect _ _ w _ <- getNodeRect na ci
                effMaxW <- if maxW < 1e8 then pure maxW else findAncestorMaxW na ci
                let cap = textWrapCap effMaxW wTag w
                (tw, _) <- measureFontLine measurer raw
                pure (cap < 1e8 && cap + 0.5 < tw)
          pure (textBaseline fm (if wrapped then fmLineHeight fm else h))
    _
      | hasCenteredLabel nt && not (nt == NodeButton && hasFlag buttonFlagClose si) -> do
          -- Widget labels take the node's font size in the default face.
          fm <- tmMetrics <$> nodeMeasurer env ci nt si
          pure (textBaseline fm h)
      | isContainerNode nt -> do
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
      | otherwise -> pure h
  where
    textBaseline fm boxH = centeredTextY fm 0 boxH (fmLineHeight fm) + fmAscent fm

-- | Place the measured modals, windows and popups within a window @winW@
-- by @winH@ logical pixels, and lay out their children. A modal is centred,
-- leaving the standard window margin where space permits. A window takes its
-- saved position and size (@lookupPos@, @lookupSize@), by default its
-- measured size near the top-right corner. A popup goes where its registered
-- anchor, side and gap put it ('computePopupPosition'), by default at the
-- origin with automatic placement.
placeFloatingNodes ::
  NodeArena ->
  Measurers ->
  Float ->
  Float ->
  (WidgetId -> IO (Maybe (Float, Float))) ->
  (WidgetId -> IO (Maybe (Float, Float))) ->
  (WidgetId -> IO (Maybe (PopupAnchor, PopupPlacement, Float))) ->
  IO ()
placeFloatingNodes na ms winW winH lookupPos lookupSize lookupAnchor = do
  env <- solveEnv na ms Nothing
  forClassNodes_ na FloatingNodes $ \idx -> do
    nt <- getNodeType na idx
    wid <- getWidgetId na idx
    Rect _ _ iw ih <- getNodeRect na idx
    case nt of
      NodeModal -> do
        let w = min iw (max 0 (winW - 2 * windowMargin))
            h = min ih (max 0 (winH - 2 * windowMargin))
        positionNodeA env 0 idx (Rect (max 0 ((winW - w) / 2)) (max 0 ((winH - h) / 2)) w h)
      NodeWindow -> do
        (w0, h0) <- fromMaybe (min iw winW, min ih winH) <$> lookupSize wid
        mpos <- lookupPos wid
        placeWindowNode na ms winW winH idx w0 h0 $ \w -> fromMaybe (winW - w - windowMargin, windowMargin) mpos
      _ -> do
        mcfg <- lookupAnchor wid
        let (anchor, placement, offset) = fromMaybe (AnchorPoint (V2 0 0), PlacementAuto, 4) mcfg
            (x, y) = computePopupPosition winW winH windowMargin iw ih anchor placement offset
        positionNodeA env 0 idx (Rect x y iw ih)

-- | Lay out window @idx@ at size @w0 h0@, clamped to its min and max size and
-- the screen, with its origin, given that size, clamped on screen. Fit sizing
-- caps at intrinsic size; floating windows use an explicit frame size.
placeWindowNode :: NodeArena -> Measurers -> Float -> Float -> NodeIdx -> Float -> Float -> (Float -> (Float, Float)) -> IO ()
placeWindowNode na ms winW winH idx w0 h0 originFor = do
  AxisSizing _ _ minW maxW <- getWidthSizing na idx
  AxisSizing _ _ minH maxH <- getHeightSizing na idx
  let w = clamp minW (min maxW winW) w0
      h = clamp minH (min maxH winH) h0
      (x0, y0) = originFor w
      x = clamp 0 (max 0 (winW - w)) x0
      y = clamp 0 (max 0 (winH - h)) y0
  setRect na idx x y w h
  env <- solveEnv na ms Nothing
  (pad, gap, dir) <- containerFlow (seArrays env) idx
  positionChildren env 0 idx dir gap pad (Rect x y w h)

-- | Horizontal placement for a widget-anchored popup. Aligns the popup's left
-- edge with the anchor even when the anchor sits inside the window margin (a
-- menu bar flush to the left, say); the margin is only there to keep the popup
-- clear of the right edge.
clampPopupX :: Float -> Float -> Float -> Float -> Float
clampPopupX margin winW iw x0
  | x0 < margin && x0 + iw <= winW = max 0 x0
  | otherwise = clamp margin (winW - iw - margin) x0

-- | Popup origin from window width/height, margin, popup width/height, anchor,
-- preferred placement, and gap. All coordinates use logical pixels.
-- Flips or clamps placement to the available window space.
computePopupPosition ::
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  PopupAnchor ->
  PopupPlacement ->
  Float ->
  (Float, Float)
computePopupPosition winW winH margin iw ih anchor placement offset =
  case anchor of
    AnchorPoint (V2 px py) ->
      -- On each axis the popup goes before the point, after it or at it, and
      -- to its other side where it would overflow and fits there.
      let onAxis p size lim isBefore isAfter =
            let p0
                  | isBefore = p - size - offset
                  | isAfter = p + offset
                  | otherwise = p
             in if p0 + size > lim - margin && p - size - margin >= 0
                  then p - size - offset
                  else clamp margin (lim - size - margin) p0
       in ( onAxis px iw winW (placement == PlacementLeft) (placement == PlacementRight)
          , onAxis py ih winH (placement == PlacementAbove) (placement == PlacementBelow)
          )
    AnchorRect (Rect rx ry rw rh) ->
      case placement of
        PlacementBelow -> (clampPopupX margin winW iw rx, clampY (after ry rh winH ih))
        PlacementAbove -> (clampPopupX margin winW iw rx, clampY (before ry rh winH ih))
        PlacementRight -> (clampPopupX margin winW iw (after rx rw winW iw), clampY ry)
        PlacementLeft -> (clampPopupX margin winW iw (before rx rw winW iw), clampY ry)
        PlacementAuto ->
          let spaceBelow = winH - margin - (ry + rh + offset)
              spaceAbove = ry - offset - margin
              y = if spaceBelow >= ih || spaceBelow >= spaceAbove
                    then ry + rh + offset
                    else ry - ih - offset
              x = clampPopupX margin winW iw rx
           in (x, clampY y)
        PlacementAtCursor ->
          (clampPopupX margin winW iw rx, clampY (ry + rh + offset))
  where
    -- Keep the popup's top edge within the window margins.
    clampY y = clamp margin (winH - ih - margin) y
    -- A popup of @size@ after (or before) the anchor span @lo@..@lo + len@ on
    -- an axis of length @lim@, taking the other side when it would overflow
    -- and the other side fits.
    after lo len lim size
      | lo + len + offset + size > lim - margin && lo - size - offset >= margin = lo - size - offset
      | otherwise = lo + len + offset
    before lo len lim size
      | lo - size - offset < margin && lo + len + offset + size <= lim - margin = lo + len + offset
      | otherwise = lo - size - offset
