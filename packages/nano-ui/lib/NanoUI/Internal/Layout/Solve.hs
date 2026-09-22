-- | The layout solver: measures and places the node arena's flow tree, then
-- positions modals, windows and popups.
module NanoUI.Internal.Layout.Solve
  ( solveLayout
  , runCustomMeasure
  , FontResolver
  , Measurers (..)
  , placeFloatingNodes
  , computePopupPosition
  , placeWindowNode
  , scrollBarSlotOf
  , windowBodyScroller
  , findAncestorMaxW
  , textWrapCap
  ) where

import Control.Monad (foldM, forM, forM_, mfilter, unless, when)
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
  ( CustomMeasureFn
  , FontMetrics (..)
  , checkboxBoxSize
  , checkboxLeading
  , treeRowLeading
  , treeItemPadding
  , classifyScrollBar
  , measureTextIO
  , lineWidthIO
  , WrapResult
  , wrapMeasure
  , tableCellInset
  , ScrollBarSlot (..)
  , widgetPadding
  , buttonPadding
  , menuItemPadX
  , menuOuterPad
  , selectPadding
  , isDefaultNodeFont
  , sliderTrackHeight
  , sliderHandleDiameter
  , sliderHandleSlack
  , centeredTextY
  )
import NanoUI.Internal.Layout.Arena
  ( AxisSizing (..)
  , NodeClass (FloatingNodes)
  , forClassNodes_
  , DirTag (..)
  , FlexScratch (..)
  , IOArr
  , LayoutCache (..)
  , CustomMeasureRecord
  , NodeArena
  , NodeArenaArrays
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaArrays
  , arenaCount
  , floatingNodeCount
  , hasCenteredLabel
  , withArenaArraysSnap
  , subtreeArrays
  , geomX
  , geomY
  , geomW
  , geomH
  , stylePadL
  , stylePadR
  , stylePadT
  , stylePadB
  , styleGap
  , styleGridMinColW
  , tagNodeType
  , tagDirection
  , treeStyleIdx
  , treeGridCols
  , tagScrollBarSlot
  , readAxisSizing
  , readGeom
  , writeTagEnum
  , writeGeom
  , readStyle
  , readTagEnum
  , readTree
  , treeParent
  , getAlignX
  , getAlignY
  , getChildCount
  , getDirection
  , findChildM
  , getFirstChild
  , getGap
  , getGridCols
  , getGridMinColW
  , getHeightSizing
  , getNodeType
  , getOptions
  , getParent
  , getStyleIdx
  , getPadding
  , getRect
  , getText
  , getWidgetId
  , getWidthSizing
  , parentIsRow
  , isContainerNode
  , isFloatingNode
  , isScrollNode
  , setRect
  , getNodeValue
  , setNodeValue
  , getNodeFontSize
  , getScrollContentW
  , setScrollContentW
  , ensureScratchCapacity
  , AxisSnapshot (..)
  , ensureAxisSnapshot
  , memoizeWidth
  , foldFlowChildrenM
  , naScratch
  , naWrapMemo
  , naFitMemo
  )
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Style (AlignX (..), AlignY (..), FontStyle (..), FontVariant (..), FontWeight (..), Padding (..), windowMargin)
import NanoUI.Internal.Types (PopupAnchor (..), PopupPlacement (..), Rect (..), V2 (..), clamp, gridSpan, onGrid)
import NanoUI.Internal.WidgetText
  ( hasFlag
  , colorPickerSvH
  , textNodeFontKey
  , textNodeFontVariant
  , textNodeFontWeight
  , textNodeFontStyle
  , treeDecodeStyle
  , selectDisplayText
  , selectChevronReserve
  , textInputFieldHeight
  , textInputMinWidth
  , textInputFlagSearch
  , textInputFlagNumeric
  , numericStepperW
  , textInputFlagSelectable
  , searchInputReserveW
  , buttonFlagTable
  , buttonFlagMenu
  , buttonFlagClose
  , tableHeaderDisplayText
  )
import NanoUI.Internal.Frame.Scroll.Geometry
  ( decodeScrollConfig
  , isScrollStyle2D
  , scrollAxisGutter
  , scrollGutters2D
  , scrollPolicyX
  , scrollPolicyY
  )

-- | Resolve size, weight, slant, and variant to metrics plus logical-pixel
-- text measurement. Size zero requests the backend default.
type FontResolver = Float -> FontWeight -> FontStyle -> FontVariant -> IO (FontMetrics, Text -> IO (Float, Float))

-- | Per-solve constants threaded through the measure and position passes.
data SolveEnv = SolveEnv
  { seArena :: !NodeArena
  , seArrays :: !NodeArenaArrays
  , seFm :: !FontMetrics
  , seMonoFm :: !FontMetrics
  , seMeasure :: !(Text -> IO (Float, Float))
  , seResolveFont :: !FontResolver
  , seLookupMeasure :: !(WidgetId -> IO (Maybe CustomMeasureFn))
  , seWrap :: !TextWrapper
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
solveEnv na Measurers {msFm, msMonoFm, msMeasure, msResolveFont, msLookupMeasure, msWrap} mCache = do
  a <- arenaArrays na
  (sub, measured) <- subtreeArrays na
  pure $
    SolveEnv na a msFm msMonoFm msMeasure msResolveFont msLookupMeasure msWrap sub measured
      (mfilter ((> 0) . lcCount) mCache)
      Nothing

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

textNodeMeasurer :: SolveEnv -> NodeIdx -> IO TextMeasurer
textNodeMeasurer SolveEnv {seArena = na, seFm = fm, seMonoFm = monoFm, seMeasure = measure, seResolveFont = resolveFont} idx = do
  si <- getStyleIdx na idx
  size <- getNodeFontSize na idx
  let variant = textNodeFontVariant si
      weight = textNodeFontWeight si
      style = textNodeFontStyle si
  (metrics, measureLine) <-
    if isDefaultNodeFont size weight style variant
      then pure (if variant == FontMono then monoFm else fm, measure)
      else resolveFont size weight style variant
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
  wrapMeasure metrics width <$> seWrap env font lineW text width
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

-- | Wrap policy once a width is assigned: wrap when allowed and the single
-- line overflows a positive wrap width.
{-# INLINE wrapsNarrower #-}
wrapsNarrower :: Bool -> Float -> Float -> Bool
wrapsNarrower allowed wrapW lineW = allowed && wrapW + 0.5 < lineW && wrapW > 0

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
solveLayout na ms rootW rootH mCache =
  withArenaArraysSnap na $ do
    count <- arenaCount na
    if count <= 0
      then pure IM.empty
      else do
        measureLog <- newIORef IM.empty
        env0 <- solveEnv na ms mCache
        let env = env0 {seMeasureLog = Just measureLog}
        measurePass env count
        positionNodeA env 0 0 0 0 rootW rootH
        floatingCount <- floatingNodeCount na
        quantizeResultsA (seArrays env) count floatingCount (fmSnapScale (msFm ms))
        readIORef measureLog

-- | Snap the solved geometry of the @count@ nodes to the device pixel grid of
-- scale @s@. @floatingCount@ is the arena's floating node count.
quantizeResultsA :: NodeArenaArrays -> Int -> Int -> Float -> IO ()
quantizeResultsA a count floatingCount s
  | s <= 0 = pure ()
  -- Nothing floats, so every node snaps and none needs marking.
  | floatingCount <= 0 =
      let go i = when (i < count) (snapNode i >> go (i + 1)) in go 0
  | otherwise = do
      -- A floating node (modal, window, popup) and everything inside it is
      -- laid out by placement after the solve, which sizes the subtree from
      -- these measured sizes. Rounding them here would size a dialog and its
      -- content-sized parts off their content, so the subtree keeps them;
      -- placement overwrites its geometry anyway. A parent always precedes
      -- its children, so one pass marks each node from its parent.
      floating <- newPrimArray count :: IO (IOArr Word8)
      let go i
            | i >= count = pure ()
            | otherwise = do
                nt <- readTagEnum a i tagNodeType
                parent <- readTree a i treeParent
                inFloating <-
                  if isFloatingNode nt
                    then pure True
                    else if parent >= 0 then (/= 0) <$> readPrimArray floating parent else pure False
                writePrimArray floating i (if inFloating then 1 else 0)
                unless inFloating (snapNode i)
                go (i + 1)
      go 0
 where
  snapNode i = do
    x <- readGeom a i geomX
    y <- readGeom a i geomY
    w <- readGeom a i geomW
    h <- readGeom a i geomH
    -- Snap both edges and take the size between them. Rounding the size on
    -- its own can push a node's far edge a pixel past the snapped origin of
    -- the sibling that starts there, and the node then paints over it (a
    -- table cell over the column rule beside it).
    writeGeom a i geomX (onGrid s x)
    writeGeom a i geomY (onGrid s y)
    writeGeom a i geomW (max 0 (gridSpan s x (x + w)))
    writeGeom a i geomH (max 0 (gridSpan s y (y + h)))

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
        if restored
          then pure True
          else if idx >= lcCount lc
          then pure False
          else do
            cw <- readPrimArray (lcMeasured lc) (idx * 2)
            ch <- readPrimArray (lcMeasured lc) (idx * 2 + 1)
            pure (cw == w && ch == h)
      unless same $ do
        p <- readTree (seArrays env) idx treeParent
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
      nt <- readTagEnum (seArrays env) idx tagNodeType
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
  (_, _, w, h) <- getRect (seArena env) idx
  let ma = seMeasured env
  writePrimArray ma (idx * 2) w
  writePrimArray ma (idx * 2 + 1) h
  pure (w, h)

measureNode :: SolveEnv -> NodeIdx -> IO ()
measureNode env@SolveEnv {seArena = na} idx = do
  nt <- readTagEnum (seArrays env) idx tagNodeType
  case nt of
    NodeText -> measureTextNode env idx
    NodeSpacer -> measureSpacer na idx
    NodeSeparator -> measureSeparator na idx
    NodeScrollContainer -> measureScrollContainer env idx
    NodeImage -> measureImage na idx
    NodeBox -> measureImage na idx
    NodeDrawing -> do
      wid <- getWidgetId na idx
      mFn <- seLookupMeasure env wid
      case mFn of
        Just fn -> measureCustomNode env fn idx
        Nothing -> measureImage na idx
    _
      | isContainerNode nt -> do
          measureContainer env idx
          when (nt == NodeModal) $ setNodeValue na idx 0
      | otherwise -> measureWidget env idx

measureCustomNode :: SolveEnv -> CustomMeasureFn -> NodeIdx -> IO ()
measureCustomNode env@SolveEnv {seArena = na} measureFn idx = do
  wAx <- getWidthSizing na idx
  hAx <- getHeightSizing na idx
  let ((mw, mh), record) = customMeasure (seFm env) measureFn wAx hAx
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
customMeasure :: FontMetrics -> CustomMeasureFn -> AxisSizing -> AxisSizing -> ((Float, Float), CustomMeasureRecord)
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
drawingHeightAt SolveEnv {seArena = na, seFm = fm, seLookupMeasure = lookupMeasure} idx w hAx fallback = do
  wid <- getWidgetId na idx
  lookupMeasure wid >>= \case
    Just measure -> pure (clamp (axMin hAx) (axMax hAx) (snd (measure fm (w, offeredExtent hAx))))
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
  let reportedW =
        if wTag == SizingGrow && parentAssigns
          then clamp minW maxW 0
          else clamp minW maxW tw
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

measureImage :: NodeArena -> NodeIdx -> IO ()
measureImage na idx = do
  wAx <- getWidthSizing na idx
  hAx <- getHeightSizing na idx
  -- Without a fixed size, an image takes its minimum, or 32 without one.
  let orMin ax = if axMin ax > 0 then axMin ax else 32
  setRect na idx 0 0 (sizeWithin wAx (orMin wAx)) (sizeWithin hAx (orMin hAx))

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

{-# INLINE measureMarkedWidget #-}
measureMarkedWidget ::
  FontMetrics ->
  (Text -> IO (Float, Float)) ->
  Text ->
  Float ->
  IO (Float, Float, Float, Float)
measureMarkedWidget fm measure body leading = do
  (mw, mh) <- measure (if T.null body then " " else body)
  pure (mw, max mh (checkboxBoxSize fm), leading, 0)

measureWidget :: SolveEnv -> NodeIdx -> IO ()
measureWidget env@SolveEnv {seArena = na, seArrays = a, seFm = fm, seMeasure = measure} idx = do
  nt <- readTagEnum a idx tagNodeType
  txt <- getText na idx
  si <- readTree a idx treeStyleIdx
  wAx <- readAxisSizing a idx True
  hAx <- readAxisSizing a idx False
  let (padX, padY) =
        case nt of
          NodeButton
            | hasFlag buttonFlagTable si ->
                (2 * tableCellInset, 0)
            -- Menu rows reserve the same gutter the text-field context menu
            -- paints (outer pad + item pad on each side of the label), so the
            -- generic popup panel sizes identically.
            | hasFlag buttonFlagMenu si ->
                (2 * (menuOuterPad + menuItemPadX), snd (buttonPadding fm))
            | otherwise -> buttonPadding fm
          NodeSelect -> selectPadding fm
          NodeTree -> treeItemPadding fm
          _
            | nt == NodeColorPicker
                || nt == NodeSlider
                || nt == NodeCheckbox
                || nt == NodeRadio
                || nt == NodeTextInput
                || nt == NodeTextArea ->
                (0, 0)
            | otherwise -> widgetPadding fm
  (tw, th, extraW, extraH) <-
    case nt of
      NodeSlider -> do
        let contentW = 60
            contentH = max sliderHandleDiameter (sliderTrackHeight + 2 * sliderHandleSlack)
        pure (contentW, contentH, 0, 0)
      NodeTree -> do
        let (_, depth, _, _) = treeDecodeStyle si
        measureMarkedWidget fm measure txt (treeRowLeading fm depth)
      NodeSelect -> do
        opts <- getOptions na idx
        let choices = if null opts then [""] else opts
        (mw, mh) <-
          foldM
            (\(!mw, !mh) c -> (\(w, h) -> (max mw w, max mh h)) <$> measure (selectDisplayText txt c))
            (0, 0)
            choices
        pure (mw, mh, selectChevronReserve, 0)
      -- Picker parts carry fixed layouts; the field grows to its square.
      NodeColorPicker -> pure (0, colorPickerSvH, 0, 0)
      NodeTextInput
        | hasFlag textInputFlagSelectable si -> do
            -- Size with the node's own font (paint and span placement resolve
            -- it too); the ambient `measure` is the default font only.
            measurer <- textNodeMeasurer env idx
            (mw, mh) <- measureFontLine measurer (if T.null txt then " " else txt)
            pure (mw, mh, 0, 0)
        -- Numeric field: a short editable box and its stepper.
        | hasFlag textInputFlagNumeric si ->
            pure (56, textInputFieldHeight fm, numericStepperW, 0)
        -- Caption-less search box: single row tall, icons counted in the
        -- width budget.
        | hasFlag textInputFlagSearch si -> do
            (lw, _) <- measure (if T.null txt then " " else txt)
            pure (max textInputMinWidth lw + searchInputReserveW fm, textInputFieldHeight fm, 0, 0)
        | otherwise -> do
            pw <- if T.null txt then pure 0 else fst <$> measure txt
            pure (max textInputMinWidth pw, textInputFieldHeight fm, 0, 0)
      NodeTextArea -> pure (textInputMinWidth, max 96 (textInputFieldHeight fm * 4), 0, 0)
      _
        | nt == NodeCheckbox || nt == NodeRadio ->
            measureMarkedWidget fm measure txt (checkboxLeading fm)
        | otherwise -> do
            let body
                  | T.null txt = " "
                  | hasFlag buttonFlagTable si = tableHeaderDisplayText txt
                  | otherwise = txt
            (mw, mh) <- measure body
            pure (mw, mh, 0, 0)
  setRect na idx 0 0 (fixedOr wAx (tw + padX + extraW)) (fixedOr hAx (th + padY + extraH))

measureContainer :: SolveEnv -> NodeIdx -> IO ()
measureContainer env@SolveEnv {seArena = na, seArrays = a} idx = do
  (pad, gap, dir) <- containerFlow a idx
  gCols <- readTree a idx treeGridCols
  minColW <- readStyle a idx styleGridMinColW
  wAx@(AxisSizing wTag _ minW _) <- readAxisSizing a idx True
  hAx <- readAxisSizing a idx False
  nt <- readTagEnum a idx tagNodeType
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
      else if dir == DirColumn && chrome
        then do
          n <- loadChildrenScratch na idx (flowChildSize env False innerMaxW innerAvailH)
          foldChromeColumnScratch na n gap
        else foldChildDimsFromParent env idx dir gap
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
          (_, _, _, bodyH) <- getRect na ci
          contentH <- getNodeValue na ci
          let innerH = bodyH - padT pad - padB pad - max 0 overflow
          pure (scrollAxisGutter (scrollPolicyY (decodeScrollConfig si)) ScrollBarWindow (padR pad) contentH innerH)

-- | The scroll container holding window or modal @idx@'s body.
windowBodyScroller :: NodeArena -> NodeIdx -> IO (Maybe NodeIdx)
windowBodyScroller na idx =
  findChildM na idx $ \ci -> do
    nt <- getNodeType na ci
    if nt /= NodeScrollContainer
      then pure False
      else (== ScrollBarWindow) <$> scrollBarSlotOf na ci

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
    if parent < 0
      then pure False
      else do
        pnt <- getNodeType na parent
        pure (pnt == NodeWindow || pnt == NodeModal)
  inPanel <- hasPanelAncestor na parent
  let slot = classifyScrollBar isWin (wTag == SizingGrow && hTag == SizingGrow && not inPanel)
  writeTagEnum a idx tagScrollBarSlot slot
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
  FlowAcc count main cross <- foldFlowChildrenM na idx step (FlowAcc 0 0 0)
  -- A row's baseline-aligned children stand on one line, so together they are
  -- as tall as the most room any takes above it plus the most any takes below.
  baseline <-
    if dir == DirRow
      then foldFlowChildrenM na idx baselineStep (0, 0)
      else pure (0, 0)
  pure
    ( case dir of
        DirRow ->
          ( main + gap * fromIntegral (max 0 (count - 1))
          , if count <= 0 then 0 else max cross (uncurry (+) baseline)
          )
        DirColumn ->
          ( if count <= 0 then 0 else main
          , cross + gap * fromIntegral (max 0 (count - 1))
          )
    )
  where
    step (FlowAcc count main cross) ci = do
      (_, _, w, h) <- getRect na ci
      pure $
        case dir of
          DirRow -> FlowAcc (count + 1) (main + w) (max cross h)
          DirColumn -> FlowAcc (count + 1) (max main w) (cross + h)
    baselineStep acc@(above, below) ci = do
      ay <- getAlignY na ci
      if ay /= AlignBaseline
        then pure acc
        else do
          (_, _, _, h) <- getRect na ci
          b <- childBaseline env ci h
          pure (max above b, max below (h - b))

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
  gapSum <- columnGapSumScratch na True n gap
  let go !i !maxW !totalH
        | i >= n = pure (maxW, totalH + gapSum)
        | otherwise = do
            w <- readPrimArray wArr i
            h <- readPrimArray hArr i
            go (i + 1) (max maxW w) (totalH + h)
  go 0 0 0

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
gridRowHeight hArr n cols r = go 0 0
  where
    go !j !accH
      | j >= cols = pure accH
      | otherwise = do
          let k = r * cols + j
          if k >= n
            then pure accH
            else do
              h <- readPrimArray hArr k
              go (j + 1) (max accH h)

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
          calcRows !r !totalH
            | r >= numRows = pure totalH
            | otherwise = do
                rowH <- gridRowHeight hArr n cols r
                calcRows (r + 1) (totalH + rowH)
      totalH <- calcRows 0 0
      let contentH = totalH + gap * fromIntegral (max 0 (numRows - 1))
      contentW <-
        if innerMaxW > 0 && innerMaxW < 1e8
          then pure innerMaxW
          else if minColW > 0
            then pure (fromIntegral cols * minColW + gap * fromIntegral (max 0 (cols - 1)))
            else do
              let getMaxChildW !i !accW
                    | i >= n = pure accW
                    | otherwise = do
                        w <- readPrimArray wArr i
                        getMaxChildW (i + 1) (max accW w)
              maxChildW <- getMaxChildW 0 0
              pure (fromIntegral cols * maxChildW + gap * fromIntegral (max 0 (cols - 1)))
      pure (contentW, contentH)

recomputeFitHeightAtWidth :: SolveEnv -> NodeIdx -> Float -> IO Float
recomputeFitHeightAtWidth env idx availW = do
  let na = seArena env
  snd <$> memoizeWidth na (naFitMemo na) idx availW ((,) 0 <$> recomputeFitHeightAtWidthGo env idx availW)

recomputeFitHeightAtWidthGo :: SolveEnv -> NodeIdx -> Float -> IO Float
recomputeFitHeightAtWidthGo env@SolveEnv {seArena = na} idx availW = do
  nt <- getNodeType na idx
  AxisSizing wTag wVal minW maxW <- getWidthSizing na idx
  hAx@(AxisSizing hTag _ minH maxH) <- getHeightSizing na idx
  (_, _, _, oldH) <- getRect na idx
  let effW = case wTag of
        SizingPercent -> availW * wVal / 100
        SizingFixed -> wVal
        _ -> availW
      effW' = clamp minW maxW effW
  case nt of
    NodeText
      | hTag /= SizingFixed -> do
          isRowChild <- parentIsRow na idx
          txt <- getText na idx
          if T.null txt
            then pure (clamp minH maxH 0)
            else do
              TextBox {tbWrapped, tbH, tbLineH} <-
                measureTextNodeAt env idx txt effW' (wrapsNarrower (wTag /= SizingFit && not isRowChild))
              pure (if tbWrapped then clamp minH maxH (max tbLineH tbH) else oldH)
      | otherwise -> pure oldH

    -- A measured drawing, like wrapped text, can be taller when narrower.
    NodeDrawing
      | hTag == SizingFit -> drawingHeightAt env idx effW' hAx oldH
      | otherwise -> pure oldH

    _ | (nt == NodeContainer || nt == NodePanel), hTag /= SizingFixed -> do
          dir <- getDirection na idx
          if dir == DirRow
            then pure oldH
            else do
              pad <- getPadding na idx
              gap <- getGap na idx
              let innerW = max 0 (effW' - padL pad - padR pad)
                  step (FlowAcc count contentH _) ci = do
                    AxisSizing subWTag subWVal _ subMaxW <- getWidthSizing na ci
                    let subW = case subWTag of
                          SizingPercent -> innerW * subWVal / 100
                          SizingFixed -> subWVal
                          _ -> innerW
                        subW' = if subMaxW < 1e8 then min subW subMaxW else subW
                    subH <- recomputeFitHeightAtWidth env ci subW'
                    pure (FlowAcc (count + 1) (contentH + subH) 0)
              FlowAcc count contentH _ <- foldFlowChildrenM na idx step (FlowAcc 0 0 0)
              let totalH =
                    if count <= 0
                      then 0
                      else contentH + gap * fromIntegral (count - 1)
              pure (clamp minH maxH (totalH + padT pad + padB pad))

    _ -> pure oldH

-- | Load a parent's flow children into the flex scratch in child order, with
-- each child's (width, height) from @sizeOf@. Returns the child count.
{-# INLINE loadChildrenScratch #-}
loadChildrenScratch :: NodeArena -> NodeIdx -> (NodeIdx -> IO (Float, Float)) -> IO Int
loadChildrenScratch na parent sizeOf = do
  cc <- getChildCount na parent
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
  w <- readGeom a ci geomW
  h <- readGeom a ci geomH
  AxisSizing wTag wVal minW maxW <- readAxisSizing a ci True
  AxisSizing hTag hVal minH maxH <- readAxisSizing a ci False
  let w' =
        case wTag of
          SizingPercent -> clamp minW maxW (availW * wVal / 100)
          _ -> w
  h' <-
    if refit && hTag /= SizingFixed && hTag /= SizingPercent && (wTag == SizingGrow || wTag == SizingPercent || availW < w)
      then recomputeFitHeightAtWidth env ci (if wTag == SizingPercent then w' else availW)
      else pure $
        case hTag of
          SizingPercent -> clamp minH maxH (availH * hVal / 100)
          _ -> h
  pure (w', h')

positionNodeA ::
  SolveEnv ->
  Int ->
  NodeIdx ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
positionNodeA env@SolveEnv {seArena = na, seArrays = a} depth idx x y availW availH = do
  wAx <- readAxisSizing a idx True
  hAx@(AxisSizing hTag _ minH maxH) <- readAxisSizing a idx False
  intrinsicW <- readGeom a idx geomW
  intrinsicH <- readGeom a idx geomH
  nt <- readTagEnum a idx tagNodeType
  let !w = resolveSize wAx intrinsicW availW
      !resolvedH = resolveSize hAx intrinsicH availH
  isRowChild <- parentIsRow na idx
  h <-
    if nt == NodeText && hTag /= SizingFixed && not isRowChild
      then do
        txt <- getText na idx
        if T.null txt
          then pure (clamp minH maxH 0)
          else do
            TextBox {tbWrapped, tbH, tbLineH} <-
              measureTextNodeAt env idx txt w (wrapsNarrower (axTag wAx /= SizingFit))
            pure (if tbWrapped then clamp minH maxH (max tbLineH tbH) else resolvedH)
      else
        if (nt == NodeContainer || nt == NodePanel) && hTag == SizingFit
          then pure (clamp minH maxH (max intrinsicH availH))
          else
            if nt == NodeDrawing && hTag == SizingFit && w /= intrinsicW
              then
                -- A measured drawing laid out at another width than it was
                -- measured at takes its height at the width it got.
                drawingHeightAt env idx w hAx resolvedH
              else pure resolvedH
  setRect na idx x y w h
  when (isContainerNode nt) $ do
    (pad, gap, dir) <- containerFlow a idx
    if isScrollNode nt
      then positionScrollChildren env depth idx dir gap pad x y w h
      else positionChildren env depth idx dir gap pad x y w h
  when (hTag == SizingFit && isContainerNode nt && not (isScrollNode nt)) $
    adjustFitHeight na idx minH maxH x y w

-- | A container's resolved padding and gap, and its direction.
{-# INLINE containerFlow #-}
containerFlow :: NodeArenaArrays -> NodeIdx -> IO (Padding, Float, DirTag)
containerFlow a idx = do
  pad <- Padding <$> readStyle a idx stylePadL <*> readStyle a idx stylePadR <*> readStyle a idx stylePadT <*> readStyle a idx stylePadB
  gap <- readStyle a idx styleGap
  dir <- readTagEnum a idx tagDirection
  pure (pad, gap, dir)

adjustFitHeight :: NodeArena -> NodeIdx -> Float -> Float -> Float -> Float -> Float -> IO ()
adjustFitHeight na idx minH maxH x y w = do
  fc <- getFirstChild na idx
  when (fc >= 0) $ do
    pad <- getPadding na idx
    let step maxB ci = do
          (_, subY, _, subH) <- getRect na ci
          pure (max maxB (subY + subH))
    maxB <- foldFlowChildrenM na idx step y
    let fitH = clamp minH maxH (maxB + padB pad - y)
    (_, _, _, curH) <- getRect na idx
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
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
positionScrollChildren env@SolveEnv {seArena = na} depth idx dir gap pad px py pw ph = do
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
      positionChildren env depth idx DirColumn gap (Padding 0 0 0 0) cx cy layoutW layoutH
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
          positionRowFromParent env depth idx gap cx cy rowMain (innerH - gutterRow)
        DirColumn -> positionColumnScroll env depth idx gap cx cy (innerW - gutterCol) innerH contentSize
  fc <- getFirstChild na idx
  when (fc >= 0) $ do
    let step (FlowAcc count maxB maxR) ci = do
          (subX, subY, subW, subH) <- getRect na ci
          pure (FlowAcc (count + 1) (max maxB (subY + subH)) (max maxR (subX + subW)))
    FlowAcc _ maxB maxR <- foldFlowChildrenM na idx step (FlowAcc 0 cy cx)
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
    if isScrollStyle2D si
      then do
        oldH <- getNodeValue na idx
        oldW <- getScrollContentW na idx
        setNodeValue na idx (max oldH actualContentH)
        setScrollContentW na idx (max oldW actualContentW)
      else do
        oldVal <- getNodeValue na idx
        let actual = case dir of DirColumn -> actualContentH; DirRow -> actualContentW
        setNodeValue na idx (max oldVal actual)

-- | Where a scroll container's bar sits, as measurement stored it. Text
-- areas and other nodes read 'ScrollBarList'.
{-# INLINE scrollBarSlotOf #-}
scrollBarSlotOf :: NodeArena -> NodeIdx -> IO ScrollBarSlot
scrollBarSlotOf na idx = arenaArrays na >>= \a -> readTagEnum a idx tagScrollBarSlot

hasPanelAncestor :: NodeArena -> NodeIdx -> IO Bool
hasPanelAncestor na = go
  where
    go p
      | p < 0 = pure False
      | otherwise = do
          nt <- getNodeType na p
          if nt == NodePanel
            then pure True
            else if isFloatingNode nt then pure False else getParent na p >>= go

positionColumnScroll ::
  SolveEnv ->
  Int ->
  NodeIdx ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
positionColumnScroll env@SolveEnv {seArena = na} depth parent gap cx cy innerW innerH contentSize = do
  n <- loadChildrenScratch (seArena env) parent (flowChildSize env True innerW innerH)
  withAxisSnaps na depth n contentSize (gap * fromIntegral (max 0 (n - 1))) False $ \idxSnap outSnap -> do
    let go !i !curY
          | i >= n = pure ()
          | otherwise = do
              ci <- readPrimArray idxSnap i
              fh <- readPrimArray outSnap i
              nt <- getNodeType na ci
              fx <- columnChildX na ci cx innerW
              let visibleSlice = max 0 (innerH - (curY - cy))
                  nodeH =
                    if isScrollNode nt
                      then min fh visibleSlice
                      else fh
              positionNodeA env (depth + 1) ci fx curY innerW nodeH
              (_, _, _, placedH) <- getRect na ci
              go (i + 1) (curY + placedH + gap)
    go 0 cy

-- | Left edge of column child @ci@ in a column of width @cw@ at @cx@. Grow and
-- percent children already take the full width; alignment is for content
-- narrower than the column, not for shifting a full-width box past it.
{-# INLINE columnChildX #-}
columnChildX :: NodeArena -> NodeIdx -> Float -> Float -> IO Float
columnChildX na ci cx cw = do
  wTag <- axTag <$> getWidthSizing na ci
  if wTag == SizingGrow || wTag == SizingPercent
    then pure cx
    else do
      (_, _, iw, _) <- getRect na ci
      ax <- getAlignX na ci
      pure $! alignX ax cx cw iw

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
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
positionChildren env@SolveEnv {seArena = na} depth idx dir gap pad px py pw ph = do
  nt <- getNodeType na idx
  gCols <- getGridCols na idx
  minColW <- getGridMinColW na idx
  let chrome = isChromeColumn nt dir
      cx = px + padL pad
      cy = py + padT pad
      cw = pw - padL pad - padR pad
      ch = ph - padT pad - padB pad
  if gCols > 0 || minColW > 0
    then positionGrid env depth idx gCols minColW gap cx cy cw ch
    else case dir of
      DirRow -> positionRowFromParent env depth idx gap cx cy cw ch
      DirColumn -> positionColumnFromParent env depth idx gap chrome px pw cx cy cw ch

childRowCrossSize :: NodeArena -> NodeIdx -> Float -> IO Float
childRowCrossSize na ci availCross = do
  hAx@(AxisSizing hTag _ minH _) <- getHeightSizing na ci
  (_, _, _, intrinsic) <- getRect na ci
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
      (_, _, _, ih) <- getRect na ci
      pure (clamp minH maxH ih)
    _ -> pure (clamp minH maxH scratchH)

{-# INLINE withAxisSnaps #-}
withAxisSnaps ::
  NodeArena ->
  Int ->
  Int ->
  Float ->
  Float ->
  Bool ->
  (IOArr Int -> IOArr Float -> IO a) ->
  IO a
withAxisSnaps na depth n availMain gapSum horizontal act = do
  distributeScratch na n availMain gapSum horizontal
  FlexScratch {fsIdx = idxArr, fsOut = outArr} <- readIORef (naScratch na)
  AxisSnapshot idxSnap outSnap <- ensureAxisSnapshot na depth n
  copyMutablePrimArray idxSnap 0 idxArr 0 n
  copyMutablePrimArray outSnap 0 outArr 0 n
  act idxSnap outSnap

-- | Like 'withAxisSnaps' but snapshots the unscaled child cross sizes instead
-- of the distributed main-axis result. Grids compute rows from the measured
-- child heights, so freezing them lets the recursion reuse the working scratch.
withGridScratch :: NodeArena -> Int -> Int -> (IOArr Int -> IOArr Float -> IO a) -> IO a
withGridScratch na depth n act = do
  FlexScratch {fsIdx = idxArr, fsH = hArr} <- readIORef (naScratch na)
  AxisSnapshot idxSnap crossSnap <- ensureAxisSnapshot na depth n
  copyMutablePrimArray idxSnap 0 idxArr 0 n
  copyMutablePrimArray crossSnap 0 hArr 0 n
  act idxSnap crossSnap

positionRowFromParent ::
  SolveEnv ->
  Int ->
  NodeIdx ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
positionRowFromParent env@SolveEnv {seArena = na} depth parent gap cx cy cw ch = do
  n <- loadChildrenScratch (seArena env) parent (flowChildSize env False cw ch)
  withAxisSnaps na depth n cw (gap * fromIntegral (max 0 (n - 1))) True $ \idxSnap outSnap -> do
    -- The shared baseline sits as low as the deepest one among the children
    -- aligned on it, so the child with the tallest ascent stays at the top.
    let goBase !i !acc
          | i >= n = pure acc
          | otherwise = do
              ci <- readPrimArray idxSnap i
              ay <- getAlignY na ci
              if ay /= AlignBaseline
                then goBase (i + 1) acc
                else do
                  b <- childRowCrossSize na ci ch >>= childBaseline env ci
                  goBase (i + 1) (max acc b)
    rowBase <- goBase 0 0
    -- The cursor stays in raw floats, never snapped: rounding it re-compounds
    -- error every child (1.667 -> 2.0 -> ...) so a shrink row overruns its
    -- fixed width. Each child's far edge is the next one's raw origin, so
    -- quantizeResultsA, which snaps edges, puts both on the same pixel.
    let goRow !i !x
          | i >= n = pure ()
          | otherwise = do
              ci <- readPrimArray idxSnap i
              fw <- readPrimArray outSnap i
              -- Fit/fixed children keep content height. Only Grow/Percent eat `ch`.
              crossH <- childRowCrossSize na ci ch
              ay <- getAlignY na ci
              fy <-
                if ay == AlignBaseline
                  then (\b -> cy + rowBase - b) <$> childBaseline env ci crossH
                  else pure (alignY ay cy ch crossH)
              positionNodeA env (depth + 1) ci x fy fw crossH
              -- A grow child that its max width stopped short of its share
              -- hands the rest to the siblings after it instead of leaving a
              -- hole.
              placedW <- readGeom (seArrays env) ci geomW
              goRow (i + 1) (x + min fw placedW + gap)
    goRow 0 cx

positionGrid ::
  SolveEnv ->
  Int ->
  NodeIdx ->
  Int ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
positionGrid env@SolveEnv {seArena = na} depth parent gCols minColW gap cx cy cw ch = do
  n <- loadChildrenScratch (seArena env) parent (flowChildSize env False cw ch)
  when (n > 0) $ do
    let cols = gridColumnCount gCols minColW cw gap
        colW = max 0 ((cw - gap * fromIntegral (cols - 1)) / fromIntegral cols)
        numRows = (n + cols - 1) `quot` cols
    -- Freeze child indices and their measured cross sizes before recursing.
    -- Children reuse the working scratch while this grid iterates rows and
    -- columns, so the live arrays would be clobbered by the first child.
    withGridScratch na depth n $ \idxArr hArr ->
      do
        let goRows !r !curY
              | r >= numRows = pure ()
              | otherwise = do
                  rowH <- gridRowHeight hArr n cols r
                  let goCols !j
                        | j >= cols = pure ()
                        | otherwise = do
                            let k = r * cols + j
                            if k >= n
                              then pure ()
                              else do
                                ci <- readPrimArray idxArr k
                                wAx <- getWidthSizing na ci
                                hAx <- getHeightSizing na ci
                                (_, _, iw, ih) <- getRect na ci
                                let childW = resolveSize wAx iw colW
                                    childH = resolveSize hAx ih rowH
                                    itemX = cx + fromIntegral j * (colW + gap)
                                ax <- getAlignX na ci
                                ay <- getAlignY na ci
                                let fx = alignX ax itemX colW childW
                                    fy = alignY ay curY rowH childH
                                positionNodeA env (depth + 1) ci fx fy colW rowH
                                goCols (j + 1)
                  goCols 0
                  goRows (r + 1) (curY + rowH + gap)
        goRows 0 cy

positionColumnFromParent ::
  SolveEnv ->
  Int ->
  NodeIdx ->
  Float ->
  Bool ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
positionColumnFromParent env@SolveEnv {seArena = na} depth parent gap chrome px pw cx cy cw ch = do
  n <- loadChildrenScratch (seArena env) parent (flowChildSize env True cw ch)
  gapSum <- columnGapSumScratch na chrome n gap
  withAxisSnaps na depth n ch gapSum False $ \idxSnap outSnap -> do
    let go !i !y
          | i >= n = pure ()
          | otherwise = do
              ci <- readPrimArray idxSnap i
              fh <- readPrimArray outSnap i
              nt <- getNodeType na ci
              (fx, nodeW) <-
                if chrome && nt == NodeSeparator
                  then pure (px, pw)
                  else (,cw) <$> columnChildX na ci cx cw
              childH <- columnChildHeight na ci fh
              positionNodeA env (depth + 1) ci fx y nodeW childH
              (_, _, _, placedH) <- getRect na ci
              gapAfter <-
                if i + 1 >= n
                  then pure 0
                  else do
                    nextCi <- readPrimArray idxSnap (i + 1)
                    pairColumnGap na chrome nextCi gap
              go (i + 1) (y + placedH + gapAfter)
    go 0 cy

columnGapSumScratch :: NodeArena -> Bool -> Int -> Float -> IO Float
columnGapSumScratch _ False _ _ = pure 0
columnGapSumScratch na True n gap = do
  FlexScratch {fsIdx = idxArr} <- readIORef (naScratch na)
  let go !i !acc
        | i >= n - 1 = pure acc
        | otherwise = do
            b <- readPrimArray idxArr (i + 1)
            g <- pairColumnGap na True b gap
            go (i + 1) (acc + g)
  go 0 0

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
      shrinkTotal <- foldUpTo n (\acc i -> (acc +) . shrinkFactor <$> sizingAt i) 0
      when (shrinkTotal > 0) $ forUpTo_ n $ \i -> do
        ax <- sizingAt i
        main <- readPrimArray out i
        let delta = negate slack * shrinkFactor ax / shrinkTotal
        writePrimArray out i (max (axMin ax) (main - delta))

-- | Strict left fold over @0 .. n - 1@.
{-# INLINE foldUpTo #-}
foldUpTo :: Int -> (a -> Int -> IO a) -> a -> IO a
foldUpTo n f = go 0
  where
    go !i !acc
      | i >= n = pure acc
      | otherwise = f acc i >>= go (i + 1)

-- | Run @f@ on @0 .. n - 1@ in order.
{-# INLINE forUpTo_ #-}
forUpTo_ :: Int -> (Int -> IO ()) -> IO ()
forUpTo_ n f = foldUpTo n (\() i -> f i) ()

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
lockGrow :: IOArr Float -> IOArr Float -> Float -> Float -> Int -> Int -> Int -> IO Int
lockGrow mainArr crossArr !free !gfSum !i !end !acc
  | i >= end = pure acc
  | otherwise = do
      gf <- readPrimArray crossArr i
      if gf > 0
        then do
          need <- readPrimArray mainArr i
          if need * gfSum > gf * free
            then do
              writePrimArray crossArr i 0
              lockGrow mainArr crossArr free gfSum (i + 1) end (acc + 1)
            else lockGrow mainArr crossArr free gfSum (i + 1) end acc
        else lockGrow mainArr crossArr free gfSum (i + 1) end acc

-- Each lock shrinks the share pool, possibly locking more children; the
-- locked set only grows, so this fixpoints within n sweeps, and a row of n
-- children could take n. Rows settle in a few; one still locking after
-- 'waterFillAfter' sweeps is settled by 'waterFillGrow', which the next sweep
-- confirms, so no row takes more than a sort and that many sweeps.
settleGrow :: IOArr Float -> IOArr Float -> Float -> Float -> Int -> Int -> IO (Float, Float)
settleGrow mainArr crossArr avail gapSum n !pass = do
  (occupied, gfSum) <- scanGrow mainArr crossArr 0 n 0 0
  let free = avail - gapSum - occupied
  locked <- lockGrow mainArr crossArr free gfSum 0 n 0
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
  let collect !i acc
        | i >= n = pure acc
        | otherwise = do
            gf <- readPrimArray crossArr i
            if gf > 0
              then do
                need <- readPrimArray mainArr i
                collect (i + 1) ((need / gf, need, gf, i) : acc)
              else collect (i + 1) acc
      lockFrom !free !g ((_, need, gf, i) : rest)
        | need * g > gf * free = do
            writePrimArray crossArr i 0
            lockFrom (free - need) (g - gf) rest
      lockFrom _ _ _ = pure ()
  growing <- collect 0 []
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
--   row that has some, and otherwise its first child's.
-- * anything else: its bottom edge.
childBaseline :: SolveEnv -> NodeIdx -> Float -> IO Float
childBaseline env@SolveEnv {seArena = na, seArrays = a, seFm = defaultFm, seResolveFont = resolveFont} ci h = do
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
          wrapped <-
            if T.any (== '\n') raw
              then pure True
              else
                if rowChild
                  then pure False
                  else do
                    AxisSizing wTag _ _ maxW <- getWidthSizing na ci
                    (_, _, w, _) <- getRect na ci
                    effMaxW <- if maxW < 1e8 then pure maxW else findAncestorMaxW na ci
                    let cap = textWrapCap effMaxW wTag w
                    (tw, _) <- measureFontLine measurer raw
                    pure (cap < 1e8 && cap + 0.5 < tw)
          pure (textBaseline fm (if wrapped then fmLineHeight fm else h))
    _
      | hasCenteredLabel nt && not (nt == NodeButton && hasFlag buttonFlagClose si) -> do
          -- Widget labels take the node's font size in the default face
          -- ('resolveFontFor').
          size <- getNodeFontSize na ci
          let weight = textNodeFontWeight 0
              style = textNodeFontStyle 0
              variant = textNodeFontVariant 0
          fm <-
            if isDefaultNodeFont size weight style variant
              then pure defaultFm
              else fst <$> resolveFont size weight style variant
          pure (textBaseline fm h)
      | isContainerNode nt -> do
          -- Children are linked last first, so consing them up as they are
          -- visited leaves the list in child order.
          kids <- foldFlowChildrenM na ci (\acc k -> pure (k : acc)) []
          case kids of
            [] -> pure h
            first : _ -> do
              (pad, _, dir) <- containerFlow a ci
              let innerH = max 0 (h - padT pad - padB pad)
                  heightOf k = (\(_, _, _, kh) -> kh) <$> getRect na k
              grouped <-
                if dir /= DirRow
                  then pure []
                  else
                    fmap concat . forM kids $ \k -> do
                      ay <- getAlignY na k
                      if ay /= AlignBaseline then pure [] else (: []) <$> (heightOf k >>= childBaseline env k)
              (padT pad +) <$> case grouped of
                _ : _ -> pure (maximum grouped)
                [] -> do
                  fh <- heightOf first
                  ay <- if dir == DirRow then getAlignY na first else pure AlignTop
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
    (_, _, iw, ih) <- getRect na idx
    case nt of
      NodeModal -> do
        let w = min iw (max 0 (winW - 2 * windowMargin))
            h = min ih (max 0 (winH - 2 * windowMargin))
        positionNodeA env 0 idx (max 0 ((winW - w) / 2)) (max 0 ((winH - h) / 2)) w h
      NodeWindow -> do
        (w0, h0) <- fromMaybe (min iw winW, min ih winH) <$> lookupSize wid
        mpos <- lookupPos wid
        placeWindowNode na ms winW winH idx w0 h0 $ \w -> fromMaybe (winW - w - windowMargin, windowMargin) mpos
      _ -> do
        mcfg <- lookupAnchor wid
        let (anchor, placement, offset) = fromMaybe (AnchorPoint (V2 0 0), PlacementAuto, 4) mcfg
            (x, y) = computePopupPosition winW winH windowMargin iw ih anchor placement offset
        positionNodeA env 0 idx x y iw ih

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
  positionChildren env 0 idx dir gap pad x y w h

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
      let x0 = case placement of
            PlacementLeft -> px - iw - offset
            PlacementRight -> px + offset
            _ -> px
          y0 = case placement of
            PlacementAbove -> py - ih - offset
            PlacementBelow -> py + offset
            _ -> py
          x = if x0 + iw > winW - margin && px - iw - margin >= 0
                then px - iw - offset
                else clamp margin (winW - iw - margin) x0
          y = if y0 + ih > winH - margin && py - ih - margin >= 0
                then py - ih - offset
                else clampY y0
       in (x, y)
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
