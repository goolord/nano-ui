module NanoUI.Layout.Solve
  ( solveLayout
  , FontResolver
  , placeModals
  , placeWindows
  , placePopups
  , computePopupPosition
  , positionWindowNode
  , scrollBarSlotOf
  ) where

import Control.Monad (foldM, unless, when)
import Data.IORef (readIORef)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , copyMutablePrimArray
  , newPrimArray
  , readPrimArray
  , writePrimArray
  )
import Data.Primitive.Types (Prim)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import GHC.Exts (RealWorld)
import NanoUI.Font
  ( CustomMeasureFn
  , FontMetrics (..)
  , checkboxBoxSize
  , checkboxLeading
  , treeRowLeading
  , treeItemPadding
  , classifyScrollBar
  , resolveLayoutGap
  , resolveLayoutPadding
  , measureTextIO
  , lineWidthIO
  , measureTextWrappedIO
  , labelContentInset
  , tableCellInset
  , ScrollBarSlot (..)
  , widgetPadding
  , buttonPadding
  , menuItemPadX
  , menuOuterPad
  , selectPadding
  , layoutLineHeight
  , isDefaultNodeFont
  , sliderTrackHeight
  , sliderHandleDiameter
  , sliderHandleSlack
  )
import NanoUI.Layout.Arena
  ( DirTag (..)
  , FlexScratch (..)
  , NodeArena
  , NodeArenaArrays
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaArrays
  , arenaCount
  , withArenaArraysSnap
  , geomX
  , geomY
  , geomW
  , geomH
  , styleWVal
  , styleHVal
  , styleMinW
  , styleMinH
  , styleMaxW
  , styleMaxH
  , tagNodeType
  , tagWSizing
  , tagHSizing
  , readGeom
  , writeGeom
  , readStyle
  , readTagEnum
  , readTree
  , treeParent
  , getAlignX
  , getAlignY
  , getChildCount
  , getDirection
  , getFirstChild
  , getGap
  , getGridCols
  , getGridMinColW
  , getHeightSizing
  , getMinMax
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
  , forNodes_
  , foldFlowChildrenM
  , naScratch
  , naWrapMemo
  , naFitMemo
  )
import NanoUI.Id (WidgetId)
import NanoUI.Style (AlignX (..), AlignY (..), FontStyle (..), FontVariant (..), FontWeight (..), Padding (..), windowMargin)
import NanoUI.Types (PopupAnchor (..), PopupPlacement (..), Rect (..), V2 (..), clamp, onGrid)
import NanoUI.WidgetText
  ( colorPickerSvH
  , textNodeFontVariant
  , textNodeFontWeight
  , textNodeFontStyle
  , treeDecodeStyle
  , selectDisplayText
  , selectChevronReserve
  , textInputFieldHeight
  , textInputMinWidth
  , textInputPlaceholder
  , textInputSearchMode
  , textInputBareMode
  , textInputNumericMode
  , numericStepperW
  , textInputSelectableMode
  , searchFieldReserveW
  , isTableHeaderStyle
  , isMenuItemStyle
  , tableHeaderDisplayText
  )
import NanoUI.Frame.Scroll.Geometry
  ( decodeScrollConfig
  , isScrollStyle2D
  , scrollAxisGutter
  , scrollGutters2D
  , scrollPolicyX
  , scrollPolicyY
  )

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
  }

-- | Env for placing floating nodes after the solve: every text node uses the
-- default font, and there is no custom measurement.
floatingEnv :: NodeArena -> FontMetrics -> IO SolveEnv
floatingEnv na fm = do
  a <- arenaArrays na
  let measure = measureTextIO fm
  pure (SolveEnv na a fm fm measure (\_ _ _ _ -> pure (fm, measure)) (const (pure Nothing)))

-- | Strict accumulator for flow-child folds: a child count and two running
-- sums or extents. The strict fields keep the folds unboxed.
data FlowAcc = FlowAcc !Int !Float !Float

-- Keep font selection and single-line/wrapped measurement together so every
-- layout pass uses the same policy. Monospaced text uses its metrics directly;
-- proportional text uses the host's shaping-aware measurement callback.
data TextMeasurer = TextMeasurer
  { tmMetrics :: !FontMetrics
  , tmVariant :: !FontVariant
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
  pure (TextMeasurer metrics variant measureLine)

-- Keep these operations as inline functions rather than allocating two
-- closures for every resolved node, including nodes that never wrap.
{-# INLINE measureFontLine #-}
measureFontLine :: TextMeasurer -> Text -> IO (Float, Float)
measureFontLine TextMeasurer {tmMetrics = metrics, tmVariant = variant, tmHostLine = hostLine} text
  | variant == FontMono = measureTextIO metrics text
  | otherwise = hostLine text

{-# INLINE measureFontWrapped #-}
measureFontWrapped :: TextMeasurer -> Text -> Float -> IO (Float, Float)
measureFontWrapped TextMeasurer {tmMetrics = metrics, tmVariant = variant, tmHostLine = hostLine} text width
  | variant == FontMono = measureTextWrappedIO (lineWidthIO metrics) metrics text width
  | otherwise = measureTextWrappedIO (fmap fst . hostLine) metrics text width

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
  let (ix, _) = labelContentInset textFm
      wrapW = max 0 (outerW - 2 * ix)
      lineH = layoutLineHeight textFm
      na = seArena env
  if T.any (== '\n') txt || shouldWrap wrapW tw0
    then do
      (tw, th) <- memoizeWidth na (naWrapMemo na) idx wrapW (measureFontWrapped measurer txt wrapW)
      pure (TextBox True tw th lineH)
    else pure (TextBox False tw0 th0 lineH)

-- | Wrap policy once a width is assigned: wrap when allowed and the single
-- line overflows a positive wrap width.
{-# INLINE wrapsNarrower #-}
wrapsNarrower :: Bool -> Float -> Float -> Bool
wrapsNarrower allowed wrapW lineW = allowed && wrapW + 0.5 < lineW && wrapW > 0

solveLayout ::
  NodeArena ->
  FontMetrics ->
  FontMetrics ->
  (Text -> IO (Float, Float)) ->
  FontResolver ->
  (WidgetId -> IO (Maybe CustomMeasureFn)) ->
  Float ->
  Float ->
  IO ()
solveLayout na fm monoFm measure resolveFont lookupMeasure rootW rootH =
  withArenaArraysSnap na $ do
    a <- arenaArrays na
    count <- arenaCount na
    when (count > 0) $ do
      let env = SolveEnv na a fm monoFm measure resolveFont lookupMeasure
      measurePass env count
      positionNodeA env 0 0 0 0 rootW rootH
      quantizeResultsA a count (fmSnapScale fm)

quantizeResultsA :: NodeArenaArrays -> Int -> Float -> IO ()
quantizeResultsA a count s
  | s <= 0 = pure ()
  | otherwise = do
      -- A floating node (modal, window, popup) and everything inside it is
      -- laid out by placement after the solve, which sizes the subtree from
      -- these measured sizes. Rounding them here would size a dialog and its
      -- content-sized parts off their content, so the subtree keeps them;
      -- placement overwrites its geometry anyway. A parent always precedes
      -- its children, so one pass marks each node from its parent.
      floating <- newPrimArray count :: IO (MutablePrimArray RealWorld Word8)
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
                unless inFloating $ do
                  x <- readGeom a i geomX
                  y <- readGeom a i geomY
                  w <- readGeom a i geomW
                  h <- readGeom a i geomH
                  writeGeom a i geomX (onGrid s x)
                  writeGeom a i geomY (onGrid s y)
                  writeGeom a i geomW (max 0 (onGrid s w))
                  writeGeom a i geomH (max 0 (onGrid s h))
                go (i + 1)
      go 0

measurePass :: SolveEnv -> Int -> IO ()
measurePass env count = do
  let go !idx
        | idx < 0 = pure ()
        | otherwise = do
            measureNode env idx
            go (idx - 1)
  go (count - 1)

measureNode :: SolveEnv -> NodeIdx -> IO ()
measureNode env@SolveEnv {seArena = na, seFm = fm} idx = do
  nt <- readTagEnum (seArrays env) idx tagNodeType
  case nt of
    NodeText -> measureTextNode env idx
    NodeSpacer -> measureSpacer na idx
    NodeSeparator -> measureSeparator na idx
    NodeScrollContainer -> measureScrollContainer na fm idx
    NodeImage -> measureImage na idx
    NodeBox -> measureImage na idx
    NodeDrawing -> do
      wid <- getWidgetId na idx
      mFn <- seLookupMeasure env wid
      case mFn of
        Just fn -> measureCustomNode na fm fn idx
        Nothing -> measureImage na idx
    _
      | isContainerNode nt -> do
          measureContainer env idx
          when (nt == NodeModal) $ setNodeValue na idx 0
      | otherwise -> measureWidget env idx

measureCustomNode ::
  NodeArena ->
  FontMetrics ->
  CustomMeasureFn ->
  NodeIdx ->
  IO ()
measureCustomNode na fm measureFn idx = do
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  let availW = case wTag of SizingFixed -> wVal; _ -> if maxW < 1e8 then maxW else 1e9
      availH = case hTag of SizingFixed -> hVal; _ -> if maxH < 1e8 then maxH else 1e9
      (mw, mh) = measureFn fm (availW, availH)
      w = case wTag of SizingFixed -> wVal; _ -> clamp minW maxW mw
      h = case hTag of SizingFixed -> hVal; _ -> clamp minH maxH mh
  setRect na idx 0 0 w h

findAncestorMaxW :: NodeArena -> NodeIdx -> IO Float
findAncestorMaxW na idx = go idx 0
  where
    go cur !padAccum = do
      p <- getParent na cur
      if p < 0
        then pure 1e9
        else do
          pad <- getPadding na p
          let padW = padL pad + padR pad
              padAccum' = padAccum + padW
          (_, _, pMaxW, _) <- getMinMax na p
          (pwTag, pwVal) <- getWidthSizing na p
          if pwTag == SizingFixed
            then pure (max 0 (pwVal - padAccum'))
            else if pMaxW < 1e8
              then pure (max 0 (pMaxW - padAccum'))
              else go p padAccum'

measureTextNode :: SolveEnv -> NodeIdx -> IO ()
measureTextNode env@SolveEnv {seArena = na} idx = do
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, _) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
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
  setRect na idx 0 0 reportedW $
    case hTag of
      SizingFixed -> clamp minH maxH hVal
      _ -> clamp minH maxH (max lineH th)

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
          (pwTag, _) <- getWidthSizing na p
          if pwTag == SizingGrow
            then getParent na p >>= go False
            else
              if isParent
                then pure False
                else do
                  nt <- getNodeType na p
                  pure (nt /= NodeModal)

measureImage :: NodeArena -> NodeIdx -> IO ()
measureImage na idx = do
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  let w =
        case wTag of
          SizingFixed -> wVal
          _ -> if minW > 0 then minW else 32
      h =
        case hTag of
          SizingFixed -> hVal
          _ -> if minH > 0 then minH else 32
  setRect na idx 0 0 (clamp minW maxW w) (clamp minH maxH h)

measureSpacer :: NodeArena -> NodeIdx -> IO ()
measureSpacer na idx = do
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
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

measureTextField ::
  FontMetrics ->
  (Text -> IO (Float, Float)) ->
  Text ->
  Bool ->
  IO (Float, Float, Float, Float)
measureTextField fm measure txt multiline = do
  pw <- if multiline || T.null txt then pure 0 else fst <$> measure (textInputPlaceholder txt)
  let fieldH = if multiline then max 96 (textInputFieldHeight fm * 4) else textInputFieldHeight fm
      contentW = max textInputMinWidth pw
  pure (contentW, fieldH, 0, 0)

-- Caption-less search box: single row tall, icons counted in the width budget.
measureSearchField ::
  FontMetrics ->
  (Text -> IO (Float, Float)) ->
  Text ->
  IO (Float, Float, Float, Float)
measureSearchField fm measure txt = do
  let lbl = if T.null txt then " " else txt
  (lw, _) <- measure lbl
  let contentW = max textInputMinWidth lw + searchFieldReserveW fm
  pure (contentW, textInputFieldHeight fm, 0, 0)

measureWidget :: SolveEnv -> NodeIdx -> IO ()
measureWidget env@SolveEnv {seArena = na, seFm = fm, seMeasure = measure} idx = do
  nt <- getNodeType na idx
  txt <- getText na idx
  si <- getStyleIdx na idx
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  let (padX, padY) =
        case nt of
          NodeButton
            | isTableHeaderStyle si ->
                let (cx, cy) = tableCellInset fm
                 in (2 * cx, 2 * cy)
            -- Menu rows reserve the same gutter the text-field context menu
            -- paints (outer pad + item pad on each side of the label), so the
            -- generic popup panel sizes identically.
            | isMenuItemStyle si ->
                (2 * (menuOuterPad + menuItemPadX), snd (buttonPadding fm))
            | otherwise -> buttonPadding fm
          NodeSelect -> selectPadding fm
          NodeTree -> treeItemPadding fm
          NodeTextInput
            | textInputSelectableMode si -> (0, 0)
          _
            | nt == NodeColorPicker
                || nt == NodeSlider
                || nt == NodeCheckbox
                || nt == NodeRadio
                || nt == NodeTextInput
                || nt == NodeTextArea ->
                let (cx, cy) = labelContentInset fm
                 in (2 * cx, cy)
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
        | textInputSelectableMode si -> do
            -- Size with the node's own font (paint and span placement resolve
            -- it too); the ambient `measure` is the default font only.
            measurer <- textNodeMeasurer env idx
            (mw, mh) <- measureFontLine measurer (if T.null txt then " " else txt)
            pure (mw, mh, 0, 0)
        -- Numeric field: a short editable box and its stepper.
        | textInputNumericMode si ->
            pure (56, textInputFieldHeight fm, numericStepperW, 0)
        -- Bare field: just the editable box (no caption, no icon chrome).
        | textInputBareMode si ->
            pure (24, textInputFieldHeight fm, 0, 0)
        | textInputSearchMode si ->
            measureSearchField fm measure txt
        | otherwise -> measureTextField fm measure txt False
      NodeTextArea -> measureTextField fm measure txt True
      _
        | nt == NodeCheckbox || nt == NodeRadio ->
            measureMarkedWidget fm measure txt (checkboxLeading fm)
        | otherwise -> do
            body <-
              if T.null txt
                then pure " "
                else
                  if isTableHeaderStyle si
                    then pure (tableHeaderDisplayText si txt)
                    else pure txt
            (mw, mh) <- measure body
            pure (mw, mh, 0, 0)
  let rawW = tw + padX + extraW
      rawH = th + padY + extraH
      w = case wTag of SizingFixed -> wVal; _ -> clamp minW maxW rawW
      h = case hTag of SizingFixed -> hVal; _ -> clamp minH maxH rawH
  setRect na idx 0 0 w h

measureContainer :: SolveEnv -> NodeIdx -> IO ()
measureContainer env@SolveEnv {seArena = na, seFm = fm} idx = do
  pad0 <- getPadding na idx
  gap0 <- getGap na idx
  let pad = resolveLayoutPadding fm pad0
      gap = resolveLayoutGap fm gap0
  dir <- getDirection na idx
  gCols <- getGridCols na idx
  minColW <- getGridMinColW na idx
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  nt <- getNodeType na idx
  let chrome = isChromeColumn nt dir
      padX = padL pad + padR pad
      padY = padT pad + padB pad
      innerMaxW =
        case wTag of
          SizingFixed -> max 0 (wVal - padX)
          _ -> max 0 (maxW - padX)
      innerAvailH =
        case hTag of
          SizingFixed -> max 0 (hVal - padY)
          _ -> max 0 (maxH - padY)
  (contentW, contentH) <-
    if gCols > 0 || minColW > 0
      then measureGridScratch env idx gCols minColW innerMaxW innerAvailH gap
      else if dir == DirColumn && chrome
        then do
          n <- loadChildrenScratch (seArena env) idx (flowChildSize env False innerMaxW innerAvailH)
          foldChromeColumnScratch na n gap
        else foldChildDimsFromParent na idx dir gap
  let w =
        case wTag of
          SizingFixed -> clamp minW maxW wVal
          _ -> clamp minW maxW (contentW + padL pad + padR pad)
      h =
        case hTag of
          SizingFixed -> clamp minH maxH hVal
          _ -> clamp minH maxH (contentH + padT pad + padB pad)
  setRect na idx 0 0 w h

measureScrollContainer :: NodeArena -> FontMetrics -> NodeIdx -> IO ()
measureScrollContainer na fm idx = do
  pad0 <- getPadding na idx
  gap0 <- getGap na idx
  let pad = resolveLayoutPadding fm pad0
      gap = resolveLayoutGap fm gap0
      padX = padL pad + padR pad
      padY = padT pad + padB pad
  dir <- getDirection na idx
  si <- getStyleIdx na idx
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  (contentW, contentH) <- foldChildDimsFromParent na idx dir gap
  slot <- scrollBarSlotOf na idx
  let fullW = contentW + padX
      fullH = contentH + padT pad + padB pad
      assignedInnerH =
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
      viewportW =
        case wTag of
          SizingFixed -> wVal
          _ -> fullW + fitGutterW
      viewportH =
        case hTag of
          SizingFixed -> hVal
          _ -> fullH
  if isScrollStyle2D si
    then do
      setNodeValue na idx contentH
      setScrollContentW na idx contentW
    else setNodeValue na idx (case dir of DirColumn -> contentH; DirRow -> contentW)
  setRect na idx 0 0 (clamp minW maxW viewportW) (clamp minH maxH viewportH)

foldChildDimsFromParent :: NodeArena -> NodeIdx -> DirTag -> Float -> IO (Float, Float)
foldChildDimsFromParent na idx dir gap = do
  FlowAcc count main cross <- foldFlowChildrenM na idx step (FlowAcc 0 0 0)
  pure
    ( case dir of
        DirRow ->
          ( main + gap * fromIntegral (max 0 (count - 1))
          , if count <= 0 then 0 else cross
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
gridRowHeight :: MutablePrimArray RealWorld Float -> Int -> Int -> Int -> IO Float
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
  (_, h) <- memoizeWidth na (naFitMemo na) idx availW ((,) 0 <$> recomputeFitHeightAtWidthGo env idx availW)
  pure h

recomputeFitHeightAtWidthGo :: SolveEnv -> NodeIdx -> Float -> IO Float
recomputeFitHeightAtWidthGo env@SolveEnv {seArena = na, seFm = fm, seLookupMeasure = lookupMeasure} idx availW = do
  nt <- getNodeType na idx
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, _) <- getHeightSizing na idx
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
      | hTag == SizingFit -> do
          wid <- getWidgetId na idx
          lookupMeasure wid >>= \case
            Just measure -> pure (clamp minH maxH (snd (measure fm (effW', if maxH < 1e8 then maxH else 1e9))))
            Nothing -> pure oldH
      | otherwise -> pure oldH

    _ | (nt == NodeContainer || nt == NodePanel), hTag /= SizingFixed -> do
          dir <- getDirection na idx
          if dir == DirRow
            then pure oldH
            else do
              pad0 <- getPadding na idx
              gap0 <- getGap na idx
              let pad = resolveLayoutPadding fm pad0
                  gap = resolveLayoutGap fm gap0
                  innerW = max 0 (effW' - padL pad - padR pad)
                  step (FlowAcc count contentH _) ci = do
                    (subWTag, subWVal) <- getWidthSizing na ci
                    (_, _, subMaxW, _) <- getMinMax na ci
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
  let write !i ci = do
        (w, h) <- sizeOf ci
        writePrimArray idxArr i ci
        writePrimArray wArr i w
        writePrimArray hArr i h
        pure (i + 1)
  n <- foldFlowChildrenM na parent write 0
  reverseScratchTriple idxArr wArr hArr 0 (n - 1)
  pure n

-- | Scratch size of a flow child: its measured box, with percent sizing
-- resolved against the parent's inner box. With @refit@, a fit-height child
-- that the parent narrows (grow or percent width, or wider than @availW@) is
-- re-measured at the assigned width.
flowChildSize :: SolveEnv -> Bool -> Float -> Float -> NodeIdx -> IO (Float, Float)
flowChildSize env refit availW availH ci = do
  let na = seArena env
  (_, _, w, h) <- getRect na ci
  (wTag, wVal) <- getWidthSizing na ci
  (hTag, hVal) <- getHeightSizing na ci
  (minW, minH, maxW, maxH) <- getMinMax na ci
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
positionNodeA env@SolveEnv {seArena = na, seArrays = a, seFm = fm, seLookupMeasure = lookupMeasure} depth idx x y availW availH = do
  minW <- readStyle a idx styleMinW
  minH <- readStyle a idx styleMinH
  maxW <- readStyle a idx styleMaxW
  maxH <- readStyle a idx styleMaxH
  wTag <- readTagEnum a idx tagWSizing
  wVal <- readStyle a idx styleWVal
  hTag <- readTagEnum a idx tagHSizing
  hVal <- readStyle a idx styleHVal
  intrinsicW <- readGeom a idx geomW
  intrinsicH <- readGeom a idx geomH
  nt <- readTagEnum a idx tagNodeType
  let w = clamp minW maxW (resolveSize wTag wVal intrinsicW availW minW maxW)
  isRowChild <- parentIsRow na idx
  h <-
    if nt == NodeText && hTag /= SizingFixed && not isRowChild
      then do
        txt <- getText na idx
        if T.null txt
          then pure (clamp minH maxH 0)
          else do
            TextBox {tbWrapped, tbH, tbLineH} <-
              measureTextNodeAt env idx txt w (wrapsNarrower (wTag /= SizingFit))
            pure . clamp minH maxH $
              if tbWrapped
                then max tbLineH tbH
                else resolveSize hTag hVal intrinsicH availH minH maxH
      else
        if (nt == NodeContainer || nt == NodePanel) && hTag == SizingFit
          then pure (clamp minH maxH (max intrinsicH availH))
          else
            if nt == NodeDrawing && hTag == SizingFit && w /= intrinsicW
              then do
                -- A measured drawing laid out at another width than it was
                -- measured at takes its height at the width it got.
                wid <- getWidgetId na idx
                lookupMeasure wid >>= \case
                  Just measure -> pure (clamp minH maxH (snd (measure fm (w, if maxH < 1e8 then maxH else 1e9))))
                  Nothing -> pure (clamp minH maxH (resolveSize hTag hVal intrinsicH availH minH maxH))
              else pure (clamp minH maxH (resolveSize hTag hVal intrinsicH availH minH maxH))
  setRect na idx x y w h
  when (isContainerNode nt) $ do
    (pad, gap, dir) <- containerFlow na fm idx
    if isScrollNode nt
      then positionScrollChildren env depth idx dir gap pad x y w h
      else positionChildren env depth idx dir gap pad x y w h
  when (hTag == SizingFit && isContainerNode nt && not (isScrollNode nt)) $
    adjustFitHeight na fm idx minH maxH x y w

-- | A container's resolved padding and gap, and its direction.
{-# INLINE containerFlow #-}
containerFlow :: NodeArena -> FontMetrics -> NodeIdx -> IO (Padding, Float, DirTag)
containerFlow na fm idx = do
  pad0 <- getPadding na idx
  gap0 <- getGap na idx
  dir <- getDirection na idx
  pure (resolveLayoutPadding fm pad0, resolveLayoutGap fm gap0, dir)

adjustFitHeight :: NodeArena -> FontMetrics -> NodeIdx -> Float -> Float -> Float -> Float -> Float -> IO ()
adjustFitHeight na fm idx minH maxH x y w = do
  fc <- getFirstChild na idx
  when (fc >= 0) $ do
    pad0 <- getPadding na idx
    let pad = resolveLayoutPadding fm pad0
        step maxB ci = do
          (_, subY, _, subH) <- getRect na ci
          pure (max maxB (subY + subH))
        -- Rounding a child's origin to the nearest device pixel can put its
        -- bottom up to half a pixel below where measurement did. That is not
        -- content outgrowing the measurement: growing for it adds half a
        -- pixel at every nested content-sized level, until a dialog sized to
        -- its content overflows its own scroll viewport. The small epsilon
        -- absorbs float error in the rounding.
        s = fmSnapScale fm
        snapSlack = if s > 0 then 0.5 / s + 1.0e-3 else 0
    maxB <- foldFlowChildrenM na idx step y
    let fitH = clamp minH maxH (maxB + padB pad - y)
    (_, _, _, curH) <- getRect na idx
    when (fitH > curH + snapSlack) $
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
  if isScrollStyle2D si
    then do
      contentW <- getScrollContentW na idx
      let cfg = decodeScrollConfig si
          (gutterW, gutterH) = scrollGutters2D slot cfg pad contentW contentSize innerW innerH
          viewW = max 0 (innerW - gutterW)
          viewH = max 0 (innerH - gutterH)
          -- Keep measured content. Shrinking to the clip wraps table columns.
          layoutW = max contentW viewW
          layoutH = max contentSize viewH
      -- cx/cy and the layout box are already inside the padding.
      positionChildren env depth idx DirColumn gap (Padding 0 0 0 0) cx cy layoutW layoutH
    else do
      let cfg = decodeScrollConfig si
          gutterCol = scrollAxisGutter (scrollPolicyY cfg) slot (padR pad) contentSize innerH
          gutterRow = scrollAxisGutter (scrollPolicyX cfg) slot (padB pad) contentSize innerW
      case dir of
        DirRow -> do
          (wTag, _) <- getWidthSizing na idx
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
    -- The trailing padding is deliberately excluded here too (so it cannot
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

scrollBarSlotOf :: NodeArena -> NodeIdx -> IO ScrollBarSlot
scrollBarSlotOf na idx = do
  nt <- getNodeType na idx
  if nt == NodeTextArea
    then pure ScrollBarList
    else do
      parent <- getParent na idx
      -- A modal's body scrolls like a window's: its bar sits just inside the
      -- panel's edge, out in the panel padding.
      isWin <-
        if parent < 0
          then pure False
          else do
            pnt <- getNodeType na parent
            pure (pnt == NodeWindow || pnt == NodeModal)
      (wTag, _) <- getWidthSizing na idx
      (hTag, _) <- getHeightSizing na idx
      inPanel <- hasPanelAncestor na parent
      let isPage = wTag == SizingGrow && hTag == SizingGrow && not inPanel
      pure (classifyScrollBar isWin isPage)

hasPanelAncestor :: NodeArena -> NodeIdx -> IO Bool
hasPanelAncestor na = go
  where
    go p
      | p < 0 = pure False
      | otherwise = do
          nt <- getNodeType na p
          case nt of
            NodePanel -> pure True
            NodeWindow -> pure False
            NodeModal -> pure False
            NodePopup -> pure False
            _ -> getParent na p >>= go

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
              (_, _, iw, _) <- getRect na ci
              ax <- getAlignX na ci
              (wTag, _) <- getWidthSizing na ci
              let cw = innerW
                  -- Grow/Percent already take full width. AlignX is for text, not for
                  -- shifting a full-width box (that would draw past the column).
                  fx =
                    if wTag == SizingGrow || wTag == SizingPercent
                      then cx
                      else alignX ax cx cw iw
                  visibleSlice = max 0 (innerH - (curY - cy))
                  nodeH =
                    if isScrollNode nt
                      then min fh visibleSlice
                      else fh
              positionNodeA env (depth + 1) ci fx curY cw nodeH
              (_, _, _, placedH) <- getRect na ci
              go (i + 1) (curY + placedH + gap)
    go 0 cy

{-# INLINE resolveSize #-}
resolveSize :: SizingTag -> Float -> Float -> Float -> Float -> Float -> Float
resolveSize SizingFixed v _ _ _ _ = v
resolveSize SizingFit _ intrinsic avail minS maxS = clamp minS maxS (min intrinsic avail)
resolveSize SizingShrink _ intrinsic avail minS maxS = clamp minS maxS (min intrinsic avail)
resolveSize SizingGrow _ _ avail _ maxS = min avail maxS
resolveSize SizingPercent _ _ avail _ maxS = min avail maxS

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
  (hTag, hVal) <- getHeightSizing na ci
  (_, _, _, intrinsic) <- getRect na ci
  (_, minH, _, maxH) <- getMinMax na ci
  let resolved = clamp minH maxH (resolveSize hTag hVal intrinsic availCross minH maxH)
  case hTag of
    SizingFixed -> pure (clamp minH maxH hVal)
    SizingGrow -> pure resolved
    SizingPercent -> pure resolved
    _ ->
      -- Fit/Shrink keep the measured box. Do not use the wrap-line
      -- or row slot as availH: that stretches every child when leftover
      -- leaks into scratch `fh`.
      pure (max minH intrinsic)

-- Column leftover must not change Fixed step height.
columnChildHeight :: NodeArena -> NodeIdx -> Float -> IO Float
columnChildHeight na ci scratchH = do
  (hTag, _) <- getHeightSizing na ci
  case hTag of
    SizingFixed -> do
      (_, minH, _, maxH) <- getMinMax na ci
      (_, _, _, ih) <- getRect na ci
      pure (clamp minH maxH ih)
    _ -> do
      (_, minH, _, maxH) <- getMinMax na ci
      pure (clamp minH maxH scratchH)

{-# INLINE withAxisSnaps #-}
withAxisSnaps ::
  NodeArena ->
  Int ->
  Int ->
  Float ->
  Float ->
  Bool ->
  (MutablePrimArray RealWorld Int -> MutablePrimArray RealWorld Float -> IO a) ->
  IO a
withAxisSnaps na depth n availMain gapSum horizontal act = do
  distributeScratch na n availMain gapSum horizontal
  FlexScratch {fsIdx = idxArr, fsOutW = outW, fsOutH = outH} <- readIORef (naScratch na)
  let outArr = if horizontal then outW else outH
  AxisSnapshot idxSnap outSnap <- ensureAxisSnapshot na depth n
  copyMutablePrimArray idxSnap 0 idxArr 0 n
  copyMutablePrimArray outSnap 0 outArr 0 n
  act idxSnap outSnap

-- | Like 'withAxisSnaps' but snapshots the unscaled child cross sizes instead
-- of the distributed main-axis result. Grids compute rows from the measured
-- child heights, so freezing them lets the recursion reuse the working scratch.
withGridScratch :: NodeArena -> Int -> Int -> (MutablePrimArray RealWorld Int -> MutablePrimArray RealWorld Float -> IO a) -> IO a
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
positionRowFromParent env@SolveEnv {seArena = na, seFm = fm} depth parent gap cx cy cw ch = do
  n <- loadChildrenScratch (seArena env) parent (flowChildSize env False cw ch)
  withAxisSnaps na depth n cw (gap * fromIntegral (max 0 (n - 1))) True $ \idxSnap outSnap -> do
    let s = fmSnapScale fm
        step = if s > 0 then 1 / s else 0
        originOf cur prev =
          -- Preserve the exact flex positions: accumulate the cursor in raw
          -- floats and snap only the placed origin, never the running sum.
          -- Rounding the cumulative cursor re-compounds error every child
          -- (1.667 -> 2.0 -> ...) so a shrink row overruns its fixed width.
          -- The max/step floor keeps two siblings from quantizing to the same
          -- pixel origin while resisting that drift.
          if s > 0
            then max (onGrid s cur) (prev + step)
            else cur
        goRow !i !cur !prev
          | i >= n = pure ()
          | otherwise = do
              ci <- readPrimArray idxSnap i
              fw <- readPrimArray outSnap i
              let x = originOf cur prev
              -- Fit/fixed children keep content height. Only Grow/Percent eat `ch`.
              crossH <- childRowCrossSize na ci ch
              ay <- getAlignY na ci
              let fy = alignY ay cy ch crossH
              positionNodeA env (depth + 1) ci x fy fw crossH
              -- A grow child that its max width stopped short of its share
              -- hands the rest to the siblings after it instead of leaving a
              -- hole.
              placedW <- readGeom (seArrays env) ci geomW
              goRow (i + 1) (cur + min fw placedW + gap) x
    goRow 0 cx (if s > 0 then onGrid s cx - step else cx)

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
                                (minW, minH, maxW, maxH) <- getMinMax na ci
                                (wTag, wVal) <- getWidthSizing na ci
                                (hTag, hVal) <- getHeightSizing na ci
                                (_, _, iw, ih) <- getRect na ci
                                let childW = clamp minW maxW (resolveSize wTag wVal iw colW minW maxW)
                                    childH = clamp minH maxH (resolveSize hTag hVal ih rowH minH maxH)
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
positionColumnFromParent env@SolveEnv {seArena = na, seFm = fm} depth parent gap chrome px pw cx cy cw ch = do
  n <- loadChildrenScratch (seArena env) parent (flowChildSize env True cw ch)
  gapSum <- columnGapSumScratch na chrome n gap
  withAxisSnaps na depth n ch gapSum False $ \idxSnap outSnap -> do
    let s = fmSnapScale fm
        step = if s > 0 then 1 / s else 0
        originOf cur prev =
          -- Mirror the row pass: accumulate the raw cursor, snap only at
          -- placement, and floor-progress on the grid so flex sizes hold and
          -- no two siblings quantize to the same pixel origin.
          if s > 0
            then max (onGrid s cur) (prev + step)
            else cur
        go !i !cur !prev
          | i >= n = pure ()
          | otherwise = do
              ci <- readPrimArray idxSnap i
              fh <- readPrimArray outSnap i
              let y = originOf cur prev
              nt <- getNodeType na ci
              (fx, nodeW) <-
                if chrome && nt == NodeSeparator
                  then pure (px, pw)
                  else do
                    (_, _, iw, _) <- getRect na ci
                    ax <- getAlignX na ci
                    (wTag, _) <- getWidthSizing na ci
                    if wTag == SizingGrow || wTag == SizingPercent
                      then pure (cx, cw)
                      else pure (alignX ax cx cw iw, cw)
              childH <- columnChildHeight na ci fh
              positionNodeA env (depth + 1) ci fx y nodeW childH
              (_, _, _, placedH) <- getRect na ci
              gapAfter <-
                if i + 1 >= n
                  then pure 0
                  else do
                    nextCi <- readPrimArray idxSnap (i + 1)
                    pairColumnGap na chrome nextCi gap
              go (i + 1) (cur + placedH + gapAfter) y
    go 0 cy (if s > 0 then onGrid s cy - step else cy)


{-# INLINE reverseScratchTriple #-}
reverseScratchTriple ::
  MutablePrimArray RealWorld Int ->
  MutablePrimArray RealWorld Float ->
  MutablePrimArray RealWorld Float ->
  Int ->
  Int ->
  IO ()
reverseScratchTriple idxArr mainArr crossArr lo hi = do
  let go !a !b
        | a >= b = pure ()
        | otherwise = do
            swapPrim idxArr a b
            swapPrim mainArr a b
            swapPrim crossArr a b
            go (a + 1) (b - 1)
  go lo hi

{-# INLINE swapPrim #-}
swapPrim :: (Prim a) => MutablePrimArray RealWorld a -> Int -> Int -> IO ()
swapPrim arr a b = do
  x <- readPrimArray arr a
  y <- readPrimArray arr b
  writePrimArray arr a y
  writePrimArray arr b x
{-# SPECIALIZE swapPrim :: MutablePrimArray RealWorld Int -> Int -> Int -> IO () #-}
{-# SPECIALIZE swapPrim :: MutablePrimArray RealWorld Float -> Int -> Int -> IO () #-}

columnGapSumScratch :: NodeArena -> Bool -> Int -> Float -> IO Float
columnGapSumScratch _ False _ _ = pure 0
columnGapSumScratch _ True n _
  | n <= 1 = pure 0
columnGapSumScratch na True n gap = do
  FlexScratch {fsIdx = idxArr} <- readIORef (naScratch na)
  let go !i !acc
        | i >= n - 1 = pure acc
        | otherwise = do
            b <- readPrimArray idxArr (i + 1)
            g <- pairColumnGap na True b gap
            go (i + 1) (acc + g)
  go 0 0

-- | Resolve the main-axis sizes of the first @n@ scratch children.
distributeScratch :: NodeArena -> Int -> Float -> Float -> Bool -> IO ()
distributeScratch na n avail gapSum horizontal = do
  FlexScratch {fsIdx = idxArr, fsW = wArr, fsH = hArr, fsOutW = outW, fsOutH = outH} <- readIORef (naScratch na)
  let off = 0
      end = n
  total <- sumScratchAxis wArr hArr horizontal off end 0
  let slack = avail - (total + gapSum)
  if slack > 0.001
    then do
      growTotal <- sumFactors growFactor na idxArr horizontal off end
      if growTotal <= 0
        then copyScratchRange wArr hArr outW outH off end
        else do
          -- Grow children share the free space by factor, but no child is
          -- squeezed below its content size (a min-content floor, like CSS
          -- flex with min-width:auto): two fillW columns come out equal unless
          -- one column's content needs more, and that one then takes exactly
          -- what it needs while the rest re-share what is left.
          --
          -- Grow factors live in the cross output (0 once a child is not or
          -- no longer growing): withAxisSnaps only consumes the main-axis
          -- array, so it is free scratch here and is restored to real cross
          -- sizes before returning. mainArr keeps the exact content size
          -- throughout; no arithmetic on markers.
          let mainArr = if horizontal then outW else outH
              crossArr = if horizontal then outH else outW
          markGrowFlags na idxArr wArr hArr mainArr crossArr horizontal off end
          (free, gfSum) <- settleGrow mainArr crossArr avail gapSum off end (n + 1)
          applyGrowShares wArr hArr mainArr crossArr horizontal free gfSum off end
    else
      if slack < -0.001
        then do
          shrinkTotal <- sumFactors shrinkFactor na idxArr horizontal off end
          if shrinkTotal <= 0
            then copyScratchRange wArr hArr outW outH off end
            else applyShrink na idxArr wArr hArr outW outH horizontal (negate slack) shrinkTotal off end
        else copyScratchRange wArr hArr outW outH off end

-- | @out[i] = (w[i], h[i])@ for the range.
copyScratchRange :: MutablePrimArray RealWorld Float -> MutablePrimArray RealWorld Float -> MutablePrimArray RealWorld Float -> MutablePrimArray RealWorld Float -> Int -> Int -> IO ()
{-# INLINE copyScratchRange #-}
copyScratchRange wArr hArr outW outH !i !end
  | i >= end = pure ()
  | otherwise = do
      w <- readPrimArray wArr i
      h <- readPrimArray hArr i
      writePrimArray outW i w
      writePrimArray outH i h
      copyScratchRange wArr hArr outW outH (i + 1) end

{-# INLINE sumScratchAxis #-}
sumScratchAxis :: MutablePrimArray RealWorld Float -> MutablePrimArray RealWorld Float -> Bool -> Int -> Int -> Float -> IO Float
sumScratchAxis wArr hArr horizontal !i !end !acc
  | i >= end = pure acc
  | otherwise = do
      v <- if horizontal then readPrimArray wArr i else readPrimArray hArr i
      sumScratchAxis wArr hArr horizontal (i + 1) end (acc + v)

-- | Sizing along the main axis: width when @horizontal@, else height.
{-# INLINE getAxisSizing #-}
getAxisSizing :: NodeArena -> NodeIdx -> Bool -> IO (SizingTag, Float)
getAxisSizing na idx horizontal =
  if horizontal then getWidthSizing na idx else getHeightSizing na idx

-- | Sum a sizing-derived flex factor over the scratch children in @[i, end)@.
{-# INLINE sumFactors #-}
sumFactors :: (SizingTag -> Float -> Float) -> NodeArena -> MutablePrimArray RealWorld Int -> Bool -> Int -> Int -> IO Float
sumFactors factor na idxArr horizontal start end = go start 0
  where
    go !i !acc
      | i >= end = pure acc
      | otherwise = do
          ci <- readPrimArray idxArr i
          (tag, val) <- getAxisSizing na ci horizontal
          go (i + 1) (acc + factor tag val)

{-# INLINE growFactor #-}
growFactor :: SizingTag -> Float -> Float
growFactor tag val = if tag == SizingGrow then val else 0

{-# INLINE shrinkFactor #-}
shrinkFactor :: SizingTag -> Float -> Float
shrinkFactor tag val =
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
    SizingFit -> 0
    _ -> 0

markGrowFlags :: NodeArena -> MutablePrimArray RealWorld Int -> MutablePrimArray RealWorld Float -> MutablePrimArray RealWorld Float -> MutablePrimArray RealWorld Float -> MutablePrimArray RealWorld Float -> Bool -> Int -> Int -> IO ()
markGrowFlags na idxArr wArr hArr mainArr crossArr horizontal !i !end
  | i >= end = pure ()
  | otherwise = do
      ci <- readPrimArray idxArr i
      iw <- readPrimArray wArr i
      ih <- readPrimArray hArr i
      (tag, val) <- getAxisSizing na ci horizontal
      let gf = growFactor tag val
      writePrimArray mainArr i (if horizontal then iw else ih)
      writePrimArray crossArr i (if gf > 0 then gf else 0)
      markGrowFlags na idxArr wArr hArr mainArr crossArr horizontal (i + 1) end

-- One sweep: sum content of non-grow + already-locked children (factor 0) and
-- grow factors of the still-unlocked.
{-# INLINE scanGrow #-}
scanGrow :: MutablePrimArray RealWorld Float -> MutablePrimArray RealWorld Float -> Int -> Int -> Float -> Float -> IO (Float, Float)
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
lockGrow :: MutablePrimArray RealWorld Float -> MutablePrimArray RealWorld Float -> Float -> Float -> Int -> Int -> Int -> IO Int
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
-- locked set only grows, so this fixpoints within n sweeps.
settleGrow :: MutablePrimArray RealWorld Float -> MutablePrimArray RealWorld Float -> Float -> Float -> Int -> Int -> Int -> IO (Float, Float)
settleGrow mainArr crossArr avail gapSum off end !passes = do
  (occupied, gfSum) <- scanGrow mainArr crossArr off end 0 0
  let free = avail - gapSum - occupied
  locked <- lockGrow mainArr crossArr free gfSum off end 0
  if locked == 0 || passes <= 1
    then pure (free, gfSum)
    else settleGrow mainArr crossArr avail gapSum off end (passes - 1)

-- Hand shares to unlocked grow children and restore real cross sizes where
-- the factors clobbered them.
applyGrowShares :: MutablePrimArray RealWorld Float -> MutablePrimArray RealWorld Float -> MutablePrimArray RealWorld Float -> MutablePrimArray RealWorld Float -> Bool -> Float -> Float -> Int -> Int -> IO ()
applyGrowShares wArr hArr mainArr crossArr horizontal !free !gfSum !i !end
  | i >= end = pure ()
  | otherwise = do
      iw <- readPrimArray wArr i
      ih <- readPrimArray hArr i
      gf <- readPrimArray crossArr i
      when (gf > 0) $
        writePrimArray mainArr i (max 0 (free * gf / gfSum))
      writePrimArray crossArr i (if horizontal then ih else iw)
      applyGrowShares wArr hArr mainArr crossArr horizontal free gfSum (i + 1) end

applyShrink :: NodeArena -> MutablePrimArray RealWorld Int -> MutablePrimArray RealWorld Float -> MutablePrimArray RealWorld Float -> MutablePrimArray RealWorld Float -> MutablePrimArray RealWorld Float -> Bool -> Float -> Float -> Int -> Int -> IO ()
applyShrink na idxArr wArr hArr outW outH horizontal !overflow !shrinkTotal !i !end
  | i >= end = pure ()
  | otherwise = do
      ci <- readPrimArray idxArr i
      iw <- readPrimArray wArr i
      ih <- readPrimArray hArr i
      (minW, minH, _, _) <- getMinMax na ci
      (tag, val) <- getAxisSizing na ci horizontal
      let sf = shrinkFactor tag val
          main = if horizontal then iw else ih
          minMain = if horizontal then minW else minH
          delta = overflow * sf / shrinkTotal
          shrunk = max minMain (main - delta)
      if horizontal
        then writePrimArray outW i shrunk >> writePrimArray outH i ih
        else writePrimArray outW i iw >> writePrimArray outH i shrunk
      applyShrink na idxArr wArr hArr outW outH horizontal overflow shrinkTotal (i + 1) end

alignX :: AlignX -> Float -> Float -> Float -> Float
alignX AlignStart cx _ _ = cx
alignX AlignCenter cx cw iw = cx + (cw - iw) / 2
alignX AlignEnd cx cw iw = cx + cw - iw

alignY :: AlignY -> Float -> Float -> Float -> Float
alignY AlignTop cy _ _ = cy
alignY AlignMiddle cy ch ih = cy + (ch - ih) / 2
alignY AlignBottom cy ch ih = cy + ch - ih

placeModals :: NodeArena -> FontMetrics -> Float -> Float -> IO ()
placeModals na fm winW winH = do
  env <- floatingEnv na fm
  let margin = resolveLayoutGap fm windowMargin
  forNodes_ na $ \idx -> do
    nt <- getNodeType na idx
    when (nt == NodeModal) $ do
      (_, _, iw, ih) <- getRect na idx
      let maxW = max 0 (winW - 2 * margin)
          maxH = max 0 (winH - 2 * margin)
          w = min iw maxW
          h = min ih maxH
          x = max 0 ((winW - w) / 2)
          y = max 0 ((winH - h) / 2)
      positionNodeA env 0 idx x y w h

placeWindows ::
  NodeArena ->
  FontMetrics ->
  Float ->
  Float ->
  (WidgetId -> IO (Maybe (Float, Float))) ->
  (WidgetId -> IO (Maybe (Float, Float))) ->
  IO ()
placeWindows na fm winW winH lookupPos lookupSize = do
  let margin = resolveLayoutGap fm windowMargin
  forNodes_ na $ \idx -> do
    nt <- getNodeType na idx
    when (nt == NodeWindow) $ do
      wid <- getWidgetId na idx
      (minW, minH, maxW, maxH) <- getMinMax na idx
      (_, _, iw, ih) <- getRect na idx
      msize <- lookupSize wid
      let w0 =
            case msize of
              Just (sw, _) -> sw
              Nothing -> min iw winW
          h0 =
            case msize of
              Just (_, sh) -> sh
              Nothing -> min ih winH
          w = clamp minW (min maxW winW) w0
          h = clamp minH (min maxH winH) h0
      mpos <- lookupPos wid
      let (x0, y0) = maybe (max 0 (winW - w - margin), margin) id mpos
          x = clamp 0 (max 0 (winW - w)) x0
          y = clamp 0 (max 0 (winH - h)) y0
      positionWindowNode na fm idx x y w h

-- Fit sizing caps at intrinsic size; floating windows use an explicit frame size.
positionWindowNode :: NodeArena -> FontMetrics -> NodeIdx -> Float -> Float -> Float -> Float -> IO ()
positionWindowNode na fm idx x y w h = do
  setRect na idx x y w h
  (pad, gap, dir) <- containerFlow na fm idx
  env <- floatingEnv na fm
  positionChildren env 0 idx dir gap pad x y w h

-- | Horizontal placement for a widget-anchored popup. Aligns the popup's left
-- edge with the anchor even when the anchor sits inside the window margin (a
-- menu bar flush to the left, say); the margin is only there to keep the popup
-- clear of the right edge.
clampPopupX :: Float -> Float -> Float -> Float -> Float
clampPopupX margin winW iw x0
  | x0 < margin && x0 + iw <= winW = max 0 x0
  | otherwise = max margin (min (winW - iw - margin) x0)

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
                else max margin (min (winW - iw - margin) x0)
          y = if y0 + ih > winH - margin && py - ih - margin >= 0
                then py - ih - offset
                else clampY y0
       in (x, y)
    AnchorRect (Rect rx ry rw rh) ->
      case placement of
        PlacementBelow ->
          let x0 = rx
              y0 = ry + rh + offset
              y = if y0 + ih > winH - margin && ry - ih - offset >= margin
                    then ry - ih - offset
                    else y0
              x = clampPopupX margin winW iw x0
           in (x, clampY y)
        PlacementAbove ->
          let x0 = rx
              y0 = ry - ih - offset
              y = if y0 < margin && ry + rh + offset + ih <= winH - margin
                    then ry + rh + offset
                    else y0
              x = clampPopupX margin winW iw x0
           in (x, clampY y)
        PlacementRight ->
          let x0 = rx + rw + offset
              y0 = ry
              x = if x0 + iw > winW - margin && rx - iw - offset >= margin
                    then rx - iw - offset
                    else x0
              y = clampY y0
           in (clampPopupX margin winW iw x, y)
        PlacementLeft ->
          let x0 = rx - iw - offset
              y0 = ry
              x = if x0 < margin && rx + rw + offset + iw <= winW - margin
                    then rx + rw + offset
                    else x0
              y = clampY y0
           in (clampPopupX margin winW iw x, y)
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
    clampY y = max margin (min (winH - ih - margin) y)

placePopups ::
  NodeArena ->
  FontMetrics ->
  Float ->
  Float ->
  (WidgetId -> IO (Maybe (PopupAnchor, PopupPlacement, Float))) ->
  IO ()
placePopups na fm winW winH lookupAnchor = do
  env <- floatingEnv na fm
  let margin = resolveLayoutGap fm windowMargin
  forNodes_ na $ \idx -> do
    nt <- getNodeType na idx
    when (nt == NodePopup) $ do
      wid <- getWidgetId na idx
      (_, _, iw, ih) <- getRect na idx
      mcfg <- lookupAnchor wid
      let (anchor, placement, offset) = case mcfg of
            Just (a, p, o) -> (a, p, o)
            Nothing -> (AnchorPoint (V2 0 0), PlacementAuto, 4)
          (x, y) = computePopupPosition winW winH margin iw ih anchor placement offset
      positionNodeA env 0 idx x y iw ih
