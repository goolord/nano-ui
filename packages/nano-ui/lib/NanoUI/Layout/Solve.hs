module NanoUI.Layout.Solve
  ( solveLayout
  , placeModals
  , placeWindows
  , placePopups
  , computePopupPosition
  , positionNode
  , positionWindowNode
  , scrollBarSlotOf
  ) where

import Control.Monad (when)
import Data.IORef (readIORef, writeIORef)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , PrimArray
  , freezePrimArray
  , indexPrimArray
  , readPrimArray
  , writePrimArray
  )
import Data.Primitive.Types (Prim)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (RealWorld)
import NanoUI.Font
  ( FontMetrics (..)
  , checkboxBoxSize
  , checkboxLeading
  , treeRowLeading
  , classifyScrollBar
  , fmLineHeight
  , resolveLayoutGap
  , resolveLayoutPadding
  , measureText
  , measureTextWrapped
  , measureTextWrappedIO
  , labelContentInset
  , tableCellInset
  , ScrollBarSlot
  , widgetPadding
  , buttonPadding
  , layoutLineHeight
  )
import NanoUI.Types (HostProfile, isCellHost)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeArena
  , NodeArenaArrays (..)
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaArrays
  , arenaCount
  , withArenaArraysSnap
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
  , getNextSibling
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
  , isFloatingNode
  , isScrollNode
  , setRect
  , getNodeValue
  , setNodeValue
  , getScrollContentW
  , setScrollContentW
  , ensureScratchCapacity
  , naScratchCount
  , naScratchIdx
  , naScratchMain
  , naScratchCross
  , naScratchOutMain
  , naScratchOutCross
  )
import NanoUI.Id (WidgetId)
import NanoUI.Style (AlignX (..), AlignY (..), FontVariant (..), Padding (..), windowMargin)
import NanoUI.Types (PopupAnchor (..), PopupPlacement (..), Rect (..), V2 (..), clamp)
import NanoUI.Widgets.ColorPicker (colorPickerMeasureSize)
import NanoUI.WidgetText
  ( textNodeFontVariant
  , treeDecodeStyle
  , treeMeasureLabel
  , selectDisplayText
  , selectChevronReserve
  , sliderValueText
  , textInputFieldHeight
  , textInputLabelGap
  , textInputMinWidth
  , textInputPlaceholder
  , isTableHeaderStyle
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

solveLayout :: NodeArena -> HostProfile -> FontMetrics -> FontMetrics -> (Text -> IO (Float, Float)) -> Float -> Float -> IO ()
solveLayout na host fm monoFm measure rootW rootH =
  withArenaArraysSnap na $ do
    a <- arenaArrays na
    count <- arenaCount na
    whenPositive count $ do
      measurePass na host fm monoFm measure
      positionNodeA a na host fm 0 0 0 rootW rootH

{-# INLINE nodeTypeA #-}
nodeTypeA :: NodeArenaArrays -> NodeIdx -> IO NodeType
nodeTypeA NodeArenaArrays {naArrTags} idx =
  readPrimArray naArrTags (idx * 8) >>= pure . toEnum . fromIntegral

{-# INLINE rectA #-}
rectA :: NodeArenaArrays -> NodeIdx -> IO (Float, Float, Float, Float)
rectA NodeArenaArrays {naArrGeom} idx = do
  let base = idx * 10
  x <- readPrimArray naArrGeom (base + 0)
  y <- readPrimArray naArrGeom (base + 1)
  w <- readPrimArray naArrGeom (base + 2)
  h <- readPrimArray naArrGeom (base + 3)
  pure (x, y, w, h)

{-# INLINE minMaxA #-}
minMaxA :: NodeArenaArrays -> NodeIdx -> IO (Float, Float, Float, Float)
minMaxA NodeArenaArrays {naArrStyle} idx = do
  let base = idx * 16
  minW <- readPrimArray naArrStyle (base + 7)
  minH <- readPrimArray naArrStyle (base + 8)
  maxW <- readPrimArray naArrStyle (base + 9)
  maxH <- readPrimArray naArrStyle (base + 10)
  pure (minW, minH, maxW, maxH)

{-# INLINE widthSizingA #-}
widthSizingA :: NodeArenaArrays -> NodeIdx -> IO (SizingTag, Float)
widthSizingA NodeArenaArrays {naArrTags, naArrStyle} idx = do
  tag <- readPrimArray naArrTags (idx * 8 + 2)
  val <- readPrimArray naArrStyle (idx * 16)
  pure (toEnum (fromIntegral tag), val)

{-# INLINE heightSizingA #-}
heightSizingA :: NodeArenaArrays -> NodeIdx -> IO (SizingTag, Float)
heightSizingA NodeArenaArrays {naArrTags, naArrStyle} idx = do
  tag <- readPrimArray naArrTags (idx * 8 + 3)
  val <- readPrimArray naArrStyle (idx * 16 + 1)
  pure (toEnum (fromIntegral tag), val)

measurePass ::
  NodeArena ->
  HostProfile ->
  FontMetrics ->
  FontMetrics ->
  (Text -> IO (Float, Float)) ->
  IO ()
measurePass na host fm monoFm measure = do
  a <- arenaArrays na
  count <- arenaCount na
  let go !idx
        | idx < 0 = pure ()
        | otherwise = do
            measureNode a na host fm monoFm measure idx
            go (idx - 1)
  go (count - 1)

whenPositive :: Int -> IO () -> IO ()
whenPositive n act = if n > 0 then act else pure ()

measureNode ::
  NodeArenaArrays ->
  NodeArena ->
  HostProfile ->
  FontMetrics ->
  FontMetrics ->
  (Text -> IO (Float, Float)) ->
  NodeIdx ->
  IO ()
measureNode a na host fm monoFm measure idx = do
  nt <- nodeTypeA a idx
  case nt of
    NodeText -> measureTextNode na host fm monoFm measure idx
    NodeSpacer -> measureSpacer na host fm idx
    NodeSeparator -> measureSeparator na idx
    NodeContainer -> measureContainer na host fm idx
    NodePanel -> measureContainer na host fm idx
    NodeScrollContainer -> measureScrollContainer na host fm idx
    NodeModal
      | isCellHost host -> do
          measureContainer na host fm idx
          -- Body scroll owns overflow. Stale value would paint a phantom gutter.
          setNodeValue na idx 0
      | otherwise -> measureScrollContainer na host fm idx
    NodeWindow -> measureContainer na host fm idx
    NodePopup -> measureContainer na host fm idx
    NodeImage -> measureImage na idx
    NodeBox -> measureImage na idx
    NodeDrawing -> measureImage na idx
    _ -> measureWidget na host fm measure idx

measureTextNode ::
  NodeArena ->
  HostProfile ->
  FontMetrics ->
  FontMetrics ->
  (Text -> IO (Float, Float)) ->
  NodeIdx ->
  IO ()
measureTextNode na host fm monoFm measure idx = do
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, _) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  parentAssigns <- growParent na idx
  si <- getStyleIdx na idx
  let fvar = textNodeFontVariant si
      textFm = if fvar == FontMono then monoFm else fm
  txt <- getText na idx
  (tw0, th0) <-
    if fvar == FontMono
      then pure (measureText host monoFm txt)
      else measure txt
  isRowChild <- parentIsRow na idx
  let plain = txt
      (ix, _) = labelContentInset host textFm
      hasNewlines = T.any (== '\n') plain
      canWrap = not isRowChild && maxW < 1e8
      wrapW = max 0 (maxW - 2 * ix)
  (tw, th) <-
    if hasNewlines || (canWrap && maxW + 0.5 < tw0)
      then
        if isCellHost host || fvar == FontMono
          then pure (measureTextWrapped host textFm plain wrapW)
          else measureTextWrappedIO (\t -> fmap fst (measure t)) textFm plain wrapW
      else pure (tw0, th0)
  let reportedW =
        if wTag == SizingGrow && parentAssigns
          then clamp minW maxW 0
          else clamp minW maxW tw
  setRect na idx 0 0 reportedW $
    case hTag of
      SizingFixed -> clamp minH maxH hVal
      _ -> clamp minH maxH (max (layoutLineHeight host textFm) th)

growParent :: NodeArena -> NodeIdx -> IO Bool
growParent na idx = do
  parent <- getParent na idx
  if parent < 0
    then pure False
    else do
      (pwTag, _) <- getWidthSizing na parent
      pure (pwTag == SizingGrow)

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

measureSpacer :: NodeArena -> HostProfile -> FontMetrics -> NodeIdx -> IO ()
measureSpacer na host _fm idx = do
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  -- SDL uses 8px for non-Fixed spacers. On TUI that becomes 8 cells and
  -- blows Fit rows (About Close + flex). Only Fit spacers shrink to 0.
  let along tag val =
        case tag of
          SizingFixed -> val
          SizingFit -> if isCellHost host then 0 else 8
          _ -> 8
      w = along wTag wVal
      h = along hTag hVal
  setRect na idx 0 0 w h

measureSeparator :: NodeArena -> NodeIdx -> IO ()
measureSeparator na idx = do
  dir <- getDirection na idx
  case dir of
    DirRow -> setRect na idx 0 0 1 20
    DirColumn -> setRect na idx 0 0 20 1

{-# INLINE measureMarkedWidget #-}
measureMarkedWidget ::
  HostProfile ->
  FontMetrics ->
  (Text -> IO (Float, Float)) ->
  Text ->
  Float ->
  IO (Float, Float, Float, Float)
measureMarkedWidget host fm measure body leading = do
  (mw, mh) <- measure (if T.null body then " " else body)
  if isCellHost host
    then pure (mw, mh, 0, 0)
    else pure (mw, max mh (checkboxBoxSize host fm), leading, 0)

measureTextField ::
  HostProfile ->
  FontMetrics ->
  (Text -> IO (Float, Float)) ->
  Text ->
  Bool ->
  IO (Float, Float, Float, Float)
measureTextField host fm measure txt multiline = do
  let lbl = if T.null txt then " " else txt
  (lw, lh) <- measure lbl
  if isCellHost host
    then do
      (vw, vh) <- measure (if multiline then " " else textInputPlaceholder lbl)
      pure (max lw vw, max lh vh, 0, 0)
    else do
      pw <- if multiline then pure 0 else fst <$> measure (textInputPlaceholder lbl)
      let gap = textInputLabelGap fm
          fieldH = if multiline then max 96 (textInputFieldHeight fm * 4) else textInputFieldHeight fm
          contentW = max textInputMinWidth (if multiline then lw else max lw pw)
      pure (contentW, lh + gap + fieldH, 0, 0)

measureWidget :: NodeArena -> HostProfile -> FontMetrics -> (Text -> IO (Float, Float)) -> NodeIdx -> IO ()
measureWidget na host fm measure idx = do
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
                let (cx, cy) = tableCellInset host fm
                 in (2 * cx, 2 * cy)
            | otherwise -> buttonPadding host fm
          NodeSelect -> buttonPadding host fm
          NodeCheckbox | isCellHost host -> (0, 0)
          NodeRadio | isCellHost host -> (0, 0)
          _ | isCellHost host -> widgetPadding host fm
            | nt == NodeColorPicker || nt == NodeSlider || nt == NodeCheckbox || nt == NodeRadio ->
                let (cx, cy) = labelContentInset host fm
                 in (2 * cx, cy)
            | otherwise -> widgetPadding host fm
  (tw, th, extraW, extraH) <-
    case nt of
      NodeSlider -> do
        let lbl = if T.null txt then " " else txt
        (lw, lh) <- measure lbl
        (vw, _) <- measure (sliderValueText 100)
        let trackExtra =
              if isCellHost host
                then fmLineHeight fm * 0.35
                else 18
            contentW = max lw vw
        pure (contentW, lh, 0, trackExtra)
      NodeCheckbox -> do
        let body = if T.null txt then " " else txt
        measureMarkedWidget host fm measure body (checkboxLeading host fm)
      NodeRadio -> do
        let body = if T.null txt then " " else txt
        measureMarkedWidget host fm measure body (checkboxLeading host fm)
      NodeTree -> do
        let (_, depth, _, _) = treeDecodeStyle si
            lbl = if T.null txt then " " else txt
            body = if isCellHost host then treeMeasureLabel depth lbl else lbl
        measureMarkedWidget host fm measure body (treeRowLeading host fm depth)
      NodeSelect -> do
        opts <- getOptions na idx
        let lbl = txt
            choices = if null opts then [""] else opts
        dims <- mapM (measure . selectDisplayText lbl) choices
        let (mw, mh) =
              case dims of
                [] -> (0, 0)
                ds -> (maximum (map fst ds), maximum (map snd ds))
        pure (mw, mh, selectChevronReserve, 0)
      NodeColorPicker -> do
        let lbl = if T.null txt then " " else txt
        (mw, mh, extraH) <- colorPickerMeasureSize host fm measure lbl
        pure (mw, mh, 0, extraH)
      NodeTextInput -> measureTextField host fm measure txt False
      NodeTextArea -> measureTextField host fm measure txt True
      _ -> do
        body <-
          if T.null txt
            then pure " "
            else
              if isTableHeaderStyle si
                then pure (tableHeaderDisplayText (isCellHost host) si txt)
                else pure txt
        (mw, mh) <- measure body
        pure (mw, mh, 0, 0)
  let rawW = tw + padX + extraW
      rawH = th + padY + extraH
      w = case wTag of SizingFixed -> wVal; _ -> clamp minW maxW rawW
      h = case hTag of SizingFixed -> hVal; _ -> clamp minH maxH rawH
  setRect na idx 0 0 w h

measureContainer :: NodeArena -> HostProfile -> FontMetrics -> NodeIdx -> IO ()
measureContainer na host fm idx = do
  pad0 <- getPadding na idx
  gap0 <- getGap na idx
  let pad = resolveLayoutPadding host fm pad0
      gap = resolveLayoutGap host fm gap0
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
      then measureGridScratch na idx gCols minColW innerMaxW innerAvailH gap
      else if dir == DirColumn && chrome
        then do
          n <- loadChildrenScratchFromParent na idx innerMaxW innerAvailH
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

measureScrollContainer :: NodeArena -> HostProfile -> FontMetrics -> NodeIdx -> IO ()
measureScrollContainer na host fm idx = do
  pad0 <- getPadding na idx
  gap0 <- getGap na idx
  let pad = resolveLayoutPadding host fm pad0
      gap = resolveLayoutGap host fm gap0
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
              DirColumn -> scrollAxisGutter (scrollPolicyY cfg) host fm slot contentH assignedInnerH
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
  fc <- getFirstChild na idx
  go fc (0 :: Int) (0 :: Float) (0 :: Float)
  where
    go ci !count !main !cross
      | ci < 0 =
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
      | otherwise = do
          nt <- getNodeType na ci
          ns <- getNextSibling na ci
          if isFloatingNode nt
            then go ns count main cross
            else do
              (_, _, w, h) <- getRect na ci
              let count' = count + 1
                  (main', cross') =
                    case dir of
                      DirRow -> (main + w, max cross h)
                      DirColumn -> (max main w, cross + h)
              go ns count' main' cross'

isChromeColumn :: NodeType -> DirTag -> Bool
isChromeColumn nt dir =
  dir == DirColumn && (nt == NodeWindow || nt == NodeModal)

pairColumnGap :: NodeArena -> Bool -> NodeIdx -> NodeIdx -> Float -> IO Float
pairColumnGap _ False _ _ gap = pure gap
pairColumnGap na True a b gap = do
  ntA <- getNodeType na a
  ntB <- getNodeType na b
  pure (if ntA == NodeSeparator || ntB == NodeSeparator then 0 else gap)

foldChromeColumnScratch :: NodeArena -> Int -> Float -> IO (Float, Float)
foldChromeColumnScratch na n gap = do
  wArr <- readIORef (naScratchMain na)
  hArr <- readIORef (naScratchCross na)
  gapSum <- columnGapSumScratch na True n gap
  let go !i !maxW !totalH
        | i >= n = pure (maxW, totalH + gapSum)
        | otherwise = do
            w <- readPrimArray wArr i
            h <- readPrimArray hArr i
            go (i + 1) (max maxW w) (totalH + h)
  go 0 0 0

measureGridScratch ::
  NodeArena ->
  NodeIdx ->
  Int ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO (Float, Float)
measureGridScratch na idx gCols minColW innerMaxW innerAvailH gap = do
  n <- loadChildrenScratchFromParent na idx innerMaxW innerAvailH
  if n <= 0
    then pure (0, 0)
    else do
      wArr <- readIORef (naScratchMain na)
      hArr <- readIORef (naScratchCross na)
      let cols =
            if gCols > 0
              then gCols
              else if minColW > 0 && innerMaxW > 0 && innerMaxW < 1e8
                then max 1 (floor ((innerMaxW + gap) / (minColW + gap)))
                else 1
          numRows = (n + cols - 1) `quot` cols
          calcRows !r !totalH
            | r >= numRows = pure totalH
            | otherwise = do
                let getRowH !j !accH
                      | j >= cols = pure accH
                      | otherwise = do
                          let k = r * cols + j
                          if k >= n
                            then pure accH
                            else do
                              h <- readPrimArray hArr k
                              getRowH (j + 1) (max accH h)
                rowH <- getRowH 0 0
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

positionNode :: NodeArena -> HostProfile -> FontMetrics -> NodeIdx -> Float -> Float -> Float -> Float -> IO ()
positionNode na host fm idx x y availW availH = do
  a <- arenaArrays na
  positionNodeA a na host fm idx x y availW availH

positionNodeA ::
  NodeArenaArrays ->
  NodeArena ->
  HostProfile ->
  FontMetrics ->
  NodeIdx ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
positionNodeA a na host fm idx x y availW availH = do
  (minW, minH, maxW, maxH) <- minMaxA a idx
  (wTag, wVal) <- widthSizingA a idx
  (hTag, hVal) <- heightSizingA a idx
  (_, _, intrinsicW, intrinsicH) <- rectA a idx
  nt <- nodeTypeA a idx
  let w = clamp minW maxW (resolveSize wTag wVal intrinsicW availW minW maxW)
      h = clamp minH maxH (resolveSize hTag hVal intrinsicH availH minH maxH)
  setRect na idx x y w h
  pad0 <- getPadding na idx
  gap0 <- getGap na idx
  let pad = resolveLayoutPadding host fm pad0
      gap = resolveLayoutGap host fm gap0
  dir <- getDirection na idx
  case nt of
    NodeContainer -> positionChildren a na host fm idx dir gap pad x y w h
    NodePanel -> positionChildren a na host fm idx dir gap pad x y w h
    NodeScrollContainer -> positionScrollChildren a na host fm idx dir gap pad x y w h
    NodeModal
      | isCellHost host -> positionChildren a na host fm idx dir gap pad x y w h
      | otherwise -> positionScrollChildren a na host fm idx dir gap pad x y w h
    NodeWindow -> positionChildren a na host fm idx dir gap pad x y w h
    NodePopup -> positionChildren a na host fm idx dir gap pad x y w h
    _ -> pure ()

positionScrollChildren ::
  NodeArenaArrays ->
  NodeArena ->
  HostProfile ->
  FontMetrics ->
  NodeIdx ->
  DirTag ->
  Float ->
  Padding ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
positionScrollChildren a na host fm idx dir gap pad px py pw ph = do
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
          (gutterW, gutterH) = scrollGutters2D host fm slot cfg contentW contentSize innerW innerH
          viewW = max 0 (innerW - gutterW)
          viewH = max 0 (innerH - gutterH)
          -- Keep measured content. Shrinking to the clip wraps table columns.
          layoutW = max contentW viewW
          layoutH = max contentSize viewH
      positionChildren a na host fm idx DirColumn gap pad cx cy layoutW layoutH
    else do
      let cfg = decodeScrollConfig si
          gutterCol = scrollAxisGutter (scrollPolicyY cfg) host fm slot contentSize innerH
          gutterRow = scrollAxisGutter (scrollPolicyX cfg) host fm slot contentSize innerW
      case dir of
        DirRow -> do
          (wTag, _) <- getWidthSizing na idx
          let rowMain =
                if wTag == SizingGrow
                  then max contentSize (innerW - gutterRow)
                  else contentSize
          positionRowFromParent a na host fm idx gap cx cy rowMain (innerH - gutterRow)
        DirColumn -> positionColumnScroll a na host fm idx gap cx cy (innerW - gutterCol) innerH contentSize

scrollBarSlotOf :: NodeArena -> NodeIdx -> IO ScrollBarSlot
scrollBarSlotOf na idx = do
  parent <- getParent na idx
  isWin <-
    if parent < 0
      then pure False
      else do
        pnt <- getNodeType na parent
        pure (pnt == NodeWindow)
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
  NodeArenaArrays ->
  NodeArena ->
  HostProfile ->
  FontMetrics ->
  NodeIdx ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
positionColumnScroll a na host fm parent gap cx cy innerW innerH contentSize = do
  n <- loadChildrenScratchFromParent na parent innerW innerH
  withAxisSnaps na n contentSize (gap * fromIntegral (max 0 (n - 1))) False $ \idxSnap outSnap -> do
    let go !i !curY
          | i >= n = pure ()
          | otherwise = do
              let ci = indexPrimArray idxSnap i
                  fh = indexPrimArray outSnap i
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
              positionNodeA a na host fm ci fx curY cw nodeH
              go (i + 1) (curY + fh + gap)
    go 0 cy

resolveSize :: SizingTag -> Float -> Float -> Float -> Float -> Float -> Float
resolveSize SizingFixed v _ _ _ _ = v
resolveSize SizingFit _ intrinsic avail minS maxS = clamp minS maxS (min intrinsic avail)
resolveSize SizingShrink _ intrinsic avail minS maxS = clamp minS maxS (min intrinsic avail)
resolveSize SizingGrow _ _ avail _ maxS = min avail maxS
resolveSize SizingPercent _ _ avail _ maxS = min avail maxS

positionChildren ::
  NodeArenaArrays ->
  NodeArena ->
  HostProfile ->
  FontMetrics ->
  NodeIdx ->
  DirTag ->
  Float ->
  Padding ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
positionChildren a na host fm idx dir gap pad px py pw ph = do
  nt <- getNodeType na idx
  gCols <- getGridCols na idx
  minColW <- getGridMinColW na idx
  let chrome = isChromeColumn nt dir
      cx = px + padL pad
      cy = py + padT pad
      cw = pw - padL pad - padR pad
      ch = ph - padT pad - padB pad
  if gCols > 0 || minColW > 0
    then positionGrid a na host fm idx gCols minColW gap cx cy cw ch
    else case dir of
      DirRow -> positionRowFromParent a na host fm idx gap cx cy cw ch
      DirColumn -> positionColumnFromParent a na host fm idx gap chrome px py pw cx cy cw ch

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

-- Column leftover must not change Fit/Fixed/Shrink step height.
columnChildHeight :: NodeArena -> NodeIdx -> Float -> IO Float
columnChildHeight na ci scratchH = do
  (hTag, _) <- getHeightSizing na ci
  if hTag == SizingGrow || hTag == SizingPercent
    then pure scratchH
    else do
      (_, minH, _, _) <- getMinMax na ci
      (_, _, _, ih) <- getRect na ci
      pure (max minH ih)

withAxisSnaps ::
  NodeArena ->
  Int ->
  Float ->
  Float ->
  Bool ->
  (PrimArray Int -> PrimArray Float -> IO a) ->
  IO a
withAxisSnaps na n availMain gapSum horizontal act = do
  distributeScratch na 0 n availMain gapSum horizontal
  idxArr <- readIORef (naScratchIdx na)
  outArr <-
    if horizontal
      then readIORef (naScratchOutMain na)
      else readIORef (naScratchOutCross na)
  idxSnap <- freezePrimArray idxArr 0 n
  outSnap <- freezePrimArray outArr 0 n
  act idxSnap outSnap

positionRowFromParent ::
  NodeArenaArrays ->
  NodeArena ->
  HostProfile ->
  FontMetrics ->
  NodeIdx ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
positionRowFromParent a na host fm parent gap cx cy cw ch = do
  n <- loadChildrenScratchFromParent na parent cw ch
  withAxisSnaps na n cw (gap * fromIntegral (max 0 (n - 1))) True $ \idxSnap outSnap -> do
    let goRow !i !curX
          | i >= n = pure ()
          | otherwise = do
              let ci = indexPrimArray idxSnap i
                  fw = indexPrimArray outSnap i
              -- Fit/fixed children keep content height. Only Grow/Percent eat `ch`.
              crossH <- childRowCrossSize na ci ch
              ay <- getAlignY na ci
              let fy = alignY ay cy ch crossH
              positionNodeA a na host fm ci curX fy fw crossH
              goRow (i + 1) (curX + fw + gap)
    goRow 0 cx

positionGrid ::
  NodeArenaArrays ->
  NodeArena ->
  HostProfile ->
  FontMetrics ->
  NodeIdx ->
  Int ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
positionGrid a na host fm parent gCols minColW gap cx cy cw ch = do
  n <- loadChildrenScratchFromParent na parent cw ch
  whenPositive n $ do
    idxArr <- readIORef (naScratchIdx na)
    hArr <- readIORef (naScratchCross na)
    let cols =
          if gCols > 0
            then gCols
            else if minColW > 0 && cw > 0
              then max 1 (floor ((cw + gap) / (minColW + gap)))
              else 1
        colW = max 0 ((cw - gap * fromIntegral (cols - 1)) / fromIntegral cols)
        numRows = (n + cols - 1) `quot` cols
        goRows !r !curY
          | r >= numRows = pure ()
          | otherwise = do
              let getRowH !j !accH
                    | j >= cols = pure accH
                    | otherwise = do
                        let k = r * cols + j
                        if k >= n
                          then pure accH
                          else do
                            h <- readPrimArray hArr k
                            getRowH (j + 1) (max accH h)
              rowH <- getRowH 0 0
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
                            positionNodeA a na host fm ci fx fy colW rowH
                            goCols (j + 1)
              goCols 0
              goRows (r + 1) (curY + rowH + gap)
    goRows 0 cy

positionColumnFromParent ::
  NodeArenaArrays ->
  NodeArena ->
  HostProfile ->
  FontMetrics ->
  NodeIdx ->
  Float ->
  Bool ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
positionColumnFromParent a na host fm parent gap chrome px _ pw cx cy cw ch = do
  n <- loadChildrenScratchFromParent na parent cw ch
  gapSum <- columnGapSumScratch na chrome n gap
  withAxisSnaps na n ch gapSum False $ \idxSnap outSnap -> do
    let go !i !curY
          | i >= n = pure ()
          | otherwise = do
              let ci = indexPrimArray idxSnap i
                  fh = indexPrimArray outSnap i
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
              positionNodeA a na host fm ci fx curY nodeW childH
              gapAfter <-
                if i + 1 >= n
                  then pure 0
                  else pairColumnGap na chrome ci (indexPrimArray idxSnap (i + 1)) gap
              go (i + 1) (curY + childH + gapAfter)
    go 0 cy


loadChildrenScratchFromParent :: NodeArena -> NodeIdx -> Float -> Float -> IO Int
loadChildrenScratchFromParent na parent availW availH = do
  fc <- getFirstChild na parent
  cc <- getChildCount na parent
  ensureScratchCapacity na cc
  idxArr <- readIORef (naScratchIdx na)
  mainArr <- readIORef (naScratchMain na)
  crossArr <- readIORef (naScratchCross na)
  let go !ci !i
        | ci < 0 = do
            reverseScratchTriple idxArr mainArr crossArr 0 (i - 1)
            writeIORef (naScratchCount na) i
            pure i
        | otherwise = do
            nt <- getNodeType na ci
            ns <- getNextSibling na ci
            if isFloatingNode nt
              then go ns i
              else do
                writeScratchEntry na ci i idxArr mainArr crossArr availW availH
                go ns (i + 1)
  go fc 0

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

writeScratchEntry ::
  NodeArena ->
  NodeIdx ->
  Int ->
  MutablePrimArray RealWorld Int ->
  MutablePrimArray RealWorld Float ->
  MutablePrimArray RealWorld Float ->
  Float ->
  Float ->
  IO ()
writeScratchEntry na ci i idxArr mainArr crossArr availW availH = do
  (_, _, w, h) <- getRect na ci
  (wTag, wVal) <- getWidthSizing na ci
  (hTag, hVal) <- getHeightSizing na ci
  (minW, minH, maxW, maxH) <- getMinMax na ci
  let w' =
        case wTag of
          SizingPercent -> clamp minW maxW (availW * wVal / 100)
          _ -> w
      h' =
        case hTag of
          SizingPercent -> clamp minH maxH (availH * hVal / 100)
          _ -> h
  writePrimArray idxArr i ci
  writePrimArray mainArr i w'
  writePrimArray crossArr i h'

columnGapSumScratch :: NodeArena -> Bool -> Int -> Float -> IO Float
columnGapSumScratch _ False _ _ = pure 0
columnGapSumScratch _ True n _
  | n <= 1 = pure 0
columnGapSumScratch na True n gap = do
  idxArr <- readIORef (naScratchIdx na)
  let go !i !acc
        | i >= n - 1 = pure acc
        | otherwise = do
            a <- readPrimArray idxArr i
            b <- readPrimArray idxArr (i + 1)
            g <- pairColumnGap na True a b gap
            go (i + 1) (acc + g)
  go 0 0

distributeScratch :: NodeArena -> Int -> Int -> Float -> Float -> Bool -> IO ()
distributeScratch na off n avail gapSum horizontal = do
  idxArr <- readIORef (naScratchIdx na)
  wArr <- readIORef (naScratchMain na)
  hArr <- readIORef (naScratchCross na)
  outW <- readIORef (naScratchOutMain na)
  outH <- readIORef (naScratchOutCross na)
  let end = off + n
      copyOut = do
        let go !i
              | i >= end = pure ()
              | otherwise = do
                  w <- readPrimArray wArr i
                  h <- readPrimArray hArr i
                  writePrimArray outW i w
                  writePrimArray outH i h
                  go (i + 1)
        go off
      sumAxis = do
        let go !i !acc
              | i >= end = pure acc
              | otherwise = do
                  v <- if horizontal then readPrimArray wArr i else readPrimArray hArr i
                  go (i + 1) (acc + v)
        go off 0
      sumFact k = do
        let go !i !acc
              | i >= end = pure acc
              | otherwise = do
                  ci <- readPrimArray idxArr i
                  f <- k na ci horizontal
                  go (i + 1) (acc + f)
        go off 0
  total <- sumAxis
  let slack = avail - (total + gapSum)
  if slack > 0.001
    then do
      growTotal <- sumFact getGrowFactor
      if growTotal <= 0
        then copyOut
        else do
          let go !i
                | i >= end = pure ()
                | otherwise = do
                    ci <- readPrimArray idxArr i
                    iw <- readPrimArray wArr i
                    ih <- readPrimArray hArr i
                    gf <- getGrowFactor na ci horizontal
                    let extra = slack * gf / growTotal
                    if horizontal
                      then writePrimArray outW i (iw + extra) >> writePrimArray outH i ih
                      else writePrimArray outW i iw >> writePrimArray outH i (ih + extra)
                    go (i + 1)
          go off
    else
      if slack < -0.001
        then do
          shrinkTotal <- sumFact getShrinkFactor
          if shrinkTotal <= 0
            then copyOut
            else do
              let overflow = negate slack
                  go !i
                    | i >= end = pure ()
                    | otherwise = do
                        ci <- readPrimArray idxArr i
                        iw <- readPrimArray wArr i
                        ih <- readPrimArray hArr i
                        (minW, minH, _, _) <- getMinMax na ci
                        sf <- getShrinkFactor na ci horizontal
                        let main = if horizontal then iw else ih
                            minMain = if horizontal then minW else minH
                            delta = overflow * sf / shrinkTotal
                            shrunk = max minMain (main - delta)
                        if horizontal
                          then writePrimArray outW i shrunk >> writePrimArray outH i ih
                          else writePrimArray outW i iw >> writePrimArray outH i shrunk
                        go (i + 1)
              go off
        else copyOut

getGrowFactor :: NodeArena -> NodeIdx -> Bool -> IO Float
getGrowFactor na idx horizontal = do
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  case if horizontal then (wTag, wVal) else (hTag, hVal) of
    (SizingGrow, g) -> pure g
    _ -> pure 0

getShrinkFactor :: NodeArena -> NodeIdx -> Bool -> IO Float
getShrinkFactor na idx horizontal = do
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  let (tag, val) = if horizontal then (wTag, wVal) else (hTag, hVal)
  case tag of
    SizingShrink -> pure val
    -- Grow also gives space back when the window is smaller than content.
    SizingGrow -> pure (if val > 0 then val else 1)
    -- Fit stays content-sized. A pinned header must not squash when a Grow
    -- sibling (page scroll) is taller than the window.
    SizingFit -> pure 0
    _ -> pure 0

alignX :: AlignX -> Float -> Float -> Float -> Float
alignX AlignStart cx _ _ = cx
alignX AlignCenter cx cw iw = cx + (cw - iw) / 2
alignX AlignEnd cx cw iw = cx + cw - iw

alignY :: AlignY -> Float -> Float -> Float -> Float
alignY AlignTop cy _ _ = cy
alignY AlignMiddle cy ch ih = cy + (ch - ih) / 2
alignY AlignBottom cy ch ih = cy + ch - ih

placeModals :: NodeArena -> HostProfile -> FontMetrics -> Float -> Float -> IO ()
placeModals na host fm winW winH = do
  count <- arenaCount na
  let margin = resolveLayoutGap host fm windowMargin
      go !idx
        | idx >= count = pure ()
        | otherwise = do
            nt <- getNodeType na idx
            when (nt == NodeModal) $ do
              (_, _, iw, ih) <- getRect na idx
              let maxW = max 0 (winW - 2 * margin)
                  maxH = max 0 (winH - 2 * margin)
                  w = min iw maxW
                  h = min ih maxH
                  x = max 0 ((winW - w) / 2)
                  y = max 0 ((winH - h) / 2)
              positionNode na host fm idx x y w h
            go (idx + 1)
  go 0

placeWindows ::
  NodeArena ->
  HostProfile ->
  FontMetrics ->
  Float ->
  Float ->
  (WidgetId -> IO (Maybe (Float, Float))) ->
  (WidgetId -> IO (Maybe (Float, Float))) ->
  IO ()
placeWindows na host fm winW winH lookupPos lookupSize = do
  count <- arenaCount na
  let margin = resolveLayoutGap host fm windowMargin
      go !idx
        | idx >= count = pure ()
        | otherwise = do
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
              positionWindowNode na host fm idx x y w h
            go (idx + 1)
  go 0

-- Fit sizing caps at intrinsic size; floating windows use an explicit frame size.
positionWindowNode :: NodeArena -> HostProfile -> FontMetrics -> NodeIdx -> Float -> Float -> Float -> Float -> IO ()
positionWindowNode na host fm idx x y w h = do
  setRect na idx x y w h
  pad0 <- getPadding na idx
  gap0 <- getGap na idx
  let pad = resolveLayoutPadding host fm pad0
      gap = resolveLayoutGap host fm gap0
  dir <- getDirection na idx
  a <- arenaArrays na
  positionChildren a na host fm idx dir gap pad x y w h

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
                else max margin (min (winH - ih - margin) y0)
       in (x, y)
    AnchorRect (Rect rx ry rw rh) ->
      case placement of
        PlacementBelow ->
          let x0 = rx
              y0 = ry + rh + offset
              y = if y0 + ih > winH - margin && ry - ih - offset >= margin
                    then ry - ih - offset
                    else y0
              x = max margin (min (winW - iw - margin) x0)
           in (x, max margin (min (winH - ih - margin) y))
        PlacementAbove ->
          let x0 = rx
              y0 = ry - ih - offset
              y = if y0 < margin && ry + rh + offset + ih <= winH - margin
                    then ry + rh + offset
                    else y0
              x = max margin (min (winW - iw - margin) x0)
           in (x, max margin (min (winH - ih - margin) y))
        PlacementRight ->
          let x0 = rx + rw + offset
              y0 = ry
              x = if x0 + iw > winW - margin && rx - iw - offset >= margin
                    then rx - iw - offset
                    else x0
              y = max margin (min (winH - ih - margin) y0)
           in (max margin (min (winW - iw - margin) x), y)
        PlacementLeft ->
          let x0 = rx - iw - offset
              y0 = ry
              x = if x0 < margin && rx + rw + offset + iw <= winW - margin
                    then rx + rw + offset
                    else x0
              y = max margin (min (winH - ih - margin) y0)
           in (max margin (min (winW - iw - margin) x), y)
        PlacementAuto ->
          let spaceBelow = winH - margin - (ry + rh + offset)
              spaceAbove = ry - offset - margin
              y = if spaceBelow >= ih || spaceBelow >= spaceAbove
                    then ry + rh + offset
                    else ry - ih - offset
              x = max margin (min (winW - iw - margin) rx)
           in (x, max margin (min (winH - ih - margin) y))
        PlacementAtCursor ->
          (max margin (min (winW - iw - margin) rx), max margin (min (winH - ih - margin) (ry + rh + offset)))

placePopups ::
  NodeArena ->
  HostProfile ->
  FontMetrics ->
  Float ->
  Float ->
  (WidgetId -> IO (Maybe (PopupAnchor, PopupPlacement, Float))) ->
  IO ()
placePopups na host fm winW winH lookupAnchor = do
  count <- arenaCount na
  let margin = resolveLayoutGap host fm windowMargin
      go !idx
        | idx >= count = pure ()
        | otherwise = do
            nt <- getNodeType na idx
            when (nt == NodePopup) $ do
              wid <- getWidgetId na idx
              (_, _, iw, ih) <- getRect na idx
              mcfg <- lookupAnchor wid
              let (anchor, placement, offset) = case mcfg of
                    Just (a, p, o) -> (a, p, o)
                    Nothing -> (AnchorPoint (V2 0 0), PlacementAuto, 4)
                  (x, y) = computePopupPosition winW winH margin iw ih anchor placement offset
              positionNode na host fm idx x y iw ih
            go (idx + 1)
  go 0
