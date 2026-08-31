module NanoUI.Layout.Solve
  (   solveLayout
  , placeModals
  , placeWindows
  , positionNode
  , positionWindowNode
  , scrollBarSlotOf
  ) where

import Control.Monad (forM, void, when)
import Data.IORef (readIORef, writeIORef)
import Data.Primitive.PrimArray (MutablePrimArray, readPrimArray, writePrimArray)
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
  , measureTextWrapped
  , measureTextWrappedIO
  , labelContentInset
  , stripWidgetMarkers
  , ScrollBarSlot
  , scrollLayoutGutter
  , widgetPadding
  , buttonPadding
  , layoutLineHeight
  )
import NanoUI.Host (HostProfile, isCellHost)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeArena
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , getAlignX
  , getAlignY
  , getAspect
  , getDirection
  , getFirstChild
  , getGap
  , getHeightSizing
  , getMinMax
  , getNextSibling
  , getNodeType
  , getParent
  , getPadding
  , getRect
  , getText
  , getWidgetId
  , getWidthSizing
  , getWrap
  , isFloatingNode
  , isScrollNode
  , setRect
  , getNodeValue
  , setNodeValue
  , ensureScratchCapacity
  , naScratchCount
  , naScratchIdx
  , naScratchMain
  , naScratchCross
  , naScratchOutMain
  , naScratchOutCross
  )
import NanoUI.Id (WidgetId)
import NanoUI.Style (AlignX (..), AlignY (..), Padding (..), windowMargin)
import NanoUI.ColorPicker (colorPickerMeasureSize)
import NanoUI.WidgetText
  ( checkboxLabelText
  , radioLabelText
  , treeLabelText
  , treeMeasureLabel
  , treeParseRow
  , selectDisplayText
  , selectChevronReserve
  , selectParseOptions
  , sliderLabelText
  , sliderParseRange
  , textInputFieldHeight
  , textInputLabelGap
  , textInputMinWidth
  , textInputPlaceholder
  , sliderValueText
  )

solveLayout :: NodeArena -> HostProfile -> FontMetrics -> (Text -> IO (Float, Float)) -> Float -> Float -> IO ()
solveLayout na host fm measure rootW rootH = do
  count <- arenaCount na
  whenPositive count $ do
    -- Wrap rows and wrapping labels need a known width. First pass sizes Grow
    -- rows unconstrained, position assigns widths, second pass remasures wrap
    -- height, then we position again so siblings sit below the wrapped content.
    measurePass na host fm measure False
    positionNode na host fm 0 0 0 rootW rootH
    needsRemeasure <- anyNeedsRemeasure na count
    when needsRemeasure $ do
      measurePass na host fm measure True
      positionNode na host fm 0 0 0 rootW rootH

measurePass ::
  NodeArena ->
  HostProfile ->
  FontMetrics ->
  (Text -> IO (Float, Float)) ->
  Bool ->
  IO ()
measurePass na host fm measure useAssignedWidth = do
  count <- arenaCount na
  let go !idx
        | idx < 0 = pure ()
        | otherwise = do
            measureNode na host fm measure useAssignedWidth idx
            go (idx - 1)
  go (count - 1)

whenPositive :: Int -> IO () -> IO ()
whenPositive n act = if n > 0 then act else pure ()

-- Flex wrap and body text both need a second measure after widths are known.
anyNeedsRemeasure :: NodeArena -> Int -> IO Bool
anyNeedsRemeasure na count = go 0
  where
    go idx
      | idx >= count = pure False
      | otherwise = do
          wrapped <- getWrap na idx
          nt <- getNodeType na idx
          ratio <- getAspect na idx
          if wrapped || nt == NodeText || ratio > 0
            then pure True
            else go (idx + 1)

measureNode ::
  NodeArena ->
  HostProfile ->
  FontMetrics ->
  (Text -> IO (Float, Float)) ->
  Bool ->
  NodeIdx ->
  IO ()
measureNode na host fm measure useAssignedWidth idx = do
  (_, _, assignedW, _) <- getRect na idx
  nt <- getNodeType na idx
  case nt of
    NodeText -> measureTextNode na host fm measure useAssignedWidth idx
    NodeSpacer -> measureSpacer na host fm idx
    NodeSeparator -> measureSeparator na idx
    NodeContainer -> measureContainer na host fm useAssignedWidth idx
    NodePanel -> measureContainer na host fm useAssignedWidth idx
    NodeScrollContainer -> measureScrollContainer na host fm idx
    NodeModal
      | isCellHost host -> do
          measureContainer na host fm useAssignedWidth idx
          -- Body scroll owns overflow. Stale value would paint a phantom gutter.
          setNodeValue na idx 0
      | otherwise -> measureScrollContainer na host fm idx
    NodeWindow -> measureContainer na host fm useAssignedWidth idx
    NodeImage -> measureImage na idx
    NodeBox -> measureImage na idx
    _ -> measureWidget na host fm measure idx
  applyAspectAfterMeasure na assignedW useAssignedWidth idx

applyAspectAfterMeasure :: NodeArena -> Float -> Bool -> NodeIdx -> IO ()
applyAspectAfterMeasure na assignedW useAssignedWidth idx = do
  ratio <- getAspect na idx
  when (ratio > 0) $ do
    (x, y, w, _) <- getRect na idx
    (_, minH, _, maxH) <- getMinMax na idx
    (wTag, wVal) <- getWidthSizing na idx
    let baseW
          | wTag == SizingFixed = wVal
          | useAssignedWidth && assignedW > 0 = assignedW
          | otherwise = w
    setRect na idx x y w (clamp (baseW / ratio) minH maxH)

measureTextNode ::
  NodeArena ->
  HostProfile ->
  FontMetrics ->
  (Text -> IO (Float, Float)) ->
  Bool ->
  NodeIdx ->
  IO ()
measureTextNode na host fm measure useAssignedWidth idx = do
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, _) <- getWidthSizing na idx
  (_, _, assignedW, _) <- getRect na idx
  let needsAssignedWrap = useAssignedWidth && wTag == SizingGrow && assignedW > 0
  if useAssignedWidth && maxW >= 1e8 && not needsAssignedWrap
    then pure ()
    else do
      txt <- getText na idx
      (tw0, th0) <- measure txt
      let plain = stripWidgetMarkers txt
          (ix, _) = labelContentInset host fm
          hasNewlines = T.any (== '\n') plain
          wrapCap
            | maxW < 1e8 = max 0 maxW
            | needsAssignedWrap = assignedW
            | otherwise = maxW
          wrapW = max 0 (wrapCap - 2 * ix)
      (tw, th) <-
        if hasNewlines || (wrapCap < 1e8 && wrapCap + 0.5 < tw0)
          then
            if isCellHost host
              then pure (measureTextWrapped host fm plain wrapW)
              else measureTextWrappedIO (\t -> fmap fst (measure t)) fm plain wrapW
          else pure (tw0, th0)
      setRect na idx 0 0 (clamp tw minW maxW) (clamp (max (layoutLineHeight host fm) th) minH maxH)

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
  setRect na idx 0 0 (clamp w minW maxW) (clamp h minH maxH)

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

measureWidget :: NodeArena -> HostProfile -> FontMetrics -> (Text -> IO (Float, Float)) -> NodeIdx -> IO ()
measureWidget na host fm measure idx = do
  nt <- getNodeType na idx
  txt <- getText na idx
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  let (padX, padY) =
        case nt of
          NodeButton -> buttonPadding host fm
          NodeSelect -> buttonPadding host fm
          NodeColorPicker
            | isCellHost host -> widgetPadding host fm
            | otherwise ->
                let (cx, cy) = labelContentInset host fm
                 in (2 * cx, cy)
          NodeSlider
            | isCellHost host -> widgetPadding host fm
            | otherwise ->
                let (cx, cy) = labelContentInset host fm
                 in (2 * cx, cy)
          NodeCheckbox
            | isCellHost host -> (0, 0)
            | otherwise ->
                let (cx, cy) = labelContentInset host fm
                 in (2 * cx, cy)
          NodeRadio
            | isCellHost host -> (0, 0)
            | otherwise ->
                let (cx, cy) = labelContentInset host fm
                 in (2 * cx, cy)
          _ -> widgetPadding host fm
  (tw, th, extraW, extraH) <-
    case nt of
      NodeSlider -> do
        let lbl =
              if T.null txt
                then " "
                else sliderLabelText txt
            (_, minV, maxV) = sliderParseRange txt
        (lw, lh) <- measure lbl
        (vwMin, _) <- measure (sliderValueText minV)
        (vwMax, _) <- measure (sliderValueText maxV)
        let vw = max vwMin vwMax
        let trackExtra =
              if isCellHost host
                then fmLineHeight fm * 0.35
                else 18
            -- Min width for column sizing; Grow still expands in positionNode.
            contentW = max lw vw
        pure (contentW, lh, 0, trackExtra)
      NodeCheckbox -> do
        let body =
              if T.null txt
                then " "
                else if isCellHost host then txt else checkboxLabelText txt
        measureMarkedWidget host fm measure body (checkboxLeading host fm)
      NodeRadio -> do
        let body =
              if T.null txt
                then " "
                else if isCellHost host then txt else radioLabelText txt
        measureMarkedWidget host fm measure body (checkboxLeading host fm)
      NodeTree -> do
        let (_, _, depth, _, _, raw) = treeParseRow txt
            lbl = if T.null raw then " " else treeLabelText txt
            body = if isCellHost host then treeMeasureLabel depth lbl else lbl
        measureMarkedWidget host fm measure body (treeRowLeading host fm depth)
      NodeSelect -> do
        let (lbl, opts) = selectParseOptions txt
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
      NodeTextInput -> do
        let lbl = if T.null txt then " " else txt
        (lw, lh) <- measure lbl
        if isCellHost host
          then do
            (vw, vh) <- measure (textInputPlaceholder lbl)
            pure (max lw vw, max lh vh, 0, 0)
          else do
            (pw, _) <- measure (textInputPlaceholder lbl)
            let fieldH = textInputFieldHeight fm
                gap = textInputLabelGap fm
                contentW = max textInputMinWidth (max lw pw)
            pure (contentW, lh + gap + fieldH, 0, 0)
      _ -> do
        let body =
              if T.null txt
                then " "
                else txt
        (mw, mh) <- measure body
        pure (mw, mh, 0, 0)
  let rawW = tw + padX + extraW
      rawH = th + padY + extraH
      w = case wTag of SizingFixed -> wVal; _ -> clamp rawW minW maxW
      h = case hTag of SizingFixed -> hVal; _ -> clamp rawH minH maxH
  setRect na idx 0 0 w h

measureContainer :: NodeArena -> HostProfile -> FontMetrics -> Bool -> NodeIdx -> IO ()
measureContainer na host fm useAssignedWidth idx = do
  pad0 <- getPadding na idx
  gap0 <- getGap na idx
  let pad = resolveLayoutPadding host fm pad0
      gap = resolveLayoutGap host fm gap0
  dir <- getDirection na idx
  wrap <- getWrap na idx
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  (_, _, assignedW, assignedH) <- getRect na idx
  nt <- getNodeType na idx
  let chrome = isChromeColumn nt dir
      padX = padL pad + padR pad
      padY = padT pad + padB pad
      innerMaxW =
        case wTag of
          SizingFixed -> max 0 (wVal - padX)
          _
            | useAssignedWidth && assignedW > 0 ->
                max 0 (assignedW - padX)
            | otherwise -> max 0 (maxW - padX)
      innerAvailH =
        case hTag of
          SizingFixed -> max 0 (hVal - padY)
          _
            | useAssignedWidth && assignedH > 0 ->
                max 0 (assignedH - padY)
            | otherwise -> max 0 (maxH - padY)
  (contentW, contentH) <-
    if wrap && dir == DirRow && innerMaxW > 0
      then do
        n <- loadChildrenScratchFromParent na idx innerMaxW innerAvailH
        foldWrappedRowScratch na n innerMaxW gap
      else if dir == DirColumn && chrome
        then do
          n <- loadChildrenScratchFromParent na idx innerMaxW innerAvailH
          foldChromeColumnScratch na n gap
        else foldChildDimsFromParent na idx dir gap
  let w =
        case wTag of
          SizingFixed -> clamp wVal minW maxW
          _ -> clamp (contentW + padL pad + padR pad) minW maxW
      h =
        case hTag of
          SizingFixed -> clamp hVal minH maxH
          _ -> clamp (contentH + padT pad + padB pad) minH maxH
  setRect na idx 0 0 w h

measureScrollContainer :: NodeArena -> HostProfile -> FontMetrics -> NodeIdx -> IO ()
measureScrollContainer na host fm idx = do
  pad0 <- getPadding na idx
  gap0 <- getGap na idx
  let pad = resolveLayoutPadding host fm pad0
      gap = resolveLayoutGap host fm gap0
  dir <- getDirection na idx
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  (contentW, contentH) <- foldChildDimsFromParent na idx dir gap
  let fullW = contentW + padL pad + padR pad
      fullH = contentH + padT pad + padB pad
  -- Overflow compares against the inner viewport, so store content only.
  -- Including pad made every padded Fit modal/list think it could scroll.
  setNodeValue na idx (case dir of DirColumn -> contentH; DirRow -> contentW)
  let viewportW =
        case wTag of
          SizingFixed -> wVal
          _ -> fullW
      viewportH =
        case hTag of
          SizingFixed -> hVal
          _ -> fullH
  setRect na idx 0 0 (clamp viewportW minW maxW) (clamp viewportH minH maxH)

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

foldWrappedRowScratch :: NodeArena -> Int -> Float -> Float -> IO (Float, Float)
foldWrappedRowScratch na n avail gap = do
  wArr <- readIORef (naScratchMain na)
  hArr <- readIORef (naScratchCross na)
  let go !start !maxW !totalH !rowCount
        | start >= n =
            pure (maxW, totalH + gap * fromIntegral (max 0 (rowCount - 1)))
        | otherwise = do
            (end, rowH, lineW) <- packWrapLineEnd wArr hArr start n avail gap
            go end (max maxW lineW) (totalH + rowH) (rowCount + 1)
  go 0 (0 :: Float) (0 :: Float) (0 :: Int)

packWrapLineEnd ::
  MutablePrimArray RealWorld Float ->
  MutablePrimArray RealWorld Float ->
  Int ->
  Int ->
  Float ->
  Float ->
  IO (Int, Float, Float)
packWrapLineEnd wArr hArr start n avail gap = go start (0 :: Float) (0 :: Float)
  where
    go !i !lineW !lineH
      | i >= n = pure (i, lineH, lineW)
      | otherwise = do
          w <- readPrimArray wArr i
          h <- readPrimArray hArr i
          let need = if lineW <= 0 then w else w + gap
          if lineW <= 0 || lineW + need <= avail + 0.001
            then go (i + 1) (lineW + need) (max lineH h)
            else pure (i, lineH, lineW)

positionNode :: NodeArena -> HostProfile -> FontMetrics -> NodeIdx -> Float -> Float -> Float -> Float -> IO ()
positionNode na host fm idx x y availW availH = do
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  (_, _, intrinsicW, intrinsicH) <- getRect na idx
  ratio <- getAspect na idx
  let w = clamp (resolveSize wTag wVal intrinsicW availW minW maxW) minW maxW
      h0 = clamp (resolveSize hTag hVal intrinsicH availH minH maxH) minH maxH
      h = if ratio > 0 then clamp (w / ratio) minH maxH else h0
  setRect na idx x y w h
  pad0 <- getPadding na idx
  gap0 <- getGap na idx
  let pad = resolveLayoutPadding host fm pad0
      gap = resolveLayoutGap host fm gap0
  dir <- getDirection na idx
  nt <- getNodeType na idx
  case nt of
    NodeContainer -> positionChildren na host fm idx dir gap pad x y w h
    NodePanel -> positionChildren na host fm idx dir gap pad x y w h
    NodeScrollContainer -> positionScrollChildren na host fm idx dir gap pad x y w h
    NodeModal
      | isCellHost host -> positionChildren na host fm idx dir gap pad x y w h
      | otherwise -> positionScrollChildren na host fm idx dir gap pad x y w h
    NodeWindow -> positionChildren na host fm idx dir gap pad x y w h
    _ -> pure ()

positionScrollChildren ::
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
positionScrollChildren na host fm idx dir gap pad px py pw ph = do
  contentSize <- getNodeValue na idx
  slot <- scrollBarSlotOf na idx
  let cx = px + padL pad
      cy = py + padT pad
      innerW = pw - padL pad - padR pad
      innerH = ph - padT pad - padB pad
      gutterCol = scrollLayoutGutter host fm slot contentSize innerH
      gutterRow = scrollLayoutGutter host fm slot contentSize innerW
  case dir of
    DirRow -> positionRowFromParent na host fm idx gap cx cy contentSize (innerH - gutterRow)
    DirColumn -> positionColumnScroll na host fm idx gap cx cy (innerW - gutterCol) innerH contentSize

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
            _ -> getParent na p >>= go

positionColumnScroll ::
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
positionColumnScroll na host fm parent gap cx cy innerW innerH contentSize = do
  n <- loadChildrenScratchFromParent na parent innerW innerH
  distributeScratch na 0 n contentSize (gap * fromIntegral (max 0 (n - 1))) False
  pairs <- snapshotScratchColumn na 0 n
  let go [] _ = pure ()
      go ((ci, fh) : rest) !curY = do
        nt <- getNodeType na ci
        (_, _, iw, _) <- getRect na ci
        ax <- getAlignX na ci
        let cw = innerW
            fx = alignX ax cx cw iw
            visibleSlice = max 0 (innerH - (curY - cy))
            nodeH =
              if isScrollNode nt
                then min fh visibleSlice
                else fh
        positionNode na host fm ci fx curY cw nodeH
        go rest (curY + fh + gap)
  go pairs cy

resolveSize :: SizingTag -> Float -> Float -> Float -> Float -> Float -> Float
resolveSize SizingFixed v _ _ _ _ = v
resolveSize SizingFit _ intrinsic avail minS maxS = clamp (min intrinsic avail) minS maxS
resolveSize SizingShrink _ intrinsic avail minS maxS = clamp (min intrinsic avail) minS maxS
resolveSize SizingGrow _ _ avail _ maxS = min avail maxS
resolveSize SizingPercent _ _ avail _ maxS = min avail maxS

positionChildren ::
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
positionChildren na host fm idx dir gap pad px py pw ph = do
  wrap <- getWrap na idx
  nt <- getNodeType na idx
  let chrome = isChromeColumn nt dir
      cx = px + padL pad
      cy = py + padT pad
      cw = pw - padL pad - padR pad
      ch = ph - padT pad - padB pad
  case dir of
    DirRow
      | wrap -> positionRowWrap na host fm idx gap cx cy cw ch
      | otherwise -> positionRowFromParent na host fm idx gap cx cy cw ch
    DirColumn -> positionColumnFromParent na host fm idx gap chrome px py pw cx cy cw ch

childRowCrossSize :: NodeArena -> NodeIdx -> Float -> IO Float
childRowCrossSize na ci availCross = do
  (hTag, hVal) <- getHeightSizing na ci
  (_, _, _, intrinsic) <- getRect na ci
  (_, minH, _, maxH) <- getMinMax na ci
  if hTag == SizingGrow || hTag == SizingPercent
    then pure (clamp (resolveSize hTag hVal intrinsic availCross minH maxH) minH maxH)
    else
      -- Fit/Fixed/Shrink keep the measured box. Do not use the wrap-line
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

positionRowFromParent ::
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
positionRowFromParent na host fm parent gap cx cy cw ch = do
  n <- loadChildrenScratchFromParent na parent cw ch
  distributeScratch na 0 n cw (gap * fromIntegral (max 0 (n - 1))) True
  pairs <- snapshotScratchRow na 0 n
  let goRow [] _ = pure ()
      goRow ((ci, fw) : rest) !curX = do
        -- Fit/fixed children keep content height. Only Grow/Percent eat `ch`.
        crossH <- childRowCrossSize na ci ch
        ay <- getAlignY na ci
        let fy = alignY ay cy ch crossH
        positionNode na host fm ci curX fy fw crossH
        goRow rest (curX + fw + gap)
  goRow pairs cx

positionRowWrap :: NodeArena -> HostProfile -> FontMetrics -> NodeIdx -> Float -> Float -> Float -> Float -> Float -> IO ()
positionRowWrap na host fm parent gap cx cy cw ch = do
  n <- loadChildrenScratchFromParent na parent cw ch
  -- Snapshot before positionNode. Nested layout reuses the same scratch arrays.
  dims <- snapshotScratchDims na 0 n
  let goLines _ [] = pure ()
      goLines !oy (rowItems : restLines) = do
        lineBudget <- lineRowCrossBudget na rowItems
        nLine <- writeScratchDims na rowItems
        distributeScratch na 0 nLine cw (gap * fromIntegral (max 0 (nLine - 1))) True
        rowEntries <- snapshotScratchRow na 0 nLine
        rowH <- goRow rowEntries cx oy lineBudget lineBudget
        goLines (oy + rowH + gap) restLines
      goRow [] _ _ _ maxH = pure maxH
      goRow ((ci, fw) : es) !curX !oy !lineCross !maxH = do
        crossH <- childRowCrossSize na ci lineCross
        let lineCross' = max lineCross crossH
        ay <- getAlignY na ci
        let fy = alignY ay oy lineCross' crossH
        positionNode na host fm ci curX fy fw crossH
        goRow es (curX + fw + gap) oy lineCross' (max maxH crossH)
  goLines cy (packRowLines dims cw gap)

positionColumnFromParent ::
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
positionColumnFromParent na host fm parent gap chrome px _ pw cx cy cw ch = do
  n <- loadChildrenScratchFromParent na parent cw ch
  gapSum <- columnGapSumScratch na chrome n gap
  distributeScratch na 0 n ch gapSum False
  pairs <- snapshotScratchColumn na 0 n
  let go [] _ = pure ()
      go ((ci, fh) : rest) !curY = do
        nt <- getNodeType na ci
        (fx, nodeW) <-
          if chrome && nt == NodeSeparator
            then pure (px, pw)
            else do
              (_, _, iw, _) <- getRect na ci
              ax <- getAlignX na ci
              pure (alignX ax cx cw iw, cw)
        childH <- columnChildHeight na ci fh
        positionNode na host fm ci fx curY nodeW childH
        gapAfter <-
          case rest of
            [] -> pure 0
            ((nextCi, _) : _) -> pairColumnGap na chrome ci nextCi gap
        go rest (curY + childH + gapAfter)
  go pairs cy

snapshotScratchDims :: NodeArena -> Int -> Int -> IO [(NodeIdx, Float, Float)]
snapshotScratchDims na off n = do
  idxArr <- readIORef (naScratchIdx na)
  wArr <- readIORef (naScratchMain na)
  hArr <- readIORef (naScratchCross na)
  forM [off .. off + n - 1] $ \i -> do
    ci <- readPrimArray idxArr i
    w <- readPrimArray wArr i
    h <- readPrimArray hArr i
    pure (ci, w, h)

writeScratchDims :: NodeArena -> [(NodeIdx, Float, Float)] -> IO Int
writeScratchDims na items = do
  let n = length items
  ensureScratchCapacity na n
  idxArr <- readIORef (naScratchIdx na)
  mainArr <- readIORef (naScratchMain na)
  crossArr <- readIORef (naScratchCross na)
  let write !i [] = do
        writeIORef (naScratchCount na) i
        pure i
      write !i ((ci, w, h) : rest) = do
        writePrimArray idxArr i ci
        writePrimArray mainArr i w
        writePrimArray crossArr i h
        write (i + 1) rest
  write 0 items

packRowLines :: [(NodeIdx, Float, Float)] -> Float -> Float -> [[(NodeIdx, Float, Float)]]
packRowLines dims avail gap = reverse (go 0 [] 0 [])
  where
    n = length dims
    go i curLine curW acc
      | i >= n = finalize curLine acc
      | otherwise =
          let item = dims !! i
              w = snd3 item
              need = if null curLine then w else w + gap
           in if null curLine || curW + need <= avail + 0.001
                then go (i + 1) (item : curLine) (curW + need) acc
                else go i [] 0 (reverse curLine : acc)
    finalize [] acc = acc
    finalize cur acc = reverse cur : acc

lineCrossSize :: [(NodeIdx, Float, Float)] -> Float
lineCrossSize line =
  if null line then 0 else maximum (map thd3 line)

lineRowCrossBudget :: NodeArena -> [(NodeIdx, Float, Float)] -> IO Float
lineRowCrossBudget na items = do
  let intrinsic = lineCrossSize items
  mins <-
    forM items $ \(ci, _, h) -> do
      (_, minH, _, _) <- getMinMax na ci
      pure (max h minH)
  pure (max intrinsic (if null mins then 0 else maximum mins))

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

snapshotScratchRow :: NodeArena -> Int -> Int -> IO [(NodeIdx, Float)]
snapshotScratchRow na off n = do
  idxArr <- readIORef (naScratchIdx na)
  outW <- readIORef (naScratchOutMain na)
  forM [off .. off + n - 1] $ \i -> do
    ci <- readPrimArray idxArr i
    fw <- readPrimArray outW i
    pure (ci, fw)

snapshotScratchColumn :: NodeArena -> Int -> Int -> IO [(NodeIdx, Float)]
snapshotScratchColumn na off n = do
  idxArr <- readIORef (naScratchIdx na)
  outH <- readIORef (naScratchOutCross na)
  forM [off .. off + n - 1] $ \i -> do
    ci <- readPrimArray idxArr i
    fh <- readPrimArray outH i
    pure (ci, fh)

loadChildrenScratchFromParent :: NodeArena -> NodeIdx -> Float -> Float -> IO Int
loadChildrenScratchFromParent na parent availW availH = do
  fc <- getFirstChild na parent
  count <- countLayoutChildren na fc
  ensureScratchCapacity na count
  writeIORef (naScratchCount na) count
  if count <= 0
    then pure 0
    else do
      idxArr <- readIORef (naScratchIdx na)
      mainArr <- readIORef (naScratchMain na)
      crossArr <- readIORef (naScratchCross na)
      void (fillLayoutChildren na fc idxArr mainArr crossArr availW availH)
      pure count

countLayoutChildren :: NodeArena -> NodeIdx -> IO Int
countLayoutChildren na ci
  | ci < 0 = pure 0
  | otherwise = do
      nt <- getNodeType na ci
      ns <- getNextSibling na ci
      rest <- countLayoutChildren na ns
      pure (if isFloatingNode nt then rest else 1 + rest)

fillLayoutChildren ::
  NodeArena ->
  NodeIdx ->
  MutablePrimArray RealWorld Int ->
  MutablePrimArray RealWorld Float ->
  MutablePrimArray RealWorld Float ->
  Float ->
  Float ->
  IO Int
fillLayoutChildren na ci idxArr mainArr crossArr availW availH
  | ci < 0 = pure 0
  | otherwise = do
      nt <- getNodeType na ci
      ns <- getNextSibling na ci
      slot <- fillLayoutChildren na ns idxArr mainArr crossArr availW availH
      if isFloatingNode nt
        then pure slot
        else do
          writeScratchEntry na ci slot idxArr mainArr crossArr availW availH
          pure (slot + 1)

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
          SizingPercent -> clamp (availW * wVal / 100) minW maxW
          _ -> w
      h' =
        case hTag of
          SizingPercent -> clamp (availH * hVal / 100) minH maxH
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

clamp :: Float -> Float -> Float -> Float
clamp v lo hi = max lo (min hi v)

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
                  w = clamp w0 minW (min maxW winW)
                  h = clamp h0 minH (min maxH winH)
              mpos <- lookupPos wid
              let (x0, y0) = maybe (max 0 (winW - w - margin), margin) id mpos
                  x = clamp x0 0 (max 0 (winW - w))
                  y = clamp y0 0 (max 0 (winH - h))
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
  positionChildren na host fm idx dir gap pad x y w h
