module NanoUI.Layout.Solve
  (   solveLayout
  , placeModals
  , placeWindows
  , positionNode
  , positionWindowNode
  , scrollBarSlotOf
  ) where

import Control.Monad (when)
import Data.IORef (readIORef, writeIORef)
import Data.Primitive.PrimArray (readPrimArray, writePrimArray)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
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
        (mw, mh) <- measure body
        if isCellHost host
          then pure (mw, mh, 0, 0)
          else do
            let box = checkboxBoxSize host fm
            pure (mw, max mh box, checkboxLeading host fm, 0)
      NodeRadio -> do
        let body =
              if T.null txt
                then " "
                else if isCellHost host then txt else radioLabelText txt
        (mw, mh) <- measure body
        if isCellHost host
          then pure (mw, mh, 0, 0)
          else do
            let box = checkboxBoxSize host fm
            pure (mw, max mh box, checkboxLeading host fm, 0)
      NodeTree -> do
        let (_, _, depth, _, _, raw) = treeParseRow txt
            lbl = if T.null raw then " " else treeLabelText txt
            body = if isCellHost host then treeMeasureLabel depth lbl else lbl
        (mw, mh) <- measure body
        if isCellHost host
          then pure (mw, mh, 0, 0)
          else do
            let box = checkboxBoxSize host fm
            pure (mw, max mh box, treeRowLeading host fm depth, 0)
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
            pure (contentW, lh + gap + fieldH, 0, lh + gap)
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
  (_, _, assignedW, _) <- getRect na idx
  nt <- getNodeType na idx
  children <- collectChildren na idx
  childDims <- collectChildDims na idx
  let chrome = isChromeColumn nt dir
      padX = padL pad + padR pad
      innerMaxW =
        case wTag of
          SizingFixed -> max 0 (wVal - padX)
          _
            | useAssignedWidth && assignedW > 0 ->
                max 0 (assignedW - padX)
            | otherwise -> max 0 (maxW - padX)
  (contentW, contentH) <-
    if wrap && dir == DirRow && innerMaxW > 0
      then pure (foldWrappedRow childDims innerMaxW gap)
      else case dir of
        DirColumn | chrome -> foldChromeColumn na children childDims gap
        _ -> pure (foldChildren dir gap childDims)
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
  childDims <- collectChildDims na idx
  let (contentW, contentH) = foldChildren dir gap childDims
      fullW = contentW + padL pad + padR pad
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

collectChildDims :: NodeArena -> NodeIdx -> IO [(Float, Float)]
collectChildDims na idx = do
  fc <- getFirstChild na idx
  go fc []
  where
    go ci acc =
      if ci < 0
        then pure acc
        else do
          nt <- getNodeType na ci
          ns <- getNextSibling na ci
          if isFloatingNode nt
            then go ns acc
            else do
              (_, _, w, h) <- getRect na ci
              go ns ((w, h) : acc)

foldChildren :: DirTag -> Float -> [(Float, Float)] -> (Float, Float)
foldChildren _ _ [] = (0, 0)
foldChildren DirRow gap dims =
  let ws = map fst dims
      hs = map snd dims
      totalW = sum ws + gap * fromIntegral (max 0 (length ws - 1))
      maxH = if null hs then 0 else maximum hs
   in (totalW, maxH)
foldChildren DirColumn gap dims =
  let ws = map fst dims
      hs = map snd dims
      maxW = if null ws then 0 else maximum ws
      totalH = sum hs + gap * fromIntegral (max 0 (length hs - 1))
   in (maxW, totalH)

isChromeColumn :: NodeType -> DirTag -> Bool
isChromeColumn nt dir =
  dir == DirColumn && (nt == NodeWindow || nt == NodeModal)

pairColumnGap :: NodeArena -> Bool -> NodeIdx -> NodeIdx -> Float -> IO Float
pairColumnGap _ False _ _ gap = pure gap
pairColumnGap na True a b gap = do
  ntA <- getNodeType na a
  ntB <- getNodeType na b
  pure (if ntA == NodeSeparator || ntB == NodeSeparator then 0 else gap)

columnGapSum :: NodeArena -> Bool -> [NodeIdx] -> Float -> IO Float
columnGapSum _ False _ _ = pure 0
columnGapSum _ True [] _ = pure 0
columnGapSum _ True [_] _ = pure 0
columnGapSum na True (a : b : rest) gap = do
  g <- pairColumnGap na True a b gap
  restG <- columnGapSum na True (b : rest) gap
  pure (g + restG)

foldChromeColumn :: NodeArena -> [NodeIdx] -> [(Float, Float)] -> Float -> IO (Float, Float)
foldChromeColumn na children dims gap = do
  let ws = map fst dims
      hs = map snd dims
      maxW = if null ws then 0 else maximum ws
  gapSum <- columnGapSum na True children gap
  pure (maxW, sum hs + gapSum)

foldWrappedRow :: [(Float, Float)] -> Float -> Float -> (Float, Float)
foldWrappedRow dims avail gap =
  let rows = packDimLines dims avail gap
      heights = map lineDimCross rows
      widths = map (lineDimMain gap) rows
   in ( if null widths then 0 else maximum widths
      , if null heights then 0 else sum heights + gap * fromIntegral (max 0 (length heights - 1))
      )

packDimLines :: [(Float, Float)] -> Float -> Float -> [[(Float, Float)]]
packDimLines dims avail gap = reverse (go 0 [] 0 [])
  where
    v = V.fromList dims
    n = V.length v
    go i curLine curW acc
      | i >= n = finalize curLine acc
      | otherwise =
          let item = v V.! i
              (w, _) = item
              need = if null curLine then w else w + gap
           in if null curLine || curW + need <= avail + 0.001
                then go (i + 1) (item : curLine) (curW + need) acc
                -- Oversized children get a row alone; parent may clip or scroll.
                else go i [] 0 (curLine : acc)
    finalize [] acc = acc
    finalize cur acc = cur : acc

lineDimMain :: Float -> [(Float, Float)] -> Float
lineDimMain gap line =
  let ws = map fst line
   in sum ws + gap * fromIntegral (max 0 (length ws - 1))

lineDimCross :: [(Float, Float)] -> Float
lineDimCross line =
  if null line then 0 else maximum (map snd line)

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
  children <- collectChildren na idx
  case dir of
    DirRow -> positionRow na host fm children gap cx cy contentSize (innerH - gutterRow)
    DirColumn -> positionColumnScroll na host fm children gap cx cy (innerW - gutterCol) innerH contentSize

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
  [NodeIdx] ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
positionColumnScroll na host fm children gap cx cy innerW innerH contentSize = do
  n <- loadChildrenScratch na children innerW innerH
  distributeScratch na 0 n contentSize (gap * fromIntegral (max 0 (n - 1))) False
  pairs <- freezeScratch na 0 n True
  let go _ [] = pure ()
      go !curY ((ci, _, fh) : rest) = do
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
        go (curY + fh + gap) rest
  go cy pairs

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
  children <- collectChildren na idx
  case dir of
    DirRow
      | wrap -> positionRowWrap na host fm children gap cx cy cw ch
      | otherwise -> positionRow na host fm children gap cx cy cw ch
    DirColumn -> positionColumn na host fm children gap chrome px py pw cx cy cw ch

-- Children are linked newest-first, so prepending while walking restores
-- declaration order.
collectChildren :: NodeArena -> NodeIdx -> IO [NodeIdx]
collectChildren na idx = do
  fc <- getFirstChild na idx
  go fc []
  where
    go ci acc =
      if ci < 0
        then pure acc
        else do
          nt <- getNodeType na ci
          ns <- getNextSibling na ci
          if isFloatingNode nt
            then go ns acc
            else go ns (ci : acc)

childRowCrossSize :: NodeArena -> NodeIdx -> Float -> IO Float
childRowCrossSize na ci availCross = do
  (_, _, _, intrinsic) <- getRect na ci
  (_, minH, _, maxH) <- getMinMax na ci
  (hTag, hVal) <- getHeightSizing na ci
  pure (clamp (resolveSize hTag hVal intrinsic availCross minH maxH) minH maxH)

positionRow :: NodeArena -> HostProfile -> FontMetrics -> [NodeIdx] -> Float -> Float -> Float -> Float -> Float -> IO ()
positionRow na host fm children gap cx cy cw ch = do
  n <- loadChildrenScratch na children cw ch
  distributeScratch na 0 n cw (gap * fromIntegral (max 0 (n - 1))) True
  pairs <- freezeScratch na 0 n True
  let go _ [] = pure ()
      go !curX ((ci, fw, _) : rest) = do
        crossH <- childRowCrossSize na ci ch
        ay <- getAlignY na ci
        let fy = alignY ay cy ch crossH
        positionNode na host fm ci curX fy fw crossH
        go (curX + fw + gap) rest
  go cx pairs

positionRowWrap :: NodeArena -> HostProfile -> FontMetrics -> [NodeIdx] -> Float -> Float -> Float -> Float -> Float -> IO ()
positionRowWrap na host fm children gap cx cy cw ch = do
  n <- loadChildrenScratch na children cw ch
  dims <- freezeScratch na 0 n False
  let goLines _ [] = pure ()
      goLines !oy (rowItems : restLines) = do
        let rowH = lineCrossSize rowItems
        nLine <- writeScratchDims na rowItems
        distributeScratch na 0 nLine cw (gap * fromIntegral (max 0 (nLine - 1))) True
        pairs <- freezeScratch na 0 nLine True
        let goRow _ [] = pure ()
            goRow !curX ((ci, fw, _) : restRow) = do
              crossH <- childRowCrossSize na ci rowH
              ay <- getAlignY na ci
              let fy = alignY ay oy rowH crossH
              positionNode na host fm ci curX fy fw crossH
              goRow (curX + fw + gap) restRow
        goRow cx pairs
        goLines (oy + rowH + gap) restLines
  goLines cy (packRowLines dims cw gap)

positionColumn ::
  NodeArena ->
  HostProfile ->
  FontMetrics ->
  [NodeIdx] ->
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
positionColumn na host fm children gap chrome px _ pw cx cy cw ch = do
  gapSum <- columnGapSum na chrome children gap
  n <- loadChildrenScratch na children cw ch
  distributeScratch na 0 n ch gapSum False
  pairs <- freezeScratch na 0 n True
  let go _ [] = pure ()
      go !curY ((ci, _, fh) : rest) = do
        nt <- getNodeType na ci
        (fx, fw) <-
          if chrome && nt == NodeSeparator
            then pure (px, pw)
            else do
              (_, _, iw, _) <- getRect na ci
              ax <- getAlignX na ci
              pure (alignX ax cx cw iw, cw)
        positionNode na host fm ci fx curY fw fh
        gapAfter <-
          case rest of
            [] -> pure 0
            ((nextCi, _, _) : _) -> pairColumnGap na chrome ci nextCi gap
        go (curY + fh + gapAfter) rest
  go cy pairs

loadChildrenScratch :: NodeArena -> [NodeIdx] -> Float -> Float -> IO Int
loadChildrenScratch na children availW availH = do
  let n = length children
  ensureScratchCapacity na n
  idxArr <- readIORef (naScratchIdx na)
  mainArr <- readIORef (naScratchMain na)
  crossArr <- readIORef (naScratchCross na)
  let write !i [] = do
        writeIORef (naScratchCount na) i
        pure i
      write !i (ci : rest) = do
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
        write (i + 1) rest
  write 0 children

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

-- Snapshot scratch before positionNode. Nested layout reuses the same arrays.
freezeScratch :: NodeArena -> Int -> Int -> Bool -> IO [(NodeIdx, Float, Float)]
freezeScratch na off n useOut = do
  idxArr <- readIORef (naScratchIdx na)
  wArr <- readIORef (if useOut then naScratchOutMain na else naScratchMain na)
  hArr <- readIORef (if useOut then naScratchOutCross na else naScratchCross na)
  let end = off + n
      go !i acc
        | i >= end = pure (reverse acc)
        | otherwise = do
            ci <- readPrimArray idxArr i
            w <- readPrimArray wArr i
            h <- readPrimArray hArr i
            go (i + 1) ((ci, w, h) : acc)
  go off []

packRowLines :: [(NodeIdx, Float, Float)] -> Float -> Float -> [[(NodeIdx, Float, Float)]]
packRowLines dims avail gap = reverse (go 0 [] 0 [])
  where
    n = length dims
    go i curLine curW acc
      | i >= n = finalize curLine acc
      | otherwise =
          let item = dims !! i
              (w, _) = (snd3 item, thd3 item)
              need = if null curLine then w else w + gap
           in if null curLine || curW + need <= avail + 0.001
                then go (i + 1) (item : curLine) (curW + need) acc
                else go i [] 0 (curLine : acc)
    finalize [] acc = acc
    finalize cur acc = cur : acc

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

lineCrossSize :: [(NodeIdx, Float, Float)] -> Float
lineCrossSize line =
  if null line then 0 else maximum (map thd3 line)

-- Write resolved (width, height) into outMain / outCross for [off, off+n).
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
