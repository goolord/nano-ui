{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Table
  ( SortDir (..)
  , SortCol (..)
  , ColSize (..)
  , TableCfg (..)
  , TableResponse (..)
  , defaultTableCfg
  , table
  , tableEx
  , tableCfg
  , useTableSort
  , tableRespChanged
  , tableRespClicked
  , tableHiddenIndices
  , sortRows
  , Colonnade
  , Headed (..)
  , headed
  , headless
  )
where

import Colonnade (Colonnade, Headed (..), headed, headless)
import Colonnade.Encode qualified as Encode
import Control.Monad (void, when)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (sortBy)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read (decimal, signed)
import Data.Vector qualified as V
import Effectful (Eff, type (:>))
import qualified Data.IntMap.Strict as IM
import NanoUI.Context (Context (..), bumpMirror, getPrevRect, getScrollOffset2D, getStore, intKey, linkScrollAxes, markDirty, setStore)
import NanoUI.Font (monoFontMarker, scrollBarGutter, stripWidgetMarkers, tableCellInset, textDisplayWidth)
import NanoUI.Id (WidgetId (..))
import NanoUI.Input (Input (..), inputMouseDown, inputMousePos, inputMousePressed, inputMouseReleased, inputMouseRightReleased)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Store (WidgetStore (..), slotDrag, slotDragW, slotKey)
import NanoUI.Style (AlignX (..), AlignY (..), Direction (..), Layout (..), Padding (..), Sizing (..), defaultLayout, fillH, fillW, tight)
import NanoUI.Types (isCellHost, rectH, rectW, v2X, V2 (..))
import NanoUI.WidgetText (tableHeaderLabel, tableSortReserve)
import NanoUI.Widgets.Behavior (useReorder)
import NanoUI.Widgets.Combinators
  ( buttonStyled
  , fitList
  , gridColumnsLay
  , headerAtPoint
  , headerEdgeHit
  , keyedRowLay
  , listAt
  , minColW
  , normalizeOrder
  , rebuildOrder
  , setAt
  , stripedRow
  , virtualIndices
  , visibleCols
  )
import NanoUI.Widgets.Layout (column, panel, row, scrollAreaIdConfigured, separator, spacer)
import NanoUI.Frame.Scroll.Geometry (ScrollConfig (..), ScrollPolicy (..), scrollHorizontalAuto, scrollVerticalAuto, scrollVerticalHidden)
import NanoUI.Widgets.Node
  ( Clickable (..)
  , Responding (..)
  , Response (..)
  , RightClickable (..)
  , addWidgetStyled
  , rawRespRect
  , setChanged
  , setClicked
  , tagContainer
  )
tableStretchAny :: TableCfg -> Bool
tableStretchAny cfg = any (== ColStretch) (tableColSizes cfg)

tableFillInner :: TableCfg -> Layout -> Bool
tableFillInner cfg outer =
  tableStretchAny cfg
    || case layoutWidth outer of
      Grow _ -> True
      _ -> False

tableSplitPanes ::
  (Ui :> es) =>
  Bool ->
  WidgetId ->
  WidgetId ->
  WidgetId ->
  Float ->
  [Int] ->
  [Int] ->
  [row] ->
  [row] ->
  (Int -> Layout) ->
  (Int -> Eff es Response) ->
  (Int -> row -> Int -> Eff es ()) ->
  Eff es [(Int, Response)]
tableSplitPanes fillInner tableWid vWid hWid rowMinH frozenIdx unfrozenIdx pinned scrollRows colBox renderHeader renderCell =
  let paneRoot =
        (if fillInner then tight . fillW . fillH else tight . fillH) defaultLayout
   in panel paneRoot $ do
        tagContainer tableWid
        row (paneRoot {layoutGap = 0}) $ do
          frozenHs <-
            if null frozenIdx
              then pure []
              else zip frozenIdx <$> pane False (not (null unfrozenIdx)) frozenIdx
          when (not (null frozenIdx) && not (null unfrozenIdx)) $ void separator
          unfrozenHs <-
            if null unfrozenIdx then pure [] else zip unfrozenIdx <$> unfrozenPane unfrozenIdx
          pure (frozenHs ++ unfrozenHs)
 where
  freezeR = length pinned
  minSum idxs = sum (map (layoutMinW . colBox) idxs) + fromIntegral (max 0 (length idxs - 1))
  vLay fill =
    let base = tight . fillH $ defaultLayout {layoutGap = 0}
     in if fill then fillW base else base
  hRowLay = defaultLayout {layoutDirection = Row, layoutPadding = Padding 0 0 0 0, layoutGap = 0}
  paneLay fill idxs =
    let base = tight $ defaultLayout {layoutGap = 0, layoutHeight = Grow 1}
     in if fill then fillW (fillH base) else base {layoutWidth = Fit, layoutMinW = minSum idxs}
  gridRowLay idxs =
    (if fillInner then fillW else id) (tight $ defaultLayout {layoutGap = 0, layoutMinW = minSum idxs})
  headerLine idxs renderHeader' =
    keyedRowLay (gridRowLay idxs) idxs $ \i ->
      column (colBox i) (renderHeader' i)
  pinnedBlock idxs =
    mapM_
      ( \(ri, r) ->
          withKey ("pin" :: Text, ri) $ do
            when (ri > 0) $ void separator
            gridColumnsLay
              (gridRowLay idxs)
              idxs
              (map colBox idxs)
              [void (renderCell ri r i) | i <- idxs]
      )
      (zip [0 ..] pinned)
  bodyBlock scrollWid virtualize idxs = do
    ctx <- askContext
    let n = length scrollRows
    (vis, topH, botH) <-
      if n == 0 || rowMinH <= 0
        then pure ([], 0, 0)
        else if not virtualize
          then pure ([0 .. n - 1], 0, 0)
          else uiIO $ do
            V2 _ scrollY <- getScrollOffset2D ctx scrollWid
            viewH <-
              getPrevRect ctx scrollWid >>= \case
                Nothing -> pure (rowMinH * 8)
                Just r -> pure (rectH r)
            let vis = virtualIndices n scrollY viewH rowMinH
                firstVis = case vis of
                  [] -> 0
                  (i : _) -> i
                lastVis = case vis of
                  [] -> -1
                  xs -> last xs
                topH = fromIntegral firstVis * rowMinH
                botH = fromIntegral (max 0 (n - lastVis - 1)) * rowMinH
            pure (vis, topH, botH)
    column ((if fillInner then fillW else id) (tight $ defaultLayout {layoutGap = 0, layoutMinW = minSum idxs})) $ do
      when (topH > 0) $ void (spacer Fit (Fixed topH))
      gridColumnsLay
        (gridRowLay idxs)
        idxs
        (map colBox idxs)
        [ mapM_
            ( \rowIdx ->
                withKey rowIdx $ do
                  when (rowIdx > 0) $ void separator
                  let r = scrollRows !! rowIdx
                  renderCell (rowIdx + freezeR) r colIdx
            )
            vis
        | colIdx <- idxs
        ]
      when (botH > 0) $ void (spacer Fit (Fixed botH))
  pane fill hideVertBar idxs = do
    column (paneLay fill idxs) $ do
      hs <- headerLine idxs renderHeader
      void separator
      pinnedBlock idxs
      when (not (null pinned) && not (null scrollRows)) $ void separator
      scrollAreaIdConfigured
        vWid
        (vLay fill)
        (if hideVertBar then scrollVerticalHidden else scrollVerticalAuto)
        (bodyBlock vWid True idxs)
      pure hs
  unfrozenPane idxs = do
    ctx <- askContext
    let host = ctxHostProfile ctx
        fm = ctxFontMetrics ctx
        vGutter = if isCellHost host then 1 else scrollBarGutter host fm + 2
    mPrevV <- uiIO (getPrevRect ctx vWid)
    let totalH = fromIntegral (length scrollRows) * rowMinH
        hasVertBar = maybe (totalH > 100) (\r -> totalH > rectH r) mPrevV
    column (paneLay fillInner idxs) $ do
      hs <-
        row (tight . (if fillInner then fillW else id) $ defaultLayout {layoutGap = 0}) $ do
          hs' <-
            scrollAreaIdConfigured
              hWid
              (if fillInner then fillW (hRowLay {layoutMinW = minSum idxs}) else hRowLay)
              scrollHorizontalAuto $
              column ((if fillInner then fillW else id) (tight $ defaultLayout {layoutGap = 0, layoutMinW = minSum idxs})) $ do
                hs'' <- headerLine idxs renderHeader
                void separator
                pinnedBlock idxs
                when (not (null pinned) && not (null scrollRows)) $ void separator
                pure hs''
          when hasVertBar $ void (spacer (Fixed vGutter) Fit)
          pure hs'
      uiIO (linkScrollAxes ctx vWid hWid)
      scrollAreaIdConfigured
        vWid
        (vLay fillInner)
        (ScrollConfig ScrollHidden ScrollAuto True)
        (bodyBlock vWid True idxs)
      pure hs

data SortDir = SortAsc | SortDesc
  deriving (Eq, Show, Enum, Bounded)

data SortCol = SortCol {sortColIndex :: !Int, sortColDir :: !SortDir}
  deriving (Eq, Show)

data ColSize = ColContent | ColStretch | ColFixed Float
  deriving (Eq, Show)

data TableCfg = TableCfg
  { tableFreezeCols :: !Int
  , tableFreezeRows :: !Int
  , tableColSizes :: [ColSize]
  , tableHidden :: IntSet
  }
  deriving (Eq, Show)

defaultTableCfg :: TableCfg
defaultTableCfg = TableCfg 0 0 [] IS.empty

data TableResponse = TableResponse
  { tableWidgetResponse :: !Response
  , tableSort :: !SortCol
  , tableColOrder :: [Int]
  , tableHiddenCols :: IntSet
  }
  deriving (Eq, Show)

instance Responding TableResponse where
  respId = respId . tableWidgetResponse
  respRect = respRect . tableWidgetResponse
  respHovered = respHovered . tableWidgetResponse
  respPressed = respPressed . tableWidgetResponse
  respClicked = respClicked . tableWidgetResponse
  respChanged = respChanged . tableWidgetResponse
  respRightPressed = respRightPressed . tableWidgetResponse
  respRightClicked = respRightClicked . tableWidgetResponse

instance Clickable TableResponse where
  respIsClicked = respClicked

instance RightClickable TableResponse where
  respIsRightClicked = respRightClicked . tableWidgetResponse

tableRespChanged :: TableResponse -> Bool
tableRespChanged = respChanged

tableRespClicked :: TableResponse -> Bool
tableRespClicked = respClicked

tableHiddenIndices :: TableResponse -> [Int]
tableHiddenIndices = IS.toAscList . tableHiddenCols

packSort :: SortCol -> Int
packSort (SortCol c SortAsc) = c * 2
packSort (SortCol c SortDesc) = c * 2 + 1

unpackSort :: Int -> SortCol
unpackSort n = SortCol (n `div` 2) (if odd n then SortDesc else SortAsc)

clampSortCol :: Int -> SortCol -> SortCol
clampSortCol n (SortCol idx dir) = SortCol (max 0 (min (max 0 (n - 1)) idx)) dir

sortMarkStyle :: SortCol -> Int -> Int
sortMarkStyle sort idx
  | sortColIndex sort /= idx = 0
  | sortColDir sort == SortDesc = 2
  | otherwise = 1

sortRows :: Colonnade Headed row Text -> SortCol -> [row] -> [row]
sortRows _ _ [] = []
sortRows cols sort rows =
  let n = V.length (Encode.getColonnade cols)
      idx = sortColIndex (clampSortCol n sort)
      enc = maybe (const T.empty) Encode.oneColonnadeEncode (Encode.getColonnade cols V.!? idx)
   in case sortColDir sort of
        SortAsc -> sortBy (\a b -> compare (enc a) (enc b)) rows
        SortDesc -> sortBy (\a b -> compare (enc b) (enc a)) rows

columnCount :: Colonnade Headed row Text -> Int
columnCount = V.length . Encode.getColonnade

columnHeaders :: Colonnade Headed row Text -> [Text]
columnHeaders cols = V.toList (Encode.header id cols)

columnCells :: Colonnade Headed row Text -> row -> [Text]
columnCells cols r = V.toList (Encode.row id cols r)

isNumericCell :: Text -> Bool
isNumericCell txt =
  let s = T.strip txt
   in not (T.null s)
        && case signed decimal s :: Either String (Integer, Text) of
          Right (_, rest) | T.null rest -> True
          _ -> False

numericColumns :: Colonnade Headed row Text -> [row] -> [Bool]
numericColumns cols rows =
  let n = length (columnHeaders cols)
   in [let cells = [columnCells cols r !! i | r <- rows] in not (null cells) && all isNumericCell cells | i <- [0 .. n - 1]]

columnWidths :: Context -> Colonnade Headed row Text -> [row] -> [Float]
columnWidths ctx cols rows =
  let host = ctxHostProfile ctx
      fm = ctxFontMetrics ctx
      mono = ctxMonoFontMetrics ctx
      terminal = isCellHost host
      (ix, _) = tableCellInset host fm
      cellPadX = 2 * ix
      headerPadX = cellPadX
      measure metrics txt = textDisplayWidth host metrics (stripWidgetMarkers txt)
      hdrs = columnHeaders cols
      numeric = numericColumns cols rows
      hdrW hdr = measure fm (hdr <> tableSortReserve terminal) + headerPadX
      cellW i txt =
        if listAt numeric i False
          then measure mono txt + cellPadX
          else measure fm txt + cellPadX
   in zipWith
        (\hdr i -> maximum (minColW : hdrW hdr : [cellW i (columnCells cols r !! i) | r <- rows]))
        hdrs
        [0 ..]

nextSortCol :: Int -> SortCol -> Int -> SortCol
nextSortCol n cur clicked =
  let clamped = clampSortCol n cur
   in if clicked == sortColIndex clamped
        then SortCol clicked (case sortColDir clamped of SortAsc -> SortDesc; SortDesc -> SortAsc)
        else SortCol clicked SortAsc

useTableSort :: Ui :> es => SortCol -> Eff es (Eff es SortCol, SortCol -> Eff es ())
useTableSort initial = do
  wid <- nextId
  ctx <- askContext
  let key = intKey wid
      packedInitial = packSort initial
      readSort = uiIO $ unpackSort . IM.findWithDefault packedInitial key . storeInt <$> getStore ctx
      setSort sort = uiIO $ do
        st <- getStore ctx
        let packed = packSort sort
            prev = IM.findWithDefault packedInitial key (storeInt st)
        when (prev /= packed) $ do
          setStore ctx (bumpMirror (st {storeInt = IM.insert key packed (storeInt st)}))
          markDirty ctx
  pure (readSort, setSort)

packResize :: Int -> Int
packResize i = -(1000 + i)

packReorder :: Int -> Int
packReorder i = -(2000 + i)

isResizeDrag :: Int -> Bool
isResizeDrag n = n <= -1000 && n > -2000

isReorderDrag :: Int -> Bool
isReorderDrag n = n <= -2000

dragCol :: Int -> Int
dragCol n = abs n `mod` 1000

resolvedWidth :: [ColSize] -> [Float] -> [Float] -> Int -> Float
resolvedWidth sizes contentWs stored i =
  let contentW = max minColW (listAt contentWs i minColW)
      saved = listAt stored i 0
   in case listAt sizes i ColContent of
        ColStretch -> if saved > contentW then saved else contentW
        ColFixed f ->
          let base = max minColW f
           in if saved > 0 then max base saved else base
        ColContent -> if saved > 0 then max contentW saved else contentW

colSizing :: Bool -> [ColSize] -> [Float] -> [Float] -> Int -> Sizing
colSizing fillInner sizes contentWs stored i =
  let saved = listAt stored i 0
      contentW = max minColW (listAt contentWs i minColW)
      hasStretch = any (\s -> case s of ColStretch -> True; _ -> False) (take (length contentWs) (sizes ++ repeat ColContent))
   in if saved > 0
        then Fixed (max minColW saved)
        else case listAt sizes i ColContent of
          ColFixed f -> Fixed (max minColW f)
          ColStretch -> if fillInner then Grow 1 else Fixed contentW
          ColContent ->
            if fillInner && not hasStretch
              then Grow 1
              else Fixed contentW

colBoxLayout :: Sizing -> Float -> Layout
colBoxLayout sizing minCol =
  let base = tight $ defaultLayout {layoutGap = 0, layoutMinW = minCol}
   in case sizing of
        Fixed w -> base {layoutWidth = Fixed w, layoutMaxW = w}
        Grow g -> base {layoutWidth = Grow g}
        _ -> base {layoutWidth = Fit}

writeColW :: Context -> Int -> [Float] -> IO ()
writeColW ctx key ws = do
  st <- getStore ctx
  setStore ctx (st {storeFloatList = IM.insert key ws (storeFloatList st)})
  markDirty ctx

finishTable ::
  (Ui :> es) =>
  Int ->
  Int ->
  Bool ->
  [Int] ->
  [Int] ->
  IS.IntSet ->
  Int ->
  Float ->
  Float ->
  [Float] ->
  [Float] ->
  SortCol ->
  [(Int, Response)] ->
  Maybe Response ->
  (Int -> Float) ->
  Eff es (TableResponse, SortCol)
finishTable n stateKey terminal vis order0 hidden0 drag0 dragX0 dragW0 widths0 widths1 sort0 headerPairs showAllResp resolvedW = do
  ctx <- askContext
  inp <- askInput
  let mouse = inputMousePos inp
      mx = v2X mouse
      edgePad = if terminal then 1 else 4
      edgeCol = headerEdgeHit edgePad headerPairs mouse
      hoverCol = headerAtPoint headerPairs mouse
      headerRects = [(i, rawRespRect r) | (i, r) <- headerPairs]
      resizing = isResizeDrag drag0 && inputMouseDown inp
  (vis', mReorder) <-
    withKey ("reorder" :: Text) $
      useReorder vis (if resizing || isJust edgeCol then [] else headerRects)
  let dragged = isReorderDrag drag0 && abs (mx - dragX0) > 8
      pressResize = inputMousePressed inp && isJust edgeCol
      pressReorder = inputMousePressed inp && edgeCol == Nothing && isJust hoverCol
      nextDrag'
        | pressResize = maybe 0 packResize edgeCol
        | pressReorder = maybe 0 packReorder hoverCol
        | inputMouseReleased inp || not (inputMouseDown inp) = 0
        | otherwise = drag0
      nextDragX
        | pressResize || pressReorder = mx
        | nextDrag' == 0 = 0
        | otherwise = dragX0
      nextDragW
        | pressResize = maybe 0 headerW edgeCol
        | nextDrag' == 0 = 0
        | otherwise = dragW0
      headerW i = maybe (resolvedW i) (\r -> let w = rectW (rawRespRect r) in if w > 0 then w else resolvedW i) (lookup i headerPairs)
      nextOrder = if vis' /= vis then rebuildOrder hidden0 vis' order0 else order0
      hideClicked = [i | (i, r) <- headerPairs, respHovered r, inputMouseRightReleased inp, drag0 == 0]
      nextHidden = case showAllResp of
        Just r | respClicked r -> IS.empty
        _ -> case hideClicked of
          (i : _) | IS.size hidden0 + 1 < n -> IS.insert i hidden0
          _ -> hidden0
      sortClick =
        if dragged || isJust mReorder || vis' /= vis || isResizeDrag drag0
          then Nothing
          else
            if isJust edgeCol && (inputMouseDown inp || inputMouseReleased inp)
              then Nothing
              else listToMaybe [i | (i, r) <- headerPairs, respClicked r]
      nextSort = maybe sort0 (nextSortCol n sort0) sortClick
      hasChanged = nextSort /= sort0 || nextOrder /= order0 || nextHidden /= hidden0 || widths1 /= widths0
      widgetResp =
        setChanged hasChanged $
          setClicked (hasChanged && isJust sortClick) (mconcat (map snd headerPairs ++ maybe [] pure showAllResp))
  uiIO $ do
    st <- getStore ctx
    let st1 =
          st
            { storeIntList = IM.insert stateKey nextOrder (storeIntList st)
            , storeIntSet = IM.insert stateKey nextHidden (storeIntSet st)
            , storeInt = IM.insert (slotKey slotDrag stateKey) nextDrag' (storeInt st)
            , storeFloat =
                IM.insert stateKey nextDragX $
                  IM.insert (slotKey slotDragW stateKey) nextDragW (storeFloat st)
            }
    when (st1 /= st) $ setStore ctx st1 >> markDirty ctx
  pure (TableResponse widgetResp nextSort nextOrder nextHidden, nextSort)


table :: (Ui :> es) => Text -> Colonnade Headed row Text -> [row] -> SortCol -> Eff es (TableResponse, SortCol)
table = tableEx (tight . fillW $ defaultLayout {layoutGap = 0})

tableEx :: (Ui :> es) => Layout -> Text -> Colonnade Headed row Text -> [row] -> SortCol -> Eff es (TableResponse, SortCol)
tableEx = tableCfg defaultTableCfg

tableCfg ::
  (Ui :> es) =>
  TableCfg ->
  Layout ->
  Text ->
  Colonnade Headed row Text ->
  [row] ->
  SortCol ->
  Eff es (TableResponse, SortCol)
tableCfg cfg outerLayout key cols rows curSort =
  withKey ("table:" <> key) $ do
    stateWid <- nextId
    vWid <- nextId
    hWid <- nextId
    tableWid <- nextId
    let n = columnCount cols
        sort0 = clampSortCol n curSort
        stateKey = intKey stateWid
    ctx <- askContext
    inp <- askInput
    st0 <- uiIO (getStore ctx)
    let host = ctxHostProfile ctx
        terminal = isCellHost host
        contentWs = columnWidths ctx cols rows
        sizes = tableColSizes cfg
        order0 = normalizeOrder n (IM.findWithDefault [0 .. n - 1] stateKey (storeIntList st0))
        hidden0 = IM.findWithDefault (tableHidden cfg) stateKey (storeIntSet st0)
        widths0 = fitList n 0 (IM.findWithDefault [] stateKey (storeFloatList st0))
        drag0 = IM.findWithDefault 0 (slotKey slotDrag stateKey) (storeInt st0)
        dragX0 = IM.findWithDefault 0 stateKey (storeFloat st0)
        dragW0 = IM.findWithDefault 0 (slotKey slotDragW stateKey) (storeFloat st0)
        mx = v2X (inputMousePos inp)
        resizing = isResizeDrag drag0 && inputMouseDown inp
        widths1 = if resizing then setAt (dragCol drag0) (max minColW (dragW0 + mx - dragX0)) widths0 else widths0
    when (widths1 /= widths0) $ uiIO $ writeColW ctx stateKey widths1
    let vis = visibleCols order0 hidden0
        freezeN = min (max 0 (tableFreezeCols cfg)) (length vis)
        freezeR = min (max 0 (tableFreezeRows cfg)) (length rows)
        frozenIdx = take freezeN vis
        unfrozenIdx = drop freezeN vis
        sorted = sortRows cols sort0 rows
        pinned = take freezeR sorted
        scrollRows = drop freezeR sorted
        hdrs = columnHeaders cols
        numeric = numericColumns cols rows
        rowMinH = if terminal then 1 else 28
        fillInner = tableFillInner cfg outerLayout
        mins = [resolvedWidth sizes contentWs widths1 i | i <- [0 .. n - 1]]
        colBox i = colBoxLayout (colSizing fillInner sizes contentWs widths1 i) (listAt mins i minColW)
        resolvedW i = listAt mins i minColW
        inner i =
          (tight defaultLayout)
            { layoutWidth = Grow 1
            , layoutHeight = Grow 1
            , layoutAlignX = if listAt numeric i False then AlignEnd else AlignStart
            , layoutAlignY = AlignMiddle
            , layoutMinH = rowMinH
            }
        shown i txt = if listAt numeric i False then monoFontMarker <> txt else txt
        renderHeader i =
          buttonStyled (tableHeaderLabel terminal (listAt hdrs i T.empty)) (if sortColIndex sort0 == i then 1 else 0) (inner i) (sortMarkStyle sort0 i)
        renderCell ri r i = void (stripedRow ri (inner i) (shown i (columnCells cols r !! i)))
    column outerLayout $ do
      showAllResp <-
        if IS.null hidden0
          then pure Nothing
          else fmap Just $ do
            wid <- nextId
            addWidgetStyled wid NodeButton "Show all columns" 0 (tight . fillW $ defaultLayout) 0 Nothing
      headerPairs <-
        tableSplitPanes (tableFillInner cfg outerLayout) tableWid vWid hWid rowMinH frozenIdx unfrozenIdx pinned scrollRows colBox renderHeader renderCell
      finishTable n stateKey terminal vis order0 hidden0 drag0 dragX0 dragW0 widths0 widths1 sort0 headerPairs showAllResp resolvedW
