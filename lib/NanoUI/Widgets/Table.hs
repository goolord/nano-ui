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
import NanoUI.Context (Context (..), getStore, intKey, markDirty, setStore)
import NanoUI.Font (measureText, monoFontMarker, scrollBarGutter)
import NanoUI.Host (isCellHost)
import NanoUI.Input
  ( Input (..)
  , inputMouseDown
  , inputMousePos
  , inputMousePressed
  , inputMouseReleased
  , inputMouseRightReleased
  )
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Store (WidgetStore (..), slotDrag, slotDragW, slotKey)
import NanoUI.Style
  ( AlignX (..)
  , AlignY (..)
  , Direction (..)
  , Layout (..)
  , Padding (..)
  , Sizing (..)
  , defaultLayout
  , fillH
  , fillW
  , tight
  )
import NanoUI.Types (Rect (..), V2 (..), rectContains, v2X, v2Y)
import NanoUI.WidgetText (tableHeaderLabel, tableScrollSlaveStyle, tableSortReserve)
import NanoUI.Widgets.Behavior (useReorder)
import NanoUI.Widgets.Combinators (buttonStyled, stripedRow)
import NanoUI.Widgets.Layout (column, panel, row, scrollAreaId, separator, spacer)
import NanoUI.Widgets.Node
  ( Clickable (..)
  , Responding (..)
  , Response (..)
  , addWidgetStyled
  , setChanged
  , setClicked
  )

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

instance Clickable TableResponse where
  respIsClicked = respClicked

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
   in [ let cells = [columnCells cols r !! i | r <- rows]
         in not (null cells) && all isNumericCell cells
      | i <- [0 .. n - 1]
      ]

columnWidths :: Context -> Colonnade Headed row Text -> [row] -> [Float]
columnWidths ctx cols rows =
  let host = ctxHostProfile ctx
      fm = ctxFontMetrics ctx
      measure txt =
        let (w, _) = measureText host fm txt
         in w + if isCellHost host then 0 else 20
      hdrs = columnHeaders cols
      pad = tableSortReserve (isCellHost host)
      nCols = length hdrs
   in zipWith
        (\hdr i -> maximum (measure (hdr <> pad) : [measure (columnCells cols r !! i) | r <- rows]))
        hdrs
        [0 .. nCols - 1]

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
          setStore ctx (st {storeInt = IM.insert key packed (storeInt st)})
          markDirty ctx
  pure (readSort, setSort)

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
        mouse = inputMousePos inp
        resizing = isResizeDrag drag0 && inputMouseDown inp
        widths1 =
          if resizing then setAt (dragCol drag0) (max minColW (dragW0 + mx - dragX0)) widths0 else widths0
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
        cellPad = if terminal then Padding 0 0 0 0 else Padding 10 8 10 8
        rowMinH = if terminal then 1 else 28
        resolvedW i = resolvedWidth sizes contentWs widths1 i
        itemLayout i =
          (colBoxLayout (colSizing sizes widths1 i) (resolvedW i))
            { layoutAlignX = if listAt numeric i False then AlignEnd else AlignStart
            , layoutAlignY = AlignMiddle
            , layoutPadding = cellPad
            , layoutMinH = rowMinH
            }
        shown i txt = if listAt numeric i False then monoFontMarker <> txt else txt
        minSum idxs = sum (map resolvedW idxs) + fromIntegral (max 0 (length idxs - 1))
        vLay fill = let base = tight . fillH $ defaultLayout {layoutGap = 0} in if fill then fillW base else base
        hRowLay = defaultLayout {layoutDirection = Row, layoutPadding = Padding 0 0 0 0, layoutGap = 0}
        hChromeH = if terminal then 1 else scrollBarGutter host (ctxFontMetrics ctx)
        paneLay fill idxs =
          let base = tight $ defaultLayout {layoutGap = 0, layoutMinW = minSum idxs, layoutHeight = Grow 1}
           in if fill then fillW base else base {layoutWidth = Fit}
        renderHeader i =
          buttonStyled
            (tableHeaderLabel terminal (listAt hdrs i T.empty))
            (if sortColIndex sort0 == i then 1 else 0)
            (itemLayout i)
            (sortMarkStyle sort0 i)
        renderCell ri r i = void (stripedRow ri (itemLayout i) (shown i (columnCells cols r !! i)))
        headerRow idxs =
          row (tight . fillW $ defaultLayout {layoutGap = 0}) $
            mapM
              ( \(k, i) -> do
                  when (k > 0) $ void separator
                  withKey i (renderHeader i)
              )
              (zip [0 :: Int ..] idxs)
        pinnedBlock idxs =
          mapM_
            ( \(ri, r) ->
                withKey ("pin" :: Text, ri) $ do
                  when (ri > 0) $ void separator
                  void $
                    row (tight . fillW $ defaultLayout {layoutGap = 0, layoutMinH = rowMinH}) $
                      mapM_
                        ( \(k, i) -> do
                            when (k > 0) $ void separator
                            withKey i (renderCell ri r i)
                        )
                        (zip [0 :: Int ..] idxs)
            )
            (zip [0 :: Int ..] pinned)
        bodyBlock idxs =
          row (tight . fillW $ defaultLayout {layoutGap = 0}) $
            mapM_
              ( \(k, i) -> do
                  when (k > 0) $ void separator
                  withKey i $
                    column (colBoxLayout (colSizing sizes widths1 i) (resolvedW i)) $
                      mapM_
                        ( \(ri, r) ->
                            withKey ri $ do
                              when (ri > 0) $ void separator
                              renderCell (ri + freezeR) r i
                        )
                        (zip [0 :: Int ..] scrollRows)
              )
              (zip [0 :: Int ..] idxs)
        pane fill slaveStyle idxs = do
          column (paneLay fill idxs) $ do
            hs <- headerRow idxs
            void separator
            pinnedBlock idxs
            when (not (null pinned) && not (null scrollRows)) $ void separator
            scrollAreaId vWid (vLay fill) slaveStyle (bodyBlock idxs)
            pure hs
        unfrozenPane idxs = do
          column (paneLay True idxs) $ do
            hs <-
              scrollAreaId hWid (fillW hRowLay) tableScrollSlaveStyle $
                column (tight . fillW $ defaultLayout {layoutGap = 0, layoutMinW = minSum idxs}) $ do
                  hs <- headerRow idxs
                  void separator
                  pinnedBlock idxs
                  when (not (null pinned) && not (null scrollRows)) $ void separator
                  pure hs
            scrollAreaId vWid (vLay True) 0 $
              scrollAreaId hWid (fillW . fillH $ hRowLay) tableScrollSlaveStyle (bodyBlock idxs)
            scrollAreaId hWid (fillW $ hRowLay {layoutHeight = Fixed hChromeH, layoutMinH = hChromeH, layoutMaxH = hChromeH}) 0 $
              void (spacer (Fixed (max minColW (minSum idxs))) (Fixed 1))
            pure hs
    column outerLayout $ do
      showAllResp <-
        if IS.null hidden0
          then pure Nothing
          else fmap Just $ do
            wid <- nextId
            addWidgetStyled wid NodeButton "Show all columns" 0 (tight . fillW $ defaultLayout) 0 Nothing
      headerPairs <-
        panel (tight . fillW . fillH $ defaultLayout) $
          row (tight . fillW . fillH $ defaultLayout {layoutGap = 0}) $ do
            frozenHs <-
              if null frozenIdx
                then pure []
                else zip frozenIdx <$> pane False (if null unfrozenIdx then 0 else tableScrollSlaveStyle) frozenIdx
            when (not (null frozenIdx) && not (null unfrozenIdx)) $ void separator
            unfrozenHs <-
              if null unfrozenIdx then pure [] else zip unfrozenIdx <$> unfrozenPane unfrozenIdx
            pure (frozenHs ++ unfrozenHs)
      let edgePad = if terminal then 1 else 4
          edgeCol = headerEdgeHit edgePad headerPairs mouse
          hoverCol = headerAtPoint headerPairs mouse
          headerRects = [(i, rawRespRect r) | (i, r) <- headerPairs]
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
          nextOrder =
            if vis' /= vis then rebuildOrder hidden0 vis' order0 else order0
          hideClicked = [i | (i, r) <- headerPairs, respHovered r, inputMouseRightReleased inp, drag0 == 0]
          nextHidden = case showAllResp of
            Just r | respClicked r -> IS.empty
            _ -> case hideClicked of
              (i : _) | IS.size hidden0 + 1 < n -> IS.insert i hidden0
              _ -> hidden0
          sortClick =
            if dragged || isJust mReorder || vis' /= vis || isResizeDrag drag0 || isJust edgeCol
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
      pure
        ( TableResponse widgetResp nextSort nextOrder nextHidden
        , nextSort
        )

minColW :: Float
minColW = 40

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

fitList :: Int -> a -> [a] -> [a]
fitList n d xs = take n (xs ++ repeat d)

setAt :: Int -> a -> [a] -> [a]
setAt i x xs
  | i < 0 || i >= length xs = xs
  | otherwise = take i xs ++ x : drop (i + 1) xs

normalizeOrder :: Int -> [Int] -> [Int]
normalizeOrder n stored =
  let valid = filter (\i -> i >= 0 && i < n) stored
      seen = IS.fromList valid
   in valid ++ [i | i <- [0 .. n - 1], not (IS.member i seen)]

visibleCols :: [Int] -> IntSet -> [Int]
visibleCols order hidden = filter (`IS.notMember` hidden) order

resolvedWidth :: [ColSize] -> [Float] -> [Float] -> Int -> Float
resolvedWidth sizes contentWs stored i =
  let contentW = max minColW (listAt contentWs i minColW)
      saved = listAt stored i 0
   in if saved > 0
        then saved
        else case listAt sizes i ColStretch of
          ColFixed f -> max minColW f
          _ -> contentW

colSizing :: [ColSize] -> [Float] -> Int -> Sizing
colSizing sizes stored i =
  let saved = listAt stored i 0
   in if saved > 0
        then Fixed saved
        else case listAt sizes i ColStretch of
          ColFixed f -> Fixed (max minColW f)
          ColContent -> Fit
          ColStretch -> Grow 1

colBoxLayout :: Sizing -> Float -> Layout
colBoxLayout sizing resolved =
  let base = tight $ defaultLayout {layoutGap = 0, layoutMinW = resolved}
   in case sizing of
        Fixed w -> base {layoutWidth = Fixed w, layoutMaxW = w}
        Grow _ -> fillW base
        _ -> base {layoutWidth = Fit}

headerEdgeHit :: Float -> [(Int, Response)] -> V2 -> Maybe Int
headerEdgeHit pad cols mouse =
  listToMaybe
    [ i
    | (i, r) <- cols
    , let Rect x y w h = rawRespRect r
    , w > 0 && h > 0
    , let mx = v2X mouse
          my = v2Y mouse
    , my >= y && my <= y + h
    , abs (mx - (x + w)) <= pad
    ]

headerAtPoint :: [(Int, Response)] -> V2 -> Maybe Int
headerAtPoint cols mouse = listToMaybe [i | (i, r) <- cols, rectContains (rawRespRect r) mouse]

rebuildOrder :: IntSet -> [Int] -> [Int] -> [Int]
rebuildOrder hidden newVis old =
  let go [] vs = vs
      go (i : is) vs
        | IS.member i hidden = i : go is vs
        | otherwise = case vs of
            (v : vs') -> v : go is vs'
            [] -> i : is
   in go old newVis

writeColW :: Context -> Int -> [Float] -> IO ()
writeColW ctx key ws = do
  st <- getStore ctx
  setStore ctx (st {storeFloatList = IM.insert key ws (storeFloatList st)})
  markDirty ctx

listAt :: [a] -> Int -> a -> a
listAt xs i d = case drop i xs of
  (x : _) -> x
  _ -> d
