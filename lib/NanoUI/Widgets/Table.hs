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
  , -- colonnade re-exports for column definitions
    Colonnade
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
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read (decimal, signed)
import Data.Vector qualified as V
import Effectful (Eff, type (:>))
import NanoUI.Context (Context (..), getStore, intKey, markDirty, setStore)
import NanoUI.Font (measureText, monoFontMarker, scrollBarGutter)
import NanoUI.Host (isCellHost)
import NanoUI.Id (WidgetId (..))
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
import NanoUI.Store (WidgetStore (..))
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
import NanoUI.Types (Rect (..), V2 (..), rectContains, rectUnion, v2X, v2Y)
import NanoUI.WidgetText
  ( tableHeaderLabel
  , tableScrollSlaveStyle
  , tableSortReserve
  , tableStripeEven
  , tableStripeOdd
  )
import NanoUI.Widgets.Layout qualified as Layout
import NanoUI.Widgets.Node
  ( Clickable (..)
  , Responding (..)
  , Response (..)
  , addWidgetStyled
  , mkResponse
  , setChanged
  , setClicked
  )
import qualified Data.IntMap.Strict as IM

-- | Sort direction for a table column.
data SortDir
  = SortAsc
  | SortDesc
  deriving (Eq, Show, Enum, Bounded)

-- | Active sort column and direction.
data SortCol = SortCol
  { sortColIndex :: !Int
  , sortColDir :: !SortDir
  }
  deriving (Eq, Show)

-- | How a column claims horizontal space.
data ColSize
  = -- | Width from header and cell text.
    ColContent
  | -- | Share leftover width after content and fixed columns.
    ColStretch
  | -- | Exact width in pixels (cells on TUI).
    ColFixed Float
  deriving (Eq, Show)

-- | Freeze counts, sizing, and initially hidden columns.
data TableCfg = TableCfg
  { tableFreezeCols :: !Int
  , tableFreezeRows :: !Int
  , tableColSizes :: [ColSize]
  , tableHidden :: IntSet
  }
  deriving (Eq, Show)

defaultTableCfg :: TableCfg
defaultTableCfg =
  TableCfg
    { tableFreezeCols = 0
    , tableFreezeRows = 0
    , tableColSizes = []
    , tableHidden = IS.empty
    }

-- | Response from a sortable table widget.
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
unpackSort n =
  SortCol
    { sortColIndex = n `div` 2
    , sortColDir = if odd n then SortDesc else SortAsc
    }

clampSortCol :: Int -> SortCol -> SortCol
clampSortCol n (SortCol idx dir) =
  SortCol (max 0 (min (max 0 (n - 1)) idx)) dir

sortMarkStyle :: SortCol -> Int -> Int
sortMarkStyle sort idx
  | sortColIndex sort /= idx = 0
  | sortColDir sort == SortDesc = 2
  | otherwise = 1

-- | Sort rows by the encoded Text in the active column.
sortRows :: Colonnade Headed row Text -> SortCol -> [row] -> [row]
sortRows _ _ [] = []
sortRows cols sort rows =
  let
    n = V.length (Encode.getColonnade cols)
    idx = sortColIndex (clampSortCol n sort)
    enc =
      case Encode.getColonnade cols V.!? idx of
        Nothing -> const T.empty
        Just col -> Encode.oneColonnadeEncode col
   in
    case sortColDir sort of
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
  let hdrs = columnHeaders cols
      n = length hdrs
   in [ let cells = [columnCells cols r !! i | r <- rows]
         in not (null cells) && all isNumericCell cells
      | i <- [0 .. n - 1]
      ]

cellAlign :: Bool -> AlignX
cellAlign True = AlignEnd
cellAlign False = AlignStart

columnWidths ::
  Context ->
  Colonnade Headed row Text ->
  [row] ->
  [Float]
columnWidths ctx cols rows =
  let
    host = ctxHostProfile ctx
    fm = ctxFontMetrics ctx
    measure txt =
      let (w, _) = measureText host fm txt
          pad = if isCellHost host then 0 else 20
       in w + pad
    hdrs = columnHeaders cols
    sortPad = tableSortReserve (isCellHost host)
    nCols = length hdrs
    bodyCols =
      [ [cell | r <- rows, let cell = columnCells cols r !! i]
      | i <- [0 .. nCols - 1]
      ]
   in
    zipWith
      (\hdr cells -> maximum (measure (hdr <> sortPad) : map measure cells))
      hdrs
      bodyCols

mergeResponses :: [Response] -> Response
mergeResponses [] = mkResponse (WidgetId 0) (Rect 0 0 0 0) False False False False
mergeResponses (r : rs) =
  foldl'
    ( \acc x ->
        Response
          { rawRespId = rawRespId x
          , rawRespRect = rectUnion (rawRespRect acc) (rawRespRect x)
          , rawRespHovered = rawRespHovered acc || rawRespHovered x
          , rawRespPressed = rawRespPressed acc || rawRespPressed x
          , rawRespClicked = rawRespClicked acc || rawRespClicked x
          , rawRespChanged = rawRespChanged acc || rawRespChanged x
          }
    )
    r
    rs

nextSortCol :: Int -> SortCol -> Int -> SortCol
nextSortCol n cur clicked =
  let
    clamped = clampSortCol n cur
    idx = sortColIndex clamped
    dir = sortColDir clamped
   in
    if clicked == idx
      then SortCol idx (case dir of SortAsc -> SortDesc; SortDesc -> SortAsc)
      else SortCol clicked SortAsc

headerButton :: Ui :> es => Layout -> Text -> Float -> Int -> Eff es Response
headerButton layout txt activeVal styleIdx = do
  wid <- nextId
  addWidgetStyled wid NodeButton txt activeVal layout styleIdx Nothing

tableCell :: Ui :> es => Layout -> Text -> Int -> Eff es Response
tableCell layout txt stripe = do
  wid <- nextId
  addWidgetStyled wid NodeText txt 0 layout stripe Nothing

-- | Uncontrolled sort-state hook keyed by widget group.
useTableSort :: Ui :> es => SortCol -> Eff es (Eff es SortCol, SortCol -> Eff es ())
useTableSort initial = do
  wid <- nextId
  ctx <- askContext
  let
    key = intKey wid
    packedInitial = packSort initial
    readSort =
      uiIO $ do
        st <- getStore ctx
        pure (unpackSort (IM.findWithDefault packedInitial key (storeTableSort st)))
    setSort sort =
      uiIO $ do
        st <- getStore ctx
        let packed = packSort sort
            prev = IM.findWithDefault packedInitial key (storeTableSort st)
        when (prev /= packed) $ do
          setStore ctx (st {storeTableSort = IM.insert key packed (storeTableSort st)})
          markDirty ctx
  pure (readSort, setSort)

-- | Sortable table with default layout and column config.
table ::
  (Ui :> es) =>
  Text ->
  Colonnade Headed row Text ->
  [row] ->
  SortCol ->
  Eff es (TableResponse, SortCol)
table = tableEx (tight . fillW $ defaultLayout {layoutGap = 0})

-- | Sortable table with custom outer layout.
tableEx ::
  (Ui :> es) =>
  Layout ->
  Text ->
  Colonnade Headed row Text ->
  [row] ->
  SortCol ->
  Eff es (TableResponse, SortCol)
tableEx = tableCfg defaultTableCfg

-- | Sortable table with freeze, sizing, and hide/reorder/resize state.
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
    let
      host = ctxHostProfile ctx
      terminal = isCellHost host
      contentWs = columnWidths ctx cols rows
      sizes = tableColSizes cfg
      order0 = normalizeOrder n (IM.findWithDefault [0 .. n - 1] stateKey (storeTableOrder st0))
      hidden0 = IM.findWithDefault (tableHidden cfg) stateKey (storeTableHidden st0)
      widths0 = fitList n 0 (IM.findWithDefault [] stateKey (storeTableColW st0))
      drag0 = IM.findWithDefault 0 stateKey (storeTableDrag st0)
      dragX0 = IM.findWithDefault 0 stateKey (storeTableDragX st0)
      dragW0 = IM.findWithDefault 0 stateKey (storeTableDragW st0)
      mx = v2X (inputMousePos inp)
      mouse = inputMousePos inp
      resizing = isResizeDrag drag0 && inputMouseDown inp
      widths1 =
        if resizing
          then setAt (dragCol drag0) (max minColW (dragW0 + mx - dragX0)) widths0
          else widths0
    when (widths1 /= widths0) $
      uiIO $ writeColW ctx stateKey widths1
    let
      vis = visibleCols order0 hidden0
      freezeN = min (max 0 (tableFreezeCols cfg)) (length vis)
      freezeR = min (max 0 (tableFreezeRows cfg)) (length rows)
      frozenIdx = take freezeN vis
      unfrozenIdx = drop freezeN vis
      sorted = sortRows cols sort0 rows
      pinned = take freezeR sorted
      scrollRows = drop freezeR sorted
      hdrs = columnHeaders cols
      numeric = numericColumns cols rows
      cellPad =
        if terminal
          then Padding 0 0 0 0
          else Padding 10 8 10 8
      rowMinH = if terminal then 1 else 28
      resolvedW i = resolvedWidth sizes contentWs widths1 i
      colBox i = colBoxLayout (colSizing sizes widths1 i) (resolvedW i)
      itemLayout i =
        let numericCol = listAt numeric i False
         in (colBox i)
              { layoutAlignX = cellAlign numericCol
              , layoutAlignY = AlignMiddle
              , layoutPadding = cellPad
              , layoutMinH = rowMinH
              }
      shown i txt =
        if listAt numeric i False then monoFontMarker <> txt else txt
      stripeOf ri = if even ri then tableStripeEven else tableStripeOdd
      minSum idxs =
        sum (map resolvedW idxs) + fromIntegral (max 0 (length idxs - 1))
      vLay fill =
        let base = tight . fillH $ defaultLayout {layoutGap = 0}
         in if fill then fillW base else base
      hRowLay =
        defaultLayout
          { layoutDirection = Row
          , layoutPadding = Padding 0 0 0 0
          , layoutGap = 0
          }
      hHeadLay = fillW hRowLay
      hBodyLay = fillW . fillH $ hRowLay
      hChromeH = if terminal then 1 else scrollBarGutter host (ctxFontMetrics ctx)
      hChromeLay =
        fillW $
          hRowLay
            { layoutHeight = Fixed hChromeH
            , layoutMinH = hChromeH
            , layoutMaxH = hChromeH
            }
      paneLay fill idxs =
        let base =
              tight $
                defaultLayout
                  { layoutGap = 0
                  , layoutMinW = minSum idxs
                  , layoutHeight = Grow 1
                  }
         in if fill then fillW base else base {layoutWidth = Fit}
      renderHeader i = do
        let activeVal = if sortColIndex sort0 == i then 1 else 0
        headerButton
          (itemLayout i)
          (tableHeaderLabel terminal (listAt hdrs i T.empty))
          activeVal
          (sortMarkStyle sort0 i)
      renderCell ri r i = do
        let cell = columnCells cols r !! i
        void (tableCell (itemLayout i) (shown i cell) (stripeOf ri))
      headerRow idxs = do
        Layout.row (tight . fillW $ defaultLayout {layoutGap = 0}) $
          mapM
            ( \(k, i) -> do
                when (k > 0) $ void Layout.separator
                withKey i (renderHeader i)
            )
            (zip [0 :: Int ..] idxs)
      pinnedBlock idxs =
        mapM_
          ( \(ri, r) ->
              withKey ("pin" :: Text, ri) $ do
                when (ri > 0) $ void Layout.separator
                void $
                  Layout.row (tight . fillW $ defaultLayout {layoutGap = 0, layoutMinH = rowMinH}) $
                    mapM_
                      ( \(k, i) -> do
                          when (k > 0) $ void Layout.separator
                          withKey i (renderCell ri r i)
                      )
                      (zip [0 :: Int ..] idxs)
          )
          (zip [0 :: Int ..] pinned)
      bodyBlock idxs =
        Layout.row (tight . fillW $ defaultLayout {layoutGap = 0}) $
          mapM_
            ( \(k, i) -> do
                when (k > 0) $ void Layout.separator
                withKey i $
                  Layout.column (colBox i) $
                    mapM_
                      ( \(ri, r) ->
                          withKey ri $ do
                            when (ri > 0) $ void Layout.separator
                            renderCell (ri + freezeR) r i
                      )
                      (zip [0 :: Int ..] scrollRows)
            )
            (zip [0 :: Int ..] idxs)
      pane fill slaveStyle idxs = do
        Layout.column (paneLay fill idxs) $ do
          hs <- headerRow idxs
          void Layout.separator
          pinnedBlock idxs
          when (not (null pinned) && not (null scrollRows)) $ void Layout.separator
          Layout.scrollAreaId vWid (vLay fill) slaveStyle (bodyBlock idxs)
          pure hs
      unfrozenPane idxs = do
        Layout.column (paneLay True idxs) $ do
          hs <-
            Layout.scrollAreaId hWid hHeadLay tableScrollSlaveStyle $
              Layout.column
                (tight . fillW $ defaultLayout {layoutGap = 0, layoutMinW = minSum idxs})
                $ do
                  hs <- headerRow idxs
                  void Layout.separator
                  pinnedBlock idxs
                  when (not (null pinned) && not (null scrollRows)) $
                    void Layout.separator
                  pure hs
          Layout.scrollAreaId vWid (vLay True) 0 $
            Layout.scrollAreaId hWid hBodyLay tableScrollSlaveStyle (bodyBlock idxs)
          Layout.scrollAreaId hWid hChromeLay 0 $
            void (Layout.spacer (Fixed (max minColW (minSum idxs))) (Fixed 1))
          pure hs
    Layout.column outerLayout $ do
      showAllResp <-
        if IS.null hidden0
          then pure Nothing
          else
            fmap Just $ do
              wid <- nextId
              addWidgetStyled wid NodeButton "Show all columns" 0 (tight . fillW $ defaultLayout) 0 Nothing
      headerPairs <-
        Layout.panel (tight . fillW . fillH $ defaultLayout) $
          Layout.row (tight . fillW . fillH $ defaultLayout {layoutGap = 0}) $ do
            frozenHs <-
              if null frozenIdx
                then pure []
                else do
                  let slave =
                        if null unfrozenIdx then 0 else tableScrollSlaveStyle
                  zip frozenIdx <$> pane False slave frozenIdx
            when (not (null frozenIdx) && not (null unfrozenIdx)) $
              void Layout.separator
            unfrozenHs <-
              if null unfrozenIdx
                then pure []
                else do
                  hs <- unfrozenPane unfrozenIdx
                  pure (zip unfrozenIdx hs)
            pure (frozenHs ++ unfrozenHs)
      let
        edgePad = if terminal then 1 else 4
        edgeCol = headerEdgeHit edgePad headerPairs mouse
        hoverCol = headerAtPoint headerPairs mouse
        dragged =
          isReorderDrag drag0 && abs (mx - dragX0) > 8
        pressResize = inputMousePressed inp && edgeCol /= Nothing
        pressReorder =
          inputMousePressed inp && edgeCol == Nothing && hoverCol /= Nothing
        nextDrag' =
          if pressResize
            then maybe 0 packResize edgeCol
            else
              if pressReorder
                then maybe 0 packReorder hoverCol
                else
                  if inputMouseReleased inp || not (inputMouseDown inp)
                    then 0
                    else drag0
        nextDragX
          | pressResize || pressReorder = mx
          | nextDrag' == 0 = 0
          | otherwise = dragX0
        nextDragW
          | pressResize = maybe 0 headerW edgeCol
          | nextDrag' == 0 = 0
          | otherwise = dragW0
        headerW i =
          case lookup i headerPairs of
            Just r ->
              let w = rectW (rawRespRect r)
               in if w > 0 then w else resolvedW i
            _ -> resolvedW i
        dropCol = headerAtPoint headerPairs mouse
        nextOrder =
          case (inputMouseReleased inp, isReorderDrag drag0, dragged, dropCol) of
            (True, True, True, Just toCol) ->
              let fromC = dragCol drag0
                  vis' = moveVisible vis fromC toCol
               in rebuildOrder hidden0 vis' order0
            _ -> order0
        hideClicked =
          [ i
          | (i, r) <- headerPairs
          , respHovered r
          , inputMouseRightReleased inp
          , drag0 == 0
          ]
        nextHidden =
          case showAllResp of
            Just r | respClicked r -> IS.empty
            _ ->
              case hideClicked of
                (i : _)
                  | IS.size hidden0 + 1 < n -> IS.insert i hidden0
                _ -> hidden0
        sortClick =
          if dragged || isResizeDrag drag0 || edgeCol /= Nothing
            then Nothing
            else
              listToMaybe
                [ i
                | (i, r) <- headerPairs
                , respClicked r
                ]
        nextSort = maybe sort0 (nextSortCol n sort0) sortClick
        hasChanged =
          nextSort /= sort0
            || nextOrder /= order0
            || nextHidden /= hidden0
            || widths1 /= widths0
        headerResp = mergeResponses (map snd headerPairs)
        extra = maybe [] pure showAllResp
        widgetResp =
          setChanged hasChanged $
            setClicked (hasChanged && sortClick /= Nothing) (mergeResponses (headerResp : extra))
      uiIO $ do
        st <- getStore ctx
        let st1 =
              st
                { storeTableOrder = IM.insert stateKey nextOrder (storeTableOrder st)
                , storeTableHidden = IM.insert stateKey nextHidden (storeTableHidden st)
                , storeTableDrag = IM.insert stateKey nextDrag' (storeTableDrag st)
                , storeTableDragX = IM.insert stateKey nextDragX (storeTableDragX st)
                , storeTableDragW = IM.insert stateKey nextDragW (storeTableDragW st)
                }
        when (st1 /= st) $ do
          setStore ctx st1
          markDirty ctx
      let tableResp =
            TableResponse
              { tableWidgetResponse = widgetResp
              , tableSort = nextSort
              , tableColOrder = nextOrder
              , tableHiddenCols = nextHidden
              }
      pure (tableResp, nextSort)

minColW :: Float
minColW = 40

packResize :: Int -> Int
packResize i = 1000 + i

packReorder :: Int -> Int
packReorder i = 2000 + i

isResizeDrag :: Int -> Bool
isResizeDrag n = n >= 1000 && n < 2000

isReorderDrag :: Int -> Bool
isReorderDrag n = n >= 2000

dragCol :: Int -> Int
dragCol n = n `mod` 1000

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
      missing = [i | i <- [0 .. n - 1], not (IS.member i seen)]
   in valid ++ missing

visibleCols :: [Int] -> IntSet -> [Int]
visibleCols order hidden = filter (`IS.notMember` hidden) order

resolvedWidth :: [ColSize] -> [Float] -> [Float] -> Int -> Float
resolvedWidth sizes contentWs stored i =
  let contentW = max minColW (listAt contentWs i minColW)
      saved = listAt stored i 0
      mode = listAt sizes i ColStretch
   in if saved > 0
        then saved
        else case mode of
          ColFixed f -> max minColW f
          _ -> contentW

colSizing :: [ColSize] -> [Float] -> Int -> Sizing
colSizing sizes stored i =
  let saved = listAt stored i 0
      mode = listAt sizes i ColStretch
   in if saved > 0
        then Fixed saved
        else case mode of
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
          right = x + w
    , w > 0 && h > 0
    , let mx = v2X mouse
          my = v2Y mouse
    , my >= y && my <= y + h
    , abs (mx - right) <= pad
    ]

headerAtPoint :: [(Int, Response)] -> V2 -> Maybe Int
headerAtPoint cols mouse =
  listToMaybe
    [ i
    | (i, r) <- cols
    , rectContains (rawRespRect r) mouse
    ]

moveVisible :: [Int] -> Int -> Int -> [Int]
moveVisible vis fromC toC =
  case (indexOf fromC vis, indexOf toC vis) of
    (Just from, Just to) -> moveIndex from to vis
    _ -> vis

indexOf :: Eq a => a -> [a] -> Maybe Int
indexOf x xs = listToMaybe [i | (i, y) <- zip [0 ..] xs, y == x]

moveIndex :: Int -> Int -> [a] -> [a]
moveIndex fromIdx toIdx xs
  | fromIdx == toIdx = xs
  | fromIdx < 0 || toIdx < 0 = xs
  | fromIdx >= length xs || toIdx >= length xs = xs
  | otherwise =
      let x = xs !! fromIdx
          without = take fromIdx xs ++ drop (fromIdx + 1) xs
          ins = if toIdx > fromIdx then toIdx - 1 else toIdx
       in take ins without ++ x : drop ins without

rebuildOrder :: IntSet -> [Int] -> [Int] -> [Int]
rebuildOrder hidden newVis old =
  let go [] vs = vs
      go (i : is) vs
        | IS.member i hidden = i : go is vs
        | otherwise =
            case vs of
              (v : vs') -> v : go is vs'
              [] -> i : is
   in go old newVis

writeColW :: Context -> Int -> [Float] -> IO ()
writeColW ctx key ws = do
  st <- getStore ctx
  setStore ctx (st {storeTableColW = IM.insert key ws (storeTableColW st)})
  markDirty ctx

listAt :: [a] -> Int -> a -> a
listAt xs i d =
  case drop i xs of
    (x : _) -> x
    _ -> d
