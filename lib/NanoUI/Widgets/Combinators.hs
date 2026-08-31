{-# LANGUAGE OverloadedStrings #-}

-- | Layout and visual helpers shared by Table, Tabs, Tree, and Radio.
module NanoUI.Widgets.Combinators
  ( gridColumns
  , syncScroll
  , headerRow
  , indentedRow
  , stripedRow
  , buttonStyled
  , selectableItem
  , keyedRow
  , listAt
  , fitList
  , listClipper
  , virtualIndices
  , setAt
  , normalizeOrder
  , visibleCols
  , rebuildOrder
  , minColW
  , headerEdgeHit
  , headerAtPoint
  , indexForest
  , visibleForest
  , forestParents
  , countForest
  , treeKeyNav
  , Tab (..)
  , TabStyle (..)
  , TabOrientation (..)
  , TabResponse (..)
  , tabRespClicked
  , tabRespChanged
  , tab
  , closableTab
  , mkTab
  , tabStrip
  , tableSplitPanes
  , SortDir (..)
  , SortCol (..)
  , ColSize (..)
  , TableCfg (..)
  , TableResponse (..)
  , defaultTableCfg
  , tableRespChanged
  , tableRespClicked
  , tableHiddenIndices
  , sortRows
  , useTableSort
  , columnCount
  , columnHeaders
  , columnCells
  , numericColumns
  , columnWidths
  , clampSortCol
  , sortMarkStyle
  , nextSortCol
  , packResize
  , packReorder
  , isResizeDrag
  , isReorderDrag
  , dragCol
  , resolvedWidth
  , colSizing
  , colBoxLayout
  , distributeColWidths
  , writeColW
  , finishTable
  )
where

import Colonnade (Colonnade, Headed (..))
import Colonnade.Encode qualified as Encode
import Control.Monad (forM_, void, when, zipWithM)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (sortBy, unfoldr)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read (decimal, signed)
import Data.Vector qualified as V
import Effectful (Eff, type (:>))
import qualified Data.IntMap.Strict as IM
import NanoUI.Context (Context (..), bumpMirror, getPrevRect, getScrollOffset, getStore, intKey, markDirty, setScrollOffset, setStore)
import NanoUI.Font (labelContentInset, stripWidgetMarkers, textDisplayWidth)
import NanoUI.Frame.Hit (findNodeByWidgetId)
import NanoUI.Host (isCellHost)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input
  ( Input (..)
  , inputMouseDown
  , inputMousePos
  , inputMousePressed
  , inputMouseReleased
  , inputMouseRightReleased
  )
import NanoUI.Layout.Arena (NodeType (..), setNodeValue)
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
  , grow
  , tight
  )
import NanoUI.Types (Rect (..), V2 (..), rectContains, rectH, rectW, v2X, v2Y)
import NanoUI.WidgetText (closeButtonMarker, tabButtonMarker, tableScrollSlaveStyle, tableSortReserve)
import NanoUI.Widgets.Behavior (KeyNav (..), useReorder)
import NanoUI.Widgets.Layout (column, panel, row, scrollAreaId, separator, spacer)
import NanoUI.Widgets.Node
  ( Clickable (..)
  , Responding (..)
  , Response (..)
  , addWidgetStyled
  , setChanged
  , setClicked
  , tagContainer
  )

-- | One row of cells keyed by caller ids (column index, not visible position).
gridColumns :: (Ui :> es) => [Int] -> [Layout] -> [Eff es ()] -> Eff es ()
gridColumns keys layouts cells =
  void $
    row (tight $ defaultLayout {layoutGap = 0}) $
      mapM_
        ( \(n, (k, lay, cell)) -> do
            when (n > 0) $ void separator
            withKey k (column lay cell)
        )
        (zip [0 :: Int ..] (zip3 keys layouts cells))

-- | Copy vertical scroll offset from master to slave.
syncScroll :: (Ui :> es) => WidgetId -> WidgetId -> Eff es ()
syncScroll master slave = do
  ctx <- askContext
  uiIO $ do
    off <- getScrollOffset ctx master
    setScrollOffset ctx slave off

headerRow :: (Ui :> es) => Layout -> Eff es a -> Eff es a
headerRow = row

indentedRow :: (Ui :> es) => Int -> Layout -> Eff es a -> Eff es a
indentedRow depth layout child =
  row layout $ do
    when (depth > 0) $
      void (spacer (Fixed (fromIntegral depth * 12)) Fit)
    child

stripedRow :: (Ui :> es) => Int -> Layout -> Text -> Eff es Response
stripedRow rowIdx layout txt = do
  wid <- nextId
  let stripe = if even rowIdx then 1 else 2
  addWidgetStyled wid NodeText txt 0 layout stripe Nothing

-- | Button with styleIdx for active, sort, badge, or close chrome.
buttonStyled :: (Ui :> es) => Text -> Float -> Layout -> Int -> Eff es Response
buttonStyled txt value layout styleIdx = do
  wid <- nextId
  addWidgetStyled wid NodeButton txt value layout styleIdx Nothing

selectableItem :: (Ui :> es) => NodeType -> Text -> Bool -> Layout -> Int -> Eff es Response
selectableItem nt txt selected layout styleIdx = do
  wid <- nextId
  addWidgetStyled
    wid
    nt
    txt
    (if selected then 1 else 0)
    layout
    styleIdx
    Nothing

keyedRow :: (Ui :> es) => [Int] -> (Int -> Eff es a) -> Eff es [a]
keyedRow = keyedRowLay (tight . fillW $ defaultLayout {layoutGap = 0})

keyedRowLay :: (Ui :> es) => Layout -> [Int] -> (Int -> Eff es a) -> Eff es [a]
keyedRowLay lay keys act =
  row lay $
    mapM
      ( \(n, k) -> do
          when (n > 0) $ void separator
          withKey k (act k)
      )
      (zip [0 :: Int ..] keys)

listAt :: [a] -> Int -> a -> a
listAt xs i d = case drop i xs of
  (x : _) -> x
  _ -> d

fitList :: Int -> a -> [a] -> [a]
fitList n d xs = take n (xs ++ repeat d)

{-# INLINE listClipper #-}
listClipper :: Int -> Float -> Float -> Float -> (Int, Int)
listClipper itemCount scrollOff viewH itemH
  | itemCount <= 0 || itemH <= 0 || viewH <= 0 = (0, -1)
  | otherwise =
      let firstVis = max 0 (floor (scrollOff / itemH))
          lastVis = min (itemCount - 1) (floor ((scrollOff + viewH - 1) / itemH))
       in if lastVis < firstVis then (0, -1) else (firstVis, lastVis)

{-# INLINE virtualIndices #-}
virtualIndices :: Int -> Float -> Float -> Float -> [Int]
virtualIndices n scrollOff viewH itemH =
  let (lo, hi) = listClipper n scrollOff viewH itemH
   in if hi < lo then [] else [lo .. hi]

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

rebuildOrder :: IntSet -> [Int] -> [Int] -> [Int]
rebuildOrder hidden newVis old =
  let go [] vs = vs
      go (i : is) vs
        | IS.member i hidden = i : go is vs
        | otherwise = case vs of
            (v : vs') -> v : go is vs'
            [] -> i : is
   in go old newVis

minColW :: Float
minColW = 40

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

indexForest :: (n -> [n]) -> [n] -> [(Int, Int, n)]
indexForest kids items = snd (go 0 0 items)
  where
    go next _ [] = (next, [])
    go next depth (x : xs) =
      let (n1, ks) = go (next + 1) (depth + 1) (kids x)
          (n2, rs) = go n1 depth xs
       in (n2, (next, depth, x) : ks ++ rs)

visibleForest :: (n -> [n]) -> IS.IntSet -> [n] -> [(Int, Int, Bool, n)]
visibleForest kids expanded items =
  unfoldr step (indexForest kids items)
  where
    step [] = Nothing
    step ((idx, depth, x) : rest) =
      let has = not (null (kids x))
          pending =
            if has && not (IS.member idx expanded)
              then dropWhile (\(_, d, _) -> d > depth) rest
              else rest
       in Just ((idx, depth, has, x), pending)

forestParents :: (n -> [n]) -> [n] -> [Int]
forestParents kids items =
  [idx | (idx, _, x) <- indexForest kids items, not (null (kids x))]

countForest :: (n -> [n]) -> [n] -> Int
countForest kids = foldl' (\n x -> n + 1 + countForest kids (kids x)) 0

treeKeyNav ::
  KeyNav ->
  [(Int, Int, Bool, a)] ->
  [Response] ->
  WidgetId ->
  Int ->
  IS.IntSet ->
  (Int, IS.IntSet, Maybe WidgetId)
treeKeyNav nav rows resps focus selected expanded
  | hashWidgetId focus == 0 || not moving = (selected, expanded, Nothing)
  | otherwise = case [(i, trow) | (i, (trow, r)) <- zip [0 ..] (zip rows resps), rawRespId r == focus] of
      ((pos, trow) : _) -> step pos trow
      [] -> (selected, expanded, Nothing)
 where
  moving = knUp nav || knDown nav || knLeft nav || knRight nav || knEnter nav || knSpace nav
  n = length rows
  widAt i = rawRespId (resps !! i)
  idxAt i = let (idx, _, _, _) = rows !! i in idx
  wantToggle = knEnter nav || knSpace nav
  toggle idx s = if IS.member idx s then IS.delete idx s else IS.insert idx s
  parentOf idx =
    case break (\(i, _, _, _) -> i == idx) rows of
      (before, (_, destDepth, _, _) : _) ->
        case [p | (p, d, _, _) <- reverse before, d < destDepth] of
          (p : _) -> p
          [] -> idx
      _ -> idx
  step pos (nodeIdx, depth, hasKids, _)
    | knDown nav, pos + 1 < n = let p = pos + 1 in (idxAt p, expanded, Just (widAt p))
    | knUp nav, pos > 0 = let p = pos - 1 in (idxAt p, expanded, Just (widAt p))
    | wantToggle, hasKids = (selected, toggle nodeIdx expanded, Nothing)
    | knRight nav, hasKids, not (IS.member nodeIdx expanded) = (selected, IS.insert nodeIdx expanded, Nothing)
    | knLeft nav, hasKids, IS.member nodeIdx expanded = (selected, IS.delete nodeIdx expanded, Nothing)
    | knLeft nav, depth > 0 =
        let pidx = parentOf nodeIdx
         in case [i | (i, (p, _, _, _)) <- zip [0 ..] rows, p == pidx] of
              (p : _) -> (pidx, expanded, Just (widAt p))
              [] -> (pidx, expanded, Nothing)
    | otherwise = (selected, expanded, Nothing)

data TabStyle = TabUnderline | TabPill | TabSegmented | TabContained
  deriving (Eq, Show, Enum, Bounded)

data TabOrientation = TabTop | TabBottom | TabLeft | TabRight
  deriving (Eq, Show, Enum, Bounded)

data Tab a body = Tab
  { tabKey :: !a
  , tabTitle :: !Text
  , tabClosable :: !Bool
  , tabDisabled :: !Bool
  , tabBadge :: !(Maybe Text)
  , tabBody :: !body
  }

data TabResponse a = TabResponse
  { tabResponse :: !Response
  , tabClosed :: !(Maybe a)
  , tabActive :: !a
  }
  deriving (Eq, Show)

instance Responding (TabResponse a) where
  respId (TabResponse r _ _) = respId r
  respRect (TabResponse r _ _) = respRect r
  respHovered (TabResponse r _ _) = respHovered r
  respPressed (TabResponse r _ _) = respPressed r
  respClicked (TabResponse r _ _) = respClicked r
  respChanged (TabResponse r _ _) = respChanged r

instance Clickable (TabResponse a) where
  respIsClicked (TabResponse r _ _) = respClicked r

tabRespClicked :: TabResponse a -> Bool
tabRespClicked = respClicked

tabRespChanged :: TabResponse a -> Bool
tabRespChanged = respChanged

tab :: a -> Text -> body -> Tab a body
tab key title body = Tab key title False False Nothing body

closableTab :: a -> Text -> body -> Tab a body
closableTab key title body = Tab key title True False Nothing body

mkTab :: a -> Text -> Bool -> Bool -> Maybe Text -> body -> Tab a body
mkTab = Tab

tabStrip ::
  (Eq a, Ui :> es) =>
  TabStyle ->
  TabOrientation ->
  a ->
  [Tab a body] ->
  Maybe (a -> Eff es ()) ->
  Eff es (TabResponse a, a)
tabStrip style orient cur tabList mRenderBody = do
  ctx <- askContext
  groupId <- nextId
  let vertical = orient == TabLeft || orient == TabRight
      h = if isCellHost (ctxHostProfile ctx) then 1 else 28
      styleVal = fromEnum style
      hdrLay =
        defaultLayout
          { layoutHeight = Fixed h
          , layoutPadding = Padding 8 8 4 4
          , layoutAlignX = AlignCenter
          , layoutAlignY = AlignMiddle
          , layoutGap = 4
          }
      barLay =
        if vertical
          then defaultLayout {layoutDirection = Column, layoutWidth = Fit, layoutHeight = Grow 1, layoutGap = 2, layoutPadding = Padding 2 2 2 2}
          else
            defaultLayout
              { layoutDirection = Row
              , layoutWidth = Grow 1
              , layoutHeight = Fixed (h + 4)
              , layoutGap = if style == TabSegmented then 0 else 4
              , layoutPadding = if style == TabContained then Padding 0 0 2 0 else Padding 0 0 0 0
              }
      headerBar =
        (if vertical then column else row) barLay $ do
          tagContainer groupId
          renderHeaders ctx hdrLay styleVal
  case mRenderBody of
    Nothing -> headerBar
    Just renderBody ->
      let shell layout = layout $ do
            (tabResp, nextTab) <- headerBar
            renderBody nextTab
            pure (tabResp, nextTab)
       in if vertical
            then shell (row (tight . fillW . grow $ defaultLayout))
            else shell (column (tight . fillW $ defaultLayout))
 where
  renderHeaders ctx hdrLay styleVal = do
    resps <- zipWithM (\i t -> withKey i (renderSingleHeader hdrLay (styleVal + 4 * i) t)) [0 :: Int ..] tabList
    let clickedKeys = [k | (k, clicked, _, _) <- resps, clicked]
        closedKey = listToMaybe [k | (_, _, Just k, _) <- resps]
        nextTab = case clickedKeys of
          (k : _) -> k
          [] -> cur
        hasChanged = nextTab /= cur
        hasClicked = not (null clickedKeys)
        overallResp =
          TabResponse
            { tabResponse = setChanged hasChanged (setClicked hasClicked (mconcat [r | (_, _, _, r) <- resps]))
            , tabClosed = closedKey
            , tabActive = nextTab
            }
    when (hasChanged || isJust closedKey) $ uiIO (markDirty ctx)
    when hasChanged $ uiIO (syncTabHeaderActive ctx nextTab resps)
    pure (overallResp, nextTab)

  renderSingleHeader hdrLay packedStyle t = do
    let isActive = tabKey t == cur
        badge = maybe "" (\b -> " (" <> b <> ")") (tabBadge t)
        headerText = tabButtonMarker <> tabTitle t <> badge
    if tabClosable t
      then do
        (tabResp, closed) <- row (tight defaultLayout) $ do
          resp <- buttonStyled headerText (if isActive then 1 else 0) hdrLay packedStyle
          closeResp <- buttonStyled (closeButtonMarker <> "\215") 0 (hdrLay {layoutPadding = Padding 2 4 4 4}) 0
          pure (resp, respClicked closeResp)
        pure (tabKey t, respClicked tabResp && not closed, if closed then Just (tabKey t) else Nothing, tabResp)
      else do
        resp <- buttonStyled headerText (if isActive then 1 else 0) hdrLay packedStyle
        pure (tabKey t, respClicked resp, Nothing, resp)

syncTabHeaderActive :: Eq a => Context -> a -> [(a, Bool, Maybe a, Response)] -> IO ()
syncTabHeaderActive ctx active resps =
  forM_ resps $ \(k, _, _, r) -> do
    mIdx <- findNodeByWidgetId ctx (respId r)
    case mIdx of
      Just i -> setNodeValue (ctxNodeArena ctx) i (if k == active then 1 else 0)
      Nothing -> pure ()

tableSplitPanes ::
  (Ui :> es) =>
  WidgetId ->
  WidgetId ->
  WidgetId ->
  Float ->
  Float ->
  [Int] ->
  [Int] ->
  [row] ->
  [row] ->
  (Int -> Layout) ->
  (Int -> Eff es Response) ->
  (Int -> row -> Int -> Eff es ()) ->
  Eff es [(Int, Response)]
tableSplitPanes tableWid vWid hWid rowMinH hChromeH frozenIdx unfrozenIdx pinned scrollRows colBox renderHeader renderCell =
  panel (tight . fillW . fillH $ defaultLayout) $ do
    tagContainer tableWid
    row (tight . fillW . fillH $ defaultLayout {layoutGap = 0}) $ do
      frozenHs <-
        if null frozenIdx
          then pure []
          else zip frozenIdx <$> pane False (if null unfrozenIdx then 0 else tableScrollSlaveStyle) frozenIdx
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
    let base = tight $ defaultLayout {layoutGap = 0, layoutMinW = minSum idxs, layoutHeight = Grow 1}
     in if fill then fillW base else base {layoutWidth = Fit}
  headerLine idxs renderHeader' =
    keyedRowLay (tight $ defaultLayout {layoutGap = 0}) idxs $ \i ->
      column (colBox i) (renderHeader' i)
  pinnedBlock idxs =
    mapM_
      ( \(ri, r) ->
          withKey ("pin" :: Text, ri) $ do
            when (ri > 0) $ void separator
            gridColumns
              idxs
              (map colBox idxs)
              [void (renderCell ri r i) | i <- idxs]
      )
      (zip [0 ..] pinned)
  bodyBlock idxs = do
    ctx <- askContext
    vis <-
      let n = length scrollRows
       in if n == 0 || rowMinH <= 0
            then pure []
            else uiIO $ do
              scroll <- getScrollOffset ctx vWid
              viewH <-
                getPrevRect ctx vWid >>= \case
                  Nothing -> pure (rowMinH * 8)
                  Just r -> pure (rectH r)
              pure (virtualIndices n scroll viewH rowMinH)
    gridColumns
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
  pane fill slaveStyle idxs = do
    column (paneLay fill idxs) $ do
      hs <- headerLine idxs renderHeader
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
            hs <- headerLine idxs renderHeader
            void separator
            pinnedBlock idxs
            when (not (null pinned) && not (null scrollRows)) $ void separator
            pure hs
      scrollAreaId vWid (vLay True) 0 $
        scrollAreaId hWid (fillW . fillH $ hRowLay) 0 (bodyBlock idxs)
      void $
        row (fillW $ hRowLay {layoutHeight = Fixed hChromeH, layoutMinH = hChromeH, layoutMaxH = hChromeH}) $
          spacer (Fixed (max minColW (minSum idxs))) (Fixed 1)
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
   in [let cells = [columnCells cols r !! i | r <- rows] in not (null cells) && all isNumericCell cells | i <- [0 .. n - 1]]

columnWidths :: Context -> Colonnade Headed row Text -> [row] -> [Float]
columnWidths ctx cols rows =
  let host = ctxHostProfile ctx
      fm = ctxFontMetrics ctx
      mono = ctxMonoFontMetrics ctx
      terminal = isCellHost host
      (ix, _) = labelContentInset host fm
      cellPadX = if terminal then 0 else 20
      headerPadX = cellPadX + if terminal then 0 else 2 * ix
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

distributeColWidths :: Int -> [Int] -> [ColSize] -> [Float] -> [Float] -> Float -> [Float]
distributeColWidths n vis sizes contentWs stored tableW =
  let mins = [resolvedWidth sizes contentWs stored i | i <- [0 .. n - 1]]
      seps = fromIntegral (max 0 (length vis - 1))
      visMin = sum [listAt mins i 0 | i <- vis] + seps
      autoStretch i =
        case listAt sizes i ColStretch of
          ColStretch ->
            listAt stored i 0 <= max minColW (listAt contentWs i minColW)
          _ -> False
      stretchVis = [i | i <- vis, autoStretch i]
      slack = max 0 (tableW - visMin)
      extra = if null stretchVis then 0 else slack / fromIntegral (length stretchVis)
   in [if autoStretch i then listAt mins i 0 + extra else listAt mins i 0 | i <- [0 .. n - 1]]

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
   in case listAt sizes i ColStretch of
        ColStretch -> if saved > contentW then saved else contentW
        ColFixed f ->
          let base = max minColW f
           in if saved > 0 then max base saved else base
        ColContent -> if saved > 0 then max contentW saved else contentW

colSizing :: [ColSize] -> [Float] -> Float -> Int -> Sizing
colSizing _sizes _stored resolved _i = Fixed resolved

colBoxLayout :: Sizing -> Float -> Layout
colBoxLayout sizing resolved =
  let base = tight $ defaultLayout {layoutGap = 0, layoutMinW = resolved}
   in case sizing of
        Fixed w -> base {layoutWidth = Fixed w, layoutMaxW = w}
        Grow _ -> fillW base
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
