{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Table
  ( SortDir (..)
  , SortCol (..)
  , ColSize (..)
  , TableConfig (..)
  , TableResponse (..)
  , defaultTableConfig
  , table
  , tableWith
  , tableConfigured
  , simpleTable
  , useTableSort
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
import Control.Monad (foldM, forM_, unless, void, when)
import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (sortOn)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Primitive.PrimArray (PrimArray, emptyPrimArray, generatePrimArray, indexPrimArray, newPrimArray, primArrayFromList, sizeofPrimArray, unsafeFreezePrimArray, writePrimArray)
import Data.Primitive.SmallArray (SmallArray, emptySmallArray, indexSmallArray, newSmallArray, sizeofSmallArray, smallArrayFromList, unsafeFreezeSmallArray, writeSmallArray)
import Data.Primitive.Types (Prim)
import Data.Vector qualified as V
import Effectful (Eff, type (:>))
import qualified Data.IntMap.Strict as IM
import NanoUI.Context (Context (..), getPrevRect, getScrollOffset2D, getStore, intKey, linkScrollAxes, setStore)
import NanoUI.Hooks (useInt)
import NanoUI.Font (ScrollBarSlot (..), scrollBarGutter, tableCellInset, lineWidthIO)
import NanoUI.Id (WidgetId (..))
import NanoUI.Input (Input (..), inputMouseDown, inputMousePos, inputMousePressed, inputMouseReleased)
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Store (WidgetStore (..), Slot (..), slotKey)
import NanoUI.Style (AlignX (..), AlignY (..), Direction (..), FontVariant (..), Layout (..), Padding (..), Sizing (..), defaultLayout, fillH, fillW, tight)
import Data.Bits ((.|.), shiftL)
import NanoUI.Types (Rect (..), clamp, rectH, rectW, rectY, v2X, V2 (..))
import NanoUI.WidgetText (buttonFlagTable, tableHeaderLabel, tableSortReserve)
import NanoUI.Widgets.Behavior (dragThresholdPx, useReorder)
import NanoUI.Widgets.Combinators
  ( buttonStyled
  , fitList
  , gridColumnsLay
  , headerAtPoint
  , headerEdgeHit
  , keyedRowLay
  , listClipper
  , minColW
  , normalizeOrder
  , rebuildOrder
  , setAt
  , stripedRow
  , visibleCols
  )
import NanoUI.Widgets.Layout (column', panel', row', scrollAreaIdConfigured, separator, spacer)
import NanoUI.Frame.Scroll.Geometry (ScrollConfig (..), ScrollPolicy (..), scrollHorizontalHidden, scrollVerticalAuto, scrollVerticalHidden)
import NanoUI.Widgets.Node
  ( HasResponse (..)
  , Response (..)
  , rawRespRect
  , respClicked
  , respRightClicked
  , setChanged
  , setClicked
  , tagContainer
  )

-- | True if the first n column sizes contain ColStretch.
{-# INLINE tableStretchN #-}
tableStretchN :: Int -> [ColSize] -> Bool
tableStretchN n = any (== ColStretch) . take n

-- | Columns fill the table width when one stretches or the table grows.
tableFillInner :: Bool -> Layout -> Bool
tableFillInner hasStretch outer =
  hasStretch
    || case layoutWidth outer of
      Grow _ -> True
      _ -> False

-- | What the frozen and scrolling panes of one table render.
data TablePanes es row = TablePanes
  { tpFillInner :: !Bool
  , tpTableWid :: !WidgetId
  , tpBodyWid :: !WidgetId
    -- ^ Body scroller; owns both scrollbars.
  , tpHeaderWid :: !WidgetId
    -- ^ Chrome-less header scroller that follows the body horizontally.
  , tpRowMinH :: !Float
  , tpFrozen :: ![Int]
  , tpUnfrozen :: ![Int]
  , tpPinned :: ![row]
  , tpScrollRows :: ![row]
  , tpColBox :: Int -> Layout
  , tpRenderHeader :: Int -> Eff es Response
  , tpRenderCell :: Int -> row -> Int -> Eff es ()
  }

tableSplitPanes :: (Ui :> es) => TablePanes es row -> Eff es [(Int, Response)]
tableSplitPanes tp =
  panel' paneRoot $ do
    tagContainer (tpTableWid tp)
    row' (paneRoot {layoutGap = 0}) $ do
      frozenHs <-
        if null frozenIdx
          then pure []
          else zip frozenIdx <$> frozenPane
      when (not (null frozenIdx) && not (null unfrozenIdx)) $ void separator
      unfrozenHs <-
        if null unfrozenIdx then pure [] else zip unfrozenIdx <$> unfrozenPane
      pure (frozenHs ++ unfrozenHs)
  where
  fillInner = tpFillInner tp
  frozenIdx = tpFrozen tp
  unfrozenIdx = tpUnfrozen tp
  pinned = tpPinned tp
  scrollRows = tpScrollRows tp
  rowMinH = tpRowMinH tp
  vWid = tpBodyWid tp
  colBox = tpColBox tp
  renderCell = tpRenderCell tp
  freezeR = length pinned
  scrollRowsArr = smallArrayFromList scrollRows
  paneRoot =
    (if fillInner then tight . fillW . fillH else tight . fillH) defaultLayout
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
  pinnedBlock idxs = do
    let !rowLay = gridRowLay idxs
        !colLays = map colBox idxs
    mapM_
      ( \(ri, r) ->
          withKey ("pin" :: Text, ri) $ do
            when (ri > 0) $ void separator
            let cell = renderCell ri r
            gridColumnsLay rowLay idxs colLays [void (cell i) | i <- idxs]
      )
      (zip [0 ..] pinned)
  -- Header row, its rule, the pinned rows and their rule: the same in both
  -- panes.
  headerBlock idxs = do
    hs <- keyedRowLay (gridRowLay idxs) idxs $ \i -> column' (colBox i) (tpRenderHeader tp i)
    void separator
    pinnedBlock idxs
    when (not (null pinned) && not (null scrollRows)) $ void separator
    pure hs
  bodyBlock idxs = do
    ctx <- askContext
    let n = sizeofSmallArray scrollRowsArr
    (lo, hi) <-
      if n == 0 || rowMinH <= 0
        then pure (0, -1)
        else uiIO $ do
          V2 _ scrollY <- getScrollOffset2D ctx vWid
          viewH <- maybe (rowMinH * 8) rectH <$> getPrevRect ctx vWid
          pure (listClipper n scrollY viewH rowMinH)
    let !rowLay = gridRowLay idxs
        !colLays = map colBox idxs
        topH = fromIntegral lo * rowMinH
        botH = if n == 0 || rowMinH <= 0 then 0 else fromIntegral (max 0 (n - hi - 1)) * rowMinH
    column' rowLay $ do
      when (topH > 0) $ void (spacer Fit (Fixed topH))
      mapM_
        ( \rowIdx ->
            withKey rowIdx $ do
              when (rowIdx > 0) $ void separator
              let r = indexSmallArray scrollRowsArr rowIdx
                  cell = renderCell (rowIdx + freezeR) r
              gridColumnsLay rowLay idxs colLays [void (cell colIdx) | colIdx <- idxs]
        )
        [lo .. hi]
      when (botH > 0) $ void (spacer Fit (Fixed botH))
  frozenPane =
    column' (paneLay False frozenIdx) $ do
      hs <- headerBlock frozenIdx
      scrollAreaIdConfigured
        vWid
        (vLay False)
        (if null unfrozenIdx then scrollVerticalAuto else scrollVerticalHidden)
        (bodyBlock frozenIdx)
      pure hs
  unfrozenPane = do
    ctx <- askContext
    -- The body scroller has no padding, so its whole lane is gutter.
    let vGutter = scrollBarGutter ScrollBarList 0
        idxs = unfrozenIdx
    mPrevV <- uiIO (getPrevRect ctx vWid)
    let totalH = fromIntegral (sizeofSmallArray scrollRowsArr) * rowMinH
        -- Prev-frame decision, one frame behind the body scroller's live 2D
        -- gutter: on the frame the vertical bar first appears (or vanishes)
        -- the header spacer disagrees with the body's reserved lane for one
        -- frame. The horizontal side dodges this class of lag by owning its
        -- bar inside the body scroller; the vertical lane cannot do that
        -- because the header must narrow by exactly the lane width at build
        -- time, and the body's live v-gutter is only known after this
        -- frame's solve. Known, accepted one-frame misalignment.
        hasVertBar = maybe (totalH > 100) (\r -> totalH > rectH r) mPrevV
    column' (paneLay fillInner idxs) $ do
      hs <-
        row' (tight . (if fillInner then fillW else id) $ defaultLayout {layoutGap = 0}) $ do
          hs' <-
            scrollAreaIdConfigured
              (tpHeaderWid tp)
              ( if fillInner
                  then fillW hRowLay
                  else hRowLay {layoutMinW = minSum idxs}
              )
              -- The header scroller is chrome-less: it follows the body's
              -- horizontal offset (linkScrollAxes below) and clips the header
              -- row at the pane edge. The horizontal scrollbar itself belongs
              -- to the body scroller so it spans the full table width at the
              -- table's bottom edge instead of sitting under the header.
              scrollHorizontalHidden
              (column' (gridRowLay idxs) (headerBlock idxs))
          when hasVertBar $ void (spacer (Fixed vGutter) Fit)
          pure hs'
      uiIO (linkScrollAxes ctx vWid (tpHeaderWid tp))
      -- The body owns both bars: the vertical one on the right, and the
      -- horizontal one at the bottom of the table. Its live 2D gutter logic
      -- reserves the lane exactly while the columns overflow, so the bar
      -- cannot flicker the way the prev-frame header lane did.
      scrollAreaIdConfigured
        vWid
        (vLay fillInner)
        (ScrollConfig ScrollAuto ScrollAuto True False)
        (bodyBlock idxs)
      pure hs

data SortDir = SortAsc | SortDesc
  deriving (Eq, Show, Enum, Bounded)

data SortCol = SortCol {sortColIndex :: !Int, sortColDir :: !SortDir}
  deriving (Eq, Show)

data ColSize = ColContent | ColStretch | ColFixed Float
  deriving (Eq, Show)

data TableConfig = TableConfig
  { tableFreezeCols :: {-# UNPACK #-} !Int
  , tableFreezeRows :: {-# UNPACK #-} !Int
  , tableColSizes :: ![ColSize]
  , tableHidden :: !IntSet
  }
  deriving (Eq, Show)

defaultTableConfig :: TableConfig
defaultTableConfig = TableConfig 0 0 [] IS.empty

data TableResponse = TableResponse
  { tableWidgetResponse :: !Response
  , tableSort :: !SortCol
  , tableColOrder :: ![Int]
  , tableHiddenCols :: !IntSet
  }
  deriving (Eq, Show)

instance HasResponse TableResponse where
  {-# INLINE toResponse #-}
  toResponse = tableWidgetResponse

tableHiddenIndices :: TableResponse -> [Int]
tableHiddenIndices = IS.toAscList . tableHiddenCols

packSort :: SortCol -> Int
packSort (SortCol c SortAsc) = c * 2
packSort (SortCol c SortDesc) = c * 2 + 1

unpackSort :: Int -> SortCol
unpackSort n = SortCol (n `div` 2) (if odd n then SortDesc else SortAsc)

clampSortCol :: Int -> SortCol -> SortCol
clampSortCol n (SortCol idx dir) = SortCol (clamp 0 (max 0 (n - 1)) idx) dir

-- Sort mark in bits 16-17 (see tableSortMarkOf): the low nibbles are the
-- font fields and a mark of 1 or 2 in bit 0-1 flips the header's font
-- variant, which blanks the arrow glyph.
sortMarkStyle :: SortCol -> Int -> Int
sortMarkStyle sort idx
  | sortColIndex sort /= idx = 0
  | sortColDir sort == SortDesc = 2 `shiftL` 16
  | otherwise = 1 `shiftL` 16

sortRows :: Foldable f => Colonnade Headed row Text -> SortCol -> f row -> [row]
sortRows cols sort inputRows =
  let rows = toList inputRows
      n = V.length (Encode.getColonnade cols)
      idx = sortColIndex (clampSortCol n sort)
      enc = maybe (const T.empty) Encode.oneColonnadeEncode (Encode.getColonnade cols V.!? idx)
   in case sortColDir sort of
        SortAsc -> sortOn enc rows
        SortDesc -> sortOn (Down . enc) rows

columnCount :: Colonnade Headed row Text -> Int
columnCount = V.length . Encode.getColonnade

isNumericCell :: Text -> Bool
isNumericCell txt =
  let s = T.strip txt
      digits = case T.uncons s of
        Just (c, rest) | c == '-' || c == '+' -> rest
        _ -> s
   in not (T.null digits) && T.all isDigit digits

columnMetrics :: Context -> Colonnade Headed row Text -> [row] -> IO (PrimArray Float, SmallArray Bool)
columnMetrics _ cols _ | columnCount cols == 0 = pure (emptyPrimArray, emptySmallArray)
columnMetrics ctx cols rows =
  let fm = ctxFontMetrics ctx
      mono = ctxMonoFontMetrics ctx
      cellPadX = 2 * tableCellInset
      hdrs = Encode.header id cols
      -- Encode each row once, sharing it across column classification and sizing.
      encodedRows = [Encode.row id cols r | r <- rows]
      measureColumn c hdr = do
        hdrW <- (+ cellPadX) <$> lineWidthIO fm (hdr <> tableSortReserve)
        let isNum = not (null rows) && all (isNumericCell . (V.! c)) encodedRows
            font = if isNum then mono else fm
        cellW <- foldM (\w row -> do
          width <- lineWidthIO font (row V.! c)
          pure $! max w (width + cellPadX)) minColW encodedRows
        pure (if null rows then hdrW else max hdrW cellW, isNum)
      count = V.length hdrs
   in do
        widths <- newPrimArray count
        numeric <- newSmallArray count False
        forM_ [0 .. count - 1] $ \c -> do
          (w, isNum) <- measureColumn c (hdrs V.! c)
          writePrimArray widths c w
          writeSmallArray numeric c isNum
        (,) <$> unsafeFreezePrimArray widths <*> unsafeFreezeSmallArray numeric

nextSortCol :: Int -> SortCol -> Int -> SortCol
nextSortCol n cur clicked =
  let clamped = clampSortCol n cur
   in if clicked == sortColIndex clamped
        then SortCol clicked (case sortColDir clamped of SortAsc -> SortDesc; SortDesc -> SortAsc)
        else SortCol clicked SortAsc

useTableSort :: Ui :> es => SortCol -> Eff es (SortCol, SortCol -> Eff es ())
useTableSort initial = do
  (packed, setPacked) <- useInt (packSort initial)
  pure (unpackSort packed, setPacked . packSort)

-- | Header pointer gesture on column @i@, stored as one Int in the drag slot:
-- 0 idle, @-(1000 + i)@ resizing, @-(2000 + i)@ dragging to reorder.
data HeaderDrag = HeaderIdle | HeaderResize !Int | HeaderReorder !Int
  deriving (Eq)

packHeaderDrag :: HeaderDrag -> Int
packHeaderDrag = \case
  HeaderIdle -> 0
  HeaderResize i -> -(1000 + i)
  HeaderReorder i -> -(2000 + i)

unpackHeaderDrag :: Int -> HeaderDrag
unpackHeaderDrag n
  | n <= -2000 = HeaderReorder (-2000 - n)
  | n <= -1000 = HeaderResize (-1000 - n)
  | otherwise = HeaderIdle

-- Metadata is indexed by original column id after reordering/hiding. Keep
-- it indexed throughout layout, rather than walking a list for each cell.
{-# INLINE primAt #-}
primAt :: Prim a => PrimArray a -> Int -> a -> a
primAt xs i fallback = if i >= 0 && i < sizeofPrimArray xs then indexPrimArray xs i else fallback

{-# INLINE smallAt #-}
smallAt :: SmallArray a -> Int -> a -> a
smallAt xs i fallback = if i >= 0 && i < sizeofSmallArray xs then indexSmallArray xs i else fallback

resolvedWidth :: SmallArray ColSize -> PrimArray Float -> PrimArray Float -> Int -> Float
resolvedWidth sizes contentWs stored i =
  let contentW = max minColW (primAt contentWs i minColW)
      saved = primAt stored i 0
   in case smallAt sizes i ColContent of
        ColStretch -> if saved > contentW then saved else contentW
        ColFixed f ->
          let base = max minColW f
           in if saved > 0 then max base saved else base
        ColContent -> if saved > 0 then max contentW saved else contentW

-- Width floor a column cannot shrink under: its declared fixed width, else
-- its content minimum. Shared by colSizing and the resize-drag clamp so a
-- dragged or stored width never wraps the cell text.
colFloor :: SmallArray ColSize -> PrimArray Float -> Int -> Float
colFloor sizes contentWs i = case smallAt sizes i ColContent of
  ColFixed f -> max minColW f
  _ -> max minColW (primAt contentWs i minColW)

colSizing :: Bool -> Bool -> SmallArray ColSize -> PrimArray Float -> PrimArray Float -> Int -> Sizing
colSizing fillInner hasStretch sizes contentWs stored i =
  let saved = primAt stored i 0
      floorW = colFloor sizes contentWs i
   in case smallAt sizes i ColContent of
        ColFixed _ -> Fixed (max floorW saved)
        ColStretch
          | saved > 0 -> Fixed (max floorW saved)
          | fillInner -> Grow 1
          | otherwise -> Fixed floorW
        ColContent
          | saved > 0 -> Fixed (max floorW saved)
          | fillInner && not hasStretch -> Grow 1
          | otherwise -> Fixed floorW

colBoxLayout :: Sizing -> Float -> Layout
colBoxLayout sizing minCol =
  let base =
        tight $
          defaultLayout
            { layoutGap = 0
            , layoutMinW = minCol
            , -- Columns stretch to the row height so every cell's background
              -- and borders span the full row even when one cell wraps.
              layoutHeight = Grow 1
            }
    in case sizing of
        Fixed w -> base {layoutWidth = Fixed w, layoutMaxW = w}
        Grow g -> base {layoutWidth = Grow g}
        _ -> base {layoutWidth = Fit}

writeColW :: Context -> Int -> [Float] -> IO ()
writeColW ctx key ws = do
  st <- getStore ctx
  setStore ctx (st {storeFloatList = IM.insert key ws (storeFloatList st)})

-- | Inputs tableConfigured collects for finishTable. Positional args invite silent
-- transposition (two [Float]s, several plain Floats), so keep them named.
data TableFinish = TableFinish
  { tfN :: Int
  , tfStateKey :: Int
  , tfVis :: [Int]
  , tfOrder0 :: [Int]
  , tfHidden0 :: IS.IntSet
  , tfDrag0 :: HeaderDrag
  , tfDragX0 :: Float
  , tfDragW0 :: Float
  , tfWidths0 :: [Float]
  , tfWidths1 :: [Float]
  , tfSort0 :: SortCol
  , tfHeaderPairs :: [(Int, Response)]
  , tfShowAllResp :: Maybe Response
  , tfResolvedW :: Int -> Float
  , tfBodyWid :: WidgetId
  }

finishTable :: (Ui :> es) => TableFinish -> Eff es TableResponse
finishTable TableFinish{tfN = n, tfStateKey = stateKey, tfVis = vis, tfOrder0 = order0, tfHidden0 = hidden0, tfDrag0 = drag0, tfDragX0 = dragX0, tfDragW0 = dragW0, tfWidths0 = widths0, tfWidths1 = widths1, tfSort0 = sort0, tfHeaderPairs = headerPairs, tfShowAllResp = showAllResp, tfResolvedW = resolvedW, tfBodyWid = bodyWid} = do
  ctx <- askContext
  inp <- askInput
  mBodyRect <- uiIO (getPrevRect ctx bodyWid)
  let mouse = inputMousePos inp
      mx = v2X mouse
      edgePad = 4
      -- Resize grab zone spans the header band plus the body scroller: a
      -- column boundary is resizable anywhere down the table, not just on
      -- the header cell. The bottom anchor is the body scroller's rect
      -- (prev frame: readable at build time). The resize cursor
      -- (Frame.Cursor.tableColResizeCursorKind) locates the same scroller
      -- structurally and uses its current-frame rect, so the grab zone and
      -- the cursor zone are the same rect and cannot drift apart. First
      -- frame (no prev rect yet): header band only.
      hdrSpans =
        [ (rectY rr, rectY rr + rectH rr)
        | (_, r) <- headerPairs
        , let rr = rawRespRect r
        ]
      (edgeTop, edgeBot) = case hdrSpans of
        [] -> (0, 0)
        _ ->
          ( minimum (map fst hdrSpans)
          , maybe (maximum (map snd hdrSpans)) (\(Rect _ by _ bh) -> by + bh) mBodyRect
          )
      edgeCol = headerEdgeHit edgePad edgeTop edgeBot headerPairs mouse
      hoverCol = headerAtPoint headerPairs mouse
      headerRects = [(i, rawRespRect r) | (i, r) <- headerPairs]
      (isResize, isReorder) = case drag0 of
        HeaderResize _ -> (True, False)
        HeaderReorder _ -> (False, True)
        HeaderIdle -> (False, False)
      resizing = isResize && inputMouseDown inp
  (vis', mReorder) <-
    withKey ("reorder" :: Text) $
      useReorder vis (if resizing || isJust edgeCol then [] else headerRects)
  let dragged = isReorder && abs (mx - dragX0) > dragThresholdPx
      pressResize = inputMousePressed inp && isJust edgeCol
      pressReorder = inputMousePressed inp && edgeCol == Nothing && isJust hoverCol
      nextDrag
        | pressResize = maybe HeaderIdle HeaderResize edgeCol
        | pressReorder = maybe HeaderIdle HeaderReorder hoverCol
        | inputMouseReleased inp || not (inputMouseDown inp) = HeaderIdle
        | otherwise = drag0
      nextDragX
        | pressResize || pressReorder = mx
        | nextDrag == HeaderIdle = 0
        | otherwise = dragX0
      nextDragW
        | pressResize = maybe 0 headerW edgeCol
        | nextDrag == HeaderIdle = 0
        | otherwise = dragW0
      headerW i = maybe (resolvedW i) (\r -> let w = rectW (rawRespRect r) in if w > 0 then w else resolvedW i) (lookup i headerPairs)
      nextOrder = if vis' /= vis then rebuildOrder hidden0 vis' order0 else order0
      -- respRightClicked, not a bare release: a right press that went down
      -- elsewhere and came up over a header must not hide that column.
      hideClicked = [i | (i, r) <- headerPairs, respRightClicked r, drag0 == HeaderIdle]
      nextHidden = case showAllResp of
        Just r | respClicked r -> IS.empty
        _ -> case hideClicked of
          (i : _) | IS.size hidden0 + 1 < n -> IS.insert i hidden0
          _ -> hidden0
      sortClick =
        if dragged || isJust mReorder || vis' /= vis || isResize
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
  -- Compare the five slots, not the whole store: rewriting the store only
  -- when a slot moved keeps an idle table from diffing every map each frame.
  uiIO $ do
    st <- getStore ctx
    let dragCode = packHeaderDrag nextDrag
        dragK = slotKey SlotDrag stateKey
        dragWK = slotKey SlotDragW stateKey
        unchanged =
          IM.lookup stateKey (storeIntList st) == Just nextOrder
            && IM.lookup stateKey (storeIntSet st) == Just nextHidden
            && IM.lookup dragK (storeInt st) == Just dragCode
            && IM.lookup stateKey (storeFloat st) == Just nextDragX
            && IM.lookup dragWK (storeFloat st) == Just nextDragW
    unless unchanged $
      setStore
        ctx
        st
          { storeIntList = IM.insert stateKey nextOrder (storeIntList st)
          , storeIntSet = IM.insert stateKey nextHidden (storeIntSet st)
          , storeInt = IM.insert dragK dragCode (storeInt st)
          , storeFloat =
              IM.insert stateKey nextDragX $
                IM.insert dragWK nextDragW (storeFloat st)
          }
  pure (TableResponse widgetResp nextSort nextOrder nextHidden)

-- | Sortable table with resizable, reorderable columns. @key@ tells tables in
-- one scope apart, and the columns are a colonnade over @row@. Pass the
-- current sort; the 'TableResponse' carries the sort after this frame's
-- header clicks, along with the column order and hidden columns.
{-# INLINE table #-}
table :: (Foldable f, Ui :> es) => Text -> Colonnade Headed row Text -> f row -> SortCol -> Eff es TableResponse
table = tableConfigured defaultTableConfig id

-- | 'table' with a layout modifier.
{-# INLINE tableWith #-}
tableWith :: (Foldable f, Ui :> es) => (Layout -> Layout) -> Text -> Colonnade Headed row Text -> f row -> SortCol -> Eff es TableResponse
tableWith = tableConfigured defaultTableConfig

-- | A table of text rows under the given headers.
simpleTable :: (Foldable f, Ui :> es) => [Text] -> f [Text] -> Eff es TableResponse
simpleTable headers rows = do
  let cols = mconcat [headed h (\r -> smallAt r i "") | (i, h) <- zip [0 ..] headers]
      indexedRows = map smallArrayFromList (toList rows)
  table "simple" cols indexedRows (SortCol 0 SortAsc)

-- | 'tableWith' with column sizes, frozen rows and columns, and initially
-- hidden columns.
tableConfigured ::
  (Foldable f, Ui :> es) =>
  TableConfig ->
  (Layout -> Layout) ->
  Text ->
  Colonnade Headed row Text ->
  f row ->
  SortCol ->
  Eff es TableResponse
tableConfigured cfg f key cols inputRows curSort =
  withKey ("table:" <> key) $ do
    let outerLayout = f (tight . fillW $ defaultLayout {layoutGap = 0})
    stateWid <- nextId
    vWid <- nextId
    hWid <- nextId
    tableWid <- nextId
    let rows = toList inputRows
        n = columnCount cols
        sort0 = clampSortCol n curSort
        stateKey = intKey stateWid
    ctx <- askContext
    inp <- askInput
    st0 <- uiIO (getStore ctx)
    (!contentWs, !numeric) <- uiIO (columnMetrics ctx cols rows)
    let sizes = smallArrayFromList (tableColSizes cfg)
        order0 = normalizeOrder n (IM.findWithDefault [0 .. n - 1] stateKey (storeIntList st0))
        hidden0 = IM.findWithDefault (tableHidden cfg) stateKey (storeIntSet st0)
        widths0 = fitList n 0 (IM.findWithDefault [] stateKey (storeFloatList st0))
        drag0 = unpackHeaderDrag (IM.findWithDefault 0 (slotKey SlotDrag stateKey) (storeInt st0))
        dragX0 = IM.findWithDefault 0 stateKey (storeFloat st0)
        dragW0 = IM.findWithDefault 0 (slotKey SlotDragW stateKey) (storeFloat st0)
        mx = v2X (inputMousePos inp)
        -- A drag cannot push a column under its colFloor: the column reserved
        -- that much space for its text, and going under it wraps the cell and
        -- drags the whole row taller.
        widths1 = case drag0 of
          HeaderResize c
            | inputMouseDown inp ->
                setAt c (max (colFloor sizes contentWs c) (dragW0 + mx - dragX0)) widths0
          _ -> widths0
    when (widths1 /= widths0) $ uiIO $ writeColW ctx stateKey widths1
    let hasStretch = tableStretchN n (tableColSizes cfg)
        indexedWidths = primArrayFromList widths1
        vis = visibleCols order0 hidden0
        freezeN = clamp 0 (length vis) (tableFreezeCols cfg)
        freezeR = max 0 (tableFreezeRows cfg)
        sorted = sortRows cols sort0 rows
        hdrs = Encode.header id cols
        rowMinH = 28
        fillInner = tableFillInner hasStretch outerLayout
        mins = generatePrimArray n (resolvedWidth sizes contentWs indexedWidths)
        colBoxes = smallArrayFromList [colBoxLayout (colSizing fillInner hasStretch sizes contentWs indexedWidths i) (primAt mins i minColW) | i <- [0 .. n - 1]]
        resolvedW i = primAt mins i minColW
        cellLayouts = smallArrayFromList $ flip map [0 .. n - 1] $ \i ->
          (tight defaultLayout)
              { layoutWidth = Grow 1
              , layoutHeight = Grow 1
              , layoutAlignX = if smallAt numeric i False then AlignEnd else AlignStart
              , layoutAlignY = AlignMiddle
              , layoutMinH = rowMinH
              , layoutFontVariant = if smallAt numeric i False then FontMono else FontRegular
              }
        cellLayout i = smallAt cellLayouts i (tight defaultLayout)
        renderHeader i =
          let !lay = cellLayout i
           in buttonStyled (tableHeaderLabel (fromMaybe T.empty (hdrs V.!? i))) (if sortColIndex sort0 == i then 1 else 0) lay (sortMarkStyle sort0 i .|. buttonFlagTable)
        renderCell ri r =
          let !rowCells = Encode.row id cols r
           in \i ->
                let !lay = cellLayout i
                 in void (stripedRow ri lay (rowCells V.! i))
    column' outerLayout $ do
      showAllResp <-
        if IS.null hidden0
          then pure Nothing
          else fmap Just $
            buttonStyled "Show all columns" 0 (tight . fillW $ defaultLayout) 0
      headerPairs <-
        tableSplitPanes
          TablePanes
            { tpFillInner = fillInner
            , tpTableWid = tableWid
            , tpBodyWid = vWid
            , tpHeaderWid = hWid
            , tpRowMinH = rowMinH
            , tpFrozen = take freezeN vis
            , tpUnfrozen = drop freezeN vis
            , tpPinned = take freezeR sorted
            , tpScrollRows = drop freezeR sorted
            , tpColBox = \i -> smallAt colBoxes i (tight defaultLayout)
            , tpRenderHeader = renderHeader
            , tpRenderCell = renderCell
            }
      finishTable
        TableFinish
          { tfN = n
          , tfStateKey = stateKey
          , tfVis = vis
          , tfOrder0 = order0
          , tfHidden0 = hidden0
          , tfDrag0 = drag0
          , tfDragX0 = dragX0
          , tfDragW0 = dragW0
          , tfWidths0 = widths0
          , tfWidths1 = widths1
          , tfSort0 = sort0
          , tfHeaderPairs = headerPairs
          , tfShowAllResp = showAllResp
          , tfResolvedW = resolvedW
          , tfBodyWid = vWid
          }
