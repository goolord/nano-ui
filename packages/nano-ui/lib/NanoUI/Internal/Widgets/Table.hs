{-# LANGUAGE OverloadedStrings #-}

-- | Text tables described by Colonnade columns, with sorting, frozen panes,
-- column resizing/reordering, and row virtualisation.
module NanoUI.Internal.Widgets.Table
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
import Control.Monad (forM, forM_, void, when)
import Control.Monad.ST (runST)
import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (sortOn)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Primitive.PrimArray (PrimArray, generatePrimArray, indexPrimArray, newPrimArray, primArrayFromList, readPrimArray, sizeofPrimArray, unsafeFreezePrimArray, writePrimArray)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, mapSmallArray', newSmallArray, sizeofSmallArray, smallArrayFromList, unsafeFreezeSmallArray, writeSmallArray)
import Data.Primitive.Types (Prim)
import Data.Vector qualified as V
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context (Context (..), getPrevRect, getScrollOffset2D, getStore, intKey, linkScrollAxes, modifyStore, writeSlots)
import NanoUI.Internal.Hooks (useInt)
import NanoUI.Internal.Font (ScrollBarSlot (..), scrollBarGutter, tableCellInset, lineWidthIO)
import NanoUI.Internal.Input (Input (..), inputMouseDown, inputMousePos, inputMousePressed, inputMouseReleased)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Internal.Store (Slot (..), fieldFloat, fieldFloatList, fieldInt, fieldIntList, fieldIntSet, findSlot, insertSlot, slotKey, slotWrite)
import NanoUI.Internal.Style (AlignX (..), AlignY (..), Direction (..), FontVariant (..), Layout (..), Padding (..), Sizing (..), defaultLayout, fillH, fillW, tight)
import Data.Bits ((.|.), shiftL)
import NanoUI.Internal.Types (Rect (..), clamp, rectH, rectW, rectY, v2X, V2 (..), rectContains)
import NanoUI.Internal.WidgetText (buttonFlagTable, tableHeaderLabel, tableSortReserve)
import NanoUI.Internal.Widgets.Behavior (dragThresholdPx, useReorder)
import NanoUI.Internal.Widgets.Combinators (buttonStyled)
import NanoUI.Internal.Widgets.Layout (column', panel', row', scrollAreaIdConfigured, separator, spacer)
import NanoUI.Internal.Frame.Scroll.Geometry (ScrollConfig (..), ScrollPolicy (..), scrollHorizontalHidden, scrollVerticalAuto, scrollVerticalHidden)
import NanoUI.Internal.Widgets.Node
  ( HasResponse (..)
  , Response (..)
  , rawRespRect
  , respClicked
  , respRightClicked
  , setChanged
  , setClicked
  , tagContainer
  , addWidgetStyled
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

-- | Ascending or descending text order.
data SortDir = SortAsc | SortDesc
  deriving (Eq, Show, Enum, Bounded)

-- | Sort column by zero-based source-column index, independent of display order.
data SortCol = SortCol {sortColIndex :: !Int, sortColDir :: !SortDir}
  deriving (Eq, Show)

-- | Size to content, share spare space, or request a fixed logical-pixel width.
data ColSize = ColContent | ColStretch | ColFixed Float
  deriving (Eq, Show)

-- | Frozen leading row/column counts, column sizing, and hidden source-column
-- indices. Column indices are zero-based; unspecified sizes use content sizing.
data TableConfig = TableConfig
  { tableFreezeCols :: {-# UNPACK #-} !Int
  , tableFreezeRows :: {-# UNPACK #-} !Int
  , tableColSizes :: ![ColSize]
  , tableHidden :: !IntSet
  }
  deriving (Eq, Show)

-- | No frozen or hidden rows/columns and content-sized columns.
defaultTableConfig :: TableConfig
defaultTableConfig = TableConfig 0 0 [] IS.empty

-- | Widget interaction plus updated sort, display order, and hidden-column set.
-- Indices refer to the original column definitions.
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

-- | Hidden source-column indices in ascending order.
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

-- | Stable sort by the selected column's rendered text, not numeric value.
-- Out-of-range column indices are clamped; with no columns, input order is retained.
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

-- | Content width and numeric flag of each column, measured once over the
-- encoded rows.
columnMetrics :: Context -> V.Vector Text -> SmallArray (V.Vector Text) -> IO (PrimArray Float, SmallArray Bool)
columnMetrics ctx hdrs encoded = do
  let fm = ctxFontMetrics ctx
      mono = ctxMonoFontMetrics ctx
      cellPadX = 2 * tableCellInset
      count = V.length hdrs
      nRows = sizeofSmallArray encoded
      cell r c = indexSmallArray encoded r V.! c
  widths <- newPrimArray count
  numeric <- newSmallArray count False
  forM_ [0 .. count - 1] $ \c -> do
    hdrW <- (+ cellPadX) <$> lineWidthIO fm (hdrs V.! c <> tableSortReserve)
    let numericFrom !r = r >= nRows || (isNumericCell (cell r c) && numericFrom (r + 1))
        isNum = nRows > 0 && numericFrom 0
        font = if isNum then mono else fm
        widest !r !w
          | r >= nRows = pure w
          | otherwise = do
              width <- lineWidthIO font (cell r c)
              widest (r + 1) (max w (width + cellPadX))
    cellW <- widest 0 minColW
    writePrimArray widths c (if nRows == 0 then hdrW else max hdrW cellW)
    writeSmallArray numeric c isNum
  (,) <$> unsafeFreezePrimArray widths <*> unsafeFreezeSmallArray numeric

nextSortCol :: Int -> SortCol -> Int -> SortCol
nextSortCol n cur clicked =
  let clamped = clampSortCol n cur
   in if clicked == sortColIndex clamped
        then SortCol clicked (case sortColDir clamped of SortAsc -> SortDesc; SortDesc -> SortAsc)
        else SortCol clicked SortAsc

-- | Local sort state and setter. Call in a stable hook position each frame.
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
    let n = columnCount cols
        sort0 = clampSortCol n curSort
        stateKey = intKey stateWid
        hdrs = Encode.header id cols
        -- Each row is encoded once and shared by measuring, sorting and the
        -- cells; the sort orders row indices.
        encoded = smallArrayFromList [Encode.row id cols r | r <- toList inputRows]
    ctx <- askContext
    inp <- askInput
    st0 <- uiIO (getStore ctx)
    (!contentWs, !numeric) <- uiIO (columnMetrics ctx hdrs encoded)
    let sizes = smallArrayFromList (tableColSizes cfg)
        order0 = normalizeOrder n (findSlot fieldIntList [0 .. n - 1] stateKey st0)
        hidden0 = findSlot fieldIntSet (tableHidden cfg) stateKey st0
        widths0 = take n (findSlot fieldFloatList [] stateKey st0 ++ repeat 0)
        drag0 = unpackHeaderDrag (findSlot fieldInt 0 (slotKey SlotDrag stateKey) st0)
        dragX0 = findSlot fieldFloat 0 stateKey st0
        dragW0 = findSlot fieldFloat 0 (slotKey SlotDragW stateKey) st0
        mx = v2X (inputMousePos inp)
        -- A drag cannot push a column under its colFloor: the column reserved
        -- that much space for its text, and going under it wraps the cell and
        -- drags the whole row taller.
        widths1 = case drag0 of
          HeaderResize c
            | inputMouseDown inp ->
                setAt c (max (colFloor sizes contentWs c) (dragW0 + mx - dragX0)) widths0
          _ -> widths0
    when (widths1 /= widths0) $ uiIO $
      modifyStore ctx (insertSlot fieldFloatList stateKey widths1)
    let hasStretch = tableStretchN n (tableColSizes cfg)
        indexedWidths = primArrayFromList widths1
        vis = filter (`IS.notMember` hidden0) order0
        freezeN = clamp 0 (length vis) (tableFreezeCols cfg)
        frozenIdx = take freezeN vis
        unfrozenIdx = drop freezeN vis
        nRows = sizeofSmallArray encoded
        sorted =
          sortIndices
            (sortColDir sort0)
            (mapSmallArray' (\cells -> fromMaybe T.empty (cells V.!? sortColIndex sort0)) encoded)
        pinnedN = min nRows (max 0 (tableFreezeRows cfg))
        scrollN = nRows - pinnedN
        rowMinH = 28
        fillInner = tableFillInner hasStretch outerLayout
        mins = generatePrimArray n (resolvedWidth sizes contentWs indexedWidths)
        colBoxes = smallArrayFromList [colBoxLayout (colSizing fillInner hasStretch sizes contentWs indexedWidths i) (primAt mins i minColW) | i <- [0 .. n - 1]]
        colBox i = smallAt colBoxes i (tight defaultLayout)
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
        -- Cell @i@ of display row @ri@, which shows encoded row @r@.
        renderCell ri r i = do
          wid <- nextId
          void (addWidgetStyled wid NodeText (indexSmallArray encoded r V.! i) 0 (cellLayout i) (if even ri then 1 else 2))
        rowCells rowLay idxs colLays ri =
          gridColumnsLay rowLay idxs colLays [renderCell ri (indexPrimArray sorted ri) i | i <- idxs]
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
        -- Header row, its rule, the pinned rows and their rule: the same in both
        -- panes.
        headerBlock idxs = do
          hs <- row' (gridRowLay idxs) $
            forM (zip [0 :: Int ..] idxs) $ \(k, i) -> do
              when (k > 0) $ void separator
              withKey i $
                column' (colBox i) $
                  buttonStyled (tableHeaderLabel (fromMaybe T.empty (hdrs V.!? i))) (if sortColIndex sort0 == i then 1 else 0) (cellLayout i) (sortMarkStyle sort0 i .|. buttonFlagTable)
          void separator
          let !rowLay = gridRowLay idxs
              !colLays = map colBox idxs
          forM_ [0 .. pinnedN - 1] $ \ri ->
            withKey ("pin" :: Text, ri) $ do
              when (ri > 0) $ void separator
              rowCells rowLay idxs colLays ri
          when (pinnedN > 0 && scrollN > 0) $ void separator
          pure hs
        bodyBlock idxs = do
          (lo, hi) <-
            if scrollN == 0
              then pure (0, -1)
              else uiIO $ do
                V2 _ scrollY <- getScrollOffset2D ctx vWid
                viewH <- maybe (rowMinH * 8) rectH <$> getPrevRect ctx vWid
                pure (listClipper scrollN scrollY viewH rowMinH)
          let !rowLay = gridRowLay idxs
              !colLays = map colBox idxs
              topH = fromIntegral lo * rowMinH
              botH = fromIntegral (max 0 (scrollN - hi - 1)) * rowMinH
          column' rowLay $ do
            when (topH > 0) $ void (spacer Fit (Fixed topH))
            forM_ [lo .. hi] $ \rowIdx ->
              withKey rowIdx $ do
                when (rowIdx > 0) $ void separator
                rowCells rowLay idxs colLays (rowIdx + pinnedN)
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
          -- The body scroller has no padding, so its whole lane is gutter.
          let vGutter = scrollBarGutter ScrollBarList 0
              idxs = unfrozenIdx
          mPrevV <- uiIO (getPrevRect ctx vWid)
          let totalH = fromIntegral scrollN * rowMinH
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
                    hWid
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
            uiIO (linkScrollAxes ctx vWid hWid)
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
    column' outerLayout $ do
      showAllResp <-
        if IS.null hidden0
          then pure Nothing
          else fmap Just $
            buttonStyled "Show all columns" 0 (tight . fillW $ defaultLayout) 0
      headerPairs <-
        panel' paneRoot $ do
          tagContainer tableWid
          row' (paneRoot {layoutGap = 0}) $ do
            frozenHs <-
              if null frozenIdx
                then pure []
                else zip frozenIdx <$> frozenPane
            when (not (null frozenIdx) && not (null unfrozenIdx)) $ void separator
            unfrozenHs <-
              if null unfrozenIdx then pure [] else zip unfrozenIdx <$> unfrozenPane
            pure (frozenHs ++ unfrozenHs)
      mBodyRect <- uiIO (getPrevRect ctx vWid)
      let mouse = inputMousePos inp
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
          hoverCol = listToMaybe [i | (i, r) <- headerPairs, rectContains (rawRespRect r) mouse]
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
      uiIO . writeSlots ctx $
        slotWrite fieldIntList stateKey nextOrder
          <> slotWrite fieldIntSet stateKey nextHidden
          <> slotWrite fieldInt (slotKey SlotDrag stateKey) (packHeaderDrag nextDrag)
          <> slotWrite fieldFloat stateKey nextDragX
          <> slotWrite fieldFloat (slotKey SlotDragW stateKey) nextDragW
      pure (TableResponse widgetResp nextSort nextOrder nextHidden)

-- | One row of cells with custom row layout.
gridColumnsLay :: (Ui :> es) => Layout -> [Int] -> [Layout] -> [Eff es ()] -> Eff es ()
gridColumnsLay lay keys layouts cells =
  void (row' lay (go True keys layouts cells))
 where
  -- Walk in lockstep without allocating zip tuples and indices per cell.
  go first (key : moreKeys) (layout : moreLayouts) (cell : moreCells) = do
    when (not first) $ void separator
    void (withKey key (column' layout cell))
    go False moreKeys moreLayouts moreCells
  go _ _ _ _ = pure ()

-- | Indices of @keys@ stably sorted by key: a bottom-up merge sort between
-- two index buffers.
sortIndices :: SortDir -> SmallArray Text -> PrimArray Int
sortIndices dir keys = runST $ do
  let n = sizeofSmallArray keys
      before l r = case compare (indexSmallArray keys l) (indexSmallArray keys r) of
        LT -> dir == SortAsc
        GT -> dir == SortDesc
        EQ -> True
  start <- newPrimArray n
  let fill !i = when (i < n) (writePrimArray start i i >> fill (i + 1))
  fill 0
  spare <- newPrimArray n
  let pass !src !dst !width
        | width >= n = unsafeFreezePrimArray src
        | otherwise = do
            let mergeFrom !lo = when (lo < n) $ do
                  let !mid = min n (lo + width)
                      !hi = min n (lo + 2 * width)
                      takeLeft !i !j !k = readPrimArray src i >>= writePrimArray dst k >> go (i + 1) j (k + 1)
                      takeRight !i !j !k = readPrimArray src j >>= writePrimArray dst k >> go i (j + 1) (k + 1)
                      go !i !j !k
                        | k >= hi = pure ()
                        | i >= mid = takeRight i j k
                        | j >= hi = takeLeft i j k
                        | otherwise = do
                            l <- readPrimArray src i
                            r <- readPrimArray src j
                            if before l r then takeLeft i j k else takeRight i j k
                  go lo mid lo
                  mergeFrom hi
            mergeFrom 0
            pass dst src (2 * width)
  pass start spare 1

-- | First and last visible item index for a uniform-height list, or
-- @(0, -1)@ when nothing is visible.
{-# INLINE listClipper #-}
listClipper :: Int -> Float -> Float -> Float -> (Int, Int)
listClipper itemCount scrollOff viewH itemH
  | itemCount <= 0 || itemH <= 0 || viewH <= 0 = (0, -1)
  | otherwise =
      let firstVis = max 0 (floor (scrollOff / itemH))
          lastVis = min (itemCount - 1) (floor ((scrollOff + viewH - 1) / itemH))
       in if lastVis < firstVis then (0, -1) else (firstVis, lastVis)

setAt :: Int -> a -> [a] -> [a]
setAt i x xs
  | i < 0 = xs
  | otherwise = case splitAt i xs of
      (before, _ : after) -> before ++ x : after
      (_, []) -> xs

normalizeOrder :: Int -> [Int] -> [Int]
normalizeOrder n stored =
  let valid = filter (\i -> i >= 0 && i < n) stored
      seen = IS.fromList valid
   in valid ++ [i | i <- [0 .. n - 1], not (IS.member i seen)]

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

-- | Hit-test a column resize edge. The grab zone spans the whole column
-- height (header top to body bottom), so a column can be resized by its
-- boundary line anywhere down the table, not just on the header cell.
headerEdgeHit :: Float -> Float -> Float -> [(Int, Response)] -> V2 -> Maybe Int
headerEdgeHit pad yTop yBot cols mouse =
  listToMaybe
    [ i
    | (i, r) <- cols
    , let Rect x y w h = rawRespRect r
    , w > 0 && h > 0
    , let mx = v2X mouse
          my = v2Y mouse
    , my >= min y yTop && my <= max (y + h) yBot
    , abs (mx - (x + w)) <= pad
    ]
