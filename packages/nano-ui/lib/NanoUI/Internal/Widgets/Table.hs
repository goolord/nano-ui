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
import Control.Monad (forM, forM_, mfilter, unless, void, when)
import Control.Monad.ST (runST)
import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.IORef (modifyIORef')
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (find, sortOn)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Primitive.PrimArray (PrimArray, copyMutablePrimArray, copyPrimArray, generatePrimArray, indexPrimArray, newPrimArray, primArrayFromList, readPrimArray, sizeofPrimArray, unsafeFreezePrimArray, writePrimArray)
import Data.Primitive.SmallArray (SmallArray, emptySmallArray, indexSmallArray, mapSmallArray', newSmallArray, sizeofSmallArray, smallArrayFromList, smallArrayFromListN, unsafeFreezeSmallArray, writeSmallArray)
import Data.Primitive.Types (Prim)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import NanoUI.Internal.Context (Context (..), InteractionState (..), getPrevRect, getScrollOffset2D, getStore, intKey, linkScrollAxes, modifyInteraction, writeSlots)
import NanoUI.Internal.Hooks (useInt)
import NanoUI.Internal.Font (ScrollBarSlot (..), scrollBarGutter, tableCellInset, lineWidthIO)
import NanoUI.Internal.Input (Input (..), MouseButton (..), Pressable (..), UiCursorKind (..))
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (NanoUI, askInput, freshWidget, lastRect, nextId, liftIO, withKey)
import NanoUI.Internal.Store (Slot (..), SlotWrites (..), eqByPtr, fieldFloat, fieldInt, fieldIntSet, findSlot, insertDyn, lookupDyn, slotKey, slotWrite)
import NanoUI.Internal.Style (AlignX (..), AlignY (..), Direction (..), FontVariant (..), Layout (..), Sizing (..), defaultLayout, fillH, fillW, minW, tight)
import Data.Bits ((.|.), shiftL)
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import NanoUI.Internal.Types (Rect (..), clamp, rectH, rectW, v2X, V2 (..), rectContains)
import NanoUI.Internal.WidgetText (buttonFlagTable, tableHeaderLabel, tableSortReserve)
import NanoUI.Internal.Widgets.Behavior (Reorder (..), useReorder)
import NanoUI.Internal.Widgets.Combinators (buttonStyledEx, readDerived, writeDerived)
import NanoUI.Internal.Widgets.Layout (column', panel', row', scrollAreaIdConfigured, separator, spacer)
import NanoUI.Internal.Frame.Scroll.Geometry (defaultScrollConfig, scrollHorizontalHidden, scrollVerticalAuto, scrollVerticalHidden)
import NanoUI.Internal.Widgets.Node

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
packSort (SortCol c dir) = c * 2 + fromEnum dir

unpackSort :: Int -> SortCol
unpackSort n = SortCol (n `div` 2) (toEnum (n `mod` 2))

clampSortCol :: Int -> SortCol -> SortCol
clampSortCol n (SortCol idx dir) = SortCol (clamp 0 (max 0 (n - 1)) idx) dir

-- The sort mark uses bits 16-17 (see tableSortMarkOf), above the font fields.
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

isNumericCell :: Text -> Bool
isNumericCell txt =
  let s = T.strip txt
      digits = case T.uncons s of
        Just (c, rest) | c == '-' || c == '+' -> rest
        _ -> s
   in not (T.null digits) && T.all isDigit digits

-- | Data derived from a table's rows: cell text, each column's content width
-- and numeric flag, and the sorted row order. Cached in 'ctxDerivedCache'
-- with each row's object and each cell's width, so a frame encodes only rows
-- that are not last frame's objects, measures only cells whose text changed,
-- and sorts only when a sort key changed.
data TableDerived = TableDerived
  { tdRows :: !Opaque
  , tdCols :: !Opaque
  , tdFont :: !Opaque
  , tdMonoFont :: !Opaque
    -- ^ The sans and mono metrics the cells were measured with.
  , tdRowObjs :: !(SmallArray Opaque)
  , tdHeaders :: !(V.Vector Text)
  , tdEncoded :: !(SmallArray (V.Vector Text))
  , tdCellW :: !(PrimArray Float)
    -- ^ Each cell's padded width in its column's font, column by column.
  , tdTextRow :: !(PrimArray Int)
    -- ^ For each column, a row whose cell is not numeric, or -1 when every
    -- cell is.
  , tdWidths :: !(PrimArray Float)
  , tdNumeric :: !(SmallArray Bool)
  , tdSort :: !SortCol
  , tdOrder :: !(PrimArray Int)
  }

-- | A value of any type, kept only to compare by pointer.
data Opaque = forall a. Opaque a

samePtr :: Opaque -> b -> Bool
samePtr (Opaque a) b = isTrue# (reallyUnsafePtrEquality# a b)

-- | The table's derived data for @rows@ under @sort@: the cached data when
-- the rows and columns are the ones it was derived from, else derived again
-- from what it holds. The rows and columns are evaluated first, so the
-- pointers kept and compared are their values, whether or not the code that
-- derives the data evaluates them before keeping them.
tableDerived :: Foldable f => Context -> Int -> Colonnade Headed row Text -> f row -> SortCol -> IO TableDerived
tableDerived ctx key !cols !rows sort = do
  cached <- readDerived ctx key
  let hdrs = Encode.header id cols
  derived <- case cached of
    Just d | samePtr (tdRows d) rows && samePtr (tdCols d) cols -> pure d
    _ -> rederive ctx cols rows sort hdrs (mfilter ((== hdrs) . tdHeaders) cached)
  let !resorted
        | tdSort derived == sort = derived
        | otherwise = derived {tdSort = sort, tdOrder = orderFor sort (tdEncoded derived)}
  case cached of
    Just d | samePtr (Opaque d) resorted -> pure ()
    _ -> writeDerived ctx key resorted
  pure resorted

-- | Row indices sorted by the sort column's text.
orderFor :: SortCol -> SmallArray (V.Vector Text) -> PrimArray Int
orderFor sort cells = sortIndices (sortColDir sort) (mapSmallArray' (sortKey sort) cells)

sortKey :: SortCol -> V.Vector Text -> Text
sortKey sort row = fromMaybe T.empty (row V.!? sortColIndex sort)

-- | A row's cells, each evaluated. Every cell of an encoded row is compared
-- or measured, so none is left as a thunk.
encodeRow :: V.Vector (Encode.OneColonnade Headed row Text) -> row -> V.Vector Text
encodeRow encoders r = V.create $ do
  let k = V.length encoders
  cells <- MV.unsafeNew k
  let fill !j = when (j < k) $ do
        MV.unsafeWrite cells j $! Encode.oneColonnadeEncode (V.unsafeIndex encoders j) r
        fill (j + 1)
  fill 0
  pure cells

-- | The first cell at which two rows differ, or -1 when they are the same.
firstDiff :: V.Vector Text -> V.Vector Text -> Int
firstDiff a b = go 0
  where
    k = min (V.length a) (V.length b)
    go !j
      | j >= k = if V.length a == V.length b then -1 else j
      | eqByPtr (V.unsafeIndex a j) (V.unsafeIndex b j) = go (j + 1)
      | otherwise = j

-- | Derive from @rows@, reusing @old@ (derived under the same headers):
-- each row that is the same object under the same columns keeps its text,
-- each cell whose text is unchanged keeps its width, and the order stands
-- while no sort key changed. Widths measured with other fonts are kept only
-- while no text changed.
rederive :: Foldable f => Context -> Colonnade Headed row Text -> f row -> SortCol -> V.Vector Text -> Maybe TableDerived -> IO TableDerived
rederive Context {ctxFontMetrics = fm, ctxMonoFontMetrics = mono} cols rows sort hdrs old = do
  -- Bound evaluated, so the loops below do not enter them again.
  let !n = length rows
      !c = V.length hdrs
      !oldN = maybe 0 (sizeofSmallArray . tdEncoded) old
      !oldObjs = maybe emptySmallArray tdRowObjs old
      !oldEnc = maybe emptySmallArray tdEncoded old
      !sameCols = any (\d -> samePtr (tdCols d) cols) old
      !encoders = Encode.getColonnade cols
  objsM <- newSmallArray n (Opaque ())
  encM <- newSmallArray n V.empty
  changedM <- newPrimArray n
  diffM <- newPrimArray n
  countM <- newPrimArray 1
  let store i o e = writeSmallArray objsM i o >> writeSmallArray encM i e
      note k i d = writePrimArray changedM k i >> writePrimArray diffM k d
      -- Lists the rows whose text changed or is new, each with its first
      -- changed cell (0 for a new row), and stores their count in countM,
      -- which keeps the counter unboxed. Rows are compared evaluated: a
      -- list's elements are often fresh thunks over the same objects. Old
      -- entries are read strictly, so kept text does not hold on to last
      -- frame's arrays.
      walk !_ !k [] = writePrimArray countM 0 k
      walk !i !k (!r : rs)
        | i < oldN = do
            let !o = indexSmallArray oldObjs i
                !e = indexSmallArray oldEnc i
            if sameCols && samePtr o r
              then store i o e >> walk (i + 1) k rs
              else do
                let !e' = encodeRow encoders r
                    !d = firstDiff e' e
                if d < 0
                  then store i (Opaque r) e >> walk (i + 1) k rs
                  else store i (Opaque r) e' >> note k i d >> walk (i + 1) (k + 1) rs
        | otherwise = store i (Opaque r) (encodeRow encoders r) >> note k i 0 >> walk (i + 1) (k + 1) rs
  walk (0 :: Int) (0 :: Int) (toList rows)
  nChanged <- readPrimArray countM 0
  objs <- unsafeFreezeSmallArray objsM
  case old of
    Just d | n == oldN && nChanged == 0 -> pure d {tdRows = Opaque rows, tdCols = Opaque cols, tdRowObjs = objs}
    _ -> do
      encoded <- unsafeFreezeSmallArray encM
      changedRows <- unsafeFreezePrimArray changedM
      firstDiffs <- unsafeFreezePrimArray diffM
      let !cellPadX = 2 * tableCellInset
          cell r i = indexSmallArray encoded r V.! i
          oldCell r i = indexSmallArray oldEnc r V.! i
          -- Whether cell i of changed row r, first changed at cell f, is
          -- kept: cells before f are, f is not, later ones are compared.
          keptCell !r !f !i = r < oldN && (i < f || (i > f && eqByPtr (oldCell r i) (cell r i)))
          -- The first changed row r, first changed at f, with p r f; or -1.
          {-# INLINE findChanged #-}
          findChanged p = go 0
            where
              go !j
                | j >= nChanged = -1
                | p r (indexPrimArray firstDiffs j) = r
                | otherwise = go (j + 1)
                where
                  r = indexPrimArray changedRows j
          -- Each column's row whose cell is not numeric, or -1. The old row
          -- stands while its cell still is not; a column whose old cells were
          -- all numeric looks only at changed rows, from their first changed
          -- cell; any other column is scanned to its first cell that is not,
          -- as without old data.
          !textRows = generatePrimArray c $ \i ->
            let isText r = not (isNumericCell (cell r i))
                scan !r
                  | r >= n = -1
                  | isText r = r
                  | otherwise = scan (r + 1)
             in case old of
                  Just d
                    | w < 0 -> findChanged (\r f -> not (r < oldN && i < f) && isText r)
                    | w < n && isText w -> w
                    where
                      w = indexPrimArray (tdTextRow d) i
                  _ -> scan 0
          isNum i = n > 0 && indexPrimArray textRows i < 0
          -- The old data, if measured with these fonts.
          prior = mfilter (\d -> samePtr (tdFont d) fm && samePtr (tdMonoFont d) mono) old
      cellWM <- newPrimArray (n * c)
      hdrWM <- newPrimArray c
      forM_ [0 .. c - 1] $ \i -> do
        let font = if isNum i then mono else fm
            measure !r = do
              let !t = cell r i
              writePrimArray cellWM (i * n + r) . (+ cellPadX) =<< lineWidthIO font t
        case prior of
          -- The column's font is unchanged, so its cells whose text is
          -- unchanged keep their widths, in changed rows too.
          Just d | indexSmallArray (tdNumeric d) i == isNum i -> do
            copyPrimArray cellWM (i * n) (tdCellW d) (i * oldN) (min n oldN)
            let remeasure !j = when (j < nChanged) $ do
                  let !r = indexPrimArray changedRows j
                  unless (keptCell r (indexPrimArray firstDiffs j) i) (measure r)
                  remeasure (j + 1)
            remeasure 0
          _ -> mapM_ measure [0 .. n - 1]
        writePrimArray hdrWM i . (+ cellPadX) =<< lineWidthIO fm (hdrs V.! i <> tableSortReserve)
      cellWs <- unsafeFreezePrimArray cellWM
      hdrWs <- unsafeFreezePrimArray hdrWM
      let widest i !r !w = if r >= n then w else widest i (r + 1) (max w (indexPrimArray cellWs (i * n + r)))
          widths = generatePrimArray c $ \i ->
            let hdrW = indexPrimArray hdrWs i in if n == 0 then hdrW else max hdrW (widest i 0 minColW)
      -- The order stands while no row came or went and no sort key changed.
      let keysKept d =
            let key = sortKey (tdSort d)
                s = sortColIndex (tdSort d)
                keyChanged r f = s == f || (s > f && not (eqByPtr (key (indexSmallArray oldEnc r)) (key (indexSmallArray encoded r))))
             in n == oldN && findChanged keyChanged < 0
          (sort', order) = case old of
            Just d | keysKept d -> (tdSort d, tdOrder d)
            _ -> (sort, orderFor sort encoded)
      pure
        TableDerived
          { tdRows = Opaque rows
          , tdCols = Opaque cols
          , tdFont = Opaque fm
          , tdMonoFont = Opaque mono
          , tdRowObjs = objs
          , tdHeaders = hdrs
          , tdEncoded = encoded
          , tdCellW = cellWs
          , tdTextRow = textRows
          , tdWidths = widths
          , tdNumeric = smallArrayFromListN c (map isNum [0 .. c - 1])
          , tdSort = sort'
          , tdOrder = order
          }

-- | The sort after a click on column @clicked@: the same column flips its
-- direction, another sorts ascending.
nextSortCol :: SortCol -> Int -> SortCol
nextSortCol cur clicked
  | clicked == sortColIndex cur = SortCol clicked (if sortColDir cur == SortAsc then SortDesc else SortAsc)
  | otherwise = SortCol clicked SortAsc

-- | Local sort state and setter. Call in a stable hook position each frame.
useTableSort :: SortCol -> NanoUI (SortCol, SortCol -> NanoUI ())
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

-- Column metadata by source column index, or the fallback when out of range.
{-# INLINE primAt #-}
primAt :: Prim a => PrimArray a -> Int -> a -> a
primAt xs i fallback = if i >= 0 && i < sizeofPrimArray xs then indexPrimArray xs i else fallback

{-# INLINE smallAt #-}
smallAt :: SmallArray a -> Int -> a -> a
smallAt xs i fallback = if i >= 0 && i < sizeofSmallArray xs then indexSmallArray xs i else fallback

-- Minimum column width, even when dragged: the declared fixed width, else the
-- content minimum so cells never wrap.
colFloor :: SmallArray ColSize -> PrimArray Float -> Int -> Float
colFloor sizes contentWs i = case smallAt sizes i ColContent of
  ColFixed f -> max minColW f
  _ -> max minColW (primAt contentWs i minColW)

-- | A column with no gap and no padding.
flatLayout :: Layout
flatLayout = tight defaultLayout {layoutGap = 0}

-- | Column @i@'s box: its stored width (never under its floor) once it has
-- one, else a share of the spare width when the columns fill the table, else
-- its floor. Columns stretch to the row height so every cell's background
-- and borders span the full row even when one cell wraps.
colBoxLayout :: Bool -> Bool -> SmallArray ColSize -> PrimArray Float -> PrimArray Float -> Int -> Layout
colBoxLayout fillInner hasStretch sizes contentWs stored i
  | saved > 0 = fixed (max floorW saved)
  | ColStretch <- size, fillInner = box {layoutWidth = Grow 1}
  | ColContent <- size, fillInner && not hasStretch = box {layoutWidth = Grow 1}
  | otherwise = fixed floorW
  where
    saved = primAt stored i 0
    floorW = colFloor sizes contentWs i
    size = smallAt sizes i ColContent
    box = (fillH flatLayout) {layoutMinW = max floorW saved}
    fixed w = box {layoutWidth = Fixed w, layoutMaxW = w}

-- | Sortable table with resizable, reorderable columns. @key@ tells tables in
-- one scope apart, and the columns are a colonnade over @row@. Pass the
-- current sort; the 'TableResponse' carries the sort after this frame's
-- header clicks, along with the column order and hidden columns.
{-# INLINE table #-}
table :: Foldable f => Text -> Colonnade Headed row Text -> f row -> SortCol -> NanoUI TableResponse
table = tableConfigured defaultTableConfig id

-- | 'table' with a layout modifier.
{-# INLINE tableWith #-}
tableWith :: Foldable f => (Layout -> Layout) -> Text -> Colonnade Headed row Text -> f row -> SortCol -> NanoUI TableResponse
tableWith = tableConfigured defaultTableConfig

-- | A table of text rows under the given headers.
simpleTable :: Foldable f => [Text] -> f [Text] -> NanoUI TableResponse
simpleTable headers rows = do
  let cols = mconcat [headed h (\r -> smallAt r i "") | (i, h) <- zip [0 ..] headers]
      indexedRows = map smallArrayFromList (toList rows)
  table "simple" cols indexedRows (SortCol 0 SortAsc)

-- | 'tableWith' with column sizes, frozen rows and columns, and initially
-- hidden columns.
tableConfigured ::
  Foldable f =>
  TableConfig ->
  (Layout -> Layout) ->
  Text ->
  Colonnade Headed row Text ->
  f row ->
  SortCol ->
  NanoUI TableResponse
tableConfigured cfg f key cols inputRows curSort =
  withKey ("table:" <> key) $ do
    (stateWid, ctx) <- freshWidget
    vWid <- nextId
    hWid <- nextId
    tableWid <- nextId
    let n = V.length (Encode.getColonnade cols)
        sort0 = clampSortCol n curSort
        stateKey = intKey stateWid
    inp <- askInput
    st0 <- liftIO (getStore ctx)
    TableDerived {tdHeaders = hdrs, tdEncoded = encoded, tdWidths = contentWs, tdNumeric = numeric, tdOrder = sorted} <-
      liftIO (tableDerived ctx stateKey cols inputRows sort0)
    let sizes = smallArrayFromList (tableColSizes cfg)
        -- The column order and the widths columns were dragged to.
        (storedOrder, storedWidths) = fromMaybe ([0 .. n - 1], []) (lookupDyn stateKey st0)
        order0 = normalizeOrder n storedOrder
        hidden0 = findSlot fieldIntSet (tableHidden cfg) stateKey st0
        widths0 = take n (storedWidths ++ repeat 0 :: [Float])
        drag0 = unpackHeaderDrag (findSlot fieldInt 0 (slotKey SlotDrag stateKey) st0)
        dragX0 = findSlot fieldFloat 0 stateKey st0
        dragW0 = findSlot fieldFloat 0 (slotKey SlotDragW stateKey) st0
        mx = v2X (inputMousePos inp)
        widths1 = case drag0 of
          HeaderResize c
            | heldIn MouseLeft inp ->
                setAt c (max (colFloor sizes contentWs c) (dragW0 + mx - dragX0)) widths0
          _ -> widths0
    let outerLayout = f (fillW flatLayout)
        hasStretch = any (== ColStretch) (take n (tableColSizes cfg))
        vis = filter (`IS.notMember` hidden0) order0
        freezeN = clamp 0 (length vis) (tableFreezeCols cfg)
        frozenIdx = take freezeN vis
        unfrozenIdx = drop freezeN vis
        nRows = sizeofSmallArray encoded
        pinnedN = clamp 0 nRows (tableFreezeRows cfg)
        scrollN = nRows - pinnedN
        rowMinH = 28
        -- Columns fill the table width when one stretches or the table
        -- grows.
        fillInner =
          hasStretch || case layoutWidth outerLayout of
            Grow _ -> True
            _ -> False
        fillIf b = if b then fillW else id
        colBoxes =
          smallArrayFromList (map (colBoxLayout fillInner hasStretch sizes contentWs (primArrayFromList widths1)) [0 .. n - 1])
        colBox i = smallAt colBoxes i (tight defaultLayout)
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
        paneRoot = fillIf fillInner (tight (fillH defaultLayout))
        minSum idxs = sum (map (layoutMinW . colBox) idxs) + fromIntegral (max 0 (length idxs - 1))
        paneLay fill idxs
          | fill = fillW (fillH flatLayout)
          | otherwise = (fillH flatLayout) {layoutMinW = minSum idxs}
        gridRowLay idxs = fillIf fillInner flatLayout {layoutMinW = minSum idxs}
        -- Header row and pinned rows, each followed by a rule. Used by both panes.
        headerBlock idxs = do
          hs <- row' (gridRowLay idxs) $
            forM (zip [0 :: Int ..] idxs) $ \(k, i) -> do
              when (k > 0) $ void separator
              withKey i $
                column' (colBox i) $
                  buttonStyledEx True (tableHeaderLabel (smallAt numeric i False) (fromMaybe T.empty (hdrs V.!? i))) (if sortColIndex sort0 == i then 1 else 0) (cellLayout i) (sortMarkStyle sort0 i .|. buttonFlagTable)
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
              else liftIO $ do
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
              (fillH flatLayout)
              (if null unfrozenIdx then scrollVerticalAuto else scrollVerticalHidden)
              (bodyBlock frozenIdx)
            pure hs
        unfrozenPane = do
          mPrevV <- lastRect vWid
          let totalH = fromIntegral scrollN * rowMinH
              -- Uses last frame's rect, so the header's spacer lags the body's
              -- vertical bar by one frame when the bar appears or disappears.
              hasVertBar = maybe (totalH > 100) (\r -> totalH > rectH r) mPrevV
          column' (paneLay fillInner unfrozenIdx) $ do
            hs <-
              row' (fillIf fillInner flatLayout) $ do
                hs' <-
                  scrollAreaIdConfigured
                    hWid
                    ((if fillInner then fillW else minW (minSum unfrozenIdx)) flatLayout {layoutDirection = Row})
                    -- Bar-less scroller that follows the body's horizontal
                    -- offset (linkScrollAxes below) and clips the header row.
                    scrollHorizontalHidden
                    (column' (gridRowLay unfrozenIdx) (headerBlock unfrozenIdx))
                -- The body scroller has no padding, so its whole lane is gutter.
                when hasVertBar $ void (spacer (Fixed (scrollBarGutter ScrollBarList 0)) Fit)
                pure hs'
            liftIO (linkScrollAxes ctx vWid hWid)
            -- The body owns both scrollbars.
            scrollAreaIdConfigured
              vWid
              (fillIf fillInner (fillH flatLayout))
              defaultScrollConfig
              (bodyBlock unfrozenIdx)
            pure hs
    column' outerLayout $ do
      showAllResp <-
        if IS.null hidden0
          then pure Nothing
          else fmap Just $
            buttonStyledEx True "Show all columns" 0 (tight . fillW $ defaultLayout) 0
      headerPairs <-
        panel' paneRoot $ do
          tagContainer tableWid
          row' (paneRoot {layoutGap = 0}) $ do
            frozenHs <-
              if null frozenIdx
                then pure []
                else zip frozenIdx <$> frozenPane
            unless (null frozenIdx || null unfrozenIdx) $ void separator
            unfrozenHs <-
              if null unfrozenIdx then pure [] else zip unfrozenIdx <$> unfrozenPane
            pure (frozenHs ++ unfrozenHs)
      mBodyRect <- lastRect vWid
      let mouse = inputMousePos inp
          headerRects = [(i, rawRespRect r) | (i, r) <- headerPairs]
          -- Also used as the resize cursor's zones.
          edgeZones = headerEdgeZones 4 mBodyRect headerRects
          hitCol zones = fst <$> find (\(_, r) -> rectContains r mouse) zones
          edgeCol = hitCol edgeZones
          hoverCol = hitCol headerRects
          (isResize, isReorder) = case drag0 of
            HeaderResize _ -> (True, False)
            HeaderReorder _ -> (False, True)
            HeaderIdle -> (False, False)
          resizing = isResize && heldIn MouseLeft inp
      unless (null edgeZones) . liftIO $
        -- Strict in the spine and the rects, so no thunk waits in the IORef.
        modifyIORef' (ctxCursorZones ctx) (\zs -> foldl' (\acc (_, !r) -> (r, UiCursorEwResize) : acc) zs edgeZones)
      reorder <-
        withKey ("reorder" :: Text) $
          useReorder vis (if resizing || isJust edgeCol then [] else headerRects)
      let vis' = reorderOrder reorder
          mReorder = reorderDragging reorder
          dragged = isReorder && reorderMoved reorder
          pressResize = pressedIn MouseLeft inp && isJust edgeCol
          pressReorder = pressedIn MouseLeft inp && edgeCol == Nothing && isJust hoverCol
          nextDrag
            | pressResize = maybe HeaderIdle HeaderResize edgeCol
            | pressReorder = maybe HeaderIdle HeaderReorder hoverCol
            | releasedIn MouseLeft inp || not (heldIn MouseLeft inp) = HeaderIdle
            | otherwise = drag0
          nextDragX
            | pressResize || pressReorder = mx
            | nextDrag == HeaderIdle = 0
            | otherwise = dragX0
          nextDragW
            | pressResize = maybe 0 headerW edgeCol
            | nextDrag == HeaderIdle = 0
            | otherwise = dragW0
          headerW i = case lookup i headerRects of
            Just r | rectW r > 0 -> rectW r
            _ -> layoutMinW (colBox i)
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
            if dragged || isJust mReorder || vis' /= vis || isResize || (isJust edgeCol && (heldIn MouseLeft inp || releasedIn MouseLeft inp))
              then Nothing
              else listToMaybe [i | (i, r) <- headerPairs, respClicked r]
          nextSort = maybe sort0 (nextSortCol sort0) sortClick
          hasChanged = nextSort /= sort0 || nextOrder /= order0 || nextHidden /= hidden0 || widths1 /= widths0
          widgetResp =
            setChanged hasChanged $
              setClicked (hasChanged && isJust sortClick) (mconcat (map snd headerPairs ++ maybe [] pure showAllResp))
      -- Compare before writing, so an idle table writes nothing.
      liftIO . writeSlots ctx $
        SlotWrites (\st -> lookupDyn stateKey st == Just (nextOrder, widths1)) (insertDyn stateKey (nextOrder, widths1))
          <> slotWrite fieldIntSet stateKey nextHidden
          <> slotWrite fieldInt (slotKey SlotDrag stateKey) (packHeaderDrag nextDrag)
          <> slotWrite fieldFloat stateKey nextDragX
          <> slotWrite fieldFloat (slotKey SlotDragW stateKey) nextDragW
      -- Keep the resize cursor for the whole drag, wherever the pointer is.
      case nextDrag of
        HeaderResize _ | heldIn MouseLeft inp -> liftIO (modifyInteraction ctx (\s -> s {isColumnResize = True}))
        _ -> pure ()
      pure (TableResponse widgetResp nextSort nextOrder nextHidden)

-- | One row of cells with custom row layout.
gridColumnsLay :: Layout -> [Int] -> [Layout] -> [NanoUI ()] -> NanoUI ()
gridColumnsLay lay keys layouts cells =
  void (row' lay (go True keys layouts cells))
 where
  -- Walk in lockstep without allocating zip tuples and indices per cell.
  go first (key : moreKeys) (layout : moreLayouts) (cell : moreCells) = do
    unless first $ void separator
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
                      copy from to len = copyMutablePrimArray dst to src from len
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
                  -- Runs already in order, or wholly reversed, are copied
                  -- without a merge, so sorted input costs O(n) compares.
                  -- Reversed means every right key is strictly first, so
                  -- swapping the runs keeps the sort stable.
                  inOrder <- if mid >= hi then pure True else before <$> readPrimArray src (mid - 1) <*> readPrimArray src mid
                  merge <- if inOrder then pure False else before <$> readPrimArray src lo <*> readPrimArray src (hi - 1)
                  if inOrder
                    then copy lo lo (hi - lo)
                    else if merge then go lo mid lo else copy mid lo (hi - mid) >> copy lo (lo + hi - mid) (mid - lo)
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
setAt i x = zipWith (\j y -> if j == i then x else y) [0 ..]

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

-- | Each column's resize zone: @pad@ either side of its header's right edge,
-- from the top of the header band to the bottom of the body's last-frame
-- rect (or of the header band, before the body has a rect).
headerEdgeZones :: Float -> Maybe Rect -> [(Int, Rect)] -> [(Int, Rect)]
headerEdgeZones pad mBody hdrs =
  [(i, Rect (x + w - pad) top (2 * pad) (bot - top)) | (i, Rect x _ w h) <- hdrs, w > 0 && h > 0]
  where
    top = minimum [y | (_, Rect _ y _ _) <- hdrs]
    bot = maximum ([y + h | (_, Rect _ y _ h) <- hdrs] ++ [by + bh | Rect _ by _ bh <- toList mBody])
