-- | Text tables described by Colonnade columns, with sorting, frozen panes,
-- column resizing/reordering, and row virtualisation.
module NanoUI.Internal.Widgets.Table
  ( SortDir (..)
  , SortCol (..)
  , SortKey (..)
  , ColSize (..)
  , TableConfig (..)
  , TableResponse (..)
  , defaultTableConfig
  , table
  , tableWith
  , tableConfigured
  , table'
  , tableWith'
  , tableConfigured'
  , simpleTable
  , tableHiddenIndices
  , sortRows
  , Colonnade
  , Headed (..)
  , headed
  , headless
  , ToCell (..)
  , Cell (..)
  , textCell
  , cellWidget
  , widgetCell
  , cellSort
  , cellSortKey
  , cellSortNum
  , cellSortInt
  , withText
  )
where

import Colonnade (Colonnade, Headed (..), headed, headless)
import Colonnade.Encode qualified as Encode
import Control.Monad (forM, forM_, mfilter, unless, void, when)
import Control.Monad.Primitive (RealWorld)
import Control.Monad.ST (runST)
import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.IORef (modifyIORef')
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (find)
import Data.List qualified as L
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Data.Primitive.PrimArray (PrimArray, copyMutablePrimArray, copyPrimArray, generatePrimArray, indexPrimArray, newPrimArray, primArrayFromList, readPrimArray, sizeofPrimArray, unsafeFreezePrimArray, writePrimArray)
import Data.Primitive.SmallArray (SmallArray, SmallMutableArray, emptySmallArray, indexSmallArray, mapSmallArray', newSmallArray, sizeofSmallArray, smallArrayFromList, smallArrayFromListN, unsafeFreezeSmallArray, writeSmallArray)
import Data.Primitive.Types (Prim)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import NanoUI.Internal.Context (Context (..), InteractionState (..), ScrollMetrics (..), getPrevRect, readScrollMetrics, recordLayoutRead, getPrevClipRect, pointerCovered, isDisabled, getScrollOffset2D, getStore, intKey, linkScrollAxes, modifyInteraction, writeSlot, writeSlots)
import NanoUI.Internal.Var (Binding (..), bindVarOn, useVar)
import NanoUI.Internal.Var.Types (Var)
import NanoUI.Internal.Derived (TableDerived (..), Opaque (..), SortCol (..), SortDir (..), SortKey (..), tableCache)
import NanoUI.Internal.Font (FontMetrics (..), ScrollBarSlot (..), scrollBarGutter, tableCellInset, lineWidthIO)
import NanoUI.Internal.Input (Input (..), MouseButton (..), Pressable (..), UiCursorKind (..))
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (NanoUI, askInput, freshWidget, lastRect, nextId, liftIO, scope, withKey)
import NanoUI.Internal.Store (SlotSpec, SlotWrites, WidgetStore, encodedSpec, eqByPtr, ptrEq, fieldFloat, fieldInt, fieldIntSet, fieldTableColumns, findSpec, lookupSpec, ownSlotSpec, specKey, specWrite)
import NanoUI.Internal.Store.Slots (slotDragItem, slotDragStartW, slotScrollRange)
import NanoUI.Internal.Style (AlignX (..), AlignY (..), Direction (..), FontVariant (..), Layout (..), Sizing (..), defaultLayout, fillH, fillW, minW, tight)
import Data.Bits ((.|.))
import Data.String (IsString (..))
import NanoUI.Internal.Types (Rect (..), clamp, rectH, rectW, v2X, V2 (..), rectContains, noLimit)
import NanoUI.Internal.WidgetText (buttonFlagTable, rowStripe, stripeBits, tableHeaderPadX)
import qualified NanoUI.Internal.StyleBits as SB
import NanoUI.Internal.Widgets.Button (buttonContentStyled)
import NanoUI.Internal.Widgets.Behavior (Reorder (..), useReorder)
import NanoUI.Internal.Widgets.Combinators (buttonStyledEx, readDerived, writeDerived)
import NanoUI.Internal.Widgets.Layout (column', panel', row', scrollAreaIdConfigured, separator, spacer)
import NanoUI.Internal.Layout.ScrollGeometry (defaultScrollConfig, scrollHorizontalHidden, scrollVerticalAuto, scrollVerticalHidden)
import NanoUI.Internal.Widgets.Node

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
  -- ^ The columns hidden on the table's first frame. From then on the table
  -- keeps its own set, which a right-click on a header changes, and a later
  -- change here is ignored.
  , tableSqueeze :: !Bool
  -- ^ Squeeze instead of scroll: when the table is narrower than the sum of
  -- its pinned widths (fixed columns, content floors, dragged widths), the
  -- unfrozen columns shrink in proportion, none under 'minColW', rather than
  -- surfacing a horizontal scrollbar. A stretch column takes part with a
  -- bounded share, frozen columns keep their widths, and the vertical
  -- scrollbar's lane comes off the room first. Off by default: a table too
  -- wide for its pane keeps its sideways scroll.
  , tableBare :: !Bool
  -- ^ Omit the outer panel border and background well so a table embedded
  -- inside a pane blends directly onto the pane's surface (default 'False').
  }
  deriving (Eq, Show)

-- | No frozen or hidden rows/columns and content-sized columns.
defaultTableConfig :: TableConfig
defaultTableConfig = TableConfig 0 0 [] IS.empty False False

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

clampSortCol :: Int -> SortCol -> SortCol
clampSortCol n (SortCol idx dir) = SortCol (clamp 0 (max 0 (n - 1)) idx) dir

-- The sort mark of column @idx@ in its header's style ('SB.headerSortMark',
-- read back by 'NanoUI.Internal.WidgetText.tableSortMarkOf').
sortMarkStyle :: SortCol -> Int -> Word64
sortMarkStyle sort idx
  | sortColIndex sort /= idx = 0
  | sortColDir sort == SortDesc = SB.placeField SB.headerSortMark 2
  | otherwise = SB.placeField SB.headerSortMark 1

minColW :: Float
minColW = 40

-- | The table's column order and the widths columns were dragged to, under
-- the table's own key.
{-# INLINE tableColumnsSlot #-}
tableColumnsSlot :: SlotSpec ([Int], [Float])
tableColumnsSlot = ownSlotSpec fieldTableColumns

-- | The table's hidden columns, under its own key.
{-# INLINE tableHiddenSlot #-}
tableHiddenSlot :: SlotSpec IntSet
tableHiddenSlot = ownSlotSpec fieldIntSet

-- | The column a header-edge drag resizes, or -1. Stored plus one, so an
-- empty slot reads as idle.
{-# INLINE tableResizeColSlot #-}
tableResizeColSlot :: SlotSpec Int
tableResizeColSlot = encodedSpec (subtract 1) (+ 1) slotDragItem

-- | The pointer's x when a column resize started, under the table's own key.
{-# INLINE tableResizeXSlot #-}
tableResizeXSlot :: SlotSpec Float
tableResizeXSlot = ownSlotSpec fieldFloat

-- | The sort the headers show, as an int on the table's container: twice the
-- column, plus one when descending. The headers show the sort only through
-- their style, which no diff covers, so a change of this slot repaints the
-- table.
{-# INLINE sortShownCode #-}
sortShownCode :: SortCol -> Int
sortShownCode sort = 2 * sortColIndex sort + fromEnum (sortColDir sort == SortDesc)

-- | What one table cell (or header) shows.
data Cell = Cell
  { cellKey  :: !(Maybe SortKey)
  -- ^ Sort key; 'Nothing' = unsortable by this cell (unless 'cellText' is set).
  , cellText :: !Text
  -- ^ Measured for width; drawn when there is no widget view.
  , cellView :: Maybe (NanoUI ())
  -- ^ Replaces drawn text when present. Built only for visible rows.
  }

-- | Anything a 'Colonnade' column can encode to. Pure and user-implementable.
class ToCell c where
  toCell :: c -> Cell

instance ToCell Text where
  {-# INLINE toCell #-}
  toCell = textCell

instance ToCell Cell where
  {-# INLINE toCell #-}
  toCell = id

instance ToCell (NanoUI ()) where
  {-# INLINE toCell #-}
  toCell = cellWidget

instance IsString Cell where
  {-# INLINE fromString #-}
  fromString = textCell . T.pack

-- | Plain text cell (sortable by its text).
textCell :: Text -> Cell
textCell t = Cell (Just (SortText t)) t Nothing

-- | Arbitrary interactive widget cell (unsortable by default).
cellWidget :: NanoUI () -> Cell
cellWidget w = Cell Nothing T.empty (Just w)

-- | Encode a function @a -> NanoUI ()@ into a table cell.
widgetCell :: (a -> NanoUI ()) -> a -> Cell
widgetCell f = cellWidget . f

-- | Attach a text sort key to a cell. If the cell's 'cellText' is empty,
-- also populates 'cellText' with the key so column width can be estimated.
cellSort :: Text -> Cell -> Cell
cellSort k c = c { cellKey = Just (SortText k), cellText = if T.null (cellText c) then k else cellText c }

-- | Alias for 'cellSort'.
cellSortKey :: Text -> Cell -> Cell
cellSortKey = cellSort

-- | Attach a floating-point numeric sort key ('SortNum') to a cell.
cellSortNum :: RealFloat a => a -> Cell -> Cell
cellSortNum x c =
  let !d
        | isNaN x = 0 / 0
        | isInfinite x = if x < 0 then -1 / 0 else 1 / 0
        | otherwise = realToFrac x
   in c { cellKey = Just (SortNum d) }

-- | Attach a 64-bit integer sort key ('SortInt') to a cell.
cellSortInt :: Integral a => a -> Cell -> Cell
cellSortInt n c = c { cellKey = Just (SortInt (fromIntegral n)) }

-- | Attach an explicit text label for column width estimation.
withText :: Text -> Cell -> Cell
withText t c = c { cellText = t }

-- | Stable sort by the selected column's sort key (or text).
-- Out-of-range or unsortable column indices retain input order: an
-- unsortable column's keys are all empty, so the stable sort keeps it.
sortRows :: (Foldable f, ToCell c) => Colonnade Headed row c -> SortCol -> f row -> [row]
sortRows cols sort inputRows =
  let rows = toList inputRows
      n = V.length (Encode.getColonnade cols)
      idx = sortColIndex (clampSortCol n sort)
   in case Encode.getColonnade cols V.!? idx of
        Nothing -> rows
        Just col ->
          let key r = let c = toCell (Encode.oneColonnadeEncode col r) in fromMaybe (SortText (cellText c)) (cellKey c)
           in case sortColDir sort of
                SortAsc -> L.sortOn key rows
                SortDesc -> L.sortOn (Down . key) rows

isNumericCell :: Text -> Bool
isNumericCell txt =
  let s = T.strip txt
      digits = case T.uncons s of
        Just (c, rest) | c == '-' || c == '+' -> rest
        _ -> s
   in not (T.null digits) && T.all isDigit digits

samePtr :: Opaque -> b -> Bool
samePtr (Opaque a) = ptrEq a

-- | The table's derived data for @rows@ under @sort@: the cached data when
-- the rows and columns are the ones it was derived from, else derived again
-- from what it holds. The rows and columns are evaluated first, so the
-- pointers kept and compared are their values, whether or not the code that
-- derives the data evaluates them before keeping them.
tableDerived :: (Foldable f, ToCell c) => Context -> Int -> Colonnade Headed row c -> f row -> SortCol -> IO TableDerived
tableDerived ctx key !cols !rows sort = do
  cached <- readDerived tableCache ctx key
  let encoders = Encode.getColonnade cols
      hdrs = V.map (cellText . toCell . getHeaded . Encode.oneColonnadeHead) encoders
  derived <- case cached of
    Just d | samePtr (tdRows d) rows && samePtr (tdCols d) cols -> pure d
    _ -> rederive ctx cols rows sort hdrs (mfilter ((== hdrs) . tdHeaders) cached)
  let !resorted
        | tdSort derived == sort = derived
        | otherwise = derived {tdSort = sort, tdOrder = orderFor sort (tdKeys derived)}
  case cached of
    Just d | samePtr (Opaque d) resorted -> pure ()
    _ -> writeDerived tableCache ctx key resorted
  pure resorted

-- | Row indices sorted by the sort column's key or text. An unsortable
-- column's keys are all empty, so the stable sort keeps the input order.
orderFor :: SortCol -> SmallArray (V.Vector SortKey) -> PrimArray Int
orderFor sort keys = sortIndices (sortColDir sort) (mapSmallArray' (sortKey sort) keys)

sortKey :: SortCol -> V.Vector SortKey -> SortKey
sortKey sort row = fromMaybe (SortText T.empty) (row V.!? sortColIndex sort)

sameSortKey :: SortKey -> SortKey -> Bool
sameSortKey a b = case (a, b) of
  (SortText x, SortText y) -> eqByPtr x y || x == y
  (SortNum x, SortNum y) -> (isNaN x && isNaN y) || x == y
  (SortInt x, SortInt y) -> x == y
  _ -> False

-- | A row's cells, each evaluated, and their sort keys. Marks in @sortable@
-- each column with a key or text, and in @views@ each column with a widget
-- view.
encodeRow :: ToCell c => SmallMutableArray RealWorld Bool -> SmallMutableArray RealWorld Bool -> V.Vector (Encode.OneColonnade Headed row c) -> row -> IO (V.Vector Text, V.Vector SortKey)
encodeRow sortable views encoders r = do
  let k = V.length encoders
  txts <- MV.unsafeNew k
  keys <- MV.unsafeNew k
  let fill !j
        | j >= k = pure ()
        | otherwise = do
            let !c = toCell (Encode.oneColonnadeEncode (V.unsafeIndex encoders j) r)
                !t = cellText c
                !ky = fromMaybe (SortText t) (cellKey c)
            MV.unsafeWrite txts j t
            MV.unsafeWrite keys j ky
            when (isJust (cellKey c) || not (T.null t)) (writeSmallArray sortable j True)
            when (isJust (cellView c)) (writeSmallArray views j True)
            fill (j + 1)
  fill 0
  !vTxts <- V.unsafeFreeze txts
  !vKeys <- V.unsafeFreeze keys
  pure (vTxts, vKeys)

-- | The first cell at which two rows differ, or -1 when they are the same.
firstDiff :: V.Vector Text -> V.Vector Text -> Int
firstDiff a b = go 0
  where
    k = min (V.length a) (V.length b)
    go !j
      | j >= k = if V.length a == V.length b then -1 else j
      | eqByPtr (V.unsafeIndex a j) (V.unsafeIndex b j) = go (j + 1)
      | otherwise = j

firstKeyDiff :: V.Vector SortKey -> V.Vector SortKey -> Int
firstKeyDiff a b = go 0
  where
    k = min (V.length a) (V.length b)
    go !j
      | j >= k = if V.length a == V.length b then -1 else j
      | sameSortKey (V.unsafeIndex a j) (V.unsafeIndex b j) = go (j + 1)
      | otherwise = j

-- | Derive from @rows@, reusing @old@ (derived under the same headers):
-- each row that is the same object under the same columns keeps its text,
-- each cell whose text is unchanged keeps its width, and the order stands
-- while no sort key changed. Widths measured with other fonts are kept only
-- while no text changed.
rederive :: (Foldable f, ToCell c) => Context -> Colonnade Headed row c -> f row -> SortCol -> V.Vector Text -> Maybe TableDerived -> IO TableDerived
rederive Context {ctxFontMetrics = fm, ctxMonoFontMetrics = mono} cols rows sort hdrs old = do
  -- Bound evaluated, so the loops below do not enter them again.
  let !n = length rows
      !c = V.length hdrs
      !oldN = maybe 0 (sizeofSmallArray . tdEncoded) old
      !oldObjs = maybe emptySmallArray tdRowObjs old
      !oldEnc = maybe emptySmallArray tdEncoded old
      !oldKeys = maybe emptySmallArray tdKeys old
      !sameCols = any (\d -> samePtr (tdCols d) cols) old
      !encoders = Encode.getColonnade cols
  objsM <- newSmallArray n (Opaque ())
  encM <- newSmallArray n V.empty
  keysM <- newSmallArray n V.empty
  changedM <- newPrimArray n
  diffM <- newPrimArray n
  countM <- newPrimArray 1
  sortableM <- newSmallArray c False
  viewsM <- newSmallArray c False
  case old of
    Just d -> forM_ [0 .. c - 1] $ \j -> do
      writeSmallArray sortableM j (smallAt (tdSortable d) j False)
      writeSmallArray viewsM j (smallAt (tdViews d) j False)
    Nothing -> pure ()
  let store i o e k = writeSmallArray objsM i o >> writeSmallArray encM i e >> writeSmallArray keysM i k
      encodeNew = encodeRow sortableM viewsM encoders
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
                !ky = indexSmallArray oldKeys i
            if sameCols && samePtr o r
              then store i o e ky >> walk (i + 1) k rs
              else do
                (!e', !ky') <- encodeNew r
                let !d = firstDiff e' e
                    !kd = firstKeyDiff ky' ky
                    !diffCol = case (d < 0, kd < 0) of
                      (True, True) -> -1
                      (False, True) -> d
                      (True, False) -> kd
                      (False, False) -> min d kd
                if diffCol < 0
                  then store i (Opaque r) e ky >> walk (i + 1) k rs
                  else store i (Opaque r) (if d < 0 then e else e') ky' >> note k i diffCol >> walk (i + 1) (k + 1) rs
        | otherwise = do
            (!e', !ky') <- encodeNew r
            store i (Opaque r) e' ky' >> note k i 0 >> walk (i + 1) (k + 1) rs
  walk (0 :: Int) (0 :: Int) (toList rows)
  nChanged <- readPrimArray countM 0
  objs <- unsafeFreezeSmallArray objsM
  sortable <- unsafeFreezeSmallArray sortableM
  views <- unsafeFreezeSmallArray viewsM
  case old of
    Just d | n == oldN && nChanged == 0 -> pure d {tdRows = Opaque rows, tdCols = Opaque cols, tdRowObjs = objs, tdSortable = sortable, tdViews = views}
    _ -> do
      encoded <- unsafeFreezeSmallArray encM
      keys <- unsafeFreezeSmallArray keysM
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
        -- As the header shows it, with the sort mark's lane beside it.
        writePrimArray hdrWM i . (+ tableHeaderPadX) =<< lineWidthIO fm (hdrs V.! i)
      cellWs <- unsafeFreezePrimArray cellWM
      hdrWs <- unsafeFreezePrimArray hdrWM
      let widest i !r !w = if r >= n then w else widest i (r + 1) (max w (indexPrimArray cellWs (i * n + r)))
          widths = generatePrimArray c $ \i ->
            let hdrW = indexPrimArray hdrWs i in if n == 0 then hdrW else max hdrW (widest i 0 minColW)
      -- The order stands while no row came or went and no sort key changed.
      let keysKept d =
            let key = sortKey (tdSort d)
                s = sortColIndex (tdSort d)
                keyChanged r f = s == f || (s > f && not (sameSortKey (key (indexSmallArray oldKeys r)) (key (indexSmallArray keys r))))
             in n == oldN && findChanged keyChanged < 0
          (sort', order) = case old of
            Just d | keysKept d -> (tdSort d, tdOrder d)
            _ -> (sort, orderFor sort keys)
      pure
        TableDerived
          { tdRows = Opaque rows
          , tdCols = Opaque cols
          , tdFont = Opaque fm
          , tdMonoFont = Opaque mono
          , tdRowObjs = objs
          , tdHeaders = hdrs
          , tdEncoded = encoded
          , tdKeys = keys
          , tdSortable = sortable
          , tdViews = views
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

-- Column metadata by source column index, or the fallback when out of range.
{-# INLINE primAt #-}
primAt :: Prim a => PrimArray a -> Int -> a -> a
primAt xs i fallback = if i >= 0 && i < sizeofPrimArray xs then indexPrimArray xs i else fallback

{-# INLINE smallAt #-}
smallAt :: SmallArray a -> Int -> a -> a
smallAt xs i fallback = if i >= 0 && i < sizeofSmallArray xs then indexSmallArray xs i else fallback

-- Initial column width before any user drag: the declared fixed width, else the
-- content minimum so undragged cells do not wrap. Once dragged, a column can
-- shrink down to 'minColW'.
colFloor :: SmallArray ColSize -> PrimArray Float -> Int -> Float
colFloor sizes contentWs i = case smallAt sizes i ColContent of
  ColFixed f -> max minColW f
  ColStretch -> minColW
  ColContent -> max minColW (primAt contentWs i minColW)

flatLayout :: Layout
flatLayout = tight defaultLayout {layoutGap = 0}

tableRowMinH :: Float
tableRowMinH = 28

textCellLayout, numCellLayout, numHeaderLayout :: Layout
textCellLayout =
  (tight defaultLayout)
    { layoutWidth = Grow 1
    , layoutHeight = Grow 1
    , layoutAlignX = AlignStart
    , layoutAlignY = AlignMiddle
    , layoutMinH = tableRowMinH
    , layoutFontVariant = FontRegular
    }
numCellLayout =
  textCellLayout
    { layoutAlignX = AlignEnd
    , layoutFontVariant = FontMono
    }
numHeaderLayout =
  numCellLayout
    { layoutFontVariant = FontRegular
    }

-- | Column @i@'s box: its stored width (never under 'minColW') once it has
-- one, else a share of the spare width when the columns fill the table, else
-- its initial floor. Columns stretch to the row height so every cell's
-- background and borders span the full row even when one cell wraps.
--
-- A stretch column is the table's slack: its minimum is an explicit zero,
-- so it compresses (under a vertical scrollbar's lane, say, which narrows
-- the viewport by the lane's width) rather than reporting its content as a
-- minimum that surfaces a horizontal bar under a table that fits the
-- scroller's full width.
--
-- Under the squeeze ('tableSqueeze') the box instead grows by its pinned
-- width (its dragged width once it has one, else its floor) up to that
-- width, so a table narrower than their sum shrinks the columns in
-- proportion to it, none under 'minColW', and scrolls sideways only once
-- every column sits at that floor. A stretch column takes part with a
-- bounded share, which keeps fixed columns from crushing it to nothing, and
-- has no cap, so it takes what the others leave; so do the content columns
-- of a table that fills its width with no stretch column.
colBoxLayout :: Bool -> Bool -> Bool -> SmallArray ColSize -> PrimArray Float -> PrimArray Float -> Int -> Layout
colBoxLayout squeeze fillInner hasStretch sizes contentWs stored i
  | squeeze = box {layoutWidth = Grow pinned, layoutMinW = minColW, layoutMaxW = if grows then noLimit else pinned}
  | saved > 0 = fixed (max minColW saved)
  | grows, ColStretch <- size = minW 0 box {layoutWidth = Grow 1}
  | grows = box {layoutWidth = Grow 1}
  | otherwise = fixed floorW
  where
    saved = primAt stored i 0
    floorW = colFloor sizes contentWs i
    size = smallAt sizes i ColContent
    -- Whether the column takes the spare width once it has no stored width.
    grows = saved <= 0 && fillInner && (size == ColStretch || size == ColContent && not hasStretch)
    pinned
      | saved > 0 = max minColW saved
      | ColStretch <- size = max minColW (min 120 (primAt contentWs i minColW))
      | otherwise = floorW
    box = (fillH flatLayout) {layoutMinW = floorW}
    fixed w = box {layoutWidth = Fixed w, layoutMinW = w, layoutMaxW = w}

-- | Sortable table with resizable, reorderable columns, with sort state bound
-- to a 'Var': a header click updates it in place. @key@ distinguishes tables
-- in one scope, and the columns are a colonnade over @row@. A right-click on
-- a header hides its column while another column stays in view, and a "Show
-- all columns" button above the table then brings them all back.
--
-- > table "files" cols files app.sort
{-# INLINE table #-}
table :: (Foldable f, ToCell c) => Text -> Colonnade Headed row c -> f row -> Var SortCol -> NanoUI ()
table = tableConfigured defaultTableConfig id

-- | 'table' returning the 'TableResponse': the column order and hidden
-- columns along with the response.
{-# INLINE table' #-}
table' :: (Foldable f, ToCell c) => Text -> Colonnade Headed row c -> f row -> Var SortCol -> NanoUI TableResponse
table' = tableConfigured' defaultTableConfig id

-- | 'table' with a layout modifier.
{-# INLINE tableWith #-}
tableWith :: (Foldable f, ToCell c) => (Layout -> Layout) -> Text -> Colonnade Headed row c -> f row -> Var SortCol -> NanoUI ()
tableWith = tableConfigured defaultTableConfig

-- | 'tableWith' returning the 'TableResponse'.
{-# INLINE tableWith' #-}
tableWith' :: (Foldable f, ToCell c) => (Layout -> Layout) -> Text -> Colonnade Headed row c -> f row -> Var SortCol -> NanoUI TableResponse
tableWith' = tableConfigured' defaultTableConfig

-- | A table of text rows under the given headers, sorted by the first column
-- until a header click sorts by another. The sort is kept in a positional
-- 'useVar'. Its columns are built again on every pass, so every row is read
-- again too; for many rows, use 'table' with columns kept outside the view.
simpleTable :: Foldable f => [Text] -> f [Text] -> NanoUI TableResponse
simpleTable headers rows = do
  sortVar <- useVar (SortCol 0 SortAsc)
  -- The cells are read from the rows themselves, not from a copy of each.
  let cols = mconcat [headed h (fromMaybe "" . listToMaybe . drop i) | (i, h) <- zip [0 ..] headers]
  -- A positional scope, like the sort, so sibling simple tables keep apart.
  scope (tableConfigured' defaultTableConfig id "simple" cols rows sortVar)


-- | 'tableWith' with column sizes, frozen rows and columns, and initially
-- hidden columns.
{-# INLINE tableConfigured #-}
tableConfigured ::
  (Foldable f, ToCell c) =>
  TableConfig ->
  (Layout -> Layout) ->
  Text ->
  Colonnade Headed row c ->
  f row ->
  Var SortCol ->
  NanoUI ()
tableConfigured cfg f key cols rows v = void (tableConfigured' cfg f key cols rows v)

-- | 'tableConfigured' returning the 'TableResponse'. The rows are ordered
-- before a header click is read, so a click that changes the sort runs the
-- view again to show them in the new order.
tableConfigured' ::
  (Foldable f, ToCell c) =>
  TableConfig ->
  (Layout -> Layout) ->
  Text ->
  Colonnade Headed row c ->
  f row ->
  Var SortCol ->
  NanoUI TableResponse
tableConfigured' cfg f key cols rows v =
  bindVarOn Conservative tableSort v (tableCore cfg f key cols rows)

-- | A table's state between frames, as 'readTableState' reads it from the
-- store and 'writeTableState' writes it back.
data TableState = TableState
  { tsOrder :: ![Int]
  -- ^ Every source column in display order, hidden ones included.
  , tsWidths :: ![Float]
  -- ^ The width each source column was dragged to, 0 for none.
  , tsHidden :: !IntSet
  , tsResizeCol :: !Int
  -- ^ The column a header-edge drag resizes, or -1.
  , tsResizeX :: !Float
  -- ^ The pointer's x when that resize started.
  , tsResizeW :: !Float
  -- ^ The column's width when that resize started.
  }

-- | The state of a table of @n@ columns under @stateKey@ in @st@.
readTableState :: TableConfig -> Int -> Int -> WidgetStore -> TableState
readTableState cfg n stateKey st =
  let (storedOrder, storedWidths) = fromMaybe ([0 .. n - 1], []) (lookupSpec tableColumnsSlot stateKey st)
   in TableState
        { tsOrder = normalizeOrder n storedOrder
        , tsWidths = take n (storedWidths ++ repeat 0)
        , tsHidden = findSpec tableHiddenSlot (tableHidden cfg) stateKey st
        , tsResizeCol = findSpec tableResizeColSlot (-1) stateKey st
        , tsResizeX = findSpec tableResizeXSlot 0 stateKey st
        , tsResizeW = findSpec slotDragStartW 0 stateKey st
        }

-- | Writes of @ts@ under @stateKey@. Each compares before it writes, so an
-- idle table writes nothing.
writeTableState :: Int -> TableState -> SlotWrites
writeTableState stateKey ts =
  specWrite tableColumnsSlot stateKey (tsOrder ts, tsWidths ts)
    <> specWrite tableHiddenSlot stateKey (tsHidden ts)
    <> specWrite tableResizeColSlot stateKey (tsResizeCol ts)
    <> specWrite tableResizeXSlot stateKey (tsResizeX ts)
    <> specWrite slotDragStartW stateKey (tsResizeW ts)

-- | The column widths this frame lays out: @ts@'s, with the column a held
-- resize drags set to its start width plus the pointer's travel, never under
-- 'minColW'.
resizedWidths :: Input -> TableState -> [Float]
resizedWidths inp ts
  | col >= 0 && heldIn MouseLeft inp =
      setAt col (max minColW (tsResizeW ts + v2X (inputMousePos inp) - tsResizeX ts)) (tsWidths ts)
  | otherwise = tsWidths ts
  where
    col = tsResizeCol ts

-- | Where a table's columns go this frame: which show, in which pane, and
-- each one's box.
data TableGeometry = TableGeometry
  { tgOuter :: !Layout
  -- ^ The layout of the column holding the table.
  , tgVisible :: ![Int]
  -- ^ The shown columns in display order.
  , tgFrozen :: ![Int]
  -- ^ The shown columns of the frozen pane.
  , tgUnfrozen :: ![Int]
  -- ^ The shown columns of the scrolling pane.
  , tgPinned :: !Int
  -- ^ Rows pinned under the header.
  , tgScrolled :: !Int
  -- ^ Rows in the body below them.
  , tgFill :: !Bool
  -- ^ The columns fill the table's width: one stretches or the table grows.
  , tgRowH :: !Float
  , tgColBoxes :: !(SmallArray Layout)
  -- ^ Each source column's box ('colBoxLayout').
  }

-- | The geometry of a table of @n@ columns and @nRows@ rows under config
-- @cfg@ and layout modifier @f@, its columns sized @widths@ this frame.
tableGeometry :: TableConfig -> (Layout -> Layout) -> FontMetrics -> Int -> Int -> SmallArray ColSize -> PrimArray Float -> [Float] -> TableState -> TableGeometry
tableGeometry cfg f fm n nRows sizes contentWs widths ts =
  TableGeometry
    { tgOuter = outerLayout
    , tgVisible = vis
    , tgFrozen = frozenIdx
    , tgUnfrozen = drop freezeN vis
    , tgPinned = pinnedN
    , tgScrolled = nRows - pinnedN
    , tgFill = fillInner
    , tgRowH = max tableRowMinH (fmLineHeight fm + 6)
    , tgColBoxes =
        smallArrayFromList
          [ colBoxLayout (squeezed i) fillInner hasStretch sizes contentWs widthsArr i
          | i <- [0 .. n - 1]
          ]
    }
  where
    widthsArr = primArrayFromList widths
    outerLayout = f (fillW flatLayout)
    hidden = tsHidden ts
    vis = filter (`IS.notMember` hidden) (tsOrder ts)
    hasStretch = any (\i -> smallAt sizes i ColContent == ColStretch) vis
    freezeN = clamp 0 (length vis) (tableFreezeCols cfg)
    frozenIdx = take freezeN vis
    pinnedN = clamp 0 nRows (tableFreezeRows cfg)
    fillInner =
      hasStretch || case layoutWidth outerLayout of
        Grow _ -> True
        _ -> False
    -- Under the squeeze the unfrozen columns shrink to the body in the same
    -- frame; frozen columns keep their widths.
    squeezed i = tableSqueeze cfg && i `notElem` frozenIdx

-- | The widgets a table's panes are made of: the body scroller, the bar-less
-- header scroller that follows it, and the panel holding both panes.
data TableIds = TableIds
  { tidBody :: !WidgetId
  , tidHeader :: !WidgetId
  , tidPanel :: !WidgetId
  }

-- | A table's panel: the frozen pane and the scrolling pane, each a header
-- row, the pinned rows and the body's rows in view. Returns each shown
-- column's header response.
tablePanes ::
  ToCell c =>
  Bool ->
  Context ->
  TableIds ->
  V.Vector (Encode.OneColonnade Headed row c) ->
  V.Vector row ->
  TableDerived ->
  TableGeometry ->
  SortCol ->
  Bool ->
  NanoUI [(Int, Response)]
tablePanes
  bare
  ctx
  ids
  encoders
  rowsVec
  TableDerived {tdEncoded = encoded, tdSortable = sortable, tdViews = views, tdNumeric = numeric, tdOrder = sorted}
  TableGeometry {tgFrozen = frozenIdx, tgUnfrozen = unfrozenIdx, tgPinned = pinnedN, tgScrolled = scrollN, tgFill = fillInner, tgRowH = rowH, tgColBoxes = colBoxes}
  sort0
  hasVertBar =
  (if bare then container NodeContainer paneRoot else panel' paneRoot) $ do
    tagContainer (tidPanel ids)
    row' (paneRoot {layoutGap = 0}) $ do
      frozenHs <-
        if null frozenIdx
          then pure []
          else zip frozenIdx <$> frozenPane
      unless (null frozenIdx || null unfrozenIdx) $ void separator
      unfrozenHs <-
        if null unfrozenIdx then pure [] else zip unfrozenIdx <$> unfrozenPane
      pure (frozenHs ++ unfrozenHs)
  where
    vWid = tidBody ids
    hWid = tidHeader ids
    rowStep = rowH + 1
    fillIf b = if b then fillW else id
    colBox i = smallAt colBoxes i (tight defaultLayout)
    cellLayout i = if smallAt numeric i False then numCellLayout else textCellLayout
    isSortable i = smallAt sortable i False
    renderHeader i active cellLay flags = do
      let oneCol = encoders V.! i
          hdrCell = toCell (getHeaded (Encode.oneColonnadeHead oneCol))
          styleIx = if active then 1 else 0
      case cellView hdrCell of
        Nothing ->
          buttonStyledEx True (cellText hdrCell) styleIx cellLay flags
        Just hdrAct -> buttonContentStyled styleIx flags cellLay hdrAct
    textCellAt ri i t = do
      wid <- nextId
      void (addWidgetStyled wid NodeText t 0 (cellLayout i) (fromIntegral (rowStripe ri)))
    -- Text cells show their cached text; only a column with widget views
    -- encodes its visible cells again, for the views.
    renderCell ri r i
      | not (smallAt views i False) = textCellAt ri i (indexSmallArray encoded r V.! i)
      | otherwise = case cellView (toCell (Encode.oneColonnadeEncode (V.unsafeIndex encoders i) (rowsVec V.! r))) of
        Nothing -> textCellAt ri i (indexSmallArray encoded r V.! i)
        -- The cell's panel carries the row fill in its style index
        -- ('panelSurface'), as a text cell's node does.
        Just widgetAct -> styledContainer NodePanel (stripeBits (rowStripe ri)) (cellLayout i) widgetAct
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
      let !rowLay = gridRowLay idxs
          !colLays = map colBox idxs
      hs <- row' rowLay $
        forM (zip [0 :: Int ..] idxs) $ \(k, i) -> do
          when (k > 0) $ void separator
          let isSort = isSortable i
              isActive = sortColIndex sort0 == i && isSort
              flags = (if isSort then sortMarkStyle sort0 i else 0) .|. buttonFlagTable
              hdrLay = if smallAt numeric i False then numHeaderLayout else textCellLayout
          withKey i $
            column' (colBox i) $
              renderHeader i isActive hdrLay flags
      void separator
      forM_ [0 .. pinnedN - 1] $ \ri -> do
        let !sourceIdx = indexPrimArray sorted ri
        withKey ("pin" :: Text, sourceIdx) $ do
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
            viewH <- maybe (rowStep * 8) rectH <$> getPrevRect ctx vWid
            pure (listClipper scrollN scrollY viewH rowStep)
      let !rowLay = gridRowLay idxs
          !colLays = map colBox idxs
          topH = if lo <= 0 then 0 else fromIntegral lo * rowH + fromIntegral (lo - 1)
          botH = fromIntegral (max 0 (scrollN - hi - 1)) * rowStep
      column' rowLay $ do
        when (topH > 0) $ void (spacer Fit (Fixed topH))
        forM_ [lo .. hi] $ \rowIdx -> do
          let !ri = rowIdx + pinnedN
              !sourceIdx = indexPrimArray sorted ri
          withKey sourceIdx $ do
            when (rowIdx > 0) $ void separator
            rowCells rowLay idxs colLays ri
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
      -- Uses last frame's rect, so the header's spacer lags the body's
      -- vertical bar by one frame when the bar appears or disappears.
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

-- | What one frame of a table's headers did, for 'resolveTableInput'.
data TableInput = TableInput
  { tiInput :: !Input
  , tiSort :: !SortCol
  -- ^ The sort the rows are in.
  , tiSortable :: !(SmallArray Bool)
  -- ^ Whether each source column can sort.
  , tiVisible :: ![Int]
  -- ^ The shown columns in display order, as laid out.
  , tiWidths :: ![Float]
  -- ^ The column widths laid out ('resizedWidths').
  , tiSizes :: !(SmallArray ColSize)
  -- ^ Declared column sizing policies.
  , tiContentWs :: !(PrimArray Float)
  -- ^ Measured content width of each source column.
  , tiFill :: !Bool
  -- ^ Whether columns fill the table width.
  , tiHeaders :: ![(Int, Response)]
  -- ^ Each shown column's header response.
  , tiShowAll :: !(Maybe Response)
  -- ^ The "Show all columns" button, while a column is hidden.
  , tiEdge :: !(Maybe (Int, Float))
  -- ^ The column whose resize edge is under the pointer, and its header's
  -- width.
  , tiReorder :: !Reorder
  }

-- | A table's state after one frame, from 'resolveTableInput'.
data TableStep = TableStep
  { tableStepState :: !TableState
  , tableStepSort :: !SortCol
  , tableStepSortClicked :: !Bool
  -- ^ A header click changed the sort.
  , tableStepChanged :: !Bool
  -- ^ The sort, order, hidden columns or widths changed.
  }

-- | One frame of a table's headers: a resize starting, running, auto-fitting
-- on double-click, or ending, a column reordered, hidden or shown again, and
-- a header click sorting.
resolveTableInput :: TableInput -> TableState -> TableStep
resolveTableInput ti ts0 =
  TableStep
    { tableStepState =
        TableState
          { tsOrder = nextOrder
          , tsWidths = nextWidths
          , tsHidden = nextHidden
          , tsResizeCol = nextDrag
          , tsResizeX = nextDragX
          , tsResizeW = nextDragW
          }
    , tableStepSort = nextSort
    , tableStepSortClicked = hasChanged && isJust sortClick
    , tableStepChanged = hasChanged
    }
  where
    inp = tiInput ti
    sort0 = tiSort ti
    vis = tiVisible ti
    order0 = tsOrder ts0
    hidden0 = tsHidden ts0
    headerPairs = tiHeaders ti
    reorder = tiReorder ti
    edgeCol = fst <$> tiEdge ti
    isResize = tsResizeCol ts0 >= 0
    doubleClick = pressedIn MouseLeft inp && inputMouseClicks inp >= 2
    autoFit col =
      let fitW = max minColW (primAt (tiContentWs ti) col minColW)
          curSaved = fromMaybe 0 (listToMaybe (drop col (tiWidths ti)))
          size = smallAt (tiSizes ti) col ColContent
       in if curSaved > 0 && abs (curSaved - fitW) <= 1
            then 0
            else if size == ColContent && not (tiFill ti)
              then 0
              else fitW
    nextWidths = case tiEdge ti of
      Just (col, _) | doubleClick -> setAt col (autoFit col) (tiWidths ti)
      _ -> tiWidths ti
    vis' = reorderOrder reorder
    mReorder = reorderDragging reorder
    (nextDrag, nextDragX, nextDragW)
      | doubleClick, isJust (tiEdge ti) = (-1, 0, 0)
      | pressedIn MouseLeft inp, Just (col, w) <- tiEdge ti = (col, v2X (inputMousePos inp), w)
      | pressedIn MouseLeft inp || releasedIn MouseLeft inp || not (heldIn MouseLeft inp) || not isResize = (-1, 0, 0)
      | otherwise = (tsResizeCol ts0, tsResizeX ts0, tsResizeW ts0)
    nextOrder = if vis' /= vis then rebuildOrder hidden0 vis' order0 else order0
    -- respRightClicked, not a bare release: a right press that went down
    -- elsewhere and came up over a header must not hide that column.
    hideClicked = [i | (i, r) <- headerPairs, respRightClicked r, not isResize, isNothing mReorder]
    nextHidden = case tiShowAll ti of
      Just r | respClicked r -> IS.empty
      _ -> case hideClicked of
        (i : _) | length vis > 1 -> IS.insert i hidden0
        _ -> hidden0
    sortClick =
      if reorderMoved reorder || isJust mReorder || isResize || (isJust edgeCol && (heldIn MouseLeft inp || releasedIn MouseLeft inp))
        then Nothing
        else listToMaybe [i | (i, r) <- headerPairs, respClicked r, smallAt (tiSortable ti) i False]
    nextSort = maybe sort0 (nextSortCol sort0) sortClick
    hasChanged = nextSort /= sort0 || nextOrder /= order0 || nextHidden /= hidden0 || nextWidths /= tsWidths ts0

-- | 'tableConfigured' on a plain sort.
tableCore ::
  (Foldable f, ToCell c) =>
  TableConfig ->
  (Layout -> Layout) ->
  Text ->
  Colonnade Headed row c ->
  f row ->
  SortCol ->
  NanoUI TableResponse
tableCore cfg f key cols inputRows curSort =
  let encoders = Encode.getColonnade cols
      n = V.length encoders
      -- Built only for a column with widget views, which reads its rows.
      rowsVec = V.fromList (toList inputRows)
  in withKey ("table:" <> key) $ do
    (stateWid, ctx) <- freshWidget
    vWid <- nextId
    hWid <- nextId
    tableWid <- nextId
    let sort0 = clampSortCol n curSort
        stateKey = intKey stateWid
        ids = TableIds {tidBody = vWid, tidHeader = hWid, tidPanel = tableWid}
    inp <- askInput
    !ts0 <- readTableState cfg n stateKey <$> liftIO (getStore ctx)
    derived <- liftIO (tableDerived ctx stateKey cols inputRows sort0)
    let sizes = smallArrayFromList (tableColSizes cfg)
        contentWs = tdWidths derived
        widths1 = resizedWidths inp ts0
        geom = tableGeometry cfg f (ctxFontMetrics ctx) n (sizeofSmallArray (tdEncoded derived)) sizes contentWs widths1 ts0
        vis = tgVisible geom
    -- Last frame's body rect, for the header's resize edges.
    mPrevBody <- lastRect vWid
    -- Whether the body showed its vertical bar last frame. The header then
    -- keeps a spacer the width of the bar's lane, so its columns line up
    -- with the rows; it follows the body a frame late when the bar comes or
    -- goes. Only a change in the bar itself asks for that frame, not every
    -- change in the body's metrics.
    hasVertBar <- liftIO $ do
      let barOf = maybe False ((> 0) . v2Y . scrollRange)
      bar <- barOf <$> readScrollMetrics ctx vWid
      recordLayoutRead ctx (specKey slotScrollRange (intKey vWid)) ((/= bar) . barOf <$> readScrollMetrics ctx vWid)
      pure bar
    column' (tgOuter geom) $ do
      showAllResp <-
        if IS.null (tsHidden ts0)
          then pure Nothing
          else fmap Just $
            buttonStyledEx True "Show all columns" 0 (tight . fillW $ defaultLayout) 0
      headerPairs <- tablePanes (tableBare cfg) ctx ids encoders rowsVec derived geom sort0 hasVertBar
      covered <- liftIO (pointerCovered ctx tableWid)
      clip <- liftIO (getPrevClipRect ctx tableWid)
      disabled <- liftIO (isDisabled ctx tableWid)
      headerClips <- liftIO (mapM (getPrevClipRect ctx . rawRespId . snd) headerPairs)
      let mouse = inputMousePos inp
          reachable = not (covered || disabled) && maybe True (`rectContains` mouse) clip
          headerRects = [(i, rawRespRect r) | (i, r) <- headerPairs]
          -- Only edges in view: a header scrolled under the frozen pane or
          -- past the body's right edge has its edge outside its clip.
          shownEdges = [h | (h@(_, Rect x _ w _), Just (Rect cx _ cw _)) <- zip headerRects headerClips, x + w >= cx, x + w <= cx + cw + 0.5]
          -- Also used as the resize cursor's zones.
          edgeZones = headerEdgeZones 4 mPrevBody shownEdges
          hitCol zones = fst <$> find (\(_, r) -> rectContains r mouse) zones
          edgeCol = if reachable then hitCol edgeZones else Nothing
          headerW i = case lookup i headerRects of
            Just r | rectW r > 0 -> rectW r
            _ -> layoutMinW (smallAt (tgColBoxes geom) i (tight defaultLayout))
      -- A press on an edge resizes and arms no reorder, but a column already
      -- held can still be dropped by an edge.
      reorder <-
        withKey ("reorder" :: Text) $
          useReorder vis (if pressedIn MouseLeft inp && isJust edgeCol then [] else headerPairs)
      -- No resize arrow over the edges while a column is held.
      when (reachable && isNothing (reorderDragging reorder) && not (null edgeZones)) . liftIO $
        -- Strict in the spine and the rects, so no thunk waits in the IORef.
        modifyIORef' (ctxCursorZones ctx) (\zs -> foldl' (\acc (_, !r) -> (r, UiCursorColResize) : acc) zs edgeZones)
      let !(TableStep ts1 nextSort sortClicked hasChanged) =
            resolveTableInput
              TableInput
                { tiInput = inp
                , tiSort = sort0
                , tiSortable = tdSortable derived
                , tiVisible = vis
                , tiWidths = widths1
                , tiSizes = sizes
                , tiContentWs = contentWs
                , tiFill = tgFill geom
                , tiHeaders = headerPairs
                , tiShowAll = showAllResp
                , tiEdge = (\col -> (col, headerW col)) <$> edgeCol
                , tiReorder = reorder
                }
              ts0
          widgetResp =
            setChanged hasChanged $
              setClicked sortClicked (mconcat (map snd headerPairs ++ maybe [] pure showAllResp))
      liftIO (writeSlots ctx (writeTableState stateKey ts1))
      -- Repaints the table when the sort shown changes, however it came (a
      -- header click or a write to the Var).
      liftIO (writeSlot fieldInt ctx tableWid (intKey tableWid) (sortShownCode sort0))
      -- Keep the resize cursor for the whole drag, wherever the pointer is.
      when (tsResizeCol ts1 >= 0 && heldIn MouseLeft inp) $
        liftIO (modifyInteraction ctx (\s -> s {isColumnResize = True}))
      pure (TableResponse widgetResp nextSort (tsOrder ts1) (tsHidden ts1))

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
sortIndices :: Ord k => SortDir -> SmallArray k -> PrimArray Int
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
                  if inOrder
                    then copy lo lo (hi - lo)
                    else do
                      merge <- before <$> readPrimArray src lo <*> readPrimArray src (hi - 1)
                      if merge then go lo mid lo else copy mid lo (hi - mid) >> copy lo (lo + hi - mid) (mid - lo)
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

-- | Each column's resize zone: @pad@ either side of its header's right edge,
-- from the top of the header band to the bottom of the body's last-frame
-- rect (or of the header band, before the body has a rect).
headerEdgeZones :: Float -> Maybe Rect -> [(Int, Rect)] -> [(Int, Rect)]
headerEdgeZones pad mBody hdrs =
  [(i, Rect (x + w - pad) top (2 * pad) (bot - top)) | (i, Rect x _ w h) <- hdrs, w > 0 && h > 0]
  where
    top = minimum [y | (_, Rect _ y _ _) <- hdrs]
    bot = maximum ([y + h | (_, Rect _ y _ h) <- hdrs] ++ [by + bh | Rect _ by _ bh <- toList mBody])
