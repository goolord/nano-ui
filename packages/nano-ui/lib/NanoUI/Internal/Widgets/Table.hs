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
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (find)
import Data.List qualified as L
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Primitive.PrimArray (PrimArray, copyMutablePrimArray, copyPrimArray, generatePrimArray, indexPrimArray, newPrimArray, primArrayFromList, readPrimArray, sizeofPrimArray, unsafeFreezePrimArray, writePrimArray)
import Data.Primitive.SmallArray (SmallArray, SmallMutableArray, emptySmallArray, indexSmallArray, mapSmallArray', newSmallArray, sizeofSmallArray, smallArrayFromList, smallArrayFromListN, unsafeFreezeSmallArray, writeSmallArray)
import Data.Primitive.Types (Prim)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import NanoUI.Internal.Context (Context (..), InteractionState (..), getPrevRect, getPrevClipRect, pointerCovered, isDisabled, getScrollOffset2D, getStore, intKey, linkScrollAxes, modifyInteraction, writeSlot, writeSlots)
import NanoUI.Internal.Var (Binding (..), bindVarOn, useVar)
import NanoUI.Internal.Var.Types (Var)
import NanoUI.Internal.Derived (TableDerived (..), Opaque (..), SortCol (..), SortDir (..), tableCache)
import NanoUI.Internal.Font (FontMetrics (..), ScrollBarSlot (..), scrollBarGutter, tableCellInset, lineWidthIO)
import NanoUI.Internal.Input (Input (..), MouseButton (..), Pressable (..), UiCursorKind (..))
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (NanoUI, askInput, freshWidget, lastRect, nextId, liftIO, scope, styled, uiTheme, withKey)
import NanoUI.Internal.Store (Slot (..), SlotWrites (..), eqByPtr, ptrEq, fieldFloat, fieldInt, fieldIntSet, fieldTableColumns, findSlot, insertSlot, lookupSlot, slotKey, slotWrite)
import NanoUI.Internal.Style (AlignX (..), AlignY (..), Direction (..), FontVariant (..), Layout (..), Sizing (..), Style (..), defaultLayout, fillH, fillW, minW, panelStyle, tight)
import Data.Bits ((.|.), shiftL)
import Data.String (IsString (..))
import NanoUI.Internal.Types (Rect (..), clamp, rectH, rectW, v2X, V2 (..), rectContains)
import NanoUI.Internal.WidgetText (buttonFlagTable, stripeColor, tableHeaderLabel, tableSortBlank)
import NanoUI.Internal.Widgets.Button (buttonContentStyled)
import NanoUI.Internal.Widgets.Behavior (Reorder (..), useReorder)
import NanoUI.Internal.Widgets.Combinators (buttonStyledEx, readDerived, writeDerived)
import NanoUI.Internal.Widgets.Layout (column', panel', row', scrollAreaIdConfigured, separator, spacer)
import NanoUI.Internal.Frame.Scroll.Geometry (defaultScrollConfig, scrollHorizontalHidden, scrollVerticalAuto, scrollVerticalHidden)
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
  -- its pinned widths (fixed columns, content floors, dragged widths), scale
  -- them all down proportionally, none under 'minColW', rather than surfacing
  -- a horizontal scrollbar. A stretch column takes part with a bounded
  -- share, and the scrollbar's own lane is budgeted for. Off by
  -- default: a table too wide for its pane keeps its sideways scroll.
  }
  deriving (Eq, Show)

-- | No frozen or hidden rows/columns and content-sized columns.
defaultTableConfig :: TableConfig
defaultTableConfig = TableConfig 0 0 [] IS.empty False

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

-- The sort mark uses bits 16-17 (see tableSortMarkOf), above the font fields.
sortMarkStyle :: SortCol -> Int -> Int
sortMarkStyle sort idx
  | sortColIndex sort /= idx = 0
  | sortColDir sort == SortDesc = 2 `shiftL` 16
  | otherwise = 1 `shiftL` 16

minColW :: Float
minColW = 40

-- | What one table cell (or header) shows.
data Cell = Cell
  { cellKey  :: !(Maybe Text)
  -- ^ Sort key; 'Nothing' = unsortable by this cell.
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
textCell t = Cell (Just t) t Nothing

-- | Arbitrary interactive widget cell (unsortable by default).
cellWidget :: NanoUI () -> Cell
cellWidget w = Cell Nothing T.empty (Just w)

-- | Encode a function @a -> NanoUI ()@ into a table cell.
widgetCell :: (a -> NanoUI ()) -> a -> Cell
widgetCell f = cellWidget . f

-- | Attach a text sort key to a cell. If the cell's 'cellText' is empty,
-- also populates 'cellText' with the key so column width can be estimated.
cellSort :: Text -> Cell -> Cell
cellSort k c = c { cellKey = Just k, cellText = if T.null (cellText c) then k else cellText c }

-- | Alias for 'cellSort'.
cellSortKey :: Text -> Cell -> Cell
cellSortKey = cellSort

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
          let key r = let c = toCell (Encode.oneColonnadeEncode col r) in fromMaybe (cellText c) (cellKey c)
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
orderFor :: SortCol -> SmallArray (V.Vector Text) -> PrimArray Int
orderFor sort keys = sortIndices (sortColDir sort) (mapSmallArray' (sortKey sort) keys)

sortKey :: SortCol -> V.Vector Text -> Text
sortKey sort row = fromMaybe T.empty (row V.!? sortColIndex sort)

-- | A row's cells, each evaluated, and their sort keys: the cells
-- themselves (the same vector) unless a cell's key is not its text. Marks in
-- @sortable@ each column with a key or text, and in @views@ each column with
-- a widget view.
encodeRow :: ToCell c => SmallMutableArray RealWorld Bool -> SmallMutableArray RealWorld Bool -> V.Vector (Encode.OneColonnade Headed row c) -> row -> IO (V.Vector Text, V.Vector Text)
encodeRow sortable views encoders r = do
  let k = V.length encoders
  txts <- MV.unsafeNew k
  keys <- MV.unsafeNew k
  let fill !j !distinct
        | j >= k = pure distinct
        | otherwise = do
            let !c = toCell (Encode.oneColonnadeEncode (V.unsafeIndex encoders j) r)
                !t = cellText c
                !ky = fromMaybe t (cellKey c)
            MV.unsafeWrite txts j t
            MV.unsafeWrite keys j ky
            when (isJust (cellKey c) || not (T.null t)) (writeSmallArray sortable j True)
            when (isJust (cellView c)) (writeSmallArray views j True)
            fill (j + 1) (distinct || not (eqByPtr ky t))
  distinct <- fill 0 False
  !vTxts <- V.unsafeFreeze txts
  !vKeys <- if distinct then V.unsafeFreeze keys else pure vTxts
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
                if d < 0
                  then store i (Opaque r) e ky >> walk (i + 1) k rs
                  else store i (Opaque r) e' ky' >> note k i d >> walk (i + 1) (k + 1) rs
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
        -- As the header shows it: the sort mark is drawn over a blank slot.
        writePrimArray hdrWM i . (+ cellPadX) =<< lineWidthIO fm (hdrs V.! i <> tableSortBlank)
      cellWs <- unsafeFreezePrimArray cellWM
      hdrWs <- unsafeFreezePrimArray hdrWM
      let widest i !r !w = if r >= n then w else widest i (r + 1) (max w (indexPrimArray cellWs (i * n + r)))
          widths = generatePrimArray c $ \i ->
            let hdrW = indexPrimArray hdrWs i in if n == 0 then hdrW else max hdrW (widest i 0 minColW)
      -- The order stands while no row came or went and no sort key changed.
      let keysKept d =
            let key = sortKey (tdSort d)
                s = sortColIndex (tdSort d)
                keyChanged r f = s == f || (s > f && not (eqByPtr (key (indexSmallArray oldKeys r)) (key (indexSmallArray keys r))))
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

-- Minimum column width, even when dragged: the declared fixed width, else the
-- content minimum so cells never wrap.
colFloor :: SmallArray ColSize -> PrimArray Float -> Int -> Float
colFloor sizes contentWs i = case smallAt sizes i ColContent of
  ColFixed f -> max minColW f
  ColStretch -> minColW
  ColContent -> max minColW (primAt contentWs i minColW)

-- | Scale pinned column widths down to @budget@ so a squeezing table fits its
-- viewport in place of scrolling sideways. Each pass rescales what is left in
-- proportion and floors every width at 'minColW', so columns that reach the
-- floor stop contributing and the others absorb the rest of the shortfall;
-- when everything sits at the floor the widths no longer shrink, and the
-- table keeps whatever scroll is left.
fitWidthsTo :: Float -> [Float] -> [Float]
fitWidthsTo budget0 = go
  where
    budget = max 0 budget0
    go ws
      | sum ws <= budget + 0.5 = ws
      | otherwise =
          let total = sum ws
              ws' = map (\w -> max minColW (w * budget / total)) ws
           in if ws' == ws then ws else go ws'

flatLayout :: Layout
flatLayout = tight defaultLayout {layoutGap = 0}

tableRowMinH :: Float
tableRowMinH = 28

textCellLayout, numCellLayout :: Layout
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

-- | Column @i@'s box: its stored width (never under its floor) once it has
-- one, else a share of the spare width when the columns fill the table, else
-- its floor. Columns stretch to the row height so every cell's background
-- and borders span the full row even when one cell wraps.
--
-- A stretch column is the table's slack: its minimum is only @stretchMinW@,
-- so it compresses (under a vertical scrollbar's lane, say, which narrows
-- the viewport by the lane's width) rather than reporting its content as a
-- minimum that surfaces a horizontal bar under a table that fits the
-- scroller's full width. Its drag floor ('colFloor') still binds once the
-- column has been resized to a stored width.
colBoxLayout :: Bool -> Bool -> Float -> SmallArray ColSize -> PrimArray Float -> PrimArray Float -> Int -> Layout
colBoxLayout fillInner hasStretch stretchMinW sizes contentWs stored i
  | saved > 0 = fixed (max floorW saved)
  | ColStretch <- size, fillInner = stretchBox {layoutWidth = Grow 1}
  | ColContent <- size, fillInner && not hasStretch = box {layoutWidth = Grow 1}
  | otherwise = fixed floorW
  where
    saved = primAt stored i 0
    floorW = colFloor sizes contentWs i
    size = smallAt sizes i ColContent
    box = (fillH flatLayout) {layoutMinW = floorW}
    stretchBox = (fillH flatLayout) {layoutMinW = stretchMinW}
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
    inp <- askInput
    st0 <- liftIO (getStore ctx)
    TableDerived
      { tdEncoded = encoded
      , tdSortable = sortable
      , tdViews = views
      , tdWidths = contentWs
      , tdNumeric = numeric
      , tdOrder = sorted
      } <-
      liftIO (tableDerived ctx stateKey cols inputRows sort0)
    let sizes = smallArrayFromList (tableColSizes cfg)
        -- The column order and the widths columns were dragged to.
        (storedOrder, storedWidths) = fromMaybe ([0 .. n - 1], []) (lookupSlot fieldTableColumns stateKey st0)
        order0 = normalizeOrder n storedOrder
        hidden0 = findSlot fieldIntSet (tableHidden cfg) stateKey st0
        widths0 = take n (storedWidths ++ repeat 0 :: [Float])
        -- The column being resized, or -1. The drag slot holds it plus one,
        -- so an empty slot reads as idle.
        drag0 = findSlot fieldInt 0 (slotKey SlotDrag stateKey) st0 - 1
        dragX0 = findSlot fieldFloat 0 stateKey st0
        dragW0 = findSlot fieldFloat 0 (slotKey SlotDragW stateKey) st0
        mx = v2X (inputMousePos inp)
        widths1
          | drag0 >= 0 && heldIn MouseLeft inp =
              setAt drag0 (max (colFloor sizes contentWs drag0) (dragW0 + mx - dragX0)) widths0
          | otherwise = widths0
    -- Last frame's body rect, for the squeeze budget and (below) the header's
    -- vertical-bar spacer: a resize settles one frame later, as that spacer
    -- already does.
    mPrevBody <- lastRect vWid
    let nRows = sizeofSmallArray encoded
        widths1Arr = primArrayFromList widths1
        outerLayout = f (fillW flatLayout)
        hasStretch = any (\i -> smallAt sizes i ColContent == ColStretch) vis
        vis = filter (`IS.notMember` hidden0) order0
        freezeN = clamp 0 (length vis) (tableFreezeCols cfg)
        frozenIdx = take freezeN vis
        unfrozenIdx = drop freezeN vis
        pinnedN = clamp 0 nRows (tableFreezeRows cfg)
        scrollN = nRows - pinnedN
        -- Columns fill the table width when one stretches or the table
        -- grows.
        fillInner =
          hasStretch || case layoutWidth outerLayout of
            Grow _ -> True
            _ -> False
        fillIf b = if b then fillW else id
        -- What column i gives up under the squeeze: its dragged width once
        -- it has one, else its floor. A stretch column has no width of its
        -- own, so it takes part with a bounded share, which keeps fixed
        -- columns from crushing it to nothing.
        pinnedW i
          | primAt widths1Arr i 0 > 0 = max (colFloor sizes contentWs i) (primAt widths1Arr i 0)
          | smallAt sizes i ColContent == ColStretch = max minColW (min 120 (primAt contentWs i minColW))
          | otherwise = colFloor sizes contentWs i
        -- Squeeze: with the flag set and the body narrower than the pinned
        -- widths plus rules, scale every pinned width down to fit, none
        -- under minColW. The vertical bar's lane, when the rows overflow,
        -- comes off the budget first, so a squeezed table does not surface a
        -- horizontal bar for the lane either.
        rulesW = fromIntegral (max 0 (length vis - 1))
        rowH = max tableRowMinH (fmLineHeight (ctxFontMetrics ctx) + 6)
        rowStep = rowH + 1
        totalH = if scrollN <= 0 then 0 else fromIntegral scrollN * rowH + fromIntegral (scrollN - 1)
        firstFrameCapH = case layoutHeight outerLayout of
          Fixed h -> max 0 (min h (layoutMaxH outerLayout) - rowStep)
          Fit
            | layoutMaxH outerLayout < 1e8 -> max 0 (layoutMaxH outerLayout - rowStep)
            | otherwise -> totalH
          _ -> 100
        rowsOverrun = totalH > maybe firstFrameCapH rectH mPrevBody + 0.5
        squeezeBudget =
          if tableSqueeze cfg
            then fmap (\r -> max 0 (rectW r - (if rowsOverrun then scrollBarGutter ScrollBarList 0 else 0))) mPrevBody
            else Nothing
        -- The sizes the columns are laid out from: under the squeeze, each
        -- column's scaled by the ratio of its fitted to its pinned width.
        (sizesLay, contentWsLay, widthsLay) = case squeezeBudget of
          Nothing -> (sizes, contentWs, widths1Arr)
          Just budget ->
            let fitted = IM.fromList (zip vis (fitWidthsTo (budget - rulesW) (map pinnedW vis)))
                scaleOf i = maybe 1 (\w -> w / max 1.0e-3 (pinnedW i)) (IM.lookup i fitted)
             in ( smallArrayFromListN n [case smallAt sizes i ColContent of ColFixed w -> ColFixed (w * scaleOf i); s -> s | i <- [0 .. n - 1]]
                , primArrayFromList [primAt contentWs i minColW * scaleOf i | i <- [0 .. n - 1]]
                , primArrayFromList [primAt widths1Arr i 0 * scaleOf i | i <- [0 .. n - 1]]
                )
        -- A stretch column's minimum: a token width outside the squeeze,
        -- which the solver takes as an explicit minimum in place of the
        -- column's content.
        stretchMinW = if isJust squeezeBudget then minColW else 1.0e-3
        colBoxes =
          smallArrayFromList (map (colBoxLayout fillInner hasStretch stretchMinW sizesLay contentWsLay widthsLay) [0 .. n - 1])
        colBox i = smallAt colBoxes i (tight defaultLayout)
        cellLayout i = if smallAt numeric i False then numCellLayout else textCellLayout
        isSortable i = smallAt sortable i False
        renderHeader i active cellLay flags = do
          let oneCol = encoders V.! i
              hdrCell = toCell (getHeaded (Encode.oneColonnadeHead oneCol))
              styleIx = if active then 1 else 0
          case cellView hdrCell of
            Nothing ->
              buttonStyledEx True (tableHeaderLabel (smallAt numeric i False) (cellText hdrCell)) styleIx cellLay flags
            Just hdrAct -> buttonContentStyled styleIx flags cellLay hdrAct
        textCellAt ri i t = do
          wid <- nextId
          void (addWidgetStyled wid NodeText t 0 (cellLayout i) (if even ri then 1 else 2))
        -- Text cells show their cached text; only a column with widget
        -- views encodes its visible cells again, for the views.
        renderCell ri r i
          | not (smallAt views i False) = textCellAt ri i (indexSmallArray encoded r V.! i)
          | otherwise = case cellView (toCell (Encode.oneColonnadeEncode (V.unsafeIndex encoders i) (rowsVec V.! r))) of
            Nothing -> textCellAt ri i (indexSmallArray encoded r V.! i)
            Just widgetAct -> do
              theme <- uiTheme
              let mBg = stripeColor theme (if even ri then 1 else 2)
                  cellStyle = case mBg of
                    Just bg -> styled (panelStyle (\s -> s {styleBg = bg, styleBorderWidth = 0, styleCornerRadius = 0}))
                    Nothing -> id
              cellStyle (panel' (cellLayout i) widgetAct)
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
              let isSort = isSortable i
                  isActive = sortColIndex sort0 == i && isSort
                  flags = (if isSort then sortMarkStyle sort0 i else 0) .|. buttonFlagTable
                  hdrLay = (cellLayout i) {layoutFontVariant = FontRegular}
              withKey i $
                column' (colBox i) $
                  renderHeader i isActive hdrLay flags
          void separator
          let !rowLay = gridRowLay idxs
              !colLays = map colBox idxs
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
                when rowsOverrun $ void (spacer (Fixed (scrollBarGutter ScrollBarList 0)) Fit)
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
          isResize = drag0 >= 0
      -- A press on an edge resizes and arms no reorder, but a column already
      -- held can still be dropped by an edge.
      reorder <-
        withKey ("reorder" :: Text) $
          useReorder vis (if pressedIn MouseLeft inp && isJust edgeCol then [] else headerPairs)
      -- No resize arrow over the edges while a column is held.
      when (reachable && isNothing (reorderDragging reorder) && not (null edgeZones)) . liftIO $
        -- Strict in the spine and the rects, so no thunk waits in the IORef.
        modifyIORef' (ctxCursorZones ctx) (\zs -> foldl' (\acc (_, !r) -> (r, UiCursorEwResize) : acc) zs edgeZones)
      let vis' = reorderOrder reorder
          mReorder = reorderDragging reorder
          headerW i = case lookup i headerRects of
            Just r | rectW r > 0 -> rectW r
            _ -> layoutMinW (colBox i)
          (nextDrag, nextDragX, nextDragW)
            | pressedIn MouseLeft inp, Just col <- edgeCol = (col, mx, headerW col)
            | pressedIn MouseLeft inp || releasedIn MouseLeft inp || not (heldIn MouseLeft inp) || not isResize = (-1, 0, 0)
            | otherwise = (drag0, dragX0, dragW0)
          nextOrder = if vis' /= vis then rebuildOrder hidden0 vis' order0 else order0
          -- respRightClicked, not a bare release: a right press that went down
          -- elsewhere and came up over a header must not hide that column.
          hideClicked = [i | (i, r) <- headerPairs, respRightClicked r, not isResize, isNothing mReorder]
          nextHidden = case showAllResp of
            Just r | respClicked r -> IS.empty
            _ -> case hideClicked of
              (i : _) | length vis > 1 -> IS.insert i hidden0
              _ -> hidden0
          sortClick =
            if reorderMoved reorder || isJust mReorder || isResize || (isJust edgeCol && (heldIn MouseLeft inp || releasedIn MouseLeft inp))
              then Nothing
              else listToMaybe [i | (i, r) <- headerPairs, respClicked r, isSortable i]
          nextSort = maybe sort0 (nextSortCol sort0) sortClick
          hasChanged = nextSort /= sort0 || nextOrder /= order0 || nextHidden /= hidden0 || widths1 /= widths0
          widgetResp =
            setChanged hasChanged $
              setClicked (hasChanged && isJust sortClick) (mconcat (map snd headerPairs ++ maybe [] pure showAllResp))
      -- Compare before writing, so an idle table writes nothing.
      liftIO . writeSlots ctx $
        SlotWrites (\st -> lookupSlot fieldTableColumns stateKey st == Just (nextOrder, widths1)) (insertSlot fieldTableColumns stateKey (nextOrder, widths1))
          <> slotWrite fieldIntSet stateKey nextHidden
          <> slotWrite fieldInt (slotKey SlotDrag stateKey) (nextDrag + 1)
          <> slotWrite fieldFloat stateKey nextDragX
          <> slotWrite fieldFloat (slotKey SlotDragW stateKey) nextDragW
      -- The headers show the sort only through their style, which no diff
      -- covers, so a slot on the table repaints it when the sort shown
      -- changes, however it came (a header click or a write to the Var).
      liftIO (writeSlot fieldInt ctx tableWid (intKey tableWid) (2 * sortColIndex sort0 + fromEnum (sortColDir sort0 == SortDesc)))
      -- Keep the resize cursor for the whole drag, wherever the pointer is.
      when (nextDrag >= 0 && heldIn MouseLeft inp) $
        liftIO (modifyInteraction ctx (\s -> s {isColumnResize = True}))
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
