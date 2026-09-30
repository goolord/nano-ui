-- | Typed, bounded caches for data derived by the built-in widgets.
module NanoUI.Internal.Derived
  ( DerivedCache
  , emptyDerivedCache
  , DerivedField
  , tableCache
  , treeCache
  , comboCache
  , lookupDerived
  , insertDerived
  , TableDerived (..)
  , Opaque (..)
  , SortCol (..)
  , SortDir (..)
  , TreeItem (..)
  , TreeRow
  , TreeRows (..)
  , ComboMatches (..)
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.SmallArray (SmallArray)
import Data.Text (Text)
import Data.Vector (Vector)

-- | Ascending or descending text order.
data SortDir = SortAsc | SortDesc
  deriving (Eq, Show, Enum, Bounded)

-- | Source-column index, independent of display order.
data SortCol = SortCol {sortColIndex :: !Int, sortColDir :: !SortDir}
  deriving (Eq, Show)

-- | Retained only for conservative pointer comparison, never cast or decoded.
data Opaque = forall a. Opaque a

-- | Encoded table cells, their widths, and the sorted row indices.
data TableDerived = TableDerived
  { tdRows :: !Opaque
  , tdCols :: !Opaque
  , tdFont :: !Opaque
  , tdMonoFont :: !Opaque
  , tdRowObjs :: !(SmallArray Opaque)
  , tdHeaders :: !(Vector Text)
  , tdEncoded :: !(SmallArray (Vector Text))
  , tdCellW :: !(PrimArray Float)
  , tdTextRow :: !(PrimArray Int)
  , tdWidths :: !(PrimArray Float)
  , tdNumeric :: !(SmallArray Bool)
  , tdSort :: !SortCol
  , tdOrder :: !(PrimArray Int)
  }

-- | A tree row's label and children. An empty child list makes a leaf.
data TreeItem = TreeItem {treeItemLabel :: !Text, treeItemChildren :: ![TreeItem]}
  deriving (Eq, Show)

-- | Pre-order index, depth, has-children flag, and label.
type TreeRow = (Int, Int, Bool, Text)

-- | The input container is retained only to compare identity.
data TreeRows = forall f. TreeRows !(f TreeItem) !IntSet !Int !(SmallArray TreeRow)

-- | Options, query, matches, and the widest match once measured.
data ComboMatches = ComboMatches ![Text] !Text [Text] !(Maybe Float)

data DerivedCache = DerivedCache
  { derivedTables :: !(IntMap TableDerived)
  , derivedTrees :: !(IntMap TreeRows)
  , derivedCombos :: !(IntMap ComboMatches)
  }

emptyDerivedCache :: DerivedCache
emptyDerivedCache = DerivedCache IM.empty IM.empty IM.empty

-- | A concrete typed map, chosen at the call site and inlined there.
data DerivedField a
  = DerivedField
      (DerivedCache -> IntMap a)
      (IntMap a -> DerivedCache -> DerivedCache)

tableCache :: DerivedField TableDerived
tableCache = DerivedField derivedTables (\m c -> c {derivedTables = m})

treeCache :: DerivedField TreeRows
treeCache = DerivedField derivedTrees (\m c -> c {derivedTrees = m})

comboCache :: DerivedField ComboMatches
comboCache = DerivedField derivedCombos (\m c -> c {derivedCombos = m})

{-# INLINE lookupDerived #-}
lookupDerived :: DerivedField a -> Int -> DerivedCache -> Maybe a
lookupDerived (DerivedField get _) k = IM.lookup k . get

-- | Bound all maps together to 64 entries. Replacing an entry does not evict;
-- growing a full cache clears it, as the previous shared cache did.
{-# INLINE insertDerived #-}
insertDerived :: DerivedField a -> Int -> a -> DerivedCache -> DerivedCache
insertDerived (DerivedField get set) k value cache =
  let
    full =
      IM.size (derivedTables cache)
        + IM.size (derivedTrees cache)
        + IM.size (derivedCombos cache)
        >= 64
    base = if full && IM.notMember k (get cache) then emptyDerivedCache else cache
   in
    set (IM.insert k value (get base)) base
