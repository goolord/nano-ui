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

import Control.Applicative ((<|>))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.SmallArray (SmallArray)
import Data.Text (Text)
import Data.Vector (Vector)

-- | A table column's sort direction: ascending or descending.
data SortDir = SortAsc | SortDesc
  deriving (Eq, Show, Enum, Bounded, Ord)

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
  , tdKeys :: !(SmallArray (Vector Text))
  , tdSortable :: !(SmallArray Bool)
  , tdViews :: !(SmallArray Bool)
  -- ^ Each column that has a cell with a widget view.
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

-- | Options, query, matches, and the widest match once measured. The options
-- container is retained only to compare identity.
data ComboMatches = forall f. ComboMatches !(f Text) !Text [Text] !(Maybe Float)

-- | Two-generation bounded map: avoids cliff wipes when live widget count
-- exceeds the per-generation cap while aging out unmounted widgets.
data DerivedGen a = DerivedGen !(IntMap a) !(IntMap a)

emptyDerivedGen :: DerivedGen a
emptyDerivedGen = DerivedGen IM.empty IM.empty

data DerivedCache = DerivedCache
  { derivedTables :: !(DerivedGen TableDerived)
  , derivedTrees :: !(DerivedGen TreeRows)
  , derivedCombos :: !(DerivedGen ComboMatches)
  }

emptyDerivedCache :: DerivedCache
emptyDerivedCache = DerivedCache emptyDerivedGen emptyDerivedGen emptyDerivedGen

-- | A concrete typed map, chosen at the call site and inlined there.
data DerivedField a
  = DerivedField
      (DerivedCache -> DerivedGen a)
      (DerivedGen a -> DerivedCache -> DerivedCache)

tableCache :: DerivedField TableDerived
tableCache = DerivedField derivedTables (\m c -> c {derivedTables = m})

treeCache :: DerivedField TreeRows
treeCache = DerivedField derivedTrees (\m c -> c {derivedTrees = m})

comboCache :: DerivedField ComboMatches
comboCache = DerivedField derivedCombos (\m c -> c {derivedCombos = m})

{-# INLINE lookupDerived #-}
lookupDerived :: DerivedField a -> Int -> DerivedCache -> Maybe a
lookupDerived (DerivedField get _) k cache =
  let DerivedGen cur prev = get cache
   in IM.lookup k cur <|> IM.lookup k prev

-- | Bound each cache generation to 64 entries. Rotating moves the current
-- generation to previous instead of wiping all live entries at once.
{-# INLINE insertDerived #-}
insertDerived :: DerivedField a -> Int -> a -> DerivedCache -> DerivedCache
insertDerived (DerivedField get set) k value cache =
  let DerivedGen cur prev = get cache
      gen'
        | IM.size cur >= 64 && IM.notMember k cur = DerivedGen (IM.singleton k value) cur
        | otherwise = DerivedGen (IM.insert k value cur) prev
   in set gen' cache
