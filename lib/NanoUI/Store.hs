module NanoUI.Store
  ( WidgetStore (..)
  , emptyWidgetStore
  , mirrorStoresChanged
  , bumpMirror
  , slotKey
  , slotDisabled
  , slotCursor
  , slotAnchor
  , slotDrag
  , slotDragW
  , slotWinSize
  , boolInt
  , intBool
  , pairList
  , listPair
  , anySelectOpen
  , isSelectOpen
  , setSelectOpen
  , closeSelects
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Text (Text)
import Data.Word (Word64)
import qualified Data.IntMap.Strict as IM
import NanoUI.Id (mix64)

-- | Unified widget state. Same-type fields that share a widget key use 'slotKey'.
data WidgetStore = WidgetStore
  { storeInt :: !(IntMap Int)
  , storeFloat :: !(IntMap Float)
  , storePoint :: !(IntMap (Float, Float))
  , storeText :: !(IntMap Text)
  , storeIntSet :: !(IntMap IntSet)
  , storeFloatList :: !(IntMap [Float])
  , storeIntList :: !(IntMap [Int])
  }
  deriving (Eq, Show)

emptyWidgetStore :: WidgetStore
emptyWidgetStore =
  WidgetStore
    { storeInt = IM.empty
    , storeFloat = IM.empty
    , storePoint = IM.empty
    , storeText = IM.empty
    , storeIntSet = IM.empty
    , storeFloatList = IM.empty
    , storeIntList = IM.empty
    }

-- useText/useFlag bump this so Frame can re-run UI without watching every map.
mirrorStoresChanged :: WidgetStore -> WidgetStore -> Bool
mirrorStoresChanged old new =
  IM.lookup mirrorRoot (storeInt old) /= IM.lookup mirrorRoot (storeInt new)

bumpMirror :: WidgetStore -> WidgetStore
bumpMirror st =
  st {storeInt = IM.insertWith (+) mirrorRoot 1 (storeInt st)}

-- Mix a field tag into a widget key so two Ints (cursor vs anchor) do not collide.
slotKey :: Word64 -> Int -> Int
slotKey tag k = fromIntegral (mix64 (fromIntegral k) tag)

slotDisabled :: Word64
slotDisabled = 0xD15AB1ED00000001

slotCursor :: Word64
slotCursor = 0xC025000100000002

slotAnchor :: Word64
slotAnchor = 0xA4C4000200000003

slotDrag :: Word64
slotDrag = 0xD2A6000400000004

slotDragW :: Word64
slotDragW = 0xD2A6000500000005

slotWinSize :: Word64
slotWinSize = 0x5712E00600000006

boolInt :: Bool -> Int
boolInt b = if b then 1 else 0

intBool :: Int -> Bool
intBool n = n /= 0

pairList :: (Float, Float) -> [Float]
pairList (a, b) = [a, b]

listPair :: [Float] -> Maybe (Float, Float)
listPair [a, b] = Just (a, b)
listPair _ = Nothing

-- One open select at a time (replaces IM.singleton on the old Bool map).
anySelectOpen :: WidgetStore -> Bool
anySelectOpen st = IM.findWithDefault 0 selectOpenRoot (storeInt st) /= 0

isSelectOpen :: WidgetStore -> Int -> Bool
isSelectOpen st k =
  k /= 0 && IM.findWithDefault 0 selectOpenRoot (storeInt st) == k

setSelectOpen :: WidgetStore -> Int -> Bool -> WidgetStore
setSelectOpen st k True =
  st {storeInt = IM.insert selectOpenRoot k (storeInt st)}
setSelectOpen st k False
  | isSelectOpen st k = closeSelects st
  | otherwise = st

closeSelects :: WidgetStore -> WidgetStore
closeSelects st =
  st {storeInt = IM.insert selectOpenRoot 0 (storeInt st)}

mirrorRoot :: Int
mirrorRoot = fromIntegral (mix64 0x4E414E4F 0x4D495200)

selectOpenRoot :: Int
selectOpenRoot = fromIntegral (mix64 0x4E414E4F 0x53454C00)
