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
  , slotMenuOpen
  , slotMenuPos
  , slotScrollCfg
  , slotScrollOff
  , slotScrollContent
  , slotScrollCross
  , slotScrollLinkX
  , slotScrollLinkY
  , slotTextAreaRow
  , slotTextAreaCol
  , slotTextAreaPrefCol
  , slotTextAreaScroll
  , slotTextAreaViewport
  , slotTextAreaAnchorRow
  , slotTextAreaAnchorCol
  , slotTextInputScroll
  , boolInt
  , intBool
  , anySelectOpen
  , isSelectOpen
  , setSelectOpen
  , closeSelects
  )
where

import Data.Dynamic (Dynamic)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Text (Text)
import Data.Word (Word64)
import qualified Data.IntMap.Strict as IM
import NanoUI.Id (mix64)

-- | Unified widget state. Same-type fields that share a widget key use 'slotKey'.
data WidgetStore = WidgetStore
  { storeMirrorGen :: {-# UNPACK #-} !Word64
  , storeOpenSelect :: {-# UNPACK #-} !Int
  , storeInt :: !(IntMap Int)
  , storeFloat :: !(IntMap Float)
  , storePoint :: !(IntMap (Float, Float))
  , storeText :: !(IntMap Text)
  , storeIntSet :: !(IntMap IntSet)
  , storeFloatList :: !(IntMap [Float])
  , storeIntList :: !(IntMap [Int])
  , storeDyn :: !(IntMap Dynamic)
  }

instance Eq WidgetStore where
  a == b =
    storeMirrorGen a == storeMirrorGen b
      && storeOpenSelect a == storeOpenSelect b
      && storeInt a == storeInt b
      && storeFloat a == storeFloat b
      && storePoint a == storePoint b
      && storeText a == storeText b
      && storeIntSet a == storeIntSet b
      && storeFloatList a == storeFloatList b
      && storeIntList a == storeIntList b

instance Show WidgetStore where
  show st =
    "WidgetStore { "
      ++ "storeMirrorGen = " ++ show (storeMirrorGen st)
      ++ ", storeOpenSelect = " ++ show (storeOpenSelect st)
      ++ ", storeInt = " ++ show (storeInt st)
      ++ ", storeFloat = " ++ show (storeFloat st)
      ++ ", storePoint = " ++ show (storePoint st)
      ++ ", storeText = " ++ show (storeText st)
      ++ ", storeIntSet = " ++ show (storeIntSet st)
      ++ ", storeFloatList = " ++ show (storeFloatList st)
      ++ ", storeIntList = " ++ show (storeIntList st)
      ++ ", storeDynCount = " ++ show (IM.size (storeDyn st))
      ++ " }"

emptyWidgetStore :: WidgetStore
emptyWidgetStore =
  WidgetStore
    { storeMirrorGen = 0
    , storeOpenSelect = 0
    , storeInt = IM.empty
    , storeFloat = IM.empty
    , storePoint = IM.empty
    , storeText = IM.empty
    , storeIntSet = IM.empty
    , storeFloatList = IM.empty
    , storeIntList = IM.empty
    , storeDyn = IM.empty
    }

-- useText/useFlag bump this so Frame can re-run UI without watching every map.
{-# INLINE mirrorStoresChanged #-}
mirrorStoresChanged :: WidgetStore -> WidgetStore -> Bool
mirrorStoresChanged old new = storeMirrorGen old /= storeMirrorGen new

{-# INLINE bumpMirror #-}
bumpMirror :: WidgetStore -> WidgetStore
bumpMirror st = st {storeMirrorGen = storeMirrorGen st + 1}

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

slotMenuOpen :: Word64
slotMenuOpen = 0x4D454E5500000007

slotMenuPos :: Word64
slotMenuPos = 0x4D454E5500000008

slotScrollCfg :: Word64
slotScrollCfg = 0x5343524346000009

slotScrollOff :: Word64
slotScrollOff = 0x53434F464600000A

slotScrollCross :: Word64
slotScrollCross = 0x5343524F5800000C

slotScrollLinkX :: Word64
slotScrollLinkX = 0x534C4E4B5800000D

slotScrollLinkY :: Word64
slotScrollLinkY = 0x534C4E4B5900000E

slotScrollContent :: Word64
slotScrollContent = 0x534352435400000B

slotTextAreaRow :: Word64
slotTextAreaRow = 0x5441524100000010

slotTextAreaCol :: Word64
slotTextAreaCol = 0x5441524100000011

slotTextAreaPrefCol :: Word64
slotTextAreaPrefCol = 0x5441524100000012

slotTextAreaScroll :: Word64
slotTextAreaScroll = 0x5441524100000013

slotTextAreaViewport :: Word64
slotTextAreaViewport = 0x5441524100000014

slotTextAreaAnchorRow :: Word64
slotTextAreaAnchorRow = 0x5441524100000015

slotTextAreaAnchorCol :: Word64
slotTextAreaAnchorCol = 0x5441524100000016

slotTextInputScroll :: Word64
slotTextInputScroll = 0x54494E5000000017

boolInt :: Bool -> Int
boolInt b = if b then 1 else 0

intBool :: Int -> Bool
intBool n = n /= 0

-- One open select at a time.
{-# INLINE anySelectOpen #-}
anySelectOpen :: WidgetStore -> Bool
anySelectOpen st = storeOpenSelect st /= 0

{-# INLINE isSelectOpen #-}
isSelectOpen :: WidgetStore -> Int -> Bool
isSelectOpen st k = k /= 0 && storeOpenSelect st == k

{-# INLINE setSelectOpen #-}
setSelectOpen :: WidgetStore -> Int -> Bool -> WidgetStore
setSelectOpen st k True = st {storeOpenSelect = k}
setSelectOpen st k False
  | isSelectOpen st k = closeSelects st
  | otherwise = st

{-# INLINE closeSelects #-}
closeSelects :: WidgetStore -> WidgetStore
closeSelects st = st {storeOpenSelect = 0}
