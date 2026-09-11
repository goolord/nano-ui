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
  , slotDrop
  , slotDropPos
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
  , slotSearchCommitted
  , slotSearchAge
  , slotComboHighlight
  , slotComboScroll
  , slotComboCount
  , slotComboScrollX
  , slotComboContentW
  , slotComboDrag
  , slotComboDragOff
  , slotComboCommitted
  , slotComboFocus
  , slotComboLive
  , slotPaneGest
  , slotPaneGrab
  , slotPaneFocus
  , slotPaneMax
  , slotPaneResize
  , slotPaneNext
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
  , storeDouble :: !(IntMap Double)
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
      && storeDouble a == storeDouble b
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
      ++ ", storeDouble = " ++ show (storeDouble st)
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
    , storeDouble = IM.empty
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

slotDrop :: Word64
slotDrop = 0xD20D000000000018

slotDropPos :: Word64
slotDropPos = 0xD20D000000000019

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

-- Search-field debounce bookkeeping. Text slots on the text widget id: the last
-- committed query and the monotonic timestamp of the last edit.
slotSearchCommitted :: Word64
slotSearchCommitted = 0x534541524300001D

slotSearchAge :: Word64
slotSearchAge = 0x534541524700001B

-- Combo box suggestion state (storeInt/storeFloat, keyed by the field
-- widget): the highlighted option index (absolute into the filtered list),
-- the start of the visible window slice (keyboard / wheel / scrollbar
-- scrolling), and the scrollbar bookkeeping the overlay painter and the
-- widget's thumb-drag gesture share (total filtered count, widest row, x
-- offset, drag axis + grab offset).
slotComboHighlight :: Word64
slotComboHighlight = 0x434F4D424F000030

slotComboScroll :: Word64
slotComboScroll = 0x434F4D424F000031

slotComboCount :: Word64
slotComboCount = 0x434F4D424F000032

slotComboScrollX :: Word64
slotComboScrollX = 0x434F4D424F000033

slotComboContentW :: Word64
slotComboContentW = 0x434F4D424F000034

slotComboDrag :: Word64
slotComboDrag = 0x434F4D424F000035

slotComboDragOff :: Word64
slotComboDragOff = 0x434F4D424F000036

-- The last committed value (storeText): typing edits the live field text but
-- only Enter, a row click, or losing focus commits it (Escape reverts).
slotComboCommitted :: Word64
slotComboCommitted = 0x434F4D424F000037

-- Had-focus flag (storeInt) so the widget can see the focus-lost transition
-- on the frame after blur and commit then.
slotComboFocus :: Word64
slotComboFocus = 0x434F4D424F000038

-- The field text as the widget last produced it (storeText): a frame-start
-- value that differs from it changed externally (a frame-side row pick or a
-- clipboard menu action), not by typing.
slotComboLive :: Word64
slotComboLive = 0x434F4D424F000039

-- PaneGrid gesture slot (storeInt): 0 none, positive = dragged pane id,
-- negative = split id being resized. Mirrors slotDrag's press-held-release
-- lifecycle but keyed by the grid widget instead of a per-pane leaf.
slotPaneGest :: Word64
slotPaneGest = 0x50414E450000001C

-- PaneGrid drag grab offset (storePoint): (mouse - pane origin) at grab start.
slotPaneGrab :: Word64
slotPaneGrab = 0x50414E450000001D

-- PaneGrid keyboard-navigation focus: focused pane id (0 = none, auto-first).
slotPaneFocus :: Word64
slotPaneFocus = 0x50414E450000001E

-- PaneGrid maximize state: maximized pane id (0 = none).
slotPaneMax :: Word64
slotPaneMax = 0x50414E450000001F

-- PaneGrid resize start (storePoint): (ratio, main-axis mouse) captured when a
-- divider is first grabbed, so dragging moves it by delta rather than snapping.
slotPaneResize :: Word64
slotPaneResize = 0x50414E4500000020

-- PaneGrid id seed (storeInt): next split / pane id to allocate. Strictly
-- monotonic per grid — ids are never reused, so per-pane state keyed by pane
-- id cannot collide with a closed pane's state.
slotPaneNext :: Word64
slotPaneNext = 0x50414E4500000021

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
