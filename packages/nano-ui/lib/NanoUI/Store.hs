-- | The widget store: per-widget state in maps by value type, keyed by widget
-- id and 'Slot'.
module NanoUI.Store
  ( WidgetStore (..)
  , emptyWidgetStore
  , mirrorStoresChanged
  , bumpMirror
  , slotKey
  , Slot (..)
  , boolInt
  , intBool
  , anySelectOpen
  , isSelectOpen
  , setSelectOpen
  , closeSelects
  , ptrEq
  , eqByPtr
  )
where

import Data.Dynamic (Dynamic)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Text (Text)
import Data.Word (Word64)
import qualified Data.IntMap.Strict as IM
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import NanoUI.Id (mix64)

-- | Physical-equality shortcut. Pointer equality implies value equality for
-- immutable values, so callers may use 'True' to skip a structural comparison
-- of a field the caller never rebuilt. 'False' only means \"compare properly\".
{-# INLINE ptrEq #-}
ptrEq :: a -> a -> Bool
ptrEq a b = isTrue# (reallyUnsafePtrEquality# a b)

-- | '==' with a physical-equality fast path. Unchanged fields of a
-- record-updated store keep their identity, so whole-store comparisons become
-- cheap when only one map was rebuilt.
{-# INLINE eqByPtr #-}
eqByPtr :: Eq a => a -> a -> Bool
eqByPtr a b = ptrEq a b || a == b

-- | Dynamic values do not implement Eq, but we can verify equality via
-- pointer equality fast path followed by checking key structure and
-- pointer equality of each Dynamic element.
{-# INLINE eqDynMap #-}
eqDynMap :: IntMap Dynamic -> IntMap Dynamic -> Bool
eqDynMap a b =
  ptrEq a b
    || (IM.size a == IM.size b && IM.isSubmapOfBy ptrEq a b)

-- | Widget state for every widget, in maps by value type. Same-type fields
-- that share a widget key use 'slotKey'.
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
      && eqByPtr (storeInt a) (storeInt b)
      && eqByPtr (storeFloat a) (storeFloat b)
      && eqByPtr (storeDouble a) (storeDouble b)
      && eqByPtr (storePoint a) (storePoint b)
      && eqByPtr (storeText a) (storeText b)
      && eqByPtr (storeIntSet a) (storeIntSet b)
      && eqByPtr (storeFloatList a) (storeFloatList b)
      && eqByPtr (storeIntList a) (storeIntList b)
      && eqDynMap (storeDyn a) (storeDyn b)

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
{-# INLINE slotKey #-}
slotKey :: Slot -> Int -> Int
slotKey s k = fromIntegral (mix64 (fromIntegral k) (slotTag s))

-- | Every built-in slot.
data Slot
  = SlotCursor
  | SlotAnchor
  | SlotDrag
  | SlotDragW
  | SlotDrop
  | SlotDropPos
  | SlotWinSize
  | SlotMenuOpen
  | SlotMenuPos
  | SlotScrollCfg
  | SlotScrollOff
  | SlotScrollCross
  | SlotScrollLinkX
  | SlotScrollLinkY
  | SlotScrollStep
  | SlotScrollAxes
  | SlotScrollViewPos
  | SlotScrollViewSize
  | SlotScrollRange
  | SlotScrollContent
  | SlotTextAreaRow
  | SlotTextAreaCol
  | SlotTextAreaPrefCol
  | SlotTextAreaScroll
  | SlotTextAreaViewport
  | SlotTextAreaAnchorRow
  | SlotTextAreaAnchorCol
  | -- | Cached text-area content extent (max line width, line count * line
    -- height) and the node font size they were measured at. Recomputing the width
    -- scans every character of the document, so it is cached and only refreshed
    -- when the text or font changes.
    SlotTextAreaContentW
  | SlotTextAreaContentH
  | SlotTextAreaContentFont
  | -- | Cached 'TextBuffer' for the text area, keyed by its flat 'Text'. Loads and
    -- paint reuse it so the document is not re-split into lines every call.
    SlotTextAreaBuffer
  | -- | Set (value 1) to signal that the text area's text changed through a path
    -- that does not flow through 'Input' (e.g. a context-menu cut/paste). The
    -- text area widget reads and clears this on its next frame, so the caller
    -- still gets a 'respChanged' pulse for edits that carry no keys or chars.
    SlotTextAreaChanged
  | -- | A text field's undo history with the text it was recorded against, in
    -- 'storeDyn'.
    SlotTextHistory
  | -- | Which kind of text field a widget id is: 1 single-line, 2 multi-line.
    -- Commands sent to the id between frames read it.
    SlotTextMode
  | -- | A text area's measured line widths, in 'storeDyn', kept in step with its
    -- lines so an edit remeasures only the lines it changed.
    SlotTextAreaWidths
  | SlotTextInputScroll
  | -- | Search-field debounce bookkeeping. Text slots on the text widget id: the last
    -- committed query and the monotonic timestamp of the last edit.
    SlotSearchCommitted
  | SlotSearchAge
  | -- | Combo box suggestion state (storeInt/storeFloat, keyed by the field
    -- widget): the highlighted option index (absolute into the filtered list),
    -- the start of the visible window slice (keyboard / wheel / scrollbar
    -- scrolling), and the scrollbar bookkeeping the overlay painter and the
    -- widget's thumb-drag gesture share (total filtered count, widest row, x
    -- offset, drag axis + grab offset).
    SlotComboHighlight
  | SlotComboScroll
  | SlotComboCount
  | SlotComboScrollX
  | SlotComboContentW
  | SlotComboDrag
  | SlotComboDragOff
  | -- | The last committed value (storeText): typing edits the live field text but
    -- only Enter, a row click, or losing focus commits it (Escape reverts).
    SlotComboCommitted
  | -- | Had-focus flag (storeInt) so the widget can see the focus-lost transition
    -- on the frame after blur and commit then.
    SlotComboFocus
  | -- | The field text as the widget last produced it (storeText): a frame-start
    -- value that differs from it changed externally (a frame-side row pick or a
    -- clipboard menu action), not by typing.
    SlotComboLive
  | -- | PaneGrid gesture slot (storeInt): 0 none, positive = dragged pane id,
    -- negative = split id being resized. Mirrors 'SlotDrag''s press-held-release
    -- lifecycle but keyed by the grid widget instead of a per-pane leaf.
    SlotPaneGest
  | -- | PaneGrid drag grab offset (storePoint): (mouse - pane origin) at grab start.
    SlotPaneGrab
  | -- | PaneGrid keyboard-navigation focus: focused pane id (0 = none, auto-first).
    SlotPaneFocus
  | -- | PaneGrid maximize state: maximized pane id (0 = none).
    SlotPaneMax
  | -- | PaneGrid resize start (storePoint): (ratio, main-axis mouse) captured when a
    -- divider is first grabbed, so dragging moves it by delta rather than snapping.
    SlotPaneResize
  | -- | PaneGrid id seed (storeInt): next split / pane id to allocate. Strictly
    -- monotonic per grid: ids are never reused, so per-pane state keyed by pane
    -- id cannot collide with a closed pane's state.
    SlotPaneNext
  | -- | The value a controlled widget last returned to its caller.
    SlotSeen
  | -- | A colour picker's opening colour.
    SlotColorBase
  | -- | The stepper arrow a numeric field's press holds: 1 up, -1 down.
    SlotNumericHeld
  | -- | When a numeric field's held stepper arrow next repeats, in monotonic
    -- seconds.
    SlotNumericRepeat
  deriving (Enum)

-- | Tag for a built-in slot: the constructor index mixed with a salt, so tags
-- are well spread.
{-# INLINE slotTag #-}
slotTag :: Slot -> Word64
slotTag s = mix64 0x534C4F5454414753 (fromIntegral (fromEnum s))

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
