-- | The widget store: per-widget state in maps by value type, keyed by widget
-- id and 'Slot'.
module NanoUI.Internal.Store
  ( WidgetStore (..)
  , emptyWidgetStore
  , mirrorStoresChanged
  , bumpMirror
  , Field
  , fieldInt
  , fieldFloat
  , fieldDouble
  , fieldPoint
  , fieldText
  , fieldIntSet
  , fieldFloatList
  , fieldIntList
  , fieldDyn
  , fieldQuiet
  , overField
  , lookupSlot
  , findSlot
  , memberSlot
  , insertSlot
  , deleteSlot
  , flagSlot
  , setFlagSlot
  , quietFlag
  , setQuietFlag
  , SlotWrites (..)
  , slotWrite
  , slotWriteOr
  , lookupDyn
  , insertDyn
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
  , diffKeys
  , slotChangedKeys
  )
where

import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Text (Text)
import Data.Word (Word64)
import qualified Data.IntMap.Strict as IM
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import NanoUI.Internal.Id (mix64)

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

-- | Keys whose values differ between two maps, a key that left or joined
-- included.
diffKeysBy :: (a -> a -> Bool) -> IntMap a -> IntMap a -> [Int]
diffKeysBy eq old new
  -- Unchanged maps keep their identity through a record update; skip the
  -- whole merge when the caller only rebuilt a different field.
  | ptrEq old new = []
  | otherwise =
      IM.keys
        ( IM.mergeWithKey
            (\_ a b -> if eq a b then Nothing else Just ())
            (IM.map (const ()))
            (IM.map (const ()))
            old
            new
        )

-- | 'diffKeysBy' with '==' behind a pointer check: an entry a map update left
-- alone keeps its object, so it compares without touching its value.
diffKeys :: Eq a => IntMap a -> IntMap a -> [Int]
diffKeys = diffKeysBy eqByPtr

-- | The keys whose values changed between two stores in every slot map but
-- the scroll offsets ('storeFloat', 'storePoint') and the 'storeQuiet'
-- bookkeeping. Lazy: a caller that only asks whether anything changed stops
-- at the first changed key.
slotChangedKeys :: WidgetStore -> WidgetStore -> [Int]
slotChangedKeys old new =
  diffKeys (storeInt old) (storeInt new)
    ++ diffKeys (storeDouble old) (storeDouble new)
    ++ diffKeys (storeText old) (storeText new)
    ++ diffKeys (storeFloatList old) (storeFloatList new)
    ++ diffKeys (storeIntList old) (storeIntList new)
    ++ diffKeys (storeIntSet old) (storeIntSet new)
    ++ diffKeysBy ptrEq (storeDyn old) (storeDyn new)

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
  , storeQuiet :: !(IntMap Int)
  -- ^ Interaction bookkeeping no paint reads, such as whether a drag hook's
  -- press is still held. Writes to it neither damage nor wake the loop: the
  -- visible effects of the interaction go through the widget's own slots.
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
      && eqByPtr (storeQuiet a) (storeQuiet b)

-- | One of the store's maps: how to read it, and how to put a new one back.
-- The slot functions inline at the field they are given, so
-- @insertSlot fieldInt k v@ compiles to the record update it stands for, and
-- a composition of them builds the store once.
data Field a = Field (WidgetStore -> IntMap a) (IntMap a -> WidgetStore -> WidgetStore)

-- | Integer slots, including boolean flags and selection indices.
fieldInt :: Field Int
fieldInt = Field storeInt (\m st -> st {storeInt = m})

-- | Single-precision numeric slots.
fieldFloat :: Field Float
fieldFloat = Field storeFloat (\m st -> st {storeFloat = m})

-- | Double-precision slots, including monotonic timestamps.
fieldDouble :: Field Double
fieldDouble = Field storeDouble (\m st -> st {storeDouble = m})

-- | Paired float slots for positions, sizes, or offsets.
fieldPoint :: Field (Float, Float)
fieldPoint = Field storePoint (\m st -> st {storePoint = m})

-- | Text-value slots.
fieldText :: Field Text
fieldText = Field storeText (\m st -> st {storeText = m})

-- | Integer-set slots, such as expanded tree-node indices.
fieldIntSet :: Field IntSet
fieldIntSet = Field storeIntSet (\m st -> st {storeIntSet = m})

-- | Ordered float-list slots.
fieldFloatList :: Field [Float]
fieldFloatList = Field storeFloatList (\m st -> st {storeFloatList = m})

-- | Ordered integer-list slots.
fieldIntList :: Field [Int]
fieldIntList = Field storeIntList (\m st -> st {storeIntList = m})

-- | Runtime-typed slots. Prefer 'lookupDyn' and 'insertDyn' for typed access.
fieldDyn :: Field Dynamic
fieldDyn = Field storeDyn (\m st -> st {storeDyn = m})

-- | Integer bookkeeping slots that no paint reads ('storeQuiet'). The store
-- diff that drives damage skips them.
fieldQuiet :: Field Int
fieldQuiet = Field storeQuiet (\m st -> st {storeQuiet = m})

-- | Read the map selected by a field descriptor.
{-# INLINE fieldMap #-}
fieldMap :: Field a -> WidgetStore -> IntMap a
fieldMap (Field get _) = get

-- | Pure update of one selected map; publish the resulting store through the context.
{-# INLINE overField #-}
overField :: Field a -> (IntMap a -> IntMap a) -> WidgetStore -> WidgetStore
overField (Field get set) f st = set (f (get st)) st

-- | Read a key from a typed map, returning 'Nothing' when absent.
{-# INLINE lookupSlot #-}
lookupSlot :: Field a -> Int -> WidgetStore -> Maybe a
lookupSlot field k = IM.lookup k . fieldMap field

-- | The slot's value, or @def@ while it has none.
{-# INLINE findSlot #-}
findSlot :: Field a -> a -> Int -> WidgetStore -> a
findSlot field def k = IM.findWithDefault def k . fieldMap field

-- | Whether a key exists in the selected map, regardless of its value.
{-# INLINE memberSlot #-}
memberSlot :: Field a -> Int -> WidgetStore -> Bool
memberSlot field k = IM.member k . fieldMap field

-- | Pure insert or replacement. Does not itself notify a context or schedule a frame.
{-# INLINE insertSlot #-}
insertSlot :: Field a -> Int -> a -> WidgetStore -> WidgetStore
insertSlot field k v = overField field (IM.insert k v)

-- | Pure removal of a key; an absent key leaves the map unchanged.
{-# INLINE deleteSlot #-}
deleteSlot :: Field a -> Int -> WidgetStore -> WidgetStore
deleteSlot field k = overField field (IM.delete k)

-- | An int slot read as a flag: set while it holds anything but 0.
{-# INLINE flagSlot #-}
flagSlot :: Int -> WidgetStore -> Bool
flagSlot k = intBool . findSlot fieldInt 0 k

-- | Raise a flag slot, or remove it.
{-# INLINE setFlagSlot #-}
setFlagSlot :: Int -> Bool -> WidgetStore -> WidgetStore
setFlagSlot k on = if on then insertSlot fieldInt k 1 else deleteSlot fieldInt k

-- | 'flagSlot' over the bookkeeping slots ('fieldQuiet').
{-# INLINE quietFlag #-}
quietFlag :: Int -> WidgetStore -> Bool
quietFlag k = intBool . findSlot fieldQuiet 0 k

-- | 'setFlagSlot' over the bookkeeping slots ('fieldQuiet').
{-# INLINE setQuietFlag #-}
setQuietFlag :: Int -> Bool -> WidgetStore -> WidgetStore
setQuietFlag k on = if on then insertSlot fieldQuiet k 1 else deleteSlot fieldQuiet k

-- | Slot writes that know whether they would change the store. Combine them
-- with '<>' and run them with 'NanoUI.Internal.Context.writeSlots', which leaves the
-- store alone when every slot already holds its value.
data SlotWrites = SlotWrites (WidgetStore -> Bool) (WidgetStore -> WidgetStore)

instance Semigroup SlotWrites where
  {-# INLINE (<>) #-}
  SlotWrites same f <> SlotWrites same' g = SlotWrites (\st -> same st && same' st) (f . g)

-- | Describe a write with an equality check. In @a <> b@, @a@ wins if both
-- writes target the same field and key.
{-# INLINE slotWrite #-}
slotWrite :: Eq a => Field a -> Int -> a -> SlotWrites
slotWrite field k v = SlotWrites (\st -> lookupSlot field k st == Just v) (insertSlot field k v)

-- | 'slotWrite' whose change check treats an absent slot as @def@. A batch
-- with no other changes can leave that slot empty; a batch that does write
-- may materialise the default value too.
{-# INLINE slotWriteOr #-}
slotWriteOr :: Eq a => Field a -> a -> Int -> a -> SlotWrites
slotWriteOr field def k v = SlotWrites (\st -> findSlot field def k st == v) (insertSlot field k v)

-- | Read a runtime-typed slot. 'Nothing' means absent or a different stored type.
{-# INLINE lookupDyn #-}
lookupDyn :: Typeable a => Int -> WidgetStore -> Maybe a
lookupDyn k st = IM.lookup k (storeDyn st) >>= fromDynamic

-- | Store a runtime-typed value, replacing any value under the same dynamic key.
{-# INLINE insertDyn #-}
insertDyn :: Typeable a => Int -> a -> WidgetStore -> WidgetStore
insertDyn k = insertSlot fieldDyn k . toDyn

-- | Empty maps, zero state generation, and no open select.
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
    , storeQuiet = IM.empty
    }

-- | Whether local-hook writes changed the generation used to request a view
-- rebuild. This compares the generation only, not individual maps.
{-# INLINE mirrorStoresChanged #-}
mirrorStoresChanged :: WidgetStore -> WidgetStore -> Bool
mirrorStoresChanged old new = storeMirrorGen old /= storeMirrorGen new

-- | Advance the local-state generation so the frame can rebuild dependent widgets.
{-# INLINE bumpMirror #-}
bumpMirror :: WidgetStore -> WidgetStore
bumpMirror st = st {storeMirrorGen = storeMirrorGen st + 1}

-- | Derive a key for a named sub-slot, separating values such as cursor and
-- anchor that share one widget and one typed map. Uses a non-cryptographic hash.
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
  | SlotTextAreaScroll
  | SlotTextAreaViewport
  | SlotTextAreaAnchorRow
  | SlotTextAreaAnchorCol
  | -- | The text area's 'NanoUI.Widgets.TextBuffer.TextBuffer' (in 'storeDyn'):
    -- its lines, which are its current document, its caret, and which lines
    -- changed since they were measured. Loads and paint read it. The document
    -- last passed or returned is under 'SlotSeen', in 'storeDyn'.
    SlotTextAreaBuffer
  | -- | For a text area over 'Text' (in 'storeDyn'): the text last passed or
    -- returned and the document it is the text of, so a frame that edits
    -- nothing neither splits nor joins the text.
    SlotTextAreaText
  | -- | Set (value 1) to signal that the text area's text changed through a path
    -- that does not flow through @Input@ (e.g. a context-menu cut/paste). The
    -- text area widget reads and clears this on its next frame, so the caller
    -- still gets a @respChanged@ pulse for edits that carry no keys or chars.
    SlotTextAreaChanged
  | -- | A text field's undo history with the text it was recorded against, in
    -- 'storeDyn'.
    SlotTextHistory
  | -- | How the text field with this id edits (its editor mode, in
    -- 'storeDyn'). Commands sent to the id between frames read it.
    SlotTextMode
  | -- | A text area's measured line widths and content extent, in 'storeDyn',
    -- kept in step with its lines so an edit remeasures only the lines it
    -- changed.
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
    -- negative = split id being resized. Shares the 'SlotDrag' press-held-release
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
  | -- | PaneGrid region span (storePoint): the (width, height) of the grid's
    -- own rect the tree was last fitted to. A different span means the grid
    -- was resized, which is when panes pinned by @pgFixedPanes@ have their
    -- splits reflowed to keep their extent. Tracked whether or not anything
    -- is pinned, so turning a pin on mid-run reflows from the size the tree
    -- really holds rather than from whenever a pin was last set.
    SlotPaneSpan
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

-- | Encode 'False' as 0 and 'True' as 1.
boolInt :: Bool -> Int
boolInt b = if b then 1 else 0

-- | Decode zero as 'False' and any nonzero integer as 'True'.
intBool :: Int -> Bool
intBool n = n /= 0

-- | Whether any select owns the single open-dropdown slot.
{-# INLINE anySelectOpen #-}
anySelectOpen :: WidgetStore -> Bool
anySelectOpen st = storeOpenSelect st /= 0

-- | Whether this nonzero widget key owns the open-dropdown slot.
{-# INLINE isSelectOpen #-}
isSelectOpen :: WidgetStore -> Int -> Bool
isSelectOpen st k = k /= 0 && storeOpenSelect st == k

-- | Open a select, replacing the current owner, or close it if it owns the slot.
{-# INLINE setSelectOpen #-}
setSelectOpen :: WidgetStore -> Int -> Bool -> WidgetStore
setSelectOpen st k True = st {storeOpenSelect = k}
setSelectOpen st k False
  | isSelectOpen st k = closeSelects st
  | otherwise = st

-- | Clear the open-dropdown owner.
{-# INLINE closeSelects #-}
closeSelects :: WidgetStore -> WidgetStore
closeSelects st = st {storeOpenSelect = 0}
