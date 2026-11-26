{-# OPTIONS_HADDOCK not-home #-}

-- | The widget store: per-widget state in maps by value type, keyed by widget
-- id and 'Slot'.
module NanoUI.Internal.Store
  ( WidgetStore (..)
  , emptyWidgetStore
  , Field
  , fieldInt
  , fieldFloat
  , fieldDouble
  , fieldPoint
  , fieldText
  , fieldIntSet
  , fieldBuffer
  , fieldDocument
  , fieldTextDocument
  , fieldHistory
  , fieldEditorMode
  , fieldLineWidths
  , fieldGrid
  , fieldTableState
  , fieldCombo
  , fieldRichSelection
  , fieldQuiet
  , overField
  , lookupSlot
  , findSlot
  , insertSlot
  , deleteSlot
  , flagSlot
  , SlotWrites (..)
  , slotWrite
  , slotWriteOr
  , slotKey
  , Slot (..)
  , SlotSpec (..)
  , slotSpec
  , ownSlotSpec
  , encodedSpec
  , floatBitsSpec
  , specKey
  , lookupSpec
  , findSpec
  , memberSpec
  , insertSpec
  , deleteSpec
  , specWrite
  , specWriteOr
  , fieldSelection
  , fieldSelectionWrite
  , setFieldSelection
  , collapseFieldSelection
  , boolInt
  , intBool
  , anySelectOpen
  , isSelectOpen
  , setSelectOpen
  , closeSelects
  , ptrEq
  , eqByPtr
  , sameFloat
  , sameDouble
  , diffKeys
  , slotChangedKeys
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Float (castFloatToWord32, castWord32ToFloat)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import NanoUI.Internal.Id (mix64)
import NanoUI.Internal.Equality (eqByPtr, ptrEq, sameDouble, sameFloat)
import NanoUI.Internal.Store.Types (ComboState, GridState, LineWidths, TableState, TextHistory, sameComboVisual)
import NanoUI.Internal.RichText.Types (RichSelection)
import NanoUI.Internal.TextEditor.Types (EditorMode)
import NanoUI.Internal.Types (V2)
import NanoUI.Internal.Widgets.TextDocument (TextDocument)
import NanoUI.Widgets.TextBuffer (TextBuffer)

-- | Keys whose values differ between two maps, a key that left or joined
-- included.
diffKeysBy :: (a -> a -> Bool) -> IntMap a -> IntMap a -> [Int]
diffKeysBy eq !old !new
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
slotChangedKeys !old !new
  | ptrEq old new = []
  | otherwise =
      diffKeys (storeInt old) (storeInt new)
        ++ diffKeys (storeDouble old) (storeDouble new)
        ++ diffKeys (storeText old) (storeText new)
        ++ diffKeys (storeIntSet old) (storeIntSet new)
        ++ diffKeysBy ptrEq (storeBuffer old) (storeBuffer new)
        ++ diffKeysBy ptrEq (storeDocument old) (storeDocument new)
        ++ diffKeysBy ptrEq (storeTextDocument old) (storeTextDocument new)
        ++ diffKeysBy ptrEq (storeHistory old) (storeHistory new)
        ++ diffKeys (storeEditorMode old) (storeEditorMode new)
        ++ diffKeysBy ptrEq (storeLineWidths old) (storeLineWidths new)
        ++ diffKeys (storeGrid old) (storeGrid new)
        ++ diffKeys (storeTableState old) (storeTableState new)
        ++ diffKeysBy sameComboVisual (storeCombo old) (storeCombo new)
        ++ diffKeys (storeRichSelection old) (storeRichSelection new)

-- | Widget state for every widget, in maps by value type. Same-type fields
-- that share a widget key use 'slotKey'.
data WidgetStore = WidgetStore
  { storeOpenSelect :: {-# UNPACK #-} !Int
  , storeInt :: !(IntMap Int)
  , storeFloat :: !(IntMap Float)
  , storeDouble :: !(IntMap Double)
  , storePoint :: !(IntMap V2)
  , storeText :: !(IntMap Text)
  , storeIntSet :: !(IntMap IntSet)
  , storeBuffer :: !(IntMap TextBuffer)
  , storeDocument :: !(IntMap TextDocument)
  , storeTextDocument :: !(IntMap (Text, TextDocument))
  , storeHistory :: !(IntMap TextHistory)
  , storeEditorMode :: !(IntMap EditorMode)
  , storeLineWidths :: !(IntMap LineWidths)
  , storeGrid :: !(IntMap GridState)
  , storeTableState :: !(IntMap TableState)
  , storeCombo :: !(IntMap ComboState)
  , storeRichSelection :: !(IntMap RichSelection)
  , storeQuiet :: !(IntMap Int)
  -- ^ Interaction bookkeeping that repaints nothing by itself, such as
  -- whether a drag hook's press is still held. Writes to it neither damage
  -- nor wake the loop: the visible effects of the interaction go through the
  -- widget's own slots.
  }

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

-- | 2D vector slots for positions, sizes, or offsets.
fieldPoint :: Field V2
fieldPoint = Field storePoint (\m st -> st {storePoint = m})

-- | Text-value slots.
fieldText :: Field Text
fieldText = Field storeText (\m st -> st {storeText = m})

-- | Integer-set slots, such as expanded tree-node indices.
fieldIntSet :: Field IntSet
fieldIntSet = Field storeIntSet (\m st -> st {storeIntSet = m})

-- | A text area's 'TextBuffer', by widget key.
fieldBuffer :: Field TextBuffer
fieldBuffer = Field storeBuffer (\m st -> st {storeBuffer = m})

-- | The 'TextDocument' last passed to or returned from a text area.
fieldDocument :: Field TextDocument
fieldDocument = Field storeDocument (\m st -> st {storeDocument = m})

-- | The text last passed to or returned from a text area over 'Text', with
-- its document.
fieldTextDocument :: Field (Text, TextDocument)
fieldTextDocument = Field storeTextDocument (\m st -> st {storeTextDocument = m})

-- | A text area's undo and redo history.
fieldHistory :: Field TextHistory
fieldHistory = Field storeHistory (\m st -> st {storeHistory = m})

-- | A text editor's modal editing mode.
fieldEditorMode :: Field EditorMode
fieldEditorMode = Field storeEditorMode (\m st -> st {storeEditorMode = m})

-- | A text area's measured line widths.
fieldLineWidths :: Field LineWidths
fieldLineWidths = Field storeLineWidths (\m st -> st {storeLineWidths = m})

-- | A grid's state.
fieldGrid :: Field GridState
fieldGrid = Field storeGrid (\m st -> st {storeGrid = m})

-- | A table's column order, dragged widths, hidden set, and active resize.
fieldTableState :: Field TableState
fieldTableState = Field storeTableState (\m st -> st {storeTableState = m})

-- | A combo box's state between frames.
fieldCombo :: Field ComboState
fieldCombo = Field storeCombo (\m st -> st {storeCombo = m})

-- | A selectable paragraph's selection, kept apart from its measurement cache.
fieldRichSelection :: Field RichSelection
fieldRichSelection = Field storeRichSelection (\m st -> st {storeRichSelection = m})

-- | Integer bookkeeping slots that repaint nothing by themselves
-- ('storeQuiet'). The store diff that drives damage skips them.
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

-- | Slot writes that know whether they would change the store. Combine them
-- with '<>' and run them with 'NanoUI.Internal.Context.writeSlots', which leaves the
-- store alone when every slot already holds its value.
data SlotWrites = SlotWrites (WidgetStore -> Bool) (WidgetStore -> WidgetStore)

instance Semigroup SlotWrites where
  {-# INLINE (<>) #-}
  SlotWrites same f <> SlotWrites same' g = SlotWrites (\st -> same st && same' st) (f . g)

instance Monoid SlotWrites where
  {-# INLINE mempty #-}
  mempty = SlotWrites (const True) id

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

-- | Where one value of a widget's state lives: the key it is stored under,
-- as a function of the widget's key, the 'Field' map that holds it, and how
-- the value is kept there (decode, encode). A spec names the map and the
-- stored encoding once, so a reader and a writer of the same state cannot
-- disagree on either. "NanoUI.Internal.Store.Slots" lists the built-in
-- ones.
--
-- The functions over a spec inline at a constant one: @insertSpec
-- (slotSpec SlotCursor fieldInt) k v@ compiles to @insertSlot fieldInt
-- (slotKey SlotCursor k) v@ (checked by the @nano-ui-inspection@ suite).
data SlotSpec a = forall s. SlotSpec (Int -> Int) (Field s) (s -> a) (a -> s)

-- | Slot @s@ of a widget in map @field@, stored as it is.
{-# INLINE slotSpec #-}
slotSpec :: Slot -> Field a -> SlotSpec a
slotSpec s field = SlotSpec (slotKey s) field id id

-- | The widget's own key in map @field@, stored as it is.
{-# INLINE ownSlotSpec #-}
ownSlotSpec :: Field a -> SlotSpec a
ownSlotSpec field = SlotSpec id field id id

-- | A spec whose values are stored as @spec@'s, through @dec@ and @enc@.
-- @dec . enc@ must be 'id'.
{-# INLINE encodedSpec #-}
encodedSpec :: (s -> a) -> (a -> s) -> SlotSpec s -> SlotSpec a
encodedSpec dec enc (SlotSpec key field d e) = SlotSpec key field (dec . d) (e . enc)

-- | A 'Float' kept as its 32 bits in an 'Int' slot, such as one in
-- 'fieldQuiet'.
{-# INLINE floatBitsSpec #-}
floatBitsSpec :: SlotSpec Int -> SlotSpec Float
floatBitsSpec = encodedSpec (castWord32ToFloat . fromIntegral) (fromIntegral . castFloatToWord32)

-- | The store key of a widget key's value.
{-# INLINE specKey #-}
specKey :: SlotSpec a -> Int -> Int
specKey (SlotSpec key _ _ _) = key

-- | 'lookupSlot' through a spec.
{-# INLINE lookupSpec #-}
lookupSpec :: SlotSpec a -> Int -> WidgetStore -> Maybe a
lookupSpec (SlotSpec key field dec _) k st = dec <$> lookupSlot field (key k) st

-- | 'findSlot' through a spec: the value, or @def@ while there is none.
{-# INLINE findSpec #-}
findSpec :: SlotSpec a -> a -> Int -> WidgetStore -> a
findSpec (SlotSpec key field dec enc) def k st = dec (findSlot field (enc def) (key k) st)

-- | Whether a key exists in the spec's map, regardless of its value.
{-# INLINE memberSpec #-}
memberSpec :: SlotSpec a -> Int -> WidgetStore -> Bool
memberSpec (SlotSpec key field _ _) k = IM.member (key k) . fieldMap field

-- | 'insertSlot' through a spec.
{-# INLINE insertSpec #-}
insertSpec :: SlotSpec a -> Int -> a -> WidgetStore -> WidgetStore
insertSpec (SlotSpec key field _ enc) k v = insertSlot field (key k) (enc v)

-- | 'deleteSlot' through a spec.
{-# INLINE deleteSpec #-}
deleteSpec :: SlotSpec a -> Int -> WidgetStore -> WidgetStore
deleteSpec (SlotSpec key field _ _) k = deleteSlot field (key k)

-- | 'slotWrite' through a spec.
{-# INLINE specWrite #-}
specWrite :: Eq a => SlotSpec a -> Int -> a -> SlotWrites
specWrite spec k v = SlotWrites (\st -> lookupSpec spec k st == Just v) (insertSpec spec k v)

-- | 'slotWriteOr' through a spec.
{-# INLINE specWriteOr #-}
specWriteOr :: Eq a => SlotSpec a -> a -> Int -> a -> SlotWrites
specWriteOr spec def k v = SlotWrites (\st -> findSpec spec def k st == v) (insertSpec spec k v)

-- | Empty maps and no open select.
emptyWidgetStore :: WidgetStore
emptyWidgetStore =
  WidgetStore
    { storeOpenSelect = 0
    , storeInt = IM.empty
    , storeFloat = IM.empty
    , storeDouble = IM.empty
    , storePoint = IM.empty
    , storeText = IM.empty
    , storeIntSet = IM.empty
    , storeBuffer = IM.empty
    , storeDocument = IM.empty
    , storeTextDocument = IM.empty
    , storeHistory = IM.empty
    , storeEditorMode = IM.empty
    , storeLineWidths = IM.empty
    , storeGrid = IM.empty
    , storeTableState = IM.empty
    , storeCombo = IM.empty
    , storeRichSelection = IM.empty
    , storeQuiet = IM.empty
    }

-- | Derive a key for a named sub-slot, separating values such as cursor and
-- anchor that share one widget and one typed map. Uses a non-cryptographic hash.
{-# INLINE slotKey #-}
slotKey :: Slot -> Int -> Int
slotKey s k = fromIntegral (mix64 (fromIntegral k) (slotTag s))

-- | Single-line field @key@'s @(anchor, cursor)@ over @text@, the one
-- reading of its slots: an unstored cursor is at the end and an unstored
-- anchor on the cursor, both clamped to the text.
fieldSelection :: WidgetStore -> Int -> Text -> (Int, Int)
fieldSelection store key text =
  let len = T.length text
      cursor = min len (findSlot fieldInt len (slotKey SlotCursor key) store)
      anchor = min len (findSlot fieldInt cursor (slotKey SlotAnchor key) store)
   in (anchor, cursor)

-- | Store field @key@'s selection over @text@, a change only when it differs
-- from what 'fieldSelection' reads.
fieldSelectionWrite :: Int -> Text -> Int -> Int -> SlotWrites
fieldSelectionWrite key text anchor cursor =
  SlotWrites
    (\st -> fieldSelection st key text == (anchor, cursor))
    (setFieldSelection key anchor cursor)

-- | Store field @key@'s anchor and cursor.
setFieldSelection :: Int -> Int -> Int -> WidgetStore -> WidgetStore
setFieldSelection key anchor cursor =
  insertSlot fieldInt (slotKey SlotAnchor key) anchor . insertSlot fieldInt (slotKey SlotCursor key) cursor

-- | Collapse field @key@'s selection onto its cursor ('fieldSelection').
collapseFieldSelection :: Int -> WidgetStore -> WidgetStore
collapseFieldSelection key = deleteSlot fieldInt (slotKey SlotAnchor key)

-- | Every built-in slot. The map each slot's values live in, and how they
-- are stored there, is in its 'SlotSpec' in "NanoUI.Internal.Store.Slots";
-- read and write a slot through that spec.
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
  | SlotScrollCross
  | SlotScrollLinkX
  | SlotScrollLinkY
  | SlotScrollStep
  | SlotScrollAxes
  | SlotScrollRange
  | SlotScrollContent
  | -- | An offset in window axes set before the scroller's first layout
    -- (storePoint), placed on the axes it owns once it has one.
    SlotScrollPending
  | SlotTextAreaScroll
  | SlotTextAreaViewport
  | SlotTextAreaAnchorRow
  | SlotTextAreaAnchorCol
  | -- | The text area's 'NanoUI.Widgets.TextBuffer.TextBuffer' (in 'storeBuffer'):
    -- its lines, which are its current document, its caret, and which lines
    -- changed since they were measured. Loads and paint read it. The document
    -- last passed or returned is under 'SlotSeen', in 'storeDocument'.
    SlotTextAreaBuffer
  | -- | For a text area over 'Text' (in 'storeTextDocument'): the text last passed or
    -- returned and the document it is the text of, so a frame that edits
    -- nothing neither splits nor joins the text.
    SlotTextAreaText
  | -- | Set (value 1) to signal that a text field's or text area's text changed
    -- through a path that does not flow through @Input@ (e.g. a context-menu
    -- cut/paste). The widget reads and clears this on its next frame, so the
    -- caller still gets a @respChanged@ pulse for edits that carry no keys or
    -- chars.
    SlotTextAreaChanged
  | -- | A text field's undo history with the text it was recorded against, in
    -- 'storeHistory'.
    SlotTextHistory
  | -- | How the text field with this id edits (its editor mode, in
    -- 'storeEditorMode'). Commands sent to the id between frames read it.
    SlotTextMode
  | -- | A text area's measured line widths and content extent, in 'storeLineWidths',
    -- kept in step with its lines so an edit remeasures only the lines it
    -- changed.
    SlotTextAreaWidths
  | -- | A text area's row height as it was last painted (storeFloat), for
    -- commands run between frames to keep the caret in view with.
    SlotTextAreaLineH
  | -- | Set (value 1, in 'storeQuiet') when a text area's editor state is
    -- saved: its next paint, which has the font, scrolls the caret into view
    -- across and clears it. The saved state already keeps the caret in view
    -- vertically, and its slots repaint the area.
    SlotTextAreaReveal
  | SlotTextInputScroll

  | -- | Search-field debounce bookkeeping, on the text widget id: the last
    -- committed query (storeText) and the monotonic time of the last edit
    -- (storeDouble).
    SlotSearchCommitted
  | SlotSearchAge
  | -- | The value a controlled widget last returned to its caller.
    SlotSeen
  | -- | A colour picker's opening colour.
    SlotColorBase
  | -- | The stepper arrow a numeric field's press holds: 1 up, -1 down.
    SlotNumericHeld
  | -- | When a numeric field's held stepper arrow next repeats, in monotonic
    -- seconds.
    SlotNumericRepeat
  | -- | When a tooltip opens, in monotonic microseconds ('storeQuiet', keyed
    -- by tooltip id); absent while its target is not hovered. Key 0 holds
    -- when any tooltip was last open, for the grace period.
    SlotTooltipShow
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
