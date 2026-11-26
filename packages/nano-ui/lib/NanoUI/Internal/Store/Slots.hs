-- | The built-in widget state: for each value, the 'Slot' it is keyed by,
-- the 'NanoUI.Internal.Store.Field' map that holds it, and how it is stored
-- there, as one 'SlotSpec'. A widget reads and writes a value through its
-- spec ('findSpec', 'insertSpec', 'specWrite'), so the map and the encoding
-- are written down once.
--
-- One 'Slot' can key values in several maps: 'SlotDrag' keys the press a
-- drag started under ('slotDragPress', in 'fieldQuiet'), the item being
-- dragged ('slotDragItem', in 'fieldInt') and the last pointer position
-- ('slotDragPoint', in 'fieldPoint'). The maps are separate, so these do not
-- collide.
module NanoUI.Internal.Store.Slots
  ( -- * Text fields
    slotTextInputScroll
  , slotTextChanged
  , slotTextHistory
  , slotTextMode
  , slotSearchCommitted
  , slotSearchAge
  , slotSeen
    -- * Text areas
  , slotTextAreaScroll
  , slotTextAreaViewport
  , slotTextAreaAnchorRow
  , slotTextAreaAnchorCol
  , slotTextAreaBuffer
  , slotTextAreaText
  , slotTextAreaWidths
  , slotTextAreaLineH
  , slotTextAreaReveal
    -- * Drags and drops
  , slotDragPress
  , slotDragItem
  , slotDragPoint
  , slotDragStartPos
  , slotDrop
  , slotDropPos
    -- * Windows, menus and tooltips
  , slotWinSize
  , slotMenuOpen
  , slotMenuPos
  , slotTooltipShow
    -- * Scrolling
  , slotScrollCross
  , slotScrollLinkX
  , slotScrollLinkY
  , slotScrollStep
  , slotScrollAxes
  , slotScrollRange
  , slotScrollContent
  , slotScrollPending
    -- * Other widgets
  , slotColorBase
  , slotNumericHeld
  , slotNumericRepeat
  , setFlagSpec
  )
where

import Data.Text (Text)
import NanoUI.Internal.Store
import NanoUI.Internal.Store.Types (LineWidths, TextHistory)
import NanoUI.Internal.TextEditor.Types (EditorMode)
import NanoUI.Internal.Types (V2)
import NanoUI.Internal.Widgets.TextDocument (TextDocument)
import NanoUI.Widgets.TextBuffer (TextBuffer)

-- | An int slot read as a flag: set while it holds anything but 0, as
-- 'flagSlot' reads it, and written as 1 or 0.
{-# INLINE flagSpec #-}
flagSpec :: SlotSpec Int -> SlotSpec Bool
flagSpec = encodedSpec intBool boolInt

-- | Raise a flag, or remove its slot, as 'setFlagSlot' does.
{-# INLINE setFlagSpec #-}
setFlagSpec :: SlotSpec Bool -> Int -> Bool -> WidgetStore -> WidgetStore
setFlagSpec spec k on = if on then insertSpec spec k True else deleteSpec spec k

-- Text fields ----------------------------------------------------------------

-- | A single-line field's horizontal text scroll.
{-# INLINE slotTextInputScroll #-}
slotTextInputScroll :: SlotSpec Float
slotTextInputScroll = slotSpec SlotTextInputScroll fieldFloat

-- | Present (value 1) when a text field's or text area's text changed by a
-- path that carries no input, such as a context-menu paste
-- ('SlotTextAreaChanged').
{-# INLINE slotTextChanged #-}
slotTextChanged :: SlotSpec Int
slotTextChanged = slotSpec SlotTextAreaChanged fieldInt

-- | A text field's or text area's undo history.
{-# INLINE slotTextHistory #-}
slotTextHistory :: SlotSpec TextHistory
slotTextHistory = slotSpec SlotTextHistory fieldHistory

-- | How a text field edits, for commands sent between frames.
{-# INLINE slotTextMode #-}
slotTextMode :: SlotSpec EditorMode
slotTextMode = slotSpec SlotTextMode fieldEditorMode

-- | A search field's last committed query.
{-# INLINE slotSearchCommitted #-}
slotSearchCommitted :: SlotSpec Text
slotSearchCommitted = slotSpec SlotSearchCommitted fieldText

-- | The monotonic time of a search field's last edit, in seconds.
{-# INLINE slotSearchAge #-}
slotSearchAge :: SlotSpec Double
slotSearchAge = slotSpec SlotSearchAge fieldDouble

-- | The value a controlled widget last returned, in the map its value
-- lives in.
{-# INLINE slotSeen #-}
slotSeen :: Field a -> SlotSpec a
slotSeen = slotSpec SlotSeen

-- Text areas -----------------------------------------------------------------

{-# INLINE slotTextAreaScroll #-}
slotTextAreaScroll :: SlotSpec V2
slotTextAreaScroll = slotSpec SlotTextAreaScroll fieldPoint

{-# INLINE slotTextAreaViewport #-}
slotTextAreaViewport :: SlotSpec V2
slotTextAreaViewport = slotSpec SlotTextAreaViewport fieldPoint

{-# INLINE slotTextAreaAnchorRow #-}
slotTextAreaAnchorRow :: SlotSpec Int
slotTextAreaAnchorRow = slotSpec SlotTextAreaAnchorRow fieldInt

{-# INLINE slotTextAreaAnchorCol #-}
slotTextAreaAnchorCol :: SlotSpec Int
slotTextAreaAnchorCol = slotSpec SlotTextAreaAnchorCol fieldInt

{-# INLINE slotTextAreaBuffer #-}
slotTextAreaBuffer :: SlotSpec TextBuffer
slotTextAreaBuffer = slotSpec SlotTextAreaBuffer fieldBuffer

{-# INLINE slotTextAreaText #-}
slotTextAreaText :: SlotSpec (Text, TextDocument)
slotTextAreaText = slotSpec SlotTextAreaText fieldTextDocument

{-# INLINE slotTextAreaWidths #-}
slotTextAreaWidths :: SlotSpec LineWidths
slotTextAreaWidths = slotSpec SlotTextAreaWidths fieldLineWidths

{-# INLINE slotTextAreaLineH #-}
slotTextAreaLineH :: SlotSpec Float
slotTextAreaLineH = slotSpec SlotTextAreaLineH fieldFloat

-- | Present (value 1) while a text area's next paint should bring its caret
-- into view across. Bookkeeping, so it repaints nothing.
{-# INLINE slotTextAreaReveal #-}
slotTextAreaReveal :: SlotSpec Int
slotTextAreaReveal = slotSpec SlotTextAreaReveal fieldQuiet

-- Drags and drops ------------------------------------------------------------

-- | The press ('NanoUI.Internal.Context.leftPress') a drag started under, 0
-- for none. Bookkeeping, so it repaints nothing.
{-# INLINE slotDragPress #-}
slotDragPress :: SlotSpec Int
slotDragPress = slotSpec SlotDrag fieldQuiet

-- | The item a drag holds: a reordered item, or a table's resized column
-- (stored plus one, see the table).
{-# INLINE slotDragItem #-}
slotDragItem :: SlotSpec Int
slotDragItem = slotSpec SlotDrag fieldInt

-- | A drag's last pointer position.
{-# INLINE slotDragPoint #-}
slotDragPoint :: SlotSpec V2
slotDragPoint = slotSpec SlotDrag fieldPoint

-- | Where a reorder's press went down.
{-# INLINE slotDragStartPos #-}
slotDragStartPos :: SlotSpec V2
slotDragStartPos = slotSpec SlotDragW fieldPoint

-- | Whether a reorder has moved past its threshold, or a file drop is under
-- way.
{-# INLINE slotDrop #-}
slotDrop :: SlotSpec Bool
slotDrop = flagSpec (slotSpec SlotDrop fieldInt)

-- | Where a file drop last reported the pointer.
{-# INLINE slotDropPos #-}
slotDropPos :: SlotSpec V2
slotDropPos = slotSpec SlotDropPos fieldPoint

-- Windows, menus and tooltips ------------------------------------------------

{-# INLINE slotWinSize #-}
slotWinSize :: SlotSpec V2
slotWinSize = slotSpec SlotWinSize fieldPoint

{-# INLINE slotMenuOpen #-}
slotMenuOpen :: SlotSpec Bool
slotMenuOpen = flagSpec (slotSpec SlotMenuOpen fieldInt)

{-# INLINE slotMenuPos #-}
slotMenuPos :: SlotSpec V2
slotMenuPos = slotSpec SlotMenuPos fieldPoint

-- | When a tooltip opens, in monotonic microseconds, 0 while its target is
-- not hovered. Key 0 holds when any tooltip was last open. Bookkeeping, so
-- it repaints nothing.
{-# INLINE slotTooltipShow #-}
slotTooltipShow :: SlotSpec Int
slotTooltipShow = slotSpec SlotTooltipShow fieldQuiet

-- Scrolling ------------------------------------------------------------------

-- | A 2D scroller's offset on the axis its own key does not hold.
{-# INLINE slotScrollCross #-}
slotScrollCross :: SlotSpec Float
slotScrollCross = slotSpec SlotScrollCross fieldFloat

{-# INLINE slotScrollLinkX #-}
slotScrollLinkX :: SlotSpec Int
slotScrollLinkX = slotSpec SlotScrollLinkX fieldInt

{-# INLINE slotScrollLinkY #-}
slotScrollLinkY :: SlotSpec Int
slotScrollLinkY = slotSpec SlotScrollLinkY fieldInt

{-# INLINE slotScrollStep #-}
slotScrollStep :: SlotSpec Float
slotScrollStep = slotSpec SlotScrollStep fieldFloat

{-# INLINE slotScrollAxes #-}
slotScrollAxes :: SlotSpec Int
slotScrollAxes = slotSpec SlotScrollAxes fieldInt

{-# INLINE slotScrollRange #-}
slotScrollRange :: SlotSpec V2
slotScrollRange = slotSpec SlotScrollRange fieldPoint

-- | How far a scrolling tab strip's headers can scroll, from its last
-- layout.
{-# INLINE slotScrollContent #-}
slotScrollContent :: SlotSpec Float
slotScrollContent = slotSpec SlotScrollContent fieldFloat

-- | An offset in window axes set before the scroller's first layout.
{-# INLINE slotScrollPending #-}
slotScrollPending :: SlotSpec V2
slotScrollPending = slotSpec SlotScrollPending fieldPoint

-- Other widgets --------------------------------------------------------------

-- | A colour picker's opening colour, packed.
{-# INLINE slotColorBase #-}
slotColorBase :: SlotSpec Int
slotColorBase = slotSpec SlotColorBase fieldInt

-- | The stepper arrow a numeric field's press holds: 1 up, -1 down.
{-# INLINE slotNumericHeld #-}
slotNumericHeld :: SlotSpec Int
slotNumericHeld = slotSpec SlotNumericHeld fieldInt

-- | When a numeric field's held arrow next repeats, in monotonic seconds.
{-# INLINE slotNumericRepeat #-}
slotNumericRepeat :: SlotSpec Double
slotNumericRepeat = slotSpec SlotNumericRepeat fieldDouble
