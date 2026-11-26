{-# OPTIONS_HADDOCK not-home #-}

-- | Concrete compound values retained by the widget store. These types depend
-- only on pure models, so the store does not depend on widget implementations.
module NanoUI.Internal.Store.Types
  ( ComboState (..)
  , sameComboVisual
  , TableState (..)
  , GridState (..)
  , Gesture (..)
  , LineWidths (..)
  , TextHistory (..)
  ) where

import Data.IntSet (IntSet)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Word (Word64)
import NanoUI.Internal.Equality (eqByPtr, ptrEq)
import NanoUI.Internal.GridNode (GridNode)
import NanoUI.Internal.TextEditor.Types (EditHistory)
import NanoUI.Internal.Types (V2)

-- | A combo's state between frames.
data ComboState = ComboState
  { csHighlight :: !Int
  -- ^ Highlighted row of the filtered list; -1 for none.
  , csWindow :: !Int
  -- ^ First visible row.
  , csScrollX :: !Float
  -- ^ How far the rows are scrolled left, in pixels, when the widest
  -- overflows the dropdown.
  , csContentW :: !Float
  -- ^ Widest matching row, measured while focused.
  , csDrag :: !Int
  -- ^ Scrollbar thumb drag: 0 none, 1 vertical, 2 horizontal.
  , csDragOff :: !Float
  -- ^ Pointer offset into the dragged thumb.
  , csWheelRem :: !Float
  -- ^ Rows the wheel moved short of a whole row, carried to the next
  -- frame, so a trackpad's small steps add up.
  , csCommitted :: !Text
  -- ^ Last committed value.
  , csLive :: !Text
  -- ^ Field text the combo last produced.
  , csFocused :: !Bool
  -- ^ The field held focus; the frame that finds it lost commits.
  , csNavPointer :: !(Maybe V2)
  -- ^ Pointer position when Up/Down last moved the highlight, cleared once
  -- the pointer moves or the wheel scrolls. Suppresses hover-highlighting
  -- while the pointer is stationary during keyboard navigation.
  }
  deriving (Eq, Show)

-- | Whether two combo states show the same dropdown and field state. Ignores
-- 'csWheelRem' and 'csNavPointer', which are quiet bookkeeping.
sameComboVisual :: ComboState -> ComboState -> Bool
sameComboVisual a b =
  ptrEq a b
    || ( csHighlight a == csHighlight b
           && csWindow a == csWindow b
           && csScrollX a == csScrollX b
           && csContentW a == csContentW b
           && csDrag a == csDrag b
           && csDragOff a == csDragOff b
           && eqByPtr (csCommitted a) (csCommitted b)
           && eqByPtr (csLive a) (csLive b)
           && csFocused a == csFocused b
       )

-- | A table's state between frames, persisted under the table's key.
data TableState = TableState
  { tsOrder :: ![Int]
  -- ^ Every source column in display order, hidden ones included.
  , tsWidths :: ![Float]
  -- ^ The width each source column was dragged to, 0 for none.
  , tsHidden :: !IntSet
  , tsResizeCol :: !Int
  -- ^ The column a header-edge drag resizes, or -1.
  , tsResizeX :: !Float
  -- ^ The pointer's x when that resize started.
  , tsResizeW :: !Float
  -- ^ The column's width when that resize started.
  }
  deriving (Eq)

-- | A grid's state between frames, under its widget id.
data GridState = GridState
  { gsTree :: !(Maybe GridNode)
  -- ^ Nothing is an empty workspace, including after the last pane closes.
  , gsSeed :: !Word64
  -- ^ Monotonic split/pane id, including across closure of the last pane.
  , gsFocus :: !Word64
  , gsMax :: !Word64
  , gsSpan :: !(Maybe (Float, Float))
  -- ^ Last fitted size, used to reflow pinned panes.
  , gsGesture :: !Gesture
  }
  deriving (Eq, Show)

-- | A press's gesture, retained until release.
data Gesture
  = NoGesture
  | -- | Split id, original ratio, and original pointer coordinate.
    Resize !Word64 !Float !Float
  | -- | Pane id, press position, crossed-threshold flag, and title.
    Drag !Word64 !V2 !Bool !Text
  deriving (Eq, Show)

-- | Font ('NanoUI.Internal.WidgetText.textNodeFontKey') and metric
-- generation, measured widths, widest line and width, and content height.
-- Incremental updates remeasure only edited lines.
data LineWidths = LineWidths !Int !Int !(Seq Float) !Int !Float !Float

-- | A history slot changes kind when a widget id changes editor kind.
data TextHistory = FieldHistory !Text !EditHistory | AreaHistory !EditHistory
