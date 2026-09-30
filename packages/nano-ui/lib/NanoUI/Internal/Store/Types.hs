-- | Concrete compound values retained by the widget store. These types depend
-- only on pure models, so the store does not depend on widget implementations.
module NanoUI.Internal.Store.Types (GridState (..), Gesture (..), LineWidths (..), TextHistory (..)) where

import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Word (Word64)
import NanoUI.Internal.TextEditor.Types (EditHistory)
import NanoUI.Internal.Types (V2)
import NanoUI.Internal.Widgets.SplitPane (GridNode)

-- | A grid's state between frames, under its widget id.
data GridState = GridState
  { gsTree :: !(Maybe GridNode)
  -- ^ Nothing after the last pane closes; the next frame starts afresh.
  , gsSeed :: !Word64
  -- ^ Monotonic split/pane id, including across closure of the last pane.
  , gsFocus :: !Word64
  , gsMax :: !Word64
  , gsSpan :: !(Maybe (Float, Float))
  -- ^ Last fitted size, used to reflow pinned panes.
  , gsGesture :: !Gesture
  , gsGiven :: !(Maybe GridNode)
  -- ^ Tree passed by the caller last frame.
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

-- | Font size and metric generation, measured widths, widest line and width,
-- and content height. Incremental updates remeasure only edited lines.
data LineWidths = LineWidths !Float !Int !(Seq Float) !Int !Float !Float

-- | A history slot changes kind when a widget id changes editor kind.
data TextHistory = FieldHistory !Text !EditHistory | AreaHistory !EditHistory
