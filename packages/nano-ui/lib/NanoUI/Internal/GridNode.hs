-- | The split tree "NanoUI.Widgets.PaneGrid" lays its panes out by. It is
-- its own module, depending on nothing but @base@, so the widget store
-- ("NanoUI.Internal.Store.Types") can keep a grid's tree without depending
-- on the widget. "NanoUI.Internal.Widgets.SplitPane" re-exports it with
-- the functions over it.
module NanoUI.Internal.GridNode
  ( GridAxis (..)
  , GridNode (..)
  ) where

import Data.Word (Word64)

-- | Divider orientation. 'AxisV' draws a vertical divider (panes left/right),
-- 'AxisH' draws a horizontal divider (panes stacked top/bottom).
data GridAxis = AxisV | AxisH
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Binary split tree node. Pane and split ids share one monotonic counter.
data GridNode
  = Split
      !Word64
      -- ^ Split id.
      !GridAxis
      -- ^ Orientation of the divider.
      !Float
      -- ^ Ratio in @[0,1]@ for the A side.
      !GridNode
      -- ^ Left / top subtree.
      !GridNode
      -- ^ Right / bottom subtree.
  | Pane
      !Word64
      -- ^ Pane id.
  deriving (Eq, Show, Read)
