-- | Commands run on a text field from outside its frame: an app's Edit menu,
-- a toolbar button, the field's own context menu.
module NanoUI.Widgets.TextField
  ( runTextCommand
  , textCanUndo
  , textCanRedo
  ) where

import NanoUI.Internal.Widgets.TextField
