-- | Commands run on a text field from outside its frame: an app's Edit menu,
-- a toolbar button, the field's own context menu.
module NanoUI.Widgets.TextField
  ( runTextCommand
  , textCanUndo
  , textCanRedo
  ) where

import Effectful (Eff, type (:>))
import NanoUI.Internal.Frame.TextEdit (applyTextFieldCommand, textFieldEditor)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Monad (Ui, withContext)
import NanoUI.Widgets.TextEditor (EditHistory, Editor (..), TextCommand, canRedo, canUndo)

-- | Run a command on the text field (text input, search field, text area)
-- with this id, as if its keys were pressed: @runTextCommand (respId resp)
-- Undo@. The field takes keyboard focus, and its next frame returns the
-- changed text and a 'NanoUI.respChanged' pulse. An id that is not a text
-- field is ignored.
runTextCommand :: Ui :> es => WidgetId -> TextCommand -> Eff es ()
runTextCommand wid cmd = withContext (\ctx -> applyTextFieldCommand ctx wid cmd)

-- | Whether 'NanoUI.Widgets.TextCommand.Undo' would change the field, for
-- enabling a menu item.
textCanUndo :: Ui :> es => WidgetId -> Eff es Bool
textCanUndo = fieldHistory canUndo

-- | Whether 'NanoUI.Widgets.TextCommand.Redo' would change the field, for
-- enabling a menu item.
textCanRedo :: Ui :> es => WidgetId -> Eff es Bool
textCanRedo = fieldHistory canRedo

fieldHistory :: Ui :> es => (EditHistory -> Bool) -> WidgetId -> Eff es Bool
fieldHistory test wid = withContext $ \ctx ->
  maybe False (\(_, ed, _) -> test (editorHistory ed)) <$> textFieldEditor ctx wid
