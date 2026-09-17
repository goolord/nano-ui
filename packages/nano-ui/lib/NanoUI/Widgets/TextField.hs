-- | Commands run on a text field from outside its frame: an app's Edit menu,
-- a toolbar button, the field's own context menu.
module NanoUI.Widgets.TextField
  ( runTextCommand
  , textCanUndo
  , textCanRedo
  , applyTextFieldCommand
  , textFieldMode
  , textFieldHistory
  ) where

import Data.Dynamic (fromDynamic)
import Data.IORef (writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Context (Context (..), WidgetStore (..), getStore, intKey, setTextInputMenu)
import NanoUI.Frame.Hit (findNodeByWidgetId)
import NanoUI.Id (WidgetId)
import NanoUI.Layout.Arena (NodeType (..), getNodeType, getStyleIdx)
import NanoUI.Monad (Ui, askContext, uiIO)
import NanoUI.Store (slotKey, Slot (..))
import NanoUI.Widgets.TextArea (applyTextAreaCommand)
import NanoUI.Widgets.TextEditor
  ( EditHistory
  , EditorMode (..)
  , TextCommand
  , canRedo
  , canUndo
  , editorModeFromCode
  , emptyHistory
  , multiLineMode
  )
import NanoUI.Widgets.TextInput (applyTextInputCommand, textInputMode)

-- | Run a command on the text field (text input, search field, text area)
-- with this id, as if its keys were pressed: @runTextCommand (respId resp)
-- Undo@. The field takes keyboard focus, and its next frame returns the
-- changed text and a 'NanoUI.respChanged' pulse. An id that is not a text
-- field is ignored.
runTextCommand :: Ui :> es => WidgetId -> TextCommand -> Eff es ()
runTextCommand wid cmd = do
  ctx <- askContext
  uiIO (applyTextFieldCommand ctx wid cmd)

-- | Whether 'NanoUI.Widgets.TextCommand.Undo' would change the field, for
-- enabling a menu item.
textCanUndo :: Ui :> es => WidgetId -> Eff es Bool
textCanUndo wid = do
  ctx <- askContext
  uiIO (canUndo <$> textFieldHistory ctx wid)

-- | Whether 'NanoUI.Widgets.TextCommand.Redo' would change the field, for
-- enabling a menu item.
textCanRedo :: Ui :> es => WidgetId -> Eff es Bool
textCanRedo wid = do
  ctx <- askContext
  uiIO (canRedo <$> textFieldHistory ctx wid)

-- | Run a command on the field with this id and focus it: the command comes
-- from a menu or button that may not be over the field, and the caret,
-- selection highlight and next keystroke belong to the field it edited.
applyTextFieldCommand :: Context -> WidgetId -> TextCommand -> IO ()
applyTextFieldCommand ctx wid cmd =
  textFieldMode ctx wid >>= \case
    Just mode -> do
      if modeMultiLine mode
        then applyTextAreaCommand ctx wid cmd
        else applyTextInputCommand ctx wid mode cmd
      writeIORef (ctxFocusId ctx) wid
      setTextInputMenu ctx Nothing
    Nothing -> pure ()

-- | How the field with this id edits: from its node when it has one this
-- frame, or from what it recorded the last time it was declared.
textFieldMode :: Context -> WidgetId -> IO (Maybe EditorMode)
textFieldMode ctx wid =
  findNodeByWidgetId ctx wid >>= \case
    Just idx ->
      getNodeType (ctxNodeArena ctx) idx >>= \case
        NodeTextInput -> Just . textInputMode <$> getStyleIdx (ctxNodeArena ctx) idx
        NodeTextArea -> pure (Just multiLineMode)
        _ -> pure Nothing
    Nothing -> do
      store <- getStore ctx
      pure (IM.lookup (slotKey SlotTextMode (intKey wid)) (storeInt store) >>= editorModeFromCode)

-- | The undo history of the field with this id, empty when it has none.
textFieldHistory :: Context -> WidgetId -> IO EditHistory
textFieldHistory ctx wid = do
  store <- getStore ctx
  let key = intKey wid
      stored = IM.lookup (slotKey SlotTextHistory key) (storeDyn store)
      text = IM.findWithDefault "" key (storeText store)
  pure $ case stored >>= fromDynamic of
    Just (recorded, h) | recorded == (text :: Text) -> h
    _ -> emptyHistory
