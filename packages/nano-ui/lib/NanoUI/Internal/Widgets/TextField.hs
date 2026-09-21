-- | Implementation of "NanoUI.Widgets.TextField", plus the context-level
-- operations the text-field context menu runs.
module NanoUI.Internal.Widgets.TextField
  ( runTextCommand
  , textCanUndo
  , textCanRedo
  , applyTextFieldCommand
  , textFieldMode
  , textFieldHistory
  , textFieldHasText
  ) where

import Data.Dynamic (fromDynamic)
import Data.IORef (writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context (Context (..), getStore, intKey, setTextInputMenu)
import NanoUI.Internal.Frame.Hit (findNodeByWidgetId)
import NanoUI.Internal.Frame.TextArea.Content (textAreaBuffer)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Layout.Arena (NodeType (..), getNodeType, getStyleIdx)
import NanoUI.Internal.Monad (Ui, withContext)
import NanoUI.Internal.Store (Slot (..), fieldDyn, fieldInt, fieldText, findSlot, lookupSlot, slotKey)
import NanoUI.Internal.Widgets.TextArea (applyTextAreaCommand)
import NanoUI.Widgets.TextBuffer qualified as TB
import NanoUI.Internal.Widgets.TextEditor
  ( EditHistory
  , EditorMode (..)
  , TextCommand
  , canRedo
  , canUndo
  , editorModeFromCode
  , emptyHistory
  , multiLineMode
  )
import NanoUI.Internal.Widgets.TextInput (applyTextInputCommand, textInputMode)

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
textCanUndo wid = withContext (\ctx -> canUndo <$> textFieldHistory ctx wid)

-- | Whether 'NanoUI.Widgets.TextCommand.Redo' would change the field, for
-- enabling a menu item.
textCanRedo :: Ui :> es => WidgetId -> Eff es Bool
textCanRedo wid = withContext (\ctx -> canRedo <$> textFieldHistory ctx wid)

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
      pure (lookupSlot fieldInt (slotKey SlotTextMode (intKey wid)) store >>= editorModeFromCode)

-- | The undo history of the field with this id, empty when it has none. A
-- text input's is recorded with its text; a text area drops its history
-- when its document is replaced, so it keeps the history alone.
textFieldHistory :: Context -> WidgetId -> IO EditHistory
textFieldHistory ctx wid = do
  store <- getStore ctx
  let key = intKey wid
      stored = lookupSlot fieldDyn (slotKey SlotTextHistory key) store
      text = findSlot fieldText "" key store
  pure $ case stored of
    Just dyn
      | Just h <- fromDynamic dyn -> h
      | Just (recorded, h) <- fromDynamic dyn, recorded == (text :: Text) -> h
    _ -> emptyHistory

-- | Whether the field with this id holds any text.
textFieldHasText :: Context -> WidgetId -> IO Bool
textFieldHasText ctx wid = do
  store <- getStore ctx
  mode <- textFieldMode ctx wid
  let key = intKey wid
  pure $ case mode of
    Just m | modeMultiLine m ->
      let buf = textAreaBuffer store key
       in TB.getLineCount buf > 1 || not (T.null (TB.lineAt 0 buf))
    _ -> not (T.null (findSlot fieldText "" key store))
