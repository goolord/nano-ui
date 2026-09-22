-- | Implementation of "NanoUI.Widgets.TextField", plus the context-level
-- operations the text-field context menu runs.
module NanoUI.Internal.Widgets.TextField
  ( runTextCommand
  , textCanUndo
  , textCanRedo
  , applyTextFieldCommand
  , textFieldEditor
  ) where

import Data.IORef (writeIORef)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
  ( Context (..)
  , InteractionState (..)
  , damageWidget
  , getStore
  , intKey
  , markDirty
  , modifyInteraction
  , modifyStore
  )
import NanoUI.Internal.Frame.Hit (findNodeByWidgetId)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Layout.Arena (NodeType (..), getNodeType, getStyleIdx)
import NanoUI.Internal.Monad (Ui, withContext)
import NanoUI.Internal.Store (Slot (..), WidgetStore, fieldInt, insertSlot, lookupDyn, slotKey)
import NanoUI.Internal.Types (DamageBounds (..))
import NanoUI.Internal.Widgets.TextArea (textAreaFieldEditor)
import NanoUI.Internal.Widgets.TextDocument (sameLines)
import NanoUI.Internal.Widgets.TextEditor
  ( EditHistory
  , Editor (..)
  , EditorMode (..)
  , TextCommand
  , canRedo
  , canUndo
  , emptyHistory
  , multiLineMode
  , runCommandIO
  , sealHistory
  )
import NanoUI.Internal.Widgets.TextInput (textInputFieldEditor, textInputMode)
import NanoUI.Widgets.TextBuffer qualified as TB

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

textFieldHistory :: Context -> WidgetId -> IO EditHistory
textFieldHistory ctx wid = maybe emptyHistory (\(_, ed, _) -> editorHistory ed) <$> textFieldEditor ctx wid

-- | Run a command on the field with this id and focus it: the command comes
-- from a menu or button that may not be over the field, and the caret,
-- selection highlight and next keystroke belong to the field it edited. A
-- change to the text pulses @respChanged@ on the field's next frame.
applyTextFieldCommand :: Context -> WidgetId -> TextCommand -> IO ()
applyTextFieldCommand ctx wid cmd =
  textFieldEditor ctx wid >>= mapM_ (\(mode, ed0, save) -> do
    ed <- runCommandIO ctx mode cmd ed0 {editorHistory = sealHistory (editorHistory ed0)}
    let edited = not (sameLines (TB.bufferLines (editorBuffer ed)) (TB.bufferLines (editorBuffer ed0)))
        pulse = if edited then insertSlot fieldInt (slotKey SlotTextAreaChanged (intKey wid)) 1 else id
    modifyStore ctx (pulse . save ed)
    -- Store damage is keyed on slots, not the widget: damage the widget so a
    -- selection-only command (Select All) repaints this frame.
    damageWidget ctx wid DamageSelf
    markDirty ctx
    writeIORef (ctxFocusId ctx) wid
    modifyInteraction ctx (\s -> s {isTextInputMenu = Nothing}))

-- | The field with this id as a command from outside its frame sees it: how
-- it edits, its stored editor, and how to store an edited one. Its mode comes
-- from its node when it has one this frame, or from what it recorded the last
-- time it was declared.
textFieldEditor :: Context -> WidgetId -> IO (Maybe (EditorMode, Editor, Editor -> WidgetStore -> WidgetStore))
textFieldEditor ctx wid = do
  store <- getStore ctx
  let key = intKey wid
  mMode <-
    findNodeByWidgetId ctx wid >>= \case
      Just idx ->
        getNodeType (ctxNodeArena ctx) idx >>= \case
          NodeTextInput -> Just . textInputMode <$> getStyleIdx (ctxNodeArena ctx) idx
          NodeTextArea -> pure (Just multiLineMode)
          _ -> pure Nothing
      Nothing -> pure (lookupDyn (slotKey SlotTextMode key) store)
  pure $ flip fmap mMode $ \mode ->
    let (ed, save) = (if modeMultiLine mode then textAreaFieldEditor else textInputFieldEditor) store key
     in (mode, ed, save)
