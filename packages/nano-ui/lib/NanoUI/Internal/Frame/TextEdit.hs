-- | Editing text fields from outside their frame: the commands an app or the
-- context menu runs on a field by its id, and the context menu itself (Undo /
-- Redo / Cut / Copy / Paste / Select All): opening it, its popup, and
-- closing it.
module NanoUI.Internal.Frame.TextEdit
  ( applyTextFieldCommand
  , textFieldEditor
  , openTextEditMenu
  , textEditMenuView
  , closeTextEditMenuOnOutsideClick
  , closeTextEditMenuOnEscape
  , textFieldWidgetAtMouse
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Internal.Context
import NanoUI.Internal.Frame.Hit (findNodeByWidgetId, reachedWidgetAt, widgetOverlayAllowed)
import NanoUI.Internal.Frame.TextArea (collapseTextFieldSelection, isMouseOnTextAreaScrollBarAt)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Input
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Monad (NanoUI, askContext, ifM, liftIO, themed, whenM, withKey, (<&&>))
import NanoUI.Internal.Store (insertSpec, lookupSpec)
import NanoUI.Internal.Store.Slots (slotTextChanged, slotTextMode)
import NanoUI.Internal.Style (gap, tight)
import NanoUI.Internal.Types (DamageBounds (..), PopupAnchor (..), PopupPlacement (..), Rect (..), V2 (..), rectContains)
import NanoUI.Internal.Var.Types (constVar)
import NanoUI.Internal.Widgets.Layout (columnWith)
import NanoUI.Internal.Widgets.Menu (menuItem, menuItemDisabled, menuSeparator)
import NanoUI.Internal.Widgets.Node (respClicked, respRect)
import NanoUI.Internal.Widgets.Popup (PopupConfig (..), defaultPopupConfig, popupWith')
import NanoUI.Internal.Widgets.TextArea (textAreaFieldEditor)
import NanoUI.Internal.Widgets.TextDocument (sameLines)
import NanoUI.Internal.Widgets.TextInput (textInputFieldEditor, textInputMode)
import NanoUI.Widgets.TextBuffer qualified as TB
import NanoUI.Widgets.TextEditor

-- | Run a command on the field with this id and focus it: the command comes
-- from a menu or button that may not be over the field, and the caret,
-- selection highlight and next keystroke belong to the field it edited.
-- Focus moves at the end of the frame, as with
-- 'NanoUI.Internal.Monad.requestFocus', so disabled fields and fields behind a
-- modal cannot take focus. A change to the text pulses @respChanged@ on the
-- field's next frame.
applyTextFieldCommand :: Context -> WidgetId -> TextCommand -> IO ()
applyTextFieldCommand ctx wid cmd =
  textFieldEditor ctx wid >>= mapM_ (\(mode, ed0, save) -> do
    ed <- runCommandIO ctx mode cmd ed0 {editorHistory = sealHistory (editorHistory ed0)}
    let edited = not (sameLines (TB.bufferLines (editorBuffer ed)) (TB.bufferLines (editorBuffer ed0)))
        pulse = if edited then insertSpec slotTextChanged (intKey wid) 1 else id
    modifyStore ctx (pulse . save ed)
    -- Store damage is keyed on slots, not the widget: damage the widget so a
    -- selection-only command (Select All) repaints this frame.
    damageWidget ctx wid DamageSelf
    markDirty ctx
    writeIORef (ctxFocusRequest ctx) (Just (FocusOn wid))
    modifyInteraction ctx (\s -> s {isTextInputMenu = Nothing}))

-- | Look up a text field's 'EditorMode', stored 'Editor', and store setter by
-- 'WidgetId' for out-of-frame commands. Uses the current frame's arena node if
-- present, or falls back to the mode recorded when the field was last
-- declared.
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
      Nothing -> pure (lookupSpec slotTextMode key store)
  pure $ flip fmap mMode $ \mode ->
    let (ed, save) = (if modeMultiLine mode then textAreaFieldEditor else textInputFieldEditor) store key
     in (mode, ed, save)

-- | The menu's rows in order: a command and its label, or a separator.
textEditMenuRows :: [Maybe (TextCommand, T.Text)]
textEditMenuRows =
  [ Just (Undo, "Undo")
  , Just (Redo, "Redo")
  , Nothing
  , Just (Cut, "Cut")
  , Just (Copy, "Copy")
  , Just (Paste, "Paste")
  , Nothing
  , Just (SelectAll, "Select All")
  ]

-- | Open the menu at the pointer over the text field a right press lands
-- on, and focus that field. 'textEditMenuView' shows it from the next view
-- pass on.
openTextEditMenu :: Context -> Input -> IO ()
openTextEditMenu ctx inp =
  when (pressedIn MouseRight inp) $ do
    let V2 mx my = inputMousePos inp
    mWid <- textFieldWidgetAtMouse ctx (inputMousePos inp)
    forM_ mWid $ \wid -> do
      prevFocus <- readIORef (ctxFocusId ctx)
      when (prevFocus /= wid) (collapseTextFieldSelection ctx prevFocus)
      writeIORef (ctxFocusId ctx) wid
      modifyInteraction ctx (\s -> s {isTextInputMenu = Just (TextInputMenu wid (V2 mx my) (Rect 0 0 0 0))})
      markDirty ctx

-- | The open text-edit menu as a popup at the press that opened it, in the
-- theme of its field, with a row per command; a command the field cannot run
-- now is a dimmed row. The frame declares it after the view, in a keyed scope
-- that moves no sibling id. Picking a row (a release on the row the press
-- went down on) runs the command on the field and records it
-- ('takeTextEditLastAction'). Escape or a press outside closes it. A menu
-- whose field a modal hides is not shown.
textEditMenuView :: NanoUI ()
textEditMenuView = do
  ctx <- askContext
  liftIO (getsInteraction ctx isTextInputMenu) >>= mapM_ (\(TextInputMenu wid at _) ->
    whenM (liftIO (widgetOverlayAllowed ctx wid)) $ do
      theme <- liftIO (widgetTheme ctx wid)
      let cfg = (defaultPopupConfig (AnchorPoint at)) {popupPlacement = PlacementAtCursor, popupOffset = 0}
      (resp, picked) <- withKey textEditMenuKey . themed theme . popupWith' (constVar True) cfg id $
        columnWith (tight . gap 0) $ fmap (listToMaybe . concat) . forM textEditMenuRows $ \case
          Nothing -> [] <$ menuSeparator
          Just (cmd, lbl) -> menuRow cmd lbl =<< liftIO (textFieldMenuEnabled ctx wid cmd)
      liftIO $ case picked of
        Just (Just cmd) -> do
          modifyInteraction ctx (\s -> s {isTextEditLastAction = Just (wid, cmd)})
          applyTextFieldCommand ctx wid cmd
        -- A dismissal (Escape, or a press outside) closes it. Otherwise it
        -- keeps the rect it was drawn at, which a press outside is judged by
        -- ('closeTextEditMenuOnOutsideClick').
        _
          | respClicked resp -> modifyInteraction ctx (\s -> s {isTextInputMenu = Nothing})
          | otherwise -> modifyInteraction ctx (\s -> s {isTextInputMenu = (\m -> m {textInputMenuRect = respRect resp}) <$> isTextInputMenu s}))
 where
  menuRow cmd lbl enabled
    | enabled = (\c -> [cmd | c]) <$> menuItem lbl
    | otherwise = [] <$ menuItemDisabled lbl

-- | The key of the scope 'textEditMenuView' declares its popup in.
textEditMenuKey :: Text
textEditMenuKey = "nano-ui text edit menu"


-- | The enabled text field or text area the pointer at @mouse@ is on, which
-- takes the text cursor and the right-click menu: the widget hover lands on
-- ('reachedWidgetAt'), when it is one. Excludes a field covered there by a
-- stack or pinned node and one whose inner control has the pointer.
textFieldWidgetAtMouse :: Context -> V2 -> IO (Maybe WidgetId)
textFieldWidgetAtMouse ctx@Context {ctxNodeArena = na} mouse =
  reachedWidgetAt ctx mouse >>= \case
    Nothing -> pure Nothing
    Just idx -> do
      nt <- getNodeType na idx
      wid <- getWidgetId na idx
      ifM
        ( pure (nt == NodeTextInput || nt == NodeTextArea)
            <&&> (not <$> isDisabled ctx wid)
            <&&> (if nt == NodeTextArea then not <$> isMouseOnTextAreaScrollBarAt ctx idx mouse else pure True)
        )
        (pure (Just wid))
        (pure Nothing)

-- | A press of any button anywhere but on the menu's popup, where the user
-- saw it ('textInputMenuRect'), closes it.
closeTextEditMenuOnOutsideClick :: Context -> Input -> IO ()
closeTextEditMenuOnOutsideClick ctx inp =
  when (anyButtonPressed inp) $
    getsInteraction ctx isTextInputMenu >>= mapM_ (\m ->
      unless (rectContains (textInputMenuRect m) (inputMousePos inp)) $
        modifyInteraction ctx (\s -> s {isTextInputMenu = Nothing}))

closeTextEditMenuOnEscape :: Context -> Input -> IO ()
closeTextEditMenuOnEscape ctx inp =
  when (pressedOnceIn KeyEscape inp) $
    whenM (isJust <$> getsInteraction ctx isTextInputMenu) $ do
      modifyInteraction ctx (\s -> s {isTextInputMenu = Nothing})
      markEscapeConsumed ctx
      markDirty ctx

-- | Whether @cmd@ can run on field @wid@ now. Paste does not look at the
-- clipboard: this runs on every paint and pointer move over the menu, a read
-- can be a round trip to the program that owns it, and pasting an empty
-- clipboard does nothing.
textFieldMenuEnabled :: Context -> WidgetId -> TextCommand -> IO Bool
textFieldMenuEnabled ctx wid cmd =
  textFieldEditor ctx wid >>= \case
    Nothing -> pure False
    Just (mode, Editor buf _ history, _) -> case cmd of
      Undo -> pure (modeEditable mode && canUndo history)
      Redo -> pure (modeEditable mode && canRedo history)
      Cut -> pure (modeEditable mode && modeCopyable mode && hasText)
      Copy -> pure (modeCopyable mode && hasText)
      Paste -> pure (modeEditable mode)
      _ -> pure hasText
     where
      hasText = TB.getLineCount buf > 1 || not (T.null (TB.lineAt 0 buf))
