-- | Editing text fields from outside their frame: the commands an app or the
-- context menu runs on a field by its id, and the context menu itself (Undo /
-- Redo / Cut / Copy / Paste / Select All): opening, picking, painting, spans
-- and cursor.
module NanoUI.Internal.Frame.TextEdit
  ( applyTextFieldCommand
  , textFieldEditor
  , openTextEditMenu
  , finalizeTextEditMenuPick
  , closeTextEditMenuOnOutsideClick
  , closeTextEditMenuOnEscape
  , drawTextEditMenuOverlays
  , collectTextEditMenuSpans
  , textEditMenuCursorKind
  , textFieldWidgetAtMouse
  ) where

import Control.Monad (forM_, when)
import Data.IORef (writeIORef)
import Data.Maybe (catMaybes, isJust, listToMaybe)
import qualified Data.Text as T
import NanoUI.Internal.Context
import NanoUI.Internal.Draw (pushRect, pushText)
import NanoUI.Internal.Font
import NanoUI.Internal.Frame.Chrome (overlayMenuStyle, paintMenuAccent, paintMenuPanel)
import NanoUI.Internal.Frame.Hit (findNodeByWidgetId, nodeClippedHit, overlayHitAllowed, overlayHitRoot, reachedWidgetAt, widgetOverlayAllowed)
import NanoUI.Internal.Frame.TextArea (isMouseOnTextAreaScrollBarAt)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Input
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Monad (ifM, whenM, (<&&>))
import NanoUI.Internal.Store (fieldInt, insertSlot, lookupDyn)
import NanoUI.Internal.Style (Style (..), Theme, themeSeparator)
import NanoUI.Internal.Types (Color (..), DamageBounds (..), Rect (..), Size (..), V2 (..), clamp, lerpColor, rectContains)
import NanoUI.Internal.Widgets.TextArea (textAreaFieldEditor)
import NanoUI.Internal.Widgets.TextDocument (sameLines)
import NanoUI.Internal.Widgets.TextInput (textInputFieldEditor, textInputMode)
import NanoUI.Widgets.TextBuffer qualified as TB
import NanoUI.Widgets.TextEditor

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

-- | Row heights, the same row metrics as generic popup menus.
textEditMenuRowHeights :: [Float]
textEditMenuRowHeights = map (maybe menuSepH (const menuItemRowH)) textEditMenuRows

-- | Every row with its band spanning the full menu width.
textEditMenuLayout :: Rect -> [(Rect, Maybe (TextCommand, T.Text))]
textEditMenuLayout (Rect mx my mw _) =
  [ (Rect mx (my + menuOuterPad + relY) mw h, row)
  | (relY, h, row) <- zip3 (scanl (+) 0 textEditMenuRowHeights) textEditMenuRowHeights textEditMenuRows
  ]

-- | The command of the row under @mouse@, when that row is a command.
textEditMenuPick :: Rect -> V2 -> Maybe TextCommand
textEditMenuPick menuRect mouse =
  listToMaybe [cmd | (band, Just (cmd, _)) <- textEditMenuLayout menuRect, rectContains band mouse]

-- | A command row's label rect, whether the pointer is on it while the
-- command can run, and its text colour, dimmed while it cannot. Shared by the
-- painter and the spans.
textEditMenuRow :: Context -> Input -> WidgetId -> Rect -> Style -> Rect -> TextCommand -> T.Text -> IO (Rect, Bool, Color)
textEditMenuRow ctx inp wid (Rect mx _ _ _) style row@(Rect _ ry _ rh) cmd lbl = do
  enabled <- textFieldMenuEnabled ctx wid cmd
  (tw, th) <- ctxMeasureText ctx lbl
  let fm = ctxFontMetrics ctx
      labelX = mx + menuOuterPad + menuItemPadX + fst (widgetContentInset fm)
      fg = if enabled then styleFg style else lerpColor (styleFg style) (styleBg style) 0.55
  pure (Rect labelX (centeredTextY fm ry rh th) tw th, enabled && rectContains row (inputMousePos inp), fg)

-- | Open the menu at the pointer, kept inside the window, over the text field
-- a right press lands on, and focus that field.
openTextEditMenu :: Context -> Input -> IO ()
openTextEditMenu ctx inp =
  when (inputMouseRightPressed inp) $ do
    let mouse@(V2 mx my) = inputMousePos inp
    mWid <- textFieldWidgetAtMouse ctx mouse
    forM_ mWid $ \wid -> do
      writeIORef (ctxFocusId ctx) wid
      -- As wide as the widest label plus padding, and no narrower than any menu.
      labelWs <- mapM (fmap fst . ctxMeasureText ctx . snd) (catMaybes textEditMenuRows)
      let menuW = max menuMinW (maximum labelWs + 2 * menuItemPadX + 2 * menuOuterPad)
          menuH = 2 * menuOuterPad + sum textEditMenuRowHeights
          Size ww wh = inputWindowSize inp
          menuRect = Rect (clamp 0 (ww - menuW) mx) (clamp 0 (wh - menuH) my) menuW menuH
      modifyInteraction ctx (\s -> s {isTextInputMenu = Just (TextInputMenu wid menuRect)})
      markDirty ctx

-- | The enabled text field or text area the pointer at @mouse@ is on, which
-- takes the text cursor and the right-click menu. Not one under a widget a
-- stack or a pinned node draws over it there ('topmostHit'), nor one whose
-- control, drawn inside it, has the pointer ('innermostHit').
textFieldWidgetAtMouse :: Context -> V2 -> IO (Maybe WidgetId)
textFieldWidgetAtMouse ctx@Context {ctxNodeArena = na} mouse = do
  top <- overlayHitRoot ctx mouse
  mIdx <-
    findClassNodeRevM na PointerNodes $ \idx -> do
      nt <- getNodeType na idx
      pure (nt == NodeTextInput || nt == NodeTextArea) <&&> do
        wid <- getWidgetId na idx
        rect <- getNodeRect na idx
        (not <$> isDisabled ctx wid)
          <&&> nodeClippedHit ctx idx rect mouse
          <&&> overlayHitAllowed ctx top idx
          <&&> (if nt == NodeTextArea then not <$> isMouseOnTextAreaScrollBarAt ctx idx mouse else pure True)
  case mIdx of
    Nothing -> pure Nothing
    -- The widget the pointer lands on, as hover finds it.
    Just idx -> ifM ((== Just idx) <$> reachedWidgetAt ctx mouse) (Just <$> getWidgetId na idx) (pure Nothing)

-- | A press on a command row runs it when it can run, recorded for the caller
-- ('NanoUI.Internal.Context.takeTextEditLastAction'); a press elsewhere on the
-- menu closes it.
finalizeTextEditMenuPick :: Context -> Input -> IO ()
finalizeTextEditMenuPick ctx inp =
  when (inputMousePressed inp) $ do
    mMenu <- getsInteraction ctx isTextInputMenu
    case mMenu of
      Just (TextInputMenu wid menuRect)
        | rectContains menuRect (inputMousePos inp) ->
            case textEditMenuPick menuRect (inputMousePos inp) of
              Nothing -> closeMenu
              Just cmd ->
                ifM
                  (textFieldMenuEnabled ctx wid cmd)
                  ( do
                      modifyInteraction ctx (\s -> s {isTextEditLastAction = Just (wid, cmd)})
                      applyTextFieldCommand ctx wid cmd
                  )
                  (closeMenu >> markDirty ctx)
      _ -> pure ()
 where
  closeMenu = modifyInteraction ctx (\s -> s {isTextInputMenu = Nothing})

-- | A press anywhere but on the menu closes it. This watches the frame's
-- input: the press it waits for is by definition not the menu's own.
closeTextEditMenuOnOutsideClick :: Context -> Input -> IO ()
closeTextEditMenuOnOutsideClick ctx inp =
  when (inputMousePressed inp || inputMouseRightPressed inp || inputMouseMiddlePressed inp) $ do
    route <- getsInteraction ctx isPointerRoute
    when (route /= RouteTextMenu) $ modifyInteraction ctx (\s -> s {isTextInputMenu = Nothing})

closeTextEditMenuOnEscape :: Context -> Input -> IO ()
closeTextEditMenuOnEscape ctx inp =
  when (inputKeysElem KeyEscape (inputKeys inp)) $
    whenM (isJust <$> getsInteraction ctx isTextInputMenu) $ do
      modifyInteraction ctx (\s -> s {isTextInputMenu = Nothing})
      markEscapeConsumed ctx
      markDirty ctx

textEditMenuCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
textEditMenuCursorKind ctx inp =
  getsInteraction ctx isTextInputMenu >>= \case
    Just (TextInputMenu wid menuRect)
      | Just cmd <- textEditMenuPick menuRect (inputMousePos inp) -> do
          enabled <- textFieldMenuEnabled ctx wid cmd
          pure (Just (if enabled then UiCursorPointer else UiCursorDefault))
    _ -> pure Nothing

-- | Resolve the allowed menu once for either painting or complete span queries.
withTextEditMenu :: Context -> a -> (WidgetId -> Rect -> Theme -> IO a) -> IO a
withTextEditMenu ctx absent consume = getsInteraction ctx isTextInputMenu >>= \case
  Nothing -> pure absent
  Just (TextInputMenu wid menuRect) ->
    ifM
      (widgetOverlayAllowed ctx wid)
      (widgetTheme ctx wid >>= consume wid menuRect)
      (pure absent)

drawTextEditMenuOverlays :: Context -> Input -> IO ()
drawTextEditMenuOverlays ctx inp = withTextEditMenu ctx () $ \wid menuRect theme -> do
  let da = ctxDrawArena ctx
      fm = ctxFontMetrics ctx
      style = overlayMenuStyle theme
  paintMenuPanel da theme style menuRect
  forM_ (textEditMenuLayout menuRect) $ \case
    (Rect rx ry rw rh, Nothing) ->
      pushRect da (Rect (rx + menuItemPadX) (ry + rh / 2) (rw - 2 * menuItemPadX) 1) (themeSeparator theme)
    (row, Just (cmd, lbl)) -> do
      (Rect tx ty _ _, hovered, fg) <- textEditMenuRow ctx inp wid menuRect style row cmd lbl
      when hovered $ do
        pushRect da row (styleHoverBg style)
        paintMenuAccent da theme row
      pushText da fm tx ty lbl fg

collectTextEditMenuSpans :: Context -> Input -> IO [(Rect, T.Text, Color, Color, Rect)]
collectTextEditMenuSpans ctx inp = withTextEditMenu ctx [] $ \wid menuRect theme -> do
  let style = overlayMenuStyle theme
  sequence
    [ do
        (rect, hovered, fg) <- textEditMenuRow ctx inp wid menuRect style row cmd lbl
        pure (rect, lbl, fg, if hovered then styleHoverBg style else styleBg style, menuRect)
    | (row, Just (cmd, lbl)) <- textEditMenuLayout menuRect
    ]

-- | Whether @cmd@ can run on field @wid@ now.
textFieldMenuEnabled :: Context -> WidgetId -> TextCommand -> IO Bool
textFieldMenuEnabled ctx wid cmd =
  textFieldEditor ctx wid >>= \case
    Nothing -> pure False
    Just (mode, Editor buf _ history, _) -> case cmd of
      Undo -> pure (modeEditable mode && canUndo history)
      Redo -> pure (modeEditable mode && canRedo history)
      Cut -> pure (modeEditable mode && modeCopyable mode && hasText)
      Copy -> pure (modeCopyable mode && hasText)
      Paste
        | modeEditable mode -> maybe False (not . T.null) <$> ctxClipboardGet ctx
        | otherwise -> pure False
      _ -> pure hasText
     where
      hasText = TB.getLineCount buf > 1 || not (T.null (TB.lineAt 0 buf))
