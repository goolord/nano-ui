-- | Text-field context menu (Undo / Redo / Cut / Copy / Paste / Select All):
-- opening, picking, painting, spans and cursor.
module NanoUI.Internal.Frame.TextEdit.Menu
  ( openTextEditMenu
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
  ( Context (..)
  , TextInputMenu (..)
  , PointerRoute (..)
  , getsInteraction
  , isDisabled
  , markDirty
  , markEscapeConsumed
  , widgetTheme
  , InteractionState (..)
  , modifyInteraction
  )
import NanoUI.Internal.Draw (pushRect, pushText)
import NanoUI.Internal.Font
  ( FontMetrics
  , centeredTextY
  , menuItemPadX
  , menuItemRowH
  , menuMinW
  , menuOuterPad
  , menuSepH
  , widgetContentInset
  )
import NanoUI.Internal.Frame.Chrome (overlayMenuStyle, paintMenuAccent, paintMenuPanel)
import NanoUI.Internal.Frame.Hit (nodeClippedHit, overlayHitAllowed, overlayHitRoot, widgetOverlayAllowed)
import NanoUI.Internal.Frame.TextArea.Content (isMouseOnTextAreaScrollBarAt)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Input
  ( Input (..)
  , Key (..)
  , UiCursorKind (..)
  , inputKeys
  , inputKeysElem
  , inputMousePos
  , inputMousePressed
  , inputMouseRightPressed
  , inputWindowSize
  )
import NanoUI.Internal.Layout.Arena (NodeClass (PointerNodes), NodeType (NodeTextArea, NodeTextInput), findClassNodeRevM, getNodeRect, getNodeType, getWidgetId)
import NanoUI.Internal.Monad (ifM, whenM, (<&&>))
import NanoUI.Internal.Style (Style (..), Theme, themeSeparator)
import NanoUI.Internal.Types (Color (..), Rect (..), Size (..), V2 (..), clamp, lerpColor, rectContains)
import NanoUI.Internal.Widgets.TextEditor (Editor (..), EditorMode (..), TextCommand (..), canRedo, canUndo)
import NanoUI.Internal.Widgets.TextField (applyTextFieldCommand, textFieldEditor)
import NanoUI.Widgets.TextBuffer qualified as TB

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

-- | Where the menu's labels start.
textEditMenuLabelX :: FontMetrics -> Rect -> Float
textEditMenuLabelX fm (Rect mx _ _ _) = mx + menuOuterPad + menuItemPadX + fst (widgetContentInset fm)

textEditMenuItemFg :: Style -> Bool -> Color
textEditMenuItemFg style enabled =
  if enabled
    then styleFg style
    else lerpColor (styleFg style) (styleBg style) 0.55

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

textFieldWidgetAtMouse :: Context -> V2 -> IO (Maybe WidgetId)
textFieldWidgetAtMouse ctx mouse = do
  let na = ctxNodeArena ctx
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
  traverse (getWidgetId na) mIdx

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
  when (inputMousePressed inp || inputMouseRightPressed inp) $ do
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
textEditMenuCursorKind ctx inp = do
  mMenu <- getsInteraction ctx isTextInputMenu
  let mouse = inputMousePos inp
  case mMenu of
    Just (TextInputMenu wid menuRect)
      | rectContains menuRect mouse
      , Just cmd <- textEditMenuPick menuRect mouse -> do
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
    (row@(Rect _ ry _ rh), Just (cmd, lbl)) -> do
      enabled <- textFieldMenuEnabled ctx wid cmd
      when (enabled && rectContains row (inputMousePos inp)) $ do
        pushRect da row (styleHoverBg style)
        paintMenuAccent da theme row
      (_, th) <- ctxMeasureText ctx lbl
      pushText da fm (textEditMenuLabelX fm menuRect) (centeredTextY fm ry rh th) lbl (textEditMenuItemFg style enabled)

collectTextEditMenuSpans :: Context -> Input -> IO [(Rect, T.Text, Color, Color, Rect)]
collectTextEditMenuSpans ctx inp = withTextEditMenu ctx [] $ \wid menuRect theme -> do
  let fm = ctxFontMetrics ctx
      style = overlayMenuStyle theme
  sequence
    [ do
        enabled <- textFieldMenuEnabled ctx wid cmd
        (tw, th) <- ctxMeasureText ctx lbl
        let bg
              | enabled && rectContains row (inputMousePos inp) = styleHoverBg style
              | otherwise = styleBg style
            labelRect = Rect (textEditMenuLabelX fm menuRect) (centeredTextY fm ry rh th) tw th
        pure (labelRect, lbl, textEditMenuItemFg style enabled, bg, menuRect)
    | (row@(Rect _ ry _ rh), Just (cmd, lbl)) <- textEditMenuLayout menuRect
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
