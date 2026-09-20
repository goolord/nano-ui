{-# LANGUAGE DataKinds #-}

-- | Text-field context menu (Cut / Copy / Paste / Select All): opening,
-- picking, painting, spans and cursor.
module NanoUI.Frame.TextEdit.Menu
  ( textEditMenuWidth
  , textEditMenuRectAt
  , openTextEditMenu
  , finalizeTextEditMenuPick
  , closeTextEditMenuOnOutsideClick
  , closeTextEditMenuOnEscape
  , drawTextEditMenuOverlays
  , collectTextEditMenuSpans
  , textEditMenuCursorKind
  , textFieldWidgetAtMouse
  , applyTextFieldMenuAction
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.IORef (writeIORef)
import qualified Data.Text as T
import NanoUI.Context
  ( Context (..)
  , TextInputMenu (..)
  , PointerRoute (..)
  , getPointerRoute
  , getTextInputMenu
  , isDisabled
  , markDirty
  , markEscapeConsumed
  , setTextInputMenu
  , widgetTheme
  , InteractionState (..)
  , modifyInteraction
  )
import NanoUI.Draw (pushRect, pushText)
import NanoUI.Font
  ( centeredTextY
  , menuItemPadX
  , menuItemRowH
  , menuMinW
  , menuOuterPad
  , menuSepH
  , widgetContentInset
  )
import NanoUI.Frame.Chrome (overlayMenuStyle, paintMenuAccent, paintMenuPanel)
import NanoUI.Frame.Hit (nodeClippedHit, overlayHitAllowed, widgetOverlayAllowed)
import NanoUI.Frame.TextArea.Content (isMouseOnTextAreaScrollBarAt)
import NanoUI.Id (WidgetId)
import NanoUI.Input
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
import NanoUI.Layout.Arena (NodeType (NodeTextArea, NodeTextInput), findNodeRevM, getNodeRect, getNodeType, getWidgetId)
import NanoUI.Monad ((<&&>))
import NanoUI.Style (Style (..), themeSeparator)
import NanoUI.Types (Color (..), Rect (..), Size (..), V2 (..), lerpColor, rectContains)
import NanoUI.Widgets.TextEditor (EditorMode (..), TextCommand (..), canRedo, canUndo)
import NanoUI.Widgets.TextField (applyTextFieldCommand, textFieldHasText, textFieldHistory, textFieldMode)

data TextEditMenuRow
  = TextEditMenuSep
  | TextEditMenuItem Int T.Text

-- | The menu's commands, in row order; a row's index is its item number.
textEditMenuCommands :: [TextCommand]
textEditMenuCommands = [Undo, Redo, Cut, Copy, Paste, SelectAll]

textEditMenuRows :: [TextEditMenuRow]
textEditMenuRows =
  [ TextEditMenuItem 0 "Undo"
  , TextEditMenuItem 1 "Redo"
  , TextEditMenuSep
  , TextEditMenuItem 2 "Cut"
  , TextEditMenuItem 3 "Copy"
  , TextEditMenuItem 4 "Paste"
  , TextEditMenuSep
  , TextEditMenuItem 5 "Select All"
  ]

-- Use the same row metrics as generic popup menus.
textEditMenuRowH :: TextEditMenuRow -> Float
textEditMenuRowH = \case
  TextEditMenuSep -> menuSepH
  TextEditMenuItem {} -> menuItemRowH

textEditMenuContentH :: Float
textEditMenuContentH = sum (map textEditMenuRowH textEditMenuRows)

textEditMenuWidth :: Context -> IO Float
textEditMenuWidth ctx = do
  ws <- mapM (fmap fst . ctxMeasureText ctx) [lbl | TextEditMenuItem _ lbl <- textEditMenuRows]
  pure (max menuMinW (maximum ws + 2 * menuItemPadX + 2 * menuOuterPad))

-- | Menu rect at the pointer, kept inside the window.
textEditMenuRectAt :: Float -> Float -> Float -> Size -> Rect
textEditMenuRectAt x y menuW (Size ww wh) =
  let h = 2 * menuOuterPad + textEditMenuContentH
   in Rect (max 0 (min x (ww - menuW))) (max 0 (min y (wh - h))) menuW h

textEditMenuContentRect :: Rect -> Rect
textEditMenuContentRect (Rect x y w _) =
  Rect (x + menuOuterPad) (y + menuOuterPad) (w - 2 * menuOuterPad) textEditMenuContentH

-- | Every row with its band spanning the full menu width.
textEditMenuLayout :: Rect -> [(TextEditMenuRow, Rect)]
textEditMenuLayout menuRect@(Rect mx _ mw _) =
  let Rect _ top _ _ = textEditMenuContentRect menuRect
      go _ [] = []
      go relY (entry : rest) =
        let h = textEditMenuRowH entry
         in (entry, Rect mx (top + relY) mw h) : go (relY + h) rest
   in go 0 textEditMenuRows

textEditMenuPickAction :: Rect -> V2 -> Maybe Int
textEditMenuPickAction menuRect mouse@(V2 _ my) =
  let Rect _ top _ _ = textEditMenuContentRect menuRect
   in if my < top || my >= top + textEditMenuContentH
        then Nothing
        else
          case [entry | (entry, row) <- textEditMenuLayout menuRect, rectContainsY row] of
            TextEditMenuItem action _ : _ -> Just action
            _ -> Nothing
  where
    rectContainsY (Rect _ ry _ rh) = let V2 _ py = mouse in py >= ry && py < ry + rh

textEditMenuItemFg :: Style -> Bool -> Color
textEditMenuItemFg style enabled =
  if enabled
    then styleFg style
    else lerpColor (styleFg style) (styleBg style) 0.55

openTextEditMenu :: Context -> Input -> IO ()
openTextEditMenu ctx inp =
  when (inputMouseRightPressed inp) $ do
    let mouse@(V2 mx my) = inputMousePos inp
    mWid <- textFieldWidgetAtMouse ctx mouse
    case mWid of
      Nothing -> pure ()
      Just wid -> do
        writeIORef (ctxFocusId ctx) wid
        menuW <- textEditMenuWidth ctx
        let menuRect = textEditMenuRectAt mx my menuW (inputWindowSize inp)
        setTextInputMenu ctx (Just (TextInputMenu wid menuRect))
        markDirty ctx

textFieldWidgetAtMouse :: Context -> V2 -> IO (Maybe WidgetId)
textFieldWidgetAtMouse ctx mouse = do
  let na = ctxNodeArena ctx
  mIdx <-
    findNodeRevM na $ \idx -> do
      nt <- getNodeType na idx
      pure (nt == NodeTextInput || nt == NodeTextArea) <&&> do
        wid <- getWidgetId na idx
        rect <- getNodeRect na idx
        (not <$> isDisabled ctx wid)
          <&&> nodeClippedHit ctx idx rect mouse
          <&&> overlayHitAllowed ctx idx mouse
          <&&> (if nt == NodeTextArea then not <$> isMouseOnTextAreaScrollBarAt ctx idx mouse else pure True)
  traverse (getWidgetId na) mIdx

finalizeTextEditMenuPick :: Context -> Input -> IO ()
finalizeTextEditMenuPick ctx inp =
  when (inputMousePressed inp) $ do
    mMenu <- getTextInputMenu ctx
    case mMenu of
      Just menu
        | rectContains (textInputMenuRect menu) (inputMousePos inp) ->
            case textEditMenuPickAction (textInputMenuRect menu) (inputMousePos inp) of
              Nothing -> setTextInputMenu ctx Nothing
              Just action -> do
                enabled <- textFieldMenuActionEnabled ctx (textInputMenuWidget menu) action
                if enabled
                  then applyTextFieldMenuAction ctx (textInputMenuWidget menu) action
                  else do
                    setTextInputMenu ctx Nothing
                    markDirty ctx
      _ -> pure ()

-- | A press anywhere but on the menu closes it. This watches the frame's
-- input: the press it waits for is by definition not the menu's own.
closeTextEditMenuOnOutsideClick :: Context -> Input -> IO ()
closeTextEditMenuOnOutsideClick ctx inp =
  when (inputMousePressed inp || inputMouseRightPressed inp) $ do
    route <- getPointerRoute ctx
    when (route /= RouteTextMenu) $ setTextInputMenu ctx Nothing

closeTextEditMenuOnEscape :: Context -> Input -> IO ()
closeTextEditMenuOnEscape ctx inp =
  when (inputKeysElem KeyEscape (inputKeys inp)) $
    getTextInputMenu ctx >>= \case
      Nothing -> pure ()
      Just _ -> do
        setTextInputMenu ctx Nothing
        markEscapeConsumed ctx
        markDirty ctx

textEditMenuCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
textEditMenuCursorKind ctx inp = do
  mMenu <- getTextInputMenu ctx
  let mouse = inputMousePos inp
  case mMenu of
    Just menu
      | rectContains (textInputMenuRect menu) mouse
      , Just action <- textEditMenuPickAction (textInputMenuRect menu) mouse -> do
          enabled <- textFieldMenuActionEnabled ctx (textInputMenuWidget menu) action
          pure (Just (if enabled then UiCursorPointer else UiCursorDefault))
    _ -> pure Nothing

drawTextEditMenuOverlays :: Context -> Input -> IO ()
drawTextEditMenuOverlays ctx inp = do
  mMenu <- getTextInputMenu ctx
  forM_ mMenu $ \menu -> do
    let wid = textInputMenuWidget menu
    allow <- widgetOverlayAllowed ctx wid
    when allow $ do
      theme <- widgetTheme ctx wid
      let da = ctxDrawArena ctx
          fm = ctxFontMetrics ctx
          menuRect = textInputMenuRect menu
          style = overlayMenuStyle theme
          Rect contentX _ _ _ = textEditMenuContentRect menuRect
          labelX = contentX + menuItemPadX + fst (widgetContentInset fm)
      paintMenuPanel da theme style menuRect
      forM_ (textEditMenuLayout menuRect) $ \case
        (TextEditMenuSep, Rect rx ry rw rh) ->
          pushRect da (Rect (rx + menuItemPadX) (ry + rh / 2) (rw - 2 * menuItemPadX) 1) (themeSeparator theme)
        (TextEditMenuItem action lbl, row@(Rect _ ry _ rh)) -> do
          enabled <- textFieldMenuActionEnabled ctx wid action
          when (enabled && rectContains row (inputMousePos inp)) $ do
            pushRect da row (styleHoverBg style)
            paintMenuAccent da theme row
          unless (T.null lbl) $ do
            (_, th) <- ctxMeasureText ctx lbl
            pushText da fm labelX (centeredTextY fm ry rh th) lbl (textEditMenuItemFg style enabled)

collectTextEditMenuSpans :: Context -> Input -> IO [(Rect, T.Text, Color, Color, Rect)]
collectTextEditMenuSpans ctx inp = do
  mMenu <- getTextInputMenu ctx
  case mMenu of
    Nothing -> pure []
    Just menu -> do
      let wid = textInputMenuWidget menu
      allow <- widgetOverlayAllowed ctx wid
      if not allow
        then pure []
        else do
          theme <- widgetTheme ctx wid
          let fm = ctxFontMetrics ctx
              menuRect = textInputMenuRect menu
              style = overlayMenuStyle theme
              Rect contentX _ _ _ = textEditMenuContentRect menuRect
              labelX = contentX + menuItemPadX + fst (widgetContentInset fm)
          fmap concat . forM (textEditMenuLayout menuRect) $ \case
            (TextEditMenuSep, _) -> pure []
            (TextEditMenuItem action lbl, row@(Rect _ ry _ rh)) -> do
              enabled <- textFieldMenuActionEnabled ctx wid action
              (tw, th) <- ctxMeasureText ctx lbl
              let bg
                    | enabled && rectContains row (inputMousePos inp) = styleHoverBg style
                    | otherwise = styleBg style
              pure [(Rect labelX (centeredTextY fm ry rh th) tw th, lbl, textEditMenuItemFg style enabled, bg, menuRect)]

applyTextFieldMenuAction :: Context -> WidgetId -> Int -> IO ()
applyTextFieldMenuAction ctx wid item =
  forM_ (take 1 (drop item textEditMenuCommands)) $ \cmd -> do
    modifyInteraction ctx (\s -> s {isTextEditLastAction = Just (wid, cmd)})
    applyTextFieldCommand ctx wid cmd

textFieldMenuActionEnabled :: Context -> WidgetId -> Int -> IO Bool
textFieldMenuActionEnabled ctx wid item = do
  mMode <- textFieldMode ctx wid
  history <- textFieldHistory ctx wid
  hasText <- textFieldHasText ctx wid
  case (mMode, drop item textEditMenuCommands) of
    (Just mode, cmd : _) -> case cmd of
      Undo -> pure (modeEditable mode && canUndo history)
      Redo -> pure (modeEditable mode && canRedo history)
      Cut -> pure (modeEditable mode && modeCopyable mode && hasText)
      Copy -> pure (modeCopyable mode && hasText)
      Paste
        | modeEditable mode -> maybe False (not . T.null) <$> ctxClipboardGet ctx
        | otherwise -> pure False
      _ -> pure hasText
    _ -> pure False
