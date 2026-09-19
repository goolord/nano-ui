-- | Text-field editing facade: single-line fields ("NanoUI.Frame.TextInput"),
-- text areas ("NanoUI.Frame.TextArea") and their context menu
-- ("NanoUI.Frame.TextEdit.Menu"), plus the dispatchers that pick between the
-- two field kinds.
module NanoUI.Frame.TextEdit
  ( -- * Dispatch between field kinds
    finalizeTextFieldMouse
  , collapseTextFieldSelection
    -- * Context menu
  , applyTextFieldMenuAction
  , textEditMenuRectAt
  , textEditMenuWidth
    -- * Shared field helpers
  , normalizeTextFieldClicks
  , textWordBounds
    -- * Text areas
  , TextAreaHit (..)
  , TextAreaScrollBarLayouts (..)
  , resolveTextAreaFont
  , textAreaContentMetrics
  , textAreaBarLane
  , textAreaLineHeight
  , textAreaHitForWidget
  , textAreaScrollBarLayout
  , textAreaHScrollBarLayout
  , textAreaScrollBarLayouts
  ) where

import Control.Monad (forM_, unless, when)
import Data.IORef (readIORef)
import NanoUI.Context
  ( Context (..)
  , InteractionState (..)
  , TextInputDrag (..)
  , getMenuPointerGesture
  , getsInteraction
  , requestWakeAfter
  , setTextInputDrag
  )
import NanoUI.Frame.Hit (findNodeByWidgetId)
import NanoUI.Frame.TextArea
import NanoUI.Frame.TextArea.Content (resolveTextAreaFont, textAreaContentMetrics)
import NanoUI.Frame.TextArea.Geometry
import NanoUI.Frame.TextEdit.Menu (applyTextFieldMenuAction, textEditMenuRectAt, textEditMenuWidth)
import NanoUI.Frame.TextInput
import NanoUI.Id (WidgetId, hashWidgetId)
import NanoUI.Input (Input, inputMouseDown, inputMousePos, inputMouseReleased)
import NanoUI.Layout.Arena (NodeType (NodeTextArea, NodeTextInput), getNodeType, getRect)
import NanoUI.Types (Rect (..), rectContains)
import NanoUI.Widgets.TextCommon (textWordBounds)

-- | Mouse selection in the focused field, whichever kind it is. A release
-- ends any drag. A press on a menu (the field's own context menu included)
-- belongs to the menu: it must not also place the caret or start a drag
-- underneath, which the release would then apply over the menu's command.
finalizeTextFieldMouse :: Context -> Input -> IO ()
finalizeTextFieldMouse ctx inp = do
  focus <- readIORef (ctxFocusId ctx)
  menuGesture <- getMenuPointerGesture ctx
  when (hashWidgetId focus /= 0 && not menuGesture) $ do
    handled <- finalizeTextInputMouse ctx inp focus
    unless handled $ finalizeTextAreaMouse ctx inp focus
    keepDragScrolling ctx inp focus
  when (inputMouseReleased inp) $
    setTextInputDrag ctx Nothing

-- | A selection dragged past the field's edge scrolls a step a frame, as the
-- caret follows the pointer. A pointer held still out there sends no input to
-- run those frames, so ask for them while the drag lasts.
keepDragScrolling :: Context -> Input -> WidgetId -> IO ()
keepDragScrolling ctx inp focus =
  when (inputMouseDown inp) $ do
    mDrag <- getsInteraction ctx isTextInputDrag
    forM_ mDrag $ \drag ->
      when (textInputDragWidget drag == focus) $ do
        mIdx <- findNodeByWidgetId ctx focus
        forM_ mIdx $ \idx -> do
          (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
          unless (rectContains (Rect x y w h) (inputMousePos inp)) $
            requestWakeAfter ctx (1 / 60)

collapseTextFieldSelection :: Context -> WidgetId -> IO ()
collapseTextFieldSelection ctx wid =
  when (hashWidgetId wid /= 0) $ do
    mIdx <- findNodeByWidgetId ctx wid
    case mIdx of
      Nothing -> pure ()
      Just idx ->
        getNodeType (ctxNodeArena ctx) idx >>= \case
          NodeTextInput -> collapseTextInputSelection ctx wid
          NodeTextArea -> collapseTextAreaSelection ctx wid
          _ -> pure ()
