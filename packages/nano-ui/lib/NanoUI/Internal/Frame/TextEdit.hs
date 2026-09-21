-- | Text-field editing facade: single-line fields ("NanoUI.Internal.Frame.TextInput"),
-- text areas ("NanoUI.Internal.Frame.TextArea") and their context menu
-- ("NanoUI.Internal.Frame.TextEdit.Menu"), plus the dispatchers that pick between the
-- two field kinds.
module NanoUI.Internal.Frame.TextEdit
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
import NanoUI.Internal.Context
  ( Context (..)
  , InteractionState (..)
  , TextInputDrag (..)
  , getsInteraction
  , requestWakeAfter
  , setTextInputDrag
  )
import NanoUI.Internal.Frame.Hit (withWidgetNode)
import NanoUI.Internal.Frame.TextArea
import NanoUI.Internal.Frame.TextArea.Content (resolveTextAreaFont, textAreaContentMetrics)
import NanoUI.Internal.Frame.TextArea.Geometry
import NanoUI.Internal.Frame.TextEdit.Menu (applyTextFieldMenuAction, textEditMenuRectAt, textEditMenuWidth)
import NanoUI.Internal.Frame.TextInput
import NanoUI.Internal.Id (WidgetId, hashWidgetId)
import NanoUI.Internal.Input (Input, inputMouseDown, inputMousePos, inputMouseReleased)
import NanoUI.Internal.Layout.Arena (NodeType (NodeTextArea, NodeTextInput), getNodeRect, getNodeType)
import NanoUI.Internal.Types (rectContains)
import NanoUI.Internal.Widgets.TextCommon (textWordBounds)

-- | Mouse selection in the focused field, whichever kind it is. A release
-- ends any drag.
finalizeTextFieldMouse :: Context -> Input -> IO ()
finalizeTextFieldMouse ctx inp = do
  focus <- readIORef (ctxFocusId ctx)
  when (hashWidgetId focus /= 0) $ do
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
        withWidgetNode ctx focus () $ \idx -> do
          rect <- getNodeRect (ctxNodeArena ctx) idx
          unless (rectContains rect (inputMousePos inp)) $
            requestWakeAfter ctx (1 / 60)

-- | Collapse selection in a current single-line or multiline field. Zero,
-- missing, and non-text widget ids do nothing.
collapseTextFieldSelection :: Context -> WidgetId -> IO ()
collapseTextFieldSelection ctx wid =
  withWidgetNode ctx wid () $ \idx ->
    getNodeType (ctxNodeArena ctx) idx >>= \case
      NodeTextInput -> collapseTextInputSelection ctx wid
      NodeTextArea -> collapseTextAreaSelection ctx wid
      _ -> pure ()
