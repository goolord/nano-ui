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
  , TextAreaGeom (..)
  , TextAreaHit (..)
  , TextAreaScrollBarLayouts (..)
  , resolveTextAreaFont
  , textAreaContentMetrics
  , textAreaBarLanes
  , textAreaGeom
  , textAreaHitForWidget
  , textAreaScrollBarLayout
  , textAreaHScrollBarLayout
  , textAreaScrollBarLayouts
  ) where

import Control.Monad (unless, when)
import Data.IORef (readIORef)
import NanoUI.Context (Context (..), setTextInputDrag)
import NanoUI.Frame.Hit (findNodeByWidgetId)
import NanoUI.Frame.TextArea
import NanoUI.Frame.TextArea.Content (resolveTextAreaFont, textAreaContentMetrics)
import NanoUI.Frame.TextArea.Geometry
import NanoUI.Frame.TextEdit.Menu (applyTextFieldMenuAction, textEditMenuRectAt, textEditMenuWidth)
import NanoUI.Frame.TextInput
import NanoUI.Id (WidgetId, hashWidgetId)
import NanoUI.Input (Input, inputMouseReleased)
import NanoUI.Layout.Arena (NodeType (NodeTextArea, NodeTextInput), getNodeType)
import NanoUI.Widgets.TextCommon (textWordBounds)

-- | Mouse selection in the focused field, whichever kind it is. A release
-- ends any drag.
finalizeTextFieldMouse :: Context -> Input -> IO ()
finalizeTextFieldMouse ctx inp = do
  focus <- readIORef (ctxFocusId ctx)
  when (hashWidgetId focus /= 0) $ do
    handled <- finalizeTextInputMouse ctx inp focus
    unless handled $ finalizeTextAreaMouse ctx inp focus
  when (inputMouseReleased inp) $
    setTextInputDrag ctx Nothing

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
