-- | Text-field editing facade: single-line fields ("NanoUI.Internal.Frame.TextInput")
-- and text areas ("NanoUI.Internal.Frame.TextArea"), plus the dispatchers that
-- pick between the two field kinds.
module NanoUI.Internal.Frame.TextEdit
  ( -- * Dispatch between field kinds
    finalizeTextFieldMouse
  , collapseTextFieldSelection
    -- * Shared field helpers
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
  , intKey
  , modifyInteraction
  , modifyStore
  , requestWakeAfter
  , slotKey
  )
import NanoUI.Internal.Frame.Hit (withWidgetNode)
import NanoUI.Internal.Frame.TextArea
import NanoUI.Internal.Frame.TextArea.Content (resolveTextAreaFont, textAreaContentMetrics)
import NanoUI.Internal.Frame.TextArea.Geometry
import NanoUI.Internal.Frame.TextInput
import NanoUI.Internal.Id (WidgetId, hashWidgetId)
import NanoUI.Internal.Input (Input, inputMouseDown, inputMousePos, inputMousePressed, inputMouseReleased)
import NanoUI.Internal.Layout.Arena (NodeType (NodeTextArea, NodeTextInput), getNodeRect, getNodeType)
import NanoUI.Internal.Store (Slot (..), fieldInt, findSlot, insertSlot)
import NanoUI.Internal.Types (rectContains)
import qualified NanoUI.Internal.Widgets.TextArea as TA
import qualified NanoUI.Widgets.TextBuffer as TB

-- | Mouse selection in the focused field, whichever kind it is. A release
-- ends any drag.
finalizeTextFieldMouse :: Context -> Input -> IO ()
finalizeTextFieldMouse ctx inp = do
  focus <- readIORef (ctxFocusId ctx)
  when (hashWidgetId focus /= 0) $
    withWidgetNode ctx focus () $ \idx -> do
      getNodeType (ctxNodeArena ctx) idx >>= \case
        NodeTextInput -> textInputMouse ctx inp focus idx
        -- An edited document is measured here, not in paint, whose cache
        -- write would damage and wake the next frame. Nothing else is read
        -- until the pointer acts.
        NodeTextArea -> do
          _ <- textAreaContentMetrics ctx idx
          when (inputMousePressed inp || inputMouseDown inp || inputMouseReleased inp) $
            textAreaMouse ctx inp focus idx
        _ -> pure ()
      -- A selection dragged past the field's edge scrolls a step a frame, as
      -- the caret follows the pointer. A pointer held still out there sends
      -- no input to run those frames, so ask for them while the drag lasts.
      when (inputMouseDown inp) $ do
        mDrag <- getsInteraction ctx isTextInputDrag
        forM_ mDrag $ \drag ->
          when (textInputDragWidget drag == focus) $ do
            rect <- getNodeRect (ctxNodeArena ctx) idx
            unless (rectContains rect (inputMousePos inp)) $
              requestWakeAfter ctx (1 / 60)
  when (inputMouseReleased inp) $
    modifyInteraction ctx (\s -> s {isTextInputDrag = Nothing})

-- | Collapse selection in a current single-line or multiline field onto its
-- cursor. Zero, missing, and non-text widget ids do nothing.
collapseTextFieldSelection :: Context -> WidgetId -> IO ()
collapseTextFieldSelection ctx wid =
  withWidgetNode ctx wid () $ \idx ->
    getNodeType (ctxNodeArena ctx) idx >>= \case
      NodeTextInput -> modifyStore ctx $ \store ->
        insertSlot fieldInt (slotKey SlotAnchor key) (findSlot fieldInt 0 (slotKey SlotCursor key) store) store
      NodeTextArea -> modifyStore ctx $ \store ->
        let state = TA.loadTextAreaState store key
         in TA.saveTextAreaState key state {TA.selectionAnchor = TB.getCursor (TA.buffer state)} store
      _ -> pure ()
 where
  key = intKey wid
