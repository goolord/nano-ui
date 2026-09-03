module NanoUI.Widgets.TextEdit
  ( applyTextFieldMenuAction
  , textFieldMenuActionEnabled
  , collapseTextFieldSelection
  , collapseTextInputSelection
  ) where

import Control.Monad (when)
import qualified Data.IntMap.Strict as IM
import NanoUI.Context (Context (..), getStore, intKey, setStore)
import NanoUI.Frame.Hit (findNodeByWidgetId)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Layout.Arena (NodeType (..), getNodeType)
import NanoUI.Store
  ( WidgetStore (..)
  , slotAnchor
  , slotCursor
  , slotKey
  , slotTextAreaCol
  , slotTextAreaRow
  )
import NanoUI.Widgets.TextArea
  ( TextAreaState (..)
  , applyTextAreaMenuAction
  , loadTextAreaState
  , saveTextAreaState
  , textAreaMenuActionEnabled
  )
import qualified NanoUI.Widgets.TextBuffer as TB
import NanoUI.Widgets.TextInput
  ( applyTextInputMenuAction
  , textInputMenuActionEnabled
  )

applyTextFieldMenuAction :: Context -> WidgetId -> Int -> IO ()
applyTextFieldMenuAction ctx wid item = do
  mIdx <- findNodeByWidgetId ctx wid
  case mIdx of
    Nothing -> pure ()
    Just idx -> do
      nt <- getNodeType (ctxNodeArena ctx) idx
      case nt of
        NodeTextInput -> applyTextInputMenuAction ctx wid item
        NodeTextArea -> applyTextAreaMenuAction ctx wid item
        _ -> pure ()

textFieldMenuActionEnabled :: Context -> WidgetId -> Int -> IO Bool
textFieldMenuActionEnabled ctx wid item = do
  mIdx <- findNodeByWidgetId ctx wid
  case mIdx of
    Nothing -> pure False
    Just idx -> do
      nt <- getNodeType (ctxNodeArena ctx) idx
      case nt of
        NodeTextInput -> textInputMenuActionEnabled ctx wid item
        NodeTextArea -> textAreaMenuActionEnabled ctx wid item
        _ -> pure False

collapseTextFieldSelection :: Context -> WidgetId -> IO ()
collapseTextFieldSelection ctx wid =
  when (hashWidgetId wid /= 0) $ do
    mIdx <- findNodeByWidgetId ctx wid
    case mIdx of
      Nothing -> pure ()
      Just idx -> do
        nt <- getNodeType (ctxNodeArena ctx) idx
        case nt of
          NodeTextInput -> collapseTextInputSelection ctx wid
          NodeTextArea -> collapseTextAreaSelection ctx wid
          _ -> pure ()

collapseTextInputSelection :: Context -> WidgetId -> IO ()
collapseTextInputSelection ctx wid = do
  store <- getStore ctx
  let key = intKey wid
      cur = IM.findWithDefault 0 (slotKey slotCursor key) (storeInt store)
  setStore ctx (store {storeInt = IM.insert (slotKey slotAnchor key) cur (storeInt store)})

collapseTextAreaSelection :: Context -> WidgetId -> IO ()
collapseTextAreaSelection ctx wid = do
  store <- getStore ctx
  let key = intKey wid
      text = IM.findWithDefault "" key (storeText store)
      row = IM.findWithDefault 0 (slotKey slotTextAreaRow key) (storeInt store)
      col = IM.findWithDefault 0 (slotKey slotTextAreaCol key) (storeInt store)
      state = loadTextAreaState store key text
      state' = state {selectionAnchor = TB.Cursor row col}
  setStore ctx (saveTextAreaState key state' store)
