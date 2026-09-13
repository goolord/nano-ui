{-# LANGUAGE OverloadedStrings #-}

module NanoUI.State
  ( -- * Local State Hooks
    useState
  , useFlag
  , useInt
  , useFloat
  , useEnum
  , useText
  , useToggle
  , useTableSort

    -- * Controlled Widgets
  , checkboxControlled
  , textInputControlled
  , sliderControlled

    -- * Reducer / Elm Architecture Emitters
  , buttonEmit
  , checkboxEmit
  , sliderEmit
  , selectEmit
  , textInputEmit
  , searchFieldEmit
  )
where

import Control.Monad (when)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context
  , intKey
  , writeStoreFloat
  , writeStoreInt
  , writeStoreText
  )
import NanoUI.Hooks
  ( useEnum
  , useFlag
  , useFloat
  , useInt
  , useState
  , useText
  , useToggle
  )
import NanoUI.Id (WidgetId)
import NanoUI.Monad (Ui, askContext, currentId, emit, uiIO)
import NanoUI.Store (boolInt)
import NanoUI.Widgets
  ( button'
  , checkbox
  , respChanged
  , respClicked
  , searchField
  , select
  , slider
  , textInput
  )
import NanoUI.Widgets.Node (Response)
import NanoUI.Widgets.Table (useTableSort)

-- | Controlled checkbox driven by caller-owned state.
checkboxControlled ::
  Ui :> es => Text -> Bool -> (Bool -> Eff es ()) -> Eff es Response
checkboxControlled txt isChecked onChange = do
  syncControlled writeStoreInt (boolInt isChecked)
  notifyChanged (checkbox txt isChecked) onChange

-- | Controlled single-line text input driven by caller-owned state.
textInputControlled ::
  Ui :> es => Text -> (Text -> Eff es ()) -> Eff es Response
textInputControlled currentText onChange = do
  syncControlled writeStoreText currentText
  notifyChanged (textInput currentText) onChange

-- | Controlled slider driven by caller-owned state.
sliderControlled ::
  Ui :> es => Float -> Float -> Float -> (Float -> Eff es ()) -> Eff es Response
sliderControlled minV maxV currentVal onChange = do
  syncControlled writeStoreFloat currentVal
  notifyChanged (slider minV maxV currentVal) onChange

-- Synchronize the slot the control will consume, without issuing an extra ID.
-- Targeted store writers handle equality, damage, and redraw notification.
syncControlled ::
  Ui :> es => (Context -> WidgetId -> Int -> a -> IO ()) -> a -> Eff es ()
syncControlled write value = do
  wid <- currentId
  ctx <- askContext
  uiIO (write ctx wid (intKey wid) value)

-- Value changes cover both pointer and keyboard interaction.
notifyChanged :: Monad m => m (Response, a) -> (a -> m ()) -> m Response
notifyChanged widget onChange = do
  (resp, value) <- widget
  when (respChanged resp) (onChange value)
  pure resp

-- | Button that emits a reducer message on click.
buttonEmit :: (Typeable msg, Ui :> es) => Text -> msg -> Eff es Response
buttonEmit txt msg = do
  resp <- button' txt
  when (respClicked resp) (emit msg)
  pure resp

-- | Uncontrolled checkbox that emits a reducer message when toggled.
checkboxEmit ::
  (Typeable msg, Ui :> es) => Text -> Bool -> (Bool -> msg) -> Eff es Response
checkboxEmit txt initial toMsg =
  notifyChanged (checkbox txt initial) (emit . toMsg)

-- | Uncontrolled slider that emits a reducer message on value change.
sliderEmit ::
  (Typeable msg, Ui :> es) =>
  Float -> Float -> Float -> (Float -> msg) -> Eff es Response
sliderEmit minV maxV initial toMsg =
  notifyChanged (slider minV maxV initial) (emit . toMsg)

-- | Uncontrolled dropdown select that emits a reducer message when selection changes.
selectEmit ::
  (Foldable f, Typeable msg, Ui :> es) => f Text -> Int -> (Int -> msg) -> Eff es Response
selectEmit opts initial toMsg =
  notifyChanged (select opts initial) (emit . toMsg)

-- | Uncontrolled text input that emits a reducer message when text changes.
textInputEmit ::
  (Typeable msg, Ui :> es) => Text -> (Text -> msg) -> Eff es Response
textInputEmit initial toMsg =
  notifyChanged (textInput initial) (emit . toMsg)

-- | Debounced search field that emits a reducer message with the query once the
-- text has settled (or immediately when cleared).
searchFieldEmit ::
  (Typeable msg, Ui :> es) => Text -> Text -> (Text -> msg) -> Eff es Response
searchFieldEmit lbl initial toMsg =
  notifyChanged (searchField lbl initial) (emit . toMsg)
