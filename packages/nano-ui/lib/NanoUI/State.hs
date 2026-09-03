{-# LANGUAGE OverloadedStrings #-}

module NanoUI.State
  ( -- * Local State Hooks
    useState
  , useFlag
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
  ) where

import Control.Monad (when)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Effectful (Eff, type (:>))
import qualified Data.IntMap.Strict as IM
import NanoUI.Context (getStore, intKey, setStore)
import NanoUI.Monad (Ui, askContext, emit, nextId, uiIO)
import NanoUI.Store (WidgetStore (..), boolInt)
import NanoUI.Widgets
  ( button
  , checkbox
  , respChanged
  , respClicked
  , select
  , slider
  , textInput
  )
import NanoUI.Widgets.Animate (useFlag, useState, useText, useToggle)
import NanoUI.Widgets.Node (Response)
import NanoUI.Widgets.Table (useTableSort)

-- | Controlled checkbox driven by caller-owned state.
checkboxControlled :: Ui :> es => Text -> Bool -> (Bool -> Eff es ()) -> Eff es Response
checkboxControlled txt isChecked onChange = do
  wid <- nextId
  ctx <- askContext
  let key = intKey wid
  st0 <- uiIO (getStore ctx)
  when (IM.lookup key (storeInt st0) /= Just (boolInt isChecked)) $
    uiIO $ setStore ctx (st0 {storeInt = IM.insert key (boolInt isChecked) (storeInt st0)})
  (resp, newVal) <- checkbox txt isChecked
  when (respClicked resp) $ onChange newVal
  pure resp

-- | Controlled single-line text input driven by caller-owned state.
textInputControlled :: Ui :> es => Text -> Text -> (Text -> Eff es ()) -> Eff es Response
textInputControlled lbl currentText onChange = do
  wid <- nextId
  ctx <- askContext
  let key = intKey wid
  st0 <- uiIO (getStore ctx)
  when (IM.lookup key (storeText st0) /= Just currentText) $
    uiIO $ setStore ctx (st0 {storeText = IM.insert key currentText (storeText st0)})
  (resp, newVal) <- textInput lbl currentText
  when (respChanged resp) $ onChange newVal
  pure resp

-- | Controlled slider driven by caller-owned state.
sliderControlled :: Ui :> es => Text -> Float -> Float -> Float -> (Float -> Eff es ()) -> Eff es Response
sliderControlled lbl minV maxV currentVal onChange = do
  wid <- nextId
  ctx <- askContext
  let key = intKey wid
  st0 <- uiIO (getStore ctx)
  when (IM.lookup key (storeFloat st0) /= Just currentVal) $
    uiIO $ setStore ctx (st0 {storeFloat = IM.insert key currentVal (storeFloat st0)})
  (resp, newVal) <- slider lbl minV maxV currentVal
  when (respChanged resp) $ onChange newVal
  pure resp

-- | Button that emits a reducer message on click.
buttonEmit :: (Typeable msg, Ui :> es) => Text -> msg -> Eff es Response
buttonEmit txt msg = do
  resp <- button txt
  when (respClicked resp) (emit msg)
  pure resp

-- | Uncontrolled checkbox that emits a reducer message when toggled.
checkboxEmit :: (Typeable msg, Ui :> es) => Text -> Bool -> (Bool -> msg) -> Eff es Response
checkboxEmit txt initial toMsg = do
  (resp, newVal) <- checkbox txt initial
  when (respClicked resp) (emit (toMsg newVal))
  pure resp

-- | Uncontrolled slider that emits a reducer message on value change.
sliderEmit :: (Typeable msg, Ui :> es) => Text -> Float -> Float -> Float -> (Float -> msg) -> Eff es Response
sliderEmit lbl minV maxV initial toMsg = do
  (resp, newVal) <- slider lbl minV maxV initial
  when (respChanged resp) (emit (toMsg newVal))
  pure resp

-- | Uncontrolled dropdown select that emits a reducer message when selection changes.
selectEmit :: (Typeable msg, Ui :> es) => Text -> [Text] -> Int -> (Int -> msg) -> Eff es Response
selectEmit lbl opts initial toMsg = do
  (resp, newVal) <- select lbl opts initial
  when (respChanged resp) (emit (toMsg newVal))
  pure resp

-- | Uncontrolled text input that emits a reducer message when text changes.
textInputEmit :: (Typeable msg, Ui :> es) => Text -> Text -> (Text -> msg) -> Eff es Response
textInputEmit lbl initial toMsg = do
  (resp, newVal) <- textInput lbl initial
  when (respChanged resp) (emit (toMsg newVal))
  pure resp
