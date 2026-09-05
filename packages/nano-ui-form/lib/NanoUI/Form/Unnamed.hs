{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NanoUI.Form.Unnamed
  ( inputText
  , inputPassword
  , inputTextArea
  , inputCheckbox
  , inputSlider
  , inputSelect
  , inputEnumSelect
  , errors
  , childErrors
  , withErrors
  , withChildErrors
  , withFieldErrors
  ) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Ditto.Backend (FormError)
import qualified Ditto.Generalized.Unnamed as Unnamed
import Ditto.Types (encodeFormId)
import NanoUI
  ( checkbox
  , columnWith
  , fillW
  , gap
  , respChanged
  , respClicked
  , select
  , slider
  , textInput
  , textInputPassword
  , textArea
  , tight
  , uiIO
  , withKey
  )
import NanoUI.Monad (askContext)
import NanoUI.Form.Backend (getActiveFormPrefix, updateFieldInput)
import NanoUI.Form.Types
  ( Form
  , FormInput (..)
  , FormView (..)
  , formInputToText
  )
import NanoUI.Form.Widgets (defaultErrorView)
import Text.Read (readMaybe)

-- | Auto-enumerated text input.
inputText :: FormError FormInput err => Text -> Form err Text
inputText initial =
  Unnamed.input
    (\case
      FormInputText t -> Right t
      other           -> Right (formInputToText other)
    )
    (\formId val -> FormView $ withKey (encodeFormId formId) $ do
      ctx <- askContext
      prefix <- uiIO (getActiveFormPrefix ctx)
      let fieldKey = encodeFormId formId
      (resp, newVal) <- textInput "" val
      when (respChanged resp || newVal /= val) $
        uiIO (updateFieldInput ctx prefix fieldKey (FormInputText newVal))
    )
    initial

-- | Auto-enumerated password input.
inputPassword :: FormError FormInput err => Text -> Form err Text
inputPassword initial =
  Unnamed.input
    (\case
      FormInputText t -> Right t
      other           -> Right (formInputToText other)
    )
    (\formId val -> FormView $ withKey (encodeFormId formId) $ do
      ctx <- askContext
      prefix <- uiIO (getActiveFormPrefix ctx)
      let fieldKey = encodeFormId formId
      (resp, newVal) <- textInputPassword "" val
      when (respChanged resp || newVal /= val) $
        uiIO (updateFieldInput ctx prefix fieldKey (FormInputText newVal))
    )
    initial

-- | Auto-enumerated text area input.
inputTextArea :: FormError FormInput err => Text -> Form err Text
inputTextArea initial =
  Unnamed.input
    (\case
      FormInputText t -> Right t
      other           -> Right (formInputToText other)
    )
    (\formId val -> FormView $ withKey (encodeFormId formId) $ do
      ctx <- askContext
      prefix <- uiIO (getActiveFormPrefix ctx)
      let fieldKey = encodeFormId formId
      (resp, newVal) <- textArea "" val
      when (respChanged resp || newVal /= val) $
        uiIO (updateFieldInput ctx prefix fieldKey (FormInputText newVal))
    )
    initial

-- | Auto-enumerated checkbox toggle.
inputCheckbox :: FormError FormInput err => Text -> Bool -> Form err Bool
inputCheckbox lbl initial =
  Unnamed.input
    (\case
      FormInputBool b -> Right b
      FormInputText t -> Right (t == "true")
      _               -> Right initial
    )
    (\formId val -> FormView $ withKey (encodeFormId formId) $ do
      ctx <- askContext
      prefix <- uiIO (getActiveFormPrefix ctx)
      let fieldKey = encodeFormId formId
      (resp, newVal) <- checkbox lbl val
      when (respClicked resp || newVal /= val) $
        uiIO (updateFieldInput ctx prefix fieldKey (FormInputBool newVal))
    )
    initial

-- | Auto-enumerated slider input.
inputSlider :: FormError FormInput err => Float -> Float -> Float -> Form err Float
inputSlider minV maxV initial =
  Unnamed.input
    (\case
      FormInputFloat f -> Right f
      FormInputText t  -> maybe (Right initial) Right (readMaybe (T.unpack t))
      _                -> Right initial
    )
    (\formId val -> FormView $ withKey (encodeFormId formId) $ do
      ctx <- askContext
      prefix <- uiIO (getActiveFormPrefix ctx)
      let fieldKey = encodeFormId formId
      (resp, newVal) <- slider "" minV maxV val
      when (respChanged resp || newVal /= val) $
        uiIO (updateFieldInput ctx prefix fieldKey (FormInputFloat newVal))
    )
    initial

-- | Auto-enumerated select dropdown.
inputSelect :: FormError FormInput err => [Text] -> Int -> Form err Int
inputSelect options initial =
  Unnamed.input
    (\case
      FormInputInt i  -> Right i
      FormInputText t -> maybe (Right initial) Right (readMaybe (T.unpack t))
      _               -> Right initial
    )
    (\formId val -> FormView $ withKey (encodeFormId formId) $ do
      ctx <- askContext
      prefix <- uiIO (getActiveFormPrefix ctx)
      let fieldKey = encodeFormId formId
      (resp, newVal) <- select "" options val
      when (respChanged resp || newVal /= val) $
        uiIO (updateFieldInput ctx prefix fieldKey (FormInputInt newVal))
    )
    initial

-- | Auto-enumerated select for bounded enums.
inputEnumSelect :: forall a err. (Bounded a, Enum a, Show a, FormError FormInput err) => a -> Form err a
inputEnumSelect initial =
  let vs = [minBound .. maxBound] :: [a]
      opts = map (T.pack . show) vs
      toIdx a = fromEnum a
      fromIdx i = toEnum (max 0 (min (length vs - 1) i))
   in fmap fromIdx (inputSelect opts (toIdx initial))

-- | Render error messages originating directly from this form node.
errors :: ([err] -> FormView) -> Form err ()
errors = Unnamed.errors

-- | Render error messages originating from this form node and any descendant nodes.
childErrors :: ([err] -> FormView) -> Form err ()
childErrors = Unnamed.childErrors

-- | Wrap a form with a custom error handler for its direct errors.
withErrors :: (FormView -> [err] -> FormView) -> Form err a -> Form err a
withErrors = Unnamed.withErrors

-- | Wrap a form with a custom error handler for errors from it or any child.
withChildErrors :: (FormView -> [err] -> FormView) -> Form err a -> Form err a
withChildErrors = Unnamed.withChildErrors

-- | Automatically display validation errors directly below the widget.
withFieldErrors :: Form Text a -> Form Text a
withFieldErrors = withChildErrors (\(FormView widget) errs -> FormView $ do
  columnWith (tight . gap 4 . fillW) $ do
    widget
    case errs of
      [] -> pure ()
      es -> runFormView (defaultErrorView es)
  )
