{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NanoUI.Form.Named
  ( inputText
  , inputTextWithPlaceholder
  , inputPassword
  , inputTextArea
  , inputCheckbox
  , inputSlider
  , inputSelect
  , inputEnumSelect
  , inputRadio
  , inputEnumRadio
  , inputColor
  , label
  , separator
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
import qualified Ditto.Core as Ditto
import qualified Ditto.Generalized.Named as Named
import Ditto.Types (encodeFormId)
import NanoUI
  ( Color
  , checkbox
  , colorPicker
  , colorPickerFromHex
  , colorPickerToHex
  , columnWith
  , fillW
  , gap
  , radioFieldset
  , respChanged
  , respClicked
  , select
  , slider
  , textInput
  , textInputPassword
  , textInputWithPlaceholder
  , textArea
  , tight
  , uiIO
  , withKey
  )
import qualified NanoUI as NUI
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

-- | Single-line text input field.
inputText :: FormError FormInput err => Text -> Text -> Form err Text
inputText name initial =
  Named.input
    name
    (\case
      FormInputText t -> Right t
      other           -> Right (formInputToText other)
    )
    (\formId val -> FormView $ withKey (encodeFormId formId) $ do
      ctx <- askContext
      prefix <- uiIO (getActiveFormPrefix ctx)
      let fieldKey = encodeFormId formId
      (resp, newVal) <- textInput name val
      when (respChanged resp || newVal /= val) $
        uiIO (updateFieldInput ctx prefix fieldKey (FormInputText newVal))
    )
    initial

-- | Single-line text input field with custom placeholder text.
inputTextWithPlaceholder :: FormError FormInput err => Text -> Text -> Text -> Form err Text
inputTextWithPlaceholder placeholder name initial =
  Named.input
    name
    (\case
      FormInputText t -> Right t
      other           -> Right (formInputToText other)
    )
    (\formId val -> FormView $ withKey (encodeFormId formId) $ do
      ctx <- askContext
      prefix <- uiIO (getActiveFormPrefix ctx)
      let fieldKey = encodeFormId formId
      (resp, newVal) <- textInputWithPlaceholder placeholder name val
      when (respChanged resp || newVal /= val) $
        uiIO (updateFieldInput ctx prefix fieldKey (FormInputText newVal))
    )
    initial

-- | Password text input masking entered characters.
inputPassword :: FormError FormInput err => Text -> Text -> Form err Text
inputPassword name initial =
  Named.input
    name
    (\case
      FormInputText t -> Right t
      other           -> Right (formInputToText other)
    )
    (\formId val -> FormView $ withKey (encodeFormId formId) $ do
      ctx <- askContext
      prefix <- uiIO (getActiveFormPrefix ctx)
      let fieldKey = encodeFormId formId
      (resp, newVal) <- textInputPassword name val
      when (respChanged resp || newVal /= val) $
        uiIO (updateFieldInput ctx prefix fieldKey (FormInputText newVal))
    )
    initial

-- | Multi-line text area input.
inputTextArea :: FormError FormInput err => Text -> Text -> Form err Text
inputTextArea name initial =
  Named.input
    name
    (\case
      FormInputText t -> Right t
      other           -> Right (formInputToText other)
    )
    (\formId val -> FormView $ withKey (encodeFormId formId) $ do
      ctx <- askContext
      prefix <- uiIO (getActiveFormPrefix ctx)
      let fieldKey = encodeFormId formId
      (resp, newVal) <- textArea name val
      when (respChanged resp || newVal /= val) $
        uiIO (updateFieldInput ctx prefix fieldKey (FormInputText newVal))
    )
    initial

-- | Checkbox toggle input.
inputCheckbox :: FormError FormInput err => Text -> Bool -> Form err Bool
inputCheckbox name initial =
  Named.input
    name
    (\case
      FormInputBool b -> Right b
      FormInputText t -> Right (t == "true")
      _               -> Right initial
    )
    (\formId val -> FormView $ withKey (encodeFormId formId) $ do
      ctx <- askContext
      prefix <- uiIO (getActiveFormPrefix ctx)
      let fieldKey = encodeFormId formId
      (resp, newVal) <- checkbox name val
      when (respClicked resp || newVal /= val) $
        uiIO (updateFieldInput ctx prefix fieldKey (FormInputBool newVal))
    )
    initial

-- | Floating-point slider input across the range @[minV, maxV]@.
inputSlider :: FormError FormInput err => Text -> Float -> Float -> Float -> Form err Float
inputSlider name minV maxV initial =
  Named.input
    name
    (\case
      FormInputFloat f -> Right f
      FormInputText t  -> maybe (Right initial) Right (readMaybe (T.unpack t))
      _                -> Right initial
    )
    (\formId val -> FormView $ withKey (encodeFormId formId) $ do
      ctx <- askContext
      prefix <- uiIO (getActiveFormPrefix ctx)
      let fieldKey = encodeFormId formId
      (resp, newVal) <- slider name minV maxV val
      when (respChanged resp || newVal /= val) $
        uiIO (updateFieldInput ctx prefix fieldKey (FormInputFloat newVal))
    )
    initial

-- | Dropdown selection among a list of text options (returns selected index).
inputSelect :: FormError FormInput err => Text -> [Text] -> Int -> Form err Int
inputSelect name options initial =
  Named.input
    name
    (\case
      FormInputInt i  -> Right i
      FormInputText t -> maybe (Right initial) Right (readMaybe (T.unpack t))
      _               -> Right initial
    )
    (\formId val -> FormView $ withKey (encodeFormId formId) $ do
      ctx <- askContext
      prefix <- uiIO (getActiveFormPrefix ctx)
      let fieldKey = encodeFormId formId
      (resp, newVal) <- select name options val
      when (respChanged resp || newVal /= val) $
        uiIO (updateFieldInput ctx prefix fieldKey (FormInputInt newVal))
    )
    initial

-- | Dropdown selection for any bounded enumeration type.
inputEnumSelect :: forall a err. (Bounded a, Enum a, Show a, FormError FormInput err) => Text -> a -> Form err a
inputEnumSelect name initial =
  let vs = [minBound .. maxBound] :: [a]
      opts = map (T.pack . show) vs
      toIdx a = fromEnum a
      fromIdx i = toEnum (max 0 (min (length vs - 1) i))
   in fmap fromIdx (inputSelect name opts (toIdx initial))

-- | Radio button group (returns selected index).
inputRadio :: FormError FormInput err => Text -> [Text] -> Int -> Form err Int
inputRadio name options initial =
  Named.input
    name
    (\case
      FormInputInt i -> Right i
      _              -> Right initial
    )
    (\formId val -> FormView $ withKey (encodeFormId formId) $ do
      ctx <- askContext
      prefix <- uiIO (getActiveFormPrefix ctx)
      let fieldKey = encodeFormId formId
      (resp, newVal) <- radioFieldset name options val
      when (respChanged resp || newVal /= val) $
        uiIO (updateFieldInput ctx prefix fieldKey (FormInputInt newVal))
    )
    initial

-- | Radio button group for any bounded enumeration type.
inputEnumRadio :: forall a err. (Bounded a, Enum a, Show a, FormError FormInput err) => Text -> a -> Form err a
inputEnumRadio name initial =
  let vs = [minBound .. maxBound] :: [a]
      opts = map (T.pack . show) vs
      toIdx a = fromEnum a
      fromIdx i = toEnum (max 0 (min (length vs - 1) i))
   in fmap fromIdx (inputRadio name opts (toIdx initial))

-- | Color picker input.
inputColor :: FormError FormInput err => Text -> Color -> Form err Color
inputColor name initial =
  Named.input
    name
    (\case
      FormInputText t -> maybe (Right initial) Right (colorPickerFromHex t)
      _               -> Right initial
    )
    (\formId val -> FormView $ withKey (encodeFormId formId) $ do
      ctx <- askContext
      prefix <- uiIO (getActiveFormPrefix ctx)
      let fieldKey = encodeFormId formId
      (resp, newVal) <- colorPicker name val
      when (respChanged resp || newVal /= val) $
        uiIO (updateFieldInput ctx prefix fieldKey (FormInputText (colorPickerToHex newVal)))
    )
    initial

-- | Static label inside a form.
label :: Text -> Form err ()
label txt = Ditto.view (FormView (NUI.label_ txt))

-- | Visual separator line inside a form.
separator :: Form err ()
separator = Ditto.view (FormView NUI.sep)

-- | Render error messages originating directly from this form node.
errors :: ([err] -> FormView) -> Form err ()
errors = Named.errors

-- | Render error messages originating from this form node and any descendant nodes.
childErrors :: ([err] -> FormView) -> Form err ()
childErrors = Named.childErrors

-- | Wrap a form with a custom error handler for its direct errors.
withErrors :: (FormView -> [err] -> FormView) -> Form err a -> Form err a
withErrors = Named.withErrors

-- | Wrap a form with a custom error handler for errors from it or any child.
withChildErrors :: (FormView -> [err] -> FormView) -> Form err a -> Form err a
withChildErrors = Named.withChildErrors

-- | Automatically display validation errors directly below the widget.
withFieldErrors :: Form Text a -> Form Text a
withFieldErrors = withChildErrors (\(FormView widget) errs -> FormView $ do
  columnWith (tight . gap 4 . fillW) $ do
    widget
    case errs of
      [] -> pure ()
      es -> runFormView (defaultErrorView es)
  )
