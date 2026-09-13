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
  )
where

import Data.Text (Text)
import Ditto.Backend (FormError)
import Ditto.Generalized.Unnamed qualified as Unnamed
import NanoUI
  ( NanoUI
  , Response
  , checkbox
  , respChanged
  , respClicked
  , select
  , slider
  , textArea
  , textInput
  , textInputPassword
  )
import NanoUI.Form.Field
  ( decodeBool
  , decodeFloatInput
  , decodeInt
  , enumField
  , fieldErrors
  , fieldView
  )
import NanoUI.Form.Types (Form, FormInput (..), FormView, formInputToText)

-- | Auto-enumerated text input.
inputText :: FormError FormInput err => Text -> Form err Text
inputText = textField textInput

-- | Auto-enumerated password input.
inputPassword :: FormError FormInput err => Text -> Form err Text
inputPassword = textField textInputPassword

-- | Auto-enumerated text area input.
inputTextArea :: FormError FormInput err => Text -> Form err Text
inputTextArea = textField textArea

textField ::
  FormError FormInput err =>
  (Text -> NanoUI (Response, Text)) -> Text -> Form err Text
textField widget =
  Unnamed.input
    (Right . formInputToText)
    (fieldView respChanged FormInputText widget)

-- | Auto-enumerated checkbox toggle.
inputCheckbox :: FormError FormInput err => Text -> Bool -> Form err Bool
inputCheckbox lbl initial =
  Unnamed.input
    (Right . decodeBool initial)
    (fieldView respClicked FormInputBool (checkbox lbl))
    initial

-- | Auto-enumerated slider input.
inputSlider ::
  FormError FormInput err => Float -> Float -> Float -> Form err Float
inputSlider minV maxV initial =
  Unnamed.input
    (Right . decodeFloatInput initial)
    (fieldView respChanged FormInputFloat (slider minV maxV))
    initial

-- | Auto-enumerated select dropdown.
inputSelect :: FormError FormInput err => [Text] -> Int -> Form err Int
inputSelect options initial =
  Unnamed.input
    (Right . decodeInt initial)
    (fieldView respChanged FormInputInt (select options))
    initial

-- | Auto-enumerated select for bounded enums.
inputEnumSelect ::
  forall a err.
  (Bounded a, Enum a, Show a, FormError FormInput err) => a -> Form err a
inputEnumSelect = enumField inputSelect

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
withFieldErrors = withChildErrors fieldErrors
