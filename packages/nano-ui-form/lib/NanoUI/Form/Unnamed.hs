-- | Form inputs without a label, with automatically numbered field names.
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
  , TextInputConfig (..)
  , checkbox'
  , defaultTextInputConfig
  , respChanged
  , respClicked
  , select'
  , slider'
  , textArea'
  , textInput'
  , textInputConfigured'
  )
import NanoUI.Form.Backend (FormInput (..), formInputToText)
import NanoUI.Form.Field
  ( decodeBool
  , decodeFloatInput
  , decodeInt
  , enumField
  , fieldView
  )
import NanoUI.Form.Named
  ( childErrors
  , errors
  , withChildErrors
  , withErrors
  , withFieldErrors
  )
import NanoUI.Form.Types (Form)

-- | Auto-enumerated text input.
inputText :: FormError FormInput err => Text -> Form err Text
inputText = textField textInput'

-- | Auto-enumerated password input.
inputPassword :: FormError FormInput err => Text -> Form err Text
inputPassword = textField (textInputConfigured' defaultTextInputConfig {ticPassword = True})

-- | Auto-enumerated text area input.
inputTextArea :: FormError FormInput err => Text -> Form err Text
inputTextArea = textField textArea'

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
    (fieldView respClicked FormInputBool (checkbox' lbl))
    initial

-- | Auto-enumerated slider input.
inputSlider ::
  FormError FormInput err => Float -> Float -> Float -> Form err Float
inputSlider minV maxV initial =
  Unnamed.input
    (Right . decodeFloatInput initial)
    (fieldView respChanged FormInputFloat (slider' minV maxV))
    initial

-- | Auto-enumerated select dropdown.
inputSelect :: (Foldable f, FormError FormInput err) => f Text -> Int -> Form err Int
inputSelect options initial =
  Unnamed.input
    (Right . decodeInt initial)
    (fieldView respChanged FormInputInt (select' options))
    initial

-- | Auto-enumerated select for bounded enums.
inputEnumSelect ::
  forall a err.
  (Bounded a, Enum a, Show a, FormError FormInput err) => a -> Form err a
inputEnumSelect = enumField inputSelect
