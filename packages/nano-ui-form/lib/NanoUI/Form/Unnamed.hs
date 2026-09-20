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
import NanoUI
  ( TextInputConfig (..)
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
import NanoUI.Form.Backend (FormInput (..))
import NanoUI.Form.Field
  ( decodeBool
  , decodeFloatInput
  , decodeInt
  , enumField
  , inputWidget
  , textField
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
inputText = textField Nothing textInput'

-- | Auto-enumerated password input.
inputPassword :: FormError FormInput err => Text -> Form err Text
inputPassword = textField Nothing (textInputConfigured' defaultTextInputConfig {ticPassword = True})

-- | Auto-enumerated text area input.
inputTextArea :: FormError FormInput err => Text -> Form err Text
inputTextArea = textField Nothing textArea'

-- | Auto-enumerated checkbox toggle.
inputCheckbox :: FormError FormInput err => Text -> Bool -> Form err Bool
inputCheckbox lbl initial =
  inputWidget Nothing (Right . decodeBool initial) respClicked FormInputBool (checkbox' lbl) initial

-- | Auto-enumerated slider input.
inputSlider ::
  FormError FormInput err => Float -> Float -> Float -> Form err Float
inputSlider minV maxV initial =
  inputWidget Nothing (Right . decodeFloatInput initial) respChanged FormInputFloat (slider' minV maxV) initial

-- | Auto-enumerated select dropdown.
inputSelect :: (Foldable f, FormError FormInput err) => f Text -> Int -> Form err Int
inputSelect options initial =
  inputWidget Nothing (Right . decodeInt initial) respChanged FormInputInt (select' options) initial

-- | Auto-enumerated select for bounded enums.
inputEnumSelect ::
  forall a err.
  (Bounded a, Enum a, Show a, FormError FormInput err) => a -> Form err a
inputEnumSelect = enumField inputSelect
