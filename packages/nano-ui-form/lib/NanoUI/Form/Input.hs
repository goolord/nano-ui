-- | Form inputs with named or positional identity and an optional caption.
-- "NanoUI.Form" re-exports these. Use a string literal for a labelled, named
-- field, or 'unnamed' for a positional input.
module NanoUI.Form.Input
  ( FieldName (..)
  , named
  , unnamed
  , inputText
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
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Ditto.Backend (FormError)
import Ditto.Core qualified as Ditto
import Ditto.Generalized.Named qualified as Named
import NanoUI
  ( Color
  , TextInputConfig (..)
  , checkbox'
  , colorFromHex
  , colorPicker'
  , colorToHex
  , defaultTextInputConfig
  , radio'
  , respChanged
  , respClicked
  , select'
  , slider'
  , textArea'
  , textInput'
  , textInputConfigured'
  )
import NanoUI qualified as NUI
import NanoUI.Form.Backend (FormInput (..), formInputToText)
import NanoUI.Form.Field
  ( decodeBool
  , decodeFloatInput
  , decodeInt
  , enumField
  , fieldErrors
  , inputWidget
  , labelled
  )
import NanoUI.Form.Types (FieldName (..), Form, FormView (..), named, unnamed)

-- | Single-line text input field.
inputText :: FormError FormInput err => FieldName -> Text -> Form err Text
inputText = textField textInput'

-- | Single-line text input field with custom placeholder text.
inputTextWithPlaceholder ::
  FormError FormInput err => Text -> FieldName -> Text -> Form err Text
inputTextWithPlaceholder placeholder =
  textField
    (textInputConfigured' defaultTextInputConfig {ticPlaceholder = placeholder})

-- | Password text input masking entered characters.
inputPassword :: FormError FormInput err => FieldName -> Text -> Form err Text
inputPassword = textField (textInputConfigured' defaultTextInputConfig {ticPassword = True})

-- | Multi-line text area input.
inputTextArea :: FormError FormInput err => FieldName -> Text -> Form err Text
inputTextArea = textField textArea'

textField ::
  FormError FormInput err =>
  (Text -> NUI.NanoUI (NUI.Response, Text)) -> FieldName -> Text -> Form err Text
textField widget name =
  inputWidget
    (fieldKey name)
    (Right . formInputToText)
    respChanged
    FormInputText
    (labelled (fieldLabel name) widget)

-- | Checkbox toggle input.
inputCheckbox :: FormError FormInput err => FieldName -> Bool -> Form err Bool
inputCheckbox name initial =
  inputWidget
    (fieldKey name)
    (Right . decodeBool initial)
    respClicked
    FormInputBool
    (checkbox' (fromMaybe "" (fieldLabel name)))
    initial

-- | Floating-point slider input across the range @[minV, maxV]@.
inputSlider ::
  FormError FormInput err =>
  FieldName -> Float -> Float -> Float -> Form err Float
inputSlider name minV maxV initial =
  inputWidget
    (fieldKey name)
    (Right . decodeFloatInput initial)
    respChanged
    FormInputFloat
    (labelled (fieldLabel name) (slider' minV maxV))
    initial

-- | Dropdown selection in fold order (returns selected index).
inputSelect ::
  (Foldable f, FormError FormInput err) =>
  FieldName -> f Text -> Int -> Form err Int
inputSelect name options initial =
  inputWidget
    (fieldKey name)
    (Right . decodeInt initial)
    respChanged
    FormInputInt
    (labelled (fieldLabel name) (select' options))
    initial

-- | Dropdown selection for any bounded enumeration type.
inputEnumSelect ::
  forall a err.
  (Bounded a, Enum a, Show a, FormError FormInput err) =>
  FieldName -> a -> Form err a
inputEnumSelect name = enumField (inputSelect name)

-- | Radio button group (returns selected index).
inputRadio ::
  (Foldable f, FormError FormInput err) =>
  FieldName -> f Text -> Int -> Form err Int
inputRadio name options initial =
  inputWidget
    (fieldKey name)
    (Right . decodeInt initial)
    respChanged
    FormInputInt
    (labelled (fieldLabel name) (radio' options))
    initial

-- | Radio button group for any bounded enumeration type.
inputEnumRadio ::
  forall a err.
  (Bounded a, Enum a, Show a, FormError FormInput err) =>
  FieldName -> a -> Form err a
inputEnumRadio name = enumField (inputRadio name)

-- | Color picker input.
inputColor :: FormError FormInput err => FieldName -> Color -> Form err Color
inputColor name initial =
  inputWidget
    (fieldKey name)
    ( \case
        FormInputText t -> Right (fromMaybe initial (colorFromHex t))
        _ -> Right initial
    )
    respChanged
    (FormInputText . colorToHex)
    (labelled (fieldLabel name) colorPicker')
    initial

-- | Static label inside a form.
label :: Text -> Form err ()
label txt = Ditto.view (FormView (NUI.label txt))

-- | Visual separator line inside a form.
separator :: Form err ()
separator = Ditto.view (FormView NUI.separator)

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
withFieldErrors = withChildErrors fieldErrors
