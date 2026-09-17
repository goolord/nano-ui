-- | Named form inputs: each takes a name that identifies the field and is
-- shown as its label. "NanoUI.Form" re-exports these.
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
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Ditto.Backend (FormError)
import Ditto.Core qualified as Ditto
import Ditto.Generalized.Named qualified as Named
import NanoUI
  ( Color
  , NanoUI
  , Response
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
import NanoUI.Form.Field
  ( decodeBool
  , decodeFloatInput
  , decodeInt
  , enumField
  , fieldErrors
  , fieldView
  )
import NanoUI.Form.Backend (FormInput (..), formInputToText)
import NanoUI.Form.Types (Form, FormView (..))

-- | Single-line text input field.
inputText :: FormError FormInput err => Text -> Text -> Form err Text
inputText = textField textInput'

-- | Single-line text input field with custom placeholder text.
inputTextWithPlaceholder ::
  FormError FormInput err => Text -> Text -> Text -> Form err Text
inputTextWithPlaceholder placeholder = textField (textInputConfigured' defaultTextInputConfig {ticPlaceholder = placeholder})

-- | Password text input masking entered characters.
inputPassword :: FormError FormInput err => Text -> Text -> Form err Text
inputPassword = textField (textInputConfigured' defaultTextInputConfig {ticPassword = True})

-- | Multi-line text area input.
inputTextArea :: FormError FormInput err => Text -> Text -> Form err Text
inputTextArea = textField textArea'

textField ::
  FormError FormInput err =>
  (Text -> NanoUI (Response, Text)) -> Text -> Text -> Form err Text
textField widget name =
  Named.input
    name
    (Right . formInputToText)
    (fieldView respChanged FormInputText (labelled name widget))

labelled :: Text -> (a -> NanoUI b) -> a -> NanoUI b
labelled name widget value = NUI.label name >> widget value

-- | Checkbox toggle input.
inputCheckbox :: FormError FormInput err => Text -> Bool -> Form err Bool
inputCheckbox name initial =
  Named.input
    name
    (Right . decodeBool initial)
    (fieldView respClicked FormInputBool (checkbox' name))
    initial

-- | Floating-point slider input across the range @[minV, maxV]@.
inputSlider ::
  FormError FormInput err => Text -> Float -> Float -> Float -> Form err Float
inputSlider name minV maxV initial =
  Named.input
    name
    (Right . decodeFloatInput initial)
    (fieldView respChanged FormInputFloat (labelled name (slider' minV maxV)))
    initial

-- | Dropdown selection in fold order (returns selected index).
inputSelect :: (Foldable f, FormError FormInput err) => Text -> f Text -> Int -> Form err Int
inputSelect name options initial =
  Named.input
    name
    (Right . decodeInt initial)
    (fieldView respChanged FormInputInt (labelled name (select' options)))
    initial

-- | Dropdown selection for any bounded enumeration type.
inputEnumSelect ::
  forall a err.
  (Bounded a, Enum a, Show a, FormError FormInput err) => Text -> a -> Form err a
inputEnumSelect name = enumField (inputSelect name)

-- | Radio button group (returns selected index).
inputRadio :: (Foldable f, FormError FormInput err) => Text -> f Text -> Int -> Form err Int
inputRadio name options initial =
  Named.input
    name
    (Right . decodeInt initial)
    (fieldView respChanged FormInputInt (labelled name (radio' options)))
    initial

-- | Radio button group for any bounded enumeration type.
inputEnumRadio ::
  forall a err.
  (Bounded a, Enum a, Show a, FormError FormInput err) => Text -> a -> Form err a
inputEnumRadio name = enumField (inputRadio name)

-- | Color picker input.
inputColor :: FormError FormInput err => Text -> Color -> Form err Color
inputColor name initial =
  Named.input
    name
    ( \case
        FormInputText t -> Right (fromMaybe initial (colorFromHex t))
        _ -> Right initial
    )
    ( fieldView
        respChanged
        (FormInputText . colorToHex)
        (labelled name colorPicker')
    )
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
