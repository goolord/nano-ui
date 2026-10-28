# Validated Forms (`nano-ui-form`)

`nano-ui-form` builds applicative forms on [ditto](https://hackage.haskell.org/package/ditto). A form is a value whose inputs draw nano-ui widgets and whose validators produce errors; running it renders the fields and returns the parsed record when the form is valid.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import NanoUI (NanoUI, label)
import NanoUI.Form hiding (label)

data Signup = Signup Text Text

signup :: Form Text Signup
signup =
  Signup
    <$> withFieldErrors (inputText "Name" "" `prove` notEmpty "Name is required")
    <*> withFieldErrors (inputText "Email" "" `prove` validEmail (const "Not an email address"))
```

`Form err a` fixes the error type (here `Text`) and the result type (here `Signup`). Each `inputText` is a field; `prove` attaches a validation `Proof`; `withFieldErrors` renders the error below the field.

## Running a form

Allocate a `FormState` once, before the session, and pass the same stable prefix every frame:

```haskell
main :: IO ()
main = do
  owner <- newFormState
  runSdlApp defaultSdlOptions (view owner)

view :: FormState -> NanoUI ()
view owner = do
  submitted <- nanoFormSubmit owner "signup" "Sign up" signup
  case submitted of
    Just (Signup name _) -> label ("Welcome, " <> name)
    Nothing              -> pure ()
```

| Runner | Result |
| --- | --- |
| `nanoFormSubmit owner prefix label form` | Adds a submit button; returns `Just a` on the frame a valid form is submitted. |
| `nanoFormLive owner prefix form` | Returns `Just a` whenever the form is valid, for previews. |
| `nanoFormEx owner config prefix form` | Configured runner; returns `FormValid a` or `FormInvalid errs`. |
| `runNanoForm owner prefix form` | Returns the form's view and result separately. |

`nanoFormLive` on every frame is fine for a preview, but `Just` is not an event: if the value must remain visible after submission, store it. `nanoFormSubmit`'s `Just` is a one-frame event.

Give each form a distinct stable prefix, and allocate one `FormState` per component or session. A changed prefix is a different form. Call `resetForm owner prefix` when starting a new entry with the same form.

`FormOnSubmit` forms show errors only after the first submission; call `markSubmitted` from your own submit button when you render one yourself. `nanoFormSubmit` and `nanoFormEx` handle that marking.

## Inputs

`NanoUI.Form.Input` provides one family for named and positional fields:

```haskell
inputText "name" "Ada"                     -- stable key and caption
inputText unnamed "Ada"                    -- numbered, no caption
inputCheckbox (unnamed {fieldLabel = Just "Accept"}) False
inputText ((named "login") {fieldLabel = Just "User name"}) "Ada"
```

The family includes `inputText`, `inputTextWithPlaceholder`, `inputPassword`, `inputTextArea`, `inputCheckbox`, `inputSlider`, `inputSelect`, `inputEnumSelect`, `inputRadio`, `inputEnumRadio`, and `inputColor`.

## Validation

`NanoUI.Form.Validation` supplies the common proofs:

```haskell
notEmpty "Required"
minLength 8 (\n -> "Use at least " <> T.pack (show n) <> " characters")
maxLength 64 (const "Too long")
inRange 0 130 (const "Out of range")
validEmail (const "Not an email address")
matches confirmPassword "Passwords do not match"
```

The numeric proofs from ditto read a `String`; apply them after `fmap T.unpack`:

```haskell
(T.unpack <$> inputText "Age" "") `prove` decimal (const "Not a number") 0
```

`customProof` and `satisfies` cover everything else. Errors accumulate applicatively: every invalid field reports its own error, and the record is produced only when all fields validate.

## Adapting a widget

`inputWidget` adapts any widget of type `Var a -> NanoUI Response`:

```haskell
import NanoUI qualified as UI

volume :: Form Text Float
volume =
  inputWidget (Just "volume") decode (OnChangeOr UI.respChanged) FormInputFloat
    (UI.knob' 0 100) 50
  where
    decode (FormInputFloat n) = Right n
    decode _ = Left "Expected a volume"
```

The arguments are the optional stable name, a decoder, a publication policy, the encoder, the widget, and the initial value. `Nothing` selects positional identity.

Publication policies decide when an edit is committed to the form:

| Policy | Publishes |
| --- | --- |
| `OnChange` | Whenever the widget's `Var` changes to a different value. |
| `OnChangeOr p` | Also when `p` holds for the response (for widgets that report an edit without value change). |
| `OnlyWhen p` | Only when `p` holds, such as `OnlyWhen UI.respSubmitted` for an Enter-committed field. |

Uncommitted edits stay in the field state across frames until committed.

> [!NOTE]
> Form state is keyed by the prefix and the field names. Changing a caption does not change the stored key; changing the prefix starts a new form. This makes forms safe across frames and sortable/filterable views, but it also means two forms with the same prefix share field state.

The package's example (`cabal run nano-ui-form-example`) needs `nano-ui-sdl`; the library itself has no backend dependency.
