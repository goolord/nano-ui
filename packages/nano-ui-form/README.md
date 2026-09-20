# nano-ui-form

Forms for [nano-ui](https://github.com/goolord/nano-ui), built on
[ditto](https://hackage.haskell.org/package/ditto).

A form is an applicative value. Each input draws a nano-ui widget and parses
its value; validators attach errors that appear under the field.

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

view :: NanoUI ()
view = do
  submitted <- nanoFormSubmit "signup" "Sign up" signup
  case submitted of
    Just (Signup name _) -> label ("Welcome, " <> name)
    Nothing -> pure ()
```

`nanoFormSubmit` adds a submit button and yields the value on the frame the form
is submitted. `nanoFormLive` yields the value whenever the form is valid,
`nanoFormEx` takes a `FormConfig`, and `runNanoForm` returns the form's view and
result separately.

## Running

```sh
cabal run nano-ui-form-example
```

The example needs `nano-ui-sdl`, behind this package's `sdl` flag (on by
default). The library itself does not depend on a backend.

## Form identity and results

Give each form a stable key, such as `"signup"`, and use different keys for
independent forms in the same view. The form runner keeps field values and
validation state between frames under that identity. Use the reset operations
in `NanoUI.Form.Runner` when starting a new entry with the same form.

The `Just` returned by `nanoFormSubmit` is an event for that frame. Store the
submitted value if it must remain visible later. In the example, the welcome
label appears only on the submission frame. `nanoFormLive` is for previews or
other views that need the current valid value on every frame.

Use `withFieldErrors` around a validated field to display its errors.
`NanoUI.Form.Validation` supplies common validators. `NanoUI.Form.Named`
supports named fields; `NanoUI.Form.Unnamed` exposes positional composition.

## Build

Use GHC 9.14 and add `nano-ui`, `nano-ui-form`, and `text` to your application's
`build-depends`. The form library requires ditto 0.5. The repository builds it
from a sibling checkout, as described in the
[development guide](https://github.com/goolord/nano-ui/blob/main/docs/development.md).
To build only the form library without SDL, use `cabal build lib:nano-ui-form`.
