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
