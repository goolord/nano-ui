# nano-ui-form

Validated applicative forms for [nano-ui](https://github.com/goolord/nano-ui), built on [ditto](https://hackage.haskell.org/package/ditto).

Each form combines widget rendering, parsing, and validation in one applicative value, attaching field-level error messages directly beneath invalid inputs.

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

-- Allocate owner <- newFormState once during setup.
view :: FormState -> NanoUI ()
view owner = do
  submitted <- nanoFormSubmit owner "signup" "Sign up" signup
  case submitted of
    Just (Signup name _) -> label ("Welcome, " <> name)
    Nothing -> pure ()
```

`nanoFormSubmit` renders a submit button and returns `Just a` on the single frame the form submits validly. `nanoFormLive` returns `Just a` continuously whenever all fields are valid, `nanoFormEx` accepts a `FormConfig`, and `runNanoForm` separates rendering from extracting the result. When using `FormOnSubmit` error display with your own custom submit button, call `markSubmitted` on click.

## Running the example

```sh
cabal run nano-ui-form-example
```

The example depends on `nano-ui-sdl` behind the `sdl` Cabal flag (off by default on Hackage; enabled in this repository's `cabal.project`). The library itself has no backend dependency.

## Form state and field identity

Give each form a unique stable key (such as `"signup"`). Allocate `newFormState` once per component or session and pass it to every runner and to `resetForm` when clearing the form for a new entry. Deferred field views capture their `FormState` automatically, so nesting or moving rendered fragments never loses track of which form they belong to.

Remember that `Just value` from `nanoFormSubmit` is a single-frame event: store the result in a `Var` or handle it immediately if it needs to stay visible after the click frame. Use `nanoFormLive` when you want live previews on every frame.

Wrap validated fields in `withFieldErrors` to display errors from `NanoUI.Form.Validation`. `NanoUI.Form.Input` supports both named and positional fields through one unified API:

```haskell
inputText "name" "Ada"                    -- stable key and visible caption
inputText unnamed "Ada"                   -- positional key, no caption
inputCheckbox (unnamed {fieldLabel = Just "Accept"}) False
inputText ((named "login") {fieldLabel = Just "User name"}) "Ada"
```

With `OverloadedStrings`, plain string literals set both key and caption; wrap dynamic `Text` keys in `named`. Changing `fieldLabel` never alters the stored key, and `fieldKey = Nothing` selects positional identity.

## Adapting custom widgets

Use `inputWidget` to turn any `Var a -> NanoUI Response` widget (such as a primed control) into a form field. The form runs the widget against a temporary `Var` and writes edits back according to your publication policy:

```haskell
import NanoUI qualified as UI

volume :: Form Text Float
volume = inputWidget (Just "volume") decode (OnChangeOr UI.respChanged) FormInputFloat
  (UI.knob' 0 100) 50
  where
    decode (FormInputFloat n) = Right n
    decode _ = Left "Expected a volume"
```

Arguments are: optional stable key (`Nothing` for positional), decoder, publication policy, encoder, widget, and initial value. Add a label inside the widget action itself if you want one displayed. Publication policies control when edits commit to the form:

- `OnChange`: Publishes whenever the widget writes a different value to its `Var`.
- `OnChangeOr p`: Also publishes whenever predicate `p` holds on the `Response`.
- `OnlyWhen p`: Publishes only when `p` holds (such as `OnlyWhen UI.respSubmitted` to commit on Enter). Uncommitted edits persist in field state until committed, overwritten externally, or reset.

## Building

Target GHC 9.10 through 9.14 and add `nano-ui`, `nano-ui-form`, and `text` to `build-depends` (requires `ditto >= 0.5` from Hackage). Build just the backend-independent library with `cabal build lib:nano-ui-form`.

## Documentation

- [The nano-ui Book: Validated Forms](https://goolord.github.io/nano-ui/book/packages/index.html#validated-forms): Overview and companion package guide.
