# Changelog

## 0.2.0.0 -- Unreleased

- **Breaking:** runners and `resetForm` take a `FormState` allocated with
  `newFormState`. `FormUI` has a typed lexical owner; field views capture it
  during evaluation, preserving deferred/nested views without dynamic slots
  or ambient prefix mutation.

- No longer depends on `effectful-core`.
- Builds with GHC 9.10 through 9.14 (`base >=4.20 && <4.23`).
- The library imports `NanoUI.Internal` modules, which change without
  notice, so it allows only nano-ui 0.2.0.x (`>=0.2.0 && <0.2.1`).
- The `sdl` flag, which builds the SDL example, is off by default, so
  depending on the library no longer needs nano-ui-sdl or SDL3.
- `markSubmitted` counts a form as submitted from the app's own button, so a
  `FormOnSubmit` form shows its errors.
- `nanoFormSubmit` includes edits made in the same frame as Enter or the
  submit click. It submitted the value from before them, and an
  `OnlyWhen respSubmitted` text field never submitted its text.
- `inputColor` stores its colour as `#rrggbbaa`, not `#rrggbb`. An edit makes
  the colour opaque, as `colorPicker'`, which has no alpha bar, does.
- `inRange` rejects NaN.
- `defaultErrorView`'s callout takes the theme's danger colour, which its
  messages are drawn in, rather than its red.
- `NanoUI.Form.Backend` is now `NanoUI.Form.Internal.Backend`. `NanoUI.Form`
  still exports `FormInput`, `FormUI` and `liftNanoUI`.
- `nanoFormSubmit` submits on Enter only when nothing or a single-line field
  is focused: not from a text area, an input method commit, another focused
  control, or behind a modal. Holding Enter submits once.
- `inputWidget` adapts custom controlled widgets to named or positional fields,
  with explicit decoding, encoding, and a `Publish` policy: `OnChange`,
  `OnChangeOr` a response predicate, or `OnlyWhen` one, which holds edits
  back until, for example, a commit. It takes an input of type
  `Var a -> NanoUI Response`, such as `UI.knob' 0 100`, as nano-ui's inputs
  are, and runs it on a temporary `Var` holding the field's value.
- `NanoUI.Form.Input` replaces the parallel `Named` and `Unnamed` modules.
  Built-in inputs take `FieldName`, separating the optional key from the optional
  caption. String literals retain the usual syntax with `OverloadedStrings`;
  use `named text` for computed names and `unnamed` for numbered inputs.
  For example, `Unnamed.inputSelect options value` becomes
  `inputSelect unnamed options value`; an unnamed checkbox with a caption uses
  `inputCheckbox (unnamed {fieldLabel = Just caption}) value`.

## 0.1.0.0

First release.
