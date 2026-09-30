# Changelog

## 0.2.0.0 -- Unreleased

- **Breaking:** runners and `resetForm` take a `FormState` allocated with
  `newFormState`. `FormUI` has a typed lexical owner; field views capture it
  during evaluation, preserving deferred/nested views without dynamic slots
  or ambient prefix mutation.

- No longer depends on `effectful-core`.
- Builds with GHC 9.10 through 9.14 (`base >=4.20 && <4.23`).
- `defaultErrorView`'s callout takes the theme's danger colour, which its
  messages are drawn in, rather than its red.
- `NanoUI.Form.Backend` is now `NanoUI.Form.Internal.Backend`. `NanoUI.Form`
  still exports `FormInput`, `FormUI` and `liftNanoUI`.
- `nanoFormSubmit` submits on Enter only when nothing or a single-line field
  is focused: not from a text area, an input method commit, another focused
  control, or behind a modal. Holding Enter submits once.
- `inputWidget` adapts custom controlled widgets to named or positional fields,
  with explicit decoding, encoding, and response policy.
- `NanoUI.Form.Input` replaces the parallel `Named` and `Unnamed` modules.
  Built-in inputs take `FieldName`, separating the optional key from the optional
  caption. String literals retain the usual syntax with `OverloadedStrings`;
  use `named text` for computed names and `unnamed` for numbered inputs.
  For example, `Unnamed.inputSelect options value` becomes
  `inputSelect unnamed options value`; an unnamed checkbox with a caption uses
  `inputCheckbox (unnamed {fieldLabel = Just caption}) value`.

## 0.1.0.0

First release.
