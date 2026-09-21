# Changelog

## Unreleased

- `NanoUI.Form.Backend` is now `NanoUI.Form.Internal.Backend`. `NanoUI.Form`
  still exports `FormInput`, `FormUI` and `liftNanoUI`.
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
