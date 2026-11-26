# Changelog

## 0.2.0.0 -- Unreleased

### Breaking changes

- Form runners and `resetForm` now take an explicit `FormState` allocated once with `newFormState`.
- `NanoUI.Form.Input` replaces the separate `Named` and `Unnamed` modules. Built-in inputs take `FieldName` (`OverloadedStrings` for literal names, `named text` for computed names, `unnamed` for positional fields).
- `NanoUI.Form.Backend` moved to `NanoUI.Form.Internal.Backend` (`FormInput`, `FormUI`, and `liftNanoUI` remain in `NanoUI.Form`).
- `inputWidget` adapts custom controls of type `Var a -> NanoUI Response` with an explicit `Publish` policy (`OnChange`, `OnChangeOr`, `OnlyWhen`).
- Dropped the `effectful-core` dependency in favor of `NanoUI`.

### Changed

- The `sdl` Cabal flag (for the example executable) is off by default, so depending on `nano-ui-form` no longer pulls in `nano-ui-sdl` or SDL3.
- `inputColor` formats values as `#rrggbbaa`, and `defaultErrorView` uses `themeDanger`.
- Builds with GHC 9.10 through 9.14 (`base >=4.20 && <4.23`).

### Fixed

- `nanoFormSubmit` includes edits made in the same frame as Enter or submit clicks, submits once per Enter press only when a single-line field or nothing has focus, and respects IME composition and modals.
- `markSubmitted` exposes `FormOnSubmit` errors from custom submit buttons; `inRange` rejects NaN.

## 0.1.0.0

First release.
