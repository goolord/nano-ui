# Changelog

## Unreleased

### Added

- `rgfw_windowHide` in `RGFW.Raw`.
- `rgfw_mouseNotAllowed`, `rgfw_mouseWait`, `rgfw_mouseProgress`, and the
  side buttons `rgfw_mouseMisc1` and `rgfw_mouseMisc2`, in `RGFW.Raw`.

### Changed

- Builds with GHC 9.10 through 9.14 (`base >=4.20 && <4.23`).
- Event field access, event size, and constants are generated from the bundled
  header with `hsc2hs`. The Haskell API is unchanged; field reads no longer call
  separate C accessors. `hsc2hs` is a build-time tool, not a runtime dependency.

- `readClipboardText` and `writeClipboardText` use `Text` instead of
  `String`.
- Non-blocking event and window accessors are `unsafe` foreign calls.

## 0.1.0.0

First release.
