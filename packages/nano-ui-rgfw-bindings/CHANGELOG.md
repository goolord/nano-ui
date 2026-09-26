# Changelog

## Unreleased

### Added

- `rgfw_windowHide` in `RGFW.Raw`.
- `rgfw_mouseLeave`, the event tag of the pointer leaving the window, which
  `pollEvent` reports as an `EventOther`.
- `showMouse`, which shows or hides the pointer over a window.
- `rgfw_mouseNotAllowed`, `rgfw_mouseWait`, `rgfw_mouseProgress`, the side
  buttons `rgfw_mouseMisc1` and `rgfw_mouseMisc2`, and the key codes of Space,
  Insert, PrintScreen, Pause and the function, paging, lock, menu and keypad
  keys, in `RGFW.Raw`.
- `EventKeyRepeat`, a held key's auto-repeat, which `EventKeyPress` no longer
  reports, and `physicalToMappedKey`.
- `setWindowIcon`, `setWindowMinSize`, `setWindowMaxSize`, `moveWindow`,
  `centerWindow`, `setWindowName`, `resizeWindow`, `maximizeWindow`,
  `minimizeWindow`, `restoreWindow`, `setWindowFullscreen`, `showWindow` and
  `hideWindow`; `windowPosition`, `windowFlags` and `windowFocused`, which
  read what RGFW keeps of the window's state; and the window flags
  `rgfw_windowNoResize`, `rgfw_windowFullscreen`, `rgfw_windowMaximize` and
  `rgfw_windowMinimize`.
- `stopWaitForEvent`, which ends a `waitForEvent` from another thread. On X11
  the bundled RGFW's wait now watches its stop pipe too.

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
