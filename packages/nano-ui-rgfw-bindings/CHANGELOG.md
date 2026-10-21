# Changelog

## 0.1.0.0 -- Unreleased

### Added

- First release: `RGFW`, the wrapped API over the bundled RGFW that
  `nano-ui-rgfw` uses, and `RGFW.Raw`, its direct bindings. Event fields and
  constants are read from the bundled header with `hsc2hs`. Builds with GHC
  9.10 through 9.14 (`base >=4.20 && <4.23`).
- `rgfw_windowHide` in `RGFW.Raw`.
- `rgfw_mouseLeave`, the event tag of the pointer leaving the window, which
  `pollEvent` reports as an `EventOther`.
- `showMouse`, which shows or hides the pointer over a window.
- `rgfw_mouseNotAllowed`, `rgfw_mouseWait`, `rgfw_mouseProgress`, the side
  buttons `rgfw_mouseMisc1` and `rgfw_mouseMisc2`, and the key codes of Space,
  Insert, PrintScreen, Pause and the function, paging, lock, menu and keypad
  keys, in `RGFW.Raw`.
- `EventKeyRepeat`, a held key's auto-repeat, which `EventKeyPress` does not
  report, and `physicalToMappedKey`.
- `setWindowIcon`, `setWindowMinSize`, `setWindowMaxSize`, `moveWindow`,
  `centerWindow`, `setWindowName`, `resizeWindow`, `maximizeWindow`,
  `minimizeWindow`, `restoreWindow`, `setWindowFullscreen`, `showWindow` and
  `hideWindow`; `windowPosition`, `windowFlags` and `windowFocused`, which
  read what RGFW keeps of the window's state; and the window flags
  `rgfw_windowNoResize`, `rgfw_windowFullscreen`, `rgfw_windowMaximize` and
  `rgfw_windowMinimize`. `setWindowIcon` rejects dimensions that overflow or
  fall outside the native integer range before passing pixel data to RGFW.
- `stopWaitForEvent`, which ends a `waitForEvent` from another thread. On X11
  the bundled RGFW's wait watches its stop pipe too.

### Fixed

- In the bundled RGFW, on X11, a `waitForEvent` with a timeout returns once
  it has passed. Each wakeup without an event took the whole time waited so
  far off what was left, and after a few the wait never ended.
