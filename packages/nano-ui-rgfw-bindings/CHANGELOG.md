# Changelog

## 0.1.0.0 -- Unreleased

### Added

- First release: `RGFW`, the wrapped API over the bundled RGFW that
  `nano-ui-rgfw` uses, and `RGFW.Raw`, its direct bindings. Event fields and
  constants are read from the bundled header with `hsc2hs`. Builds with GHC
  9.10 through 9.14 (`base >=4.20 && <4.23`). The licence is `MIT AND Zlib`:
  RGFW's own notice ships in `LICENSE-RGFW`, and every change made to the
  bundled RGFW is marked `nano-ui:` in `cbits/RGFW.h`.
- `rgfw_windowHide` in `RGFW.Raw`.
- `rgfw_mouseLeave`, the event tag of the pointer leaving the window, which
  `pollEvent` reports as an `EventOther`, and `rgfw_mouseEnter`, the pointer
  entering it, which `pollEvent` reports as an `EventMouseMotion` to where it
  came in.
- `rgfw_windowRefresh` and `rgfw_windowRestored`, the event tags of a window
  the desktop uncovers and of one restored from minimized or maximized, in
  `RGFW.Raw`.
- `showMouse`, which shows or hides the pointer over a window.
- `rgfw_mouseNotAllowed`, `rgfw_mouseWait`, `rgfw_mouseProgress`, the side
  buttons `rgfw_mouseMisc1` and `rgfw_mouseMisc2`, and the key codes of Space,
  Insert, PrintScreen, Pause, the left and right modifier keys
  (`rgfw_keyShiftL`, `rgfw_keyShiftR` and the same for Control, Alt and
  Super) and the function, paging, lock, menu and keypad keys, in `RGFW.Raw`.
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
- `pollEvent` is a safe foreign call (`c_RGFW_window_checkEvent`): on Windows
  a drag of the title bar runs the system's move loop inside it, and other
  Haskell threads keep running meanwhile.

### Fixed

Changes to the bundled RGFW, and to how the bindings drive it:

- Waits end when they should. On X11 a `waitForEvent` with a timeout returns
  once it has passed: each wakeup without an event took the whole time
  waited so far off what was left, and after a few the wait never ended. On
  macOS a wait takes milliseconds, as elsewhere, and a negative one blocks:
  the idle wait was a busy loop and a 14 ms wait lasted 14 s. On Windows and
  macOS a `stopWaitForEvent` made before a wait ends that wait even when
  events were polled in between, as X11's stop pipe already did, so a
  background wake is not held back until a timed wait runs out.
- The event queue holds 1024 events instead of 32. RGFW drops a full queue
  whole, so fast input during a slow frame lost button and key releases.
- On Windows the pointer leaving the window is reported, and a press
  captures the mouse until the last button comes up, as SDL and GLFW do: a
  drag released outside the window ends, and moves the pointer off it, and a
  capture taken away mid-drag releases the buttons still down.
  `physicalToMappedKey` maps keys with no modifiers held there, as on X11 and
  macOS, so a press and its release map alike.
- Fullscreen covers the monitor at its current display mode. Windows
  switched the monitor to the window's size and saved that to the registry,
  and macOS changed the mode without restoring it. A fullscreen window no
  longer minimizes when it loses focus, and on Windows it is on top rather
  than topmost.
- `createWindowGL` turns vsync off (swap interval 0) on Windows too, as RGFW
  already did on X11, Wayland and macOS.
- On X11 a maximum size of (0, 0) is no limit, as on the other platforms,
  instead of pinning the window at its minimum.
- Closing the root window forgets it, so a later window's monitor events no
  longer read the freed one.
- `readClipboardText` returns `Nothing` when the clipboard holds no text;
  RGFW answered such a read with the last text read. `writeClipboardText`
  passes the NUL that RGFW's Windows and macOS writers read up to, so copies
  there no longer end in junk.
