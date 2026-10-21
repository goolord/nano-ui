# Changelog

## 0.1.0.0 -- Unreleased

### Added

- First release: `NanoUI.Backend.Rgfw` runs nano-ui views in a native window
  through RGFW and OpenGL 3.2, with the bundled Cozette bitmap font. Builds
  with GHC 9.10 through 9.14 (`base >=4.20 && <4.23`).
- Reducer runners take `model -> NanoUIE msg ()` from the `nano-ui-emit`
  package, so message types are checked statically.
- `optExplainLayout` opens a window with the layout overlay on, and
  `debugWindowBody` has a checkbox for it.
- The middle mouse button, the side buttons as back and forward, and the
  misc buttons past them as `MouseOther 6` to `MouseOther 8`.
- The pointer leaving the window moves it off every widget, so nothing stays
  hovered.
- Cursor shapes show RGFW's standard cursors or the nearest it has, the grab
  hands show the move arrows, and `UiCursorHidden` hides the pointer.
- `optTheme`, `newRgfwContext` and `applyRgfwTheme` take a core `Theme`,
  drawn with square corners and 1px borders. `optThemeFor` picks the theme
  for the desktop's light or dark setting, as SDL's `sdlAppThemeFor`. RGFW
  cannot read the setting, so it is given `Nothing`, for which `lightDark`
  picks the dark theme.
- The core's window: the window opens from `optWindow`, a core
  `WindowSettings` (`defaultWindowSettings {wsTitle = t, wsSize = s}`), with
  a position, size limits, an icon, a mode and whether a close request ends
  the session. Its size is in layout units, converted at the scale the
  window opens at, so a window opens at the same size on every monitor. The
  core's window setters and commands, `askWindow`, `quitUi` and
  `requestScreenshot` work from a view, but for opacity and transparency:
  RGFW windows are opaque and do not fade. Focus, maximized, minimized and
  fullscreen are read from what RGFW keeps from its events, with no round
  trip to the X server.
- Super (Command) is `modSuper`, and Ctrl+letter is a key chord rather than
  typed text. Every key RGFW reports comes in as a `Key`, with its release
  and held state, and each auto-repeat of a held key as a press, which
  `inputKeysNew` leaves out. Losing the keyboard (RGFW's focus-out event)
  lets go of the keys held.
- A frame repaints only its damage. Frames draw into a retained offscreen
  framebuffer, which each present copies to the window, and a frame that is
  not forced, resized or rescaled paints and uploads only the widgets and
  glyphs inside its damage pieces. A frame with no damage skips the OpenGL
  render and the buffer swap.
- Images are drawn from a texture of the core's image atlas.
- Another thread can wake the loop, so a background job's result shows
  without waiting for input.
