# Changelog

## Unreleased

### Added

- `optExplainLayout` opens a window with the layout overlay on, and
  `debugWindowBody` has a checkbox for it.
- The middle mouse button, the side buttons as back and forward, and the
  misc buttons past them as `MouseOther 6` to `MouseOther 8`.
- The pointer leaving the window moves it off every widget, so nothing stays
  hovered.
- Window options `optIcon`, `optMinSize` and `optMaxSize` (in native
  pixels). The core's window setters and `requestScreenshot` work from a
  view, but for `setWindowOpacityUi`, which does nothing: RGFW windows do not
  fade.

### Changed

- Builds with GHC 9.10 through 9.14 (`base >=4.20 && <4.23`).
- `NanoUI.Rgfw.Context`, `.Debug`, `.Gl`, `.Session` and `.Font.Cozette` are
  now under `NanoUI.Rgfw.Internal`. `NanoUI.Backend.Rgfw` is the API.
- `RgfwOptions`'s `optTheme`, `newRgfwContext` and `applyRgfwTheme` take a
  core `Theme`, drawn with square corners and 1px borders.
- The Cozette glyph table uses `primitive` arrays.
- No longer depends on `vector`.
- `NanoUI.Rgfw.Gl` exports `physClip` and `toPhysRect`.
- A frame repaints only its damage. Frames draw into a retained offscreen
  framebuffer, which each present copies to the window, and a frame that is
  not forced, resized or rescaled paints and uploads only the widgets and
  glyphs inside its damage. In `nano-ui-rgfw-profile`, a changing counter
  above a window of text went from 0.49 to 0.19 ms a frame, and from 1160 to
  378 KB allocated. `renderArenaGl` takes the frame's `Damage`, and a frame
  at a new size must be `DamageFull`; `writeSpanQuads` clips to a box of
  physical pixels instead of a framebuffer size.
- `renderArenaGl` takes the frame's damage pieces and cuts text to each. Two
  labels changing in opposite corners of a window of text went from 0.41 to
  0.11 ms a frame, since they no longer repaint everything between them.
- The new cursor shapes show RGFW's standard cursors or the nearest it has,
  and the grab hands show the move arrows instead of the arrow.
- Super (Command) is `modSuper` rather than Ctrl, and Ctrl+letter is a key
  chord rather than typed text. Every key RGFW reports comes in as a `Key`,
  with its release and held state, and Enter, Escape, Tab, Insert, the
  function and lock keys no longer auto-repeat.
- `optCenter` is replaced by `optPosition`: `WindowPositionCentered`, the
  default, or `WindowPositionAt` a point on the desktop.

### Fixed

- Wheel events that arrive in one batch add up instead of keeping only the
  last.
- Animations run at the display rate: a per-frame sleep on top of the core
  loop's pacing halved it.
- A frame the opening frame asks for is drawn at once. The session cleared
  the dirty flag after it, so the view waited for input to show what it had
  asked to show.
- A frame with no damage skips the OpenGL render and the buffer swap, since
  it would swap in the picture already on screen. An animation scrolled out
  of view costs its UI pass and nothing on the GPU.
- Images are drawn, from a texture of the core's image atlas, instead of as
  a rectangle in their tint colour.
- Another thread can wake the loop: the session installs `ctxWakeLoop`, so a
  background job's result shows without waiting for input.

### Removed

- `RgfwTheme`, `rgfwCoreTheme`, `defaultDarkTheme`, `defaultLightTheme`,
  `tomorrowMinLightTheme`, `tomorrowNightMinDarkTheme` and
  `tomorrowMidnightMinDarkTheme` from `NanoUI.Backend.Rgfw`; use the core
  themes from `NanoUI`.
- The software rasteriser modules `NanoUI.Rgfw.Render` and
  `NanoUI.Rgfw.Surface`, now part of the test suite, and
  `renderTextScaledToBuffer` from `NanoUI.Rgfw.Font.Cozette`.
- `noteLoop` and `notePresent` from `NanoUI.Rgfw.Debug`.

## 0.1.0.0

First release.
