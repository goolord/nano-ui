# Changelog

## Unreleased

### Changed

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
  378 KB allocated. `renderArenaGl` takes the frame's `Damage` and returns
  whether it kept the retained pixels, and `writeSpanQuads` clips to a box of
  physical pixels instead of a framebuffer size.

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
