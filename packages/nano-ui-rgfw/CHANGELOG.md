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
