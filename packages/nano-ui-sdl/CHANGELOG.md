# Changelog

## Unreleased

### Added

- Shaped text. Lines are laid out by SDL_ttf and HarfBuzz with ligatures,
  contextual forms and marks, and drawn glyph by glyph from the atlas.
  Arabic, Hebrew, Devanagari, CJK and other scripts the UI font lacks are
  drawn from installed fallback fonts (Noto, DejaVu, and the Windows and
  macOS system fonts), found the first time a text needs them.

### Changed

- Bold and italic are drawn by the core's synthetic weight and slant over the
  regular face, since SDL_ttf's style flags do not match the glyph images
  shaped text draws. Text measurement uses the shaped width, so layout and
  drawing agree.
- Fallback fonts for other scripts are shared across font sizes and opened
  only when a text needs a character they cover. Each coverage font is
  opened once a session as a probe, and each size draws from a copy that
  shares the probe's file. Resolving 40 sizes that draw CJK, Arabic and
  Devanagari opens 59 file descriptors instead of 959.
- A shaped line is shaped once and shared by measuring, preparing and
  drawing, and the font directories are walked once per process.
- No longer depends on `vector`.
- The framebuffer, glyph rasterization and snapping follow the window's
  pixel density (`SDL_GetWindowPixelDensity`) instead of its display scale.
  On Windows window coordinates are already pixels, so at 125% scaling every
  frame drew 1.56 times the window's pixels and shrank them back when
  presenting. Frames now draw at the window's size and text is no longer
  resampled. Sizes are unchanged; geometry snaps to whole pixels rather
  than to the 1.25x grid, so edges can move by up to half a pixel.
- The retained framebuffer is allocated in 256 pixel blocks and reused while
  the window fits, so a resize drag no longer creates a new render target
  for every pixel the border moves.

### Removed

- `newSdlContext`; the runners create their own context.
- `isDebugActive`, `newSdlDebugSampler`, `readSdlDebug` and `takeDebugLive`
  from `NanoUI.Backend.Sdl`. The core event loop samples debug timing; read
  the readout with `askSdlDebug`.
- The font debugging exports `saveFontRenderText`, `queryFontKerning`,
  `queryFontPairKerning`, `debugFontPair` and `dumpFontLayout`.
- `NanoUI.Sdl.Session` is no longer an exposed module.

## 0.1.0.0

First release.
