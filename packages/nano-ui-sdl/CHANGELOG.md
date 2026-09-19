# Changelog

## Unreleased

### Added

- `nano-ui-sdl-idle`, a window for measuring what an app costs while nobody
  touches it: a static view, a focused search field, a background wake, a
  spinner that goes away, a `wakeAfter` clock, a typed character, and frames
  asked for during startup. `hidden` runs a scene off screen. See
  "Idle cost" in `docs/development.md`.
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

### Fixed

- `saveScreenshot` reads the retained frame. It read the window
  backbuffer, which SDL leaves undefined after a present.
- A resize drag no longer trails the border by a step. Each step was drawn
  on `SDL_EVENT_WINDOW_RESIZED`, before the renderer had resized its swap
  chain, so it went to the old-size backbuffer and showed cropped or with a
  bare strip; frames are now drawn on the pixel size change that follows.
- A frame asked for while the window opens is drawn. The opening frames are
  drawn before the loop starts, and the session then cleared the dirty flag
  and drained the wakes they had queued, so a view that asked for another
  frame, or a thread that finished its work that early, sat unseen until the
  pointer crossed the window.
- Waking the loop from another thread runs a frame and presents what that
  frame damaged, which is nothing when the change is not on screen. Every
  wake used to repaint and present the whole window. A font or UI scale
  switch, which does change every pixel, asks for the full repaint itself.
- Wakes are coalesced: while one is queued, another costs an atomic swap and
  no `SDL_PushEvent`. The core wakes the loop on every `markDirty`, most of
  them made by the loop's own thread in the middle of a frame.

### Removed

- `newSdlContext`; the runners create their own context.
- `isDebugActive`, `newSdlDebugSampler`, `readSdlDebug` and `takeDebugLive`
  from `NanoUI.Backend.Sdl`. The core event loop samples debug timing; read
  the readout with `askSdlDebug`.
- The font debugging exports `saveFontRenderText`, `queryFontKerning`,
  `queryFontPairKerning`, `debugFontPair` and `dumpFontLayout`.
- `NanoUI.Sdl.Session` is no longer an exposed module.

## 0.1.0.1 -- 2026-09-18

* 

## 0.1.0.0

First release.
