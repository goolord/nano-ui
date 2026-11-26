# Changelog

## 0.2.0.0 -- Unreleased

See the [0.1 to 0.2 migration guide](https://goolord.github.io/nano-ui/book/getting-started/migrating-0.2.html)
for upgrade examples.

### Breaking changes

- **Concrete `NanoUI` monad and session-owned `SdlEnv`:** Views use `NanoUI`
  instead of `Ui :> es => Eff es` (dropping the `effectful-core` dependency)
  and never thread an `SdlEnv` handle. `askOpenFileDialog`,
  `askSaveFileDialog`, `askOpenFolderDialog`, `pollFileDialogUi`,
  `setSdlUiFont`, `setSdlUiScale`, `windowCaption`, `windowCaptionWith`,
  `setWindowChromeUi`, and `askSdlDebug` find the active session on the
  `Context`. Removed `runSdlAppWith` and `runSdlAppReduceWith`; allocate state
  once in `IO` and pass it to `runSdlApp` or `runSdlAppReduce`. `SdlEnv` is
  exported opaquely (`askSdlEnv` opts into IO-level helpers; fields moved to
  `NanoUI.Sdl.Internal.Window`).
- **Opaque `FileDialogId`:** Dialog launch helpers return `FileDialogId`
  directly (failures report as `FileDialogFailed` on poll). `pollFileDialogUi`
  consumes a result once and restores window focus; use `peekFileDialogUi` to
  observe without consuming.
- **Window settings and decorations:** Replaced `sdlWindowTitle`,
  `sdlWindowSize`, `sdlWindowResizable`, `sdlWindowFullscreen`, and
  `sdlWindowHidden` with `sdlWindowSettings :: WindowSettings`. Replaced
  `sdlWindowBorderless` with `sdlWindowDecorations :: WindowDecorations`
  (`DecorationsFull`, `DecorationsFrame`, `DecorationsNone`).
- **Typed reducer runner:** `runSdlAppReduce` takes `model -> NanoUIE msg ()`
  from `nano-ui-emit`.
- **Shared TTF text layer (`nano-ui-ttf`):** Extracted SDL3_ttf shaping, font
  discovery, the glyph atlas, and bundled Inter (`LICENSE-Inter`) into
  `nano-ui-ttf`. Moved `NanoUI.Sdl.Input` and `NanoUI.Sdl.NanoUIFont` to
  `NanoUI.Sdl.Internal.*` (`NanoUIFont` stays exported from
  `NanoUI.Backend.Sdl`).
- **Removed `+simd` flag:** Replaced the AVX2-only damage culler with a
  portable triangle culler; remove `+simd`/`-simd` from local project files.

### Added

- **GPU fragment shaders (`nano-ui-shader`):** Tier 1 (`shaderBox`) and
  Tier 2 (`withShader`) shader execution on OpenGL 3.2 / GLSL 150 and SDL3's
  GPU renderer, with straight-alpha blending and unshaded fallback on compile
  failure.
- **Custom title bars and native chrome:** `windowCaption`,
  `windowCaptionWith`, `CaptionOptions`, `WindowChrome`, `setWindowChrome`,
  `clearWindowChrome`, `setWindowChromeUi`, `clearWindowChromeUi`,
  `DecorationsFrame` (native Win32 resize frame and DWM shadow without a title
  bar), `setWindowDecorationsUi`, `setWindowShadowUi`, `windowResizable`, and
  `windowZoom`.
- **Platform integration:** Full core `WindowSettings` and view-side window
  commands, `captureScreenshot`, `nativeWindow` live-resize support on
  Windows, IME text composition (`useInputMethod`), full `Key` and
  `MouseButton` support, system light/dark theme following (`sdlAppThemeFor`),
  `sdlRenderDriver` (`RenderDriverAuto`, `RenderDriverSdlDefault`,
  `RenderDriverNamed`), `sdlExplainLayout`, `CoreDebugSnapshot` formatters
  (`formatFpsRows`, `formatDrawRows`, `formatExplainRows`),
  `resetRenderTextures`, and the `nano-ui-sdl-idle` measurement tool.

### Changed

- **Windows OpenGL default and smooth live resize:** `RenderDriverAuto` picks
  `"opengl"` on Windows (with native GLSL 150 shaders) and installs a Win32
  resize subclass to avoid D3D11 flip-model resize stalls and Vulkan WSI
  stretch artifacts.
- **Typography and font search:** Uses real bold faces when available beside
  the regular font, applies full hinting to monospace fonts, splits line gap
  above and below glyphs like CSS, searches XDG/fontconfig directories on Unix,
  walks font directories in the background, skips unreadable folders, and
  includes Windows/macOS fallback aliases for CJK, Indic, symbols, and serif.
- **Performance and dependencies:** Dynamic per-font shaping caches, caching
  for lines over 4096 bytes, dirty-rect CPU atlas uploads with fast resets,
  SDL 3.4 clamp texture addressing, skipping damage diffs on continuous frames,
  `Data.Acquire` session teardown, and support for GHC 9.10 through 9.14 and
  `sdl3-bindgen-sys >=0.0.0.2 && <0.0.2`. Dropped `dir-traverse`, `hashable`,
  and `unordered-containers`.

### Fixed

- Paced animations and continuous sessions by the active display's refresh
  rate (including fractional rates and monitor moves) when drivers disable
  vsync.
- Fixed OpenGL ES 2 framebuffer errors after closing secondary windows,
  Direct3D render device resets (`EvRenderDeviceReset`), fractional-DPI
  retained framebuffer and clip alignment, Wayland-to-X11 driver fallback,
  un-clamped Wayland tiling resizes, UTF-8 file-dialog filters under C
  locales, coalesced cross-thread wakes, color-emoji tinting, and exception
  propagation out of live-resize callbacks.

## 0.1.0.1 -- 2026-09-18

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

### Fixed

- A resize drag no longer trails the border by a step. Each step was drawn
  on `SDL_EVENT_WINDOW_RESIZED`, before the renderer had resized its swap
  chain, so it went to the old-size backbuffer and showed cropped or with a
  bare strip; frames are now drawn on the pixel size change that follows.

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
