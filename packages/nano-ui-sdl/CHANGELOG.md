# Changelog

## Unreleased

### Added

- Everything a window without the desktop's title bar has to do for itself.
  `windowCaption` is the whole of it in one call from a view -- it draws the
  three caption buttons, minimizes and maximizes for you, tells the desktop
  which strip of the bar drags the window and which edges resize it, and
  answers whether the window was asked to close. `windowCaptionWith` takes a
  `CaptionOptions`: the buttons' size, and how far in from the window's edges
  takes hold of one to resize it. The top edge has a reach of its own, since
  it is the one edge with no frame outside it to take hold of instead. A
  window that still has the desktop's title bar gets the buttons and nothing
  else, and a fullscreen one has no bar to drag and no edges to resize.
- `WindowChrome`, `setWindowChrome` and `clearWindowChrome` for a window that
  draws chrome of another shape. The regions are given in layout units and
  converted by the window's zoom, and they go to an `SDL_SetWindowHitTest`,
  so a drag region also snaps the window to the sides of the screen,
  maximizes it on a double click, and hangs the window menu off the right
  button.
- `setWindowTitle`, `setWindowSize` (the size of the view, whatever frame
  the desktop keeps around it), `minimizeWindow`, `maximizeWindow`,
  `restoreWindow`, `toggleMaximized`, `windowMaximized` and `windowResizable`,
  with `setWindowTitleUi`, `minimizeWindowUi`, `toggleMaximizedUi`,
  `windowMaximizedUi` and `setWindowChromeUi` for calling them from a view.
- `WindowDecorations`: `DecorationsFull`, `DecorationsFrame` or
  `DecorationsNone`, how much of the desktop's title bar and frame a window
  keeps. `sdlWindowDecorations` picks it when the window opens and
  `setWindowDecorations` changes it afterwards.
  `DecorationsFrame` is for a view that draws its own title bar. On Windows a
  window with no frame is a popup: there is nothing outside its edges, so
  nothing to take hold of it by, and an application that resizes by its edges
  has to spend its own chrome on them. `DecorationsFrame` puts the ordinary
  frame back, the sizing frame's width in on every side and no caption, so
  the frame stays where it always is -- invisible, outside the window you can
  see, what the desktop resizes the window by, and what carries its shadow.
  The window is made that much larger, so `sdlWindowSize` is still the size
  of the view. Two things go with it. The desktop's own border line is taken
  off, since with no caption that line falls on the view's first row, over
  the border the view draws there. And the desktop's rounding is turned off:
  the corners it rounds are the frame's, a frame's width outside the view, so
  the view's own corners are square whatever happens there, and a rounding
  that only shapes the shadow leaves the shadow curving round a window that
  is not. A window that cannot be resized, or is fullscreen, gets DWM's
  shadow and no frame. Elsewhere it is a borderless window and the
  compositor decides.
  `DecorationsNone` keeps nothing of the desktop's; `setWindowShadow` puts
  DWM's shadow under one that wants it.
- `windowZoom`, the window coordinates a layout unit is worth.

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
- `sdlRenderDriver` and `RenderDriver`, for picking the SDL render driver a
  window asks for. `RenderDriverAuto` is the default and lets nano-ui choose,
  `RenderDriverSdlDefault` leaves SDL's own order alone, and
  `RenderDriverNamed` asks for one by name. An `SDL_RENDER_DRIVER` in the
  environment still wins over all three.

### Changed

- `SdlEnv` no longer has `sdlDialogState`: file dialogs are tracked per
  process, and every dialog shares one native callback instead of a wrapper
  apiece. `RenderDriver` is exported from `NanoUI.Backend.Sdl`, which
  `sdlRenderDriver` needed. In `NanoUI.Sdl.Internal.Input`, the four
  left/right press/release events are one `EvMouseButton`, `EvResize` and
  `EvDisplayScale` are one `EvWindowChanged`, and `waitEvent` takes a
  timeout (negative waits indefinitely) in place of `waitEventTimeout`.
- `sdlDrawFrame` answers only whether another frame is needed, not the input
  it was given as well. `FileDialogId` holds the dialog's result cell rather
  than a number: it keeps `Eq` and drops `Ord`, `Show` and the `Int`
  constructor. `SdlEnv`'s `sdlDebug` is the core `DebugSamplerRef`, beside
  `sdlDebugSnapshot` and `sdlFrameTrace`.
- Font variants other than `FontMono` share the sans font at a size instead
  of each opening its own copy.
- A line over 4096 bytes is cached like any other, counting one entry per
  4096 bytes against the shaping caches' limits, where it used to be shaped
  again on every paint. Glyphs outside the clip emit no quads. Repainting a
  text area that holds a 20,000-character line went from 6.1 ms and 8 MB
  allocated a frame to 0.12 ms and 15 KB in `nano-ui-sdl-profile`.
- `NanoUI.Sdl.Input` and `NanoUI.Sdl.NanoUIFont` are now
  `NanoUI.Sdl.Internal.Input` and `NanoUI.Sdl.Internal.NanoUIFont`.
  `NanoUIFont` is still exported from `NanoUI.Backend.Sdl`.
- Glyph surfaces upload directly to SDL's streaming atlas. SDL owns its
  initialization/reset storage; the backend no longer maintains a second
  full-size CPU pixel buffer.

- `sdlWindowBorderless` is replaced by `sdlWindowDecorations`:
  `DecorationsFull` for `False`, and `DecorationsNone` for what `True` did.

- Windows windows render through OpenGL rather than D3D11. D3D11 presents
  through a flip-model swap chain, so a present blocks for about a refresh
  even with vsync off; during a border drag those stalls land in Windows'
  modal size loop and the window judders. Set
  `sdlRenderDriver = RenderDriverSdlDefault` for the old behavior. A machine
  whose GL will not create a context opens on SDL's own choice instead.
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
- Command rendering uses the core's unboxed vectors, traversing the already
  layer-ordered command stream directly.
- Session resources are composed with `Data.Acquire` from `resourcet`; each
  acquisition carries its release action. This adds no per-frame resource layer.
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

- Partial redraws retain independent triangles and a command's final triangle.
  Damage rejection checks all three vertices rather than assuming quad pairs.

- A failed window/renderer acquisition shuts down SDL's initialized subsystems.
  Resources acquired before later startup failures are released too.

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

- The `simd` Cabal flag and AVX2-only damage culler. Remove `+simd`/`-simd`
  from local project flags; the triangle culler works on every supported CPU.

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
