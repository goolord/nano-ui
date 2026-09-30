# Changelog

## 0.2.0.0 -- Unreleased

- **Breaking:** the application facade exports `SdlEnv` opaquely. Native
  integrations that access its record fields must explicitly import
  `NanoUI.Sdl.Internal.Window`; ordinary views use the session operations.

- **Breaking:** `FileDialogId` is opaque and session-owned. `pollFileDialogUi`
  takes the session environment and consumes results once, restoring window
  focus just like `pollFileDialog`. Use `peekFileDialogUi` for observation.
  Dialog launch helpers return `FileDialogId`, not an always-`Just` value;
  failures are reported through `FileDialogFailed` when polled.

- Direct-backbuffer screenshots invoke callbacks after presentation, matching
  retained frames. Session teardown closes pending screenshot requests.

- **Breaking:** `runSdlAppReduce` takes `model -> NanoUIE msg ()` from the new
  `nano-ui-emit` package, without `Typeable` or runtime message filtering.

- **Breaking:** SDL-specific debug, font/scale, dialog launch, and chrome
  helpers take `SdlEnv` explicitly. `runSdlAppWith` takes an
  `SdlEnv -> NanoUI ()` view; capture application state in its closure.
  Dialog polling takes the same environment;
  the session no longer installs itself in a dynamic host registry.

### Added

- `runSdlAppReduceWith` supplies the live SDL environment to typed reducer
  views, preserving access to dialogs, debug information and window chrome.

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
- `windowResizable`, and `setWindowChromeUi` for handing over chrome regions
  from a view. The title, size, mode, maximizing and minimizing are the
  core's, for any backend (`setWindowTitleUi`, `resizeWindowUi`,
  `toggleMaximizedUi` and the rest); `windowCaption`'s buttons use them.
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
  The window is made that much larger, so `wsSize` is still the size
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
- The new cursor shapes, as SDL's system cursors or the nearest it has; help,
  zoom, copy, alias and context-menu show the arrow.
- `sdlExplainLayout` opens a window with the layout overlay on.
- Every key comes in as a `Key`, with its release and held state: F1 to F24,
  paging, Insert, Space, PrintScreen, Pause, the lock and menu keys, the
  keypad, and a typing key as the `KeyChar` it types in the current layout.
  The GUI key is `modSuper`. Each auto-repeat of a held key is a press,
  which `inputKeysNew` leaves out, and the window losing the keyboard lets
  go of the keys held.
- Input methods compose inside the text fields, with the candidate window by
  the caret. Text input runs only while a widget takes text (a focused text
  field, or a widget that calls `useInputMethod`), with SDL's text input type
  for what it takes, so a password's input method hides it and a number
  gets a numeric on-screen keyboard; with nothing taking text it stops, so
  no composition builds up unseen and no on-screen keyboard stays up. A
  widget of your own that reads typed text, such as a terminal, asks with
  `useInputMethod` and draws the composition it answers;
  `SDL_IME_IMPLEMENTED_UI=none` in the environment lets the input method draw
  it again.
- The middle mouse button, the side buttons as back and forward, and any
  further button as a `MouseOther` of its SDL number. The pointer leaving
  the window (`EvMouseLeave`) moves it off every widget, so nothing stays
  hovered. `UiCursorHidden` hides the pointer.
- The desktop's light or dark setting reaches `systemAppearance`, and a change
  arrives as `EvSystemThemeChanged`. `sdlAppThemeFor` takes the theme for
  each setting, such as `lightDark light dark`, and follows it as it
  changes.
- The core's window, in full: the window opens from `sdlWindowSettings`, a
  core `WindowSettings`, which adds a position, size limits, an icon, a
  mode, transparency, opacity and whether a close request ends the session.
  Views change it with the core's setters and commands (`setWindowTitleUi`,
  `setWindowModeUi`, `moveWindowUi`, `resizeWindowUi` and the rest), read it
  with `askWindow`, and end the session with `quitUi`. A transparent window
  (`wsTransparent`) shows the desktop where the theme's window colour is
  translucent.
- `captureScreenshot`, the last presented frame as a `Screenshot`, and
  answers to the core's `requestScreenshot`.
- The jobs a view's `useTask` hooks started end with the session.

### Changed

- Widgets and view functions have `NanoUI` types instead of
  `Ui :> es => Eff es`, and the package no longer depends on `effectful-core`.
- Builds with GHC 9.10 through 9.14 (`base >=4.20 && <4.23`).
- With SDL 3.4 or later, the renderer's texture address mode is set to clamp
  when the session starts. Left on auto, SDL scans every UV in the frame's
  vertex buffer on each draw call, which cost more than a quarter of a
  full-window frame. SDL 3.2 has no such setting, so there each draw call
  passes SDL only the vertices it uses: submitting the demo's Controls tab
  went from 0.77 to 0.22 ms a frame.
- The text caches use the core's generational cache, so the package no longer
  depends on `hashable` or `unordered-containers`.
- A frame presented in full, as every frame of a continuous session is, no
  longer works out what changed since the last one: it clears the core's
  `ctxDamageWanted` for its UI pass, and sets it back even when the pass
  throws. Scrolling a 3000-row list continuously takes about 20% less time
  a frame on the Haskell side. An idle frame still works it out, cheaply,
  so it can hand back the last frame's draw data instead of painting.
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
- A font's caches of prepared and drawn texts hold twice what the last frame
  looked up in them, and 1024 at least, where they held 1024. A frame that
  drew more distinct texts in one font rotated each out before the next
  frame drew it, and shaped and placed every one again: one label of 2000
  lines went from 57 ms and 83 MB allocated a frame to 5.9 ms and 8.2 MB,
  and a grid of 2400 labels from 16.7 ms to 4.0 ms.
- `NanoUI.Sdl.Input` and `NanoUI.Sdl.NanoUIFont` are now
  `NanoUI.Sdl.Internal.Input` and `NanoUI.Sdl.Internal.NanoUIFont`.
  `NanoUIFont` is still exported from `NanoUI.Backend.Sdl`.
- Glyph surfaces upload directly to SDL's streaming atlas. SDL owns its
  initialization/reset storage; the backend no longer maintains a second
  full-size CPU pixel buffer.

- `sdlWindowBorderless` is replaced by `sdlWindowDecorations`:
  `DecorationsFull` for `False`, and `DecorationsNone` for what `True` did.
- `sdlWindowTitle`, `sdlWindowSize`, `sdlWindowResizable`,
  `sdlWindowFullscreen` and `sdlWindowHidden` are replaced by
  `sdlWindowSettings`, the core `WindowSettings` both backends open their
  windows from: `sdlWindowSettings = defaultWindowSettings {wsTitle = t,
  wsSize = s, wsResizable = r}`, and `wsMode = Fullscreen` or `wsMode =
  Hidden` for the two flags. `sdlWindowDecorations` and
  `sdlWindowAlwaysOnTop` stay, as the SDL window's own.

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
- A key held with Ctrl is a key chord (`KeyChar` with `modCtrl`) instead of
  its symbol typed into `inputChars`.

### Fixed

- A background job's wake whose refresh event could not be queued (SDL's
  queue was full) no longer stops later wakes from waking the loop until
  other input arrives.
- A minimum size set above the maximum, or a maximum below the minimum, on
  X11, Windows or macOS takes effect, moving the other limit to it as on
  Wayland. SDL rejected the pair and the new limit was lost.
- Resizing a Wayland window past its minimum or maximum size no longer
  stutters under a compositor that ignores the limits, such as sway's tiling.
  SDL clamped each such configure back to the current size without reporting
  it, so no frame committed it and sway waited out its 200 ms transaction
  timeout on every step of the drag. The limits now go to the compositor
  without SDL clamping to them, and the window takes the size it is given.

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
- The grab hands show the move arrows. The backend asked SDL 3.2 for cursors
  it does not have, which showed the arrow on X11.

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
