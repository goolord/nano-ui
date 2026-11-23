# nano-ui-sdl

Desktop SDL3 backend for [nano-ui](https://github.com/goolord/nano-ui). It shapes TrueType text through SDL3_ttf and HarfBuzz using either the bundled Inter font or installed system fonts (with script fallbacks), and provides non-blocking native file dialogs, custom title bars, and screenshots.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import NanoUI
import NanoUI.Backend.Sdl (defaultSdlOptions, runSdlApp)

main :: IO ()
main = runSdlApp defaultSdlOptions (label "Hello")
```

For Elm-style views, `runSdlAppReduce` pairs a `model -> NanoUIE msg ()` view with an update function `msg -> model -> model` (add `nano-ui-emit` and import `NanoUI.Emit`). Configure initial window settings, fonts, base font size, theme, and vsync through `SdlOptions`.

SDL-specific view helpers run directly inside `NanoUI` against the active session, so you never pass a backend handle down through your UI code:

```haskell
main = runSdlApp defaultSdlOptions $
  whenM (button "Reset scale") (setSdlUiScale 1)
```

Use `useVar` for view-local primitives, or allocate shared/custom state once in `IO` with `newVar` and pass it into the view:

```haskell
main = do
  picker <- newFilePicker
  runSdlApp defaultSdlOptions (filePicker picker)

newtype FilePicker = FilePicker (Var (Maybe FileDialogId))

newFilePicker :: IO FilePicker
newFilePicker = FilePicker <$> newVar Nothing

filePicker :: FilePicker -> NanoUI ()
filePicker (FilePicker pending) =
  whenM (button "Open") $
    writeVar pending . Just =<< askOpenFileDialog defaultFileDialogOptions
```

`pollFileDialogUi handle` consumes a finished dialog result once and restores focus to the main window, while `peekFileDialogUi handle` checks status without consuming it. Dialog launch helpers return a handle immediately and report errors as `FileDialogFailed` (including when called outside a live SDL session). View actions like `askSdlDebug`, `setSdlUiFont`, `setSdlUiScale`, `windowCaption`, and the `*Ui` window helpers likewise act on the active session and become no-ops in headless tests.

When you explicitly need low-level `IO` operations (`openFileDialog`, `pollFileDialog`, `setWindowChrome`, `saveScreenshot`), fetch the opaque `SdlEnv` session capability via `askSdlEnv`. Because those functions call directly into SDL, run them on the display thread (only `cancelFileDialog` is safe from another thread). Internal mutable caches and native pointers live in `NanoUI.Sdl.Internal.Window`, which is reserved for render tests and unstable across releases.

## Windows and screenshots

`sdlWindowSettings` accepts core `WindowSettings` (shared with `nano-ui-rgfw`): title, logical size, position, min/max bounds, icon, window mode, transparency, opacity, and close-request handling:

```haskell
runSdlApp
  defaultSdlOptions
    {sdlWindowSettings = defaultWindowSettings {wsTitle = "Notes", wsSize = Size 900 600}}
  notesView
```

Read live window metadata with `askWindow`, and update it via core setters and commands (`setWindowTitleUi`, `setWindowModeUi`, `moveWindowUi`, `resizeWindowUi`, and so on). For frameless custom title bars, combine `sdlWindowDecorations` with `windowCaption`.

Enabling `wsTransparent` lets the desktop show through wherever your theme's window background is translucent (note that SDL's software renderer accumulates alpha where translucent surfaces overlap). Capture the last presented frame with `captureScreenshot` (raw `Screenshot`) or `saveScreenshot` (writes a BMP file).

## Adding to an application

Target GHC 9.10 through 9.14 with `default-language: GHC2024`, add `nano-ui` and `nano-ui-sdl` to `build-depends`, and link executables with `-threaded` so background threads keep running while the SDL event wait sleeps.

High-level runners (`runSdlApp`, `runSdlAppReduce`) manage the window and renderer for the full session. If you are embedding `nano-ui` inside an existing event loop, use the lower-level session API exported from `NanoUI.Backend.Sdl`, keeping all window, renderer, and font calls on the display thread and closing the session cleanly on exit.

## Fonts and DPI scaling

Set your starting font and logical size in `SdlOptions`, or switch fonts mid-session with `setSdlUiFont`. `NanoUIFont` supports both the bundled Inter default and searching installed font families by name (note that system fonts vary by machine).

Views always work in logical pixels. On macOS and Wayland, the backend scales coordinates automatically across monitors. On Windows, SDL reports a density of `1` by default; set `sdlAppUiScale = 0` to follow the Windows desktop scale setting.

If SDL reports a recoverable render device reset (such as a Direct3D driver reset or Remote Desktop reconnect), the runner automatically rebuilds textures and repaints. Custom event loops should call `resetRenderTextures` upon receiving `EvRenderDeviceReset`. Unrecoverable device loss raises an `IOError`.

## Examples and benchmarks

```sh
cabal run nano-ui-sdl-anim     # tween and spring animations
cabal bench nano-ui-sdl-bench  # frame and SDL rendering timings
```

## System requirements

Requires SDL3, SDL3_ttf 3.2 or later, and `pkg-config`. The backend sits behind the `sdl` Cabal flag (enabled by default). Geometry submission uses standard SDL3 rendering with conservative triangle damage rejection and requires no AVX2 support.

Verify library discovery before building with:

```sh
pkg-config --modversion sdl3 sdl3-ttf
```

## Documentation

- [The nano-ui Book](https://goolord.github.io/nano-ui/book/): Tutorials, widget gallery, window management, and architecture guide.
- [Development Setup](https://goolord.github.io/nano-ui/book/getting-started/development.html): Testing, profiling, and full-workspace builds.

## License

MIT, except the bundled Inter font under the SIL Open Font License 1.1 (`LICENSE-Inter`). Because every binary built with `nano-ui-sdl` embeds Inter, ship `LICENSE-Inter` alongside your application.
