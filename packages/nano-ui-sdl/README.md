# nano-ui-sdl

SDL3 window backend for [nano-ui](https://github.com/goolord/nano-ui). Text is
shaped with SDL_ttf and HarfBuzz, drawn from the bundled Inter font or installed
fonts looked up by family name, with fallback fonts for other scripts. The
backend also opens native file dialogs.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import NanoUI
import NanoUI.Backend.Sdl (defaultSdlOptions, runSdlApp)

main :: IO ()
main = runSdlApp defaultSdlOptions (label "Hello")
```

`runSdlAppReduce` pairs a `model -> NanoUIE msg ()` view with an update function
`msg -> model -> model`. Add `nano-ui-emit` and import `NanoUI.Emit` for typed
emission and widget adapters. `SdlOptions` sets the window, fonts, font size,
theme, and vsync.

SDL-specific operations are ordinary view actions. They act on the session
the view runs in, so nothing is passed to them:

```haskell
main = runSdlApp defaultSdlOptions $
  whenM (button "Reset scale") (setSdlUiScale 1)
```

`useVar` keeps local state with no setup. When a view needs explicitly owned
state or a value type `useVar` does not hold, make a `Var` once in IO, keep it
in a record, and pass it to the view:

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

`pollFileDialogUi handle` consumes a completed result once and restores
window focus. `peekFileDialogUi handle` observes without consuming. Launch
helpers return a handle directly; failures arrive as `FileDialogFailed`, as
they do for a dialog asked for outside an SDL session. `askSdlDebug`,
`setSdlUiFont`, `setSdlUiScale`, `windowCaption` and the `*Ui` chrome
setters likewise act on the open session, and do nothing without one.

To opt into the session explicitly, for the IO-level operations
(`openFileDialog`, `pollFileDialog`, `setWindowChrome`, `saveScreenshot`),
take it with `askSdlEnv`. They call SDL, so call them on the session's
display thread; only `cancelFileDialog` is safe from another. `SdlEnv` is
an opaque session capability. Its mutable caches and native pointers are not
application API. Low-level render integrations and native tests that need
those details explicitly import `NanoUI.Sdl.Internal.Window`, whose record
representation is unstable.

## Windows and screenshots

`sdlWindowSettings` uses the core's `WindowSettings` (shared with the RGFW
backend): title, size in layout units, position, size limits, icon, mode,
transparency, opacity, and whether a close request ends the session.

```haskell
runSdlApp
  defaultSdlOptions
    {sdlWindowSettings = defaultWindowSettings {wsTitle = "Notes", wsSize = Size 900 600}}
  notesView
```

`askWindow` reads the current window state, and the core's setters and
commands (`setWindowTitleUi`, `setWindowModeUi`, `moveWindowUi`,
`resizeWindowUi`, and the rest) update it. `sdlWindowDecorations` and
`windowCaption` configure custom title bars. A transparent window
(`wsTransparent`) shows the desktop where the theme's window colour is
translucent; the software renderer adds up the alpha where translucent
colours overlap. `captureScreenshot` returns the last presented frame as a
`Screenshot`, and `saveScreenshot` writes it to a BMP file.

## Add to an application

Use GHC 9.10 or later and include `nano-ui` and `nano-ui-sdl` in `build-depends`.
Set `default-language: GHC2024`; the example enables `OverloadedStrings`
for text literals. Build the executable with `-threaded` for SDL callbacks.

The high-level runners own the window and renderer until the window closes.
For an existing event loop, use the session operations exported by
`NanoUI.Backend.Sdl`. Keep window, renderer, and font operations on the
session's display thread, and close the session when finished.

## Fonts and coordinates

`SdlOptions` selects the initial font and logical font size. `NanoUIFont`
defines font choices, and `setSdlUiFont` requests a change during a session.
Font discovery can use installed families; a family present on one machine
may be absent on another. The bundled Inter font provides the default.

Views use logical pixels. The backend converts input and drawing to the
window's display scale, including changes when the window moves to another
monitor. Custom widgets should use the logical rectangle supplied by nano-ui.

## Running

```sh
cabal run nano-ui-sdl-anim     # tween and spring animations
cabal bench nano-ui-sdl-bench  # runFrame and SDL drawing timings
```

## Requirements

SDL3 and SDL3_ttf 3.2 or later, and pkg-config. The backend is behind the `sdl`
flag, which is on by default.

Geometry submission uses SDL3 with conservative triangle damage rejection;
it does not require AVX2.

From this package's source directory, run `cabal build` after installing the
native libraries. Check discovery with `pkg-config --modversion sdl3 sdl3-ttf`.

See the [development guide](https://github.com/goolord/nano-ui/blob/main/docs/development.md)
for tests, profiling, and building the complete repository.

## License

MIT, except the bundled Inter font, which is under the SIL Open Font License
1.1 (`LICENSE-Inter`). Every application built with this package embeds the
font, so ship that notice with it.
