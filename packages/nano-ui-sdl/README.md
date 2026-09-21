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

`runSdlAppReduce` runs a view against a model and an update function, for use
with `NanoUI.Emit`. `SdlOptions` sets the window, fonts, font size, theme, and
vsync.

## Add to an application

Use GHC 9.14 and include `nano-ui` and `nano-ui-sdl` in `build-depends`.
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
The repository's `cabal.project` enables SIMD; the published package defaults
to the scalar culler.

See the [development guide](https://github.com/goolord/nano-ui/blob/main/docs/development.md)
for tests, profiling, and building the complete repository.
