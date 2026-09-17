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

## Running

```sh
cabal run nano-ui-sdl-anim     # tween and spring animations
cabal bench nano-ui-sdl-bench  # runFrame and SDL drawing timings
```

## Requirements

SDL3 and SDL3_ttf 3.2 or later, and pkg-config. The backend is behind the `sdl`
flag, which is on by default.

On x86-64, `-f simd` compiles the draw-batch culler with AVX2. A binary built
that way needs an AVX2 CPU.
