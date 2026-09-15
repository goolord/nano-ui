# nano-ui-sdl

SDL3 window backend for [nano-ui](https://github.com/goolord/nano-ui).

```haskell
import NanoUI
import NanoUI.Backend.Sdl (defaultSdlOptions, runSdlApp)

main :: IO ()
main = runSdlApp defaultSdlOptions (label "Hello")
```

`runSdlAppReduce` runs a view against a model and an update function, for use
with `NanoUI.Emit`. The backend also opens native file dialogs, bundles the
Inter font, and can use installed fonts by family name.

## Building

You need SDL3 and SDL3_ttf 3.2 or later, and pkg-config. The backend is behind
the `sdl` flag:

```sh
cabal build -f sdl nano-ui-sdl
```

On x86-64, `-f simd` compiles the draw-batch culler with AVX2. A binary built
that way needs an AVX2 CPU.
