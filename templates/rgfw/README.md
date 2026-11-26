# my-app (nano-ui + RGFW)

Zero-SDL starter template for a desktop `nano-ui` application using `nano-ui-rgfw`. RGFW's C source and the Cozette bitmap font are bundled inside the Cabal packages, so you do not need SDL3, SDL3_ttf, `pkg-config`, or MSYS2.

## Prerequisites

- GHC 9.10 through 9.14 and Cabal 3.12+
- OpenGL 3.2 and standard OS windowing libraries (already present on Windows and macOS; on Linux, `libx11-dev`, `libxcursor-dev`, `libxrandr-dev`, `libxi-dev`, `libgl1-mesa-dev`, or use `nix develop`)

## Run

```sh
cabal run -j1 my-app
```

When you are ready for TrueType fonts, HarfBuzz shaping, system font discovery, or native file dialogs, switch `nano-ui-rgfw` to `nano-ui-sdl` (or use `nix flake init -t github:goolord/nano-ui#sdl`).
