# my-app (nano-ui + SDL3)

Starter template for a desktop `nano-ui` application using `nano-ui-sdl` (TrueType fonts with HarfBuzz shaping, system font discovery, native file dialogs, and GPU shaders).

## Prerequisites

- GHC 9.10 through 9.14 and Cabal 3.12+
- SDL3, SDL3_ttf 3.2+, and `pkg-config`

With Nix (Linux and macOS):

```sh
nix develop
cabal run -j1 my-app
```

On Windows (MSYS2 UCRT64):

```sh
pacman -S mingw-w64-ucrt-x86_64-sdl3 mingw-w64-ucrt-x86_64-sdl3-ttf mingw-w64-ucrt-x86_64-pkg-config
```

```powershell
$env:PKG_CONFIG_PATH = "C:\msys64\ucrt64\lib\pkgconfig"
cabal run -j1 my-app
```

Want to experiment without installing SDL3? Try the `rgfw` template instead:

```sh
nix flake init -t github:goolord/nano-ui#rgfw
```
