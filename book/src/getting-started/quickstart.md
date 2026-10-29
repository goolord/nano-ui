# Quick Start

Set up a project and open a window. The [tutorials](../tutorials/index.md) continue from here.

## Prerequisites

- GHC 9.10 through 9.14 (development uses 9.14)
- Cabal 3.12+
- SDL3 and SDL3_ttf 3.2+ for the SDL backend
- pkg-config

### Nix (Linux / macOS)

In the nano-ui repository, `nix develop` provides GHC, Cabal, SDL3, SDL3_ttf, and pkg-config.

### Windows

Install SDL3 and SDL3_ttf through MSYS2 UCRT64:

```sh
pacman -S mingw-w64-ucrt-x86_64-sdl3 mingw-w64-ucrt-x86_64-sdl3-ttf mingw-w64-ucrt-x86_64-pkg-config
```

Then point `PKG_CONFIG_PATH` at MSYS2's pkgconfig directory:

```powershell
$env:PKG_CONFIG_PATH = "C:\msys64\ucrt64\lib\pkgconfig"
```

## Add nano-ui to a project

`nano-ui` is on Hackage, but this book follows the repository, which may be ahead of the release. Pin the source in `cabal.project`:

```cabal
source-repository-package
  type: git
  location: https://github.com/goolord/nano-ui
  subdir:
    packages/nano-ui
    packages/nano-ui-sdl

package nano-ui-sdl
  flags: +sdl
```

Add the other packages you need (`nano-ui-emit`, `nano-ui-diagrams`, and so on) to the `subdir` list and to your `.cabal` dependencies.

In `my-app.cabal`:

```cabal
executable my-app
  main-is: Main.hs
  default-language: GHC2024
  default-extensions: OverloadedStrings
  ghc-options: -threaded -Wall
  build-depends:
      base >= 4.20 && < 5
    , nano-ui
    , nano-ui-sdl
    , text
```

### Haddocks and Hoogle

Hackage has Haddocks for the released version. For the current source, from a checkout:

```sh
cabal haddock-project --output=haddocks --hoogle
```

Open `haddocks/index.html`. Building every package is memory-heavy; `cabal haddock nano-ui --haddock-hoogle` builds one. See [Development Setup](development.md#haddocks-and-hoogle) for Hoogle indexing.

> [!IMPORTANT]
> Compile executables with `-threaded`. The immediate-mode loop, task threads, and native window event processing require the GHC threaded runtime.

## Your first application

Create `Main.hs`:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Quickstart.hs:quickstart}}
```

Run it:

```sh
cabal run -j1 my-app
```

`-j1` keeps memory down; a normal project can use cabal's defaults.

## How it works

1. `runSdlApp` initializes SDL3, creates a window with TrueType font rendering, and starts the frame loop.
2. `columnWith` and `rowWith` are layout containers. Their argument composes layout modifiers (`gap 12 . padAll 20`); the leftmost modifier wins when two set the same property.
3. `useVar (0 :: Int)` allocates a `Var` on the first frame and returns the same `Var` on later frames, matched by its call position in the view. The initial value is used only on the first frame.
4. `button "+"` draws an interactive button and returns `True` only on the frame it is activated.
5. `modifyVar n (+ 1)` updates the value. Reading `n` below the buttons means the label shows the new value in the same pass; reading it above them would cost a second pass.

The view runs every frame, so two rules follow:

- Guard side effects with an event: `whenM (button "Save") save`. `button "Save" >> save` saves every frame.
- Allocate long-lived resources (`newVar`, tasks, caches) in `IO` before `runSdlApp`, never inside the view.

Next: [Tutorial 1: Build a Todo List](../tutorials/todo-list.md).
