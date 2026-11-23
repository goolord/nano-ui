# Quick Start

This page walks through installing dependencies, adding `nano-ui` to a Cabal project, and opening your first window. Once it runs, head to the [tutorials](../tutorials/index.md) to build complete applications.

## Prerequisites

You will need:

- GHC 9.10 through 9.14 (development uses 9.14)
- Cabal 3.12 or newer
- SDL3 and SDL3_ttf 3.2+ (for the SDL desktop backend)
- `pkg-config`

### Nix (Linux and macOS)

Inside a checkout of the `nano-ui` repository, `nix develop` drops you into a shell with GHC, Cabal, SDL3, SDL3_ttf, and `pkg-config` already configured.

### Windows

Install SDL3 and SDL3_ttf through MSYS2 UCRT64:

```sh
pacman -S mingw-w64-ucrt-x86_64-sdl3 mingw-w64-ucrt-x86_64-sdl3-ttf mingw-w64-ucrt-x86_64-pkg-config
```

Then point `PKG_CONFIG_PATH` at the MSYS2 `pkgconfig` directory:

```powershell
$env:PKG_CONFIG_PATH = "C:\msys64\ucrt64\lib\pkgconfig"
```

## Adding nano-ui to your project

Although `nano-ui` is published on Hackage, this book tracks the git repository, which is often ahead of the latest release. Pin the repository in your `cabal.project`:

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

If you want optional companion packages such as `nano-ui-emit` or `nano-ui-diagrams`, list them in both `subdir` and your `.cabal` file's `build-depends`.

Next, configure your executable in `my-app.cabal`:

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

> [!IMPORTANT]
> Always compile executables with `-threaded`. Between frames, the backend sleeps inside SDL's foreign event-wait call. On the non-threaded runtime, that foreign call stalls every Haskell thread, so `Task` and `Stream` workers freeze whenever the window is idle.

### Haddocks and Hoogle

Hackage hosts Haddock pages for the released version. To build local documentation from a git checkout, run:

```sh
cabal haddock-project --output=haddocks --hoogle
```

Then open `haddocks/index.html`. Building docs for the entire workspace takes substantial memory; run `cabal haddock nano-ui --haddock-hoogle` if you only need the core library. See [Development Setup](development.md#haddocks-and-hoogle) for indexing the output with Hoogle.

## Your first application

Create `Main.hs`:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Quickstart.hs:quickstart}}
```

Build and run it:

```sh
cabal run -j1 my-app
```

Passing `-j1` keeps GHC's peak memory usage modest during the initial build of the library. Once dependencies are built, regular `cabal run` works fine.

## How it works

1. `runSdlApp` initializes SDL3, opens a window with TrueType text rendering, and starts the event loop.
2. `columnWith` and `rowWith` are layout containers. Their first argument is a chain of layout modifiers (`gap 12 . padAll 20`). When two modifiers set the same field, the leftmost one wins.
3. `useVar (0 :: Int)` creates a `Var` on the first frame and returns that same `Var` on every frame after, matched by its call position. The initial value is only used on frame one.
4. `button "+"` draws the button and returns `True` on the frame the user clicks it (or presses Enter/Space while it has focus).
5. `modifyVar n (+ 1)` updates the counter. Because `readVar n` runs *after* the buttons, the label immediately displays the new value in the same pass. Reading above the buttons would require a second pass.

Since the view runs on every frame, keep two habits in mind from the start:

- Gate side effects behind events: write `whenM (button "Save") save`, never `button "Save" >> save` (which would save on every frame).
- Create long-lived resources (`newVar`, background tasks, caches) once in `IO` before calling `runSdlApp`, not inside the view.

Next up: [Tutorial 1: Build a Todo List](../tutorials/todo-list.md).
