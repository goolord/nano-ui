# Quick Start

This guide sets up a project and builds a small windowed application. It should take about ten minutes. The [tutorials](../tutorials/index.md) take it from there.

## Prerequisites

- **GHC 9.10 through 9.14** (development uses 9.14)
- **Cabal 3.12+**
- **SDL3 and SDL3_ttf 3.2+** for the SDL backend
- **pkg-config**

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

`nano-ui` is not on Hackage. Pin it as a Cabal `source-repository-package`. In your `cabal.project`:

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

> [!IMPORTANT]
> Compile executables with `-threaded`. The immediate-mode loop, task threads, and native window event processing require the GHC threaded runtime.

## Your first application

Create `Main.hs`:

```haskell
module Main (main) where

import Control.Monad (whenM)
import Data.Text qualified as T
import NanoUI
import NanoUI.Backend.Sdl

main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlWindowSettings =
          defaultWindowSettings {wsTitle = "Hello nano-ui"}
      }
    appView

appView :: NanoUI ()
appView = columnWith (gap 12 . padAll 20 . alignCenter) $ do
  heading "Counter Demo"

  -- State at this call site; allocated on the first frame only.
  n <- useVar (0 :: Int)

  rowWith (gap 8 . alignMid) $ do
    whenM (button "-") (modifyVar n (subtract 1))

    value <- readVar n
    labelWith (fontMono . fontSize 18) (T.pack (show value))

    whenM (button "+") (modifyVar n (+ 1))

  whenM (button "Reset") (writeVar n 0)
```

Run it:

```sh
cabal run -j1 my-app
```

`-j1` is the recommendation for this repository and for memory-constrained machines; a normal project can use cabal's defaults.

## How it works

1. **`runSdlApp`** initializes SDL3, creates a window with TrueType font rendering, and starts the frame loop.
2. **`columnWith` / `rowWith`** are layout containers. Their argument composes layout modifiers (`gap 12 . padAll 20`); the leftmost modifier wins when two set the same property.
3. **`useVar (0 :: Int)`** declares state at this position in the view. On later frames it retrieves the existing value; the initial value is used only on the first frame.
4. **`button "+"`** draws an interactive button and returns `True` only on the frame it is activated.
5. **`modifyVar n (+ 1)`** updates the value. Reading `n` below the buttons means the label shows the new value in the same pass; reading it above them would cost a second pass.

The view runs every frame, so ordinary side effects must be guarded by an event (`whenM (button "Save") save`), and per-frame resources must not be allocated inside it. [State & `Var`](../state/index.md) explains both rules in depth.

## Next steps

- Build something end to end: [Tutorial 1: Build a Todo List](../tutorials/todo-list.md).
- Understand the state model: [State & `Var`](../state/index.md).
- Find a widget: [Widget Gallery](../widgets/index.md).
- Arrange a window: [Layout & Scrolling](../layout/index.md).
- Work on nano-ui itself: [Development Setup](development.md).
