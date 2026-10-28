# Quickstart

This guide walks you through setting up a project with `nano-ui` and building your first native desktop application.

## Prerequisites & Installation

`nano-ui` requires:
- **GHC 9.10 through 9.14** (recommended: GHC 9.14)
- **Cabal 3.12+**
- **SDL3 & SDL3_ttf** (3.2+) when using the SDL backend (`nano-ui-sdl`).
- **pkg-config**

### Nix (Linux / macOS)
In this repository, running `nix develop` provides all dependencies (GHC, Cabal, SDL3, SDL3_ttf, pkg-config).

### Windows
On Windows, install SDL3 and SDL3_ttf via MSYS2 UCRT64:
```sh
pacman -S mingw-w64-ucrt-x86_64-sdl3 mingw-w64-ucrt-x86_64-sdl3-ttf mingw-w64-ucrt-x86_64-pkg-config
```
Point `PKG_CONFIG_PATH` to MSYS2's ucrt64 pkgconfig:
```powershell
$env:PKG_CONFIG_PATH = "C:\msys64\ucrt64\lib\pkgconfig"
```

### Cabal Pinning
`nano-ui` is pinned as a Cabal `source-repository-package`:

```cabal
-- In your cabal.project:
source-repository-package
  type: git
  location: https://github.com/goolord/nano-ui
  subdir:
    packages/nano-ui
    packages/nano-ui-sdl

package nano-ui-sdl
  flags: +sdl
```

In your application's `.cabal` file:

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
> Always compile executables with `-threaded`. Immediate-mode GUI loops, task threads, and native window event processing require the GHC threaded runtime.

---

## Your First Application

Create a `Main.hs`:

```haskell
module Main (main) where

import Control.Monad (whenM)
import Data.Text qualified as T
import NanoUI
import NanoUI.Backend.Sdl

main :: IO ()
main = runSdlApp defaultSdlOptions { sdlWindowSettings = defaultWindowSettings { wsTitle = "Hello nano-ui" } } appView

appView :: NanoUI ()
appView = columnWith (gap 12 . padAll 20 . alignCenter) $ do
  heading "Counter Demo"
  
  -- Positional mutable variable initialized to 0
  n <- useVar (0 :: Int)
  
  rowWith (gap 8 . alignMid) $ do
    whenM (button "-") (modifyVar n (subtract 1))
    
    val <- readVar n
    labelWith (fontMono . fontSize 18) (T.pack (show val))
    
    whenM (button "+") (modifyVar n (+ 1))
    
  whenM (button "Reset") (writeVar n 0)
```

Run with Cabal:
```sh
cabal run -j1 my-app
```

## How It Works

1. **`runSdlApp`** initializes SDL3, creates a window with TrueType font rendering, and begins the frame event loop.
2. **`columnWith` / `rowWith`** are layout containers parameterized by composable layout modifiers (`gap 12 . padAll 20`).
3. **`useVar (0 :: Int)`** declares state at this position in the view hierarchy. On subsequent frames, it retrieves the existing state.
4. **`button "+"`** renders an interactive button that returns `True` only in the frame it was clicked.
5. **`modifyVar n (+ 1)`** mutates the variable. If a variable was read earlier in the frame, nano-ui safely invalidates and re-runs the pass without repeating one-shot click events.
