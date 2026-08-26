# nano-ui

Purely functional immediate-mode GUI core for Haskell. Backend-agnostic: emits batched vertex/index draw lists in pinned off-heap memory.

## Features

- **SrcLoc IDs**: `HasCallStack` hashing for stable widget identity without manual ID stacks
- **Two-pass flex layout**: measure/position over struct-of-arrays node arena
- **Zero-allocation draw path**: pinned `ForeignPtr` vertex/index arenas reused each frame
- **Damage tracking**: `needsRedraw` gates on commands, hover target change, `markDirty`, or active animation. Mouse motion on the same widget is skipped. SDL presents only the dirty rect for hover and animation frames.
- **Headless verification**: ASCII renderer + golden-style tests, no window/GL dependency

## Build

```bash
cabal build
cabal test
cabal run nano-ui-demo
```

## Nix

Linux and macOS only (Nix does not replace the Windows MSYS2/Zig workflow).

```bash
nix develop          # GHC 9.14, cabal, HLS, SDL3, SDL3_ttf, pkg-config; sdl flag on
nix build            # build all wired packages
nix run .#nano-ui-sdl-demo
nix run .#nano-ui-tui
nix flake check      # run the test suite
```

## Quickstart

```haskell
import NanoUI

main :: IO ()
main = do
  ctx <- newContext
  let inp = emptyInput { inputWindowSize = Size 800 600 }
  (_, msgs, drawData) <-
    runFrame ctx inp $
      column (grow defaultLayout) $
        do
          button "OK"
          label "Hello"
  mapM_ putStrLn (renderASCII 80 24 drawData)
```

## RTS flags

For interactive 60fps apps, use the non-moving GC and latency-tuned flags:

```bash
+RTS -N --nonmoving-gc -qb0
```

The demo and TUI set these via cabal `-with-rtsopts`.

### Terminal backend

```bash
cabal run nano-ui-tui
```

Uses `nano-ui-term` (`runTermApp`, `newTerminalContext`) with 1-cell font metrics.

**Windows** — native Win32 console API (no notcurses). Frames are cell-rasterised
and diffed to ANSI; mouse hover uses console input records. Only `Win32` is
required; MSYS2 UCRT64 is the usual build path:

```bash
cabal run nano-ui-tui
```

**Linux / macOS / Nix** — [notcurses](https://github.com/dankamongmen/notcurses)
handles rendering and input. **Requires `notcurses-core`** (pkg-config); there is
no VT-only fallback on POSIX. Use `nix develop` or install the library before
building. Changed cells are patched each frame; a full plane erase also runs on
resize, dimension mismatch, or when a cell update fails mid-blit.
### SDL3 backend

Requires SDL3, SDL3_ttf, and `pkg-config`. On Windows, MSYS2 UCRT64 is the usual path:

```bash
pacman -S mingw-w64-ucrt-x86_64-sdl3 mingw-w64-ucrt-x86_64-sdl3-ttf mingw-w64-ucrt-x86_64-pkg-config
```

Text uses a system TrueType font (Segoe UI on Windows). Override with `NANO_UI_FONT=/path/to/font.ttf`.

```bash
cabal build -fsdl
cabal run -fsdl nano-ui-sdl-demo
```

Uses `nano-ui-sdl` (`runSdlApp`, `newSdlContext`) with SDL_ttf text rendering.
Window DPI is read via `SDL_GetWindowDisplayScale`; fonts and geometry rasterize
at native pixel density while layout stays in logical coordinates.
The backend renders pinned `DrawData` quads through SDL3's 2D renderer, sorts
draw commands by layer (background → content → overlay), and skips `runFrame`
when idle (`SDL_WaitEvent` until a command, hover change, `markDirty`, or animation).
Hover and animation frames scissor to the dirty rect and blit only that box from the retain buffer.
Debug HUD refreshes at 4 Hz instead of every frame.

Build with Zig as the C compiler (MSYS2 UCRT64 for SDL3 + pkg-config):

```powershell
$env:PKG_CONFIG_PATH = "C:\msys64\ucrt64\lib\pkgconfig"
$env:PATH = "C:\msys64\ucrt64\bin;$env:PATH"
$env:CC = "zig cc -target x86_64-windows-gnu"
$env:CXX = "zig c++ -target x86_64-windows-gnu"
cabal build -fsdl
cabal run -fsdl nano-ui-sdl-demo
```

## Architecture

```
UI monad → node arena → layout solver → shape lowering → DrawData
                ↑                              ↓
         persistent Context (hot/active/focus, prev rects, store)
```

## Modules

| Module | Role |
|--------|------|
| `NanoUI` | Public API re-export |
| `NanoUI.Monad` | `UI` monad, `emit`, `withKey`, `currentId` |
| `NanoUI.Widgets` | `button`, `checkbox`, `slider lbl min max initial`, `textInput`, `panel` / `row` / `column`, `useFlag` / `useText` |
| `NanoUI.Frame` | `runFrame`, `needsRedraw` |
| `NanoUI.Draw` | Pinned vertex arena, draw command batching |
| `NanoUI.Layout.Solve` | Two-pass flexbox constraint solver |
| `NanoUI.Render.ASCII` | Headless ASCII rasterizer |
| `NanoUI.Backend.Term` | `runTermApp`, terminal event loop |
| `NanoUI.Term.Cells` | Draw commands and spans to a terminal cell grid |
| `NanoUI.Term.Ansi` | Cell grid to ANSI bytes, diffed against the last frame |
| `NanoUI.Term.Vt` | Incremental VT input decoder, including mouse motion |

## License

MIT
