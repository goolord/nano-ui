# nano-ui

Purely functional immediate-mode GUI core for Haskell. Backend-agnostic: emits batched vertex/index draw lists in pinned off-heap memory.

## Features

- **SrcLoc IDs**: `HasCallStack` hashing for stable widget identity without manual ID stacks
- **Typed reducers**: `emit` collects messages; `reduceMessages` / `runFrameReduce` apply them to app state at frame end
- **Two-pass flex layout**: measure/position over struct-of-arrays node arena. `percent` and `aspect` (width / height) are first-class constraints
- **Compact regions**: `compactHost` / `askCompact` keep large read-heavy app state off the GC walk
- **Zero-allocation draw path**: pinned `ForeignPtr` vertex/index arenas reused each frame
- **Damage tracking**: `needsRedraw` gates on commands, hover target change, scroll drag, focused text field, `markDirty`, or active animation. Mouse motion on the same widget is skipped. Scroll or widget-store text changes force a full redraw. SDL scissors hover and animation into the retain texture; partial damage blits only the dirty rect to the window.
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

Uses `nano-ui-term` (`runTermApp`, `newTerminalContext` from `NanoUI.Backend.Term`) with `CellHost` metrics.

**Linux / macOS / Nix** uses [notcurses](https://github.com/dankamongmen/notcurses). **`notcurses-core` is required** (pkg-config). Changed cells are patched each frame; a full plane erase also runs on resize, dimension mismatch, or when a cell update fails mid-blit.

```bash
cabal run nano-ui-tui
```

**Windows** uses the native Win32 console API in CMD, PowerShell, and Windows Terminal. Frames are cell-rasterised and diffed to ANSI. No notcurses, no MSYS2 wrapper.

```bash
cabal run nano-ui-tui
```

#### Nerd Font and Font Awesome icons

With a Nerd Font (or Font Awesome) the TUI draws its chrome with glyphs instead of brackets: checkbox, close button, select caret, and window/modal title marks. All of them come from the Font Awesome block (U+F000 to U+F2E0), which every Nerd Font ships. In the terminal those icons occupy two cells each; layout and rasterisation use column counts, not `Text` length.

A terminal cannot report which font is loaded, and a missing glyph renders as a same-width box, so there is nothing to probe. The tier is read from the environment and stays ASCII unless something says otherwise:

1. `NANOUI_ICONS=nerd | fontawesome | ascii | auto`
2. `NERD_FONT` / `NERDFONT` / `NERD_FONTS` set to `1`, `true`, `yes`, or `on`
3. A terminal that ships a Nerd Font by default (WezTerm, Ghostty)

```bash
NANOUI_ICONS=nerd cabal run nano-ui-tui
```

To choose in code instead, `withIcons ctx IconsNerd` before `runTermApp`. An explicit choice is never overwritten by detection.

### SDL3 backend

Requires SDL3, SDL3_ttf, and `pkg-config`. Unlike the TUI, SDL needs no MSYS2 wrapper: it builds and runs natively.

```bash
cabal run -fsdl nano-ui-sdl-demo
```

On Windows, install the UCRT64 packages and keep `<msys2>\ucrt64\bin` on PATH.

```bash
pacman -S mingw-w64-ucrt-x86_64-sdl3 mingw-w64-ucrt-x86_64-sdl3-ttf
```

That one PATH entry covers both steps: `pkg-config.exe` resolves `sdl3.pc` at configure time, and `SDL3.dll` loads at runtime. Configure reads `sdl3.pc`, never the DLL, so copying `SDL3.dll` next to the exe does not fix a resolver error.

SDL benchmarks statically link SDL3 (`executable-static` in `cabal.project`). The demo and library use dynamic SDL DLLs.

Text uses a system TrueType font (Adwaita, Liberation, or Noto on Linux; Segoe UI on Windows). Override with `NANO_UI_FONT=/path/to/font.ttf`.

Profile the SDL demo draw path (hidden window, 400 timed frames):

```powershell
cd profiles
.\run-sdl-profile.ps1
```

Uses `nano-ui-sdl-profile` with RTS `-pj` / `-P`. Open `profile-sdl-json.prof` in [speedscope](https://www.speedscope.app/).

Uses `nano-ui-sdl` (`runSdlApp`, `newSdlContext` from `NanoUI.Backend.Sdl`) with SDL_ttf text rendering.
Window DPI is read via `SDL_GetWindowDisplayScale`; fonts and geometry rasterize
at native pixel density while layout stays in logical coordinates.
The backend renders pinned `DrawData` quads through SDL3's 2D renderer, sorts
draw commands by layer (background → content → overlay), and skips `runFrame`
when idle (`SDL_WaitEvent` until a command, hover change, `markDirty`, or animation).
Cross-thread `markDirty` wakes the loop via a registered SDL user event (`runSdlApp` wires this automatically).
Hover and animation frames scissor into the retain texture. Partial damage blits only the dirty rect to the window.
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
NanoUI → node arena → layout solver → shape lowering → DrawData
                ↑                              ↓
         persistent Context (HostProfile, hot/active/focus, prev rects, store)
```

Workspace packages: `nano-ui` (core), `nano-ui-term` (`term-lib`), `nano-ui-sdl` (`sdl-lib`, `-fsdl`). Backends set `CellHost` or `PixelHost` on `Context`. Core does not name terminal or SDL.

## Modules

| Module | Role |
|--------|------|
| `NanoUI` | Public API re-export |
| `NanoUI.Host` | `HostProfile` (`PixelHost` / `CellHost`) |
| `NanoUI.Monad` | `NanoUI` effect stack, `emit`, `withKey`, `currentId` |
| `NanoUI.Widgets` | `button`, `checkbox`, `slider lbl min max initial`, `textInput`, `panel` / `row` / `column`, `useFlag` / `useText` |
| `NanoUI.Frame` | `runFrame`, `runFrameReduce`, `needsRedraw` |
| `NanoUI.Draw` | Pinned vertex arena, draw command batching |
| `NanoUI.Layout.Solve` | Two-pass flexbox constraint solver |
| `NanoUI.Render.ASCII` | Headless ASCII rasterizer |
| `NanoUI.Backend.Term` | `runTermApp`, `newTerminalContext`, terminal event loop |
| `NanoUI.Term.Cells` | Draw commands and spans to a terminal cell grid |
| `NanoUI.Term.Ansi` | Cell grid to ANSI bytes, diffed against the last frame |
| `NanoUI.Term.Vt` | Incremental VT input decoder, including mouse motion |
| `NanoUI.Backend.Sdl` | `runSdlApp`, `newSdlContext`, SDL3 event loop |

## License

MIT
