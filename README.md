# nano-ui

Purely functional immediate-mode GUI core for Haskell. Backend-agnostic: emits batched vertex/index draw lists in pinned off-heap memory.

## Features

- **SrcLoc IDs**: `HasCallStack` hashing for stable widget identity without manual ID stacks
- **Typed reducers**: `emit` collects messages; backend `run*AppReduce` applies them to app state at frame end
- **Two-pass flex layout**: measure/position over struct-of-arrays node arena. `percent` and `aspect` (width / height) are first-class constraints
- **Compact regions**: `compactHost` / `askCompact` keep large read-heavy app state off the GC walk
- **Zero-allocation draw path**: pinned `ForeignPtr` vertex/index arenas reused each frame
- **Damage tracking**: idle rendering skips work until input, hover, scroll drag, focused text field, `markDirty`, or animation demands a redraw
- **Headless verification**: `NanoUI.Testing` provides deterministic frames and ASCII render checks

## Build

```bash
cabal build
cabal test
cabal run nano-ui-demo
```

On Linux/macOS, pass `-fnotcurses` for `nano-ui-term` tests (`cabal test -fnotcurses`). SDL and Windows do not need notcurses.

## Nix

Linux and macOS only (Nix does not replace the Windows MSYS2/Zig workflow).

```bash
nix develop          # GHC 9.14, cabal, HLS, SDL3, SDL3_ttf, pkg-config; sdl flag on
nix build            # build all wired packages
nix run .#nano-ui-sdl-demo
nix run .#nano-ui-sdl-anim
nix run .#nano-ui-tui
nix flake check      # run the test suite
```

## Quickstart

Application UI lives in `NanoUI`. Runtime loops live in backend packages.

```haskell
import NanoUI
import NanoUI.Backend.Term (TermOptions (..), defaultTermOptions, runTermApp)

main :: IO ()
main =
  runTermApp
    defaultTermOptions
      { termAppShouldQuit = \inp -> KeyEscape `elem` inputKeys inp
      }
    app

app :: NanoUI ()
app =
  column (grow defaultLayout) $ do
    void (button "OK")
    label "Hello"
```

Headless frame checks use `NanoUI.Testing`:

```haskell
import NanoUI
import NanoUI.Testing (newContext, renderASCII, runFrame)

main :: IO ()
main = do
  ctx <- newContext
  let inp = emptyInput { inputWindowSize = Size 80 24 }
  (_, _, drawData, _) <- runFrame ctx inp app
  mapM_ putStrLn (renderASCII 80 24 drawData)
```

## RTS flags

For interactive 60fps apps, use the non-moving GC and latency-tuned flags:

```bash
+RTS -N --nonmoving-gc -qb0
```

The demo and TUI set these via cabal `-with-rtsopts`.

### Terminal backend

Uses `nano-ui-term` (`runTermApp`, `runTermAppReduce`, `TermOptions` from `NanoUI.Backend.Term`) with `CellHost` metrics.

**Linux / macOS / Nix** uses [notcurses](https://github.com/dankamongmen/notcurses). **`notcurses-core` is required** (pkg-config).

```bash
cabal run nano-ui-tui
```

**Windows** uses the native Win32 console API in CMD, PowerShell, and Windows Terminal.

```bash
cabal run nano-ui-tui
```

#### Nerd Font and Font Awesome icons

With a Nerd Font (or Font Awesome) the TUI draws its chrome with glyphs instead of brackets. Override detection with `TermOptions`:

```haskell
defaultTermOptions { termAppIcons = Just IconsNerd }
```

Or set `NANOUI_ICONS=nerd` in the environment.

### SDL3 backend

Requires SDL3, SDL3_ttf, and `pkg-config`.

```bash
cabal run -fsdl nano-ui-sdl-demo
cabal run -fsdl nano-ui-sdl-anim
```

Register initial RGBA assets through `SdlOptions`:

```haskell
import NanoUI.Backend.Sdl (RgbaImage (..), SdlOptions (..), defaultSdlOptions, runSdlApp)

main =
  runSdlApp
    defaultSdlOptions
      { sdlAppImages = [RgbaImage (ImageId 1) 32 32 pixels]
      , sdlAppShouldQuit = \inp -> KeyEscape `elem` inputKeys inp
      }
    app
```

On Windows, install the UCRT64 packages and keep `<msys2>\ucrt64\bin` on PATH.

```bash
pacman -S mingw-w64-ucrt-x86_64-sdl3 mingw-w64-ucrt-x86_64-sdl3-ttf
```

Text uses a system TrueType font (Adwaita, Liberation, or Noto on Linux; Segoe UI on Windows). Override with `NANO_UI_FONT=/path/to/font.ttf`.

## Architecture

```
NanoUI (app DSL) -> backend runners -> frame loop -> DrawData -> host render
```

Workspace packages: `nano-ui` (core DSL + `NanoUI.Testing`), `nano-ui-term` (`term-lib`), `nano-ui-sdl` (`sdl-lib`, `-fsdl`).

## Modules

| Module | Role |
|--------|------|
| `NanoUI` | App-facing DSL: widgets, layout, style, animation, input types |
| `NanoUI.Testing` | Deterministic frames, context setup, ASCII render inspection |
| `NanoUI.Backend.Term` | `TermOptions`, `runTermApp`, `runTermAppReduce` |
| `NanoUI.Backend.Sdl` | `SdlOptions`, `RgbaImage`, `runSdlApp`, `runSdlAppReduce` |

## License

MIT
