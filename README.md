# nano-ui

`nano-ui` is a purely functional, immediate-mode GUI toolkit for Haskell. Applications describe their user interface as a pure function of state. The engine evaluates layout, focus, input handling, animations, and damage tracking every frame, returning a batched draw list to the host renderer.

The core library is backend-agnostic. The exact same UI code runs inside a terminal window, a native desktop window, or a headless ASCII test runner.

```haskell
counter :: NanoUI ()
counter = do
  (count, setCount) <- useInt 0
  column' (grow defaultLayout) $ do
    heading "Counter"
    row' defaultLayout $ do
      minus <- button "-"
      when (respClicked minus) (setCount (count - 1))
      label (T.pack (show count))
      plus <- button "+"
      when (respClicked plus) (setCount (count + 1))
```

## Core Principles

- **Pure Declarative UI**: State drives the interface. There are no persistent widget trees, object handles, or manual DOM updates.
- **Backend Portability**: The core engine outputs abstract draw commands. Host packages target SDL3 windows, RGFW surfaces, or terminal cells without altering UI logic.
- **Dual State Models**: Use local hooks (`useInt`, `useState`) for transient component state, or pure Elm-style reducers (`buttonEmit`, `runTermAppReduce`) for application-wide state management.
- **Damage-Tracked Rendering**: The engine calculates region bounds for changed components and skips redrawing idle frames when state remains untouched.
- **SIMD Layout & Draw Pipeline**: AVX2 SIMD acceleration (`-mavx2`) optimizes layout tree calculations and vertex buffer assembly.
- **Deterministic Headless Testing**: Run UIs frame-by-frame in unit tests and assert visual output using ASCII render inspection.

## Package Architecture

The repository is structured as a Cabal multi-package workspace:

| Package | Role | Target / Backend | Key Modules |
| :--- | :--- | :--- | :--- |
| `nano-ui` | Core library | Core engine, flex layout, damage tracker, testing harness | `NanoUI`, `NanoUI.Testing` |
| `nano-ui-term` | TUI backend | Win32 Console (Windows) or Notcurses (POSIX) | `NanoUI.Backend.Term` |
| `nano-ui-sdl` | Window backend | Hardware-accelerated SDL3 and SDL3_ttf host | `NanoUI.Backend.Sdl` |
| `nano-ui-rgfw` | Window backend | Standalone lightweight RGFW host with embedded Cozette font | `NanoUI.Backend.Rgfw` |
| `nano-ui-rgfw-bindings` | FFI bindings | Low-level C bindings for RGFW | `NanoUI.Rgfw.Native` |
| `nano-ui-diagrams` | Graphics bridge | Vector diagrams and plotting (`diagrams-lib` integration) | `NanoUI.Diagrams` |
| `nano-ui-form` | Form framework | Composable formlets built on `ditto` | `NanoUI.Form` |
| `nano-ui-demo` | Applications | Multi-backend showcase and profiling benchmarks | `nano-ui-sdl-demo`, `nano-ui-sdl-profile` |

Architecture and rendering pipeline diagrams are available in `docs/`:
- [Architecture Overview](docs/architecture.svg)
- [Rendering Pipeline](docs/rendering-pipeline.svg)

## Quickstart

### 1. Local Hooks Pattern

Transient UI state (such as counters, text inputs, or toggle states) can be stored directly within the widget context using hooks:

```haskell
import Control.Monad (when)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

main :: IO ()
main = runSdlApp defaultSdlOptions { sdlAppShouldQuit = inputKeysElem KeyEscape . inputKeys } app

app :: NanoUI ()
app = do
  (count, setCount) <- useInt 0
  column' (grow defaultLayout) $ do
    heading "Local Hooks Counter"
    row' defaultLayout $ do
      dec <- button "-"
      when (respClicked dec) (setCount (count - 1))
      label (T.pack (show count))
      inc <- button "+"
      when (respClicked inc) (setCount (count + 1))
```

### 2. Elm Architecture Pattern (Emitters & Reducers)

For global application state, widgets emit typed messages processed by a pure update reducer at the end of each frame:

```haskell
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Term (TermOptions (..), defaultTermOptions, runTermAppReduce)

data Msg = Increment | Decrement deriving (Eq)

update :: Msg -> Int -> Int
update Increment n = n + 1
update Decrement n = n - 1

view :: Int -> NanoUI ()
view n = column' (grow defaultLayout) $ do
  heading "Elm-Style Counter"
  row' defaultLayout $ do
    buttonEmit "-" Decrement
    label (T.pack (show n))
    buttonEmit "+" Increment

main :: IO ()
main = runTermAppReduce defaultTermOptions { termAppShouldQuit = inputKeysElem KeyEscape . inputKeys } update 0 view
```

### 3. Headless Inspection & Testing

Test component behavior deterministically without opening a window or initializing graphics drivers:

```haskell
import NanoUI
import NanoUI.Testing (newContext, renderASCII, runFrame)

testCounter :: IO ()
testCounter = do
  ctx <- newContext
  let input = emptyInput { inputWindowSize = Size 80 24 }
  (_, _, drawData, _) <- runFrame ctx input counter
  putStrLn "Rendered Frame Output:"
  mapM_ putStrLn (renderASCII 80 24 drawData)
```

## Layout & Styling

Layouts are defined declaratively using flex containers and layout modifiers:

### Containers
- `column`, `column'` : Vertical stack arrangement.
- `row`, `row'` : Horizontal flex row.
- `grid`, `grid'` : Multi-column grid layout.
- `scrollArea`, `scrollArea2D` : Scrollable viewports with auto-hiding scrollbars.
- `paneGrid`, `splitPane` : Resizable panels and dockable workspaces.
- `window`, `modal` : Floating movable windows and modal overlays.

### Modifiers
- `grow` : Fill remaining space along the primary layout axis.
- `fillW`, `fillH` : Stretch width or height to container boundaries.
- `fixedWH w h`, `fixedW w`, `fixedH h` : Constrain element dimensions in logical pixels.
- `percent w h` : Set percentage-based dimensions relative to parent.
- `padAll p`, `padXY x y` : Apply padding around inner contents.
- `gap g` : Set spacing between child items.
- `alignMid`, `alignCenter`, `alignTop`, `alignBottom` : Adjust alignment within the flex context.

## Component Catalog

`nano-ui` includes a broad suite of built-in components:

- **Input Controls**: `button`, `checkbox`, `slider`, `textInput`, `textArea`, `comboBox`, `colorPicker`, `radioFieldset`, `knob`, `toggleSwitch`.
- **Data Display**: `label`, `heading`, `sparkline`, `progressBar`, `circularProgress`, `table`, `tree`, `kv`, `card`.
- **Navigation & Structure**: `tabs`, `tabBar`, `menuItem`, `contextMenu`, `popup`, `tooltip`, `toolbar`, `separator`, `spacer`.
- **Graphics & Custom Painting**: `image`, `drawing`, `canvas`, `customWidget` (direct canvas operations and quad or gradient primitives).
- **Animations**: `animate`, `animateEase`, `animateToSpring`, `pulse` (supporting smooth, bouncy, and stiff spring physics).

## Building and Running Demos

### Build Flags and Prerequisites

| Backend | Cabal Flag | Dependencies | Supported Platforms |
| :--- | :--- | :--- | :--- |
| Terminal (`nano-ui-term`) | `-fnotcurses` | `notcurses-core >= 3.0` | Linux, macOS (Win32 natively on Windows) |
| SDL3 Window (`nano-ui-sdl`) | `-fsdl` | `sdl3 >= 3.2`, `sdl3-ttf >= 3.2`, `pkg-config` | Linux, macOS, Windows |
| RGFW Window (`nano-ui-rgfw`) | None (Default) | C compiler (Self-contained) | Linux, macOS, Windows |
| SIMD Optimization | `-fsimd` | x86-64 hardware with AVX2 | x86-64 |

### Commands

```bash
# Build the workspace
cabal build all

# Run unit and integration test suites
cabal test all

# Run standalone RGFW window demo
cabal run nano-ui-rgfw-demo

# Run SDL3 window showcase (requires -fsdl flag)
cabal run -fsdl nano-ui-sdl-demo

# Run POSIX terminal showcase (requires -fnotcurses flag on Linux/macOS)
cabal run -fnotcurses nano-ui-tui

# Run performance profiling benchmarks
cabal run nano-ui-profile
```

### Nix Environment

A Nix flake is included for reproducible development and builds:

```bash
# Spawn shell with GHC 9.14, Cabal, HLS, SDL3, and notcurses pre-installed
nix develop

# Run the SDL3 showcase directly via Nix
nix run .#nano-ui-sdl-demo

# Execute all flake check targets and tests
nix flake check
```

## License

[MIT](LICENSE)
