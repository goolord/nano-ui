# nano-ui

A purely functional immediate-mode GUI toolkit for Haskell. You describe your
interface as a plain function of application state, and nano-ui handles layout,
input, focus, animation, and rendering. The core is backend-agnostic, so the
same app can run in a terminal or in a native window.

There is no retained widget tree and no callback soup: the whole UI is a
function that runs every frame, and the host receives a batched draw list to
present.

## Motivation

Immediate-mode GUIs fit Haskell unusually well. The interface is just a
function of state, side effects stay in one place, and there are no widget
objects to keep in sync with your data. nano-ui tries to take that idea as far
as it goes:

- **The UI is a value.** Running the frame is the only thing that touches the
  outside world; everything else is expressible, composable code.
- **Backends are interchangeable.** The core emits a draw list; terminal and
  windowing hosts are replaceable packages rather than parts of your app.
- **State is explicit.** Local state lives in hooks, app state lives in your
  model, and changes are delivered as typed messages.
- **Performance is a feature.** Rendering is allocation-light and idle frames
  are skipped when nothing changed, so a UI that is quiet is also cheap.

## Features

- **Immediate-mode core** with a familiar widget set: buttons, labels, text
  fields, sliders, checkboxes, selects, tabs, tables, trees, menus, popups,
  floating windows, panes, and more.
- **Expressive flex layout** with grow, fit, percent, and aspect sizing.
- **Typed messages** via emitters and frame-end reducers, an Elm-style update
  loop that stays type-safe.
- **Local state hooks** (`useInt`, `useState`, `useFlag`, ...) for UI-only
  state that does not belong in your model.
- **Animation** with easing, springs, and tweened values.
- **Damage-tracked rendering** so idle apps stop redrawing until something
  actually changes.
- **Headless testing** with deterministic frames and ASCII render inspection.
- **Plots and vector graphics** through an optional diagrams-lib backend.

## Quickstart

The app DSL lives in `NanoUI`; the runtime loop lives in a backend package.
Here is a small counter in a terminal:

```haskell
import Control.Monad (when)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Term (TermOptions (..), defaultTermOptions, runTermApp)

main :: IO ()
main =
  runTermApp
    defaultTermOptions
      { termAppShouldQuit = inputKeysElem KeyEscape . inputKeys
      }
    counter

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

The same `counter` runs in a window by swapping the backend:

```haskell
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = inputKeysElem KeyEscape . inputKeys
      }
    counter
```

For larger apps, keep state in a model and let widgets emit typed messages.
The backend applies them at the end of the frame:

```haskell
data Msg = Increment | Decrement deriving (Eq)

update :: Msg -> Int -> Int
update Increment n = n + 1
update Decrement n = n - 1

view :: Int -> NanoUI ()
view n = do
  column' (grow defaultLayout) $ do
    buttonEmit "-" Decrement
    label (T.pack (show n))
    buttonEmit "+" Increment

main :: IO ()
main =
  runTermAppReduce defaultTermOptions update 0 view
```

Headless tests render frames without a host:

```haskell
import NanoUI
import NanoUI.Testing (newContext, renderASCII, runFrame)

main :: IO ()
main = do
  ctx <- newContext
  let inp = emptyInput {inputWindowSize = Size 80 24}
  (_, _, drawData, _) <- runFrame ctx inp counter
  mapM_ putStrLn (renderASCII 80 24 drawData)
```

## Backends

| Package | Host | Entry points |
|---------|------|--------------|
| `nano-ui-term` | Terminal (Win32 console / notcurses) | `runTermApp`, `runTermAppReduce` |
| `nano-ui-sdl` | SDL3 window | `runSdlApp`, `runSdlAppReduce` |
| `nano-ui-rgfw` | RGFW window | `runRgfwApp`, `runRgfwAppReduce` |

Each backend exposes options for quitting, assets, fonts, and icons. See the
package of interest for what it requires.

## Building

```bash
cabal build
cabal test
cabal run nano-ui-demo
```

Some components need host libraries and flags:

- Terminal backend on Linux/macOS needs `notcurses-core` (`-fnotcurses`);
  Windows and SDL do not.
- SDL3 backend needs SDL3, SDL3_ttf, and `pkg-config` (`-fsdl`).
- RGFW backend is self-contained.

```bash
cabal run -fsdl nano-ui-sdl-demo
```

On Linux/macOS a Nix flake provides the full toolchain:

```bash
nix develop
nix run .#nano-ui-sdl-demo
nix flake check
```

## Workspace

The repository is a multi-package workspace:

- `nano-ui` — the core DSL, layout engine, and testing harness
- `nano-ui-term` — terminal backend
- `nano-ui-sdl` — SDL3 backend
- `nano-ui-rgfw` — lightweight RGFW backend
- `nano-ui-diagrams` — diagrams-lib bridge for plots and vector graphics
- `nano-ui-form` — composable formlets built on ditto
- `nano-ui-demo` — showcase demos and profiling apps

Architecture and rendering-pipeline diagrams live in `docs/`.

## License

MIT
