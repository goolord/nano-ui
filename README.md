# nano-ui

An immediate-mode GUI toolkit for Haskell.

A view is a function that runs every frame. Each widget is an ordinary effect
that adds a layout node, reads this frame's input, and returns a result: `Bool`
for a button, the new value for an input. There are no widget objects to keep
and no callbacks to register. Widget state lives in a store keyed by where the
widget was called, or in a model you pass through the view.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import NanoUI
import NanoUI.Backend.Sdl (defaultSdlOptions, runSdlApp)

main :: IO ()
main = runSdlApp defaultSdlOptions counter

counter :: NanoUI ()
counter = do
  (n, setN) <- useInt 0
  row $ do
    whenM (button "-") (setN (n - 1))
    label (T.pack (show n))
    whenM (button "+") (setN (n + 1))
```

## Features

- Text inputs and a multi-line text area, numeric fields, sliders, knobs,
  selects, combo boxes, sortable tables, trees, tabs, menus, context menus,
  modals, floating windows, pane grids, colour pickers, progress bars,
  sparklines, and drag and drop. `customWidget` and a canvas API cover
  anything else.
- Row, column, and grid layout with scrolling. Layout options are
  `Layout -> Layout` modifiers, as in `columnWith (gap 8 . padAll 12)`.
- Scrollers take a wheel step and a glide time (`setScrollTuning`,
  `setScrollStep`), and move from code: `scrollTo`, `scrollBy`, `scrollPages`,
  and `scrollIntoView`. `getScrollMetrics` gives a virtualized list the
  viewport and offset it needs to pick its rows.
- Keyboard focus and navigation for every control.
- Backends block on input and run a frame only when something needs
  redrawing. Each frame computes its damage against the previous one.
- State in local hooks (`useInt`, `useText`, `useState`), in your own model,
  or in a reducer with `NanoUI.Emit`.
- Eased and spring animation, and themes, including ones built from Base16
  schemes.
- `NanoUI.Testing` runs frames headlessly on scripted input, for tests.

## Examples

Inputs are controlled: pass the current value and keep the result. A change you
don't store is undone on the next frame.

```haskell
greeter :: NanoUI ()
greeter = columnWith (gap 8 . padAll 16) $ do
  (name, setName) <- useText "world"
  (size, setSize) <- useFloat 16
  (shout, setShout) <- useFlag False

  heading "Greeter"
  setName =<< textInput name
  setSize =<< slider 10 48 size
  setShout =<< checkbox "Shout" shout

  let greeting = "Hello, " <> name <> "!"
  labelWith (fontSize size) (if shout then T.toUpper greeting else greeting)
```

If you are a fan of the elm architecture, the widgets in
`NanoUI.Emit` emit messages instead of returning
values, and `runSdlAppReduce` folds them into the model:

```haskell
import NanoUI.Emit qualified as Emit

data Msg = Increment | Decrement

main :: IO ()
main = runSdlAppReduce defaultSdlOptions update 0 view

update :: Msg -> Int -> Int
update Increment n = n + 1
update Decrement n = n - 1

view :: Int -> NanoUI ()
view n = row $ do
  Emit.button "-" Decrement
  label (T.pack (show n))
  Emit.button "+" Increment
```

## How it works

`NanoUI` is `Eff '[Ui, IOE]` from
[effectful](https://hackage.haskell.org/package/effectful), and widgets have
types like `Ui :> es => Eff es Bool`, so a view can run in a larger effect
stack. A frame:

1. Resets the node and vertex arenas and runs the view. Widgets add layout
   nodes and read and write the widget store.
2. Solves layout.
3. Resolves pointer, keyboard, and focus against the new geometry.
4. Paints into pinned vertex and index buffers, background layer then overlays.
5. Computes damage against the previous frame and hands the draw list to the
   backend.

[docs/rendering-pipeline.svg](docs/rendering-pipeline.svg) has the diagram.
Per-frame code is profiled for allocation, and an
[inspection-testing](https://hackage.haskell.org/package/inspection-testing)
suite checks that the vertex writers compile without dictionaries or tuples.

## Packages

| Package | What it is |
| --- | --- |
| `nano-ui` | Widgets, layout, input handling, and the draw list |
| `nano-ui-sdl` | Window backend on SDL3, with TrueType fonts, installed-font lookup, and native file dialogs |
| `nano-ui-rgfw` | Lightweight window backend on RGFW and OpenGL 3.2, with a bundled bitmap font and no system dependencies beyond windowing |
| `nano-ui-rgfw-bindings` | Haskell bindings to RGFW |
| `nano-ui-diagrams` | Line, bar, scatter, and area charts, and drawing with [diagrams](https://diagrams.github.io/) |
| `nano-ui-form` | Validated forms built on [ditto](https://hackage.haskell.org/package/ditto) |
| `nano-ui-demo` | Example applications |

## Running the demos

You need GHC 9.14 and Cabal. The SDL backend also needs SDL3 and SDL3_ttf.
`nix develop` sets all of this up.

```sh
cabal run nano-ui-sdl-demo       # widget and chart tour
cabal run nano-ui-sdl-notepad    # text editor with menus and file dialogs
cabal run nano-ui-sdl-logs       # streaming log viewer
cabal run nano-ui-sdl-terminal   # terminal on /bin/sh (Linux and macOS)
cabal run nano-ui-rgfw-demo      # the RGFW backend
```

## Documentation

Start with the `NanoUI` module documentation: it explains how widgets return
values, how inputs keep their state, and how layout modifiers compose.
[docs/development.md](docs/development.md) covers building, testing,
profiling, and the layout of this repository.

## License

MIT
