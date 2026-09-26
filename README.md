# nano-ui

An immediate-mode GUI toolkit for Haskell.

A view is a function that runs every frame. Each widget is an ordinary effect
that adds a layout node, reads this frame's input, and returns a result: `Bool`
for a button, the new value for an input. There are no widget objects to keep
and no callbacks to register. Widget state lives in a store keyed by each
widget's position among its siblings (or by a key you give it with `withKey`),
or in a model you pass through the view.

https://github.com/user-attachments/assets/5994a348-3bc5-4a2c-80fa-c8db2be94397

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
  sparklines, and drag and drop. `customWidget` and a canvas API, with curves
  and transforms (`NanoUI.Path`), cover anything else.
- Row, column, grid, and stacked (`stack`) layout with scrolling, wrapping
  (`wrap`), and pinned children (`pinAt`). Layout options are
  `Layout -> Layout` modifiers, as in `columnWith (gap 8 . padAll 12)`.
  `explainLayout` outlines every layout node.
- Scrollers take a wheel step and a glide time (`setScrollTuning`,
  `setScrollStep`), and move from code: `scrollTo`, `scrollBy`, `scrollPages`,
  and `scrollIntoView`. `getScrollMetrics` gives a virtualized list the
  viewport and offset it needs to pick its rows, and `sensor` says when a
  widget scrolls into view.
- Keyboard focus and navigation for every control, focus from code, and
  shortcuts, as in `shortcut (ctrl <> key 's')`.
- Every mouse button, held and clicked per widget (`respClickedWith
  MouseMiddle`), cursor shapes, and tooltips with a hover delay.
- Shaped text in the SDL backend, with fallback fonts for other scripts and
  mixed left-to-right and right-to-left lines. `richText` wraps a paragraph
  of mixed styles and links, as in
  `richText ["Read ", strong "the guide", " or ", hyperlink "faq" "the FAQ"]`.
- Text fields with undo and redo, driven by `TextCommand` values that code
  can run too, and input-method composition in the SDL backend.
- Images with a content fit, opacity and rotation (`imageConfigured`), SVG
  icons (`loadSvg`, `svgIcon`) and a `spinner`.
- Backends block on input when no animation or timed update needs a frame.
  Each frame computes
  its damage against the previous one. `wakeAfter` schedules a frame for a
  view that changes on a timer, `useTaskStatus` and `useTask` run background
  work, `useStream` folds a producer's updates into a view's state, and
  `askWake` lets any thread wake the loop.
- State in local hooks (`useInt`, `useText`, `useState`), in your own model,
  or in a reducer with `NanoUI.Emit`.
- Eased and spring animation.
- Themes, including ones built from Base16 schemes, changed for part of a
  view with `styled` and composable modifiers, as in
  `styled (primary . buttonStyle (cornerRadius 6)) (button "Save")`.
  `disabledWhen` switches widgets off, and `followSystemTheme` follows the
  desktop's light or dark setting.
- One `WindowSettings` for every backend, a view that reads its window
  (`askWindow`), changes it (`setWindowTitleUi`, `moveWindowUi`,
  `setWindowModeUi`), takes screenshots (`requestScreenshot`) and decides
  when it closes (`quitUi`).
- `NanoUI.Testing` runs frames headlessly on scripted input, for tests, and
  `NanoUI.Backend` has the font, input and damage plumbing a window backend
  is written against. `NanoUI` itself is only what writing a GUI needs.

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

For an Elm-style update function, `NanoUI.Emit` adapts ordinary widgets to emit
messages, and `runSdlAppReduce` folds them into the model:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import NanoUI
import NanoUI.Backend.Sdl (defaultSdlOptions, runSdlAppReduce)
import NanoUI.Emit qualified as Emit

data Msg = Increment | Decrement

main :: IO ()
main = runSdlAppReduce defaultSdlOptions update 0 view

update :: Msg -> Int -> Int
update Increment n = n + 1
update Decrement n = n - 1

view :: Int -> NanoUI ()
view n = row $ do
  Emit.emitWhen (button "-") Decrement
  label (T.pack (show n))
  Emit.emitWhen (button "+") Increment
```

## How it works

`NanoUI` is `Eff '[Ui, IOE]` from
[effectful](https://hackage.haskell.org/package/effectful), and widgets have
types like `Ui :> es => Eff es Bool`, so a view can run in a larger effect
stack. A frame:

1. Routes the pointer to whatever was on top under it (a menu, a dropdown,
   the floating panel in front, or the page), resets the node and vertex
   arenas, and runs the view. Widgets add layout nodes and read and write the
   widget store; one the pointer was not routed to sees no pointer at all.
2. Solves layout.
3. Resolves pointer, keyboard, and focus against the new geometry.
4. Computes damage against the previous frame, including geometry, state,
   theme, and animation changes.
5. Paints the required region into pinned vertex and index buffers, in
   background, content, overlay, and chrome layers, then hands the draw list
   to the backend.

[docs/rendering-pipeline.svg](docs/rendering-pipeline.svg) has the diagram.
Per-frame code is profiled for allocation, and an
[inspection-testing](https://hackage.haskell.org/package/inspection-testing)
suite checks that the vertex writers compile without dictionaries or tuples.

## Packages

| Package | What it is |
| --- | --- |
| `nano-ui` | Widgets, layout, input handling, and the draw list |
| `nano-ui-sdl` | Window backend on SDL3, with TrueType fonts, installed-font lookup, and native file dialogs |
| `nano-ui-rgfw` | Window backend on RGFW and OpenGL 3.2, with a bundled bitmap font and no system dependencies beyond windowing |
| `nano-ui-rgfw-bindings` | Haskell bindings to RGFW |
| `nano-ui-diagrams` | Line, bar, scatter, and area charts, and drawing with [diagrams](https://diagrams.github.io/) |
| `nano-ui-form` | Validated forms built on [ditto](https://hackage.haskell.org/package/ditto) |
| `nano-ui-markdown` | Markdown documents drawn with rich text, parsed incrementally for streamed chat replies |
| `nano-ui-demo` | Example applications |

## Running the demos

You need GHC 9.10 or newer (development uses 9.14) and Cabal. The SDL backend
also needs SDL3, SDL3_ttf, and pkg-config. `nix develop` sets these up. `cabal.project` builds
`nano-ui-form`'s ditto dependency from a checkout at `../ditto`, so clone
[ditto](https://github.com/goolord/ditto) next to this repository first.

```sh
cabal run nano-ui-sdl-demo       # widget and chart tour
cabal run nano-ui-sdl-notepad    # text editor with menus and file dialogs
cabal run nano-ui-sdl-logs       # streaming log viewer
cabal run nano-ui-sdl-terminal   # terminal on /bin/sh (Linux and macOS)
cabal run nano-ui-rgfw-demo      # the RGFW backend
cabal run nano-ui-markdown-example  # a Markdown chat reply streaming in
```

## Documentation

The [user guide](packages/nano-ui/GUIDE.md) covers application setup, stable
widget identity, layout, background work, custom drawing, and headless tests.

Start with the `NanoUI` module documentation: it explains how widgets return
values, how inputs keep their state, and how layout modifiers compose.
[docs/development.md](docs/development.md) covers building, testing,
profiling, and the layout of this repository.

## License

MIT
