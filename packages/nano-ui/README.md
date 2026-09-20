# nano-ui

An immediate-mode GUI toolkit for Haskell.

A view is a function that runs every frame. Each widget is an ordinary effect
that adds a layout node, reads this frame's input, and returns a result: `Bool`
for a button, the new value for an input. There are no widget objects to keep
and no callbacks to register. Widget state lives in a store keyed by each
widget's position among its siblings (or by a key you give it with `withKey`),
or in a model you pass through the view.

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

## Backends

This package has the widgets, layout, input handling, and the draw list. It
opens no window. A backend package runs the view:

| Package | What it is |
| --- | --- |
| [nano-ui-sdl](https://hackage.haskell.org/package/nano-ui-sdl) | SDL3, with TrueType fonts, installed-font lookup, and native file dialogs |
| [nano-ui-rgfw](https://github.com/goolord/nano-ui/tree/main/packages/nano-ui-rgfw) | RGFW and OpenGL 3.2, with a bundled bitmap font and no system dependencies beyond windowing |

[nano-ui-diagrams](https://hackage.haskell.org/package/nano-ui-diagrams) adds
charts and [diagrams](https://diagrams.github.io/) drawings, and
[nano-ui-form](https://hackage.haskell.org/package/nano-ui-form) adds validated
forms. `NanoUI.Testing` in this package runs frames headlessly on scripted
input, for tests.

## Inputs are controlled

Pass the current value and keep the result. A change you don't store is undone
on the next frame.

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

State can live in local hooks (`useInt`, `useText`, `useState`), in your own
model, or in a reducer: `NanoUI.Emit` adapts widgets to emit messages, and the
backends' reducer runners fold them into the model.

## What is in the box

- Text inputs and a multi-line text area with undo and redo, numeric fields,
  sliders, knobs, selects, combo boxes, sortable tables, trees, tabs, menus,
  context menus, modals, floating windows, pane grids, colour pickers, progress
  bars, sparklines, rich text, SVG icons, and drag and drop. `customWidget` and
  a canvas API cover anything else.
- Row, column, and grid layout with scrolling. Layout options are
  `Layout -> Layout` modifiers, as in `columnWith (gap 8 . padAll 12)`.
- Keyboard focus and navigation for every control.
- Themes, including ones built from Base16 schemes, changed for part of a view
  with `styled`, as in
  `styled (primary . buttonStyle (cornerRadius 6)) (button "Save")`.
- Eased and spring animation.
- Backends block on input when no animation or timed update needs a frame.
  Each frame computes
  what changed since the previous one, and only that part is repainted.

## Documentation

The [user guide](https://github.com/goolord/nano-ui/blob/main/packages/nano-ui/GUIDE.md)
is also included as `GUIDE.md` in the source distribution. It covers application
setup, state and identity, layout, background work, custom drawing, and tests.

Start with the `NanoUI` module documentation: it explains how widgets return
values, how inputs keep their state, and how layout modifiers compose.

The [repository](https://github.com/goolord/nano-ui) has a video tour, the
demo applications, and
[docs/development.md](https://github.com/goolord/nano-ui/blob/main/docs/development.md),
which covers building, testing, profiling, and how a frame works.

## License

MIT
