# nano-ui

An immediate-mode GUI toolkit for Haskell. Because each frame is a monadic
action, you can build interfaces with ordinary functions, `do` notation, and
standard control flow (`if`, `case`, `for_`). Widgets return values directly
(such as `Bool` from `button`), and persistent state is held in `Var`s, mutable
variables in the `NanoUI` monad.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import NanoUI
import NanoUI.Backend.Sdl (defaultSdlOptions, runSdlApp)

main :: IO ()
main = runSdlApp defaultSdlOptions counter

counter :: NanoUI ()
counter = row $ do
  n <- useVar (0 :: Int)
  whenM (button "-") (modifyVar n (subtract 1))
  label . T.pack . show =<< readVar n
  whenM (button "+") (modifyVar n (+ 1))
```

## Backends

This package provides the widgets, layout solver, input handling, and draw
list. Window creation and rendering are provided by a backend package:

| Package | What it is |
| --- | --- |
| [nano-ui-sdl](https://hackage.haskell.org/package/nano-ui-sdl) | SDL3, with TrueType fonts, installed-font lookup, and native file dialogs |
| [nano-ui-rgfw](https://github.com/goolord/nano-ui/tree/main/packages/nano-ui-rgfw) | RGFW and OpenGL 3.2, with a bundled bitmap font and no system dependencies beyond windowing |

[nano-ui-diagrams](https://hackage.haskell.org/package/nano-ui-diagrams) adds
charts and [diagrams](https://diagrams.github.io/) drawings,
[nano-ui-form](https://hackage.haskell.org/package/nano-ui-form) adds validated
forms, and
[nano-ui-markdown](https://github.com/goolord/nano-ui/tree/main/packages/nano-ui-markdown)
renders Markdown incrementally as it streams in. Within this package,
`NanoUI.Testing` runs frames headlessly on scripted input for tests, and
`NanoUI.Backend` exports the interface for implementing window backends.
Application code only needs to import `NanoUI`.

## State and Vars

Create state with `useVar`, pass it to input widgets, and read the current
value with `readVar`:

```haskell
greeter :: NanoUI ()
greeter = columnWith (gap 8 . padAll 16) $ do
  name <- useVar "world"
  size <- useVar 16
  shout <- useVar False

  heading "Greeter"
  textInput name
  slider 10 48 size
  checkbox "Shout" shout

  greeting <- ("Hello, " <>) . (<> "!") <$> readVar name
  loud <- readVar shout
  px <- readVar size
  labelWith (fontSize px) (if loud then T.toUpper greeting else greeting)
```

`useVar` allocates view-local state, `newVar` allocates state in `IO` to pass
into your UI action, and `v.field` projects a `Var` for a single field of a
record. A frame only runs a second pass when a `Var` is written after already
being read earlier in the same frame, so reading a `Var` below the widget that
edits it costs no extra pass. State can also be managed with an Elm-style
reducer: the `nano-ui-emit` package provides `NanoUI.Emit` and `NanoUIE msg a`
for typed messages, which the backend reducer runners fold into a model.

## Features

- Text inputs and a multi-line text area with undo and redo, numeric fields,
  sliders, knobs, selects, combo boxes, sortable tables, trees, tabs, menus,
  context menus, modals, floating windows, pane grids, colour pickers, progress
  bars, sparklines, rich text, SVG icons, and drag and drop. Custom drawing and
  interaction are supported through `customWidget` and a 2D canvas API with
  paths, fill rules, dashed and joined strokes, gradients, clips, and
  transforms.
- Row, column, grid, and layered (`layers`) layout with scrolling, wrapping
  (`wrap`, `lineAlign`), pinned children (`pinAt`), and aspect ratios
  (`aspect`). Layout options are `Layout -> Layout` modifiers composed with
  `(.)`, as in `columnWith (gap 8 . padAll 12)`.
- Keyboard focus and navigation for every control, programmatic focus, and
  shortcuts such as `shortcut (ctrl <> key 's')`. IME composition works in the
  built-in text fields and is exposed for custom widgets via `useInputMethod`.
- Background tasks with `useTaskStatus` and `useTask`, streaming producers with
  `useStream`, and `askWake` to wake the event loop from another thread.
- Themes, including Base16 schemes, scoped to part of a view with `styled`, as
  in `styled (primary . buttonStyle (cornerRadius 6)) (button "Save")`.
- Eased and spring animations.
- Input blocking while idle (no animation or timer is active), and per-frame
  damage tracking so only changed regions are redrawn.

## Documentation

- [User guide](https://github.com/goolord/nano-ui/blob/main/packages/nano-ui/GUIDE.md)
  (also bundled as `GUIDE.md` in the sdist tarball): application setup, state
  and widget identity, layout, background work, custom drawing, and headless
  tests.
- `NanoUI` Haddock: reference documentation for widgets, state, and layout
  modifiers.
- [GitHub repository](https://github.com/goolord/nano-ui): demo applications,
  video tour, and
  [docs/development.md](https://github.com/goolord/nano-ui/blob/main/docs/development.md).

## License

MIT
