# nano-ui

An immediate-mode GUI toolkit for Haskell. Because each frame is a monadic
action, you can build interfaces with ordinary functions, `do` notation, and
standard control flow (`if`, `case`, `for_`). Widgets return values directly
(such as `Bool` from `button`), and persistent state is held in `Var`s, mutable
variables in the `NanoUI` monad.

https://github.com/user-attachments/assets/5994a348-3bc5-4a2c-80fa-c8db2be94397

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

## Features

- Built-in widgets (text inputs, tables, trees, menus, modals, floating windows,
  pane grids, charts, and more)
- Custom widgets and a 2D canvas for vector drawing
- Row, column, grid, and layered layout with composable modifiers, scrolling,
  wrapping, and debug outlines
- Rich text, bidirectional text shaping, fallback fonts, images, and SVG icons
- Keyboard navigation, composable shortcuts, and IME input
- Custom themes (defined directly, composed from built-in light and dark themes,
  or generated from a Base16 palette), automatic system light/dark mode
  switching, and composable, scoped styling
- Animations with easing curves and spring physics
- First-class support for background tasks and streams
- Idle sleep and per-frame damage tracking
- Direct mutable state or an Elm-style message/update loop
- Headless testing with scripted input
- Modular backends, with built-in support for SDL3 and RGFW+OpenGL

## Examples

See [`nano-ui-demo`](packages/nano-ui-demo/README.md) for full demo
applications and single-topic examples.

Running the demos requires GHC 9.10 through 9.14 (development uses 9.14) and
Cabal. The SDL backend also requires SDL3, SDL3_ttf, and pkg-config, all of
which are provided in the `nix develop` shell.

```sh
cabal run nano-ui-sdl-demo       # widget and chart tour
cabal run nano-ui-sdl-notepad    # text editor with menus and file dialogs
cabal run nano-ui-sdl-logs       # streaming log viewer
cabal run nano-ui-sdl-terminal   # terminal on /bin/sh (Linux and macOS)
cabal run nano-ui-rgfw-demo      # the RGFW backend
cabal run nano-ui-markdown-example  # a Markdown chat reply streaming in
cabal run nano-ui-example-hello  # the first of the single-topic examples
```

The [single-topic examples](packages/nano-ui-demo/README.md#examples) are
short, self-contained programs, each focused on one concept: state, layout,
form validation, background tasks, virtualized lists of 100,000 rows, and so
on.

## How it works

`NanoUI` has `MonadIO` and `MonadUnliftIO` instances, and exception handling
from `exceptions`. It is built on
[effectful](https://hackage.haskell.org/package/effectful), with the underlying
`Eff` stack exported in `NanoUI.Effectful` if you want to share effects with the
rest of your application. Each frame:

1. Pointer input is routed to the topmost layer at the cursor (a menu,
   dropdown, frontmost floating panel, or the main page), the node and vertex
   arenas are reset, and the UI action runs. Widget calls append layout nodes
   and read from or update the widget store; layers that do not receive the
   pointer get no pointer events.
2. Layout is solved.
3. Pointer, keyboard, and focus states are resolved against the new geometry.
4. Damage is computed against the previous frame from geometry, state, theme,
   and animation changes.
5. The damaged region is painted into pinned vertex and index buffers in the
   background, content, overlay, and chrome layers to build the backend draw
   list.

See [docs/rendering-pipeline.svg](docs/rendering-pipeline.svg) for a diagram of
the pipeline. Per-frame code is profiled for allocation, and
[inspection-testing](https://hackage.haskell.org/package/inspection-testing) is
used to check that the vertex writers compile without dictionaries or tuples.

## Packages

| Package | What it is |
| --- | --- |
| `nano-ui` | Widgets, layout, input handling, and the draw list |
| `nano-ui-sdl` | Window backend on SDL3, with TrueType fonts, installed-font lookup, and native file dialogs |
| `nano-ui-rgfw` | Window backend on RGFW and OpenGL 3.2, with a bundled bitmap font and no system dependencies beyond windowing |
| `nano-ui-rgfw-bindings` | Haskell bindings to RGFW |
| `nano-ui-diagrams` | Line, bar, scatter, and area charts, and drawing with [diagrams](https://diagrams.github.io/) |
| `nano-ui-form` | Validated forms built on [ditto](https://hackage.haskell.org/package/ditto) |
| `nano-ui-emit` | Statically typed emitting views, widget adapters, and reducers |
| `nano-ui-markdown` | Markdown documents drawn with rich text, parsed incrementally for streamed chat replies |
| `nano-ui-demo` | Example applications |

## Documentation

- [User guide](packages/nano-ui/GUIDE.md): application setup, stable widget
  identity, layout, background work, custom drawing, and headless tests.
- [State](docs/typed-state.md): `Var`s, where they come from, write costs,
  handle-based resource APIs, and migration from the old hooks.
- [Development guide](docs/development.md): building, testing, profiling, and
  repository structure.
- `NanoUI` Haddock: reference documentation for widgets, state, and layout
  modifiers.

## License

MIT, except the bundled Inter font (SIL Open Font License 1.1,
[`LICENSE-Inter`](packages/nano-ui-sdl/LICENSE-Inter)), the bundled Cozette
font (MIT by its author,
[`LICENSE-Cozette`](packages/nano-ui-rgfw/LICENSE-Cozette)) and the bundled
RGFW (zlib, [`LICENSE-RGFW`](packages/nano-ui-rgfw-bindings/LICENSE-RGFW)).
