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

- Text inputs, multi-line text areas, numeric fields, sliders, knobs, selects,
  combo boxes, sortable tables, trees, tabs, menus, context menus, modals,
  floating windows, pane grids, colour pickers, progress bars, sparklines, and
  drag and drop. Custom drawing and interaction are supported through
  `customWidget` and a 2D canvas API (`NanoUI.Path`) with paths, fill rules,
  dashed and joined strokes, gradients, clips, and transforms.
- Row, column, grid, and layered (`layers`) layout with scrolling, wrapping
  (`wrap`, `lineAlign`), pinned children (`pinAt`), and aspect ratios
  (`aspect`). Layout options are `Layout -> Layout` modifiers composed with
  `(.)`, as in `columnWith (gap 8 . padAll 12)`. Debug outlines can be enabled
  with `explainLayout`.
- Configurable wheel step and glide duration (`setScrollTuning`,
  `setScrollStep`), programmatic scrolling (`scrollTo`, `scrollBy`,
  `scrollPages`, `scrollIntoView`), viewport metrics for virtualized lists
  (`getScrollMetrics`), and visibility detection (`sensor`).
- Keyboard focus and navigation for every control, programmatic focus, and
  shortcuts such as `shortcut (ctrl <> key 's')`. IME composition works in the
  built-in text fields and is exposed for custom widgets via `useInputMethod`.
- Per-widget mouse button queries (`respClickedWith MouseMiddle`), cursor
  shapes, and tooltips with a hover delay.
- Text shaping in the SDL backend, with fallback fonts for other scripts and
  mixed left-to-right and right-to-left lines. Multi-style paragraphs and links
  are built with `richText`, as in
  `richText ["Read ", strong "the guide", " or ", hyperlink "faq" "the FAQ"]`.
- Undo and redo in text fields via `TextCommand` values, which can also be
  dispatched from code.
- Images with content fit, crop, zoom, opacity, and rotation
  (`imageConfigured`), scoped RGBA texture registration (`useImageRgba`), SVG
  icons (`loadSvg`, `svgIcon`), and a `spinner`.
- Input blocking while idle (no animation or timer is active), and per-frame
  damage tracking so only changed regions are redrawn. Timed redraws and
  background work are handled with `wakeAfter`, `useTask`, `useTaskStatus`,
  `useStream`, and `askWake` (to wake the event loop from another thread).
- State in `Var`s passed directly to input widgets (`useVar` for view-local
  state, `newVar` for application state, `v.field` for a single field of a
  record), or in a reducer with `NanoUI.Emit`. Writing to a `Var` that was
  already read earlier in the frame re-runs the view automatically.
- Eased and spring animations.
- Themes, including Base16 schemes, scoped to part of a view with `styled` and
  composable modifiers, as in
  `styled (primary . buttonStyle (cornerRadius 6)) (button "Save")`. Widgets
  can be disabled with `disabledWhen`, coloured by status with `tone` and
  `fontTone`, and switched between light and dark themes based on the system
  preference (`lightDark`, `followSystemTheme`).
- One `WindowSettings` shared across backends, with `NanoUI` actions to inspect
  the window (`askWindow`), change its title, position, or mode
  (`setWindowTitleUi`, `moveWindowUi`, `setWindowModeUi`), take screenshots
  (`requestScreenshot`), and close the app (`quitUi`).
- Headless frame execution with scripted input for tests (`NanoUI.Testing`),
  and a backend interface (`NanoUI.Backend`) containing the font, input, and
  damage plumbing needed to implement a window backend.

## Examples

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

For an Elm-style update loop, use `NanoUI.Emit` from `nano-ui-emit` to emit
statically typed messages from widgets and fold them into a model with
`runSdlAppReduce`:

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

view :: Int -> Emit.NanoUIE Msg ()
view n = Emit.withNanoUI row $ do
  Emit.emitWhen (button "-") Decrement
  Emit.liftNanoUI (label (T.pack (show n)))
  Emit.emitWhen (button "+") Increment
```

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

## Running the demos

Requires GHC 9.10 or newer (development uses 9.14) and Cabal. The SDL backend
also requires SDL3, SDL3_ttf, and pkg-config, all of which are provided in the
`nix develop` shell.

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

MIT
