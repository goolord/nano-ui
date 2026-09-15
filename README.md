# nano-ui

An immediate-mode GUI toolkit for Haskell.

Your interface is a function that runs every frame. Widgets are plain calls
that draw themselves and return what the user did, so there are no widget
objects to keep track of and no callbacks to wire up.

```haskell
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

- Text fields and editors, sliders, selects, tables, trees, tabs, menus,
  floating windows, and colour pickers
- Row, column, and grid layout with scrolling
- Keyboard focus and navigation for every control
- Idle apps stay idle: frames are redrawn only when something changed
- State in local hooks, in your own model, or in an Elm-style reducer
- The same UI code runs on either window backend, or headless in tests

## Packages

| Package | What it is |
| --- | --- |
| `nano-ui` | Widgets, layout, input handling, and the draw list |
| `nano-ui-sdl` | Window backend on SDL3, with TrueType fonts |
| `nano-ui-rgfw` | Lightweight window backend on RGFW and OpenGL, with a bundled bitmap font |
| `nano-ui-rgfw-bindings` | Haskell bindings to RGFW |
| `nano-ui-diagrams` | Charts, and drawing with [diagrams](https://diagrams.github.io/) |
| `nano-ui-form` | Validated forms built on [ditto](https://hackage.haskell.org/package/ditto) |
| `nano-ui-demo` | Example applications |

## Try it

You need GHC 9.14 and Cabal. The SDL backend also needs SDL3 and SDL3_ttf;
`nix develop` sets all of this up.

```sh
cabal run nano-ui-sdl-demo       # widget tour
cabal run nano-ui-sdl-notepad    # a small text editor
cabal run nano-ui-rgfw-demo      # the same kind of UI on the RGFW backend
```

`nano-ui-sdl-logs` and `nano-ui-sdl-terminal` are there too.

## Documentation

Start with the `NanoUI` module documentation: it explains how widgets return
values, how inputs keep their state, and how layout modifiers compose.
[docs/development.md](docs/development.md) covers building, testing, and the
layout of this repository.

## License

MIT
