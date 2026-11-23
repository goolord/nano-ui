# nano-ui

Core immediate-mode GUI toolkit for Haskell. You describe a window as a single monadic action that runs every frame, using ordinary functions, `do` blocks, and standard control flow (`if`, `case`, `for_`). Widgets return results directly (such as `Bool` from `button`), and mutable state lives in typed `Var` references inside `NanoUI`.

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

## Backends and companion packages

This package provides the widgets, layout solver, input handling, theming, and draw lists. Pair it with a backend package to open windows and render frames:

| Package | Description |
| --- | --- |
| [nano-ui-sdl](https://hackage.haskell.org/package/nano-ui-sdl) | Desktop SDL3 backend with HarfBuzz TrueType fonts, system font lookup, and native file dialogs |
| [nano-ui-rgfw](https://github.com/goolord/nano-ui/tree/main/packages/nano-ui-rgfw) | RGFW + OpenGL 3.2 backend with a bundled bitmap font and no extra library dependencies |
| [nano-ui-sdl-web](https://github.com/goolord/nano-ui/tree/main/packages/nano-ui-sdl-web) | Browser backend for GHC's WebAssembly target using SDL_ttf and WebGL2 |

Optional companion packages add [charts and diagrams](https://hackage.haskell.org/package/nano-ui-diagrams) (`nano-ui-diagrams`), [validated forms](https://hackage.haskell.org/package/nano-ui-form) (`nano-ui-form`), [streamable Markdown](https://github.com/goolord/nano-ui/tree/main/packages/nano-ui-markdown) (`nano-ui-markdown`), and [Elm-style message reducers](https://github.com/goolord/nano-ui/tree/main/packages/nano-ui-emit) (`nano-ui-emit`).

Inside `nano-ui` itself, `NanoUI.Testing` runs frames headlessly on scripted input, and `NanoUI.Backend` exports the hooks needed to implement custom backends. Regular application code only needs `import NanoUI`.

## State and `Var`s

Allocate view-local state with `useVar`, pass it straight to input widgets, and read the latest value with `readVar`:

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

`useVar` handles view-local primitives without setup, `newVar` allocates state of any type in `IO` to pass into your view, and `v.field` projects a `Var` for a single field of a `Generic` record. A frame only runs a second pass if a `Var` is written *after* it was already read earlier in the same pass, so placing `readVar` below the widget that edits it keeps updates to a single pass. If you prefer an Elm-style reducer loop, add `nano-ui-emit` for `NanoUIE msg a`.

## Highlights

- **Widgets:** Single-line inputs, multi-line editors with undo/redo, numeric fields, sliders, knobs, selects, combo boxes, sortable tables, trees, tabs, menus, context menus, modals, floating windows, pane grids, color pickers, progress bars, sparklines, rich text, SVG icons, and drag and drop. Build custom controls with `customWidget` and a 2D vector canvas supporting paths, fill rules, dashed strokes, gradients, clips, and transforms.
- **Layout:** Rows, columns, grids, wrapping (`wrap`, `lineAlign`), layers (`layers`), pinned overlays (`pinAt`), scrolling, and aspect ratios (`aspect`), all configured through composable `Layout -> Layout` functions such as `columnWith (gap 8 . padAll 12)`.
- **Input & IME:** Full keyboard focus navigation, programmatic focus control, composable shortcuts (`shortcut (ctrl <> key 's')`), and IME composition for both built-in fields and custom widgets (`useInputMethod`).
- **Async work:** Keyed background jobs (`useTaskStatus`, `useTask`), streaming producers (`useStream`), and thread-safe wakes (`askWake`).
- **Theming & animation:** Built-in light/dark themes, Base16 palettes, subtree-scoped overrides (`styled`), plus eased tweens and spring physics.
- **Idle efficiency:** The event loop blocks while idle, and per-frame damage tracking repaints only changed regions.

## Documentation

- [The nano-ui Book](https://goolord.github.io/nano-ui/book/): Tutorials, widget gallery, interactive styling lab, architecture guide, and troubleshooting.
- [User guide](https://github.com/goolord/nano-ui/blob/main/packages/nano-ui/GUIDE.md) (also bundled as `GUIDE.md` in the Hackage tarball): Concise reference covering setup, state, widget identity, layout, async tasks, custom drawing, and headless testing.
- `NanoUI` Haddocks: Complete API reference for widgets, state, and layout modifiers.
- [GitHub repository](https://github.com/goolord/nano-ui): Example apps, video walkthrough, and [development guide](https://goolord.github.io/nano-ui/book/getting-started/development.html).

## License

MIT
