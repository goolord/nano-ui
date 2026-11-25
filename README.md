# nano-ui

`nano-ui` is an immediate-mode GUI toolkit for Haskell. You describe a window as a single monadic action that runs on every frame, using ordinary functions, `do` blocks, and standard control flow (`if`, `case`, `for_`). Widgets return results directly (for example, `button` returns `Bool`), and mutable state lives in typed `Var` references inside the `NanoUI` monad.

https://github.com/user-attachments/assets/92436ce7-1202-4e51-8df7-b34ee0a31d93

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

- Built-in widgets: text inputs, multi-line documents with undo, sortable tables, trees, tabs, menus, modals, floating windows, dockable pane grids, charts, and Markdown
- Custom widgets, a 2D vector canvas with paths, strokes, gradients, clips, and transforms, and typed GPU fragment shaders (`nano-ui-shader`)
- Row, column, grid, wrapping, and layered layouts driven by composable modifiers, with scrolling and an interactive layout inspector
- Rich text, HarfBuzz bidirectional text shaping, fallback fonts, images, and SVG icons
- Keyboard focus, composable shortcuts, and IME composition
- Built-in light and dark themes, Base16 palettes, OS light/dark tracking, and subtree-scoped styling
- Eased tween and spring-physics animations
- Background `Task` and `Stream` workers with automatic cancellation
- Zero-CPU idle sleep and per-frame damage tracking
- Direct `Var` state or an optional Elm-style message/reducer architecture (`nano-ui-emit`)
- Headless testing with scripted user input and SVG frame export
- Desktop backends for SDL3 and RGFW+OpenGL, plus a browser WebAssembly backend

## Live demos and examples

Try the full widget sampler in your browser at **[goolord.github.io/nano-ui/demo](https://goolord.github.io/nano-ui/demo/)**, or explore the interactive landing page demo at **[goolord.github.io/nano-ui](https://goolord.github.io/nano-ui/)**. Both compile the desktop Haskell views to WebAssembly using [`nano-ui-sdl-web`](packages/nano-ui-sdl-web/README.md).

Browse [`nano-ui-demo`](packages/nano-ui-demo/README.md) for all full apps and focused single-topic examples.

Building locally requires GHC 9.10 through 9.14 (development uses 9.14) and Cabal. The SDL backend also requires SDL3, SDL3_ttf 3.2+, and `pkg-config`, all included in the `nix develop` shell.

```sh
cabal run nano-ui-sdl-demo          # full widget and chart sampler
cabal run nano-ui-sdl-notepad       # text editor with menus and native file dialogs
cabal run nano-ui-sdl-logs          # streaming log viewer
cabal run nano-ui-sdl-terminal      # PTY terminal emulator (Linux and macOS)
cabal run nano-ui-rgfw-demo         # lightweight RGFW + OpenGL backend
cabal run nano-ui-markdown-example  # streamed Markdown chat reply
cabal run nano-ui-example-shaders   # typed GPU surface shaders and subtree effects
cabal run nano-ui-example-hello     # first single-topic example
```

The [single-topic examples](packages/nano-ui-demo/README.md#examples) are small standalone programs covering one topic each: state, layout, form validation, shaders, background tasks, 100,000-row virtualized lists, and more.

## How a frame runs

`NanoUI` has `MonadIO`, `MonadUnliftIO`, and `exceptions` instances. Internally it runs on [effectful](https://hackage.haskell.org/package/effectful), with the raw `Eff` stack exposed in `NanoUI.Effectful` when you want to combine it with your own effects.

On each frame:

1. Pointer input routes to the topmost layer under the cursor (an open menu, dropdown, frontmost floating window, or the main page), the layout and vertex arenas reset, and your view action runs. Covered layers receive no pointer events.
2. The layout solver calculates exact bounds for every widget.
3. Pointer, keyboard, and focus states resolve against the new geometry.
4. Damage tracking diffs the frame against the previous one to find changed regions.
5. Only damaged regions rasterize into pinned vertex and index buffers across the background, content, overlay, and chrome layers.

See [docs/rendering-pipeline.svg](docs/rendering-pipeline.svg) for the full pipeline diagram. Hot per-frame paths are tracked by an allocation regression gate, and [inspection-testing](https://hackage.haskell.org/package/inspection-testing) verifies that vertex writers compile without dictionary passing or intermediate tuples.

## Packages

| Package | Description |
| --- | --- |
| `nano-ui` | Core widgets, layout solver, input handling, theming, and draw lists |
| `nano-ui-sdl` | Desktop SDL3 backend with TrueType fonts, system font lookup, and native file dialogs |
| `nano-ui-rgfw` | RGFW + OpenGL 3.2 backend with a bundled bitmap font and no extra library dependencies |
| `nano-ui-rgfw-bindings` | Haskell bindings to RGFW (C source bundled) |
| `nano-ui-sdl-web` | Browser backend for GHC's WebAssembly target using SDL_ttf and WebGL2 |
| `nano-ui-shader` | Pure-Haskell typed fragment shader EDSL, multi-target GPU compiler (GLSL, SPIR-V, MSL, CPU), pass fusion, and built-in effects |
| `nano-ui-diagrams` | Line, bar, scatter, area, and step charts, plus [diagrams](https://diagrams.github.io/) widgets |
| `nano-ui-form` | Validated applicative forms built on [ditto](https://hackage.haskell.org/package/ditto) |
| `nano-ui-emit` | Statically typed emitting views, widget adapters, and pure reducers |
| `nano-ui-markdown` | CommonMark rendering with incremental appends for streamed text |
| `nano-ui-svg` | `DrawOp` and full-frame SVG export |
| `nano-ui-demo` | Full example applications and single-topic tutorials |

[`nano-ui`](https://hackage.haskell.org/package/nano-ui) is published on Hackage; this repository may be ahead of the latest release.

## Documentation

Read **[The nano-ui Book](https://goolord.github.io/nano-ui/book/)** online (or in [`book/`](book/)):

- **[Quick Start](https://goolord.github.io/nano-ui/book/getting-started/quickstart.html)**: Setting up a windowed app with Cabal, Nix, or Windows MSYS2.
- **[Tutorials](https://goolord.github.io/nano-ui/book/tutorials/index.html)**: Step-by-step builds of a todo list, a notepad, and a live-data chart.
- **[State & `Var`](https://goolord.github.io/nano-ui/book/state/index.html)** and **[Elm-Style State](https://goolord.github.io/nano-ui/book/state/elm.html)**: State lifetimes, write costs, `VarMap`, and typed reducers.
- **[Layout & Scrolling](https://goolord.github.io/nano-ui/book/layout/index.html)**: Containers, modifiers, and virtualizing 100,000-row lists.
- **[Widget Gallery](https://goolord.github.io/nano-ui/book/widgets/index.html)** and **[Styling & Theming](https://goolord.github.io/nano-ui/book/styling/index.html)**: Reference for every widget family, themes, typography, and the [Interactive Styling Lab](https://goolord.github.io/nano-ui/book/styling/lab.html).
- **[Keyboard, Focus & IME](https://goolord.github.io/nano-ui/book/input/index.html)**: Shortcuts, focus rules, and custom text input.
- **[Async Work & Animation](https://goolord.github.io/nano-ui/book/advanced/async.html)**, **[Shaders](https://goolord.github.io/nano-ui/book/advanced/shaders.html)**, and **[Ecosystem Packages](https://goolord.github.io/nano-ui/book/packages/index.html)**: Background tasks, streams, GPU shaders, charts, forms, Markdown, SVG export, and backends.
- **[Troubleshooting](https://goolord.github.io/nano-ui/book/troubleshooting/recurring-bugs.html)**: Recurring bugs from real applications and how to fix them.
- **[Architecture & Internals](https://goolord.github.io/nano-ui/book/architecture/index.html)**: Frame lifecycle and application patterns.
- **[Development Setup](https://goolord.github.io/nano-ui/book/getting-started/development.html)**: Building, testing, profiling, and repo conventions.
- **[`NanoUI` Haddock](https://hackage.haskell.org/package/nano-ui)**: API reference for the released version. For the current checkout, run `cabal haddock-project --output=haddocks --hoogle`.

## License

MIT, except the bundled Inter font (SIL Open Font License 1.1, [`LICENSE-Inter`](packages/nano-ui-ttf/LICENSE-Inter)), the bundled Cozette bitmap font (MIT, [`LICENSE-Cozette`](packages/nano-ui-rgfw/LICENSE-Cozette)), and the bundled RGFW library (zlib, [`LICENSE-RGFW`](packages/nano-ui-rgfw-bindings/LICENSE-RGFW)).
