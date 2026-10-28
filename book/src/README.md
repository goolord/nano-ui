# nano-ui

An immediate-mode GUI toolkit for Haskell.

A view is a `NanoUI ()` action that the backend re-runs every frame. Widgets return values directly (`button` returns `Bool`), input widgets take a mutable `Var` holding their state, and layout is a composable modifier argument. You write UIs with ordinary Haskell functions, `do` notation, and standard control flow (`if`, `case`, `for_`).

```haskell
counter :: NanoUI ()
counter = row $ do
  n <- useVar (0 :: Int)
  whenM (button "-") (modifyVar n (subtract 1))
  label . T.pack . show =<< readVar n
  whenM (button "+") (modifyVar n (+ 1))
```

## How this book is organized

Chapters follow this order:

1. **[Quick Start](getting-started/quickstart.md)** installs the toolchain and gets a window on screen.
2. **[Tutorials](tutorials/index.md)** build three small applications end to end: a todo list, a notepad, and a live data view.
3. **[State & `Var`](state/index.md)** explains where state lives, what writes cost, and the [Elm-style alternative](state/elm.md).
4. **[Layout & Scrolling](layout/index.md)** covers containers, modifiers, and large lists.
5. The **[Widget Gallery](widgets/index.md)** is the entry point for controls, data views, overlays, and custom drawing. **[Styling & Theming](styling/index.md)** is next to it, with a [live lab](styling/lab.md) and [gotchas](styling/gotchas.md).
6. **[Keyboard, Focus & IME](input/index.md)** covers chords, focus, and text input.
7. **[Async Work & Animation](advanced/async.md)**, **[Desktop Windows](advanced/window-management.md)**, and **[Ecosystem Packages](packages/index.md)** go further.
8. **[Troubleshooting](troubleshooting/recurring-bugs.md)**, **[Development Setup](getting-started/development.md)**, and **[Architecture & Internals](architecture/index.md)** are the reference material.

If you already know immediate-mode UI, skim the Quick Start and start with [State & `Var`](state/index.md).

## Packages in the monorepo

| Package | What it is | Read |
| --- | --- | --- |
| `packages/nano-ui` | Core toolkit: widgets, layout, input, theming, draw list, text arena. | [State](state/index.md), [Layout](layout/index.md), [Widget Gallery](widgets/index.md) |
| `packages/nano-ui-sdl` | SDL3 backend: TrueType fonts via HarfBuzz, system font search, native non-blocking file dialogs, custom title bars. | [Ecosystem Packages](packages/index.md), [Desktop Windows](advanced/window-management.md) |
| `packages/nano-ui-rgfw` | Lightweight RGFW + OpenGL 3.2 backend with a bundled bitmap font and zero system dependencies. | [Ecosystem Packages](packages/index.md) |
| `packages/nano-ui-emit` | Elm-style typed messages and update reducers (`NanoUIE msg a`). | [Elm-Style State](state/elm.md) |
| `packages/nano-ui-diagrams` | Line, bar, scatter, and area charts (`NanoUI.Plot`) with diagrams-lib integration. | [Ecosystem Packages](packages/index.md) |
| `packages/nano-ui-form` | Validated applicative forms built on ditto. | [Ecosystem Packages](packages/index.md) |
| `packages/nano-ui-markdown` | Markdown rendering, incrementally appendable for streamed replies. | [Ecosystem Packages](packages/index.md) |
| `packages/nano-ui-svg` | Headless `DrawOp`-to-SVG export and visual regression testing. | [Ecosystem Packages](packages/index.md) |
| `packages/nano-ui-demo` | Runnable example applications and single-topic tutorials. | [Tutorials](tutorials/index.md) |

## Styling workbenches

Interactive SVG workbenches are embedded in the styling chapters. Toggle modifiers, themes, and interaction states live:

<nano-ui-hero></nano-ui-hero>

Code samples assume `import NanoUI` and `OverloadedStrings`. The runnable source for each tutorial is in [`packages/nano-ui-demo/examples`](https://github.com/goolord/nano-ui/tree/main/packages/nano-ui-demo/examples).
