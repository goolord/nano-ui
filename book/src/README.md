# nano-ui

An immediate-mode GUI toolkit for Haskell.

A view is a `NanoUI ()` action that the backend re-runs every frame. Widgets return values directly (`button` returns `Bool`), input widgets take a mutable `Var` holding their state, and layout is a composable modifier argument. You write UIs with ordinary functions, `do` notation, and `if`/`case`/`for_`.

```haskell
{{#include ../../packages/nano-ui-demo/book/Book/Intro.hs:counter}}
```

Code samples assume `import NanoUI` and `OverloadedStrings`. The runnable source for each tutorial is in [`packages/nano-ui-demo/examples`](https://github.com/goolord/nano-ui/tree/main/packages/nano-ui-demo/examples). If you already know immediate-mode UI, skim the [Quick Start](getting-started/quickstart.md) and go to [State & `Var`](state/index.md).

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

The styling chapters embed live specimens. Toggle modifiers, themes, and interaction states:

<nano-ui-hero></nano-ui-hero>
