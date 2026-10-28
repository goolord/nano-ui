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

The book is a learning path first, a reference second:

1. **[Quick Start](getting-started/quickstart.md)** installs the toolchain and gets a window on screen.
2. **[Tutorials](tutorials/index.md)** build three small applications end to end: a todo list, a notepad, and a live data view.
3. **[State](state/introduction.md)** explains where state lives, what a `Var` is, and what writes cost.
4. **[The Elm Architecture](elm/overview.md)** covers the alternative typed-message style in `nano-ui-emit`.
5. **[Keyboard & Focus](input/shortcuts.md)** covers chords, focus, and IME.
6. **[Layout](layout/containers.md)** and the **[Widget Gallery](widgets/index.md)** are the API tour.
7. **[Styling](styling/overview.md)** and the **[Advanced Guides](advanced/background-work.md)** go deeper.
8. **[Ecosystem Packages](packages/nano-ui-sdl.md)**, **[Troubleshooting](troubleshooting/recurring-bugs.md)**, and **[Architecture Notes](architecture/frame-model.md)** are the reference material for when an application grows.

If you already know immediate-mode UI, skim the Quick Start and start with [Introduction to State](state/introduction.md).

## Packages in the monorepo

| Package | What it is | Read |
| --- | --- | --- |
| `packages/nano-ui` | Core toolkit: widgets, layout, input, theming, draw list, text arena. | [State](state/introduction.md), [Layout](layout/containers.md), [Widget Gallery](widgets/index.md) |
| `packages/nano-ui-sdl` | SDL3 backend: TrueType fonts via HarfBuzz, system font search, native non-blocking file dialogs, custom title bars. | [SDL3 Backend](packages/nano-ui-sdl.md) |
| `packages/nano-ui-rgfw` | Lightweight RGFW + OpenGL 3.2 backend with a bundled bitmap font and zero system dependencies. | [RGFW Backend](packages/nano-ui-rgfw.md) |
| `packages/nano-ui-emit` | Elm-style typed messages and update reducers (`NanoUIE msg a`). | [Elm Architecture](elm/overview.md) |
| `packages/nano-ui-diagrams` | Line, bar, scatter, and area charts (`NanoUI.Plot`) with diagrams-lib integration. | [Charts & Diagrams](packages/nano-ui-diagrams.md) |
| `packages/nano-ui-form` | Validated applicative forms built on ditto. | [Validated Forms](packages/nano-ui-form.md) |
| `packages/nano-ui-markdown` | Markdown rendering, incrementally appendable for streamed replies. | [Markdown Rendering](packages/nano-ui-markdown.md) |
| `packages/nano-ui-svg` | Headless `DrawOp`-to-SVG export and visual regression testing. | [Headless SVG Export](packages/nano-ui-svg.md) |
| `packages/nano-ui-demo` | Runnable example applications and single-topic tutorials. | [Tutorials](tutorials/index.md) |

## Interactive styling laboratory

This documentation includes interactive, headlessly rendered SVG workbenches embedded directly in the chapters. You can toggle styling modifiers, themes, and interaction states live:

<nano-ui-hero></nano-ui-hero>

Every code sample assumes `import NanoUI` and `OverloadedStrings`; examples that use anything else import it explicitly. The complete, runnable source for each tutorial lives in [`packages/nano-ui-demo/examples`](https://github.com/goolord/nano-ui/tree/main/packages/nano-ui-demo/examples).
