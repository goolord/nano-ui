# nano-ui

An immediate-mode GUI toolkit for Haskell.

A view is a `NanoUI ()` action that the backend re-runs every frame. Widgets return values directly (e.g. `button` returns `Bool`), input widgets take a mutable `Var` holding their state, and layout is a composable modifier argument. You write UIs with ordinary Haskell functions, `do` notation, and standard control flow (`if`, `case`, `for_`).

```haskell
counter :: NanoUI ()
counter = row $ do
  n <- useVar (0 :: Int)
  whenM (button "-") (modifyVar n (subtract 1))
  label . T.pack . show =<< readVar n
  whenM (button "+") (modifyVar n (+ 1))
```

## Packages in the Monorepo

| Package | Purpose |
| --- | --- |
| [`packages/nano-ui`](packages/nano-ui-sdl.md) | Core toolkit: widgets, layout, input, theming, draw list, text arena. |
| [`packages/nano-ui-sdl`](packages/nano-ui-sdl.md) | SDL3 backend: TrueType fonts via HarfBuzz, system font search, native non-blocking file dialogs, custom title bars. |
| [`packages/nano-ui-rgfw`](packages/nano-ui-rgfw.md) | Lightweight RGFW + OpenGL 3.2 backend with a bundled bitmap font and zero system dependencies. |
| [`packages/nano-ui-emit`](packages/nano-ui-emit.md) | Elm-style typed messages and update reducers (`NanoUIE msg a`). |
| [`packages/nano-ui-diagrams`](packages/nano-ui-diagrams.md) | Line, bar, scatter, and area charts (`NanoUI.Plot`) with diagrams-lib integration. |
| [`packages/nano-ui-form`](packages/nano-ui-form.md) | Validated applicative forms built on ditto. |
| [`packages/nano-ui-markdown`](packages/nano-ui-markdown.md) | Markdown rendering, incrementally appendable for streaming chat replies. |
| [`packages/nano-ui-svg`](packages/nano-ui-svg.md) | Headless `DrawOp`-to-SVG export and visual regression testing. |
| [`packages/nano-ui-demo`](packages/nano-ui-sdl.md) | Interactive demos, full apps, and single-topic tutorials. |

## Interactive Styling Laboratory

This documentation includes interactive, headlessly-rendered SVG workbenches embedded directly within the chapters. You can toggle styling modifiers, themes, and interaction states live:

<nano-ui-hero></nano-ui-hero>

Explore the rest of this book to learn the mental model, API conventions, state management with `Var`, layout engine, and production best practices.
