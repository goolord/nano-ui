# nano-ui

`nano-ui` is an immediate-mode GUI toolkit for Haskell.

Instead of building and mutating a persistent widget tree, you describe the entire window as a single `NanoUI ()` action (called a *view* throughout this book). The backend runs that action on every frame. Drawing a button and checking if it was clicked is one function call, input widgets read and update mutable `Var` references directly, and layout is passed as composable modifier functions. Because a view is just ordinary Haskell, you build screens with normal functions, `do` blocks, and standard control flow (`if`, `case`, `for_`), without registering callbacks.

```haskell
{{#include ../../packages/nano-ui-demo/book/Book/Intro.hs:counter}}
```

The code samples in this book assume `import NanoUI` and `OverloadedStrings`. You can find the full runnable source for each tutorial in [`packages/nano-ui-demo/examples`](https://github.com/goolord/nano-ui/tree/main/packages/nano-ui-demo/examples).

If you want to try the widgets before installing anything, open the [interactive sampler](../demo/index.html) right in your browser. It runs the same `examples/Demo.hs` view as the desktop app, compiled to WebAssembly with `nano-ui-sdl-web`. A couple of browser-specific details apply: file dialogs work with file contents rather than disk paths, and the font picker pulls from Fontsource everywhere (plus your installed system fonts in Chromium browsers after you grant permission).

## Packages in this repository

| Package | What it does | Where to read more |
| --- | --- | --- |
| `packages/nano-ui` | Core toolkit: widgets, layout solver, input, theming, draw lists, and text buffers. | [State](state/index.md), [Layout](layout/index.md), [Widget Gallery](widgets/index.md) |
| `packages/nano-ui-sdl` | Desktop SDL3 backend with HarfBuzz TrueType shaping, system font lookup, native file dialogs, and custom title bars. | [Ecosystem Packages](packages/index.md), [Desktop Windows](advanced/window-management.md) |
| `packages/nano-ui-rgfw` | Lightweight RGFW + OpenGL 3.2 backend with a bundled bitmap font and no extra library dependencies. | [Ecosystem Packages](packages/index.md) |
| `packages/nano-ui-sdl-web` | Browser backend for GHC's WebAssembly target using SDL_ttf and WebGL2. | [Interactive Sampler](../demo/index.html) |
| `packages/nano-ui-emit` | Elm-style architecture with typed messages and pure update reducers (`NanoUIE msg a`). | [Elm-Style State](state/elm.md) |
| `packages/nano-ui-diagrams` | Line, bar, scatter, and area charts (`NanoUI.Plot`) alongside `diagrams-lib` support. | [Ecosystem Packages](packages/index.md) |
| `packages/nano-ui-form` | Validated applicative forms built on `ditto`. | [Ecosystem Packages](packages/index.md) |
| `packages/nano-ui-markdown` | CommonMark rendering with incremental appends for streamed text. | [Ecosystem Packages](packages/index.md) |
| `packages/nano-ui-shader` | Pure-Haskell typed fragment shader EDSL, multi-target GPU compiler, pass fusion, and built-in effects. | [Shaders](advanced/shaders.md) |
| `packages/nano-ui-svg` | Headless `DrawOp` and full-frame SVG export for documentation and visual tests. | [Ecosystem Packages](packages/index.md) |
| `packages/nano-ui-demo` | Full example applications and focused single-topic demos. | [Tutorials](tutorials/index.md) |

## Live styling specimens

The styling chapters include interactive previews rendered directly from `nano-ui`. Click the toggles below to see how theme modifiers and surface styles affect each widget:

<nano-ui-hero></nano-ui-hero>
