# Tutorials

Three applications, built one feature at a time. Each chapter walks through a runnable program; the full source is in `packages/nano-ui-demo/examples/`.

| Tutorial | You build | You learn |
| --- | --- | --- |
| [1. Build a Todo List](todo-list.md) | A filtered, reorderable todo list with per-row editing. | `Var`, keys, local state, `scope`, derived values, keyboard submission. |
| [2. Build a Notepad](notepad.md) | A menu-bar text editor with file dialogs and unsaved-change prompts. | Menus and popups, shortcuts, `TextDocument`, modals, window close requests. |
| [3. Live Data & Background Work](live-data.md) | A background search, streamed progress, and a live chart. | `Task`, `Stream`, spinners and progress bars, charts, cancellation rules. |

## Before you start

Follow the [Quick Start](../getting-started/quickstart.md) once. You need a project that builds with GHC 9.10+ and the SDL3 backend, and executables must be compiled with `-threaded`.

If you are reading this book inside the nano-ui repository, every example can be run directly:

```sh
cabal run nano-ui-example-todo-list
cabal run nano-ui-sdl-notepad
cabal run nano-ui-example-tasks
cabal run nano-ui-example-live-chart
```

## How to read the code

Most of the code in these chapters is ordinary Haskell: data types, pure helpers, and functions that take a `Var` or a model and return `NanoUI ()`. The only unusual idea is the frame:

- The view action runs every frame. A `button` call draws the button *and* reports what happened to it in this frame; there is no event handler to register.
- Anything that must survive between frames lives in a `Var` (`useVar` for ordinary values, `newVar` for application-owned state), an Elm-style model, or a reference you own.
- A side effect belongs inside an event check, never after a bare widget call. `whenM (button "Save") save` is correct; `button "Save" >> save` would save every frame.

[State & `Var`](../state/index.md) explains the frame model in more depth. For now, the three rules above are enough to follow along.

## After the tutorials

Read the [Widget Gallery](../widgets/index.md) to find the widget that fits a problem, [Layout & Scrolling](../layout/index.md) to arrange it, and [Styling & Theming](../styling/index.md) to make it look intentional. When the application grows, [Architecture & Internals](../architecture/index.md) shows how the tutorial state shapes scale up to multi-pane applications.
