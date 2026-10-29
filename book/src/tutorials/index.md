# Tutorials

Three applications, built one feature at a time. The full source of each is in `packages/nano-ui-demo/examples/`.

| Tutorial | You build | You learn |
| --- | --- | --- |
| [1. Build a Todo List](todo-list.md) | A filtered, reorderable todo list with per-row editing. | `Var`, keys, local state, `scope`, derived values, keyboard submission. |
| [2. Build a Notepad](notepad.md) | A menu-bar text editor with file dialogs and unsaved-change prompts. | Menus and popups, shortcuts, `TextDocument`, modals, window close requests. |
| [3. Live Data & Background Work](live-data.md) | A background search, streamed progress, and a live chart. | `Task`, `Stream`, spinners and progress bars, charts, cancellation rules. |

Do the [Quick Start](../getting-started/quickstart.md) first. From a nano-ui checkout, run the examples directly:

```sh
cabal run nano-ui-example-todo-list
cabal run nano-ui-sdl-notepad
cabal run nano-ui-example-tasks
cabal run nano-ui-example-live-chart
```
