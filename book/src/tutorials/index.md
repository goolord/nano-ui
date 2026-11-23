# Tutorials

These three tutorials build complete applications one feature at a time. You can read the full source for each in `packages/nano-ui-demo/examples/`.

| Tutorial | What you build | What you learn |
| --- | --- | --- |
| [1. Build a Todo List](todo-list.md) | A filterable, reorderable todo list with inline editing. | `Var`, stable keys, per-row state, `scope`, derived values, and Enter/Escape handling. |
| [2. Build a Notepad](notepad.md) | A text editor with a menu bar, file dialogs, and unsaved-change warnings. | Menus and popups, keyboard shortcuts, `TextDocument`, modals, and close confirmation. |
| [3. Live Data & Background Work](live-data.md) | Background search, streaming progress, and a live chart. | `Task`, `Stream`, spinners, progress bars, charts, and cancellation. |

Start with the [Quick Start](../getting-started/quickstart.md) if you have not set up your toolchain yet. From a checkout of `nano-ui`, you can run any of the finished examples directly:

```sh
cabal run nano-ui-example-todo-list
cabal run nano-ui-sdl-notepad
cabal run nano-ui-example-tasks
cabal run nano-ui-example-live-chart
```
