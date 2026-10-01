# nano-ui-demo

Example applications for [nano-ui](https://github.com/goolord/nano-ui), all on
the SDL backend: a few complete apps, and short single-file examples that each
teach one topic.

| Executable | What it shows |
| --- | --- |
| `nano-ui-sdl-demo` | A tour of the widgets and charts |
| `nano-ui-sdl-notepad` | A text editor with a menu bar and native file dialogs |
| `nano-ui-sdl-logs` | A streaming log viewer with selectable lines |
| `nano-ui-sdl-terminal` | A terminal attached to `/bin/sh` (Linux and macOS) |
| `nano-ui-sdl-profile` | Frame timings for the demo UI |

The terminal is kept small: 80 by 24 cells, UTF-8, the 16 ANSI colours with
bold and inverse, basic cursor movement and erasing, and 2,000 lines of
scrollback. It advertises `TERM=ansi` and has no wide-character layout or full
VT100 support. It takes typed text through the input method, whose
composition it shows at the cursor (`useInputMethod`).

## Examples

Each file in `examples/` is one short program about one topic. They import
only the library packages, so any of them can be copied out as a starting
point. Read them in roughly this order:

| Example | Run | Shows |
| --- | --- | --- |
| [`Hello.hs`](examples/Hello.hs) | `nano-ui-example-hello` | The smallest app, and how a view runs every frame |
| [`State.hs`](examples/State.hs) | `nano-ui-example-state` | Hooks, state cells, `scope` around conditional widgets, `withKey` for lists |
| [`Layout.hs`](examples/Layout.hs) | `nano-ui-example-layout` | Rows, columns, sizing, centring, wrapping, grids, badges, and the layout overlay |
| [`Components.hs`](examples/Components.hs) | `nano-ui-example-components` | Writing reusable widgets, with and without state of their own |
| [`Forms.hs`](examples/Forms.hs) | `nano-ui-example-forms` | Validation, a disabled submit button, submitting on Enter, resetting |
| [`TodoList.hs`](examples/TodoList.hs) | `nano-ui-example-todo-list` | A list you add to, edit, filter and reorder, with state per item |
| [`Keyboard.hs`](examples/Keyboard.hs) | `nano-ui-example-keyboard` | Shortcuts, arrow keys, focus, and menus with shortcuts |
| [`Overlays.hs`](examples/Overlays.hs) | `nano-ui-example-overlays` | Menu bars, context menus, tooltips, popovers, modals, windows, confirm before quitting |
| [`Theming.hs`](examples/Theming.hs) | `nano-ui-example-theming` | Built-in and custom themes, following the system, style modifiers |
| [`Animation.hs`](examples/Animation.hs) | `nano-ui-example-animation` | Tweens, springs, colours, hover effects, and what animation costs |
| [`Images.hs`](examples/Images.hs) | `nano-ui-example-images` | Generated images, content fits, regenerating an image, SVG icons |
| [`Canvas.hs`](examples/Canvas.hs) | `nano-ui-example-canvas` | Drawing with the mouse on a canvas: a small paint program |
| [`Tasks.hs`](examples/Tasks.hs) | `nano-ui-example-tasks` | Background jobs, failures and retries, streamed progress |
| [`Files.hs`](examples/Files.hs) | `nano-ui-example-files` | Opening and saving files with native dialogs and drag and drop |
| [`LongList.hs`](examples/LongList.hs) | `nano-ui-example-long-list` | 100,000 rows, building only the visible ones |
| [`LiveChart.hs`](examples/LiveChart.hs) | `nano-ui-example-live-chart` | A chart fed live data from a background producer |

Run one with `cabal run nano-ui-example-hello`, and so on.

## Running

```sh
cabal run nano-ui-sdl-demo
cabal run nano-ui-sdl-notepad
cabal run nano-ui-sdl-logs
cabal run nano-ui-sdl-terminal
cabal run nano-ui-sdl-profile
cabal test nano-ui-demo-test
cabal test nano-ui-terminal-test
```

`nano-ui-sdl-demo --help` lists the demo's options; `--explain` starts it
with the layout overlay on, which the Debug panel (F12) also toggles. Its
Screenshot button saves `nano-ui-demo.png`.

`nano-ui-demo-test` drives the demo, notepad, log viewer, and an input-method
composition in hidden windows, and fails if a check does not hold.

## Requirements

SDL3, SDL3_ttf 3.2 or later, and pkg-config, as for `nano-ui-sdl`. The
executables are behind this package's `sdl` flag, on by default.

Use GHC 9.14. From an extracted source distribution, run `cabal build` and
then the commands above. From the repository, follow the
[development setup](https://github.com/goolord/nano-ui/blob/main/docs/development.md#setup).

## Reading the examples

- `lib/SdlDemo.hs` builds the widget tour, one function per tab; `lib/DemoData.hs` supplies its data.
- `lib/SdlNotepad.hs` shows text-document state, menus, and asynchronous file dialogs.
- `lib/SdlLogs.hs` shows a bounded log buffer and scroll-follow behaviour.
- `lib/SdlTerminal.hs` contains the terminal parser and PTY integration.

The terminal executable and its test are not built on Windows. The other
examples support the platforms provided by `nano-ui-sdl`.
