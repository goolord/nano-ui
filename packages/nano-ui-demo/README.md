# nano-ui-demo

Example applications for [nano-ui](https://github.com/goolord/nano-ui), all on
the SDL backend.

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
VT100 support.

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
with the layout overlay on, which the Debug panel (F12) also toggles.

`nano-ui-demo-test` drives the demo, notepad, and log viewer in hidden windows
and fails if a check does not hold.

## Requirements

SDL3, SDL3_ttf 3.2 or later, and pkg-config, as for `nano-ui-sdl`. The
executables are behind this package's `sdl` flag, on by default.

Use GHC 9.14. From an extracted source distribution, run `cabal build` and
then the commands above. From the repository, follow the
[development setup](https://github.com/goolord/nano-ui/blob/main/docs/development.md#setup),
which also covers the sibling ditto checkout used by the form package.

## Reading the examples

- `lib/SdlDemo.hs` builds the widget tour; `lib/DemoData.hs` supplies its data.
- `lib/SdlNotepad.hs` shows text-document state, menus, and asynchronous file dialogs.
- `lib/SdlLogs.hs` shows a bounded log buffer and scroll-follow behaviour.
- `lib/SdlTerminal.hs` contains the terminal parser and PTY integration.

The terminal executable and its test are not built on Windows. The other
examples support the platforms provided by `nano-ui-sdl`.
