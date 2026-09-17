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
cabal test nano-ui-terminal-test
```

The demo, notepad, and log viewer take `--selftest`, which drives the app in a
hidden window and exits with an error if a check fails.

## Requirements

SDL3, SDL3_ttf 3.2 or later, and pkg-config, as for `nano-ui-sdl`. The
executables are behind this package's `sdl` flag, on by default.
