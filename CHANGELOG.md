# Revision history for nano-ui

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
* Terminal backend uses notcurses on all platforms. On Windows, `nano-ui-tui` re-enters via `msys2_shell.cmd` so notcurses gets a POSIX tty.
* SDL3 pkg-config is only required when building with `-fsdl` (TUI and headless tests do not need SDL3).
