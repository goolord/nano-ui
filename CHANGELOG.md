# Revision history for nano-ui

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
* Terminal backend uses native Win32 console on Windows; notcurses on POSIX. notcurses OSC probes echo as garbage in conhost, so Windows does not use it.
* SDL3 pkg-config is only required when building with `-fsdl` (TUI and headless tests do not need SDL3).
