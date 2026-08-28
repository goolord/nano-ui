# Revision history for nano-ui

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
* Windows terminal backend restored to native Win32 console + ANSI diff rendering. notcurses remains POSIX-only because its startup probes echo as garbage in conhost/PowerShell (notcurses #2914).
* `Theme` gains a `themeMuted` field, so record construction of a `Theme` needs updating. `contrastRatio` is exported for checking a palette against WCAG AA.
* SDL3 pkg-config is only required when building with `-fsdl` (TUI and headless tests do not need SDL3).
