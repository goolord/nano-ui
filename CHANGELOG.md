# Revision history for nano-ui

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
* Windows terminal backend restored to native Win32 console + ANSI diff rendering. notcurses remains POSIX-only because its startup probes echo as garbage in conhost/PowerShell (notcurses #2914).
* `Theme` gains `themeMuted` and `themeFloatingWindow` fields, so record construction of a `Theme` needs updating. `contrastRatio` is exported for checking a palette against WCAG AA.
* Removed unused `defaultStyle`, `percentH`, `inputChanged`, `renderASCIIFromRects`, `sliderTrackRect`, and `sliderTrackMargin`.
* SDL3 pkg-config is only required when building with `-fsdl` (TUI and headless tests do not need SDL3).
* TUI chrome can use Nerd Font / Font Awesome glyphs for the checkbox, close button, select caret, and title marks. The tier comes from `NANOUI_ICONS`, `NERD_FONT`, or a terminal known to ship a Nerd Font, and defaults to the previous ASCII look. Font Awesome codepoints (U+F000 to U+F2E0) count as two terminal columns for layout and rasterisation. New `IconSet` / `Icons` types with `withIcons`; `Context` gains `ctxIcons`.
