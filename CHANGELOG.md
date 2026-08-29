# Revision history for nano-ui

## 0.1.0.0 -- YYYY-mm-dd

* First version.
* TUI and SDL are separate packages (`nano-ui-term`, `nano-ui-sdl`). `-fsdl` for SDL. `-fnotcurses` for POSIX TUI and its tests.
* Windows TUI uses the Win32 console. POSIX TUI uses notcurses.
* Public API cutover: app code imports `NanoUI` plus backend runners only. Tests and tools use `NanoUI.Testing` (and backend testing facades).
* Core split by ownership: context types, frame hit/focus/clip/chrome, widget node/chrome helpers, term/SDL session lifecycles.
* Shared pixel-host context setup: `newPixelHostContext` (core), aliased by `NanoUI.Testing.newPixelContext` and `NanoUI.Sdl.Context.newSdlContext`.
* `animate` / `animateTo` / `animateToSpring` / delay / `box`. Quad and cubic-Bezier eases. Spring presets `presetBouncy`, `presetSmooth`, `presetStiff`. SDL example: `nano-ui-sdl-anim`.
* `Theme` adds `themeMuted` and `themeFloatingWindow`. Record construction of a `Theme` needs updating.
* Nerd Font icons via `NANOUI_ICONS`, `NERD_FONT`, or `withIcons`. Font Awesome codepoints count as two terminal columns.
* Core uses `HostProfile` (`PixelHost` / `CellHost`) instead of inferring a terminal from font metrics.
* Removed unused `defaultStyle`, `percentH`, `inputChanged`, `renderASCIIFromRects`, `sliderTrackRect`, and `sliderTrackMargin`.
