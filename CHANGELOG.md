# Revision history for nano-ui

## Unreleased

Breaking API: prefer `Text` and `Vector` over `String` and `[ ]` in core types.

* `inputKeys`: `[Key]` -> `Vector Key`. Use `inputKeysElem`, `foldInputKeys`, `inputKeysFromList` instead of list `elem`/`++`.
* `inputChars`: `[Char]` -> `Text`. Append with `(<>)`, test empty with `T.null`.
* `drawCommands` in `DrawData`: `[DrawCmd]` -> `Vector DrawCmd`. Use `drawCmdNull`, `drawCmdElems`, `drawCmdFilter`, `drawCmdPartitionByLayer`.
* `textInput`, `useText`, `kvBlock`: `String` -> `Text`.
* Clipboard hooks: `ctxClipboardGet` returns `Maybe Text`; `ctxClipboardSet` takes `Text`.
* `storeText` / `storeNote`: `IntMap Text`.
* Dependency: core adds `vector ^>=0.13`.
* `enableMouse` / `disableMouse`: `ByteString` -> `ShortByteString` (short long-lived CAFs stay unpinned).

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
