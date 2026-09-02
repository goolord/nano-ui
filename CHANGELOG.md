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
* `drawing`: immediate vector ops (`Stroke`, `FillTriangle`, `FillRect`, `DrawText`) painted after layout. `DrawText` takes an alignment point `(ax, ay)` (`ay < 0` is baseline).
* Package `nano-ui-diagrams`: diagrams-lib backend for plots and drawings (`diagram`, `linePlot`, `scatterPlot`, `barPlot`). Chart chrome (`labeledChart`, legends, tick labels, frame) and series helpers (`inkLine`, `fillBars`, `inkScatter`) come from `PlotStyle`, derived from the nano-ui `Theme`. `fillW` charts keep the envelope aspect, request a min size so host-font tick labels do not overlap, and center in leftover width. Scale stays uniform. Label fit skips path tessellation. Fitted layout is cached per widget and theme. Draw ops are cached by size and translated when the widget moves. A debug window does not force a 60fps redraw; SDL refreshes the HUD on `debugRefreshSec`.
* `Theme` adds `themeRed`, `themeOrange`, `themeYellow`, `themeGreen`, and `themePurple`. `themeSeries` is that list plus `themeAccent`. Record construction of a `Theme` needs updating. Plot ink uses `themeRed`.
* Right-click Cut/Copy/Paste on a text input or textarea does not require focus first. Hovering the field always uses the I-beam.
* I-beam cursor tracks the field well even when the hot widget does not change. SDL still syncs the cursor on skipped frames.
* Cut collapses the selection to a caret after deleting in a text input or textarea.
* Selected radio inner disc is `0.72` of the well (was `0.58`).
* 2D scroll lays content out at its measured size. Vertical bar clip no longer shrinks table columns.
* Table cell text is inset. Zebra and header fills use the full column box.
* Table tab help is two lines, and the table is fillW, so columns use the card width.
* Grow labels skip unwrapped width when the parent will assign a slot (wrap/Grow). A Fit parent measures content so a lone `muted` does not collapse.
* Color picker shows Current Color and New Color swatches side by side.
* Plot y-axis labels keep a left phantom so host-font glyphs are not clipped.
* Plot widgets grow until host-font tick labels do not overlap. `DrawText` boxes use host metrics (`drawTextBox`).
* Color picker copies New Color onto Current Color when the SV or hue drag is released, and after arrow-key edits.
* Checked checkbox uses the same well and rounded stroke as unchecked. The mark is coverage-AA, not capsule stamps.
* Text field caret, selection, and click use glyph advances, not TTF string size, so runs of `f` do not shift the caret left.

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
