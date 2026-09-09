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
* SDL3 native file dialogs: non-blocking `openFileDialog` / `askOpenFileDialog`, `saveFileDialog` / `askSaveFileDialog`, and `openFolderDialog` / `askOpenFolderDialog` wrap `SDL_ShowOpenFileDialog`, `SDL_ShowSaveFileDialog`, and `SDL_ShowOpenFolderDialog`. Poll results with `pollFileDialog` / `pollFileDialogUi` (each result delivered once; a stale handle polls as `FileDialogUnknown`), abandon a handle with `cancelFileDialog`, and clear state at teardown with `clearDialogState`. The SDL dialog callback only records the outcome and wakes the loop, so completion is safe on SDL's background thread.
* `searchField` / `searchFieldConfigured` and `SearchFieldConfig`: a caption-less search box built on the existing `NodeTextInput`. It reuses the whole text-edit stack (caret, selection, context menu, scroll) and adds an embedded magnifier on the left and a clear (×) button on the right that empties the field and keeps focus. Change pulses are debounced (default 300 ms trailing edge; `searchFieldEmit` delivers the settled query); clearing fires immediately. The placeholder is the caption-less label. Config exposes `sfcPlaceholder`, `sfcDebounceMs`, and `sfcLayout`.
* Search-mode is a style bit on the text-input node, so text inputs keep their old caption layout unless the flag is set; a search box measures to field height only (no caption row).
* Steady-state clip damage: a parked pointer no longer re-damages its hot widget and panel backdrop every frame (only an id change or rect move repaints), animated widgets damage their own rect without expanding to the whole panel backdrop, an animation that is live but scroll-clipped out of view no longer forces `DamageFull`, and a running animation no longer forces a full present every frame (only the settle frame where it finishes). Same-key text changes that keep the rect (monospace counters, HUD readouts) damage the text node's rect via the revived `dsPrevNodeTexts` snapshot. The animating wait is paced at ~16 ms instead of a 0 timeout so skip frames do not busy-spin. With the debug HUD open the demo went from 60 full presents/s to ~4 clipped presents/s; an in-view animation presents only its own ~1%-of-window rect.
* Damage-scoped final blit (SDL): when damage is a clip and the renderer's backbuffer persists across presents (the SDL software renderer — GPU swapchains may discard, so they keep the full-blit repair), the retain texture blits only the snapped damaged region instead of the whole window. Verified pixel-identical against the forced-full path.
* Scrollbar chrome repaints: wheel and keyboard offsets move content via `storePoint` under the `slotTextAreaScroll` slot (not just floating-pane offsets in `storeFloat`), so offset damage now scans both and returns the scroll node's full rect (content viewport plus its scrollbar lane), not the content clip. A wheel at the top, a scroll round-trip, and text refresh no longer leave a stale 1px thumb edge that only a forced-full repaint fixes.
* Expose/occlude: a window redraw event (SDL `SDL_EVENT_WINDOW_EXPOSED` / `SDL_EVENT_WINDOW_OCCLUDED_RESUMING`) marks the frame dirty and forces the next present full, so a restored window never shows a stale backbuffer. Tracked via `inputWindowRedraw` on `Input`.
* Honest present rate: `presentRate` counts actual presents since the previous HUD refresh instead of falling back to `1000/frameMs`, so the debug HUD shows the real frame rate when the app is idle with frames skipping.
* Wait pacing: the present-locked 0 ms wait ("let the display throttle") only applies when vsync is enabled, because `SDL_RenderPresent` is what blocks. With `--vsync=false` a live in-view animation would otherwise spin the loop at max speed and flicker the whole window, so it paces at `animateTimeout` instead. When frames skip (empty damage, e.g. an animation scrolled out of view), it also paces at `animateTimeout` so the loop does not busy-spin.
* Frame-aligned pacing: after a timed-out event wait the loop winds on to the exact frame boundary (bulk `threadDelay`, then a ≤1 ms tail spin) instead of drifting with the event waiter's timer granularity. The pacing period is the window's current display refresh (queried from `SDL_GetCurrentDisplayMode`), so a vsync-less animation runs at the display cadence (16.666 ms on 60 Hz, ~6.9 ms on 144 Hz) rather than a fixed 16 ms clock tick — removes the beat between the two rates. The bounded spin runs only while an animation is actively presenting, so an eternal in-view animation no longer renders with low, uneven frame times.
* Display refresh observability: the debug snapshot carries `dbgRefreshHz` (the pacing target from SDL, with a fallback to the highest same-size mode refresh when the current mode reports 0), shown in the HUD (`Display`) and in `NANO_FRAME_TRACE=1` output so measured presents can be compared against the pacing target.
* Event-wait overrun: the paced wait now sleeps ~2 ms short of the frame period so the frame-boundary spin absorbs SDL's event-waiter overrun; without it the loop cadence settled ~1 ms late and vsync-less animation looked uneven.
* Float-clock precision: the demo's progress sweep used `realToFrac <$> getMonotonicTime`, which quantizes wall-clock seconds to 'Float' (~3 ms lost at 8 h uptime, coarser than an 8.3 ms frame at 120 Hz) and made the bar edge step. Clock math now stays in 'Double'.
* Time/clock API: `NanoUI.Monad.uiTime` is the monotonic seconds clock as a 'Double', for time-based animation math (never funnel it through 'Float'). `pulse` / `keepAnimating` in `Widgets.Animate` (re-exported through `nanoui`): `pulse period` yields a smooth clock-driven `[0,1]` sine sweep and `keepAnimating resp` holds a wall-clock widget live forever so the frame loop keeps it repainting; `progressBar =<< pulse 6` + `keepAnimating resp` replaces the old manual `getMonotonicTime` + eternal `startAnimation` demo pattern. `stopAnimation` (from `NanoUI.Context` and `NanoUI.Testing`) freezes a running animation in place at its current value and keeps the context from reporting as animating.
* Search debounce timing moved to a new `storeDouble` store slot: the trailing-edge window is measured in 'Double' wall-clock seconds, so it stops shifting (or skipping) at long uptimes where Float loses resolution (~125 ms at 12 days).
* A perpetual animation whose widget leaves the arena (a `keepAnimating` progress bar behind a tab switch) no longer forces a whole-window repaint forever. `writeDamage` only escalates a rect-less live animation to `DamageFull` for the first `orphanEscalateFrames` (2) consecutive frames it is missing, tracked by a per-key rect-less frame counter maintained in `updatePrevRects`. A fresh animation on a widget that is about to be laid out still gets full coverage for those first frames.
* RGFW flicker: the RGFW backend no longer unconditionally clears, re-renders, and re-blits the whole window on every loop tick. It now sleeps blocking until the first event when idle (skip frames do no work at all), paces live animations at the refresh period (`optRefreshHz`, default probe→60 Hz) instead of a fixed 8.3 ms spin, and only present when the frame actually needs repainting (damage, running animation, settle frame, text editing, or a pointer/scroll edge). A new `RGFW_waitForEvent` binding (blocking/polling wait over the display connection) backs the gated event waiter. The opening frame is presented before the loop so the window has content immediately. Build fix: the RGFW `SessionDriver` was missing the `sdAlignSec` field and never compiled; `cozetteMetrics` was missing `fmSnapScale`.

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
