# Recurring bugs and their fixes

Issues that came up repeatedly in real `nano-ui` apps: `ned` (a vim-like editor), `stronghead` (a GHC profiling and diagnostics GUI), and `winget-gui` (a Windows package manager front end). Scan here first when debugging a visual or input symptom.

| Symptom | Section |
| --- | --- |
| Buttons and labels jitter every frame | Layout shifting and jitter |
| A list scrolls sideways when one row is wide | Layout shifting and jitter |
| A dropdown cuts off its longest option | Layout shifting and jitter |
| A box appears inside another box in a dialog | Double borders and boxes in boxes |
| Text shimmers while scrolling at 125% scaling | Text metrics and the pixel grid |
| A custom canvas stops updating | Cached drawings that do not update |
| A drag ghost freezes where it first appeared | Cached drawings that do not update |
| List content slides under the scrollbar | Scroll details |
| Ctrl+A fires while typing in a text field | Input ordering and focus |
| Escape stops working while a field is focused | Input ordering and focus |
| Typed keys never reach a custom widget | Input ordering and focus |
| SVG clip paths collide on one page | SVG export (`nano-ui-svg`) |

## Layout shifting and jitter

- **Changing labels push siblings around every frame.** Counters (`9 -> 10 -> 100`), timers, and byte totals change natural width as digits change, reflowing the row on every tick. Pin them to the width of their widest value (`stronghead` uses `paneMetaWith (fixedW 32) (fmtInt nMarkers)`).
- **One wide cell makes a scrolling list scroll horizontally**, with the horizontal bar popping in and out as wide rows enter and leave view. Give the cell `fillW` so it truncates with `...` instead of widening the column (from `winget-gui`):

  ```haskell
  -- In a row, a label narrower than its text truncates with "..." rather than wrapping.
  nameLine f = rowWith (fillW . tight) . labelWith (fillW . f)
  ```

- **A dropdown clips its longest option.** Hardcoding a guessed width breaks on long labels (`"Unknown disk · 113 apps · 9.24 GB"`). Measure all options with the font and add room for padding and the dropdown chevron: `widths <- mapM (lineWidthUi fm) options`, then `fixedW (maximum (0 : widths) + 56)`.
- **Content-derived geometry jumps when visible rows change.** Keep column widths and row heights stable: `ned` persists tree and editor widths in layout state, and `winget-gui` derives row heights from constants rather than row text.
- **Prefer `fillW` over `flex` around a single expanding widget.** Replacing `fixedW 90` + `flex` around a filter box with `fillW` in `stronghead` simplified layout and removed dead space.

## Double borders and boxes in boxes

Padded containers paint the panel color, and `scrollArea`, `panel`, and `card` each draw their own background, border, and radius. Nesting them inside popups, dialogs, or panes produces concentric boxes.

- If a `scrollArea` inside a popup shows a recessed bordered well, give the outer popup `padAll 0`, move padding onto the inner `columnWith (fillW . padAll 12 ...)`, and make the scroller borderless so only one level owns the chrome (`stronghead`). Similarly, `winget-gui` matches `themeFloatingWindow` and `themePopup` to the panel surface so padded dialog bodies do not show an inner frame.
- Strip chrome from an inner scroll well with `styled (inputStyle (background colorTransparent . borderWidth 0)) ...`.
- Between adjacent chrome regions, draw a single `separator` in the parent instead of adjacent bottom and top borders.

## Text metrics and the pixel grid

- **Per-glyph advance times length drifts from shaped run width.** HarfBuzz positions glyphs with fractional advances (for example, 8.25 px vs. 8 px at size 15), drifting a full character every ~32 characters and overdrawing following content. Measure the full run in the view with `lineWidthUi fm run` (`ned`).
- **Snap row heights to whole device pixels.** At 125% scaling, 22.4 logical-pixel rows land 22, 23, 22, 23 physical pixels apart and shimmer while scrolling. Snap using `fmSnapScale` (from `winget-gui`'s `rowButton`):

  ```haskell
  snap v = let k = max 1 (fmSnapScale f) in fromIntegral (round (v * k) :: Int) / k
  ```

- **Line-box centering looks slightly high.** Stacking a large label over a small one leaves extra empty space below the smaller baseline, reading ~4 px too high. Nudge with `padTop`, or compute optical centering from glyph ink bounds and cap/x-height midpoints in `FontMetrics`.
- **`alignMid` on a row positions the row itself**, not its contents. Put `alignMid` on each child inside the row instead.

## Cached drawings that do not update

- **A keyed canvas freezes if its content key omits an input.** `winget-gui`'s sort arrow stayed stuck on the old column until its key included every value read during painting. Stale pixels on a keyed widget almost always mean a missing key input.
- **A pinned overlay with fixed layout bounds freezes on frame 1.** A drag ghost moved with `pinAt` does not trigger a layout diff, so wrap it in `drawingVersioned` keyed by its position (`ned`'s tab ghost).
- **Include scroll viewport width in custom row keys.** When rows clamp to `rectW (scrollViewport metrics)`, fold that width into the key so rows update whenever a scrollbar appears or disappears (`ned`'s finder).

## Scroll details

- **Content runs underneath the scrollbar.** The scrollbar claims a lane out of `scrollViewport`. Clamp row content and hit targets to `scrollViewport`, while keeping row highlight backgrounds spanning the full widget width so no vertical gap appears along the scrollbar edge (`ned`'s finder).
- **Match mouse-wheel step to row height.** The default 60 px notch feels uneven on 54 px rows or compact lists. Call `setScrollTuning` (or `setScrollStepUi`) so one notch steps cleanly by one row (`winget-gui`).

## Input ordering and focus

- **Run focus queries at the end of the view.** The node arena clears before the view runs, so calling `isFocused` at the top always returns `False` (in `winget-gui`, that caused Ctrl+A and Delete to fire while typing in the search box). Move focus checks and focus-dependent shortcuts below the widgets.
- **Keep Escape separate from text-editing guards.** Built-in text inputs leave Escape unclaimed. Sharing an "a field is focused" guard between Ctrl+A, Delete, and Escape stops Escape from clearing selections while a field has focus.
- **Call `useInputMethod` every focused frame for custom widgets reading `inputChars`.** Backends only enable OS text input when `useInputMethod` is active. Even non-editor widgets (like `ned`'s file tree accepting `j`/`k` as typed characters) need it to receive characters.
- **Preserve a text drag's anchor from the initial mouse press** instead of recomputing it from the current selection each frame; otherwise dragging slowly leftward drags the anchor along and never selects past one character (`ned`).

## SVG export (`nano-ui-svg`)

- When embedding multiple exported SVGs on one HTML page, ensure each uses a distinct `clip_`/`grad_` ID prefix (handled automatically by `frameToSvg`'s document hash, by passing `svgIdPrefix`, or by prefixing each `<g><defs>` block with an index as `stronghead` does).
- Right-aligned numerals can lose their anchor across browsers: `text-anchor` and `textLength` may need post-processing, and `font-variant-numeric: tabular-nums` at the document root keeps numeric columns aligned.

## Testing custom-drawn widgets

Custom canvases emit no text spans, so span-based assertions cannot inspect them. Drive clicks in a hidden window, capture a screenshot, and assert on the rendered pixels (`winget-gui`'s self-test clicks a column header and checks the rendered sort arrow).
