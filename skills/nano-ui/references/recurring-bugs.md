# Recurring bugs and their fixes

Bugs that were made and fixed in real nano-ui applications: ned (a vim-like
editor), stronghead (a GHC profiling and diagnostics GUI) and winget-gui (a
Windows package manager front end). Check here before debugging a similar
symptom; the patterns repeat.

| Symptom | Section |
| --- | --- |
| Buttons and labels shift every frame | Layout shifting and jitter |
| A list scrolls sideways when one row is long | Layout shifting and jitter |
| A drop-down clips its longest option | Layout shifting and jitter |
| A box inside a box inside a dialog | Double borders |
| Text shimmers while scrolling at 125% scaling | Text metrics |
| A custom drawing does not update | Cached drawings |
| A drag ghost freezes where it first appeared | Cached drawings |
| Content runs under the scrollbar | Scroll details |
| Ctrl+A fires while typing | Input ordering and focus |
| Escape does nothing while a field is focused | Input ordering and focus |
| Typed keys never reach a custom widget | Input ordering and focus |
| SVG clip paths collide | SVG export |

## Layout shifting and jitter

- **A label whose text changes length moves its siblings every frame.** A
  marker count going 9, 10, 100, a timer, a byte total: each change re-flows
  the row. Pin the label to the width of its widest value, as stronghead does
  with `paneMetaWith (fixedW 32) (fmtInt nMarkers)`.
- **A long value inside a scrolling list widens the whole list**, which then
  scrolls sideways, and the horizontal bar comes and goes as wide rows are
  built and dropped. Make the cell fill its column instead of contributing
  natural width; the text then ends in `...`. From winget-gui:

  ```haskell
  -- In a row, a label too narrow for its text cuts it off rather than wrapping.
  nameLine f = rowWith (fillW . tight) . labelWith (fillW . f)
  ```

- **A drop-down clips its longest option.** A guessed fixed width breaks on
  long options ("Unknown disk · 113 apps · 9.24 GB"). Measure the options
  with the font and add the insets and the arrow:
  `fixedW (maximum (0 : map (lineWidth fm) options) + 56)`.
- **Content-derived grid sizes recompute every frame and jitter.** Lock the
  geometry: ned keeps its tree and editor column widths in saved layout state;
  winget-gui keys row heights on constants, not on text.
- `flex` inside a row works, but making the one growable child `fillW` is
  simpler. stronghead replaced `fixedW 90` plus `flex` around a filter box with
  `fillW`, which also removed dead space.

## Double borders and boxes in boxes

Padded containers fill with the panel colour, and `scrollArea`, `panel` and
`card` each draw their own background, border and radius. Nested inside
popups, dialogs and panes they draw a box inside a box.

- A `scrollArea` in a popup showed a bordered well inside the dialog.
  stronghead's fix: `padAll 0` on the popup's layout, the padding moved onto
  the inner `columnWith (fillW . padAll 12 ...)`, and a borderless scroller, so
  one level owns the chrome. Dialogs and floating windows work the same way:
  winget-gui sets `themeFloatingWindow` and `themePopup` to the panel surface
  so a padded body shows no panel box inside the dialog.
- To strip chrome from an inner surface:
  `styled (inputStyle (background colorTransparent . borderWidth 0)) ...`.
- Between two chrome regions, draw one `separator` at the seam in the parent,
  not a border on the bottom of one and the top of the other.

## Text metrics and the pixel grid

- **Per-glyph advance times count is not a shaped run's width.** A shaping
  backend lays out a run with unrounded advances (8 against 8.25 px per glyph
  at size 15), so per-glyph arithmetic drifts a cell every ~32 characters and
  long runs overdraw what follows. Measure the whole run: `lineWidth fm run`
  (ned).
- **Rows must be a whole number of device pixels tall.** At 125% scaling,
  rows of 22.4 px land 22, 23, 22, 23 px apart and text shimmers. Snap
  positions with the font's snap scale, as winget-gui's `rowButton` does:

  ```haskell
  snap v = let k = max 1 (fmSnapScale f) in fromIntegral (round (v * k) :: Int) / k
  ```

- **Box-centred is not optically centred.** A big label over a small one,
  centred by line boxes, leaves more room under the smaller line and reads
  about 4 px high (fixed with a 4 px `padTop` in winget-gui). For exact optical
  centring, centre on the ink: winget-gui's `rowButton` measures the ink span
  and the capital and x-height midpoints from `FontMetrics` glyphs.
- **`alignMid` on a row positions the row itself** in its parent. Centring the
  row's contents takes the modifier on each child. The same holds for a header
  band, or the row's own padding dominates what the eye measures.

## Cached drawings that don't update

- **A keyed custom widget freezes on an input its key missed.** winget-gui's
  sort arrow stayed on the old column until the key covered everything the
  drawing reads. Stale pixels from a keyed drawing mean the key is missing an
  input, not that the widget is broken.
- **A pinned overlay whose rect never changes keeps its first frame's ops.** A
  drag ghost following the pointer causes no layout change, so an unversioned
  drawing caches the frame it appeared in. Version it by what moves it with
  `drawingVersioned` (ned's tab ghost).
- **Widths derived from the scroll viewport belong in the key too.** ned's
  finder rows clamp to `rectW (scrollViewport metrics)` and key on it, so the
  drawing follows the viewport when the scrollbar appears.

## Scroll details

- **Content can run under the scrollbar.** The scrollbar keeps a lane out of
  the viewport; a row drawn to the widget's full width runs beneath the bar.
  Clamp content and hit targets to the viewport width, but keep the backdrop
  filling the widget or its edge shows as a seam down the lane (ned's finder).
- **Tune the wheel step to the row height.** The default notch (~60 px) jumps
  about one 54 px row or three small ones. winget-gui calls `setScrollTuning`
  so one notch is exactly one row, eased.

## Input ordering and focus

- **Ask focus questions at the end of the view.** The node arena is reset
  before the view runs, so a focus query at the top of the view always
  answered "no": winget-gui's Ctrl+A selected every app while the search box
  was being typed into, and Delete opened the uninstall dialog. The fix moved
  the guards below the widgets.
- **Escape is not claimed by text inputs**, so Escape shortcuts must keep
  working while a field is focused. Don't share one "a field is editing" guard
  between Ctrl+A, Delete and Escape: winget-gui's Escape could not clear the
  selection while the search box had focus until it got its own condition.
- **A focused widget that reads typed text calls `useInputMethod` every
  focused frame**, even when it is not a text field. ned's file tree takes
  vim's j/k as typed text; only the editor asked for text input, so the keys
  never reached the tree.
- **Keep a drag's anchor from its press**, not re-derived from the selection
  each frame. A one-character selection's anchor sits on its left, so
  leftward drags carried the anchor along and selected one character (ned).

## SVG export (nano-ui-svg)

- Several canvas drawings in one SVG frame emit colliding `clip_`/`grad_`
  element ids. Scope them per drawing: stronghead prefixes each `<g><defs>`
  block with an index.
- Right-aligned numerals lose their anchor: `text-anchor`/`textLength` may
  need post-processing, and tabular numbers help readouts
  (`font-variant-numeric: tabular-nums` at the root).

## Testing custom-drawn widgets

A custom-drawn widget has no text spans, so span-based assertions see
nothing. Click it, take a screenshot, and assert on the image: winget-gui's
self-test clicks a column header and checks the screenshot for the sort arrow.
