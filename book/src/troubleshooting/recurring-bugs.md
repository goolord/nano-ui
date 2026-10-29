# Recurring Bugs & Fixes

Bugs and fixes from the git histories of nano-ui applications (`ned`, `stronghead`, and `winget-gui`). Each entry starts with the symptom.

## Symptom index

| Symptom | Section |
| --- | --- |
| Buttons and labels shift every frame | [Layout jitter](#1-layout-shifting--jitter) |
| A list scrolls sideways when one row is long | [Layout jitter](#1-layout-shifting--jitter) |
| A dropdown clips its longest option | [Layout jitter](#1-layout-shifting--jitter) |
| A box inside a box inside a dialog | [Double borders](#2-double-borders--box-in-a-box) |
| Text shimmers while scrolling at 125% scaling | [Text metrics](#3-text-metrics--the-pixel-grid) |
| A custom drawing does not update | [Stale cached drawings](#4-stale-cached-drawings) |
| A drag ghost freezes where it first appeared | [Stale cached drawings](#4-stale-cached-drawings) |
| Content runs under the scrollbar | [Scroll details](#5-scroll-details) |
| Ctrl+A fires while typing | [Focus and input ordering](#6-focus-and-input-ordering) |
| Escape does not close anything while a field is focused | [Focus and input ordering](#6-focus-and-input-ordering) |
| Typed keys never reach a custom widget | [Focus and input ordering](#6-focus-and-input-ordering) |
| A drag selects one character leftward | [Focus and input ordering](#6-focus-and-input-ordering) |
| SVG clip paths collide | [SVG export](#7-svg-export) |

## 1. Layout shifting & jitter

### A label whose text changes length moves its siblings every frame

A counter going `9 -> 10 -> 100`, a timer, or a byte total changes the label's natural width, which re-flows the row on every tick.

**Fix:** pin the label to the width of its widest expected value:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Troubleshooting.hs:fixed_width}}
```

`stronghead` made this fix more than once ("fix markers count to fixed width").

### A long value inside a scrolling list widens the whole list

One long row increases the column's natural width, the list grows a horizontal scrollbar, and the bar appears and disappears as wide rows enter and leave.

**Fix:** make the cell fill its column instead of contributing natural width, and ellipsize the text:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Troubleshooting.hs:name_line}}
```

### A drop-down clips its longest option

A fixed width guessed for a select breaks when an option is long (`"Unknown disk · 113 apps · 9.24 GB"`).

**Fix:** measure the options with the font and add the chrome:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Troubleshooting.hs:measured_select}}
```

### Content-derived geometry recomputes and jitters

Tree column widths, row heights, and pane splits that are derived from the current content every frame move when the content changes.

**Fix:** lock the geometry. Use a constant row height, and store a pane split the user chose in saved layout state (`ned`'s "grid size locking").

### `flex` around the wrong child

Make the one growable child `fillW` instead. `stronghead` replaced `fixedW 90` + `flex` around a filter box with `fillW`, which also removed dead space.

## 2. Double borders & box in a box

nano-ui fills padded containers with the panel color, and `scrollArea`, `panel`, and `card` each draw their own background, border, and radius. Nesting them inside popups, dialogs, and panes draws a box inside a box.

Symptoms:

- A `scrollArea` in a popup shows a bordered well inside the dialog.
- A dialog has two concentric borders.
- A pane with a panel inside a card looks recessed twice.

**Fix:** one level supplies the chrome.

- Give the outer dialog `padAll 0` and move the padding to the inner `columnWith (fillW . padAll 12 ...)`.
- Or strip the inner surface:

  ```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Troubleshooting.hs:strip_chrome}}
```

- Set `themeFloatingWindow` and `themePopup` to the panel surface, so a padded body does not show a panel box inside the dialog.
- Between two chrome regions, draw one `separator` at the seam, not a border on the bottom of one and the top of the other.

`stronghead` ("Remove duplicate borders from workspace popovers and notifications") and `winget-gui`'s dialogs both made this fix.

## 3. Text metrics & the pixel grid

### Glyphs drift over a long run

`fmAdvance` per glyph multiplied by the character count is not the shaped width of a run. A shaping backend lays a run out by unrounded advances (8 vs 8.25 px at size 15), so per-glyph arithmetic drifts a cell every ~32 characters and long runs overdraw what follows.

**Fix:** measure the whole run:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Troubleshooting.hs:run_width}}
```

(`ned`: "text stays on the cell grid along a long run".)

### Text shimmers at fractional DPI scales

At 125% scaling, rows of 22.4 px stack as 22, 23, 22, 23 and text shimmers while scrolling. Positions and row heights must be whole device pixels.

**Fix:** snap with the font's snap scale:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Troubleshooting.hs:snap}}
```

`winget-gui`'s `rowButton` does this.

### Box-centred is not optically centred

A large label over a small one centres by line boxes, which leave more room under the smaller line, so the block reads about 4 px high. Nudge it with `padTop`, or centre on the ink: measure the ink span and the cap/x-height midpoints from `FontMetrics` glyphs.

### `alignMid` on a row positions the row, not its contents

Centring the row's *contents* takes `alignMid` on each child. This was `stronghead`'s "header dead space" fix.

## 4. Stale cached drawings

Without a content key, a custom widget's ops are rebuilt and diffed every frame. With a key, unchanged frames skip both, so a key that misses an input shows stale pixels.

### The drawing freezes on an input the key missed

`winget-gui`'s sort arrow stayed on the old column until the key covered everything the drawing read.

**Fix:** key on every input: values, colours, text, theme, scroll viewport width, animation state. `contentKey` hashes numbers; fold colours and text in with `mix64` from `NanoUI.Backend`.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Troubleshooting.hs:sort_arrow}}
```

### A pinned overlay keeps its first frame's ops

A drag ghost pinned under the pointer never changes its layout rectangle, so an unversioned drawing keeps the frame it appeared in (`ned`'s "tab ghost").

**Fix:** version it by what moves it:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/CustomDrawing.hs:versioned}}
```

### Widths from the scroll viewport belong in the key

`ned`'s finder rows clamp to `rectW (scrollViewport metrics)` and key on it, so the drawing follows the viewport when the scrollbar appears.

## 5. Scroll details

### Content runs under the scrollbar

The scrollbar keeps a lane out of the viewport. A wide row drawn to the widget's full width runs beneath the bar. Clamp content and hit targets to the viewport width, but keep the backdrop filling the widget or its edge shows as a seam down the lane (`ned`'s "hold the finder's rows out of its scrollbar's lane").

### The wheel step does not match the row height

The default notch (60 px) jumps about one row of 54 px or three smaller rows. Set the step so a notch is one row:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Layout.hs:wheel_step}}
```

A scroller can also set its own step with `setScrollStepUi`; `0` returns it to the context step.

## 6. Focus and input ordering

### Focus questions at the top of the view always say "no"

The node arena is reset before the view runs, so `isFocused` (or any check of editing state) at the top of `appView` answers `False` every frame.

**Symptom:** Ctrl+A selects every item while the search box is being typed into; Delete opens the uninstall dialog.

**Fix:** read focus and place shortcuts at the **end** of the view, after the widgets have run:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Troubleshooting.hs:focus_at_end}}
```

### Escape behind the same guard as Ctrl+A

Text inputs do not claim Escape. If Escape shares the "a field is editing" guard with Ctrl+A and Delete, Escape can never clear a selection once a field has focus.

**Fix:** give Escape its own condition.

### Typed keys never reach a custom widget

A focused widget that reads `inputChars` must call `useInputMethod` every frame it accepts text, even when it is not a text field. `ned`'s file tree takes vim's `j`/`k` as typed text; only the editor asked for input, so the keys never reached the tree.

**Fix:**

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:input_method}}
```

### A slow leftward drag selects one character

A one-character selection's anchor sits on its left, so re-deriving the anchor from the selection each frame carries it along with a leftward drag (`ned`'s "slow leftward drags").

**Fix:** keep the anchor from the press.

### A widget that appears next frame cannot be focused this frame

`requestFocus` resolves after layout, but only for widgets declared on that pass. If a field appears because of a state change, request focus on the pass that declares it (store a `focusPending` flag). See [Keyboard, Focus & IME](../input/index.md).

### Conditional widgets shift ids

A branch that declares different widgets than its alternative changes the id count for everything after it; widgets appear to swap caret state. Wrap the branch in `scope`, and keep overlays unconditional. See [Widget Identity & Keys](../state/identity.md).

## 7. SVG export

- Several canvas drawings in one frame emit colliding `clip_`/`grad_` element ids. Scope them per drawing (prefix each `<g><defs>` block with an index), as `stronghead` does.
- Right-aligned numerals lose their anchor: `text-anchor`/`textLength` may need post-processing, and tabular numbers help readouts (`font-variant-numeric: tabular-nums` at the root).

## Quick checklist

- [ ] `liftIO (newVar ...)` inside a view: allocates every pass. Move it to setup.
- [ ] Unguarded side effects: the view re-runs. Wrap them in `whenM (button ...)`.
- [ ] Conditional widgets or `useVar`s without `scope`; keyed lists using indices.
- [ ] Overlays called conditionally instead of unconditionally with a `Var Bool`.
- [ ] `useTask` inside a tab or branch: the task is cancelled when hidden.
- [ ] `keepAnimating` at an always-run top level: a busy loop. Call it in the animating widget; use `wakeAfter` for timers.
- [ ] Writing a freshly computed `NaN` to a `Var Float`: `NaN /= NaN`, so it redraws forever. Guard the write.
- [ ] A custom widget's content key missing an input: stale pixels or a frozen drawing.
- [ ] A changing label without `fixedW`: its siblings shift every frame.
- [ ] A wide cell in a scrolling list without `fillW`: the list scrolls sideways.
- [ ] A `scrollArea` / `panel` / `card` nested in a popup: a box inside a box.
- [ ] Focus queries at the top of the view: the arena was reset; read them at the end.
- [ ] Escape behind the same guard as Ctrl+A/Delete: text inputs do not claim Escape.
- [ ] A focused widget reading `inputChars` without `useInputMethod`: typed text never arrives.
- [ ] Per-glyph advance × count for a long run: use `lineWidth`.
- [ ] Parsing or rebuilding documents per frame: parse once and keep the result outside the view.
- [ ] `readVar` above the widget that edits the `Var`: an extra pass per change.
- [ ] Missing `-threaded`.
- [ ] Touching `Context` from a worker thread: wake with `askWake` instead.
