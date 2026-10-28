# Recurring Bugs & Fixes

A field guide mined from the git histories of real nano-ui applications (`ned`, `stronghead`, and `winget-gui`). Each entry states the symptom first, because that is what you have when you arrive here.

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

---

## 1. Layout shifting & jitter

### A label whose text changes length moves its siblings every frame

A counter going `9 -> 10 -> 100`, a timer, or a byte total changes the label's natural width, which re-flows the row on every tick.

**Fix:** pin the label to the width of its widest expected value:

```haskell
labelWith (fixedW 48 . alignEnd) (fmtInt count)
```

This is a recurring fix in `stronghead` ("fix markers count to fixed width") and in dashboards generally.

### A long value inside a scrolling list widens the whole list

One long row increases the column's natural width, the list grows a horizontal scrollbar, and the bar appears and disappears as wide rows enter and leave.

**Fix:** make the cell fill its column instead of contributing natural width, and ellipsize the text:

```haskell
-- In a row, a label too narrow for its text cuts it off rather than wrapping.
nameLine f = rowWith (fillW . tight) . labelWith (fillW . f)
```

Inside a scrolling list, cells should be `fillW`, not sized by content.

### A drop-down clips its longest option

A fixed width guessed for a select breaks when an option is long (`"Unknown disk · 113 apps · 9.24 GB"`).

**Fix:** measure the options with the font and add the chrome:

```haskell
let w = maximum (0 : map (lineWidth fm) options) + 56
selectWith (fixedW w) options selectedVar
```

### Content-derived geometry recomputes and jitters

Tree column widths, row heights, and pane splits that are derived from the current content every frame move when the content changes.

**Fix:** lock the geometry in saved layout state (`ned`'s "grid size locking") or key constants off the content type, not the text. For a row height, use a constant; for a pane split, store the user's chosen size.

### `flex` around the wrong child

`flex` still works, but the clearer pattern is to make the one growable child `fillW` directly. `stronghead` replaced `fixedW 90` + `flex` around a filter box with `fillW`, which also removed dead space.

---

## 2. Double borders & box in a box

nano-ui fills padded containers with the panel color, and `scrollArea`, `panel`, and `card` each draw their own background, border, and radius. Nesting them inside popups, dialogs, and panes draws a box inside a box.

Symptoms:

- A `scrollArea` in a popup shows a bordered well inside the dialog.
- A dialog has two concentric borders.
- A pane with a panel inside a card looks recessed twice.

**Fix:** exactly one level owns the chrome.

- Give the outer dialog `padAll 0` and move the padding to the inner `columnWith (fillW . padAll 12 ...)`.
- Or strip the inner surface:

  ```haskell
  styled (inputStyle (background colorTransparent . borderWidth 0)) ...
  ```

- Set `themeFloatingWindow` and `themePopup` to the panel surface, so a padded body does not show a panel box inside the dialog.
- Between two chrome regions, draw one `separator` at the seam — not a border on the bottom of one and the top of the other.

The `stronghead` fix was named "Remove duplicate borders from workspace popovers and notifications"; the same change appears in `winget-gui`'s dialogs.

---

## 3. Text metrics & the pixel grid

### Glyphs drift over a long run

`fmAdvance` per glyph multiplied by the character count is not the shaped width of a run. A shaping backend lays a run out by unrounded advances (8 vs 8.25 px at size 15), so per-glyph arithmetic drifts a cell every ~32 characters and long runs overdraw what follows.

**Fix:** measure the whole run:

```haskell
lineWidth fm run
```

`ned` fixed this as "text stays on the cell grid along a long run".

### Text shimmers at fractional DPI scales

At 125% scaling, rows of 22.4 px stack as 22, 23, 22, 23 and text shimmers while scrolling. Positions and row heights must be whole device pixels.

**Fix:** snap with the font's snap scale:

```haskell
snap v = let k = max 1 (fmSnapScale f) in fromIntegral (round (v * k) :: Int) / k
```

`winget-gui`'s `rowButton` is the reference implementation.

### Box-centred is not optically centred

A two-line block with a large label over a small one centres by line boxes, which leave more room under the smaller line; it reads ~4 px high. For exact optical centring, centre on the ink: measure the ink span and the capital/x-height midpoints from `FontMetrics` glyphs.

### `alignMid` on a row positions the row, not its contents

`alignMid` on a row positions the row itself in its parent. Centring the row's *contents* takes the modifier on each child (or `alignMid` in the child's layout). This is the "header dead space" fix in `stronghead`.

---

## 4. Stale cached drawings

A custom-drawn widget has no text spans and no automatic diff of its inputs. Without a content key, its ops are rebuilt and diffed every frame. With a key, unchanged frames skip both — and a key that misses an input shows stale pixels.

### The drawing freezes on an input the key missed

`winget-gui`'s sort arrow stayed on the old column until the key covered everything the drawing read.

**Fix:** include every input in the key — values, colors, text, theme, scroll viewport width, animation state:

```haskell
drawingVersioned (contentKey [round sortCol, round scrollY, fromEnum theme]) $ \dc -> ...
```

If a keyed drawing shows stale pixels, the key is missing an input; the widget is not broken.

### A pinned overlay keeps its first frame's ops

A drag ghost pinned under the pointer with `pinAt mx my` never changes its layout rectangle, so an unversioned drawing caches the frame it appeared in (`ned`'s "tab ghost").

**Fix:** give it an explicit version that includes what moves it:

```haskell
drawingVersioned (contentKey [round mx, round my]) $ do
  drawDragGhost
```

### Widths from the scroll viewport belong in the key

`ned`'s finder rows clamp to `rectW (scrollViewport metrics)` and key on it, so the drawing follows the viewport when the scrollbar appears.

---

## 5. Scroll details

### Content runs under the scrollbar

The scrollbar keeps a lane out of the viewport. A wide row drawn to the widget's full width runs beneath the bar. Clamp content and hit targets to the viewport width, but keep the backdrop filling the widget or its edge shows as a seam down the lane (`ned`'s "hold the finder's rows out of its scrollbar's lane").

### The wheel step does not match the row height

The default notch (~60 px) jumps about one row of 54 px or three smaller rows. Set the wheel step per scroller so a notch is exactly one row:

```haskell
ctx <- askContext
liftIO $ setScrollTuning ctx defaultScrollTuning
  {scrollWheelStep = rowHeight, scrollSmoothTime = 0}
```

A scroller can also set its own step with `setScrollStepUi`; `0` returns it to the context step.

---

## 6. Focus and input ordering

### Focus questions at the top of the view always say "no"

The node arena is reset before the view runs. `isFocused` (and any check of editing state) at the top of `appView` answers `False` every frame.

**Symptom:** Ctrl+A selects every item while the search box is being typed into; Delete opens the uninstall dialog.

**Fix:** read focus and place shortcuts at the **end** of the view, after the widgets have run:

```haskell
appView = do
  searchResp <- searchInput' query
  -- ... all widgets ...
  searching <- isFocused (respId searchResp)
  when (not searching) $ whenM (shortcut (key KeyDelete)) deleteSelected
```

This was `winget-gui`'s worst input bug.

### Escape behind the same guard as Ctrl+A

Text inputs do not claim Escape. If Escape shares the "a field is editing" guard with Ctrl+A and Delete, Escape can never clear a selection once a field has focus.

**Fix:** give Escape its own condition. It must keep working while a field is focused.

### Typed keys never reach a custom widget

A focused widget that reads `inputChars` must call `useInputMethod` every frame it accepts text, even when it is not a text field. `ned`'s file tree takes vim's `j`/`k` as typed text; only the editor asked for input, so the keys never reached the tree.

**Fix:**

```haskell
-- Every frame the widget accepts text:
preedit <- useInputMethod wid InputNormal caretRect
for_ preedit drawCompositionAtCaret
```

### A slow leftward drag selects one character

The drag anchor must be taken from the press, not re-derived from the selection each frame. A one-character selection's anchor sits on its left, so re-deriving it carries the anchor along and selects one character (`ned`'s "slow leftward drags").

### A widget that appears next frame cannot be focused this frame

`requestFocus` resolves after layout, but only for widgets declared on that pass. If a field appears because of a state change, request focus on the pass that declares it (store a `focusPending` flag). See [Focus, Navigation & IME](../input/focus.md).

### Conditional widgets shift ids

A branch that declares different widgets than its alternative changes the id count for everything after it; widgets appear to swap caret state. Wrap the branch in `scope`, and keep overlays unconditional. See [Widget Identity & Keys](../state/identity.md).

---

## 7. SVG export

When exporting with `nano-ui-svg`:

- Several canvas drawings in one frame emit colliding `clip_`/`grad_` element ids. Scope them per drawing (prefix each `<g><defs>` block with an index), as `stronghead` does.
- Right-aligned numerals lose their anchor: `text-anchor`/`textLength` may need post-processing, and tabular numbers help readouts (`font-variant-numeric: tabular-nums` at the root).

---

## Quick checklist

- [ ] `liftIO (newVar ...)` inside a view — allocates every pass. Move it to setup.
- [ ] Unguarded side effects — the view re-runs. Wrap them in `whenM (button ...)`.
- [ ] Conditional widgets or `useVar`s without `scope`; keyed lists using indices.
- [ ] Overlays called conditionally instead of unconditionally with a `Var Bool`.
- [ ] `useTask` inside a tab or branch — the task is cancelled when hidden.
- [ ] `keepAnimating` at an always-run top level — a busy loop. Call it in the animating widget; use `wakeAfter` for timers.
- [ ] Writing a freshly computed `NaN` to a `Var Float` — `NaN /= NaN`, so it redraws forever. Guard the write.
- [ ] A custom widget's content key missing an input — stale pixels or a frozen drawing.
- [ ] A changing label without `fixedW` — its siblings shift every frame.
- [ ] A wide cell in a scrolling list without `fillW` — the list scrolls sideways.
- [ ] A `scrollArea` / `panel` / `card` nested in a popup — a box inside a box.
- [ ] Focus queries at the top of the view — the arena was reset; read them at the end.
- [ ] Escape behind the same guard as Ctrl+A/Delete — text inputs do not claim Escape.
- [ ] A focused widget reading `inputChars` without `useInputMethod` — typed text never arrives.
- [ ] Per-glyph advance × count for a long run — use `lineWidth`.
- [ ] Parsing or rebuilding documents per frame — parse once and keep the result outside the view.
- [ ] `readVar` above the widget that edits the `Var` — an extra pass per change.
- [ ] Missing `-threaded`.
- [ ] Touching `Context` from a worker thread — wake with `askWake` instead.
