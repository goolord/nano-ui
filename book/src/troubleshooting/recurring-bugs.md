# Recurring Bugs & Fixes

These issues showed up repeatedly while building real applications with `nano-ui` (`ned`, `stronghead`, and `winget-gui`). Scan the symptom table to jump straight to what you are seeing.

## Symptom index

| Symptom | Section |
| --- | --- |
| Buttons and labels jitter sideways every frame | [Layout shifting & jitter](#1-layout-shifting--jitter) |
| A list starts scrolling sideways when one row is wide | [Layout shifting & jitter](#1-layout-shifting--jitter) |
| A dropdown cuts off its longest option | [Layout shifting & jitter](#1-layout-shifting--jitter) |
| A box appears inside another box in a dialog | [Double borders & box in a box](#2-double-borders--box-in-a-box) |
| Text shimmers while scrolling at 125% DPI scaling | [Text metrics & the pixel grid](#3-text-metrics--the-pixel-grid) |
| A custom canvas stops updating | [Stale cached drawings](#4-stale-cached-drawings) |
| A drag ghost freezes where it first appeared | [Stale cached drawings](#4-stale-cached-drawings) |
| List content slides underneath the scrollbar | [Scroll details](#5-scroll-details) |
| Ctrl+A selects everything while typing in a search box | [Focus and input ordering](#6-focus-and-input-ordering) |
| Escape stops working while a text field is focused | [Focus and input ordering](#6-focus-and-input-ordering) |
| Typed characters never reach a custom widget | [Focus and input ordering](#6-focus-and-input-ordering) |
| Dragging slowly leftward only selects one character | [Focus and input ordering](#6-focus-and-input-ordering) |
| SVG clip paths collide on the same page | [SVG export](#7-svg-export) |

## 1. Layout shifting & jitter

### A changing label pushes its siblings around

Counters (`9 -> 10 -> 100`), timers, and byte totals change width as their text grows or shrinks, causing the whole row to reflow on every tick.

**Fix:** reserve enough width for the widest value you expect:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Troubleshooting.hs:fixed_width}}
```

`stronghead` ran into this repeatedly on live marker counters until they were pinned with `fixedW`.

### One long row makes a scrolling list scroll horizontally

A single wide label stretches its column's natural width, causing a horizontal scrollbar to pop in and out as wide rows scroll through the viewport.

**Fix:** give the cell `fillW` so it fits within the column instead of widening it, letting `label` truncate overflow with `...`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Troubleshooting.hs:name_line}}
```

### A dropdown clips its longest option

Hardcoding a guessed width for `select` breaks as soon as an option is longer than expected (such as `"Unknown disk · 113 apps · 9.24 GB"`).

**Fix:** measure the options with the active font and add room for padding and the dropdown arrow:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Troubleshooting.hs:measured_select}}
```

### Content-driven column widths jitter as rows change

Deriving tree column widths, row heights, or pane splits from the currently visible rows makes the layout jump whenever the visible set changes.

**Fix:** keep structural geometry stable. Use constant row heights, and store user-adjusted column widths or pane splits in state (as `ned` does for its locked grid layout).

### Using `flex` where `fillW` belongs

When only one child in a row should grow (such as a filter box), give that widget `fillW` directly instead of pairing `fixedW` with a `flex` spacer. Doing so in `stronghead` both simplified the row and removed awkward dead space.

## 2. Double borders & box in a box

Padded containers paint the panel surface, and `scrollArea`, `panel`, and `card` each draw their own background, border, and corner radius. Nesting them inside popups, modals, or panes quickly creates nested boxes.

Common signs:

- A `scrollArea` inside a popup renders a recessed bordered box inside the popup frame.
- A dialog shows two concentric borders.
- A `panel` inside a `card` looks double-framed.

**Fix:** let only one layer own the chrome.

- Set `padAll 0` on the outer dialog and put the padding on an inner `columnWith (fillW . padAll 12 ...)`.
- Or strip the inner surface's background and border:

  ```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Troubleshooting.hs:strip_chrome}}
```

- Match `themeFloatingWindow` and `themePopup` to your panel surface so padded content blends cleanly into the dialog.
- Between adjacent chrome regions, place a single `separator` in the parent instead of drawing touching borders on both sides.

Both `stronghead` and `winget-gui` adopted these patterns to eliminate duplicate popover and dialog borders.

## 3. Text metrics & the pixel grid

### Character positions drift over long lines

Multiplying a single glyph's `fmAdvance` by the character count does not match a shaped run's true width. HarfBuzz lays runs out with fractional advances (for example, 8.25 px vs. 8 px at size 15), so per-glyph math drifts by a full cell every ~32 characters and causes text to overlap whatever follows.

**Fix:** measure the entire run in the view so the font backend shapes and caches the glyphs first:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Troubleshooting.hs:run_width}}
```

### Text shimmers while scrolling at fractional DPI scales

At 125% Windows display scaling, rows of 22.4 logical pixels round unevenly to 22, 23, 22, 23 physical pixels, making glyphs shimmer as the list scrolls. Row heights and positions need to land on whole device pixels.

**Fix:** snap coordinates with `fmSnapScale` (the pattern `winget-gui` uses in `rowButton`):

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Troubleshooting.hs:snap}}
```

### Geometric centering looks slightly high

Stacking a large label over a smaller label and centering by line boxes leaves extra empty space beneath the smaller line's baseline, making the pair look roughly 4 px too high. Either nudge it with `padTop`, or compute optical centering from the ink bounds and cap/x-height midpoints exposed by `FontMetrics`.

### `alignMid` on a row aligns the row itself, not its children

Applying `alignMid` to a `row` centers the row inside its parent. To vertically center items *inside* the row, put `alignMid` on each child instead.

## 4. Stale cached drawings

Unkeyed custom widgets rebuild and diff their draw ops every frame. Adding a content key avoids that work on unchanged frames, which means anything missing from the key will never trigger a redraw.

### A keyed canvas freezes when an input changes

In `winget-gui`, a custom sort arrow stayed stuck on the previous column until its key included every value read during painting.

**Fix:** include every dynamic input in the key: values, colors, strings, theme, scroll viewport width, and animation state. Hash floats with `contentKey`, and fold in colors or text with `mix64` from `NanoUI.Backend`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Troubleshooting.hs:sort_arrow}}
```

### A pinned overlay freezes on its first frame

An overlay positioned with `pinAt` (such as a drag ghost following the cursor) keeps the same layout rectangle even as it moves, so an unversioned drawing stays stuck on its first frame's draw ops.

**Fix:** wrap it in `drawingVersioned` keyed by the coordinates or state that move it:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/CustomDrawing.hs:versioned}}
```

### Scroll viewport width belongs in the key too

If a custom drawing clamps rows to `rectW (scrollViewport metrics)`, include that width in its content key so rows re-render properly when a scrollbar appears or disappears (as `ned` does in its finder).

## 5. Scroll details

### Rows draw underneath the scrollbar

The scrollbar reserves a vertical lane out of the scroll viewport. If you paint custom rows across the widget's full width, content and click targets extend behind the scrollbar. Clamp row content and hit targets to `scrollViewport`, while letting background highlights span the full width so no vertical gap appears along the scrollbar edge.

### Mouse-wheel scrolling feels misaligned with row height

The default wheel notch scrolls 60 logical pixels, which can feel awkward over 54 px rows or dense lists. Match the step to your row height:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Layout.hs:wheel_step}}
```

You can also override one scroller at a time with `setScrollStepUi`; pass `0` to revert to the context default.

## 6. Focus and input ordering

### Focus checks at the top of the view always return `False`

The layout arena clears before your view runs, so calling `isFocused` at the top of `appView` always reports `False`.

**Symptom:** pressing Ctrl+A selects all rows even while typing in a search input, or pressing Delete triggers an item-delete dialog instead of deleting a character.

**Fix:** run focus queries and focus-dependent shortcuts at the **end** of your view, after all widgets have been declared:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Troubleshooting.hs:focus_at_end}}
```

### Escape shares the same guard as Ctrl+A or Delete

Built-in text inputs intentionally leave Escape unclaimed. If you disable Escape whenever "a field is focused" (using the same guard as Ctrl+A or Delete), Escape stops clearing selections or closing panels while typing.

**Fix:** handle Escape with its own condition instead of tying it to text-field focus.

### Typed characters never reach a custom widget

Backends only enable OS text input on frames where `useInputMethod` is called. Even if a custom widget is not a traditional text box (like `ned`'s file tree accepting `j`/`k` vim keys as characters), it must call `useInputMethod` on every focused frame to receive `inputChars`.

**Fix:**

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:input_method}}
```

### Dragging slowly leftward only selects one character

If you re-derive a text drag's anchor from the current selection on every frame, a one-character selection anchors at its left edge and drags that anchor along with the cursor as you move left.

**Fix:** store the original anchor from the initial mouse press and hold it constant for the rest of the drag.

### Focusing a widget that has not appeared yet fails

`requestFocus` only resolves widgets declared during the current pass. When clicking a button reveals a new text input on the *next* pass, store a `focusPending` flag and call `requestFocus` on the frame that actually declares the input. See [Keyboard, Focus & IME](../input/index.md).

### Conditional widgets scramble sibling state

Any branch that runs different widget calls than its alternative shifts sibling IDs for everything after it, making unrelated fields swap carets or focus. Wrap conditional subtrees in `scope` and call overlays unconditionally. See [Widget Identity & Keys](../state/identity.md).

## 7. SVG export

- Exporting several canvas drawings onto one HTML page can create colliding `clip_` and `grad_` IDs unless each SVG uses a unique prefix (handled automatically by `frameToSvg`'s document hash, or by passing `svgIdPrefix` / prefixing each `<g><defs>` block with an index as `stronghead` does).
- Right-aligned numerals can lose their anchor depending on browser font rendering: `text-anchor` and `textLength` may need post-processing, and `font-variant-numeric: tabular-nums` at the document root keeps numeric columns aligned.

## Quick debugging checklist

- [ ] `liftIO (newVar ...)` inside the view allocates a fresh `Var` every pass; move it to setup.
- [ ] Unguarded side effects run every frame; wrap them in `whenM (button ...)` or an explicit event check.
- [ ] Conditional widgets or `useVar` calls need `scope`; dynamic lists need `withKey` with stable IDs (never indices).
- [ ] Call `modal`, `window`, and `popup` unconditionally every frame with a `Var Bool`.
- [ ] Putting `useTask` inside a tab or conditional cancels the worker as soon as it is hidden.
- [ ] Calling `keepAnimating` unconditionally at the top level creates a busy loop; call it inside the animating widget, and use `wakeAfter` for timers.
- [ ] Writing a computed `NaN` to a `Var Float` redraws endlessly (`NaN /= NaN`); guard float math.
- [ ] Missing an input from a custom widget's content key produces stale or frozen pixels.
- [ ] Changing labels without `fixedW` shift their siblings; wide list cells without `fillW` trigger horizontal scrolling.
- [ ] Nesting `scrollArea`, `panel`, or `card` inside a popup creates double borders; let one layer own the chrome.
- [ ] Check `isFocused` and focus-sensitive shortcuts at the end of the view, not the top.
- [ ] Give Escape its own guard instead of sharing the text-editing guard used for Ctrl+A or Delete.
- [ ] Custom widgets reading `inputChars` must call `useInputMethod` on every focused frame.
- [ ] Measure long text runs with `lineWidthUi` instead of multiplying single-glyph advance by length.
- [ ] Parse documents and compute expensive data outside the view, updating only when inputs change.
- [ ] Place `readVar` below the widget that edits it to avoid an extra pass per change.
- [ ] Compile executables with `-threaded`.
- [ ] Never touch `Context` from a worker thread; use `writeVarIO` and `askWake` instead.
