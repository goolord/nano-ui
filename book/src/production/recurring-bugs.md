# Recurring Bugs & Fixes

Mined from the git histories and production deployments of real-world `nano-ui` applications (`ned`, `stronghead`, and `winget-gui`). Check this reference whenever diagnosing unexpected UI behavior.

---

## 1. Layout Shifting & Jitter

### A label whose text changes length moves its siblings every frame
- **Symptom**: A counter going `9 -> 10 -> 100`, a live stopwatch, or a byte counter causes adjacent buttons to shift left and right on every tick.
- **Fix**: Pin changing labels to a fixed width sized to their widest expected value:
  ```haskell
  labelWith (fixedW 48 . alignEnd) (fmtInt count)
  ```

### Long values inside a scrolling list widen the whole list
- **Symptom**: A wide row causes horizontal scrollbars to flash into existence, scrolling the entire container sideways.
- **Fix**: Make cells fill their column rather than taking natural width, and enable text clipping:
  ```haskell
  nameLine f = rowWith (fillW . tight) . labelWith (fillW . f)
  ```

### Dropdowns clipping their longest option
- **Symptom**: Fixed-width select dropdowns truncate items like `"Drive C: \ 412 GB free"`.
- **Fix**: Measure option text lengths using font metrics:
  ```haskell
  let w = maximum (0 : map (lineWidth fm) options) + 56
  selectWith (fixedW w) options selVar
  ```

---

## 2. Double Borders & Box-in-a-Box Chrome

- **Symptom**: Nesting `panel`, `card`, or `scrollArea` inside modals, popovers, or inspector panes creates redundant double borders and nested boxes.
- **Fix**: Only one container level should own surface chrome:
  - Give outer dialogs `padAll 0` and pad inner containers, OR
  - Strip chrome from the inner container:
    ```haskell
    styled (inputStyle (background colorTransparent . borderWidth 0)) ...
    ```
  - Between adjacent sections, draw a single `separator` rather than opposing borders.

---

## 3. Text Metrics & Pixel Grid

### Glyphs drifting over long text runs
- **Symptom**: `fmAdvance` multiplied by character count drifts ~1px every 30 characters compared to actual rendered text.
- **Fix**: Always measure runs using `lineWidth fm run` (which accounts for HarfBuzz kerning and unrounded fractional advances).

### Text shimmering at fractional DPI scales (125%, 150%)
- **Symptom**: Rows of 22.4px alternate 22px and 23px apart, producing shimmering during scrolling.
- **Fix**: Snap coordinates to the device pixel grid using `fmSnapScale`:
  ```haskell
  snap v = let k = max 1 (fmSnapScale f) in fromIntegral (round (v * k) :: Int) / k
  ```

---

## 4. Stale Cached Drawings

- **Symptom**: A custom-drawn widget (e.g. table sort arrow or audio visualizer) freezes or shows outdated information.
- **Fix**: The `widgetContent` key is missing an input! The key must cover **every variable, color, viewport dimension, and animation channel** the drawing reads.

---

## 5. Input Ordering & Focus

### Asking focus questions at the top of the view
- **Symptom**: Global shortcuts (`Ctrl+A`, `Delete`) fire while the user is actively typing inside a text field.
- **Fix**: The node arena is reset before the view action runs. Calling `isFocused` or checking text editing state at the *top* of the view always returns `False`. Place keyboard shortcuts and focus queries at the **end of the view**, after widgets have executed.

### Escape claimed by text inputs
- **Symptom**: Pressing `Escape` inside a search box fails to clear a modal or dismiss a dropdown.
- **Fix**: Text fields do not claim `Escape`. Give `Escape` handlers their own distinct conditional branch separate from `inputChars` suppression.

---

## Quick Pitfalls Checklist

- [ ] `liftIO (newVar ...)` inside a view — allocates every pass. Move to setup.
- [ ] Unguarded side effects — view re-runs. Wrap in `whenM (button ...)`.
- [ ] Conditional widgets or `useVar` without `scope` — shifts sibling IDs.
- [ ] Overlays called conditionally instead of unconditionally with `Var Bool`.
- [ ] `useTask` inside a hidden tab — task is cancelled when skipped.
- [ ] Writing freshly computed `NaN` to a `Var Float` — causes infinite redraw loop (`NaN /= NaN`).
- [ ] Missing `-threaded` in executable cabal options.
- [ ] Touching `Context` from a background thread — call `askWake` instead.
