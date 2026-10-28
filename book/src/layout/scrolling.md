# Scrolling & Virtualization

Scroll containers handle wheel and drag scrolling, a scrollbar, and (for huge lists) virtualization. The one requirement is a **bounded viewport** along the scrolling axis.

## Bounded scroll viewports

```haskell
-- Bounded vertically to 300 px; fills the parent horizontally.
scrollWith (fixedH 300 . fillW) $
  column $ for_ items $ \item ->
    label item
```

> [!WARNING]
> A scroll container given `grow` inside an unbounded parent has nothing to size against and can collapse. Make sure an ancestor provides a definite height (or width, for horizontal scrolling).

`scrollWith` returns the body's result. `scrollArea` also returns the scroller's `WidgetId`, which is what programmatic scrolling needs:

```haskell
(sid, picked) <- scrollArea (fixedH 240 . fillW) $ do
  ...
```

## 2D scrolling

`scroll2DWith` and `scrollArea2D` scroll both axes. The log viewer and data grids use this:

```haskell
scroll2DWith (fixedWH 600 400) largeCanvasContent
```

Use 2D scrolling only when the content genuinely needs both axes; a one-axis container has a simpler scrollbar and wheel behavior.

## Virtualization for massive lists

A list of 100,000 rows cannot create 100,000 layout nodes per frame. Virtualization draws only the rows inside the viewport, with spacers standing in for the rows before and after.

The scroller's geometry is read from the **previous** frame's layout, so capture the id and read the metrics before building the content:

```haskell
virtualizedList :: [Item] -> NanoUI ()
virtualizedList items = do
  sid <- currentId                       -- the id the scroller will receive
  scrollWith (fixedH 500 . fillW) $ do
    metrics <- getScrollMetricsUi sid    -- previous frame's geometry, or Nothing
    let rowH = 28
        total = length items
        (offsetY, viewH) = case metrics of
          Nothing -> (0, 0)
          Just m  -> (v2Y (scrollOffset m), rectH (scrollViewport m))
        firstVisible = max 0 (floor (offsetY / rowH))
        visibleCount = ceiling (viewH / rowH) + 2
        lastVisible  = min total (firstVisible + visibleCount)
        topH    = fromIntegral firstVisible * rowH
        bottomH = fromIntegral (total - lastVisible) * rowH
        visibleRows = take (lastVisible - firstVisible) (drop firstVisible items)
    columnWith (tight . fillW) $ do
      when (topH > 0) (spacer Fit (Fixed topH))
      for_ visibleRows $ \item ->
        withKey (itemId item) $
          labelWith (fixedH rowH . fillW) (itemTitle item)
      when (bottomH > 0) (spacer Fit (Fixed bottomH))
```

Why each piece matters:

- **`currentId` before the scroller** names the widget that `getScrollMetricsUi` reads. The metrics are from the previous frame, like `respRect`.
- **`scrollViewport` includes padding and excludes the scrollbar lane.** Use it, not the container's rectangle, for the visible height.
- **`scrollOffset` is the current offset**, which may have moved since layout (a wheel event or glide).
- **The top and bottom spacers keep the scroll range correct.** Without them, the scrollbar would shrink to just the visible rows.
- **`withKey (itemId item)`** keeps row state attached to the item, not the visible position.
- **A metrics read registers a layout dependency**: when the viewport changes size, the view gets another frame with the new metrics.

`packages/nano-ui-demo/examples/LongList.hs` runs this pattern with 100,000 rows; `Logs.hs` adds sticky-to-bottom behavior on top.

## Programmatic scrolling

The scroller id unlocks commands:

```haskell
metrics <- getScrollMetricsUi sid
when (shouldReveal row) $
  scrollRectIntoViewUi sid (Rect 0 (row * rowH) 1 rowH) ScrollNearest ScrollInstant
```

Available from a view: `scrollToUi`, `scrollByUi`, `scrollPagesUi`, `scrollRectIntoViewUi`, `setScrollOffsetUi`, and `setScrollStepUi`. The `Context`-based variants (`scrollTo`, `scrollBy`, `getScrollOffset2D`, ...) are used from `IO` when a view is not the caller.

`scrollRectIntoViewUi` respects ancestor clips and is what focus uses to reveal a Tab target. `ScrollAlign` chooses nearest/start/end/center; `ScrollInstant` versus `ScrollSmooth` chooses the motion.

## Wheel behavior

By default a wheel notch scrolls 60 logical pixels. For a list of fixed-height rows, set the step so one notch is exactly one row:

```haskell
ctx <- askContext
liftIO $ setScrollTuning ctx defaultScrollTuning
  {scrollWheelStep = rowHeight, scrollSmoothTime = 0}
```

`setScrollStepUi sid px` overrides the step for one scroller; `0` returns it to the context setting. A nonzero `scrollSmoothTime` turns each notch into a short glide.

## Performance notes

- Build only the visible rows. A `label` per row of 100,000 is still 100,000 layout nodes even if the drawing is clipped.
- Keep row heights constant when virtualizing; content-derived heights make the visible slice computation wrong.
- Avoid `readVar` above the scroller for values that change while scrolling; a read above the editor forces a second pass per change.
- Use `scroll2D` only when needed; the extra axis costs a scrollbar lane and hit-testing.
- A `scrollArea` nested in a popup or dialog draws a box in a box; see [Double borders](../troubleshooting/recurring-bugs.md#2-double-borders--box-in-a-box).

## Related

- Container inventory: [Containers](containers.md).
- Sizing modifiers: [Layout Modifiers](modifiers.md).
- The 100,000-row example and the log viewer: `packages/nano-ui-demo/examples/`.
