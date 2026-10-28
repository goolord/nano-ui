# Scrolling & Virtualization

`nano-ui` provides high-performance scroll containers supporting momentum scrolling, custom scrollbars, and virtualization for lists containing hundreds of thousands of items.

## Bounded Scroll Viewports

A scroll container must have a bounded dimension along its scrolling axis:

```haskell
-- Bounded vertically to 300px; fills parent horizontally
scrollWith (fixedH 300 . fillW) $ do
  column $ for_ items $ \item -> do
    label item
```

> [!WARNING]
> Giving a scroll container `grow` without a bounded parent can lead to unexpected collapses. Ensure the ancestor container provides a definite constraint.

---

## 2D Scrolling

For data grids, code canvases, or image viewports:

```haskell
scroll2DWith (fixedWH 600 400) $ do
  largeCanvasContent
```

---

## Virtualization for Massive Datasets

When rendering datasets with 10,000+ to 100,000+ rows, creating thousands of layout nodes degrades frame rates. Virtualization solves this by rendering only the rows currently visible within the scroll viewport.

### Implementation Pattern

```haskell
virtualizedList :: [Item] -> NanoUI ()
virtualizedList items = do
  -- Capture the widget ID of the scroller
  sid <- currentId
  
  scrollWith (fixedH 500 . fillW) $ do
    -- Query scroll metrics (current offset and viewport height)
    metrics <- getScrollMetricsUi sid
    
    let rowHeight = 28.0
        totalCount = length items
        totalHeight = fromIntegral totalCount * rowHeight
        
        -- Compute visible row slice
        firstVisible = max 0 (floor (smOffsetY metrics / rowHeight))
        visibleCount = ceiling (smViewportHeight metrics / rowHeight) + 2
        lastVisible = min totalCount (firstVisible + visibleCount)
        
        topSpacerHeight = fromIntegral firstVisible * rowHeight
        bottomSpacerHeight = fromIntegral (totalCount - lastVisible) * rowHeight
        
        visibleRows = take (lastVisible - firstVisible) (drop firstVisible items)
    
    column $ do
      -- Top spacer maintains scroll height
      when (topSpacerHeight > 0) $ spacerWith (fixedH topSpacerHeight)
      
      -- Render only active rows
      for_ visibleRows $ \rowItem -> do
        withKey rowItem.id $ do
          labelWith (fixedH rowHeight . fillW) rowItem.title
          
      -- Bottom spacer maintains scroll height
      when (bottomSpacerHeight > 0) $ spacerWith (fixedH bottomSpacerHeight)
```

See `nano-ui-example-long-list` in `packages/nano-ui-demo` for a complete working demo of 100,000 rows virtualized at 60 FPS.
