# Drag & Drop

The application owns the data. The hooks report gestures; your code decides what a drop does.

## Reordering a list

`useReorder` takes the current order of item ids and each item's response, in drawn order:

```haskell
order <- readVar orderVar            -- orderVar <- newVar [0 .. 4] in setup
drawn <- forM order $ \i -> (i,) <$> chip i
r <- useReorder order drawn
writeVar orderVar (reorderOrder r)
```

`reorderOrder` changes on the frame the item is dropped. Draw from `reorderPreview` instead to show the other items making room during the drag. `reorderDragging` is the held item, and `reorderMoved` tells a drag from a click.

## Gestures with `useDrag`

For anything else, `useDrag` tracks a thresholded drag across keyed sources. Allocate the handle once with `newDrag` in setup:

```haskell
headers <- tabBar' active documents
gesture <- useDrag dragHandle (tabHeaders headers)
```

`dragPhase` is `DragStarted`, `Dragging`, `DragReleased`, or `DragCancelled` (Escape, a lost hold, or the source disappearing). The last two last one frame. Commit only on `DragReleased`.

`insertionIndex` turns the pointer into a slot in a row or column, or `Nothing` outside its bounds:

```haskell
for_ gesture $ \d -> when (dragPhase d == DragReleased) $ do
  let rest = filter ((/= dragPayload d) . fst) (tabHeaders headers)
  for_ (insertionIndex DragAxisX (tabStripRect headers) (map (respRect . snd) rest) (dragAt d)) $
    moveTab (dragPayload d)
```

## Files and text from the OS

`dropZone` is a panel that reports what the OS dropped on it:

```haskell
(_, _, target) <- dropZone (fillW . padAll 12) (label "Drop files here")
when (dropReceived target) (mapM_ openFile (dropFiles target))
```

`dropHovered` is true while a drag is over the zone. `useDrop rect` does the same for any rectangle. Only the topmost layer sees the drop, so a zone behind a modal gets nothing.

## Pane grids

`paneGrid` lays out dockable, splittable panes. Its response's `pgrDropTarget` is the destination under the pointer, which pairs with a `useDrag` gesture:

```haskell
grid <- paneGrid cfg
case (gesture, pgrDropTarget grid) of
  (Just d, Just target) | dragPhase d == DragReleased -> commitPaneDrop target
  _ -> pure ()
```

`pgdLocation` is `BesidePane paneId edge` or `OutsideGrid edge`; `pgdRect` and `pgdPane` help validate the drop. Save `pgrTree` when `pgrCommitted` is set.
