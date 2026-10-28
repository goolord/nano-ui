# Drag and Drop

`nano-ui` includes local drag-and-drop primitives via `useDrag`, `useDrop`, `dropZone`, and `useReorder`. Applications own the data and choose which target accepts a release gesture.

## Drag Lifecycle with `useDrag`

Allocate a `dragHandle <- newDrag` once during component setup, then use it in the view:

```haskell
headers <- tabBar' active documents
gesture <- useDrag dragHandle (tabHeaders headers)
```

`useDrag` tracks gesture coordinates and reports one of four phases:
- **`DragStarted`**: Pointer exceeded the drag distance threshold.
- **`Dragging`**: Pointer is actively moving while button is held.
- **`DragReleased`**: User released the mouse button over a target.
- **`DragCancelled`**: Gesture cancelled via Escape key or lost focus.

---

## Reordering Tab Strips

```haskell
case gesture of
  Just d | dragPhase d /= DragCancelled -> do
    let remaining = filter ((/= dragPayload d) . fst) (tabHeaders headers)
        slot = insertionIndex DragAxisX (tabStripRect headers)
                 (map (respRect . snd) remaining) (dragAt d)
    case slot of
      Just i | dragPhase d == DragReleased -> moveTab (dragPayload d) i
      _ -> pure ()
  _ -> pure ()
```

`insertionIndex` returns `Nothing` outside the strip bounds and calculates the target insertion index respecting ancestor clips.

---

## Pane Grids (`paneGrid`)

Dockable, splittable multi-pane arrangements (as seen in IDEs and profiling debuggers):

```haskell
grid <- paneGrid cfg
case (gesture, pgrDropTarget grid) of
  (Just d, Just target)
    | dragPhase d == DragReleased -> do
        commitPaneDrop target
  _ -> pure ()
```
The destination provides `pgdLocation` (`BesidePane paneId edge` or `OutsideGrid edge`), `pgdRect`, and `pgdPane` for custom drop validation.

## See also

- Widget inventory: [Widget Gallery](index.md).
- Tabs and tables that drag reordering is usually paired with: [Tabs, Tables & Trees](data-views.md).
- Styling drag targets: [Styling & Theming](../styling/index.md).
