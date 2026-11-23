# Drag & Drop

Drag and drop in `nano-ui` falls into two categories: pointer drags inside the window (reordering rows, dragging tabs, docking panes) and external files or text dragged in from other apps. In every case, the hooks only report drag state and drop locations; updating your application data is up to you.

## Reordering a list

Pass the current list of item IDs and each item's drawn `Response` (in render order) to `useReorder`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/DragDrop.hs:reorder}}
```

`reorderOrder` returns the final order on the frame the user drops an item. Render from `reorderPreview` instead if you want surrounding rows to shift live during the drag. `reorderDragging` identifies the item currently held, and `reorderMoved` turns `True` once the cursor passes the drag threshold so you can suppress normal click handlers.

## General gestures with `useDrag`

For custom drag interactions (like dragging tabs or moving items between containers), use `useDrag`. Allocate a drag handle once in setup with `newDrag`. In the example below, `insertionIndex` converts the pointer coordinates into a target slot along a tab bar (returning `Nothing` when outside):

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/DragDrop.hs:tab_drag}}
```

`dragPhase` transitions through `DragStarted`, `Dragging`, and either `DragReleased` or `DragCancelled` (triggered by Escape, losing capture, or the dragged source disappearing). Both terminal phases last a single frame; commit changes only on `DragReleased`.

## Files and text dropped from other apps

The SDL backend forwards OS drag-and-drop events (the RGFW backend does not). `dropZone` draws a panel that tracks hover state and reports dropped files or text:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/DragDrop.hs:drop_zone}}
```

`dropHovered` is `True` while an external drag hovers over the zone. If you want drop detection on a custom region without drawing a panel, call `useDrop rect`. Drop events respect layering, so a drop zone covered by an open modal receives nothing.

## Dockable pane grids

`paneGrid` manages splittable, resizable pane layouts. Its response includes `pgrDropTarget`, which reports where a dragged pane or tab would dock under the cursor and pairs naturally with `useDrag`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/DragDrop.hs:pane_grid}}
```

`pgdLocation` is either `BesidePane paneId edge` or `OutsideGrid edge`, and `pgdRect` / `pgdPane` let you validate or highlight the target. Save `pgrTree` whenever `pgrCommitted` is `True`.
