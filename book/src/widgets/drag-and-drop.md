# Drag & Drop

There are two kinds of drag: a pointer drag inside the window (reordering a list, moving tabs, docking panes), and files or text dropped in from another application. The hooks only report what happened. Applying a drop to your data is up to you.

## Reordering a list

`useReorder` takes the current order of item ids and each item's response, in drawn order:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/DragDrop.hs:reorder}}
```

`reorderOrder` changes on the frame the item is dropped. Draw from `reorderPreview` instead to move the other items aside during the drag. `reorderDragging` is the held item, and `reorderMoved` is set once the press has moved past the drag threshold, so a click can be ignored.

## Gestures with `useDrag`

For anything else, `useDrag` tracks a thresholded drag across keyed sources. Allocate the handle once with `newDrag` in setup. Here it reorders tabs, with `insertionIndex` turning the pointer into a slot in the strip (`Nothing` outside it):

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/DragDrop.hs:tab_drag}}
```

`dragPhase` is `DragStarted`, `Dragging`, `DragReleased`, or `DragCancelled` (Escape, a lost hold, or the source disappearing). The last two last one frame. Commit only on `DragReleased`.

## Files and text from other applications

The SDL backend delivers the operating system's drag and drop; the RGFW backend does not. `dropZone` is a panel that reports the files and text dropped on it:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/DragDrop.hs:drop_zone}}
```

`dropHovered` is true while a drag is over the zone. `useDrop rect` does the same for any rectangle. Only the topmost layer sees the drop, so a zone behind a modal gets nothing.

## Pane grids

`paneGrid` lays out dockable, splittable panes. Its response's `pgrDropTarget` is the destination under the pointer, which pairs with a `useDrag` gesture:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/DragDrop.hs:pane_grid}}
```

`pgdLocation` is `BesidePane paneId edge` or `OutsideGrid edge`; `pgdRect` and `pgdPane` help validate the drop. Save `pgrTree` when `pgrCommitted` is set.
