# Local drag and drop

`useDrag` tracks a gesture over `(payload, Response)` pairs. Applications
own the data and choose which target accepts a release. Allocate
`dragHandle <- newDrag` once during component setup, then use it in the view:

```haskell
headers <- tabBar' active documents
gesture <- useDrag dragHandle (tabHeaders headers)
```

Call with the same handle every frame, including for empty sources. Payloads
must be unique and have an `Eq` instance. Capturing the payload
on press preserves identity through reordering; removing its source cancels.

Below the threshold, the result is `Nothing`. Otherwise it is a `Drag` whose
`dragAt` and `dragFrom` are window coordinates, with one of these phases:

* `DragStarted`: first frame beyond the threshold.
* `Dragging`: subsequent held frames.
* `DragReleased`: release frame; a target may commit.
* `DragCancelled`: Escape, lost button hold, or removed source; do not commit.

Terminal phases are transient. Consume them in the view that calls the hook,
and do not retain an old gesture as though it were current input. As with
other interaction responses, a mirror rebuild may strip the interaction.
Ignore ordinary pointer-click selection while handling a drag response.

## Reorder or transfer between strips

`tabHeaders` supplies ordered header responses. `tabStripRect` supplies the
visible viewport, excluding horizontal paging buttons and trailing actions.
Use `DragAxisX` for top/bottom tabs and `DragAxisY` for left/right tabs.

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

`moveTab` removes the item, then inserts it at `i` in the remaining list.
Use the same slot for an insertion indicator. `insertionIndex` returns
`Nothing` outside the viewport, zero for empty targets, and preserves
offscreen item indices. Tab viewports respect ancestor clips.

For cross-group moves, collect every strip's response before resolving
targets. Exclude the dragged item only from its source strip. Carry the
source group in the payload when tab keys are not globally unique. This
also avoids making target priority depend on pane rendering order.

## Create a pane

The grid reports `pgrDropTarget` at the current pointer, regardless of whether
anything is being dragged. The destination exposes:

* `pgdLocation`: `BesidePane paneId edge` or `OutsideGrid edge`.
* `pgdRect`: preview rectangle in window coordinates, laid out only when read.
* `pgdPane`: the pane under the pointer, for application-specific acceptance.

Edges are `PaneLeft`, `PaneRight`, `PaneAbove`, and `PaneBelow`. Querying a
destination does not change the tree, render speculative pane content, or
draw a ghost. The application can draw its own highlight using `pgdRect`.

For sources inside `pgViewPane`, collect their responses using a per-build
accumulator or existing application effects, then call `useDrag` after
`paneGrid` returns. Include the pane id when tab keys are only locally unique.

```haskell
grid <- paneGrid cfg
sources <- collectedHeaderResponses
gesture <- useDrag dragHandle sources

case (gesture, pgrDropTarget grid) of
  (Just d, Just target)
    | dragPhase d == DragReleased
    , isEditorPane (pgdPane target)
    , not stripAccepted -> do
        committed <- commitPaneDrop target
        forM_ committed $ \(paneId, tree) -> do
          moveDocumentInto (dragPayload d) paneId
          saveArrangement tree
  _ -> pure ()
```

Here `stripAccepted` comes from resolving strip targets first; the other
application functions move documents and save the committed arrangement.

Pane centers, gutters, covered or clipped areas, outside positions, maximized
grids, and internal resize/move gestures produce no insertion destination.
Application acceptance and gesture cancellation are decided by the caller.

`commitPaneDrop` explicitly creates and focuses a pane, returning its id and
the new tree. It does not inspect the mouse or require a drag; the caller owns
the decision to commit. Fill the pane's content and, for a controlled grid,
save the **returned tree**, not the earlier `pgrTree`. That response is a
pre-commit snapshot; `pgrCommitted` describes changes during `paneGrid`, not
this later operation. A successful commit is itself the save notification.

The operation requests a new frame without forcing an immediate view rebuild.
It returns `Nothing` if the grid snapshot changed or that destination was
already committed. Use targets in the context that produced them and resolve
them in the same view build.

`useReorder` retains its existing nearest-item behavior for wrapped lists.
OS file/text drops continue to use `useDrop` and `dropZone`.
