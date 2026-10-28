# The Frame Model

`nano-ui` is built on an immediate-mode execution model. Unlike retained-mode frameworks where you build an object graph of widgets and mutate them via event listeners, in `nano-ui` the view action re-runs every frame to describe the entire interface.

## The Frame Lifecycle

Each frame proceeds through six deterministic stages:

![Rendering Pipeline](../images/rendering-pipeline.svg)

1. **View Execution (`NanoUI ()`)**: The user action executes top-to-bottom. It emits layout nodes into an unboxed arena, reads input events, queries layout geometry from the previous frame, and reads/writes mutable `Var` references.
2. **Re-run Check (Self-Invalidation)**: If a `Var` is written *after* it was already read in the same pass, the view action is re-run once with one-shot input events cleared (preventing buttons or keystrokes from firing twice).
3. **Layout Solver**: The layout tree is solved from root to leaves, determining logical rectangles for every node based on flex weights, gaps, and explicit constraints.
4. **Hit-Testing & Focus Resolution**: Pointer coordinates, keyboard shortcuts, and Tab cycles are resolved against the freshly solved geometry.
5. **Damage Calculation**: The new frame geometry and draw operations are diffed against the previous frame to calculate the minimal damaged bounding box.
6. **Rasterization / Paint**: Only the damaged region is uploaded to the GPU / backend renderer for presentation.

---

## Architectural Consequences

### 1. View Actions Can Run More Than Once Per Frame
Writing a `Var` that was read earlier in the same pass triggers a second pass to reconcile the UI.
> [!CAUTION]
> Never allocate per-frame resources inside a view. For instance, `liftIO (newVar x)` inside a view is a critical bug—it allocates a new reference on every pass. Guard all side effects behind event checks:
> ```haskell
> whenM (button "Save") (liftIO saveDocument)
> ```

### 2. `readVar` Placement Matters
The position of `readVar` relative to mutating widgets impacts performance:
- **Single Pass**: Reading a `Var` *below* the input widget that edits it allows the mutation and new value to resolve in a single pass.
- **Double Pass**: Reading a `Var` *above* the input widget forces a second pass whenever the user types or interacts.
- **Widget-Level Repaint**: Widgets bound directly to a `Var` without any external `readVar` calls only repaint that specific widget.

### 3. Geometry Lags One Frame
Because layout is calculated *after* the view action executes, `respRect` returns the geometry from the **previous frame** (empty on first appearance). When writing automated tests or positioning dynamic popups based on rects, warm up one frame before checking coordinates.

### 4. Idle Costs Zero CPU
When no animations, background tasks, or OS window events are pending, the backend event loop blocks on the OS event queue. The application consumes 0% CPU while idle.
