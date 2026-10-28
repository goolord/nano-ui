# The Frame Model

`nano-ui` is an immediate-mode toolkit. There is no retained widget tree to look up and mutate: the view action runs every frame and describes the entire interface. This chapter is the machinery behind the rules introduced in [Introduction to State](../state/introduction.md). You do not need it to write a view, but it explains why the rules exist.

## The frame lifecycle

Each frame proceeds through six deterministic stages:

![Rendering Pipeline](../images/rendering-pipeline.svg)

1. **View execution.** The `NanoUI ()` action runs top-to-bottom. It appends layout nodes to an arena, reads input events, reads the previous frame's geometry, and reads or writes `Var`s.
2. **Re-run check.** If a `Var` was written *after* it was already read this pass, the action is re-run once with one-shot input events cleared, so a button click or keystroke cannot fire twice.
3. **Layout solve.** The layout tree is solved root-to-leaf: flex weights, gaps, padding, explicit constraints, and the container's available space determine every rectangle.
4. **Hit testing and focus.** Pointer coordinates, keyboard shortcuts, hover, press, and Tab order are resolved against the freshly solved geometry.
5. **Damage calculation.** The new geometry and draw operations are diffed against the previous frame to find the smallest changed region.
6. **Paint.** Only the damaged region is rasterized and presented. The rest of the window is reused.

Two details are easy to miss:

- **The view runs before layout.** Widgets can read the *previous* frame's rectangle (`respRect`), but not the rectangle they are about to be given.
- **Focus is resolved after the view.** A focus request made in the view takes effect at the end of the frame, so the next frame observes it.

## Consequences

### The view can run more than once per frame

A write to a `Var` that was read earlier in the pass triggers a second pass. The second pass runs with one-shot input events cleared: the click or keystroke that caused it does not fire again. This is why:

- side effects belong behind `whenM (button ...)`, never after a bare widget call;
- allocating a resource with `liftIO (newVar ...)` in the view is a bug — the second pass allocates again;
- `modifyVar` is preferred over read-then-write, so multiple changes in one frame compose.

### `readVar` placement is a performance decision

- A read **below** the widget that edits the value sees the new value in the same pass. One pass.
- A read **above** the widget sees the old value, then the write invalidates the pass. Two passes.
- A widget bound to a `Var` with **no `readVar` anywhere** repaints only itself on change. A `readVar` promotes the change to a full-window repaint.

The [State](../state/introduction.md) chapter turns this into a rule of thumb: keep reads below writers when order is free.

### Geometry lags one frame

`respRect` returns the rectangle from the previous frame, and is empty on a widget's first appearance. Automated tests must run a warm-up frame before targeting a widget by rectangle; dynamic overlays anchored to a rectangle see the old one for a frame. `requestFocus` is the exception that can name a widget declared later in the same pass, because it is resolved after layout.

### Idle costs zero

When no animation, timed wake, task, or stream is pending, the event loop blocks on the OS event queue. A static window consumes no CPU. This is why the toolkit is strict about unnecessary animation and redraw:

- call `keepAnimating` inside the widget that is actually animating;
- call `wakeAfter` on every frame that still needs a future wake;
- do not mark the context dirty every frame to force a redraw — that is an unpaced busy loop.

### Widget ids are positional

Ids are assigned in call order among siblings. Conditional widgets, reordered lists, and overlays called conditionally all change the count and therefore the ids of later widgets. `scope` and `withKey` are the tools that keep the counts stable; [Widget Identity & Keys](../state/identity.md) covers them.

## Where the practical rules live

| Question | Chapter |
| --- | --- |
| Where should this value live? | [Introduction to State](../state/introduction.md) |
| What does a write cost? | [`Var` in Depth](../state/var.md) |
| Why did state jump to another row? | [Widget Identity & Keys](../state/identity.md) |
| Why is focus wrong at the top of the view? | [Focus, Navigation & IME](../input/focus.md) |
| Why does a custom drawing show stale pixels? | [Recurring Bugs & Fixes](../troubleshooting/recurring-bugs.md) |
| How do application state and the frame fit together? | [Application Patterns](patterns.md) |
