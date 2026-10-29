# Async Work & Animation

The view must never block, and nothing should redraw while nothing moves. Slow work runs on a worker, timed work on a wake, and the frame loop sleeps in between. Compile with `-threaded`.

## `Task`: one job per key

Allocate the handles once in `IO`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:handles}}
```

In the view:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:task_status}}
```

When only the latest result matters, use `useTask`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:use_task}}
```

- The key identifies the job. A new key starts a worker and cancels the old job with an asynchronous exception.
- A skipped hook cancels the job. If the view does not call `useTask` for a full frame, because the pane is in an inactive tab or inside a branch, the work is released. Keep the hook outside tabs when work must continue while hidden.
- To retrigger the same input, put a counter in the key. A Retry button must change the key or the job never reruns.
- Cancellation is an async exception. Use `bracket` or `withFile` inside the job so resources are released when the user changes the query.
- The result is forced on the worker. Return a value in weak head normal form; force a list's spine with `length found \`seq\` pure found`.
- `TaskRunning previous` carries the last result, so a list does not flash empty while the next query runs.

## `Stream`: repeated pushes

A `Stream` is for sensors, downloads, token streams, and anything else that pushes repeatedly:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:stream}}
```

The producer receives `update :: (s -> s) -> IO ()` and returns when done, cancelled, or throwing. Each update wakes the loop; updates between frames coalesce into one frame.

- Stop by not calling the hook. `when isRunning (useStream ...)` releases the producer.
- Stopping discards the accumulated state. Save it outside and pass it as the next initial value to resume.
- Producers must not touch the UI. They may call `update` from their thread or threads they start; never touch the UI `Context`.

## Threads outside those handles

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:wake}}
```

`readVarIO`, `writeVarIO`, and `modifyVarIO` are the thread-safe `Var` operations. The wake repaints the whole window, because it carries no record of which widget changed. Use a `Stream` for frequent updates.

## Animation

`animateTo` moves a value toward a target:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:animate_to}}
```

A `Transition` is either:

- `Tween ease duration delay`, in seconds. Curves include `EaseOutQuad`, `EaseInOutCubic`, `EaseOutBack`, and `EaseCubicBezier`.
- `Spring params`, a damped spring. `presetSmooth`, `presetBouncy`, and `presetStiff` are the presets.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:spring}}
```

Springs retarget mid-flight from their current position and velocity. `animateToA` animates any `Animatable` value (`Float`, `Double`, `V2`, or `Color`) channel by channel.

### Continuous animation

`pulse` returns a value oscillating in `[0, 1]` with the given period in seconds:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:pulse}}
```

Clock-driven values do not, by themselves, keep the loop awake. A widget that draws from them must call `keepAnimating` **with its own response**, every frame it is on screen:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:keep_animating}}
```

The loop goes back to sleep on the first frame that leaves it out.

> [!CAUTION]
> Never force redraws by dirtying the context every frame. That is an unpaced busy loop.

### Timers

`wakeAfter` requests one frame after a delay in **seconds**:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:wake_after}}
```

Call it on every frame that still needs the wake; pending wakes reset each frame.

## Choosing a tool

| The work… | Use |
| --- | --- |
| Returns one result for an input | `Task` + `useTask` / `useTaskStatus` |
| Pushes many updates | `Stream` + `useStream` |
| Is already running in a thread you own | `Var` IO operations + `askWake` |
| Must keep running while its pane is hidden | Keep the hook outside the tab or branch |
| Changes on a schedule | `wakeAfter` |
| Animates continuously | `animateTo` / `pulse` + `keepAnimating` |

Runnable versions: [Live Data & Background Work](../tutorials/live-data.md), `Tasks.hs`, `LiveChart.hs`, `Animation.hs`.
