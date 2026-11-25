# Async Work & Animation

Two rules keep a `nano-ui` application feeling snappy and quiet: never block inside the view, and never redraw when nothing is changing. Move slow I/O onto worker threads, schedule delayed updates with timers, and let the event loop sleep in between. Always compile executables with `-threaded`.

## `Task`: one background job per key

Allocate `Task` and `Stream` handles once in `IO` during startup:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:handles}}
```

Inside the view, call `useTaskStatus` to run a job and inspect whether it is loading, finished, or failed:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:task_status}}
```

If you only care about the most recent completed result, `useTask` is simpler:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:use_task}}
```

How task lifecycles work:

- **Key changes restart the job.** Whenever the key changes, `nano-ui` cancels the previous worker with an asynchronous exception and starts a new one.
- **Skipping the hook cancels the job.** If a full frame runs without calling `useTask` (for instance, because the user switched to another tab or hid a conditional panel), the running worker is canceled. Call `useTask` outside the tab or branch if work should keep going while hidden.
- **Include a counter in the key to support retries.** Pressing Retry on the same input only restarts the task if the key changes (for example, `(query, attempt)`).
- **Clean up resources with `bracket`.** Because cancellation arrives as an asynchronous exception, acquire files, sockets, or database handles inside `bracket` or `withFile`.
- **Force results on the worker thread.** `useTask` evaluates the result to weak head normal form on the worker; if you return a lazy list, force its spine (`length found \`seq\` pure found`) before returning so evaluation does not happen on the UI thread.
- **Keep old results visible while loading.** `TaskRunning previous` carries the last completed result so search lists do not flash blank on every new query.

## `Stream`: continuous pushed updates

Use `Stream` for background producers that emit values over time: download progress, sensor telemetry, log tails, or streamed model responses.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:stream}}
```

The producer receives `update :: (s -> s) -> IO ()` and runs until it returns, throws, or gets canceled. Each call to `update` wakes the frame loop; multiple pushes between frames coalesce automatically into a single redraw.

- **Stop by skipping the call.** Guarding `useStream` with `when isRunning` cancels the producer as soon as `isRunning` becomes `False`.
- **Save state outside if you want pause/resume.** Stopping a stream discards its internal accumulator and restarts from the initial value next time.
- **Never touch the UI `Context` from a producer.** Only call `update` from the producer thread (or threads it spawns).

## Integrating your own threads

If you already manage worker threads outside `Task` and `Stream`, mutate shared `Var`s with the thread-safe `IO` functions and wake the loop with the callback from `askWake`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:wake}}
```

`readVarIO`, `writeVarIO`, and `modifyVarIO` are safe from any thread. Calling `wake` schedules a frame that repaints only the widgets whose text, value, or style changed. For high-frequency updates, prefer `Stream`.

## Animation

`animateTo` smoothly transitions a float toward a target value across frames:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:animate_to}}
```

A `Transition` is either:

- `Tween ease duration delay` (durations in seconds), with easing curves including `EaseOutQuad`, `EaseInOutCubic`, `EaseOutBack`, and `EaseCubicBezier`.
- `Spring params` for damped spring physics, with presets `presetSmooth`, `presetBouncy`, and `presetStiff`.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:spring}}
```

Springs preserve velocity when retargeted mid-flight, so toggling a state back and forth never snaps abruptly. To animate any `Animatable` value channel by channel (`Float`, `Double`, `V2`, or `Color`), use `animateToA`. Once a tween or spring settles, it stops requesting frames and the loop goes back to sleep.

### Continuous animations

`pulse` returns a value oscillating smoothly between `0` and `1` with a given period in seconds:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:pulse}}
```

Unlike `animateTo`, clock-based values like `pulse` do not wake the loop by themselves. Any widget that draws from a continuous animation must call `keepAnimating` **with its own `Response`** on every frame it is on screen:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:keep_animating}}
```

As soon as a frame stops calling `keepAnimating`, the loop goes right back to sleep.

> [!CAUTION]
> Never force animation frames by marking the context dirty every frame. That creates an unpaced busy loop and burns CPU even when nothing is moving.

### Timers

Call `wakeAfter` to schedule a single future frame after a delay in **seconds**:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Async.hs:wake_after}}
```

Pending wakes clear after every frame, so keep calling `wakeAfter` on each frame while the timer is still active.

## Picking the right primitive

| What you need | Primitive to use |
| --- | --- |
| Fetch or compute one result for a given input | `Task` with `useTask` or `useTaskStatus` |
| Receive a stream of updates over time | `Stream` with `useStream` |
| Push updates from an existing background thread | `writeVarIO` / `modifyVarIO` plus `askWake` |
| Keep work running when its tab or panel is hidden | Place `useTask` / `useStream` outside the tab or branch |
| Trigger periodic or delayed refreshes | `wakeAfter` |
| Animate values smoothly or continuously | `animateTo` / `animateToA`, or `pulse` with `keepAnimating` |

For runnable programs, see [Tutorial 3: Live Data & Background Work](../tutorials/live-data.md) along with `Tasks.hs`, `LiveChart.hs`, and `Animation.hs`.
