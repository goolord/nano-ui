# Async Work & Animation

The view must never block, and it should not redraw when nothing is moving. nano-ui handles both with the same idea: work is scheduled on a worker or a timed wake, and the frame loop sleeps until there is something to draw.

Compile with `-threaded`; the loop sleeps while a job runs and the job runs on a Haskell thread while it is blocked.

## `Task`: one job per key

Allocate the handle once in `IO`:

```haskell
data App = App
  { appSearch :: Task Text [Text]
  , appFlaky  :: Task Int Text
  }

newApp :: IO App
newApp = App <$> newTask <*> newTask
```

Use it from the view:

```haskell
status <- useTaskStatus (appSearch app) queryKey (fetchSearchResults queryKey)

case status of
  TaskRunning previous -> showPreviousOrSpinner previous
  TaskDone results     -> renderResults results
  TaskFailed err prev  -> showError (displayException err)
```

`useTask` is the simpler form when only the latest result matters:

```haskell
contents <- useTask (appFile app) path (T.readFile (T.unpack path))
label (fromMaybe "Loading..." contents)
```

Rules:

- **The key identifies the job.** A new key starts a worker and cancels the old job with an asynchronous exception.
- **A skipped hook cancels the job.** If the view does not call `useTask` for a full frame — because the pane is in an inactive tab or inside a branch — the work is released. Keep the hook outside tabs when work must continue while hidden.
- **To retrigger the same input, put a counter in the key.** A Retry button must change the key or the job never reruns.
- **Cancellation is an async exception.** Use `bracket` or `withFile` inside the job so resources are released when the user changes the query.
- **The result is forced on the worker.** Return a value in weak head normal form; force a list's spine with `length found \`seq\` pure found`.
- **`TaskRunning previous` carries the last result**, so a list does not flash empty while the next query runs.

## `Stream`: repeated pushes

A `Stream` is for sensors, downloads, token streams, and anything that pushes repeatedly:

```haskell
data App = App {appProgress :: Stream () Int}

newApp = App <$> newStream

pct <- useStream (appProgress app) () (0 :: Int) $ \update ->
  forM_ [1 .. 100] $ \i -> do
    threadDelay 50000
    update (const i)
progressBar (fromIntegral pct / 100)
```

The producer receives `update :: (s -> s) -> IO ()` and returns when done, cancelled, or throwing. Each update wakes the loop; updates between frames coalesce into one frame.

- **Stop by not calling the hook.** `when isRunning (useStream ...)` releases the producer.
- **Stopping discards the accumulated state.** Save it outside and pass it as the next initial value to resume.
- **The producer owns no UI.** It may call `update` from its thread or threads it starts; never touch the UI `Context`.

## Threads outside those handles

```haskell
wake <- askWake        -- in the view, once

forkIO $ do
  result <- expensiveQuery
  writeVarIO queryResult result
  wake                 -- schedule one frame
```

`readVarIO`, `writeVarIO`, and `modifyVarIO` are the thread-safe `Var` operations. The wake repaints the whole window because nothing says which widget changed; fine for occasional updates, not for a per-token stream.

## Animation

`animateTo` moves a value toward a target:

```haskell
currentWidth <- animateTo (Tween EaseOutQuad 0.25 0) targetWidth
```

A `Transition` is either:

- **`Tween ease duration delay`**, in seconds. `EaseOutQuad`, `EaseInOutCubic`, `EaseOutBack`, and `EaseCubicBezier` are some available curves.
- **`Spring params`**, a damped spring. `presetSmooth`, `presetBouncy`, and `presetStiff` are the presets.

```haskell
pos <- animateToA (Spring presetSmooth) (V2 targetX targetY)
```

Springs retarget mid-flight from their current position and velocity. `animateToA` animates any `Animatable` value — `Float`, `Double`, `V2`, or `Color` — channel by channel.

### Continuous animation

`pulse` returns a value oscillating in `[0, 1]` with the given period in seconds:

```haskell
opacity <- pulse 1.2
```

Clock-driven values do not, by themselves, keep the loop awake. A widget that draws from them must call `keepAnimating` **with its own response**, every frame it is on screen:

```haskell
(resp, ()) <- customWidget defaultCustomWidgetSpec
  { widgetLayout = fixedWH 24 24
  , widgetContent = contentKey [t * 100]
  , widgetDraw = \cdc (Rect x y w h) -> runCanvasFor cdc $
      drawCircle (V2 (x + w / 2) (y + h / 2)) (6 + 3 * t) (themeAccent (cdcTheme cdc))
  }
keepAnimating resp
```

The loop goes back to sleep on the first frame that leaves it out.

> [!CAUTION]
> Never force continuous redraws by dirtying the context every frame. That is an unpaced busy loop. Call `keepAnimating` only in the widget that is actually moving.

### Timers

`wakeAfter` requests one frame after a delay in **seconds**:

```haskell
clock = do
  label =<< currentTimeText
  wakeAfter 1
```

Call it on every frame that still needs the future wake; pending wakes reset each frame. Use `wakeAfter` for clocks, debounces, and polling on a schedule; use `keepAnimating` for continuous motion.

## Choosing a tool

| The work… | Use |
| --- | --- |
| Returns one result for an input | `Task` + `useTask` / `useTaskStatus` |
| Pushes many updates | `Stream` + `useStream` |
| Is already running in a thread you own | `Var` IO operations + `askWake` |
| Must keep running while its pane is hidden | Keep the hook outside the tab or branch |
| Changes on a schedule | `wakeAfter` |
| Animates continuously | `animateTo` / `pulse` + `keepAnimating` |

The runnable versions are in [Live Data & Background Work](../tutorials/live-data.md), `Tasks.hs`, and `LiveChart.hs`.

## See also

- Background work in a full application: [Live Data & Background Work](../tutorials/live-data.md).
- Custom animation drawing: [Custom Drawing & Controls](../widgets/custom-drawing.md).
- Window updates and screenshots: [Desktop Windows](window-management.md).
