# Asynchronous Work & Streams

The view runs in the frame loop, so it must never block. Long-running I/O, network requests, and streaming data go on worker threads behind two typed handles: `Task` for one-shot work and `Stream` for repeated pushes. Both wake the loop when there is a new result to draw.

Compile with `-threaded`; the loop sleeps while a job runs, and the job runs on a Haskell thread while it is blocked.

## `Task`: one job per key

Allocate the handle once, during setup:

```haskell
data App = App
  { appSearch :: Task Text [Text]
  , appFlaky  :: Task Int Text
  }

newApp :: IO App
newApp = App <$> newTask <*> newTask
```

Then call the hook from the view. `useTaskStatus` reports the whole lifecycle:

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

- **A key identifies the job.** The first frame with a new key starts the worker. Changing the key cancels the old job with an asynchronous exception and starts the new one.
- **A skipped hook cancels the job.** If the view does not call `useTask` for a full frame — because the pane is in an inactive tab, or inside a branch — the work is released. Keep the hook outside tabs when the work must continue while hidden.
- **To retrigger the same input, put a counter in the key.** Pressing Retry must change the key, or the key stays equal and the job never reruns:

  ```haskell
  attempt <- readVar attemptVar
  status <- useTaskStatus (appFlaky app) attempt (flakyRequest attempt)
  whenM (button "Retry") (modifyVar attemptVar (+ 1))
  ```

- **Cancellation is an async exception.** Use `bracket`, `withFile`, or a similar resource combinator inside the job. A connection or temporary directory must be released even when the user changes the query mid-request.
- **The result is forced on the worker.** Return a value in weak head normal form; force a list's spine with `length found \`seq\` pure found` if the view should not evaluate it.
- **`TaskRunning previous` carries the last result**, which is why a search list does not flash empty while the next query runs.

## `Stream`: repeated pushes

A `Stream` is for sensors, downloads, token streams, and anything that pushes values repeatedly:

```haskell
data App = App
  { appProgress :: Stream () Int
  }

newApp = App <$> newStream
```

```haskell
pct <- useStream (appProgress app) () (0 :: Int) $ \update ->
  forM_ [1 .. 100] $ \i -> do
    threadDelay 50000
    update (const i)
progressBar (fromIntegral pct / 100)
```

The producer receives `update :: (s -> s) -> IO ()` and returns when it is done, cancelled, or throws. Each update wakes the loop; updates that arrive between frames coalesce into one frame.

- **Stop by not calling the hook.** There is no stop function. `when isRunning (useStream ...)` releases the producer when the condition turns false.
- **Stopping discards the accumulated state.** If the view needs to resume from where it left off, save the current value outside and pass it as the next initial value. The live chart example does exactly this with a `Var Feed`.
- **The producer owns no UI.** It may call `update` from the thread it runs on, including threads it starts; it must not touch the UI `Context`.

## Threads outside the handles

When state already lives outside `Task`/`Stream`:

```haskell
wake <- askWake        -- in the view, once

forkIO $ do
  result <- expensiveQuery
  writeVarIO queryResult result
  wake                 -- schedule one frame
```

`readVarIO`, `writeVarIO`, and `modifyVarIO` are the thread-safe `Var` operations. The wake repaints the whole window because nothing says which widget changed; that is fine for occasional external updates, not for a per-token stream.

Never call UI functions or touch context/damage from a worker thread.

## Choosing a tool

| The work… | Use |
| --- | --- |
| Returns one result for an input | `Task` + `useTask` / `useTaskStatus` |
| Pushes many updates | `Stream` + `useStream` |
| Is already running in a thread you own | `Var` IO operations + `askWake` |
| Needs to keep running while its pane is hidden | Keep the hook outside the tab or branch |
| Needs to stop and resume with state | Save the state, pass it as the next initial value |

The complete runnable version of these patterns is in [Tutorial 3: Live Data & Background Work](../tutorials/live-data.md) and `packages/nano-ui-demo/examples/Tasks.hs`.

## Animations are not background work

Animation is driven by the frame loop, not a worker. Use `animateTo`, `pulse`, and `keepAnimating` from [Animations & Timers](animation.md); do not start a thread to change a value every display frame.
