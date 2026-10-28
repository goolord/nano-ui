# Tutorial 3: Live Data & Background Work

A frame must finish quickly. Searching a database, downloading a file, or waiting for a sensor cannot happen inside the view, or the window stops responding while the work runs. nano-ui moves that work to worker threads behind two handles, `Task` and `Stream`, and wakes the frame loop when there is something new to draw.

This tutorial builds three small panes from [`packages/nano-ui-demo/examples/Tasks.hs`](https://github.com/goolord/nano-ui/blob/main/packages/nano-ui-demo/examples/Tasks.hs) plus the live chart from [`LiveChart.hs`](https://github.com/goolord/nano-ui/blob/main/packages/nano-ui-demo/examples/LiveChart.hs). Run them with:

```sh
cabal run nano-ui-example-tasks
cabal run nano-ui-example-live-chart
```

> [!IMPORTANT]
> Compile with `-threaded`. The frame loop sleeps while a job runs, and the job runs on a Haskell thread while the loop is blocked.

## 1. Allocate handles once

A `Task` owns one typed job; a `Stream` owns one typed push channel. Both are created during setup, exactly like a `Var`:

```haskell
data App = App
  { appSearch   :: Task Text [Text]
  , appFlaky    :: Task Int Text
  , appProgress :: Stream () Int
  }

newApp :: IO App
newApp = App <$> newTask <*> newTask <*> newStream
```

The key type and result type are fixed by the handle. That means a task cannot accidentally be reused for a different job, and no message type is erased at runtime.

## 2. A background search

The query field is debounced: `searchInput'` reports `respChanged` once typing pauses, not on every keystroke. The committed query becomes the task's key:

```haskell
searchCard :: App -> NanoUI ()
searchCard app = do
  query <- useVar ""
  committed <- useVar ""
  resp <- searchInput' "Find a word" query
  when (respChanged resp) (writeVar committed =<< readVar query)
  key <- readVar committed

  status <- useTaskStatus (appSearch app) key (slowSearch key)

  let (hits, running) = case status of
        TaskRunning previous -> (fromMaybe [] previous, True)
        TaskDone found       -> (found, False)
        TaskFailed _ previous -> (fromMaybe [] previous, False)
  rowWith (tight . gap 8 . alignMid) $ do
    -- The spinner comes and goes, so it sits in a scope.
    scope (when running spinner)
    muted (T.pack (show (length hits)) <> " matches"
             <> if running then " (searching...)" else "")
  rowWith (wrap . gap 6 . fillW) $
    forM_ (take 24 hits) $ \w -> withKey w (label w)
```

`slowSearch` is ordinary blocking IO:

```haskell
slowSearch :: Text -> IO [Text]
slowSearch q = do
  threadDelay 500000
  let found = filter (T.isInfixOf (T.toLower q)) wordList
  length found `seq` pure found
```

What the loop does with it:

- The first frame calls `useTaskStatus` with a new key, which starts a worker thread.
- Later frames return immediately. `TaskRunning previous` carries the last completed result, so the list does not flash empty when a new query starts.
- The worker finishes and wakes the loop for a frame. The next call reports `TaskDone`.
- Changing the key (typing a new committed query) cancels the old worker with an asynchronous exception and starts the new one.

`useTask` is the simpler variant when only the latest result matters:

```haskell
contents <- useTask (appFile app) path (T.readFile (T.unpack path))
label (fromMaybe "Loading..." contents)
```

## 3. Retrying a failed job

A job that throws becomes `TaskFailed err previous`. The exception is shown and the key includes an attempt counter, so pressing Retry reruns the *same* input:

```haskell
flakyCard :: App -> NanoUI ()
flakyCard app = do
  attemptVar <- useVar (0 :: Int)
  attempt <- readVar attemptVar
  status <- useTaskStatus (appFlaky app) attempt (flakyRequest attempt)
  scope $ case status of
    TaskRunning _ -> rowWith (tight . gap 8 . alignMid) $ do
      spinner
      label "Contacting the server..."
    TaskDone reply -> rowWith (tight . gap 8 . alignMid) $ do
      label reply
      whenM (button "Run again") (modifyVar attemptVar (+ 1))
    TaskFailed err _ -> rowWith (tight . gap 8 . alignMid) $ do
      danger (T.pack (displayException err))
      whenM (button "Retry") (modifyVar attemptVar (+ 1))
```

Two rules are visible here:

- **The key must change to rerun.** Keying by the input alone would never restart an identical request; the attempt number is part of the key.
- **Each branch is a whole `scope`.** The branches declare different widgets; the scope keeps the widget ids after the card stable as the status changes.

Resource safety belongs inside the job. A file handle, a network connection, or a temporary directory should be acquired with `bracket`, because cancellation is an asynchronous exception:

```haskell
withConnection connInfo $ \conn -> ...
```

## 4. Streams: many updates, one frame

A `Stream` is for work that pushes repeatedly: progress ticks, sensor readings, tokens from a model. The producer is a function that receives `update` and runs until it returns or is cancelled:

```haskell
progressCard :: App -> NanoUI ()
progressCard app = do
  running <- useVar False
  checkbox "Run" running
  isRunning <- readVar running
  scope $ when isRunning $ do
    pct <- useStream (appProgress app) () (0 :: Int) $ \update ->
      forM_ [1 .. 100] $ \i -> do
        threadDelay 50000
        update (const i)
    progressBar (fromIntegral pct / 100)
    muted (if pct >= 100
             then "Done. Untick and tick Run to start over."
             else T.pack (show pct) <> "%")
```

- **Stopping is not calling the hook.** When `isRunning` is false, `useStream` is not called, and the producer is cancelled. There is no stop button to wire up.
- **Updates coalesce.** If the producer pushes five values between two frames, the next frame draws the latest one; it does not run five frames.
- **The stream's state starts from the initial value.** Restarting after a pause begins at `0` here, which is why the label explains how to start over. To resume from the last state, save it outside and pass it as the initial value (the live chart below does exactly that).
- **Keep the hook outside tabs when the stream must keep running.** Like a task, a stream is released when its hook is skipped for a whole frame.

The frame loop sleeps until the next update. A paused progress pane costs no frames.

## 5. A live chart

The chart is a `Stream` feeding a `PlotCache`. The cache is allocated once and passed to `plot`; it compares the chart against the one it drew last time and rebuilds the diagram only when the data or theme changes:

```haskell
data ChartApp = ChartApp
  { appFeed  :: Stream () Feed
  , appSaved :: Var Feed
  , appPlots :: PlotCache
  }

newChartApp :: IO ChartApp
newChartApp = ChartApp <$> newStream <*> newVar emptyFeed <*> newPlotCache
```

The view pauses by not calling the stream, and resumes from the saved feed:

```haskell
chartCard :: ChartApp -> NanoUI ()
chartCard app = do
  running <- useVar True
  isRunning <- readVar running
  saved <- readVar (appSaved app)
  feed <-
    scope $
      if isRunning
        then useStream (appFeed app) () saved $ \update -> forever $ do
          threadDelay 100000
          update advance
        else pure saved

  rowWith (tight . gap 8 . alignMid . fillW) $ do
    whenM (button (if isRunning then "Pause" else "Resume")) $ do
      when isRunning (writeVar (appSaved app) feed)
      writeVar running (not isRunning)
    muted (if isRunning then "Live: a sample every 100 ms"
                        else "Paused: the loop is idle")

  let points = feedPoints feed
  resp <- withCursorShape UiCursorCrosshair
            (plot (appPlots app) (fillW . minH 300) (walkChart points))

  rowWith (tight . gap 16 . alignMid . fillW) $ do
    let ys = fmap snd points
    scope $ if Seq.null ys
      then muted "Waiting for the first sample..."
      else do
        kv "min" (fmt (minimum ys))
        kv "max" (fmt (maximum ys))
        kv "last" (fmt (feedLevel feed))
    scope . for_ (plotHover resp) $ \hover ->
      kv "cursor" ("#" <> T.pack (show (round (hoverDataX hover) :: Int))
                     <> " = " <> fmt (hoverDataY hover))
```

`Feed`, `advance`, and `fmt` are in `LiveChart.hs`; `walkChart` builds the chart value:

```haskell
walkChart :: Seq (Double, Double) -> Chart
walkChart points =
  withGrid GridBoth . withLegend LegendNone . withXAxis "sample" . withYAxis "value" $
    chart [line "walk" points]
```

The chart API is small:

- `line`, `scatter`, `bar`, `area`, and `step` build series from points (or category/value pairs for bars).
- `chart` takes the list of series; `withGrid`, `withLegend`, `withXAxis`, `withYAxis`, and `withTitle` decorate it.
- `plot cache layout theChart` draws it and returns a `PlotResponse` whose `plotHover` is the nearest data point under the pointer.

Building the `Chart` value every frame is fine — it is a plain value, and comparing a few hundred points is cheap. What is expensive is rebuilding the *diagram*; the `PlotCache` is what prevents that.

## 6. The rules, collected

| Situation | Use | Key rule |
| --- | --- | --- |
| One slow request per input | `Task` + `useTaskStatus` | Key changes restart the job; include a retry counter to rerun the same input. |
| Latest result only | `Task` + `useTask` | Previous result stays while a new key runs. |
| Repeated pushes | `Stream` + `useStream` | Stop by not calling; updates coalesce; pass saved state as the initial value to resume. |
| Work must survive a hidden tab | Move the hook outside the tab | A skipped hook releases the job. |
| Worker threads you own | `readVarIO` / `modifyVarIO` + `askWake` | Never touch the UI `Context` from another thread. |

## 7. Try it

- Add a search that runs on every keystroke (key by the query) and compare the cancellation behaviour with `searchInput'`.
- Stream markdown tokens into `NanoUI.Markdown` and append them incrementally (see [Ecosystem Packages](../packages/index.md)).
- Virtualize a 100,000-row log list using `getScrollMetricsUi` (see [Layout & Scrolling](../layout/index.md) and the `Logs.hs` example).
- Replace the streamed chart with a `Task` that loads a whole dataset once.

For the full cancellation and lifecycle reference, read [Async Work & Animation](../advanced/async.md).
