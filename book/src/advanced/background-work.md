# Asynchronous Work & Streams

Long-running I/O, network requests, and streaming data must not block the immediate-mode GUI frame loop. `nano-ui` provides structured concurrency primitives via `Task` and `Stream`.

## One-Shot Background Work: `Task`

Allocate a task handle once during setup:

```haskell
task <- newTask
```

Inside your view, call `useTask` or `useTaskStatus`:

```haskell
-- useTask keeps the previous result while a new key runs (preventing UI flashes)
mResult <- useTask task queryKey $ \key -> do
  fetchSearchResults key

case mResult of
  Nothing      -> spinner
  Just results -> resultList results
```

### Cancellation Semantics
- **Key Change**: A task restarts automatically whenever its key changes. Ongoing work is cancelled via an asynchronous exception (`bracket` resources safely!).
- **Skipped Frames**: If a `useTask` call is **skipped for an entire frame** (for example, if hidden inside an inactive tab), the task is automatically cancelled! To keep background downloads running while hidden, keep the `useTask` call outside conditional branches.

---

## Continuous Pushing: `Stream`

For continuous telemetry, progress indicators, or token streaming:

```haskell
stream <- newStream

-- Inside the view:
tokens <- useStream stream downloadId [] $ \push -> do
  runDownloadWithCallback push
```

Updates between frames coalesce automatically so high-frequency pushes never starve the UI.
