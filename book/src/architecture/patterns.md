# Application Patterns

Real applications built on nano-ui — `ned` (a vim-like modal editor), `stronghead` (a profiling debugger), and `winget-gui` (a Windows package-manager front end) — converge on the same architecture. The differences are about where the durable state lives, not about the frame.

## The core rule: setup once in IO, then run

Everything frame-lived is either a value created in `IO` before the session, or a `Var` declared in the view. Nothing else survives.

```haskell
main :: IO ()
main = do
  cache <- newViewCache      -- a record of newVar calls and IORef caches
  env   <- newAppEnv         -- domain state outside nano-ui's knowledge
  runSdlApp
    defaultSdlOptions {sdlWindowSettings = ...}
    (appView env cache)
```

The view is a pure-ish projection of that state. It may read and write it, but it must not create a new source of truth on every pass.

## Pattern A: `winget-gui` — one domain state and a wake channel

`winget-gui` runs package-manager commands on worker threads and keeps the result in one `IORef`. The view snapshots that state at the start of each frame:

```haskell
data Env = Env
  { envState :: IORef AppState
  , envWake  :: IORef (IO ())
  }

main :: IO ()
main = do
  stateRef <- newIORef initialAppState
  wakeRef  <- newIORef (pure ())
  runSdlApp opts (appView (Env stateRef wakeRef))

appView :: Env -> NanoUI ()
appView env = do
  -- Install the wake callback on the first frame.
  wake <- askWake
  liftIO $ writeIORef env.envWake wake

  -- Snapshot the frame's state once, before building anything.
  state <- liftIO $ readIORef env.envState

  mainLayout state
```

A worker publishes a change atomically, then wakes the loop:

```haskell
forkIO $ do
  results <- queryPackageDatabase
  atomicModifyIORef' env.envState (\s -> (s {packages = results}, ()))
  wake <- readIORef env.envWake
  wake
```

The company pattern around this:

- **`prepareFrame`** gathers every read into one `Frame` record before the window is built. Widgets do not each read the `IORef` and risk seeing different snapshots.
- **Memoized derived collections** live in `IORef (Maybe (key, value))` caches keyed by a revision counter. Expensive grouping or sorting runs when the revision changes, not every frame.
- **Keyboard shortcuts are read after** the widgets have run, so focus-dependent guards see the current focus.
- **A headless mode runs the same view** against real data with `--screenshot` / `--selftest` (SDL in a hidden window), which is how the UI is regression-tested.

Use this pattern when the domain state is shared with worker threads, a database, or a long-running protocol.

## Pattern B: `stronghead` — an app record of `Var`s

`stronghead` has many panels, each with its own interactive state. It keeps one large record of handles:

```haskell
data App = App
  { filterText   :: Var Text
  , activeTab    :: Var TabId
  , zoomLevel    :: Var Float
  , isInspecting :: Var Bool
  , panes        :: PaneState
  }

newApp :: IO App
newApp = App
  <$> newVar ""
  <*> newVar TabOverview
  <*> newVar 1.0
  <*> newVar False
  <*> newPaneState
```

Panel modules take the slice they own:

```haskell
inspectorPane :: Var Bool -> Var Float -> NanoUI ()
inspectorPane inspecting zoom = panel $ do
  ...
```

The record is passed into view functions, which read and mutate its fields directly. There is no global state and no lost update: every handle is explicit in a type.

Use this pattern when the application is mostly interactive UI state, with many independent surfaces and little external I/O.

## Pattern C: `ned` — the application owns the state

`ned` is an editor first: its state is a document and an editing model that exist independently of the UI. It keeps a plain `IORef App` and lets the toolkit know nothing about it. The view projects slices of it each frame and re-reads after any part that might have changed it:

```haskell
newtype Editor = Editor (IORef App)

editorView :: Editor -> NanoUI ()
editorView (Editor ref) = do
  app <- liftIO (readIORef ref)
  renderEditor app

  -- The parts above may have changed the model (a command, a click).
  app' <- liftIO (readIORef ref)
  when (viewChanged app app') requestFrame
```

`requestFrame` asks for one more frame when what was drawn no longer matches the state. This is a deliberate escape hatch; it is not a substitute for `Var` dependencies.

Use this pattern for editors, terminals, games, and anything whose model is already a well-tested library that should not be rewritten in terms of `Var`s.

## Choosing

| The application… | Pattern |
| --- | --- |
| Owns one domain state, uses worker threads, runs commands | A — `IORef` + `askWake` |
| Is a large set of interactive panes and tools | B — record of `Var`s |
| Has a model that predates the UI or is shared with a library | C — app-owned `IORef` + `requestFrame` |
| Is a state machine with explicit transitions and few external effects | Elm-style (`nano-ui-emit`) |
| Is a small tool | Whatever is simplest; a handful of `useVar`s is fine |

The patterns mix. An `IORef` domain state with `Var` view state is normal; an Elm reducer can sit inside one pane of a `Var`-based application.

## The shared view discipline

Independent of where state lives, the view has the same shape:

1. **Read the state once at the top**, or read each source immediately before the subtree that uses it.
2. **Build top-to-bottom.** Layout is solved after the view runs, so geometry is from the previous frame; do not target widgets by rectangle in the same pass they are first declared.
3. **Read after the parts that may write.** A read placed before a writer sees the old value and forces a second pass.
4. **Run keyboard and focus queries last.** Focus state is resolved after the widgets have run.
5. **Guard effects with events.** A view can run more than once per frame; file I/O, subprocesses, and logging belong behind a button or shortcut check.
6. **Keep derived values derived.** Cache expensive computations by revision, not by copying every field into a `Var`.

## Anti-patterns

- **A global `IORef` read from many widget helpers.** The frame sees inconsistent snapshots; gather state once, or read it explicitly at one point.
- **Allocating a source of truth in the view.** `newVar`/`newIORef`/`newTask` inside the view leaks or resets every pass.
- **Mirroring the model in `Var`s.** Two sources of truth drift. Keep local UI state local and durable state single.
- **Using `requestFrame` to force animation.** Use `keepAnimating` inside the animating widget, and `wakeAfter` for timers.
- **Rebuilding caches every frame.** Compare first (a revision counter, a pointer, a small key) and rebuild only when it changes.

For the frame that makes these patterns necessary, read [The Frame Model](frame-model.md). For pure reducers, read [Elm-Style Views](../state/elm-overview.md).
