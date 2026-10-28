# Architecture & Internals

Reference for how a frame executes and how applications are structured around it. It explains the cost model behind `readVar`, focus, and damage; a view does not need any of it to work.

## The frame lifecycle

![Rendering Pipeline](../images/rendering-pipeline.svg)

Each frame:

1. **View execution.** The `NanoUI ()` action runs top-to-bottom, appending layout nodes, reading input, and reading/writing `Var`s.
2. **Re-run check.** If a `Var` was written after it was read this pass, the action runs again with one-shot input events cleared.
3. **Layout solve.** Flex weights, gaps, padding, and constraints produce a rectangle for every node.
4. **Hit testing and focus.** Pointer coordinates, shortcuts, hover, press, and Tab order resolve against the new geometry.
5. **Damage calculation.** The new geometry and draw ops are diffed against the previous frame.
6. **Paint.** Only the damaged region is rasterized and presented.

The view runs before layout, so `respRect` is the **previous** frame's rectangle. Focus is resolved after the view, so a focus request takes effect next frame.

## Consequences

- **The view can run more than once per frame.** A write to a `Var` read earlier in the pass triggers a second pass with one-shot events cleared. Guard side effects with an event (`whenM (button ...)`), and never allocate frame-lived resources inside the view.
- **`readVar` placement is a performance decision.** Reading below the input that edits a value keeps a change to one pass; reading above costs a second pass. A `Var` bound to a widget with no `readVar` repaints only that widget; a `readVar` promotes the change to a full-window repaint.
- **Geometry lags one frame.** `respRect` is empty on a widget's first appearance. Tests warm up before targeting rectangles.
- **Widget ids are positional.** Conditional widgets, reordered lists, and conditional overlays change the count and therefore later ids. `scope` and `withKey` keep the counts stable; see [Widget Identity & Keys](../state/identity.md).
- **Idle costs zero.** With no animation, timed wake, task, or input, the loop blocks on the OS event queue. Do not dirty the context every frame to force redraws.

## Application patterns

Three real applications (`ned`, `stronghead`, `winget-gui`) converge on the same architecture; the difference is where durable state lives. All of them follow the core rule: **setup once in `IO`, then run.**

```haskell
main :: IO ()
main = do
  cache <- newViewCache
  env   <- newAppEnv
  runSdlApp defaultSdlOptions (appView env cache)
```

### Pattern A: one domain state and a wake channel

`winget-gui` keeps package-manager results in one `IORef` and snapshots it at frame start:

```haskell
data Env = Env
  { envState :: IORef AppState
  , envWake  :: IORef (IO ())
  }

appView env = do
  wake <- askWake
  liftIO $ writeIORef env.envWake wake
  state <- liftIO $ readIORef env.envState
  mainLayout state

-- worker thread
atomicModifyIORef' env.envState (\s -> (s {packages = results}, ()))
wake <- readIORef env.envWake
wake
```

Gather every read into one `Frame` record before building the window, memoize derived collections in `IORef (Maybe (key, value))` caches keyed by a revision counter, and read shortcuts after the widgets. Use this when the domain state is shared with workers, a database, or a protocol.

### Pattern B: an app record of `Var`s

`stronghead` has many independent panels, each with its own view state:

```haskell
data App = App
  { filterText   :: Var Text
  , activeTab    :: Var TabId
  , zoomLevel    :: Var Float
  , isInspecting :: Var Bool
  }

newApp = App <$> newVar "" <*> newVar TabOverview <*> newVar 1.0 <*> newVar False
```

Panel modules take the slice they own. Use this when the application is mostly interactive UI state with little external I/O.

### Pattern C: application-owned state

`ned` keeps a plain `IORef App`; the toolkit knows nothing about it. The view projects slices and re-reads after anything that may change the model:

```haskell
editorView (Editor ref) = do
  app <- liftIO (readIORef ref)
  renderEditor app
  app' <- liftIO (readIORef ref)
  when (viewChanged app app') requestFrame
```

`requestFrame` asks for one more frame when what was drawn no longer matches the state. Use this for editors, terminals, games, and models that already exist as a library. It is an escape hatch, not a replacement for `Var` dependencies.

### Choosing

| The application… | Pattern |
| --- | --- |
| Has one domain state, uses worker threads, runs commands | A: `IORef` + `askWake` |
| Is a large set of interactive panes and tools | B: record of `Var`s |
| Has a model that predates the UI or is shared with a library | C: app-owned `IORef` + `requestFrame` |
| Is a state machine with explicit transitions | Elm-style (`nano-ui-emit`) |
| Is a small tool | Whatever is simplest; a handful of `useVar`s is fine |

Patterns mix. An `IORef` domain state with `Var` view state is normal; an Elm reducer can sit inside one pane of a `Var`-based application.

## The shared view discipline

1. **Read state once at the top**, or immediately before the subtree that uses it.
2. **Build top-to-bottom.** Layout is solved after the view.
3. **Read after the parts that may write.**
4. **Run keyboard and focus queries last.**
5. **Guard effects with events.** The view can run more than once per frame.
6. **Keep derived values derived.** Cache expensive work by revision, not by copying fields.

## Refactoring history

The repository keeps a small record of source-level simplifications. These are internals, not user-facing APIs.

| Area | Change |
| --- | --- |
| Widget construction | Inlined leaf initializers; text-style packing kept on the styled path |
| Controlled inputs | Shared activation/completion policies instead of per-widget copies |
| UI scopes | Shared bridge for bracketed id allocation and paint scopes |
| Forms | Named/positional input families and zero-allocation adapters |
| Reducers | Three generic adapters replacing dozens of per-widget `NanoUI.Emit` wrappers |
| Scrollbars | Unified scalar calculation for both orientations |
| Themes | Shared Base16 construction with dark/light transforms |
| Animation | Indexed applicative channel traversal |
| Draw commands | `vector` `IsoUnbox`/`As` deriving replacing handwritten `Prim` instances |
| Canvas | Strict standard `State` monad inlining into draw-op arrays |

Validation gates for these changes: `cabal build -j1 all` with `-Werror`, the full test suite (including `nano-ui-inspection`), hidden-window SDL demos, the RGFW profile test, and the per-frame id benchmark. The detailed audits live in the git history rather than the reading path.

## See also

- State and `Var`: [State & `Var`](../state/index.md).
- Keys and ids: [Widget Identity & Keys](../state/identity.md).
- Reducer runners: [Elm-Style State](../state/elm.md).
- Contributing and tests: [Development Setup](../getting-started/development.md).
