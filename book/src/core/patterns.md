# Architecture & Application Patterns

Real-world applications built on `nano-ui` (such as `ned`, a vim-like modal editor, `stronghead`, a profiling debugger, and `winget-gui`, a Windows package manager client) converge on clean, highly reliable architectural patterns.

## The Core Rule: Setup Once in IO, Then Run

Everything frame-lived is either managed via a `Var` or declared directly inside the view action:

```haskell
main :: IO ()
main = do
  cache <- newViewCache      -- Persistent records of newVar calls & caches
  env   <- newAppEnv         -- Application state outside nano-ui
  runSdlApp defaultSdlOptions (appView env cache)
```

---

## Pattern A: The `winget-gui` Pattern (Worker Threads + IORef)

For applications performing extensive I/O, subprocess execution, or database queries:

```haskell
data Env = Env
  { envState :: IORef AppState
  , envWake  :: IORef (IO ())
  }

main :: IO ()
main = do
  stateRef <- newIORef initialAppState
  wakeRef  <- newIORef (pure ())
  let env = Env stateRef wakeRef
  runSdlApp opts (appView env)

appView :: Env -> NanoUI ()
appView env = do
  -- Install the wake callback on the first frame
  wakeCb <- askWake
  liftIO $ writeIORef env.envWake wakeCb

  -- Snapshot the state at the beginning of the frame
  state <- liftIO $ readIORef env.envState

  -- Build UI hierarchy top-to-bottom
  mainLayout state
```

### Thread Safety with `askWake`
Background threads update `envState` atomically, then trigger a redraw:

```haskell
forkIO $ do
  results <- queryPackageDatabase
  atomicModifyIORef' env.envState (\s -> (s { packages = results }, ()))
  wake <- readIORef env.envWake
  wake  -- Wake the event loop for a new frame
```

---

## Pattern B: The `stronghead` Pattern (App Record of `Var`s)

For complex multi-pane applications with tightly coupled interactive state:

```haskell
data App = App
  { filterText :: Var Text
  , activeTab  :: Var TabId
  , zoomLevel  :: Var Float
  , isInspecting :: Var Bool
  }

newApp :: IO App
newApp = App
  <$> newVar ""
  <*> newVar TabOverview
  <*> newVar 1.0
  <*> newVar False
```

The `App` record is passed into view functions, which read and mutate its fields directly with zero global state.
