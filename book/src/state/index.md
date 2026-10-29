# State & `Var`

The view runs every frame, and nothing survives between frames except state you keep. Most state lives in a `Var`, a typed mutable reference. Input widgets take the `Var` they display and mutate it directly, with no callback.

```haskell
counter :: NanoUI ()
counter = rowWith (gap 8 . alignMid) $ do
  n <- useVar (0 :: Int)
  whenM (button "-") (modifyVar n (subtract 1))
  label . T.pack . show =<< readVar n
  whenM (button "+") (modifyVar n (+ 1))
```

## Keep state, derive values

A value that changes over time and cannot be recomputed belongs in a `Var`. A value that *can* be recomputed belongs in a local binding, computed fresh every frame:

```haskell
appView :: App -> NanoUI ()
appView app = do
  -- Keep: the inputs to the next frame.
  draft <- useVar ""
  settings <- readVar app.appSettings

  -- Derive: computed from what was kept.
  label ("Signed in as " <> settings.nickname)
  label ("Draft: " <> T.pack (show (T.length draft)) <> " characters")
```

A derived value copied into a second `Var` drifts as soon as one writer forgets to update it.

## Allocating state

There are two ways to create a `Var`, plus one for dynamic collections:

| | `useVar` | `newVar` |
| --- | --- | --- |
| Where | Inside the view | In `IO`, during setup |
| Types | `Int`, `Float`, `Double`, `Bool`, `Text`, `SortCol` (`useEnumVar` for any `Bounded`/`Enum` type) | Any type |
| Identity | The call site, counted in call order | The reference itself |
| Best for | View-local state: a draft, an open flag, a filter | Application state: a document, a list, a settings record, a task |

`useVar` needs no setup. Its initial value is used only when the slot is first created; call `writeVar` to reset it later. Its slots are counted separately from widget ids, but a **conditional `useVar`** still needs `scope` so later slots keep their positions:

```haskell
showAdvanced <- useVar False
scope . whenM (readVar showAdvanced) $ do
  level <- useVar (3 :: Int)
  ...
```

`newVar` can hold any type. Allocate it once in `IO` and pass it into the view:

```haskell
data App = App
  { appSettings :: Var Settings
  , appTodos :: Var [Todo]
  }

newApp :: IO App
newApp = App <$> newVar (Settings 40 False "Ada") <*> newVar []

main :: IO ()
main = do
  app <- newApp
  runSdlApp opts (appView app)
```

Passing the same `Var` to two widgets shares their state. Allocating a `Var` *inside* the view creates a fresh one on every pass.

For dynamic collections, `VarMap` gives one `Var` per key:

```haskell
rows <- newVarMap

rowState <- varAt rows todoId (RowState False "")
retainVars (`elem` liveIds) rows
```

`varAt` returns the existing `Var` for a key or creates it. `dropVar` removes one; `retainVars keep` prunes everything whose key fails the predicate.

## Projections and read-only views

A record in a `Var` gives a `Var` for each field. With `Generic` and `OverloadedRecordDot`:

```haskell
data Settings = Settings
  { volume :: !Float
  , silent :: !Bool
  , nickname :: !Text
  }
  deriving (Eq, Generic)

settings <- newVar (Settings 40 False "Ada")

slider 0 100 settings.volume
checkbox "Silent" settings.silent
textInput settings.nickname
```

`settings.volume` is a `Var Float` projecting one field. Editing it does not repaint what reads only `settings.nickname`.

- **`constVar x`** is read-only; writes are no-ops. Use it for a derived value shown in an input widget.
- **`withValue val widget`** runs a `Var` widget against a temporary `Var` and returns the result and final value. `nano-ui-emit` uses it for `emitChanged`.
- **`zoomVar getter setter`** projects a computed part of a larger `Var`; `zoomVarL` does the same from a lens.
- **Lists:** `forEachVar key list body` gives per-element `Var`s that follow their keys across reorders. `withElemVars` is the variant for filtering or sorting yourself.

## What a write costs

`readVar` records a dependency for the current pass. When the value changes, nano-ui classifies the write:

| Write | When | Cost |
| --- | --- | --- |
| **Forward** | Written before any read this pass | One pass; later reads see the new value |
| **Backward** | Written after being read earlier this pass | A second view pass, with one-shot input events cleared |
| **Unchanged** | New value `==` old value | No-op: no pass, no repaint |

Two habits follow:

- Place `readVar` **below** the widget that edits the value when the order is free. Typing then resolves in one pass; reading above it costs a second pass per change.
- Do not `readVar` a value only to pass it back into the same widget. Passing the `Var` is enough.

A freshly computed `NaN` is never unchanged (`NaN /= NaN`), so writing one every pass redraws forever. Guard the write.

> [!TIP]
> When an input widget is bound to a `Var` and nothing in the view calls `readVar` on it, only that widget repaints on change. A `readVar` promotes the write to a full-window repaint.

`peekVar` reads without recording a dependency. `readVarIO`, `writeVarIO`, and `modifyVarIO` are the thread-safe versions; after a background write, call the callback from `askWake` to schedule a frame. Never touch the UI `Context` from a worker thread.

## Lifetime and resources

Tasks, streams, image handles, drag handles, plot caches, and file-dialog ids are allocated in `IO` during setup and stored in the application record, exactly like a `newVar`:

```haskell
data App = App
  { appDocument :: Var TextDocument
  , appTask :: Task Text [Text]
  }
```

A handle allocated inside the view is a per-frame leak. If the view needs a new handle when a key changes, allocate it in setup and key its use, or use a hook that owns its lifetime (`useTask`, `useStream`, `useImageRgba`).

## Where should this value live?

| The value… | Put it in |
| --- | --- |
| Is a draft, open flag, selection, or filter used by one view | `useVar` / `useEnumVar` |
| Is a list, document, settings record, or handle | `newVar` in `IO`, stored in the app record |
| Belongs to one row of a dynamic list | A keyed `Var` (`forEachVar`, `withElemVars`, `VarMap`) |
| Already lives outside the UI | An `IORef` or app record, woken with `askWake` |
| Is a pure state machine with a clear `msg -> model -> model` | The [Elm-style reducer](elm.md) |

Next: [Widget Identity & Keys](identity.md) for keyed lists and conditional UI, or [Elm-Style State](elm.md) for the reducer alternative.
