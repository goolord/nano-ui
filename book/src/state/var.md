# `Var` in Depth

A `Var a` is a typed mutable reference. Values are not stored in `Data.Dynamic`, looked up by runtime type representation, or cast out of untyped widget slots. The same `NanoUI` code works whether state lives in `Var`s, an Elm-style reducer, or an application-owned `IORef`.

## Allocating a `Var`

There are three allocation strategies, and they answer different questions.

### `useVar`: state owned by a position in the view

```haskell
n <- useVar (0 :: Int)
```

`useVar` allocates on the first frame it is reached at that call site and retrieves the existing value afterwards. It holds `Int`, `Double`, `Float`, `Bool`, `Text`, or `SortCol`; `useEnumVar` holds any `Bounded`/`Enum` type. Positional slots are counted separately from widget ids, so a widget that comes and goes does not shift them — but a **conditional `useVar`** still needs `scope` to reserve its slot:

```haskell
showAdvanced <- useVar False
scope . whenM (readVar showAdvanced) $ do
  level <- useVar (3 :: Int)     -- its slot is reserved whether shown or not
  ...
```

The initial value is used only when the slot is first created. Use `writeVar` to reset it later.

### `newVar`: application-owned state

```haskell
data App = App
  { appSettings :: Var Settings
  , appTodos :: Var [Todo]
  , appTask :: Task Text [Text]
  }

newApp :: IO App
newApp = App <$> newVar (Settings 40 False "Ada") <*> newVar [] <*> newTask

main :: IO ()
main = do
  app <- newApp
  runSdlApp opts (appView app)
```

`newVar` can hold any type, including a list, a document, a chart cache, or a task handle. Pass the resulting record into the view. The `Var`'s identity is the reference itself, not a call position, so it can be shared by any number of widgets and helper functions. Passing the same `Var` to two widgets shares their state.

`useVar` is usually the right default for view-local state; `newVar` is for state that the application owns, that outlives a particular view function, or that is shared with code outside the view.

### `VarMap`: one `Var` per key

For dynamic collections where rows come and go, a `VarMap k a` gives a `Var` per key:

```haskell
rows <- newVarMap   -- in IO

rowState <- varAt rows todoId (RowState False "")
...
retainVars (`elem` liveIds) rows
```

`varAt` returns the existing `Var` for a key or creates one with the supplied initial value. `dropVar` removes one entry; `retainVars keep` prunes everything whose key fails the predicate. This is the tool for state that must outlive a row's visibility, such as a collapsed tree node or a half-typed edit in a filtered list.

## Projections and read-only views

A record in a `Var` gives a `Var` for each field. With `GHC.Generics.Generic` and `OverloadedRecordDot`:

```haskell
data Settings = Settings
  { volume :: !Float
  , silent :: !Bool
  , nickname :: !Text
  }
  deriving (Eq, Generic)

settings <- newVar (Settings 40 False "Ada")

-- In the view:
slider 0 100 settings.volume
checkbox "Silent" settings.silent
textInput settings.nickname
```

`settings.volume` is a `Var Float` projecting one field. Editing it does not repaint what reads only `settings.nickname`.

- **`constVar x`** makes a read-only `Var` whose writes are no-ops. Use it to show a derived value in an input widget and react through the primed `Response`.
- **`withValue val widget`** runs a `Var` widget against a temporary `Var` and returns both the widget's result and the final value. Mutations to the temporary do not trigger reruns or damage; `nano-ui-emit` uses this for `emitChanged`.
- **`zoomVar getter setter`** projects a `Var s` to a `Var a`. **`zoomVarL`** does the same from a lens. Both are useful for adapting a widget to a field that is computed rather than stored.
- **Lists:** `forEachVar key list body` gives per-element `Var`s that follow their keys across reorders. `withElemVars` is the variant for when you filter or reorder the list yourself and need the elements plus an `at` accessor.

## What a write costs

`readVar` records a dependency in the current pass. When a `Var` changes, nano-ui classifies the write:

| Write type | Description | Cost |
| --- | --- | --- |
| **Forward** | Written before any read this pass. | One pass; later reads see the new value. |
| **Backward** | Written after being read earlier this pass. | A second pass with one-shot input events cleared. |
| **Unchanged** | New value is `==` to the old value (pointer checked first). | No-op: no pass, no repaint. |

> [!TIP]
> **Granular repainting:** when an input widget is bound to a `Var` and nothing in the view calls `readVar` on it, only that widget's bounds are repainted on change. Calling `readVar` promotes the write to a full-window repaint.

Two habits follow directly:

- Place `readVar` **below** the widget that edits the value when the order is free. Typing then resolves in a single pass.
- Do not `readVar` a value only to hand it to the widget that edits it. Passing the `Var` is enough.

## Reading without depending, writing from threads

- **`peekVar`** reads the current value *without* recording a dependency. Use it for previous-frame comparisons and for state the view only inspects.
- **`readVarIO`, `writeVarIO`, `modifyVarIO`** are the `IO` forms, safe from worker threads and setup code. A background write does not wake the event loop on its own: call the callback from `askWake` afterwards.

```haskell
wake <- askWake          -- in the view, on the first frame
liftIO $ forM_ workers $ \w -> forkIO $ do
  result <- expensiveQuery
  writeVarIO results result
  wake                   -- schedule one frame
```

Never touch the UI `Context` from a worker thread. State, `Var`s, and `askWake` are the whole interface.

## Lifetime and resources

Some handles are not values in a `Var` at all: tasks, streams, image handles, drag handles, plot caches, and file-dialog ids. They are allocated in `IO` during setup and stored in the application record, exactly like a `newVar`:

```haskell
data App = App
  { appDocument :: Var TextDocument
  , appTask :: Task Text [Text]
  , appImages :: Var (Maybe ImageId)
  }
```

A handle allocated inside the view is a per-frame leak. If the view needs a new handle when a key changes, allocate it in setup and key the *use* of it, or use a scope that owns its lifetime (`useImageRgba`, `useTask`, `useStream`).

## Common mistakes

- **`liftIO (newVar ...)` inside a view.** Allocates a new reference on every pass; move it to setup.
- **A conditional `useVar` without `scope`.** The slot shifts when the branch runs, and later state lands in the wrong place.
- **Reading above the input that writes.** Costs a second pass per change; move the read below when possible.
- **Writing a freshly computed `NaN`.** `NaN /= NaN`, so the write looks changed on every frame and redraws forever. Guard the write.
- **A second `Var` holding a derived value.** It drifts. Compute it in the view instead.

Next: [Widget Identity & Keys](identity.md) applies these rules to widgets that come and go, and to lists that reorder.
