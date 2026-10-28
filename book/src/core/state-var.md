# State Management & `Var`

State in `nano-ui` is statically typed through explicit handles. Values are not stored in `Data.Dynamic`, looked up by runtime type representations, or cast out of untyped widget slots. `NanoUI` works identically whether state is managed via `Var`s, an Elm-style reducer (`nano-ui-emit`), or application-owned references (`IORef`).

## The `Var a` Reference

A `Var a` is a typed mutable reference. Input widgets take the `Var` they display and mutate directly:

```haskell
counter :: NanoUI ()
counter = row $ do
  n <- useVar (0 :: Int)
  whenM (button "-") (modifyVar n (subtract 1))
  label . T.pack . show =<< readVar n
  whenM (button "+") (modifyVar n (+ 1))
```

### Allocating `Var`s

There are three ways to allocate a `Var`:

1. **`useVar initial`**: Allocates a positional `Var` inside `NanoUI` for `Int`, `Double`, `Float`, `Bool`, `Text`, or `SortCol` (`useEnumVar` for custom `Enum` types). Positional `Var`s use a per-scope counter and key space separate from widget IDs, so conditional widgets do not shift `useVar` slots.
2. **`newVar initial`**: Allocates an arbitrary `Var a` in `IO`. Allocate it during setup and store it in an application environment record passed into the view. Passing the same `Var` to multiple widgets naturally shares state.
3. **`VarMap`**: One `Var` per key for dynamic collections (chat logs, file trees, dynamic rows). Use `varAt` to retrieve or create a `Var`, and `dropVar` or `retainVars` to clean up expired entries.

### Read-Only and Projected `Var`s

- **`constVar x`**: A read-only `Var` where writes are no-ops. Ideal for displaying derived values in input widgets and reacting via the primed `Response`.
- **`withValue val widget`**: Runs a widget against a temporary `Var` initialized to `val`, returning both the widget result and the updated value.
- **Record Projections (`OverloadedRecordDot`)**: With `GHC.Generics.Generic`, `settings.volume` projects a `Var Float` for that specific field. Writes to `settings.volume` do not invalidate widgets reading `settings.title`!
- **Lenses & Zooming**: Use `zoomVar` (getter/setter) or `zoomVarL` (lens) for custom projections.
- **Lists**: `forEachVar key list body` provides per-element `Var`s that track keys across reorders.

---

## What a Write Costs

`readVar` records a dependency in the current pass. When a `Var` changes, nano-ui classifies the write:

| Write Type | Description | Cost |
| --- | --- | --- |
| **Forward Write** | Written *before* any reads in the current pass. | Single pass. Subsequent reads get the new value immediately. |
| **Backward Write** | Written *after* being read earlier in the same pass. | Triggers a second pass with one-shot events cleared. |
| **Unchanged Write** | New value is `==` to old value (pointer checked first). | No-op. Zero passes, zero repaints. |

> [!TIP]
> **Granular Repainting**: When an input widget is bound to a `Var` without any `readVar` calls anywhere in the view tree, only that individual widget's bounds are repainted on change! Calling `readVar` promotes the write to a full-window repaint.

### Peek and Thread-Safe Operations

- **`peekVar`**: Reads the value of a `Var` *without* recording a dependency (useful for comparing previous vs current frame without triggering second passes).
- **Background Threads**: Use `readVarIO`, `writeVarIO`, and `modifyVarIO` from external worker threads, then signal the UI to repaint by calling the callback obtained from `askWake`.
