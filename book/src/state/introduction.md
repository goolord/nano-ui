# Introduction to State

The view runs every frame. Nothing is retained between frames except the state you keep explicitly, so the first design question in a nano-ui application is not "which widget?" but "where does this value live?"

This chapter is the map. It covers the three ways to keep state and the frame-model facts that make them behave the way they do. The direct-mutation details are in [`Var` in Depth](var.md) and [Widget Identity & Keys](identity.md); the typed-message alternative is in [Elm-Style Views](elm-overview.md).

## State you keep, state you derive

A value that changes over time and cannot be recomputed from something else belongs in a `Var`. A value that *can* be recomputed belongs in a local binding, computed fresh every frame:

```haskell
appView :: App -> NanoUI ()
appView app = do
  -- Keep: these are the input to the next frame.
  draft <- useVar ""
  settings <- readVar app.appSettings

  -- Derive: computed from what was kept, every frame.
  label ("Signed in as " <> settings.nickname)
  label ("Draft: " <> T.pack (show (T.length draft)) <> " characters")
```

Keeping a derived value in a second `Var` means every place that changes the source must also update the copy. The first place that forgets is a bug. Compute it instead.

## A `Var` is the default

A `Var a` is a typed mutable reference. Input widgets take the `Var` they display and mutate it directly; there is no callback and no on-change handler:

```haskell
counter :: NanoUI ()
counter = rowWith (gap 8 . alignMid) $ do
  n <- useVar (0 :: Int)
  whenM (button "-") (modifyVar n (subtract 1))
  label . T.pack . show =<< readVar n
  whenM (button "+") (modifyVar n (+ 1))
```

There are two ways to allocate one:

| | `useVar` | `newVar` |
| --- | --- | --- |
| Where | Inside the view | In `IO`, during setup |
| Types | `Int`, `Float`, `Double`, `Bool`, `Text`, `SortCol` (`useEnumVar` for any `Enum`) | Any type |
| Identity | The call site, counted in call order | The reference itself |
| Best for | View-local state: a draft text, an open flag, a filter | Application state: a document, a list, a settings record, a task |

`newVar` state is passed into the view, usually in an application record:

```haskell
data App = App
  { appTodos :: Var [Todo]
  , appFilter :: Var Filter
  }

newApp :: IO App
newApp = App <$> newVar [] <*> newVar All

appView :: App -> NanoUI ()
appView app = column $ do
  todos <- readVar app.appTodos
  ...
```

Passing the same `Var` to two widgets shares the state between them. Passing a record in a `Var` gives each field its own projection (`settings.volume`), tracked independently of the other fields.

## Four operations

```haskell
n <- useVar (0 :: Int)          -- allocate at this call site (first frame only)
v <- readVar n                  -- read and record a dependency
writeVar n 10                   -- replace
modifyVar n (+ 1)               -- update from the latest value
```

`modifyVar` is preferred over `writeVar n =<< (+ 1) <$> readVar n` when several changes can land in one frame: each modification is applied to the latest value, so none are lost.

`peekVar` reads without recording a dependency. Use it to compare against the previous frame when a read would otherwise force a second pass.

## What a write costs

A `readVar` records a dependency for the current pass. When the value changes, nano-ui classifies the write:

| Write | When | Cost |
| --- | --- | --- |
| Forward | Written before any read this pass | One pass; later reads see the new value. |
| Backward | Written after being read earlier this pass | A second view pass, with one-shot input events cleared. |
| Unchanged | New value `==` old value | No-op: no pass, no repaint. |

Two practical consequences:

- **Reading below the widget that edits a `Var` keeps typing to one pass.** Reading above it costs a second pass on every change.
- **A widget bound directly to a `Var` with no `readVar` anywhere repaints only that widget.** Any `readVar` promotes the write to a full-window repaint.

The second rule is why the counter above reads `n` in a `label` — the label must show the new value — while a `textInput` bound to a `Var` can update without a `readVar` if nothing else displays it.

## The two rules that prevent most bugs

1. **Never allocate a frame-lived resource inside the view.**

   ```haskell
   -- Wrong: allocates a new Var every pass.
   appView = do
     n <- liftIO (newVar 0)
     ...

   -- Right: allocate once in main and pass it in.
   main = do
     n <- newVar 0
     runSdlApp opts (appView n)
   ```

   `useVar` is the exception: it is designed to be called in the view and allocates only on the first frame at that position.

2. **Guard side effects behind events.**

   The view re-runs, and it can re-run within a single frame. Work that should happen once must be behind an event check:

   ```haskell
   whenM (button "Save") saveDocument
   ```

   `button "Save" >> saveDocument` would save on every frame. File I/O, process launches, logging, and state writes outside the UI all follow the same rule.

## Where should this value live?

| The value… | Put it in |
| --- | --- |
| Is a draft, open flag, selection, or filter used by one view | `useVar` / `useEnumVar` |
| Is a list, document, settings record, or task handle | `newVar` in `IO`, stored in the app record |
| Belongs to one row of a dynamic list | A `Var` addressed by a stable key (`withElemVars`, `VarMap`) |
| Must survive reordering or filtering | A keyed `Var`, plus `withKey` for widget state |
| Already lives outside the UI (a database handle, a worker's state) | Keep it in `IORef` or a record, wake the UI with `askWake` |
| Is a pure state machine with a clear `msg -> model -> model` | The [Elm-style reducer](elm-overview.md) in `nano-ui-emit` |

The last two are escape hatches, not the default. Most applications mix them: a `winget-gui`-style app keeps domain state in an `IORef` and its view state in `Var`s; `stronghead` keeps a large record of `Var`s; `ned` keeps its editor state in an `IORef` and re-reads it each frame.

## Reading the state in the right place

A few ordering rules matter enough to repeat here:

- **Read a `Var` below the widget that edits it** when the order is free. This keeps a change to one pass.
- **Per-row `useVar` calls belong under `withKey`.** Otherwise their positions move when a row is removed or filtered.
- **Conditional `useVar` calls belong in `scope`.** Widgets and `Var`s have separate counters, but a `useVar` that comes and goes still shifts the `useVar` slots after it.
- **Focus questions belong at the end of the view.** The node arena is reset before the view runs, so asking "is a text field focused?" at the top always answers no.

[Widget Identity & Keys](identity.md) covers the keying rules in detail.

## Next

- [`Var` in Depth](var.md) — projections, `VarMap`, `constVar`, thread-safe reads and writes.
- [Widget Identity & Keys](identity.md) — why call order matters and how to keep state attached to the right row.
- [Elm-Style Views](elm-overview.md) — a pure model and reducer instead of direct mutation.
