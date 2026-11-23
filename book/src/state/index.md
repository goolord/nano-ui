# State & `Var`

Because a view runs fresh on every frame, any value that needs to survive from one frame to the next must be stored explicitly. In `nano-ui`, most mutable state lives in a `Var`, a typed mutable reference. Input widgets take a `Var` directly and update it in place, so you do not need to wire up `onChange` callbacks.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/State.hs:counter}}
```

## Store state, derive everything else

Keep a value in a `Var` only when it cannot be recomputed from other state. Anything you *can* calculate from existing state belongs in a regular Haskell binding computed fresh each frame:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/State.hs:keep_derive}}
```

Duplicating a derived value into a second `Var` invites bugs as soon as one code path updates the source and forgets the copy.

## Creating `Var`s

You have two main ways to create a `Var`, plus `VarMap` for dynamic keyed collections:

| | `useVar` | `newVar` |
| --- | --- | --- |
| Where you call it | Inside the view | In `IO`, during setup |
| Supported types | `Int`, `Float`, `Double`, `Bool`, `Text`, `SortCol` (`useEnumVar` handles any `Bounded`/`Enum` type) | Any type |
| Identity | Call position in the view | The reference itself |
| Good fit for | View-local state (input drafts, open flags, filters) | Shared or app-wide state (documents, lists, settings records) |

`useVar` needs no boilerplate. Its initial value is only used the first time that slot is created; to reset it later, call `writeVar`. Although `useVar` tracks its slots separately from widget IDs, a **conditional `useVar`** still needs `scope` so slots after it keep their positions:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/State.hs:conditional_use_var}}
```

`newVar` works for any Haskell type. Create it once in `IO` before starting the window loop and pass it into your view:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/State.hs:new_var}}
```

Passing the same `Var` to multiple widgets lets them share state directly. Never call `liftIO (newVar ...)` inside the view, or you will allocate a brand-new `Var` on every pass.

For collections whose items come and go, `VarMap` stores one `Var` per key:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/State.hs:var_map}}
```

`varAt` looks up the `Var` for a key (allocating one if needed), `dropVar` removes a single entry, and `retainVars keep` clears out any key that fails the predicate.

## Projections and read-only views

If a `Var` holds a record that derives `Generic`, `OverloadedRecordDot` projects individual fields into their own `Var`s:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/State.hs:projections}}
```

Here `settings.volume` is a `Var Float` targeting just that field. Dragging the volume slider will not repaint widgets that only read `settings.nickname`.

Several helpers cover other shapes:

- `constVar x` builds a read-only `Var` whose writes are no-ops. Use it when you want to display a derived value inside an input widget and handle changes through the widget's primed `Response`.
- `withValue val widget` runs a `Var`-based widget against a temporary `Var` and returns both the widget result and the updated value. `nano-ui-emit` uses this under the hood for `emitChanged`.
- `zoomVar getter setter` projects a custom slice of a larger `Var`, and `zoomVarL` does the same using a lens.
- `forEachVar key list body` gives each item of a `Var [a]` its own `Var a` keyed across reorders. Use `withElemVars` when you need to filter or sort the list yourself before drawing.

## What a write costs

Calling `readVar` records a dependency for the current frame pass. Whenever a write changes the stored value, `nano-ui` handles it in one of three ways:

| Write kind | When it happens | Cost |
| --- | --- | --- |
| **Forward** | Written before anything reads it this pass | Single pass; later reads immediately see the new value |
| **Backward** | Written after being read earlier in the same pass | Triggers a second view pass (with one-shot input events cleared) |
| **Unchanged** | New value equals (`==`) old value | No-op: no extra pass and no repaint |

Two practical habits follow from this:

- Put `readVar` **below** the widget that edits the value whenever the layout allows it. Typing or dragging then finishes in a single pass rather than running a second pass per change.
- Do not call `readVar` just to pass the value back into a widget that already takes the `Var`.

Watch out for floating-point `NaN`: since `NaN /= NaN`, writing a freshly computed `NaN` every frame looks like a new value each time and keeps the window redrawing endlessly. Guard float calculations that can produce `NaN`.

> [!TIP]
> Updating a `Var` only repaints the widgets whose text, value, or style actually changed, whether they are bound to the `Var` directly or render something computed from `readVar`.

If you need to inspect a `Var` without recording a read dependency, use `peekVar`. From worker threads or plain `IO`, use the thread-safe functions `readVarIO`, `writeVarIO`, and `modifyVarIO`, then invoke the wake callback from `askWake` to schedule a frame. Never touch the UI `Context` from a background thread.

## Lifetimes and external handles

Tasks, streams, image handles, drag handles, plot caches, and file-dialog IDs follow the same rule as `newVar`: create them once in `IO` during setup and keep them in your application state record.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/State.hs:handles}}
```

Allocating any of these inside the view leaks resources on every frame. When a resource needs to track dynamic keys, either allocate the handle at setup and key its hook (`useTask`, `useStream`, `useImageRgba`), or let a `VarMap` manage it.

## Where should a piece of state live?

| If the value... | Store it in |
| --- | --- |
| Is a local draft, toggle, selection, or filter for one view | `useVar` or `useEnumVar` |
| Is a list, document, settings record, or async handle | `newVar` in `IO`, held in your app record |
| Belongs to a single row in a dynamic list | A keyed `Var` (`forEachVar`, `withElemVars`, or `VarMap`) |
| Already lives in an external store or worker | An `IORef` or app record, waking the UI with `askWake` |
| Fits a pure `msg -> model -> model` state machine | An [Elm-style reducer](elm.md) |

Next: read [Widget Identity & Keys](identity.md) to see how keyed lists and conditional UI preserve widget state, or check out [Elm-Style State](elm.md) for the reducer alternative.
