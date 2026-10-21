# State

State in `nano-ui` is statically typed through explicit handles. Values are not
stored in `Data.Dynamic`, looked up by runtime type representation, or cast out
of untyped widget slots. `NanoUI` works the same way whether state is kept in
`Var`s, an Elm-style reducer, or application-owned references.

## Vars

A `Var a` is a typed mutable reference. Input widgets take the `Var` to read
and update directly, so callers do not need to write the edited value back
manually.

```haskell
newCounter :: IO (NanoUI ())
newCounter = do
  count <- newVar (0 :: Int)
  pure $ row $ do
    whenM (button "-") (modifyVar count (subtract 1))
    label . T.pack . show =<< readVar count
    whenM (button "+") (modifyVar count (+ 1))
    whenM (button "Reset") (writeVar count 0)
```

A `Var` can be created in three ways:

- `newVar` allocates a `Var` of any type in `IO`. Allocate it during setup and
  pass it into your UI action. Two calls to `newVar` create independent
  references; passing the same `Var` to two widgets shares the underlying
  state.
- `useVar` allocates a positional `Var` inside `NanoUI` for `Int`, `Double`,
  `Float`, `Bool`, `Text`, or `SortCol` (and `useEnumVar` for `Enum` types).
  Positional `Var`s use a per-scope counter and key space separate from widget
  IDs, so conditional widgets do not shift `useVar` slots. If a slot's type
  changes across frames, it resets to the new initial value.
- `VarMap` stores one `Var` per application key for dynamic collections:
  `varAt` looks up or creates the `Var` for a key, and `dropVar` / `retainVars`
  remove entries.

`constVar x` creates a read-only `Var` where writes do nothing, which is useful
when displaying a derived value in an input widget. There is no `IsString`
instance for `Var`, so passing a string literal where a `Var Text` is expected
is a compile-time type error rather than a silently dropped edit.
`withValue value widget` runs a widget against a temporary `Var` initialized to
`value` and returns both the widget's result and the final value; the emit and
form adapters are built on this helper.

When a `Var` holds a record with a `Generic` instance, `OverloadedRecordDot`
projects a `Var` for any field (e.g. `settings.volume`). Field projections are
resolved at compile time: the inspection test suite checks that focused reads
and writes compile down to direct record selectors and updates with no residual
`Generic` representations or typeclass dictionaries. For custom projections, use
`zoomVar` (getter/setter pair) or `zoomVarL` (van Laarhoven lens). For lists in
a `Var [a]`, `forEachVar` and `withElemVars` provide a per-element `Var` keyed
by element identity.

## What a write costs

`readVar` records that the current pass read from the `Var` (as does binding an
input widget to a `Var`). When a write changes the value of a `Var`, its cost
depends on whether the `Var` was already read during the current pass:

- A write to a `Var` not yet read in the current pass is a forward write: later
  reads in the same pass return the new value, with no extra pass required.
- A write to a `Var` already read earlier in the current pass is a backward
  write: earlier widgets were built using the old value. On the first pass of a
  frame, the UI action is re-run once with one-shot input events cleared so
  clicks or key presses are not triggered twice. If a backward write occurs
  again on the second pass, a follow-up frame is scheduled instead of looping
  within the same frame.
- If a changed `Var` was read via `readVar` (before or after the write), the
  full window is marked damaged because arbitrary layout or drawing may depend
  on the value. If a `Var` is only bound to an input widget and never read with
  `readVar`, only that widget's bounds are marked damaged.

Writes that do not change the value are no-ops. A write compares the new value
with the old one by pointer first and then with `==`: writing back the same
`Text` compares no characters, and `TextDocument`'s `==` compares only the
lines two documents do not share. Text areas compare their documents by
`sameDocument`. The built-in float inputs (`slider`, `knob` and
`numericInput`) treat a `NaN` as equal to itself, but `writeVar` of a newly
computed `NaN` is a change every time, since `NaN /= NaN`: guard such writes,
or a view that writes one after reading the `Var` redraws continuously.

Read/write tracking operates at up to two levels of sub-part granularity (such
as a record field or list element, plus a nested field within it). Writing
`app.body` after reading `app.title` is not treated as a backward write;
similarly, mutating a row's element inside `forEachVar` does not invalidate
preceding rows because `forEachVar` only reads the list's keys and order. Each
sub-part maps to a bit in a fixed-width mask, so colliding hashes conservatively
share tracking state. Reading the entire `Var`, projecting through `zoomVar`, or
using `withElemVars` marks the whole `Var` as read; changing an element's key in
`forEachVar` marks the whole list as written.

`readVar` can be used normally inside event handlers: reading and then updating
a `Var` costs at most one additional pass on the frame the event occurred. To
read a `Var` without recording a dependency (for example, to compare the
previous frame's value against the current frame's without being affected by a
second pass), use `peekVar` from `NanoUI.Var`. From `IO` or background threads,
use `readVarIO`, `writeVarIO`, and `modifyVarIO`, then invoke the callback from
`askWake` to wake the event loop.

Custom input widgets bind to a `Var` using `bindVar` or `bindVarBy` from
`NanoUI.Widgets.Custom`:

- `RenderFinal` indicates that the widget already rendered the updated value
  returned at the end of the call (if a child widget also wrote to the same
  `Var` during the call, the child's write takes precedence).
- `Conservative` indicates that the widget rendered using the prior value
  before resolving the edit, so a change triggers a second pass (used by
  tables, body-first tabs, and pane grids).

## Resource and extension handles

Stateful resources are allocated once during setup (typically in `IO`) and
passed into the UI action:

| Allocation | Per-frame use |
| --- | --- |
| `newTask` | `useTask task key action`, `useTaskStatus task key action` |
| `newStream` | `useStream stream key initial producer` |
| `newImageHandle` | `useImageRgba owner key width height pixels` |
| `newDrag` | `useDrag owner sources` |
| `newFormState` | `nanoFormLive forms prefix form`, other runners, `resetForm forms prefix` |
| `newPlotCache` | `plot cache layout chart`, single-series chart helpers |
| `newMarkdownCache` | `markdown cache doc`, `markdownConfigured cache config doc` |
| `newHost` | `setHost host value`, `askHost host`, compact-host helpers |

Tasks, streams, and scoped images use frame-lease semantics: changing the key
replaces the active resource, skipping the call for a full frame releases it,
and exiting the session releases all active leases. If a frame runs two view
passes, both passes stamp the same lease without restarting the task. Resource
values are stored directly in their typed handles while the frame context only
tracks cleanup callbacks and lease timestamps. Cleanup clears the handle before
running the finalizer so remounting starts cleanly. Task results are retained
across key changes until the lease is released. Each concurrent resource
instance requires its own handle, which fixes its key and result types.

Forms capture their `FormState` handle in field closures when constructed, so
deferred or nested form fields update the form without relying on an ambient
prefix map.

`Host a` (exported by `NanoUI.Monad` and `NanoUI.Testing`) provides an explicit
typed slot in the context; separate `Host a` values are independent even when
they share the same type `a`. Core subsystems (window state, visibility
sensors, paragraph layout caches, and SVG caches) use dedicated fields on the
context record.

Positional `useVar` calls require no external setup. For application-owned
state and resource handles, allocate them in a record in `IO` before starting
the backend runner:

```haskell
state <- newEditor
runSdlApp opts (editorUi state)
```

For SDL-specific `IO` operations, use `askSdlEnv` to obtain the session
environment. On RGFW, `askRgfwDebug` finds the session's debug sampler the
same way.

## Access cost

- Reading a `Var` or `Host` is an O(1) reference read. Projecting a record
  field adds a selector function, and indexing into a `Var [a]` adds a hash
  lookup when the list has changed since the row was constructed. Tracked reads
  and writes also update three bitmasks in a primitive array.
- Positional `useVar` performs a single `IntMap` lookup with zero heap
  allocation on a cache hit.
- Retained task, stream, and image lookups are O(1) plus a key equality
  check. Lease registration and release use an `IntMap`, and the end-of-frame
  sweep only scans when the active lease count indicates that a lease was
  skipped.
- Internal widget state slots and keyed caches use `IntMap` (O(min(n, W))).
  Form field maps use `Map` (O(log n) plus key comparisons).
- If the immutable widget store is unchanged at the end of a frame, slot-diff
  construction is skipped.

## Dependency boundaries

- The CommonMark integration in `nano-ui-markdown` keeps its own typed map of
  link destinations and titles via CommonMark's `insertReference` API rather
  than NanoUI state stores.
- Message emission is isolated in the `nano-ui-emit` package (`NanoUIE msg a`),
  so the core package has no untyped message queue.

## Migration and verification

The hook-based state API (`useInt`, `useFloat`, `useFlag`, `useEnum`,
`useText`, `useToggle`, `StateCell` / `newState` / `useState` / `modifyState`,
and `useTableSort`) was replaced by `Var`:

| Before | After |
| --- | --- |
| `(t, setT) <- useText ""` then `setT =<< textInput t` | `t <- useVar ""` then `textInput t` |
| `cell <- newState x` then `(v, setV) <- useState cell` | `v <- newVar x` then `readVar v` / `writeVar v` |
| `modifyState cell f` | `modifyVar v f` |
| `useEnum x` | `useEnumVar x` |
| `useTableSort` | `useVar (SortCol 0 SortAsc)` |
| `x' <- slider 0 1 x` on a model value | `(_, x') <- withValue x (slider 0 1)` |

Integration tests in `Cases/Passes.hs` verify the number of view passes, damage
region (`clip`, `full`, or `none`), and follow-up frames for common
interactions (compared against the older hook API, where any state change forced
a second pass and full-window damage):

| Interaction | Hooks | Vars |
| --- | --- | --- |
| Keystroke, nothing else reads the field | 2, full, 1 | 1, clip, 1 |
| Keystroke, label below reads it | 2, full, 1 | 1, full, 1 |
| Keystroke, label above reads it | 2, full, 1 | 2, full, 1 |
| Keystroke, field value kept in an `IORef` | 1, clip, 1 | 1, clip, 1 |
| Slider drag frame or checkbox click | 2, full, 1 | 1, clip, 1 |
| Button, count displayed above | 2, full, 1 | 2, full, 0 |
| Button, count displayed below | 2, full, 1 | 1, full, 0 |
| Equal writes while idle | not measured | 1, none, 0 |

When storing a multi-field record in a single `Var`, sub-part tracking avoids
unnecessary passes and full-window redraws when unrelated fields or rows are
edited:

| Interaction | Without sub-part tracking | With sub-part tracking |
| --- | --- | --- |
| Slider drag, another field shown above | 2, full, 1 | 1, clip, 1 |
| Slider drag, another field shown below | 1, full, 1 | 1, clip, 1 |
| Keystroke, another field shown above | 2, full, 1 | 1, clip, 1 |
| Keystroke, the whole model read above | 2, full, 1 | 2, full, 1 |
| Checkbox in row 3 of 20, `forEachVar`, name read after it | 2, full, 1 | 1, clip, 1 |
| The same with `withElemVars` | 2, full, 1 | 2, full, 1 |

The remaining follow-up frame on a keystroke is triggered by the text input's
internal store update rather than the `Var` write.

Additional tests in the suite cover positional stability across conditional
widgets and `scope`, slot type changes, keyed list reordering, `VarMap`
lifetimes, sharing a `Var` across two window sessions, the two-pass cap on
backward writes, and `IO` writes from background threads.

