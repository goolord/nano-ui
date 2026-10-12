# State

State stays with owners that know its type. NanoUI does not store arbitrary
values in `Data.Dynamic`, look them up by runtime type, or cast them back from
widget slots. `NanoUI` remains model-agnostic: Vars, reducers, and
application-owned references compose normally.

## Vars

A `Var a` is a typed, mutable cell. Inputs take the `Var` they edit: the widget
shows its value and writes the user's edits into it, so the caller has no
write-back to do.

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

A Var comes from one of three places:

- `newVar` makes a Var of any type in IO. Make it once during setup and pass it
  to the view. Two calls make two independent Vars; rendering the same
  constructed component twice shares them.
- `useVar` keeps a Var at its position in the view. It holds `Int`, `Double`,
  `Float`, `Bool`, `Text` or `SortCol`, and `useEnumVar` holds an enumeration.
  Positional Vars count apart from widget ids, with their own per-scope
  counter and key space, so a widget that comes and goes never moves them. A
  slot that changes type resets to the new initial value rather than
  reinterpreting the old one.
- A `VarMap` holds one Var per application key, for dynamic children:
  `varAt` finds or makes one, and `dropVar`/`retainVars` discard them.

`Var x`, or a string literal through the `IsString` instance, is a
constant whose writes go nowhere. `withValue value widget`
runs a widget on a temporary Var and returns its result with the value it
left; the emit and form adapters use it.

A record in a Var gives a Var for each field with `OverloadedRecordDot` and a
`Generic` instance (`settings.volume`). The path is resolved at compile time:
the inspection suite checks that a focused read or write compiles to the
record code it stands for, with no `Generic` representation or dictionaries
left. `zoomVar` and `zoomVarL` focus with your own getter and setter or a van
Laarhoven lens. `forEachVar` and `withElemVars` give a Var per element of a
`Var [a]` that follows its key.

## What a write costs

`readVar` is tracked: it notes that the current view pass observed the Var.
Binding a widget to a Var also observes it, from the widget's start. A write
that changes the value is then classified:

- A write to a Var nothing has observed yet in this pass is forwards: whatever
  reads it later sees the new value. It costs nothing extra.
- A write to a Var already observed in this pass is backwards: something was
  built from the old value. In the first pass the frame runs the view again,
  without the one-shot input it already acted on. In the second it asks for a
  follow-up frame, so a view writing backwards every frame cannot spin within
  one frame.
- A changed value repaints the window when a tracked `readVar` saw the old
  value or reads the new one later in the frame, because a view can draw
  anything from a value it read. A Var only its widget shows repaints only that
  widget.

Equality cuts writes off: equal writes do nothing. Text and documents compare
by pointer first, a `TextDocument` by pointer only, and floating-point values
treat NaN as equal to itself so a NaN cannot keep a view dirty.

Stamps are per root cell, not per field: writing `app.body` after showing
`app.title` counts as backwards. Keep values a view reads and writes in
different places in separate Vars when that matters.

`peekVar`, from `NanoUI.Var`, reads without tracking, for a value that drives an action rather
than a drawing. `readVarIO`, `writeVarIO` and `modifyVarIO` work from IO and
other threads with no bookkeeping; after writing from another thread, run the
action from `askWake` so the view runs.

Custom inputs bind with `bindVar` or `bindVarBy` from `NanoUI.Widgets.Custom`.
`RenderFinal` declares that the widget draws the value it returns. If a child
of the widget writes the Var during the widget, the child's write wins.
`Conservative` declares that it drew something from the old value before the
edit resolved, and reruns the view on change; tables, body-first tabs and pane
grids bind this way.

## Resource and extension owners

Allocate these during setup and pass the handle first:

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

Tasks, streams, and images retain their lease semantics: a changed key
replaces the resource, a skipped frame releases it, and session exit releases
everything. Two view passes stamp one lease without starting another job.
Values remain in typed handles; the registry holds cleanup callbacks and
frame stamps. Cleanup clears the handle before releasing the resource, so
remounting starts fresh. Task results survive key changes but not release.
Each independent resource needs its own handle, fixing its key/result types.

Forms capture their owner in field-view closures during evaluation. Deferred
and nested views update their original form without an ambient prefix store.

`Host a`, exposed by `NanoUI.Monad` and `NanoUI.Testing`, is an explicit
optional typed slot. Multiple slots of the same type are independent. Core
window, sensor, paragraph, and SVG state has concrete context ownership.

Positional Vars need no setup. Allocate explicitly owned state and resources
once in IO, as a record, and pass it to the view given to `runSdlApp`:
`state <- newEditor; runSdlApp opts (editorUi state)`. SDL-specific view
operations act on the open session without being handed it; `askSdlEnv`
opts into the explicit session for IO-level operations. RGFW's
`runRgfwAppWith` and `runRgfwAppReduceCustomWith` supply a typed debug sampler.
Ordinary runners serve views that need only the core API.

## Access cost

Var and host reads are O(1) direct reference accesses; a field focus adds
the record selector, and an element of a `Var [a]` adds a hash lookup unless
the list is still the one the row was built from. A tracked read or a write
also touches three stamps in a primitive array. A positional `useVar` is one
`IntMap` lookup that allocates nothing on a hit. Retained task,
stream, and image reads are O(1), plus key equality. Registration/release use
an `IntMap`; skipped-resource sweeping scans only when not all leases ran.

Concrete widget slots and keyed caches still use `IntMap`, with
O(min(n, W)) lookup for W-bit keys. Form prefixes/fields use `Map`, with
O(log n) lookup excluding text-key comparisons. User transitions, equality,
and rendering have their own costs. Not all widget storage is direct-indexed.

An unchanged immutable store snapshot skips slot-diff construction. A future
indexed widget store also needs write-time change tracking to preserve damage
snapshots efficiently, rather than merely replacing its lookup structure.

## Dependency boundary

The CommonMark integration retains a concrete map of destinations and titles
and uses CommonMark's typed `insertReference` API. CommonMark internally uses
runtime-typed extension entries; those are not NanoUI state stores. Optional
emission lives in `nano-ui-emit`: `NanoUIE msg a` captures the message type,
with no runtime-typed message queue or filtering in core.

## Migration and verification

This is a source-level API migration, with no serialized state to convert.
The positional hooks (`useInt`, `useFloat`, `useFlag`, `useEnum`, `useText`,
`useToggle`), `StateCell` with `newState`/`useState`/`modifyState`, and
`useTableSort` are gone; the nano-ui CHANGELOG maps each to its replacement.

| Before | After |
| --- | --- |
| `(t, setT) <- useText ""` then `setT =<< textInput t` | `t <- useVar ""` then `textInput t` |
| `cell <- newState x` then `(v, setV) <- useState cell` | `v <- newVar x` then `readVar v` / `writeVar v` |
| `modifyState cell f` | `modifyVar v f` |
| `useEnum x` | `useEnumVar x` |
| `useTableSort` | `useVar (SortCol 0 SortAsc)` |
| `x' <- slider 0 1 x` on a model value | `(_, x') <- withValue x (slider 0 1)` |

Tests in `Cases/Passes.hs` measure each interaction as view passes, damage
kind, and follow-up frames. The old rows are from the hook-based API, where
every changed write forced a second pass and full damage:

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

The remaining follow-up frame on a keystroke comes from the widget's own store
write, not from the Var.

Other tests cover positional independence from conditional widgets and scoped
Vars, type changes at one slot, keyed reorders, `VarMap` lifetimes, Vars
shared by two sessions, backwards writes capped at two passes, writes from IO,
and the field-focus type errors.

