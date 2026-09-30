# Typed state ownership

State stays with owners that know its type. NanoUI no longer stores arbitrary
values in `Data.Dynamic`, looks them up by runtime type, or casts them back
from widget slots. `NanoUI` remains model-agnostic: controlled widgets,
reducers, and application-owned references compose normally.

## Component setup and rendering

Allocate custom state once, then return the per-frame view:

```haskell
newCounter :: IO (NanoUI ())
newCounter = do
  count <- newState (0 :: Int)
  pure $ row $ do
    (n, setN) <- useState count
    whenM (button "-") (modifyState count (subtract 1))
    label (T.pack (show n))
    whenM (button "+") (modifyState count (+ 1))
    whenM (button "Reset") (setN 0)
```

`newState` fixes the initial value at construction. `useState` takes a
`StateCell a`, requires only `Eq a`, and consumes no widget id. Its setter
compares with the latest value; `modifyState` applies a pure transition to
that latest value. Equal writes do not invalidate or wake the view.

A changed cell advances the generation used for the second view pass and
queues damage for this frame. One-shot input is stripped from that second
pass. Because any part of the view may read a cell, a change currently
requests full damage, separately from widget-local targeted damage.

Construct twice for independent state. Rendering the same constructed
component twice shares its cells. Hiding a view retains its state while its
owner is retained. For dynamic children, keep typed handles in a map keyed
by application identity, allocating on insertion and dropping them when
their state should be discarded. Use cells on the UI thread in one session.

Positional `useInt`, `useFloat`, `useFlag`, `useEnum`, `useText`, and
`useToggle` use concrete typed widget slots. Their identity follows scopes
and stable call order. Use `StateCell Int`, `StateCell Text`, etc. for explicit
ownership and direct access.

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

`runSdlAppWith` takes an `SdlEnv -> NanoUI ()` view and passes only the session
environment. Primitive hooks need no setup. Capture any explicitly owned
state or resources in an ordinary closure, constructing them once in IO
before calling the runner. SDL-specific operations take the environment
explicitly. RGFW's
`runRgfwAppWith` and `runRgfwAppReduceCustomWith` supply a typed debug sampler.
Ordinary runners serve views that need only the core API.

## Access cost

Cell and host reads are O(1) direct reference accesses. Retained task,
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
All in-repository consumers use the new owners. Rollback means using the
pre-migration revision and its matching application sources; typed handles
are not compatible with the old implicit-slot API.

Tests cover equal-write idleness, retained setters, successive updates,
independent cells, hiding/reordering, second-pass input, task retries and
cancellation, shutdown, image leases, deferred forms, and editor reset.

Core profiling compared `a980e250` against this branch on Windows, GHC 9.14.1,
Cabal `-O1`, with seven alternating baseline/candidate runs using
`scripts/profile/compare-builds.py --suite core`. Median changes after adding
the unchanged-store fast path:

| Scene | Mutator wall time | Total allocated bytes |
| --- | ---: | ---: |
| widgets | +0.41% | -0.18% |
| canvas | +0.43% | -0.11% |
| canvas-keyed | +0.35% | -2.98% |
| textarea | -0.20% | +1.93% |
| svg | -1.45% | approximately unchanged |

These are whole-scene measurements, not isolated cell timings. Small timing
differences do not establish a speedup. The text-area scene allocates modestly
more; direct handles remove runtime recovery without replacing every keyed
widget store with an array.
