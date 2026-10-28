# nano-ui-emit

Typed messages and reducers for [nano-ui](https://github.com/goolord/nano-ui).
Add `nano-ui-emit` to your Cabal dependencies and import `NanoUI.Emit`.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import NanoUI.Emit

data Msg = Save | SetVolume Float

view :: Float -> NanoUIE Msg ()
view volume = column $ do
  label "Settings"
  emitChangedE (slider 0 100) volume SetVolume
  emitWhenE (button "Save") Save
```

`NanoUIE msg a` fixes the message type for a view. `emit :: msg -> NanoUIE msg ()`
needs no `Typeable` constraint: emitting a different type is a compile error.
Function-valued messages and polymorphic message types work too.

Use `mapMessages ChildMessage childView` to embed a reusable component's
message type in a parent message type. It preserves the result and sends
messages directly to the parent in order, without a nested collection queue.

Every widget exported by `nano-ui` is wrapped and re-exported by `NanoUI.Emit`
(via `NanoUI.Emit.Widgets`) to run directly in `NanoUIE msg`: containers
(`column`, `row`, `grid`, `panel`, `scroll`, etc.), text (`label`, `heading`,
`mono`, etc.), controls (`button`, `checkbox`, `slider`, `textInput`, `select`,
etc.), overlays (`modal`, `popup`, `window`, `tooltip`), and scopes (`withKey`,
`styled`, `disabledWhen`, etc.). Views write ordinary `do`-notation directly in
`NanoUIE` without manual widget lifting.

- `emitWhenE` emits on activation of a wrapped widget (`button`, etc.), while
  `emitWhen` works on an unwrapped `NanoUI Bool` action.
- `emitChangedE` runs a wrapped `Var`-based widget on a temporary `Var` initialized
  to the current model value and emits only when the value differs. `emitChanged`
  is the equivalent for an unwrapped `Var a -> NanoUI r` widget.
- `emitEditedE` and `emitEdited` additionally require `respChanged`.
- `liftNanoUI` and `withNanoUI` remain available for custom `NanoUI` actions or
  third-party container combinators.
- `withRunInNanoUIE` supplies a runner for widgets taking callbacks or multiple
  untyped bodies:

  ```haskell
  view volume = withRunInNanoUIE $ \run -> customContainer $ do
    run (label "Settings")
    run (emitWhenE (button "Save") Save)
  ```

`emitWhen` emits on activation, `emitChanged` only when the returned value
differs, and `emitEdited` additionally requires `respChanged`. Each adapter
runs its ordinary widget once per view pass.

## Running

The SDL and RGFW reducer runners accept `model -> NanoUIE msg ()`, paired
with an update function `msg -> model -> model`.

For headless frames, `runFrameE` returns `(result, [msg], drawData, dirty)`.
`runFrameReduce` returns `(result, model, [msg], drawData, dirty)` and requests
another frame when the reduced model changes. Drawing uses the model from
before reduction, as with the backend runners. `runClickReduce` drives a
press/release pair in tests.

Messages are collected in emission order across all passes of a frame,
including local-state rebuilds. Each invocation owns its queue; exceptions
cannot leave messages for the next frame. Guard event-driven emissions with
an event, since unconditional emissions run on every pass.

`runNanoUIE` collects messages from a single `NanoUI` action. For a complete
frame, use `runFrameE` rather than putting `runNanoUIE` inside `runFrame`:
the latter only returns the final pass's messages if the view rebuilds.

## Migrating from core emission

- Add the `nano-ui-emit` dependency; `NanoUI.Emit` now lives here.
- Change emitting views from `NanoUI a` to `NanoUIE Msg a`, lift ordinary
  actions, and wrap containers as above.
- Import `emit`, `runFrameReduce`, `runClickReduce`, `reduceMessages`, and
  `reduceUpdates` from `NanoUI.Emit`.
- `FrameMsg` and `decodeMessages` are gone. Messages are already typed; no
  messages are silently ignored by runtime type.
- Core `runFrame`, `runFrameEff`, and `run2Frames` now return
  `(result, drawData, dirty)`. Use `runFrameE` for a typed message list.
- The old runtime-typed `runFrameReduceEff` has been removed. Ordinary
  effectful views still use `NanoUI.Effectful` and `runFrameEff`.

## Documentation

- [The nano-ui Book](https://goolord.github.io/nano-ui/index.html): the comprehensive
  online guide, tutorials, and Elm-style state documentation.

## Verification

Run `cabal test nano-ui-emit-test`. The suite compiles a view that emits a
`Bool` from an `Int` view with its type error deferred, and checks that the
error fires with an `Int`/`Bool` mismatch.
