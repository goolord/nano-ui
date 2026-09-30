# nano-ui-emit

Typed messages and reducers for [nano-ui](../nano-ui). Add `nano-ui-emit` to
your Cabal dependencies and import `NanoUI.Emit`.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import NanoUI
import NanoUI.Emit qualified as Emit

data Msg = Save | SetVolume Float

view :: Float -> Emit.NanoUIE Msg ()
view volume = Emit.withNanoUI column $ do
  Emit.liftNanoUI (label "Settings")
  Emit.emitChanged (slider 0 100) volume SetVolume
  Emit.emitWhen (button "Save") Save
```

`NanoUIE msg a` fixes the message type for a view. `emit :: msg -> NanoUIE msg ()`
needs no `Typeable` constraint: emitting a different type is a compile error.
Function-valued messages and polymorphic message types work too.

Use `mapMessages ChildMessage childView` to embed a reusable component's
message type in a parent message type. It preserves the result and sends
messages directly to the parent in order, without a nested collection queue.

- `liftNanoUI` runs an ordinary widget or UI operation.
- `withNanoUI` wraps a typed body in an ordinary layout or scope, such as
  `column`, `rowWith layout`, `withKey key`, or `disabledWhen True`.
- `withRunInNanoUIE` supplies a function for running typed actions inside
  ordinary widgets with callbacks or multiple bodies:

  ```haskell
  view volume = Emit.withRunInNanoUIE $ \run -> column $ do
    label "Settings"
    run (Emit.emitChanged (slider 0 100) volume SetVolume)
    run (Emit.emitWhen (button "Save") Save)
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

## Verification

Run `cabal test nano-ui-emit-test`. The negative typechecking fixture is
checked from the repository root with:

```sh
cabal exec -- ghc -fno-code -package nano-ui-emit packages/nano-ui-emit/test/compile-fail/WrongMessage.hs
```

This command must fail with an `Int`/`Bool` mismatch at `emit True`.
