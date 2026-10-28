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
  emitChanged (slider 0 100) volume SetVolume
  emitWhen (button "Save") Save
```

`NanoUIE msg a` fixes the message type for a view. It is an ordinary
`NanoUI a` action with the view's message sink in scope as the implicit
parameter `?emit`, so every widget exported by `nano-ui` runs in it unchanged:
containers, controls, overlays, scopes, and widgets that take callbacks or
several bodies. `NanoUI.Emit` re-exports `NanoUI`. `emit :: msg -> NanoUIE
msg ()` needs no `Typeable` constraint: emitting a different type is a compile
error. Function-valued messages and polymorphic message types work too.

Use `mapMessages ChildMessage childView` to embed a reusable component's
message type in a parent message type. It preserves the result and sends
messages directly to the parent in order, without a nested collection queue.

- `emitWhen` emits when a widget such as `button` activates.
- `emitChanged` runs a `Var`-based widget on a temporary `Var` initialized to
  the current model value and emits only when the value differs.
- `emitEdited` additionally requires `respChanged`.

Each adapter runs its widget once per view pass.

Give views a signature, `view :: Model -> NanoUIE Msg ()`: the runners take
views of that shape, and a binding outside any view has no sink for its
`emit` without one. A widget that emits itself, passed by name to
`emitChanged`, needs a lambda (`emitChanged (\v -> myInput v) old SetValue`).

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
