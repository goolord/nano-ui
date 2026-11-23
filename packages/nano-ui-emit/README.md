# nano-ui-emit

Statically typed message emission and Elm-style reducers for [nano-ui](https://github.com/goolord/nano-ui). Add `nano-ui-emit` to your `build-depends` and import `NanoUI.Emit`.

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

`NanoUIE msg a` is a regular `NanoUI a` action with a typed message sink in scope via the implicit parameter `?emit`. Because `NanoUI.Emit` re-exports all of `NanoUI`, every built-in container, control, overlay, and scope runs inside `NanoUIE` unchanged. `emit :: msg -> NanoUIE msg ()` requires no `Typeable` constraint, so emitting a mismatched message type fails at compile time, and polymorphic or function-valued messages work out of the box.

Embed reusable child components using `mapMessages ChildMsg childView`, which forwards messages directly to the parent's sink in traversal order without allocating an intermediate queue.

Widget adapters cover the common interaction cases:

- `emitWhen widget msg`: Emits `msg` when a `NanoUI Bool` control (such as `button`) activates.
- `emitChanged widget old toMsg`: Runs a `Var`-based widget on a temporary `Var` seeded with `old`, emitting `toMsg new` whenever the resulting value differs.
- `emitEdited widget old toMsg`: Like `emitChanged`, but also checks `respChanged` so programmatic updates are not mistaken for user edits.

Always annotate top-level views and helpers that call `emit` with their explicit `NanoUIE Msg a` signature so GHC knows the message sink is in scope. When passing a widget that itself emits into `emitChanged`, eta-expand it (`emitChanged (\v -> myInput v) old SetValue`).

## Running and testing

Desktop reducer runners (`runSdlAppReduce`, `runRgfwAppReduce`, `runSdlWebAppReduce`) pair a `model -> NanoUIE msg ()` view with a pure update function `msg -> model -> model`.

For headless tests and custom loops:

- `runFrameE` runs one frame and returns `(result, [msg], drawData, dirty)`.
- `runFrameReduce` folds emitted messages into the model, returning `(result, model, [msg], drawData, dirty)` and flagging another frame whenever the model changes. (Just like desktop runners, `drawData` reflects the model before reduction.)
- `runClickReduce` drives a press/release click pair in tests.

Messages emitted across all passes of a frame (including local-`Var` second passes) are collected in order, and exceptions cannot leak leftover messages into the next frame. Never call `emit` unconditionally at the top level of a view, or it will fire on every pass.

`runNanoUIE` collects messages from a single `NanoUI` action. For full frames, prefer `runFrameE` over nesting `runNanoUIE` inside `runFrame`, since the latter would drop messages from an earlier pass if the view runs twice.

## Migrating from older core emission

- Add `nano-ui-emit` to `build-depends` and import `emit`, `runFrameReduce`, `runClickReduce`, `reduceMessages`, and `reduceUpdates` from `NanoUI.Emit`.
- Change emitting view signatures from `NanoUI a` to `NanoUIE Msg a`.
- `FrameMsg` and `decodeMessages` are gone because messages are statically typed.
- Core `runFrame`, `runFrameEff`, and `run2Frames` return `(result, drawData, dirty)`; use `runFrameE` when you need emitted messages.
- The old runtime-typed `runFrameReduceEff` has been removed. Effectful views continue to use `NanoUI.Effectful` and `runFrameEff`.

## Verification

Run `cabal test nano-ui-emit-test`. Among other checks, the suite compiles a view emitting `Bool` inside an `Int` view using deferred type errors and verifies that GHC reports the type mismatch.

## Documentation

- [The nano-ui Book: Elm-Style State](https://goolord.github.io/nano-ui/book/state/elm.html): Patterns for combining `NanoUIE` with local `Var` state and testing reducers.
