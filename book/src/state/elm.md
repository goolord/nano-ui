# Elm-Style State

If you prefer unidirectional data flow over direct `Var` mutation, `nano-ui-emit` provides an Elm-style model: your view emits typed messages, and a pure `update` function folds them into the next model.

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Elm.hs:full_app}}
```

Everything else in the toolkit (widgets, layout modifiers, focus, and keys) works the exact same way. Only state management changes.

## Emitting messages

`NanoUIE msg a` is a UI action that can only emit values of type `msg`. There is no `Typeable` constraint and no runtime casting, so emitting the wrong message type is caught at compile time.

Under the hood, `NanoUIE msg a` is just a `NanoUI a` action with a message sink in scope (via the implicit parameter `?emit`). That means every core widget runs inside it without wrappers, and `NanoUI.Emit` re-exports all of `NanoUI` alongside the emission helpers below. Calling `emit` inside a container, modal, tab body, or menu callback sends messages straight to the enclosing view's sink.

| Function | When it emits |
| --- | --- |
| `emit msg` | Immediately |
| `emitWhen widget msg` | When `widget` returns `True` (such as a button click or submitted field) |
| `emitChanged widget old toMsg` | When a `Var a -> NanoUI r` widget produces a value different from `old` |
| `emitEdited widget old toMsg` | When the value changed **and** `respChanged` is `True` |

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Elm.hs:emit}}
```

`emitChanged` runs a widget against a temporary `Var` initialized with `old` and emits `toMsg newVal` whenever the value changes. `emitEdited` adds a `respChanged` check for text fields so programmatic updates are not mistaken for user edits.

Always give views an explicit type signature mentioning their message type, such as `view :: Model -> NanoUIE Msg ()`. The backend runners expect that shape, and any top-level helper that calls `emit` needs the signature too so GHC knows the sink is in scope.

> [!CAUTION]
> Only call `emit` inside an event check. A bare `emit Save` at the top level of a view will emit on every frame pass and run your reducer nonstop. The `emitWhen`, `emitChanged`, and `emitEdited` adapters already guard emission for you.

Messages emitted across all passes of a frame (including local-`Var` second passes) are collected in order and folded at the end of the frame.

## Composing components

Use `mapMessages` to embed a child view with its own smaller message type inside a parent view:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Elm.hs:components}}
```

Messages flow directly to the parent's sink in traversal order with no intermediate queue. Because `mapMessages` only translates message types and leaves widget IDs untouched, dynamic lists of child components still need `withKey` around each item, just like `Var`-based rows.

You can still use local `useVar` state inside a component for transient UI flags (like a hover dropdown or draft text) while keeping durable application state in the `NanoUIE` model.

## Running and testing

`runSdlAppReduce` and `runRgfwAppReduce` wire an update function, initial model, and view into a window loop:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Elm.hs:run}}
```

On each frame, the view renders the current model and messages are reduced once the view finishes. That means the frame currently being drawn reflects the state *before* reduction; if the model changed, the runner automatically schedules a follow-up frame to draw the updated state.

For headless testing and custom loops, `nano-ui-emit` provides:

| Function | Return type |
| --- | --- |
| `runNanoUIE view` | `NanoUI (result, [msg])` |
| `runFrameE ctx input view` | `IO (result, [msg], DrawData, Bool)` |
| `runFrameReduce update ctx input model view` | `IO (result, model, [msg], DrawData, Bool)` |
| `runClickReduce update ctx input model view pos` | `IO (model, [msg], Bool)` |

`runFrameReduce` flags a follow-up frame whenever reduction changes the model. `reduceMessages` and `reduceUpdates` are plain helpers for folding message lists in unit tests.

## Choosing between `Var` and Elm-style

| | `Var`-first | Elm-style (`nano-ui-emit`) |
| --- | --- | --- |
| Where state lives | Spread across the view and an app record | One unified model type |
| State updates | Direct mutation | Explicit `Msg` values and a central `update` function |
| Testing | Widget/frame tests via `NanoUI.Testing` | Pure unit tests on `update`, plus frame tests |
| Transient UI state | `useVar` naturally fits everywhere | Still use `useVar` for open flags and drafts |
| Strongest fit | Desktop tools and editors with external I/O | State machines, wizards, undoable workflows, clean single-model apps |

You do not have to pick one exclusively. An Elm-style view can keep transient toggles in `useVar`, and a `Var`-based app can embed a self-contained Elm component using `runNanoUIE` and react to the messages it returns.

Keep `update` pure: run file or network I/O inside an event branch in the view, or queue effects to run outside the reducer. Avoid mirroring model fields into `Var`s just to read them a frame sooner, since keeping durable state in one place is the whole benefit of the reducer pattern.
