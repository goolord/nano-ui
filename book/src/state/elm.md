# Elm-Style State

`nano-ui-emit` is the reducer-based alternative to `Var`: instead of mutating state directly, the view emits typed messages, and a pure reducer folds them into a model.

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Elm.hs:full_app}}
```

Widgets, layout, focus, and keys are unchanged. Only the state model differs.

## Emitting

`NanoUIE msg a` is a UI action that can emit only `msg`. There is no `Typeable` constraint and no runtime type test: emitting the wrong type is a compile error.

A `NanoUIE` view is an ordinary `NanoUI` action with the view's message sink in scope (an implicit parameter, `?emit`). Every core widget runs in it unchanged, so `NanoUI.Emit` re-exports `NanoUI` and adds the functions below. An `emit` inside a container, an overlay body, a tab or a menu callback goes to the sink of the view around it.

| Function | Emits when |
| --- | --- |
| `emit msg` | Immediately |
| `emitWhen widget msg` | The widget returns `True` (button click, submitted field) |
| `emitChanged widget old toMsg` | A `Var a -> NanoUI r` widget's final value differs from `old` |
| `emitEdited widget old toMsg` | The widget's value changed **and** `respChanged` is true |

```haskell
column $ do
  label "Settings"
  withKey "form" $ do
    emitChanged (slider 0 100) volume SetVolume
    emitWhen (button "Save") Save
```

`emitChanged` runs the widget against a temporary `Var` seeded with the model value and emits the new value only if it differs. `emitEdited` is its stricter form for text fields, where programmatic writes should not look like edits.

Give each view a signature in its own message type, `view :: Model -> NanoUIE Msg ()`. The runners take a view of that shape, and a local helper defined outside a view needs the signature too, or GHC finds no sink for its `emit`.

> [!CAUTION]
> Emit only inside an event branch. `emit Save` at the top level of a view emits on every pass, and the reducer runs every message. The adapters above already guard on the widget's event.

Messages are collected across every pass of a frame, including local-`Var` rebuilds, and reduced at frame end in emission order.

## Components

`mapMessages` embeds a child view whose message type is smaller than its parent's:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Elm.hs:components}}
```

Messages go straight to the parent's sink in traversal order, with no intermediate queue. `mapMessages` changes the message type, not widget identity: a list still needs `withKey` around each component, exactly as a `Var`-based row does.

For local UI state, keep using `Var` inside the component; the durable model stays in `NanoUIE`.

## Running and testing

`runSdlAppReduce` and `runRgfwAppReduce` take an update function, an initial model, and a view:

```haskell
runSdlAppReduce defaultSdlOptions update initialModel view
```

The view runs with the current model, messages are reduced at frame end, and the drawing shown for that frame is from **before** the reduction. If the model changed, another frame is requested to draw it. That follow-up frame is intentional.

For tests and embedded uses:

| Function | Returns |
| --- | --- |
| `runNanoUIE view` | `NanoUI (result, [msg])` |
| `runFrameE ctx input view` | `IO (result, [msg], DrawData, Bool)` |
| `runFrameReduce update ctx input model view` | `IO (result, model, [msg], DrawData, Bool)` |
| `runClickReduce update ctx input model view pos` | `IO (model, [msg], Bool)` |

`runFrameReduce` requests a follow-up frame when the reduced model differs. `reduceMessages` and `reduceUpdates` fold message lists.

## When to use it

| | `Var`-first | Elm-style |
| --- | --- | --- |
| State shape | Distributed through the view and app record | One model value |
| Transitions | Direct mutation | Explicit `Msg` and one `update` |
| Testing | Widget-level with `NanoUI.Testing` | Pure `update` tests plus frame tests |
| Local widget state | Natural | Still use `Var` for drafts and open flags |
| Best for | Tools and editors with external state | State machines, wizards, undo/history, small apps with a clear model |

The styles mix. An emitting view can keep `Var`s for transient UI state; a `Var`-based view can run a small component with `runNanoUIE` and act on the returned messages.

## Rules and pitfalls

- **Keep `update` pure.** File and network I/O belong in the view's event branch or in an effect queue interpreted outside the reducer.
- **Do not duplicate model state in `Var`s.** Local UI state is fine; durable state should have one home.
- **Guard every emission.** Unconditional emissions run on every pass.
- **Component keys are required in lists.** Give each instance a distinct `withKey`.
- **Keyboard and focus rules are unchanged.** Focus queries still belong at the end of the view.
- **Model changes draw on the next frame.** Do not compensate by writing the model into a `Var` mid-frame.

The `Var` style is covered in [State & `Var`](index.md); the frame consequences are in [Architecture & Internals](../architecture/index.md).
