# Elm-Style State

`nano-ui-emit` is the reducer-based alternative to `Var`: instead of mutating state directly, the view emits typed messages, and a pure reducer folds them into a model.

```haskell
data Model = Model {counter :: Int, name :: Text}
data Msg = Increment | Decrement | SetName Text

view :: Model -> Emit.NanoUIE Msg ()
view m = Emit.withNanoUI column $ do
  Emit.liftNanoUI (heading "Profile")
  Emit.withNanoUI row $ do
    Emit.emitWhen (button "-") Decrement
    Emit.liftNanoUI (label (T.pack (show m.counter)))
    Emit.emitWhen (button "+") Increment
  Emit.emitChanged textInput m.name SetName

update :: Msg -> Model -> Model
update Increment m = m {counter = m.counter + 1}
update Decrement m = m {counter = m.counter - 1}
update (SetName n) m = m {name = n}

main :: IO ()
main = runSdlAppReduce defaultSdlOptions update (Model 0 "Ada") view
```

Everything else about nano-ui — widgets, layout, focus, and keys — is unchanged. The state-management style is what differs.

## Emitting

`NanoUIE msg a` is a `NanoUI` action that can emit only `msg`. There is no `Typeable` constraint and no runtime type test: emitting the wrong type is a compile error.

| Function | Emits when |
| --- | --- |
| `emit msg` | Immediately |
| `emitWhen widget msg` | The widget returns `True` (button click, submitted field) |
| `emitChanged widget old toMsg` | The widget's final value differs from `old` |
| `emitEdited widget old toMsg` | The value changed **and** `respChanged` is true |

`liftNanoUI` runs an ordinary widget or action. `withNanoUI` wraps a typed body in an ordinary layout, key, or scope:

```haskell
Emit.withNanoUI column $ do
  Emit.liftNanoUI (label "Settings")
  Emit.withNanoUI (withKey "form") $ do
    Emit.emitChanged (slider 0 100) volume SetVolume
    Emit.emitWhen (button "Save") Save
```

`withRunInNanoUIE` supplies a runner for ordinary widgets that take callbacks or multiple bodies:

```haskell
view model = Emit.withRunInNanoUIE $ \run -> column $ do
  label "Settings"
  run (Emit.emitChanged (slider 0 100) model.volume SetVolume)
  run (Emit.emitWhen (button "Save") SaveDocument)
```

`emitChanged` runs the widget against a temporary `Var` seeded with the model value and emits the new value only if it differs. `emitEdited` is its stricter form for text fields, where programmatic writes should not look like edits.

> [!CAUTION]
> Emit only inside an event branch. `emit Save` at the top level of a view emits on every pass, and the reducer runs every message. The adapters above already guard on the widget's event.

Messages are collected across every pass of a frame, including local-`Var` rebuilds, and reduced at frame end in emission order.

## Components

`mapMessages` embeds a child view whose message type is smaller than its parent's:

```haskell
data ChildMsg = ChildIncrement | ChildReset
data ParentMsg = Child ChildMsg | SaveAll

childView :: Int -> Emit.NanoUIE ChildMsg ()
childView n = Emit.withNanoUI row $ do
  Emit.emitWhen (button "+") ChildIncrement
  Emit.liftNanoUI (label (T.pack (show n)))
  Emit.emitWhen (button "Reset") ChildReset

parentView :: Int -> Emit.NanoUIE ParentMsg ()
parentView n = Emit.withNanoUI column $ do
  Emit.mapMessages Child (childView n)
  Emit.emitWhen (button "Save All") SaveAll
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
