# Elm-Style Views

`nano-ui-emit` adds a second way to write a view: instead of mutating `Var`s directly, the view emits typed messages, and a pure reducer folds them into a model. The shape will be familiar if you have used Elm, Redux, or `brick`'s state machines.

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

## What the types say

`NanoUIE msg a` is a `NanoUI` action that can emit only `msg`. There is no `Typeable` constraint and no runtime type test: emitting a message of the wrong type is a compile error.

- **`liftNanoUI`** runs an ordinary widget or UI operation in an emitting view.
- **`withNanoUI`** wraps a typed body in an ordinary layout, key, or scope: `withNanoUI column`, `withNanoUI row`, `withNanoUI (withKey todoId)`, `withNanoUI (disabledWhen True)`.
- **`emit`** emits a message directly.
- **`emitWhen`** emits when an ordinary widget activates: `emitWhen (button "Save") Save`.
- **`emitChanged`** runs a `Var` widget against the current model value and emits only if the value changed.
- **`emitEdited`** additionally requires the widget's `respChanged`, filtering out programmatic value updates.
- **`mapMessages`** lifts a child component's message type into its parent's.

The `Var` widgets are still the widgets. `emitChanged (slider 0 100) volume SetVolume` is the `slider` bound to a temporary `Var`; the model is the source of truth, and the emitted message carries the new value back.

## The frame under a reducer

With `runSdlAppReduce`:

1. The view runs with the current model and emits messages as widgets report events.
2. At the end of the frame, the messages are folded through `update` in emission order.
3. The drawing shown for that frame is from the model **before** reduction.
4. If the reduced model differs from the old one, another frame is requested to draw the new state.

That last point surprises people: a click that changes the model still costs a follow-up frame. The alternative — reducing mid-frame — would make the view read a model that changed halfway through the pass, which is worse. Keep the model update cheap and the follow-up frame is not noticeable.

Messages from every pass of a frame are collected in order, including passes caused by a local `Var` rebuild. Each runner invocation owns its queue, so an exception cannot leave messages for a later frame.

## When to use it

| | `Var`-first | Elm-style |
| --- | --- | --- |
| State shape | Distributed through the view and app record | One model value |
| Transitions | Direct mutation | Explicit `Msg` constructors and one `update` |
| Testing | Widget-level with `NanoUI.Testing` | Pure `update` tests plus frame tests |
| Local widget state | Natural | Still use `Var` for drafts and open flags |
| Large application | Scales by modules and app records | Scales by components and `mapMessages` |
| Best for | Tools and editors with external state | State machines, wizards, undo/history, small apps with a clear model |

The two styles are not exclusive. An emitting view can allocate and read `Var`s for transient UI state (a draft, an open dropdown), while the durable model stays in `NanoUIE`. A `Var`-based view can call `runNanoUIE` on a small emitting component and act on the returned messages.

If you want the `Var` style, read [Introduction to State](../state/introduction.md). If you want the reducer style, continue with [Messages, Reducers & Components](messages.md).

## Trade-offs in practice

- **A reducer is easier to unit test.** `update` is a pure function; no context, no frame, no widgets.
- **A reducer centralizes transitions.** Every way the model changes is a `Msg` constructor you can grep for. That is valuable when the state machine is the hard part.
- **Direct `Var` mutation is less ceremony for local state.** A text field's draft does not need a message type and an `update` branch.
- **Side effects stay in the view or outside the reducer.** `update` is pure. A save button can emit `Save` *and* perform the write in the view on the same event, or the model can hold a queue of effects that an interpreter drains. Keep file and network I/O out of `update`.

> [!TIP]
> The reducer runners are `runSdlAppReduce` and `runRgfwAppReduce`; the SDL one is in `NanoUI.Backend.Sdl` and the RGFW one in `NanoUI.Backend.Rgfw`. For headless tests, use `runFrameE`, `runFrameReduce`, and `runClickReduce` from `NanoUI.Emit`.
