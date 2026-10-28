# Messages, Reducers & Components

`NanoUIE msg a` is a view that can emit only `msg`. This chapter covers the adapters, component composition, the runners, and the rules that keep message flow predictable.

```haskell
import NanoUI
import NanoUI.Emit qualified as Emit
```

## The view

A `NanoUIE msg ()` view uses the same `do` notation as `NanoUI`. Widgets that are not adapted are lifted:

```haskell
settingsView :: Model -> Emit.NanoUIE Msg ()
settingsView model = Emit.withNanoUI column $ do
  Emit.liftNanoUI (heading "Application Settings")

  -- Emit on activation.
  Emit.emitWhen (button "Save") SaveDocument

  -- Run the widget against the model value; emit only on change.
  Emit.emitChanged (slider 0 100) model.volume SetVolume

  Emit.withNanoUI row $ do
    Emit.emitWhen (button "-") Decrement
    Emit.liftNanoUI (label (T.pack (show model.counter)))
    Emit.emitWhen (button "+") Increment
```

`withNanoUI` is `mapReaderT`: it applies an ordinary layout or scope function to the typed body. It does not change which messages are emitted; it changes where the widgets are laid out. `liftNanoUI` is the identity adapter for an ordinary `NanoUI` action.

`withRunInNanoUIE` is for ordinary widgets that take callbacks or multiple bodies, such as a custom container that runs a child action twice:

```haskell
view model = Emit.withRunInNanoUIE $ \run -> column $ do
  label "Settings"
  run (Emit.emitChanged (slider 0 100) model.volume SetVolume)
  run (Emit.emitWhen (button "Save") SaveDocument)
```

Call the supplied `run` function only inside the enclosing UI action.

## Emitting

| Function | Emits when |
| --- | --- |
| `emit msg` | Immediately. |
| `emitWhen widget msg` | The widget returns `True` (a button click, a submitted field). |
| `emitChanged widget old toMsg` | The widget's final value differs from `old`. |
| `emitEdited widget old toMsg` | The widget's value changed **and** its `Response` reports `respChanged`. |

`emitChanged` is built on `withValue`: the widget edits a temporary `Var` seeded with the model value, and the temporary's final value is compared to the old one. This is why it works with any `Var` widget, and why mutations to the temporary do not trigger a second pass or a repaint.

`emitEdited` is the right choice for text fields when programmatic model updates should not look like user edits. `emitChanged` is usually right for sliders and checkboxes.

> [!CAUTION]
> Only emit inside an event branch. `emit SaveDocument` at the top level of a view emits on every pass, and the reducer runs every emitted message. The adapters above already guard on the widget's event.

Messages are queued for the whole frame, including local-`Var` rebuilds. The queue is drained at frame end, so the order is emission order — top-to-bottom through the view, and left-to-right among siblings.

## Components

`mapMessages` embeds a child view whose message type is smaller than the parent's:

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

The child's messages go straight to the parent's sink, in traversal order, with no intermediate queue and no allocation of a wrapper per message. The parent's `update` handles `Child ChildIncrement` and `SaveAll` together.

A component's *widget* state is still ordinary nano-ui state. If the child is a list item, key it with `withKey` inside the child or around the `mapMessages` call. `mapMessages` changes the message type, not widget identity.

For components that need their own durable state, pass a `Var` for the local part and a model slice for the rendered part:

```haskell
searchView :: Var Text -> [Text] -> Emit.NanoUIE Msg ()
searchView query results = do
  Emit.liftNanoUI (searchInput query)
  -- Read the query after the field that edits it.
  Emit.liftNanoUI $ do
    q <- readVar query
    when (not (T.null q) && null results) (muted "No matches")
```

## Running

The backend runners take an update function, an initial model, and a view of the model:

```haskell
main :: IO ()
main = runSdlAppReduce defaultSdlOptions update (Model 0 "Ada") view
```

`runSdlAppReduce` is in `NanoUI.Backend.Sdl`; `runRgfwAppReduce` is the RGFW equivalent. Both fold messages through `update` in emission order at the end of each frame.

For tests and embedded uses:

| Function | Returns |
| --- | --- |
| `runNanoUIE view` | `NanoUI (result, [msg])` — collects one action's messages |
| `runFrameE ctx input view` | `IO (result, [msg], DrawData, Bool)` |
| `runFrameReduce update ctx input model view` | `IO (result, model, [msg], DrawData, Bool)` |
| `runClickReduce update ctx input model view pos` | `IO (model, [msg], Bool)` — a press/release pair |
| `reduceMessages update model msgs` | The folded model |
| `reduceUpdates model fns` | The folded model for function-valued messages |

`runFrameReduce` requests a follow-up frame when the reduced model differs. The `Bool` in the tuple reports whether one was requested; the drawing in the same tuple is from before the reduction.

A `Var`-based view can run a small emitting component and act on its messages immediately:

```haskell
(result, msgs) <- runNanoUIE (toolbarView model)
for_ msgs handleToolbarMessage
```

## Rules and pitfalls

- **Keep `update` pure.** File reads, process launches, and logging belong in the view's event branch, or in an effect queue the application interprets outside the reducer.
- **Model changes draw on the next frame.** That is intentional; do not compensate by writing the model into a `Var` mid-frame.
- **Do not duplicate model state in `Var`s.** Local UI state (draft text, open menus) is fine; durable state should have one home.
- **Unconditional emissions run on every pass.** Guard them with `emitWhen`, `emitChanged`, or an explicit event.
- **The keyboard and focus rules are unchanged.** Shortcuts are read in the view (or adapter); focus queries still belong at the end of the view.
- **Component keys are required for lists.** Two instances of the same component in a reorderable list need distinct `withKey` keys, exactly as two `Var`-based rows do.

See [Elm-Style Views](elm-overview.md) for when this style is the right choice, and [The Frame Model](../architecture/frame-model.md) for what a follow-up frame costs.
