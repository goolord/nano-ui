# nano-ui-emit: Elm-style Architecture

`nano-ui-emit` provides Elm-style typed messages and pure state reducers on top of `nano-ui`.

## The `NanoUIE msg a` View

Instead of mutating `Var`s directly in views, a `NanoUIE msg ()` view emits messages:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import NanoUI
import NanoUI.Emit qualified as Emit

data Msg
  = SaveDocument
  | SetVolume Float
  | Increment
  | Decrement

view :: AppModel -> Emit.NanoUIE Msg ()
view model = Emit.withNanoUI column $ do
  Emit.liftNanoUI (label "Application Settings")
  
  -- Emit on button activation
  Emit.emitWhen (button "Save") SaveDocument
  
  -- Emit on value change
  Emit.emitChanged (slider 0 100) model.volume SetVolume
  
  Emit.withNanoUI row $ do
    Emit.emitWhen (button "-") Decrement
    Emit.liftNanoUI (label (T.pack (show model.counter)))
    Emit.emitWhen (button "+") Increment
```

## The Reducer Runner

Pair your view with a pure update reducer `msg -> model -> model`:

```haskell
update :: Msg -> AppModel -> AppModel
update SaveDocument m = m  -- Trigger side effect via external command
update (SetVolume v) m = m { volume = v }
update Increment m = m { counter = counter m + 1 }
update Decrement m = m { counter = counter m - 1 }

main :: IO ()
main = runSdlAppReduce view update initialModel
```

### Component Hierarchies with `mapMessages`
Embed reusable child components using `mapMessages`:

```haskell
data ParentMsg = ChildMsg SubMsg | ParentSave

parentView :: SubModel -> Emit.NanoUIE ParentMsg ()
parentView sub = do
  Emit.mapMessages ChildMsg (childView sub)
  Emit.emitWhen (button "Save All") ParentSave
```
Messages are dispatched in order directly to the parent queue with zero intermediate wrapper allocations.
