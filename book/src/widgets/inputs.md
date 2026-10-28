# Input Controls

Input widgets in `nano-ui` take the mutable `Var` they display and edit. They manage their own caret, selection, and keyboard event routing internally.

## Input Widget Overview

| Widget | Signature | Description |
| --- | --- | --- |
| `checkbox` | `Text -> Var Bool -> NanoUI ()` | Two-state toggle box with label. |
| `toggleButton` | `Text -> Var Bool -> NanoUI ()` | Two-state push button with active fill. |
| `radio` | `Eq a => Text -> a -> Var a -> NanoUI ()` | Single choice radio button for value `a`. |
| `enumRadio` | `(Enum a, Bounded a, Show a) => Var a -> NanoUI ()` | Radio button group for all enum values. |
| `select` | `Foldable f => f Text -> Var Int -> NanoUI ()` | Dropdown select by index. |
| `comboBox` | `Foldable f => f Text -> Var Text -> NanoUI ()` | Editable text field with dropdown suggestions. |
| `slider` | `Float -> Float -> Var Float -> NanoUI ()` | Slider with track and thumb (min, max). |
| `textInput` | `Var Text -> NanoUI ()` | Single-line editable text input. |
| `searchInput` | `Var Text -> NanoUI ()` | Search field with clear button. |
| `numericInput` | `(Num a, Ord a) => a -> a -> Var a -> NanoUI ()` | Numeric stepper field with increment/decrement buttons. |

---

## Examples

### Text Input with Response

Use the primed variant `textInput'` to react to submission (Enter) or changes:

```haskell
queryVar <- useVar ""
resp <- textInput' queryVar
when (respSubmitted resp) $ do
  performSearch =<< readVar queryVar
```

### Debounced Search with `searchInput'`

`searchInput'` fires `respChanged` only after a typing pause, saving expensive queries:

```haskell
searchVar <- useVar ""
resp <- searchInput' searchVar
when (respChanged resp) $ do
  triggerBackendQuery =<< readVar searchVar
```
