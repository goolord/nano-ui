# Input Controls

Input widgets take the mutable `Var` they display and edit. They manage their own caret, selection, focus, and keyboard routing; the application reads the result when it needs it.

## Signatures

| Widget | Signature | Description |
| --- | --- | --- |
| `checkbox` | `Text -> Var Bool -> NanoUI ()` | Two-state box with a label. |
| `toggleButton` | `Tone -> Text -> Var Bool -> NanoUI ()` | Two-state button, filled in the tone's color while on. |
| `radio` | `Foldable f => f Text -> Var Int -> NanoUI ()` | Radio group over indexed options. |
| `boundedRadio` | `(Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUI ()` | Radio group over every enum value. |
| `enumRadio` | `(Bounded a, Enum a, Show a) => Var a -> NanoUI ()` | `boundedRadio` labelled with `show`. |
| `select` | `Foldable f => f Text -> Var Int -> NanoUI ()` | Drop-down select by index. |
| `boundedSelect` | `(Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUI ()` | Drop-down over every enum value. |
| `enumSelect` | `(Bounded a, Enum a, Show a) => Var a -> NanoUI ()` | `boundedSelect` labelled with `show`. |
| `comboBox` | `Foldable f => Text -> f Text -> Var Text -> NanoUI ()` | Placeholder, suggestions, free-text value. |
| `slider` | `Float -> Float -> Var Float -> NanoUI ()` | Min, max, value. |
| `textInput` | `Var Text -> NanoUI ()` | Single-line editable text. |
| `searchInput` | `Text -> Var Text -> NanoUI ()` | Placeholder + search field with a clear button. |
| `numericInput` | `Var Double -> NanoUI ()` | Numeric field with steppers (configure for range/step). |
| `colorPicker` | `Var Color -> NanoUI ()` | Saturation/value field and hue bar. |

Every widget has the usual variants: primed (`...'`) returns a `Response`, `...With` takes a layout modifier, and `...Configured` takes a config record. For example, `numericInputConfigured` sets the range, step, decimals, and hexadecimal mode:

```haskell
numericInputConfigured defaultNumericInputConfig
  {nicMin = 0, nicMax = 255, nicHex = True} byte
```

## Text input and response

```haskell
query <- useVar ""
resp <- textInput' query
when (respSubmitted resp) (search =<< readVar query)
when (respChanged resp) (preview =<< readVar query)
```

`respChanged` fires on every edit; `respSubmitted` fires on Enter. For expensive work, use `searchInput'`, whose `respChanged` is debounced (300 ms by default) and fires immediately when the field is cleared:

```haskell
resp <- searchInput' "Find a package" query
when (respChanged resp) (refreshResults =<< readVar query)
```

The debounce means a fast typist starts one search, not one per keystroke. The `Var` itself is still updated on every keystroke, so a cheap label can show the live query.

## Choices

```haskell
enabled <- useVar True
checkbox "Enable notifications" enabled

bold <- useVar False
toggleButton Accent "B" bold

data Pace = Walk | Jog | Sprint
  deriving (Eq, Show, Enum, Bounded)

pace <- useEnumVar Jog
enumRadio pace                    -- radio group over Walk / Jog / Sprint

mode <- useEnumVar Jog
enumSelect mode                   -- drop-down over the same values
```

`select` is index-based and is the right choice when the options come from data:

```haskell
choice <- useVar 0
select ["Small", "Medium", "Large"] choice
```

`comboBox` takes free text plus suggestions; its `respChanged` fires on commit, not on every model write. `select` and `comboBox` route through different theme surfaces (`themeButton` versus `themeInput`), which is one of the [unexpected styling interactions](../styling/unexpected-interactions.md).

## Sliders and numbers

```haskell
volume <- useVar (40 :: Float)
slider 0 100 volume

count <- useVar (0 :: Double)
numericInput count
```

`numericInput` accepts digits, a leading minus when the range allows it, and arrow-key steps; `Shift` steps ten times as far. The configured `nicDecimals` controls both display and accepted precision, and a step is rounded to that precision.

## Color

```haskell
accent <- newVar (colorHex "#38bdf8")
colorPicker accent
```

`colorPickerRGBA` adds an alpha bar and an `#RRGGBBAA` field.

## Reading and resetting

All input widgets mutate the `Var` directly and return `()`. To reset a field, write the `Var`; to react, use the primed response. Two rules keep behavior predictable:

- **Read below the widget that edits it** when something else displays the value, so the change lands in one pass.
- **Do not read a value just to pass it back** into the same widget; pass the `Var`.

For multi-line editing, see [Text Editing](text-editing.md). For the full response record, see the [Widget Gallery](index.md).
