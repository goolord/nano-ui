# Buttons, Menus & Inputs

This chapter covers the everyday interactive controls: buttons, menus, choice widgets, sliders, numeric fields, single-line text inputs, and color pickers.

## Buttons

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Signatures.hs:buttons}}
```

`button` draws a clickable button and returns `True` on the single frame it is activated (either by a left click, or by pressing Enter or Space while focused):

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:button}}
```

Reach for the primed variant (`button'`) whenever you need the `Response` for hover checks, anchoring a popup, or attaching a tooltip:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:button_response}}
```

`buttonContent` wraps an arbitrary `NanoUI ()` view inside a clickable button. The inner widgets are purely visual; clicking anywhere on the button activates it:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:button_content}}
```

`iconButton` draws an SVG icon next to a label. Passing an empty string `""` produces a square icon-only button.

## Menus

Menu rows are built from `menuItem` (returns `True` when clicked), `menuItemShortcut` (shows a shortcut label and activates on that chord while the menu is open), `menuSeparator`, and `menuItemDisabled`.

Build a menu bar by pairing `menuButton'` triggers with `popup'` overlays anchored to each button's rectangle:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:menu_bar}}
```

Here `constVar` passes a read-only open flag derived from `openMenu`, and clicks on the popup update `openMenu` directly.

> [!IMPORTANT]
> Call overlays unconditionally on every frame. A closed `popup'` renders nothing, whereas wrapping it in `when isOpen` shifts sibling widget IDs every time a menu opens or closes.

Because `menuItemShortcut` only listens while its menu is open, bind the same chord outside the menu with `shortcut` if you want it to work when the menu is closed:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:menu_and_shortcut}}
```

Only the first matching binding in a frame fires, so the open menu item naturally takes priority over the fallback shortcut. For a complete menu bar implementation, see [Tutorial 2: Build a Notepad](../tutorials/notepad.md).

Attach a right-click menu to any widget with a `Response` using `contextMenu`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:context_menu}}
```

## Input widgets

Input widgets display a `Var` and mutate it in place, keeping transient editing state (caret position, text selection, open dropdowns) under their own widget ID. Write to the `Var` directly to reset or overwrite a field, and use the primed variant's `Response` when you need to react to edits.

| Widget | Signature | Purpose |
| --- | --- | --- |
| `checkbox` | `Text -> Var Bool -> NanoUI ()` | Checkbox with a label |
| `toggleButton` | `Tone -> Text -> Var Bool -> NanoUI ()` | Two-state button filled with its tone color when active |
| `radio` | `Foldable f => f Text -> Var Int -> NanoUI ()` | Radio button group indexed by position |
| `boundedRadio` | `(Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUI ()` | Radio group covering every constructor of an enum |
| `select` | `Foldable f => f Text -> Var Int -> NanoUI ()` | Dropdown selector indexed by position |
| `boundedSelect` | `(Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUI ()` | Dropdown selector over every constructor of an enum |
| `comboBox` | `Foldable f => Text -> f Text -> Var Text -> NanoUI ()` | Free-text field with filterable autocomplete suggestions |
| `slider` | `Float -> Float -> Var Float -> NanoUI ()` | Continuous horizontal slider between min and max |
| `textInput` | `Var Text -> NanoUI ()` | Single-line text input |
| `searchInput` | `Text -> Var Text -> NanoUI ()` | Search field with placeholder, clear button, and debounced changes |
| `numericInput` | `Var Double -> NanoUI ()` | Numeric input with increment/decrement steppers |
| `colorPicker` | `Var Color -> NanoUI ()` | Saturation/value square and hue slider |

Every widget in this table provides the standard variants: primed (`...'`) returns a `Response`, `...With` accepts a layout modifier, and `...Configured` accepts a config record:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:configured}}
```

### Text and search fields

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:text_input}}
```

On `textInput'`, `respChanged` is `True` on every edit and `respSubmitted` is `True` when the user presses Enter. When an edit triggers expensive work like filtering a database or calling an API, use `searchInput'` instead: its `respChanged` is debounced (300 ms by default) and fires immediately when the clear button is clicked:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:search_input}}
```

### Choices

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:choices}}
```

Use `select` when the options come from runtime data rather than a fixed enum:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:select}}
```

`comboBox` accepts both suggested options and arbitrary text. Its `respChanged` flag fires only when the user commits a choice, not when your code updates the `Var` externally.

### Sliders, numbers, and color

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:values}}
```

`numericInput` accepts digits (plus a leading minus sign when negative numbers are allowed). Pressing Up or Down steps the value, and holding Shift multiplies the step by 10. Set `nicDecimals` to control both formatting and precision. For colors with transparency, use `colorPickerRGBA`, which adds an alpha bar.
