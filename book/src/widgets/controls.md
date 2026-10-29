# Buttons, Menus & Inputs

## Buttons

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Signatures.hs:buttons}}
```

`button` returns `True` only on the frame it is activated, by left click or by Enter/Space while focused:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:button}}
```

Use the primed form when you need the response: hover state, a rectangle for an anchored popup, or a tooltip.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:button_response}}
```

`buttonContent` puts any view inside a button. The content is for display; a click anywhere on the button activates it:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:button_content}}
```

`iconButton` puts an icon before the label; an empty label gives an icon-only square button.

## Menus

`menuItem` draws a row and returns `True` when clicked. `menuItemShortcut` also displays a chord and binds it while the menu is open. `menuSeparator` and `menuItemDisabled` complete the row set.

To build a menu bar, anchor a `popup'` to a `menuButton'`'s rectangle:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:menu_bar}}
```

`constVar` supplies a read-only open flag: the truth is the menu's own state, and a click on the popup is written back to the real `Var`.

> [!IMPORTANT]
> Call overlays unconditionally. `popup'` draws nothing while closed, but wrapping it in `when isOpen` changes later widget ids each time the menu opens.

Bind a command chord **outside** the menu as well as on its row, so it works with the menu closed:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:menu_and_shortcut}}
```

Only the first binding of a chord in a frame fires, so the open menu's row wins. A complete menu bar is built in [Build a Notepad](../tutorials/notepad.md).

`contextMenu` attaches a right-click menu to any widget with a `Response`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:context_menu}}
```

## Input widgets

Input widgets take the `Var` they display and edit it, and manage caret, selection, focus, and keys themselves. To reset a field, write the `Var`; to react to an edit, use the primed form's `Response`.

| Widget | Signature | Description |
| --- | --- | --- |
| `checkbox` | `Text -> Var Bool -> NanoUI ()` | Two-state box with a label |
| `toggleButton` | `Tone -> Text -> Var Bool -> NanoUI ()` | Two-state button, filled in the tone while on |
| `radio` | `Foldable f => f Text -> Var Int -> NanoUI ()` | Radio group over indexed options |
| `boundedRadio` | `(Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUI ()` | Radio group over every enum value |
| `select` | `Foldable f => f Text -> Var Int -> NanoUI ()` | Drop-down select by index |
| `boundedSelect` | `(Bounded a, Enum a) => (a -> Text) -> Var a -> NanoUI ()` | Drop-down over every enum value |
| `comboBox` | `Foldable f => Text -> f Text -> Var Text -> NanoUI ()` | Placeholder, suggestions, free-text value |
| `slider` | `Float -> Float -> Var Float -> NanoUI ()` | Min, max, value |
| `textInput` | `Var Text -> NanoUI ()` | Single-line editable text |
| `searchInput` | `Text -> Var Text -> NanoUI ()` | Placeholder + search field with a clear button |
| `numericInput` | `Var Double -> NanoUI ()` | Numeric field with steppers |
| `colorPicker` | `Var Color -> NanoUI ()` | Saturation/value field and hue bar |

Every widget has the usual variants: primed (`...'`) returns a `Response`, `...With` takes a layout modifier, and `...Configured` takes a config record. For example:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:configured}}
```

### Text and search fields

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:text_input}}
```

`respChanged` fires on every edit; `respSubmitted` fires on Enter. For expensive work use `searchInput'`, whose `respChanged` is debounced (300 ms by default) and fires immediately when the field is cleared:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:search_input}}
```

### Choices

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:choices}}
```

`select` is index-based for data-driven options:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:select}}
```

`comboBox` takes free text plus suggestions; its `respChanged` fires on commit, not on every model write.

### Sliders, numbers, color

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Controls.hs:values}}
```

`numericInput` accepts digits and a leading minus when the range allows it; arrow keys step, and `Shift` steps ten times as far. `nicDecimals` controls display and accepted precision. `colorPickerRGBA` adds an alpha bar.
