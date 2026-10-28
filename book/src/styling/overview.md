# Theming & Surface Architecture

`nano-ui` uses a structured surface routing architecture rather than individual per-widget CSS rules. 

## The Five Surfaces of a `Theme`

A `Theme` holds five distinct `Style` records corresponding to semantic surfaces:

1. **`themeButton`**: Interactive controls, buttons, toggles, and dropdown triggers.
2. **`themeInput`**: Editable text fields, search boxes, numeric inputs, and sliders.
3. **`themePanel`**: Cards, containers, sidebars, toolbars, and background chrome.
4. **`themePopup`**: Dropdown menus, context menus, and select popouts.
5. **`themeFloatingWindow`**: Modals, draggable windows, and inspector dialogs.

In addition, a `Theme` defines semantic color roles:
- `themeAccent` & `themeOnAccent`: Primary brand actions and focus rings.
- `themeMuted`: Secondary labels, placeholders, and subtle borders.
- `themeSelection`: Highlighted text and selected list rows.
- `themeFocusRing`: Keyboard focus outlines.
- Semantic Tones: `themeSuccess`, `themeWarning`, `themeDanger`.

---

## Interactive Surface Overview

Toggle surfaces below to observe how each surface category routes across composite controls:

<nano-ui-hero></nano-ui-hero>

---

## Scoped Theming with `styled` and `themed`

Themes are composable and scoped lexically using standard Haskell combinators:

```haskell
-- Switch base theme for a subtree
themed defaultLightTheme $ do
  mainContent

-- Scope a theme transformation
styled (primary . rounded 999) $ do
  button "Submit Order"
```

Modifiers compose cleanly with `(.)`. Nested scopes inherit modifications made by outer scopes.
