# Styling & Theming

Styling in nano-ui is scoped: a `Theme` holds a `Style` per surface kind, and `styled`/`themed` modify that theme for a subtree. There is no per-widget CSS and no global mutable stylesheet.

For a live tour of the modifiers, see the [Interactive Styling Lab](lab.md). For non-obvious behavior, see [Styling Gotchas](gotchas.md).

## The theme's five surfaces

| Field | Used by |
| --- | --- |
| `themeButton` | Buttons, toggles, dropdown triggers |
| `themeInput` | Text fields, search boxes, numeric inputs, sliders |
| `themePanel` | Cards, containers, sidebars, toolbars |
| `themePopup` | Dropdowns, context menus, select popouts |
| `themeFloatingWindow` | Modals, floating windows, inspectors |

A `Style` sets `bg`, `fg`, `border`, `borderWidth`, `cornerRadius`, `hoverBg`, and `activeBg`. A `Theme` also defines semantic colors: `themeAccent` / `themeOnAccent`, `themeMuted`, `themeSelection`, `themeFocusRing`, `themeWindow`, `themeSeparator`, and the success/warning/danger tones.

The page behind the view is `pageBackground`: `themeWindow`, unless the theme sets `themePage`. `pageColor` sets one. `panelPage` puts a view that draws no panel of its own on the panel surface. The page sits over the window background, which shows in the gaps between panels:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Styling.hs:panel_page}}
```

A full-window scroller (`scrollWith grow`) fills the page colour.

Built-in themes: `defaultTheme` (dark), `defaultLightTheme`, the Tomorrow variants, and `themeFromBase16` / `themeFromBase16Dark` / `themeFromBase16Light`. Set one at startup with `sdlAppTheme = Just appTheme`, switch per frame with `setUiTheme`, or follow the OS with `followSystemTheme` / `lightDark`.

## Scoped styling

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Styling.hs:scoped}}
```

Modifiers compose with `(.)`; nested scopes inherit the outer modification. Theme scopes affect painting only, never layout metrics.

- `background`, `foreground`, `borderColor`, `borderWidth`, `borderSides`, `cornerRadius`, `hoverBackground`, `pressBackground`, and `fillColor` set `Style` fields.
- `buttonStyle`, `inputStyle`, `panelStyle`, `everyStyle`, `accentColor`, `textColor`, `rounded`, `subtle`, `tinted`, and `readableOn` modify the `Theme`.
- `primary`, `destructive`, `success`, `warning`, `tone Danger`, and `toneColor Warning` apply semantic colours.

A quiet button:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Styling.hs:quiet}}
```

For a fully custom look, define a palette record, build a `Theme` from it, and derive scoped helpers. Keep three surface levels (well, shelf, controls) and reserve color for meaning.

## Interaction states

A widget is drawn in one of four states:

| State | Drawn with |
| --- | --- |
| Default | `styleBg`, `styleFg`, `styleBorder` |
| Hover | `styleHoverBg` |
| Pressed | `styleActiveBg`, while the primary button is held inside the widget |
| Disabled | Dimmed colours; pointer and keyboard input are ignored |

`disabledWhen cond subtree` disables interaction and dims rendering while preserving layout and state:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Styling.hs:disabled}}
```

Shortcuts and pointer events inside a disabled subtree are suppressed automatically.

All four states, light and dark:

<nano-ui-state-lab></nano-ui-state-lab>

## Typography and fonts

Font modifiers apply to any text-capable widget (`labelWith`, `headingWith`, `textInputWith`, `buttonWith`, ...):

- `fontSize px`, `fontMono`, `fontBold` (`fontWeight`), `fontItalic`, `fontUnderline`, `fontStrike`.
- `fontTone Warning`, `fontColor color`.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Styling.hs:font_modifiers}}
```

The SDL backend bundles Inter and finds installed fonts by family name:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Styling.hs:fonts}}
```

`setSdlUiFont` changes fonts mid-session. `sdlAppUiScale = 0` follows the desktop scale on Windows. Measure text with `lineWidth`, `fmLineHeight`, and `fmSnapScale`, not by summing per-glyph advances.

The font modifiers side by side:

<nano-ui-typo-lab></nano-ui-typo-lab>
