# Styling & Theming

Styling in `nano-ui` is scoped rather than selector-based. A `Theme` holds a `Style` record for each surface kind, and `styled` or `themed` adjusts that theme for a subtree. There is no per-widget CSS and no global mutable stylesheet.

To experiment with modifiers live, open the [Interactive Styling Lab](lab.md). For subtle surface interactions, check [Styling Gotchas](gotchas.md).

## The five theme surfaces

| Theme field | Widgets that read it |
| --- | --- |
| `themeButton` | Buttons, toggles, dropdown triggers |
| `themeInput` | Text fields, search boxes, numeric inputs, sliders |
| `themePanel` | Panels, cards, containers, sidebars, toolbars |
| `themePopup` | Dropdown menus, context menus, popups |
| `themeFloatingWindow` | Modals, floating windows, inspectors |

Each `Style` sets `styleBg`, `styleFg`, `styleBorder`, `styleBorderWidth`, `styleCornerRadius`, `styleHoverBg`, `styleActiveBg`, and `styleBorderSides`. Beyond those five surfaces, a `Theme` defines shared semantic colors: `themeAccent`, `themeOnAccent`, `themeMuted`, `themeSelection`, `themeFocusRing`, `themeWindow`, `themeSeparator`, and the success, warning, and danger tones. Construct colors with `colorRGB`, `colorRGBA`, `colorHex "#38bdf8"`, or `colorHexA "#38bdf8cc"`.

The backdrop behind your view is `pageBackground`, which defaults to `themeWindow` unless the theme sets `themePage` (via `pageColor`). Calling `styled panelPage` puts a borderless view directly on the panel surface. The window color itself still peeks through gaps between panels:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Styling.hs:panel_page}}
```

A full-window scroller (`scrollWith grow`) paints using this page color instead of drawing a bordered well.

`defaultTheme` (dark) and `defaultLightTheme` live in `NanoUI`; extra presets (Tomorrow variants) and Base16 generators (`themeFromBase16`, `themeFromBase16Dark`, `themeFromBase16Light`) live in `NanoUI.Themes`. Set a theme at startup with `sdlAppTheme = Just appTheme`, change it dynamically per frame with `setTheme`, or match the OS appearance automatically with `followSystemTheme` or `lightDark`.

## Scoped styling

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Styling.hs:scoped}}
```

Style and theme modifiers compose with `(.)`, and nested `styled` blocks build on top of outer overrides. Theme changes affect painting only and never alter layout sizes.

- **Style modifiers** (`Style -> Style`): `background`, `foreground`, `borderColor`, `borderWidth`, `borderSides`, `cornerRadius`, `hoverBackground`, `pressBackground`, and `fillColor`.
- **Theme modifiers** (`Theme -> Theme`): `buttonStyle`, `inputStyle`, `panelStyle`, `everyStyle`, `accentColor`, `textColor`, `rounded`, `subtle`, `tinted`, and `readableOn`.
- **Semantic shortcuts**: `primary`, `destructive`, `success`, `warning`, `tone Danger`, `calloutTone Warning`, and `toneColor Warning`.

For example, here is a quiet, borderless button helper:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Styling.hs:quiet}}
```

When designing a custom visual identity, start with a palette record, construct one complete `Theme` from it, and build small scoped helpers on top. Keeping three clear surface levels (recessed wells, panel surfaces, and elevated controls) while reserving accent color for meaning keeps interfaces clean and readable.

## Interaction states

Every interactive widget renders in one of four visual states:

| State | Visual treatment |
| --- | --- |
| Default | `styleBg`, `styleFg`, `styleBorder` |
| Hover | `styleHoverBg` |
| Pressed | `styleActiveBg` while the primary mouse button is held on the widget |
| Disabled | Dimmed colors with all pointer and keyboard input ignored |

Wrap any subtree in `disabledWhen cond` to dim it and disable interaction without changing layout or losing state:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Styling.hs:disabled}}
```

Shortcuts and mouse events inside a disabled subtree are blocked automatically.

Compare all four states across dark and light themes:

<nano-ui-state-lab></nano-ui-state-lab>

## Typography and fonts

Font modifiers compose right alongside layout modifiers on text and labeled widgets (`labelWith`, `buttonWith`, `selectWith`, `textAreaWith`, and `ticLayout` inside `textInputConfigured`):

- **Size and face:** `fontSize px`, `fontSizeScale factor`, `fontRegular`, `fontProportional`, `fontMono`, `fontTabular` (tabular lining digits so numeric columns align without jumping)
- **Weight, slant, decoration:** `fontBold` (or `fontWeight`), `fontItalic`, `fontUnderline`, `fontStrike`
- **Color and tone:** `fontTone Warning`, `fontColor color`
- **Single-line truncation:** `singleLine` keeps a label on one line even inside a column and truncates overflow with an ellipsis; `ellipsisEnd` (default), `ellipsisMiddle` (for file paths), or `ellipsisStart` chooses where the ellipsis sits (`textEllipsis`).

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Styling.hs:font_modifiers}}
```

The SDL backend bundles Inter as its default font and can also search installed system fonts by family name:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Styling.hs:fonts}}
```

Call `setSdlUiFont` to swap fonts mid-session, and set `sdlAppUiScale = 0` on Windows to follow the OS display scale. Whenever you need exact string widths or line metrics, use `lineWidthUi`, `fmLineHeight`, and `fmSnapScale` instead of adding per-glyph advances by hand.

Preview the font modifiers side by side:

<nano-ui-typo-lab></nano-ui-typo-lab>
