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

A `Style` is `bg`, `fg`, `border`, `borderWidth`, `cornerRadius`, `hoverBg`, and `activeBg`. A `Theme` also defines semantic colors: `themeAccent` / `themeOnAccent`, `themeMuted`, `themeSelection`, `themeFocusRing`, `themeWindow`, `themeSeparator`, and the success/warning/danger tones.

The page behind the view is `pageBackground`: `themeWindow` by default,
unless the theme sets `themePage`. `panelPage` gives a view that draws no
panel the panel surface, so the app sits on the panel colour while
`themeWindow` stays the window background behind it; `pageColor` sets any
other page. The page is laid over the window background, so the window shows
in the gaps between a page's panels:

```haskell
-- An app with no panel of its own: the panel surface is the page.
sdlAppTheme = Just (panelPage defaultTheme)

-- Or scope it to a page with no panels.
styled panelPage (columnWith (padAll 12) (label "Body"))
```

A full-window scroller (`scrollWith grow`) fills the page colour.

Built-in themes: `defaultTheme` (dark), `defaultLightTheme`, the Tomorrow variants, and `themeFromBase16` / `themeFromBase16Dark` / `themeFromBase16Light`. Set one at startup with `sdlAppTheme = Just appTheme`, switch per frame with `setUiTheme`, or follow the OS with `followSystemTheme` / `lightDark`.

## Scoped styling

```haskell
-- Replace the base theme for a subtree.
themed defaultLightTheme mainContent

-- Modify the active theme for a subtree.
styled (primary . rounded 999) $ button "Continue"
```

Modifiers compose with `(.)`; nested scopes inherit the outer modification. Theme scopes affect painting only, never layout metrics.

Useful modifiers:

- **Style fields:** `background`, `foreground`, `borderColor`, `borderWidth`, `borderSides`, `cornerRadius`, `hoverBackground`, `pressBackground`, `fillColor`.
- **Theme-level:** `buttonStyle`, `inputStyle`, `panelStyle`, `everyStyle`, `accentColor`, `textColor`, `rounded`, `subtle`, `tinted`, `readableOn`.
- **Semantic shortcuts:** `primary`, `destructive`, `success`, `warning`, `tone Danger`, `toneColor Warning`.

A common local helper is a quiet button:

```haskell
quiet = subtle . buttonStyle (foreground muted)

whenM (styled quiet (button "Skip")) skip
```

For a fully custom look, define a palette record, build a `Theme` from it, and derive scoped helpers. Keep three surface levels (well, shelf, controls) and reserve color for meaning.

## Interaction states

Widgets move through four states:

1. **Default**: `styleBg`, `styleFg`, `styleBorder`.
2. **Hover**: `styleHoverBg`.
3. **Pressed**: `styleActiveBg`, while the primary button is held inside the widget.
4. **Disabled**: dimmed and non-interactive.

`disabledWhen cond subtree` disables interaction and dims rendering while preserving layout and state:

```haskell
disabledWhen isSaving $ row $ do
  button "Cancel"
  button "Confirm"
```

Shortcuts and pointer events inside a disabled subtree are suppressed automatically.

The interactive state laboratory renders all four states across controls in both light and dark mode:

<nano-ui-state-lab></nano-ui-state-lab>

## Typography and fonts

Font modifiers apply to any text-capable widget (`labelWith`, `headingWith`, `textInputWith`, `buttonWith`, ...):

- `fontSize px`, `fontMono`, `fontBold` (`fontWeight`), `fontItalic`, `fontUnderline`, `fontStrike`.
- `fontTone Warning`, `fontColor color`.

```haskell
labelWith (fontMono . fontSize 15 . fontTone Warning) "warning: low disk"
```

The SDL backend bundles Inter and uses installed fonts by family name:

```haskell
defaultSdlOptions
  { sdlAppFont = FontSearch ["Segoe UI", "SF Pro Text", "Helvetica Neue"]
  , sdlAppMonoFont = FontSearch ["Cascadia Mono", "JetBrains Mono", "Consolas"]
  , sdlAppFontSize = 14
  }
```

`setSdlUiFont` changes fonts mid-session. `sdlAppUiScale = 0` follows the desktop scale on Windows. For exact font metrics and text measurement, use `lineWidth`, `fmLineHeight`, and `fmSnapScale` from the drawing API rather than summing per-glyph advances.

The typography laboratory shows the modifiers side by side:

<nano-ui-typo-lab></nano-ui-typo-lab>

## See also

- Widgets: [Widget Gallery](../widgets/index.md).
- Live modifier workbenches: [Interactive Styling Lab](lab.md).
- Behavior that surprises people: [Styling Gotchas](gotchas.md).
- Layout spacing and sizing: [Layout & Scrolling](../layout/index.md).
