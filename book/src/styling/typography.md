# Typography & Fonts

`nano-ui` supports rich font styling including font sizes, monospace styling, weights, slants, text decorations, semantic font tones, and custom font colors.

## Font Modifiers

Font modifiers can be applied directly to text-capable widgets (`labelWith`, `headingWith`, `textInputWith`, `buttonWith`):

- **`fontSize px`**: Sets font size in logical units.
- **`fontMono`**: Switches to the monospace font family (`sdlAppMonoFont`).
- **`fontBold`**: Sets heavy font weight (700).
- **`fontItalic`**: Applies italic slant.
- **`fontUnderline`**: Draws an underline rule.
- **`fontStrike`**: Draws a strikethrough line.
- **`fontTone Tone`**: Applies a semantic tone (`fontTone Danger`, `fontTone Warning`, `fontTone Success`).
- **`fontColor Color`**: Overrides text color with an explicit RGB/RGBA value.

---

## Typography Laboratory

Inspect how font modifiers render across different controls:

<nano-ui-typo-lab></nano-ui-typo-lab>

---

## Font Discovery & Backend Configuration

Under the SDL backend, default TrueType fonts are bundled (Inter for proportional text). You can configure system font lookup via `FontSearch`:

```haskell
sdlOptions
  { sdlAppFont = FontSearch ["Segoe UI", "SF Pro Text", "Helvetica Neue"]
  , sdlAppMonoFont = FontSearch ["Cascadia Mono", "JetBrains Mono", "Consolas"]
  , sdlAppFontSize = 14
  }
```

On Windows, font metrics are resolved with HarfBuzz shaping and FreeType rendering.
