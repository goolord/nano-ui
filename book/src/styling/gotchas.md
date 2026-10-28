# Styling Gotchas

Twelve non-obvious styling interactions, edge cases, and gotchas discovered in `nano-ui`'s layout, theming, and rendering pipelines. Start with [Styling & Theming](index.md) for the concepts and the [Interactive Styling Lab](lab.md) for live examples; come here when the result is not what you expected.

Each interaction includes an explanation of **why it happens** under the hood, the **recommended idiom or fix**, code comparisons, and interactive headless SVG specimens showing the trap vs the fix.

---

## 1. `buttonWith` Drops Font Flags

`buttonWith` drops font flags like `fontBold`, `fontItalic`, `fontUnderline`, and `fontTone`.

<nano-ui-trap id="trap-01"></nano-ui-trap>

---

## 2. `heading` and `fontHeading` Differ

`heading` and `fontHeading` produce different sizes and margins.

<nano-ui-trap id="trap-02"></nano-ui-trap>

---

## 3. `checkbox` & `radio` Span Three Surfaces

Checkboxes and radio buttons route their indicator, box well, and text label through different surface records.

<nano-ui-trap id="trap-03"></nano-ui-trap>

---

## 4. `select` and `comboBox` Use Different Surfaces

`select` trigger buttons read `themeButton`, while `comboBox` fields read `themeInput`.

<nano-ui-trap id="trap-04"></nano-ui-trap>

---

## 5. `scrollWith` Changes Surface With Its Sizing

Sizing modifiers on `scrollWith` change whether the scroll well is drawn.

<nano-ui-trap id="trap-05"></nano-ui-trap>

---

## 6. `callout` Tints Nested Panels

`callout` applies `panelStyle` overrides that cascade down to any nested panels.

<nano-ui-trap id="trap-06"></nano-ui-trap>

---

## 7. `toggleButton` Ignores `buttonStyle` When On

When active, `toggleButton` overrides `themeButton` background with `themeAccent`.

<nano-ui-trap id="trap-07"></nano-ui-trap>

---

## 8. `background` Resets on Focus

Setting background color directly on an input can be masked when the widget gains focus.

<nano-ui-trap id="trap-08"></nano-ui-trap>

---

## 9. `borderWidth` Does Not Add Layout Space

Borders in `nano-ui` are drawn inset and do not expand widget layout bounds.

<nano-ui-trap id="trap-09"></nano-ui-trap>

---

## 10. Modifier Order in `(.)` and `styled`

Leftmost-wins layout composition vs nested `styled` lexical scoping.

<nano-ui-trap id="trap-10"></nano-ui-trap>

---

## 11. `tabs` Read `themePanel`, Not `themeButton`

Tab headers read their styling from `themePanel`, not `themeButton`.

<nano-ui-trap id="trap-11"></nano-ui-trap>

---

## 12. `frameToSvg` Geometry & FontMetrics Quirks

Headless SVG rendering considerations for text bounds and square geometry.

<nano-ui-trap id="trap-12"></nano-ui-trap>
