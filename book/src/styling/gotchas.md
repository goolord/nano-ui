# Styling Gotchas

A few widgets route their theme slots in ways that catch newcomers off guard. Each card below shows the unexpected result, explains why it happens, and gives the idiomatic fix.

## `checkbox` and `radio` span three surfaces

The unchecked interior comes from `themeInput`, the outline and label come from `themeButton` (or `themePopup` inside a modal), and the checked fill comes from `themeAccent`.

<nano-ui-trap id="trap-01"></nano-ui-trap>

## `select` and `comboBox` use different surfaces

Even though they look similar when closed, `select` reads `themeButton` while `comboBox` reads `themeInput`.

<nano-ui-trap id="trap-02"></nano-ui-trap>

## `scrollWith` changes surface depending on its size

A bounded scroller draws a bordered `themeInput` well. A `grow` scroller assumes it is a full-page container, drops the border, and fills with the page background instead.

<nano-ui-trap id="trap-03"></nano-ui-trap>

## `callout` tints nested panels

Because `callout` applies a tinted `panelStyle` to its subtree, any `panel` or `card` inside it inherits that tint unless you restore the base panel style.

<nano-ui-trap id="trap-04"></nano-ui-trap>

## `toggleButton` ignores `buttonStyle` when active

When turned on, `toggleButton` paints itself using its tone color (`themeAccent` for `Accent`) instead of `buttonStyle`.

<nano-ui-trap id="trap-05"></nano-ui-trap>

## `borderWidth` does not reserve layout padding

Borders are stroked inset inside a widget's bounds and never push child content inward, so thick borders need matching padding.

<nano-ui-trap id="trap-06"></nano-ui-trap>

## Modifier order in `(.)` vs. nested `styled`

In a `(.)` chain, the leftmost modifier wins. When nesting `styled` calls, the innermost scope wins.

<nano-ui-trap id="trap-07"></nano-ui-trap>

## `tabs` read `themePanel`, not `themeButton`

Although tab headers behave like buttons, their colors come from `themePanel` and `themeAccent`.

<nano-ui-trap id="trap-08"></nano-ui-trap>
