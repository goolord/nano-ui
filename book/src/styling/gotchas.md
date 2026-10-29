# Styling Gotchas

Styling results that surprise people. Each specimen shows the trap, why it happens, and the fix.

## `checkbox` and `radio` span three surfaces

The unchecked well reads `themeInput`, the outline and label read `themeButton`, and the checked state reads `themeAccent`.

<nano-ui-trap id="trap-01"></nano-ui-trap>

## `select` and `comboBox` use different surfaces

They look alike when closed, but `select` reads `themeButton` and `comboBox` reads `themeInput`.

<nano-ui-trap id="trap-02"></nano-ui-trap>

## `scrollWith` changes surface with its sizing

A bounded scroller draws a well. A `grow` scroller drops it and fills the page background.

<nano-ui-trap id="trap-03"></nano-ui-trap>

## `callout` tints nested panels

The tinted `panelStyle` a callout sets reaches every panel inside it.

<nano-ui-trap id="trap-04"></nano-ui-trap>

## `toggleButton` ignores `buttonStyle` when on

On, it fills with its tone colour (`themeAccent` for `Accent`), whatever `buttonStyle` says.

<nano-ui-trap id="trap-05"></nano-ui-trap>

## `borderWidth` does not add layout space

Borders are drawn inset and never grow a widget's bounds.

<nano-ui-trap id="trap-06"></nano-ui-trap>

## Modifier order in `(.)` and `styled`

The leftmost layout modifier wins; nested `styled` scopes apply outside in, so the inner one wins.

<nano-ui-trap id="trap-07"></nano-ui-trap>

## `tabs` read `themePanel`, not `themeButton`

Tab headers are buttons, but their colours come from `themePanel` and `themeAccent`.

<nano-ui-trap id="trap-08"></nano-ui-trap>
