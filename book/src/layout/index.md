# Layout & Scrolling

Layout is a modifier argument to a container: `row` and `column` take a layout, and `rowWith`/`columnWith` take a `Layout -> Layout` function first. Modifiers compose with `(.)`, and **when two set the same property, the leftmost wins**.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Layout.hs:modifiers}}
```

Logical pixels, origin top-left, `y` downward. Backends handle DPI scaling.

Each example below expands to its code and the frame it renders. The frames add `padAll 14` and fill the window so the spacing shows.

## Containers

A container is a `NanoUI` action that arranges its children. The plain form (`row`) uses the inherited layout; the `...With` form takes a modifier function.

<details class="nano-example">
<summary><code>row</code> / <code>rowWith</code>: children left to right</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:row}}
```

<img class="nano-example-shot" src="../images/layout/row.svg" alt="Three buttons in a horizontal row.">

</details>

<details class="nano-example">
<summary><code>column</code> / <code>columnWith</code>: children top to bottom</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:column}}
```

<img class="nano-example-shot" src="../images/layout/column.svg" alt="Three full-width buttons stacked vertically.">

</details>

<details class="nano-example">
<summary><code>grid</code> / <code>gridWith</code>: rows of equal columns</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:grid}}
```

<img class="nano-example-shot" src="../images/layout/grid.svg" alt="Six buttons in a three-column grid.">

</details>

<details class="nano-example">
<summary><code>panel</code> / <code>panelWith</code>: a styled surface</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:panel}}
```

<img class="nano-example-shot" src="../images/layout/panel.svg" alt="A panel with a heading and a muted line of text.">

</details>

<details class="nano-example">
<summary><code>card</code>: a full-width panel with a minimum width</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:card}}
```

<img class="nano-example-shot" src="../images/layout/card.svg" alt="A full-width card showing a build result.">

</details>

<details class="nano-example">
<summary><code>callout</code>: a panel tinted by a semantic colour</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:callout}}
```

<img class="nano-example-shot" src="../images/layout/callout.svg" alt="An amber callout warning about an expiring certificate.">

</details>

<details class="nano-example">
<summary><code>layers</code>: children stacked in one box, later on top</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:layers}}
```

<img class="nano-example-shot" src="../images/layout/layers.svg" alt="A button with a small red count badge stacked at its top right.">

</details>

<details class="nano-example">
<summary><code>center</code>: content centred on both axes</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:center}}
```

<img class="nano-example-shot" src="../images/layout/center.svg" alt="A label centred in a large panel.">

</details>

<details class="nano-example">
<summary><code>responsive</code> / <code>responsiveRowCol</code>: one layout when wide, another when narrow</summary>

`responsive breakpoint wide narrow body` picks a container by window width; `responsiveRowCol` is the common row/column pair.

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:responsive}}
```

A 420 px window:

<img class="nano-example-shot" src="../images/layout/responsive-narrow.svg" alt="Sidebar and Content stacked vertically at 420 pixels wide.">

The same view at 760 px:

<img class="nano-example-shot" src="../images/layout/responsive-wide.svg" alt="Sidebar and Content side by side at 760 pixels wide.">

</details>

<details class="nano-example">
<summary><code>mouseArea</code>: hover and clicks for a whole region</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:mousearea}}
```

<img class="nano-example-shot" src="../images/layout/mousearea.svg" alt="A card containing two lines, wrapped in a mouse area.">

</details>

<details class="nano-example">
<summary><code>separator</code>: a one-pixel rule</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:separator}}
```

<img class="nano-example-shot" src="../images/layout/separator.svg" alt="A horizontal rule between two labels.">

</details>

<details class="nano-example">
<summary><code>spacer</code>: flexible space between siblings</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:spacer}}
```

<img class="nano-example-shot" src="../images/layout/spacer.svg" alt="Left and Right labels pushed to opposite ends of a row.">

</details>

## Modifiers

Modifiers are `Layout -> Layout` functions. The leftmost wins when two set the same property, so `tight . padAll 14` has no padding while `padAll 14 . tight` keeps it.

### Sizing

<details class="nano-example">
<summary><code>fillW</code>: take all remaining width</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:fillw}}
```

<img class="nano-example-shot" src="../images/layout/fillw.svg" alt="A fixed-width button beside a button filling the rest of the row.">

</details>

<details class="nano-example">
<summary><code>fillH</code>: take all remaining height</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:fillh}}
```

<img class="nano-example-shot" src="../images/layout/fillh.svg" alt="One panel filling a fixed-height row and one panel at its natural height.">

</details>

<details class="nano-example">
<summary><code>grow</code>: fill both axes (<code>fillW . fillH</code>)</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:grow}}
```

<img class="nano-example-shot" src="../images/layout/grow.svg" alt="A small fixed panel beside a panel growing to fill the remaining space.">

</details>

<details class="nano-example">
<summary><code>fixedW</code>: an exact width</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:fixedw}}
```

<img class="nano-example-shot" src="../images/layout/fixedw.svg" alt="Three buttons at 60, 110, and 170 pixels wide.">

</details>

<details class="nano-example">
<summary><code>fixedH</code>: an exact height</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:fixedh}}
```

<img class="nano-example-shot" src="../images/layout/fixedh.svg" alt="Three buttons at 30, 56, and 82 pixels tall.">

</details>

<details class="nano-example">
<summary><code>fixedWH</code>: an exact width and height</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:fixedwh}}
```

<img class="nano-example-shot" src="../images/layout/fixedwh.svg" alt="A small button and a larger button with explicit sizes.">

</details>

<details class="nano-example">
<summary><code>minW</code>: a lower width bound</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:minw}}
```

<img class="nano-example-shot" src="../images/layout/minw.svg" alt="A wide minimum-width button beside a natural-width button.">

</details>

<details class="nano-example">
<summary><code>maxW</code>: an upper width bound</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:maxw}}
```

<img class="nano-example-shot" src="../images/layout/maxw.svg" alt="A capped button beside an uncapped fill-width button.">

</details>

<details class="nano-example">
<summary><code>minH</code>: a lower height bound</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:minh}}
```

<img class="nano-example-shot" src="../images/layout/minh.svg" alt="A panel forced to 80 pixels tall beside one at its natural height.">

</details>

<details class="nano-example">
<summary><code>maxH</code>: an upper height bound</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:maxh}}
```

<img class="nano-example-shot" src="../images/layout/maxh.svg" alt="A panel capped at 50 pixels beside one filling a 100-pixel row.">

</details>

<details class="nano-example">
<summary><code>percent</code>: a percentage of the parent's width</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:percent}}
```

<img class="nano-example-shot" src="../images/layout/percent.svg" alt="Two panels sized to 30 and 60 percent of their row.">

</details>

<details class="nano-example">
<summary><code>aspect</code>: keep a width / height ratio</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:aspect}}
```

<img class="nano-example-shot" src="../images/layout/aspect.svg" alt="A wide panel with a 16 by 9 aspect ratio.">

</details>

### Spacing

<details class="nano-example">
<summary><code>padAll</code>: equal padding on all four edges</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:padall}}
```

<img class="nano-example-shot" src="../images/layout/padall.svg" alt="A panel inset by 20 pixels on every side inside another panel.">

</details>

<details class="nano-example">
<summary><code>padXY</code>: horizontal, then vertical, padding</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:padxy}}
```

<img class="nano-example-shot" src="../images/layout/padxy.svg" alt="A panel with wide horizontal padding and shallow vertical padding.">

</details>

<details class="nano-example">
<summary><code>padLeft</code>: left padding</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:padleft}}
```

<img class="nano-example-shot" src="../images/layout/padleft.svg" alt="A panel inset 48 pixels from the left.">

</details>

<details class="nano-example">
<summary><code>padRight</code>: right padding</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:padright}}
```

<img class="nano-example-shot" src="../images/layout/padright.svg" alt="A panel inset 48 pixels from the right.">

</details>

<details class="nano-example">
<summary><code>padTop</code>: top padding</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:padtop}}
```

<img class="nano-example-shot" src="../images/layout/padtop.svg" alt="A panel inset 40 pixels from the top.">

</details>

<details class="nano-example">
<summary><code>padBottom</code>: bottom padding</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:padbottom}}
```

<img class="nano-example-shot" src="../images/layout/padbottom.svg" alt="A panel inset 40 pixels from the bottom.">

</details>

<details class="nano-example">
<summary><code>padLRTB</code>: left, right, top, bottom padding</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:padlrtb}}
```

<img class="nano-example-shot" src="../images/layout/padlrtb.svg" alt="A panel with different padding on each side.">

</details>

<details class="nano-example">
<summary><code>gap</code>: space between children</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:gap}}
```

<img class="nano-example-shot" src="../images/layout/gap.svg" alt="Two rows with tight and wide spacing between their buttons.">

</details>

<details class="nano-example">
<summary><code>tight</code>: remove container padding, keep the gap</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:tight}}
```

<img class="nano-example-shot" src="../images/layout/tight.svg" alt="Two panels comparing normal padding with tight.">

</details>

### Alignment and wrapping

`alignStart`, `alignCenter`, and `alignEnd` place a child across a column. `alignTop`, `alignMid`, and `alignBottom` place it down a row. A child always aligns itself; putting `alignMid` on a row positions the row in its parent, not the row's contents.

<details class="nano-example">
<summary><code>alignStart</code>: the left edge</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:alignstart}}
```

<img class="nano-example-shot" src="../images/layout/alignstart.svg" alt="A button aligned to the left edge of a panel.">

</details>

<details class="nano-example">
<summary><code>alignCenter</code>: the horizontal centre</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:aligncenter}}
```

<img class="nano-example-shot" src="../images/layout/aligncenter.svg" alt="A button centred horizontally in a panel.">

</details>

<details class="nano-example">
<summary><code>alignEnd</code>: the right edge</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:alignend}}
```

<img class="nano-example-shot" src="../images/layout/alignend.svg" alt="A button aligned to the right edge of a panel.">

</details>

<details class="nano-example">
<summary><code>alignTop</code>: the top edge</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:aligntop}}
```

<img class="nano-example-shot" src="../images/layout/aligntop.svg" alt="A button aligned to the top of a tall row.">

</details>

<details class="nano-example">
<summary><code>alignMid</code>: the vertical centre</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:alignmid}}
```

<img class="nano-example-shot" src="../images/layout/alignmid.svg" alt="A button centred vertically in a tall row.">

</details>

<details class="nano-example">
<summary><code>alignBottom</code>: the bottom edge</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:alignbottom}}
```

<img class="nano-example-shot" src="../images/layout/alignbottom.svg" alt="A button aligned to the bottom of a tall row.">

</details>

<details class="nano-example">
<summary><code>alignBaseline</code>: share the first baseline</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:alignbaseline}}
```

<img class="nano-example-shot" src="../images/layout/alignbaseline.svg" alt="A large and a small label lined up on their baselines.">

</details>

<details class="nano-example">
<summary><code>wrap</code>: start a new line when a child does not fit</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:wrap}}
```

<img class="nano-example-shot" src="../images/layout/wrap.svg" alt="Buttons wrapped onto two lines.">

</details>

<details class="nano-example">
<summary><code>lineGap</code>: space between wrapped lines</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:linegap}}
```

<img class="nano-example-shot" src="../images/layout/linegap.svg" alt="Two wrapped rows comparing a small and a large line gap.">

</details>

<details class="nano-example">
<summary><code>lineAlign</code>: align wrapped lines in the container</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:linealign}}
```

<img class="nano-example-shot" src="../images/layout/linealign.svg" alt="Wrapped buttons with the final line centred.">

</details>

### Positioning and pointer behavior

`pinAt` takes a child out of the flow. `pointer` decides whether an overlapping region consumes pointer events (`PointerBlock`) or passes them to what is underneath (`PointerPass`). The pointer examples park the pointer over the overlay.

<details class="nano-example">
<summary><code>pinAt</code>: position absolutely inside the parent</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:pinat}}
```

<img class="nano-example-shot" src="../images/layout/pinat.svg" alt="A count badge pinned to the top-right corner of a panel.">

</details>

<details class="nano-example">
<summary><code>pointer PointerBlock</code>: consume pointer events over a region</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:pointerblock}}
```

<img class="nano-example-shot" src="../images/layout/pointer-block.svg" alt="A dimming scrim over a button labelled Open logs.">

</details>

<details class="nano-example">
<summary><code>pointer PointerPass</code>: let pointer events through</summary>

```haskell
{{#include ../../../packages/nano-ui-svg/app/LayoutExamples.hs:pointerpass}}
```

<img class="nano-example-shot" src="../images/layout/pointer-pass.svg" alt="A decorative label over a button that still receives hover.">

</details>

## Scrolling

A scroll container needs a bounded viewport on its scrolling axis:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Layout.hs:scroll}}
```

Without a bounded ancestor, `grow` can leave a scroller with nothing to size against.

- `scrollWith` returns the body's result.
- `scrollArea` also returns the scroller's `WidgetId`, for programmatic control.
- `scroll2DWith` / `scrollArea2D` scroll both axes.
- A scroller that fills both axes (`grow`) drops its well and paints the page
  background (`pageBackground`, `themeWindow` by default). Scope a view in
  `styled panelPage` for a page on the panel surface.

### Virtualization

For 10,000+ rows, build only the visible slice and use spacers for the rest. Capture the scroller id and read the previous layout with `getScrollMetricsUi`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Layout.hs:virtualized}}
```

`scrollViewport` excludes padding and the scrollbar lane; use it, not the container rectangle, for the visible height. `scrollOffset` is the current offset. The metrics read registers a layout dependency, so a resize triggers another frame.

See `packages/nano-ui-demo/examples/LongList.hs` (100,000 rows) and `Logs.hs` (sticky-to-bottom).

### Programmatic scrolling and wheel tuning

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Layout.hs:reveal}}
```

From a view: `scrollToUi`, `scrollByUi`, `scrollPagesUi`, `scrollRectIntoViewUi`, `setScrollOffsetUi`, `setScrollStepUi`. The `Context` variants (`scrollTo`, `getScrollOffset2D`, ...) are for `IO`.

One wheel notch scrolls 60 logical pixels by default. For fixed-height rows, set the step so a notch is one row (`askContext` is in `NanoUI.Monad`):

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Layout.hs:wheel_step}}
```

`setScrollStepUi sid px` overrides one scroller; `0` restores the context step.

## Nesting and chrome

`scrollArea`, `panel`, and `card` each draw a background, border, and radius. Nesting them inside a popup or dialog draws a box in a box. Let one level own the chrome: flatten the inner surface with `styled (inputStyle (background colorTransparent . borderWidth 0))`, or give the outer dialog `padAll 0` and pad the inner body.

See [Styling Gotchas](../styling/gotchas.md) and [Troubleshooting](../troubleshooting/recurring-bugs.md) for the recurring cases.
