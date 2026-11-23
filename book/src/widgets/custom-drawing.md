# Custom Drawing & Controls

When the built-in widgets do not cover what you need, you can draw directly with the 2D canvas API. The six controls in `NanoUI.Widgets.Custom` are built on this same API and double as clean templates for your own widgets.

## Built-in custom controls

`NanoUI` re-exports all six from `NanoUI.Widgets.Custom`:

| Control | Signature | Default size | Notes |
| --- | --- | --- | --- |
| `knob` | `Float -> Float -> Var Float -> NanoUI ()` | 36x36 | Min, max, value; drag vertically, scroll wheel, or use arrow keys |
| `toggleSwitch` | `Var Bool -> NanoUI ()` | 44x24 | Toggle with click, Space, or Enter |
| `circularProgress` | `Float -> NanoUI ()` | 32x32 | Progress fraction in `[0, 1]` |
| `progressBar` | `Float -> NanoUI ()` | full width, 12 high | Progress fraction in `[0, 1]` |
| `spinner` | `NanoUI ()` | 18x18 | Indeterminate spinner; wakes the loop at 60 Hz while visible |
| `sparkline` | `[Float] -> NanoUI ()` | 80x24 | Auto-scales vertically to the input range |

Every control provides a primed form (`knob'`, `toggleSwitch'`, and so on) returning a `Response`, plus a `...With` variant accepting a layout modifier and custom dimensions:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/CustomDrawing.hs:with_forms}}
```

`sparkline` stretches the minimum value to the bottom of its plot box and the maximum to the top, so series of different scales remain readable in a tight strip. Here is a sparkline at its default size next to a wider `sparklineWith`:

```haskell
{{#include ../../../packages/nano-ui-svg/app/ChartExamples.hs:sparkline}}
```

<img src="../images/charts/sparkline.svg" alt="Two small boxed blue line graphs of the same wavy series, the second wider and taller.">

If you need axes, legends, or hover readouts, use `plot` from `nano-ui-diagrams` instead (see [Charts and diagrams](../packages/index.md#charts-and-diagrams)).

Here is a compact settings row combining a knob and a live readout:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/CustomDrawing.hs:settings_row}}
```

Pinning the numeric label with `fixedW` prevents the row from jittering as digits change, and reading the `Var` *below* the knob keeps drags to a single pass.

`spinner` paces itself with `wakeAfter` (60 steps per second) only while it is visible on screen; once scrolled or clipped out of view, it stops requesting frames. Avoid calling a global `keepAnimating`, which would keep the event loop spinning even when no spinner is on screen.

### How custom controls are structured

Each control in `NanoUI.Widgets.Custom` follows the same four steps:

1. Claim an ID with `freshWidget`.
2. Size and render it via `CustomWidgetSpec` and `runCanvasFor`.
3. Handle user input with `useDrag2DOn`, `useWheelDeltaOn`, and `useKeyNav`.
4. Commit edits back to the caller's `Var` with `bindVar` or `finishInput`.

## Canvas drawing

`canvas` reserves a rectangle using a layout modifier and runs a `CanvasM` block inside it. The rectangle passed to your drawing callback is in window coordinates, so draw relative to its `Rect x y w h`:

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:import_path}}

{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:badge}}
```

<img src="../images/canvas/badge.svg" alt="A blue rounded badge with a white ring on the left and the word Ready.">

`canvas` returns a `Response`; wrap it in `void` if your drawing is purely decorative. `canvasConfigured` adds content-key caching, custom cursor shapes, and pointer tracking. When you need full control over sizing, keyboard handling, or focus, drop down to `customWidget` with a `CustomWidgetSpec`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/CustomDrawing.hs:custom_widget}}
```

Inside `widgetDraw`, `runCanvasFor` adjusts curve tessellation automatically for the display's DPI scale, and `drawContext` / `cdcTheme` give you the active `Theme`, font metrics, and hover/press state.

### Reading the canvas examples

The visual reference below comes straight from [`CanvasExamples.hs`](https://github.com/goolord/nano-ui/blob/main/packages/nano-ui-svg/app/CanvasExamples.hs), rendered directly to SVG by `nano-ui-svg-glossary`. Each list pairs a label with a drawing callback, sharing these colors and helper:

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:palette}}
```

Thin gray guides, dots, dashed outlines, and red crosshairs in the images below are reference annotations overlaid by the generator, not part of the snippets themselves.

### Rectangles, circles, and gradients

The basic drawing primitives take a rectangle or a center and radius, with no `NanoUI.Path` import required.

<img src="../images/canvas/primitives.svg" alt="Eight cells: a blue rectangle, a teal rounded rectangle, an amber disc, a rose ring, a diagonal line, a horizontal blue-to-teal gradient, a vertical amber-to-rose gradient, and a four-color corner gradient.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:primitives}}
```

`drawStrokeCircle` insets its stroke inside the radius. `drawStrokeAA` draws an anti-aliased segment with square ends, while `drawStroke` skips anti-aliasing for crisp horizontal or vertical rules. `drawQuadGradient` takes its four corner colors clockwise starting from the top-left.

### Paths

A `Path` is a sequence of commands combined with `<>`. Start a subpath with `moveTo`, extend it with `lineTo`, `quadTo`, `cubicTo`, and `arc`, and connect back to the start with `close`. Below, each path is stroked with `drawStrokePath (path r) 3 blue`; solid dots mark path vertices, while hollow rings mark control points and arc centers.

<img src="../images/canvas/segments.svg" alt="Five cells: a two-segment peak, a quadratic curve with its control point above, an S-shaped cubic curve with two control points, a three-quarter arc, and the same arc closed through its center into a pie slice.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:segments}}
```

Angles are in radians. Because `y` increases downward, positive angles sweep clockwise: `0` points to 3 o'clock, so a positive sweep of `3 * pi / 2` runs three-quarters of the way around clockwise. Calling `arc` after `moveTo` draws a straight line from the current point to the start of the arc, which makes pie wedges trivial.

Shape constructors build complete standalone subpaths. They never connect to prior subpaths, so joining them with `<>` builds a single multi-subpath shape:

<img src="../images/canvas/shapes.svg" alt="Seven cells, each a translucent blue fill with a blue outline: a rectangle, a rounded rectangle, a rectangle with two large and two small corner radii, a circle, a wide ellipse, a triangle, and an open zigzag line.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:shapes}}
```

`roundedRectCorners` takes corner radii in CSS `border-radius` order: top-left, top-right, bottom-right, bottom-left. `polygon` closes the contour while `polyline` leaves it open (so the polyline is only stroked above). Filling a path implicitly closes every subpath.

### Fill rules

`drawPath` fills using `P.NonZero`, while `drawPathWith` lets you choose both the fill rule and paint. All three cells below fill three concentric circles:

<img src="../images/canvas/fill-rules.svg" alt="Three cells of teal concentric circles: NonZero fills a solid disc; NonZero with the middle circle reversed and EvenOdd both show a ring around a dot.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:fill_rules}}
```

Because `P.circle` always winds clockwise, all three circles add together under `NonZero` to produce a solid disc. Reversing the middle circle cancels out its interior and punches a hole. `EvenOdd` ignores winding direction and simply alternates filled and empty as subpaths nest. Note that self-intersecting or crossing subpaths are not resolved against each other, so keep cutouts strictly inside their enclosing contour.

### Paints

`P.Solid` fills with a flat color. `P.Linear from to stops` interpolates color stops along the segment from `from` (offset `0`) to `to` (offset `1`):

<img src="../images/canvas/paints.svg" alt="Four cells: a solid blue rounded rectangle, a blue-to-teal gradient across the full width, the same gradient squeezed into the middle third with solid ends, and a disc shading amber to rose to blue along its diagonal.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:paints}}
```

Outside the first and last stops, the gradient clamps to the endpoint colors (visible in the middle-third example). Strokes accept `Paint` values too, so outlines can use gradients just like fills.

### Strokes

`drawStrokePathWith` accepts a `P.Stroke` record. Start with `P.stroke width` (butt caps, miter joins, solid line) and update the fields you need. Path strokes are always centered on the path geometry.

Line caps control how open path ends and individual dashes terminate. Compared against the thin path guide, `ButtCap` stops flush at the endpoints, while `RoundCap` and `SquareCap` extend past them by half the stroke width:

<img src="../images/canvas/caps.svg" alt="Three thick blue lines over a thin guide line: ButtCap ends flush with the end points, RoundCap ends in semicircles past them, SquareCap ends in squares past them.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:caps}}
```

Line joins control corners between consecutive segments. If a sharp miter point would stretch farther than `strokeMiterLimit * width`, it falls back to a flat bevel (demonstrated in the fourth cell with a miter limit of `1.5`):

<img src="../images/canvas/joins.svg" alt="Four thick teal zigzags over a thin guide: a pointed miter corner, a rounded corner, a cut-off bevel corner, and a miter beveled by a low miter limit.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:joins}}
```

`strokeDash` takes alternating dash and gap lengths that repeat along each subpath, and `strokeDashOffset` shifts where the pattern begins. Combining a zero-length dash with `RoundCap` produces round dots:

<img src="../images/canvas/dashes.svg" alt="Four amber dashed rounded rectangles: even dashes, the same dashes shifted along, a long-short dash-dot pattern, and a row of round dots.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:dashes}}
```

### Transforms

`withTransform` applies a `P.Transform` to everything drawn inside its block. Transforms compose with `<>` right-to-left (the right-hand transform runs first). The dashed outline below shows the arrow's untransformed position:

<img src="../images/canvas/transforms.svg" alt="Four cells with a teal arrow and a dashed outline of its starting position: moved right and down, rotated 45 degrees about its center, scaled 1.5 times about its center with a thicker outline, and mirrored to point left.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:transforms}}
```

`P.rotate` and `P.scale` pivot around the window origin `(0, 0)`. To pivot around a shape's center instead, translate it to the origin first (`P.rotateAround` does this for rotation, and `scaleAround` above shows the equivalent for scaling). Stroke widths and dash patterns scale with the transform, which is why the scaled arrow has a thicker outline. Text positions and scales under transforms, though individual glyphs do not rotate.

### Clipping

`withClip rect` restricts drawing inside its block to the intersection of `rect` and the active clip region. Every canvas starts clipped to its own widget bounds, so drawings can never accidentally spill outside their widget.

<img src="../images/canvas/clip.svg" alt="Two cells with a rose disc and the text 'text past the edge': without a clip both spill past a dashed box; with withClip they are cut at the box.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:clip}}
```

### Text

`drawText pos alignX alignY txt color` places text relative to an anchor point (the red dot below). Alignment specifies which part of the text sits on that point: `AlignStart` anchors the left edge, `AlignTop` anchors the top of the line box, and `AlignBaseline` anchors the text baseline. Use `drawTextWith` with a `TextFont` to customize size, variant, weight, or slant.

<img src="../images/canvas/text.svg" alt="Eight cells showing the word Anchor placed against a red dot: left, center and right aligned; then top, middle, baseline and bottom aligned; and a large bold amber 42.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:text}}
```

Measure strings inside the view using `lineWidthUi` and `uiFontMetrics` (or trim them to fit with `truncateTextUi`) and pass those measurements into your drawing callback. Both helpers ensure the needed glyphs are loaded first. The pure `lineWidth` is only exact for text that has already been measured or drawn; otherwise it falls back to summing per-glyph advances, which drifts from HarfBuzz's unrounded shaped runs over long strings.

### Borders: inset vs. centered

`drawStrokeRoundedRect` draws its entire border inside the given rectangle. By contrast, `drawStrokePath` centers the stroke on the path edge, pushing half the border width outside the rectangle. The thin guide below shows the exact rectangle passed to each call:

<img src="../images/canvas/borders.svg" alt="Two blue rounded borders over a thin guide rectangle: drawStrokeRoundedRect sits entirely inside it, drawStrokePath straddles it.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:borders}}
```

Always use `drawStrokeRoundedRect` (or inset your path by half the stroke width) when stroking a widget's outer border so the edges do not get clipped by the widget bounds.

### Content keys and caching

Without a `widgetContent` key, `nano-ui` rebuilds and diffs a custom widget's draw ops on every frame. Adding a content key lets unchanged frames skip both rebuild and repaint, **as long as the key includes every input the drawing reads**:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/CustomDrawing.hs:content_key}}
```

If you forget an input in the key, the canvas will freeze on stale pixels until something else forces a rebuild. See [Stale cached drawings](../troubleshooting/recurring-bugs.md#4-stale-cached-drawings).

A drawing whose layout bounds never change (such as a drag ghost moved via `pinAt` under the pointer) receives no layout diff when it moves. Give it an explicit version hash covering whatever drives its movement:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/CustomDrawing.hs:versioned}}
```

Without a version key, it stays frozen on the draw ops from its very first frame.

## SVG icons

Parse inline SVG markup with `parseSvg` or load a file once at startup with `loadSvg`. Any stroke or fill using `currentColor` automatically inherits the active text color (so `fontColor` and semantic tones tint it cleanly), while hard-coded colors in the SVG stay untouched:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/CustomDrawing.hs:svg_icons}}
```

`svgIconConfigured` takes an `ImageConfig` for CSS-style fitting (`FitContain`, `FitCover`, `FitFill`).

To paint an SVG inside a custom `canvas`, rasterize it with `rasterizeSvg` from `NanoUI.Svg`, upload it with `useImageRgba`, and draw the resulting `ImageId`. Make sure the lease key covers everything that changes the rasterized pixels (like the tint color):

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/CustomDrawing.hs:svg_canvas}}
```

For going in the other direction (exporting `nano-ui` draw ops or entire frames to SVG), see [SVG export](../packages/index.md#svg-export).
