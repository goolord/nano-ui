# Custom Drawing & Controls

When the built-in widgets do not fit, draw the control yourself with the 2D canvas API.

The widgets in `NanoUI.Widgets.Custom` (`knob`, `toggleSwitch`, `circularProgress`, `progressBar`, `spinner`, `sparkline`) are designed to serve as **reference implementations** for custom widgets users may want to implement themselves. This API is meant to **accompany the canvas API**, demonstrating how 2D vector drawing, layout sizing, gesture hooks (`useDrag2DOn`, `useWheelDeltaOn`), keyboard navigation, and responsive state binding (`bindVar`) integrate into cohesive, interactive controls.

## Reference custom controls

```haskell
import NanoUI.Widgets.Custom
```

| Control | Signature | Default size | Notes |
| --- | --- | --- | --- |
| `knob` | `Float -> Float -> Var Float -> NanoUI ()` | 36×36 | Min, max, value; vertical drag, wheel, arrows |
| `toggleSwitch` | `Var Bool -> NanoUI ()` | 44×24 | Click, Space, or Enter toggles |
| `circularProgress` | `Float -> NanoUI ()` | 32×32 | Fraction in `[0, 1]` |
| `progressBar` | `Float -> NanoUI ()` | fills width, 12 high | Fraction in `[0, 1]` |
| `spinner` | `NanoUI ()` | 18×18 | Indeterminate; requests animation frames while declared |
| `sparkline` | `[Float] -> NanoUI ()` | 80×24 | Scales to the values' range |

Each has a primed form returning `Response` (`knob'`, `toggleSwitch'`, ...) and a `...With` form that takes a layout modifier and size parameters:

```haskell
knobWith alignMid 48 0 100 volume
progressBarWith fillW 6 progress
sparklineWith (fixedWH 120 32) 120 32 samples
```

`sparkline` scales the values to their own range, so it shows the shape of a series, not its size. At its default size and with `sparklineWith`:

```haskell
{{#include ../../../packages/nano-ui-svg/app/ChartExamples.hs:sparkline}}
```

<img src="../images/charts/sparkline.svg" alt="Two small boxed blue line graphs of the same wavy series, the second wider and taller.">

For a chart with axes, a legend, and hover, use `plot` from `nano-ui-diagrams` (see [Ecosystem Packages](../packages/index.md#charts-and-diagrams)).

A settings row:

```haskell
settingsRow :: Var Bool -> Var Float -> NanoUI ()
settingsRow enabled volume = rowWith (gap 12 . alignMid) $ do
  toggleSwitch enabled
  labelWith (alignMid . fixedW 64) "Enabled"
  knob 0 100 volume
  v <- readVar volume
  labelWith (alignMid . fontMono . fixedW 48) (T.pack (show (round v :: Int)))
```

The `fixedW` on the changing number keeps the controls from shifting. Read the value *below* the knob so a drag resolves in one pass.

`spinner` calls `keepAnimating` internally while it is drawn. Do not add a global `keepAnimating`; that would keep the loop running when the spinner is not visible.

### Implementing custom controls

You can inspect the source code of `NanoUI.Widgets.Custom` as a blueprint for implementing your own controls. A typical interactive control follows this pattern:

1. **Allocate widget identity**: `(wid, ctx) <- freshWidget` obtains a unique `WidgetId`.
2. **Define geometry & drawing**: Specify layout constraints and vector draw ops via `CustomWidgetSpec` and `runCanvasFor`.
3. **Capture gestures & input**: Use gesture hooks (`drag <- useDrag2DOn resp`, `(_, scrollY) <- useWheelDeltaOn resp`, `nav <- useKeyNav wid`) to compute value deltas.
4. **Synchronize state**: Apply `bindVar` or `finishInput` to bind the pure calculation to the caller's mutable `Var`.

## Canvas drawing

`canvas` sizes a rectangle with a layout modifier and runs a `CanvasM` drawing in it. The rectangle is the widget's place in the window, so draw relative to it:

```haskell
import NanoUI.Path qualified as P

{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:badge}}
```

<img src="../images/canvas/badge.svg" alt="A blue rounded badge with a white ring on the left and the word Ready.">

`canvas` returns the widget's `Response`; discard it with `void` when the drawing needs no hover or clicks. `canvasConfigured` adds a content key, a cursor, and pointer tracking. For full control, use `customWidget` with a `CustomWidgetSpec`:

```haskell
(resp, ()) <- customWidget defaultCustomWidgetSpec
  { widgetLayout = fixedWH 48 48
  , widgetContent = contentKey [angle, volume]
  , widgetDraw = \cdc (Rect x y w h) -> runCanvasFor cdc $ do
      drawCircle (V2 (x + w / 2) (y + h / 2)) 16 (themeAccent (cdcTheme cdc))
  , widgetCursor = Just (\_ _ _ -> UiCursorPointer)
  }
```

`runCanvasFor` adapts tessellation to the display scale inside `widgetDraw`. `drawContext` / `cdcTheme` supply the active theme, font, hover, and press state.

### Reading the examples

The rest of this section is compiled from [`CanvasExamples.hs`](https://github.com/goolord/nano-ui/blob/main/packages/nano-ui-svg/app/CanvasExamples.hs), and `nano-ui-svg-glossary` renders the same definitions to the images. Each list pairs a caption with a drawing, and the drawing gets one cell's rectangle. They share a palette and a helper:

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:palette}}
```

Thin grey lines, small dots, dashed outlines, and the red crosshair in the images are annotations drawn on top. They are not part of the example code.

### Rectangles, circles, and gradients

The basic ops take a rectangle or a centre and radius. They need no `NanoUI.Path` import.

<img src="../images/canvas/primitives.svg" alt="Eight cells: a blue rectangle, a teal rounded rectangle, an amber disc, a rose ring, a diagonal line, a horizontal blue-to-teal gradient, a vertical amber-to-rose gradient, and a four-colour corner gradient.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:primitives}}
```

`drawStrokeCircle` keeps its border inside the radius. `drawStrokeAA` is an anti-aliased line with square ends; `drawStroke` is the same line without anti-aliasing, for axis-aligned rules. `drawQuadGradient` takes its colours clockwise from the top left.

### Paths

A `Path` is a list of steps joined with `<>`. `moveTo` starts a subpath. `lineTo`, `quadTo`, `cubicTo`, and `arc` extend it from the current point, and `close` draws a line back to where it started. Each path below is stroked with `drawStrokePath (path r) 3 blue`. Filled dots are points on the path; hollow dots are control points and arc centres.

<img src="../images/canvas/segments.svg" alt="Five cells: a two-segment peak, a quadratic curve with its control point above, an S-shaped cubic curve with two control points, a three-quarter arc, and the same arc closed through its centre into a pie slice.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:segments}}
```

Angles are in radians. Because y points down, a positive sweep turns clockwise on screen: the arc starts at 3 o'clock and runs three quarters of the way round. An `arc` after `moveTo` joins the current point to its start with a line, which is how the last cell draws a pie slice.

Shape helpers build whole subpaths. They never join onto what comes before them, so `<>` combines them into one path with several subpaths.

<img src="../images/canvas/shapes.svg" alt="Seven cells, each a translucent blue fill with a blue outline: a rectangle, a rounded rectangle, a rectangle with two large and two small corner radii, a circle, a wide ellipse, a triangle, and an open zigzag line.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:shapes}}
```

`roundedRectCorners` takes its radii in CSS `border-radius` order: top left, top right, bottom right, bottom left. `polygon` closes its outline and `polyline` leaves it open, so the polyline is only stroked here. A fill closes every subpath.

### Fill rules

`drawPath` fills with `P.NonZero`. `drawPathWith` takes the rule and a paint. All three cells fill three concentric circles:

<img src="../images/canvas/fill-rules.svg" alt="Three cells of teal concentric circles: NonZero fills a solid disc; NonZero with the middle circle reversed and EvenOdd both show a ring around a dot.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:fill_rules}}
```

`P.circle` always runs clockwise, so under `NonZero` the inner circles add to the outer one and the disc is solid. Running the middle circle the other way cancels it out, cutting a hole. `EvenOdd` ignores direction: each nested subpath toggles between filled and empty. Crossing subpaths are not resolved against each other, so keep holes inside the shape they cut.

### Paints

`P.Solid` is one colour. `P.Linear from to stops` is a gradient along the line from `from` (offset 0) to `to` (offset 1).

<img src="../images/canvas/paints.svg" alt="Four cells: a solid blue rounded rectangle, a blue-to-teal gradient across the full width, the same gradient squeezed into the middle third with solid ends, and a disc shading amber to rose to blue along its diagonal.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:paints}}
```

Past the first and last stop the gradient keeps their colours, which the short span shows. Strokes take a `Paint` too, so a gradient can run along an outline.

### Strokes

`drawStrokePathWith` takes a `P.Stroke`. Start from `P.stroke width` (butt caps, miter joins, solid) and override fields with record update. Strokes are centred on the path.

Caps finish the ends of open subpaths and of every dash. The thin line is the path; butt caps stop at its end points, round and square caps extend past them by half the width.

<img src="../images/canvas/caps.svg" alt="Three thick blue lines over a thin guide line: ButtCap ends flush with the end points, RoundCap ends in semicircles past them, SquareCap ends in squares past them.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:caps}}
```

Joins shape the corners between segments. A miter longer than `strokeMiterLimit` times the width falls back to a bevel, which the last cell forces with a limit of 1.5.

<img src="../images/canvas/joins.svg" alt="Four thick teal zigzags over a thin guide: a pointed miter corner, a rounded corner, a cut-off bevel corner, and a miter beveled by a low miter limit.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:joins}}
```

`strokeDash` lists alternating dash and gap lengths, repeated along each subpath. `strokeDashOffset` starts the pattern part of the way in. A zero-length dash with round caps draws a dot.

<img src="../images/canvas/dashes.svg" alt="Four amber dashed rounded rectangles: even dashes, the same dashes shifted along, a long-short dash-dot pattern, and a row of round dots.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:dashes}}
```

### Transforms

`withTransform` draws a block through a `P.Transform`. Transforms compose with `<>`, and the right-hand one applies first. The dashed outline is the arrow before the transform.

<img src="../images/canvas/transforms.svg" alt="Four cells with a teal arrow and a dashed outline of its starting position: moved right and down, rotated 45 degrees about its centre, scaled 1.5 times about its centre with a thicker outline, and mirrored to point left.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:transforms}}
```

`P.rotate` and `P.scale` work about the window origin, not the widget. Move the pivot to the origin first: `P.rotateAround` does that for rotation, and `scaleAround` above does it for scale. Stroke widths and dashes scale with the transform, so the enlarged arrow's outline is thicker. Text moves its anchor and scales, but glyphs do not rotate.

### Clipping

`withClip rect` clips a block to `rect`, intersected with the current clip. The current clip is at most the widget's rectangle, so a canvas never draws outside its own bounds.

<img src="../images/canvas/clip.svg" alt="Two cells with a rose disc and the text 'text past the edge': without a clip both spill past a dashed box; with withClip they are cut at the box.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:clip}}
```

### Text

`drawText pos alignX alignY txt colour` anchors text to a point. The alignment says which part of the text sits on the point (the red dot): `AlignStart` puts the point at the left edge, `AlignTop` at the top of the line, `AlignBaseline` on the baseline. `drawTextWith` takes a `TextFont` for size, variant, weight, and slant.

<img src="../images/canvas/text.svg" alt="Eight cells showing the word Anchor placed against a red dot: left, centre and right aligned; then top, middle, baseline and bottom aligned; and a large bold amber 42.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:text}}
```

Measure text with `lineWidth` and `uiFontMetrics`. Measure a whole run with `lineWidth` rather than adding per-glyph advances, because a shaping backend lays runs out by unrounded advances.

### Borders: inset or centred

`drawStrokeRoundedRect` keeps its border inside the rectangle. A path stroke is centred on the path, so half its width falls outside. The thin outline is the rectangle both cells were given.

<img src="../images/canvas/borders.svg" alt="Two blue rounded borders over a thin guide rectangle: drawStrokeRoundedRect sits entirely inside it, drawStrokePath straddles it.">

```haskell
{{#include ../../../packages/nano-ui-svg/app/CanvasExamples.hs:borders}}
```

Use the inset form for a widget's own border, so it never paints past the widget's rectangle.

### Content keys and caching

Without `widgetContent`, a custom widget's draw ops are rebuilt and diffed every frame. With a key, unchanged frames skip rebuild and repaint, but the key must cover **everything the drawing reads**:

```haskell
(resp, ()) <- customWidget defaultCustomWidgetSpec
  { widgetLayout = fixedWH 48 48
  , widgetContent = contentKey [angle, volume, fromIntegral (round scrollY)]
  , widgetDraw = \cdc rect -> runCanvasFor cdc (drawGauge rect)
  }
```

A key that misses an input shows stale pixels. See [Styling Gotchas](../styling/gotchas.md).

A drawing whose layout rectangle never changes (a drag ghost pinned under the pointer with `pinAt`) gets no layout change when it moves. Give it an explicit version that includes what moves it:

```haskell
-- dragGhostOps :: Rect -> SmallArray DrawOp
drawingVersioned (contentKey [mx, my]) (fixedWH 32 32) dragGhostOps
```

Otherwise it keeps the ops of the frame it first appeared in.

## SVG & Vector Graphics

`nano-ui` includes vector SVG support at both the widget level and the canvas drawing level:

- **SVG icons**: Parse in-memory SVG strings (`parseSvg`) or load SVG files (`loadSvg`), then display them with `svgIcon` or in buttons.
- **Canvas SVG textures**: Rasterize SVGs to RGBA buffers with `rasterizeSvg` from `NanoUI.Svg` and draw them onto a canvas using `drawImage`.
- **Canvas to SVG export**: Export `DrawOp` sequences directly to SVG vector files using `nano-ui-svg` (`drawOpsToSvg`).

### 1. SVG Icons in Views

Monochrome SVG icons use `currentColor` and automatically inherit the active text color. This means they can be tinted with `fontColor` or semantic styles:

```haskell
import qualified Data.ByteString.Char8 as BS8
import NanoUI

-- Parse an inline SVG icon (e.g. 24x24 viewBox)
starIcon :: Svg
starIcon = case parseSvg "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24'><polygon points='12,2 15,9 22,9.5 16.5,14.5 18.5,21.5 12,17.5 5.5,21.5 7.5,14.5 2,9.5 9,9'/></svg>" of
  Right svg -> svg
  Left err  -> error ("Invalid SVG: " ++ err)

-- Render SVG icons with various sizes and theme colors
iconBar :: Svg -> NanoUI ()
iconBar icon = rowWith (gap 12 . alignMid) $ do
  theme <- uiTheme

  -- 1. Standard icon sizing (24x24 logical pixels)
  svgIcon 24 icon

  -- 2. Scoped styling and tinting via fontColor
  svgIconWith (fixedWH 32 32 . fontColor (themeAccent theme)) icon

  -- 3. In an icon button
  whenM (iconButton icon "Star") $
    putStrLn "Starred!"

  -- 4. Custom button content
  whenM (buttonContent $ rowWith (gap 6 . alignMid) $ do
            svgIcon 16 icon
            label "Favorite") $
    putStrLn "Favorited!"
```

Multi-color SVGs preserve their embedded fill and stroke colors. For flexible image-style scaling, `svgIconConfigured` accepts `ImageConfig` with CSS-like fitting (`FitContain`, `FitCover`, `FitFill`).

### 2. Drawing SVG inside Canvas

Within `CanvasM`, you can draw rasterized textures with `drawImage` / `drawImageUV` / `drawImageWith`. To draw an `Svg` onto a canvas, rasterize it to RGBA bytes using `rasterizeSvg` (or `rasterizeSvgIn`) from `NanoUI.Svg`:

```haskell
import NanoUI
import NanoUI.Svg (rasterizeSvg)
import NanoUI.Widgets.Custom (CanvasM, canvas, drawImage, drawRoundedRect, drawStrokeRoundedRect, drawText)

-- | A custom badge widget that draws an SVG icon alongside custom vector geometry.
customBadge :: ImageHandle () -> Svg -> Text -> NanoUI ()
customBadge imgHandle svg title = do
  theme <- uiTheme
  let iconPx = 32
      -- Rasterize the SVG to RGBA at target pixel dimensions with a tint color
      rgba = rasterizeSvg iconPx iconPx (themeAccent theme) svg

  -- Lease the rasterized texture in the backend atlas
  mIid <- useImageRgba imgHandle () iconPx iconPx rgba

  canvas (fixedWH 140 44) $ \(Rect x y w h) -> do
    -- Draw container geometry
    drawRoundedRect (Rect x y w h) 8 (styleBg (themeButton theme))
    drawStrokeRoundedRect (Rect x y w h) 8 1 (styleBorder (themeButton theme))

    -- Draw the rasterized SVG icon if registered
    case mIid of
      Just iid -> drawImage (Rect (x + 8) (y + 6) 32 32) iid
      Nothing  -> pure ()

    -- Draw accompanying text
    drawText (V2 (x + 48) (y + 22)) AlignStart AlignMiddle title (styleFg (themeButton theme))
```

### 3. Exporting Canvas Drawings to SVG (`nano-ui-svg`)

When you need vector screenshots, reports, or test fixtures, `nano-ui-svg` can export any `DrawOp` array generated by `runCanvas` or `runCanvasFor` directly to an SVG string:

```haskell
import qualified Data.Text.IO as TIO
import NanoUI
import NanoUI.Draw.Svg (defaultSvgConfig, drawOpsToSvg, renderDrawOpsSvg, SvgConfig (..))
import NanoUI.Path qualified as P
import NanoUI.Widgets.Custom

-- | Draw a vector gauge and export it to an SVG file.
exportGaugeSvg :: FilePath -> Float -> IO ()
exportGaugeSvg filePath value = do
  let bounds = Rect 0 0 160 50
      ops = runCanvas bounds $ do
        -- Outer background
        drawRoundedRect bounds 10 (colorRGBA 30 41 59 255)
        drawStrokeRoundedRect bounds 10 1.5 (colorRGBA 56 189 248 255)

        -- Progress bar path
        let barW = 140 * clamp 0 1 value
        drawRoundedRect (Rect 10 32 barW 8) 4 (colorRGBA 56 189 248 255)

        -- Indicator circle & text
        drawStrokePath (P.circle (V2 24 18) 8) 2 colorWhite
        drawText (V2 42 18) AlignStart AlignMiddle "CPU Load" colorWhite

  -- Export directly as a standalone SVG document:
  TIO.writeFile filePath (drawOpsToSvg bounds ops)
```

To embed the output inside an existing HTML/SVG document, pass `svgStandalone = False`:

```haskell
-- Emits an inner <g> fragment instead of a full <svg> document
let fragment = renderDrawOpsSvg defaultSvgConfig { svgStandalone = False } bounds ops
```

For whole-window or view-level SVG captures, see `NanoUI.Draw.FrameSvg` in `nano-ui-svg`.

## See also

- Widget inventory: [Widget Gallery](index.md).
- Drawing styles, colors, and themes: [Styling & Theming](../styling/index.md).
- SVG export of canvas drawings: [Ecosystem Packages](../packages/index.md).

