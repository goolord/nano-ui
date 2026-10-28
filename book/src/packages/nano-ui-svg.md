# Headless SVG Export (`nano-ui-svg`)

`nano-ui-svg` turns nano-ui drawing into SVG. It has two levels:

- **`NanoUI.Draw.Svg`** exports `DrawOp` sequences from canvases, custom widgets, and `nano-ui-diagrams`.
- **`NanoUI.Draw.FrameSvg`** re-renders a whole painted frame: widget geometry, vector drawings, and text.

## Drawing operations

```haskell
import NanoUI
import NanoUI.Draw.Svg (drawOpsToSvg)
import NanoUI.Path qualified as P

exportBadge :: CustomDrawContext -> IO ()
exportBadge cdc = do
  let bounds = Rect 0 0 120 40
      ops = runCanvasFor cdc $ do
        drawRoundedRect bounds 8 (colorRGBA 37 99 235 255)
        drawStrokePath (P.circle (V2 20 20) 8) 2 colorWhite
        drawText (V2 68 20) AlignCenter AlignMiddle "Ready" colorWhite
  TIO.writeFile "badge.svg" (drawOpsToSvg bounds ops)
```

`renderDrawOpsSvg` with an `SvgConfig` emits an inner `<g>` fragment (`svgStandalone = False`), adjusts coordinate precision (`svgPrecision`), or supplies an image URI resolver (`svgImageUri`).

## Whole frames

Widget chrome is not represented as `DrawOp`s — paint lowers widgets directly to the GPU vertex buffer. `frameToSvg` rebuilds a frame from what the host sees: solid geometry from `DrawData`, vector drawings from their draw-op caches, and text from the raster spans.

```haskell
import NanoUI
import NanoUI.Draw.FrameSvg (defaultFrameSvgConfig, frameToSvg)
import NanoUI.Internal.Context (setDrawExternalText, setDrawSquareGeometry)
import NanoUI.Testing (newPixelContext, runFrame)
import NanoUI.Testing.Harness (withInputOff)

exportViewToSvg :: NanoUI () -> IO Text
exportViewToSvg view = do
  ctx <- newPixelContext
  let inp = withInputOff 1280 800
  -- Once, before running frames:
  setDrawSquareGeometry ctx True
  setDrawExternalText ctx True
  (_, drawData, _) <- runFrame ctx inp view
  frameToSvg defaultFrameSvgConfig ctx inp drawData
```

The two `setDraw...` switches matter:

- **`setDrawSquareGeometry`** makes every widget shape a flat axis-aligned quad. Without it, anti-aliased edges are approximations of the rounded shapes.
- **`setDrawExternalText`** moves all text onto the spans the exporter reads. Without it, drawing text is missing from the export.

The export includes proper `clip-path` masks, linear gradients, rounded rectangles, and font metrics. The interactive specimens and workbenches in this book are generated with this path, and the `nano-ui-demo-svg` executable exports the demo's Plots tab.

## Use cases

- **Documentation**: render a view or widget to an SVG for a static page.
- **Visual regression testing**: run frames headlessly in CI (no X11, Wayland, or Windows desktop) and diff against golden SVGs.
- **Diagram export**: write charts and canvases to standalone `.svg` files for reports.

## Pitfalls

- Several canvas drawings in one frame can emit colliding `clip_`/`grad_` element ids. Scope them per drawing, for example by prefixing each `<g><defs>` block with an index.
- Right-aligned numerals can lose their anchor: `text-anchor`/`textLength` may need post-processing. Use tabular numbers (`font-variant-numeric: tabular-nums`) for readouts.
- Widget chrome comes from the frame's vertex data, not from `DrawOp`s; a custom widget must draw through the canvas API to appear in the vector output.

## Related

- Custom drawing: [Canvas Drawing](../widgets/canvas-custom.md).
- Charts: [Charts & Diagrams](nano-ui-diagrams.md).
- Headless testing: `NanoUI.Testing` and `NanoUI.Testing.Harness`.
