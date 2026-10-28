# nano-ui-svg: Headless SVG Export

`nano-ui-svg` exports `nano-ui` draw operations (`DrawOp`) and complete layout frames directly to scalable vector graphics (SVG).

## Use Cases

- **Interactive Documentation**: All the interactive specimens and workbenches in this book are generated headlessly using `frameToSvg`!
- **Headless Screenshot & Regression Testing**: Render entire views to SVG in CI without a display server (X11, Wayland, or Windows desktop) and diff against golden SVGs.
- **Diagram Export**: Export charts and canvases to standalone `.svg` files for reports.

## Exporting a Frame to SVG

```haskell
import NanoUI
import NanoUI.Testing (newPixelContext, runFrame)
import NanoUI.Draw.FrameSvg (defaultFrameSvgConfig, frameToSvg)

exportViewToSvg :: NanoUI () -> IO Text
exportViewToSvg myView = do
  ctx <- newPixelContext
  (result, drawData, isDirty) <- runFrame ctx myView
  pure (frameToSvg defaultFrameSvgConfig ctx drawData)
```

The exported SVG includes proper `clip-path` masks, linear gradients, rounded rectangles, and embedded font metrics.
