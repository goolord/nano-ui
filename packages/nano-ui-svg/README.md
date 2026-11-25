# nano-ui-svg

SVG exporter for [nano-ui](https://github.com/goolord/nano-ui). Export either raw `DrawOp` lists (from canvases, custom widgets, and `nano-ui-diagrams` charts) or full rendered frames of any UI view.

## Exporting draw ops

```haskell
import Data.Text (Text)
import NanoUI
import NanoUI.Draw.Svg (drawOpsToSvg)
import NanoUI.Path qualified as P
import NanoUI.Widgets.Custom (runCanvasFor)

badgeSvg :: CustomDrawContext -> Text
badgeSvg cdc = drawOpsToSvg bounds $ runCanvasFor cdc $ do
  drawRoundedRect bounds 8 (colorRGBA 37 99 235 255)
  drawStrokePath (P.circle (V2 20 20) 8) 2 colorWhite
  drawText (V2 68 20) AlignCenter AlignMiddle "Ready" colorWhite
  where
    bounds = Rect 0 0 120 40
```

For finer control, `renderDrawOpsSvg` takes an `SvgConfig`:

- `svgStandalone = False` emits an inline `<g>` fragment instead of a full `<svg>` document.
- `svgPrecision` caps coordinate decimal places.
- `svgImageUri` maps `ImageId` values to `href` URLs or data URIs (unmapped images are omitted).
- `svgIdPrefix` namespaces clip-path and gradient IDs so multiple SVGs can share one HTML page safely.

## Exporting entire UI frames

```haskell
import NanoUI.Draw.FrameSvg (defaultFrameSvgConfig, enableSvgExport, frameToSvg)

enableSvgExport ctx -- call once before running frames
(_, drawData, _) <- runFrame ctx inp view
svg <- frameToSvg defaultFrameSvgConfig ctx inp drawData
```

Normally, widgets paint straight into vertex buffers. Calling `enableSvgExport` tells the paint pass to also record vector shapes and text spans (`frameToSvg` fails on a context where recording was not enabled).

Exported text aligns to the metrics measured during the frame. Headless test contexts (`newPixelContext`) measure text on a 16 px monospace grid; wrap your frames in `withSdl` if you want exact TrueType metrics matching your desktop app.

See `nano-ui-demo-svg` in `nano-ui-demo` for a complete runnable example.

## Documentation

- [The nano-ui Book: SVG export](https://goolord.github.io/nano-ui/book/packages/index.html#svg-export)
