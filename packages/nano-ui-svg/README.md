# nano-ui-svg

SVG export for [nano-ui](https://github.com/goolord/nano-ui): render `DrawOp`s
from canvases, custom widgets and `nano-ui-diagrams` charts, or a whole
painted frame of any view.

## Draw ops

```haskell
import Data.Text (Text)
import NanoUI
import NanoUI.Draw.Svg (drawOpsToSvg)
import NanoUI.Path qualified as P

badgeSvg :: CustomDrawContext -> Text
badgeSvg cdc = drawOpsToSvg bounds $ runCanvasFor cdc $ do
  drawRoundedRect bounds 8 (colorRGBA 37 99 235 255)
  drawStrokePath (P.circle (V2 20 20) 8) 2 colorWhite
  drawText (V2 68 20) AlignCenter AlignMiddle "Ready" colorWhite
  where
    bounds = Rect 0 0 120 40
```

`renderDrawOpsSvg` takes an `SvgConfig`: `svgStandalone = False` emits a `<g>`
fragment instead of an `<svg>` document, `svgPrecision` sets coordinate
digits, `svgImageUri` maps image ids to URIs (unresolved images are dropped),
and `svgIdPrefix` keeps clip and gradient ids apart when several renders share
a page.

## Whole frames

```haskell
import NanoUI.Draw.FrameSvg (defaultFrameSvgConfig, enableSvgExport, frameToSvg)

enableSvgExport ctx -- once, before running frames
(_, drawData, _) <- runFrame ctx inp view
svg <- frameToSvg defaultFrameSvgConfig ctx inp drawData
```

Widgets paint straight to the vertex buffer, so `enableSvgExport` turns on
recording of the shapes they draw and moves text onto spans. `frameToSvg`
fails on a context without it.

Text is fitted to the widths the context measured. A headless test context
measures in a 16 px monospace grid, so run frames inside an SDL session
(`withSdl`) when the text should read like the app's.

`nano-ui-demo-svg` in `nano-ui-demo` is a working example.

## Documentation

[The nano-ui Book](https://goolord.github.io/nano-ui/index.html)
