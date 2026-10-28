# nano-ui-svg

Render `DrawOp` sequences from [nano-ui](https://github.com/goolord/nano-ui) canvases, custom widgets, and `nano-ui-diagrams` into SVG documents or `<g>` fragments — or export a whole painted frame of any view with `frameToSvg`.

Add `nano-ui-svg` to your Cabal dependencies and import `NanoUI.Draw.Svg`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text.IO qualified as TIO
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

Use `renderDrawOpsSvg` with `SvgConfig` to emit an inner `<g>` fragment (`svgStandalone = False`), adjust coordinate precision (`svgPrecision`), or supply an image URI resolver (`svgImageUri`).

## Whole frames

Widget chrome never becomes `DrawOp`s — paint lowers every widget straight to the GPU vertex buffer — so `NanoUI.Draw.FrameSvg` re-renders a finished frame from what a host already sees: solid geometry from `DrawData`, vector drawings from their draw-op caches, and text from the raster spans:

```haskell
import NanoUI.Draw.FrameSvg (defaultFrameSvgConfig, frameToSvg)
import NanoUI.Internal.Context (setDrawExternalText, setDrawSquareGeometry)

-- once, before running frames:
setDrawSquareGeometry ctx True
setDrawExternalText ctx True

(_, drawData, _) <- runFrame ctx inp view
svg <- frameToSvg defaultFrameSvgConfig ctx inp drawData
```

Square geometry makes every widget shape a flat axis-aligned quad and external text moves all text onto the spans; without them the export still renders, but anti-aliased edges approximate and drawing text is missing. The `nano-ui-demo-svg` executable in `nano-ui-demo` uses this to export the demo's Plots tab.

## Documentation

- [The nano-ui Book](https://goolord.github.io/nano-ui/index.html): the comprehensive
  online guide, tutorials, and SVG export documentation.
