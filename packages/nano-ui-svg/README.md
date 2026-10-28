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

Paint lowers every widget straight to the GPU vertex buffer, so widget chrome never becomes `DrawOp`s of its own. `NanoUI.Draw.FrameSvg` re-renders a finished frame from the shapes paint recorded while drawing it, vector drawings from their draw-op caches, and text from the raster spans:

```haskell
import NanoUI.Draw.FrameSvg (defaultFrameSvgConfig, enableSvgExport, frameToSvg)

-- once, before running frames:
enableSvgExport ctx

(_, drawData, _) <- runFrame ctx inp view
svg <- frameToSvg defaultFrameSvgConfig ctx inp drawData
```

`enableSvgExport` records each shape paint draws and moves all text onto the spans; `frameToSvg` fails on a context without it rather than export the text alone. Clip and gradient ids take a prefix from the document's hash, so several documents can be inlined in one page. The `nano-ui-demo-svg` executable in `nano-ui-demo` uses this to export the demo's Plots tab.

## Documentation

- [The nano-ui Book](https://goolord.github.io/nano-ui/index.html): the comprehensive
  online guide, tutorials, and SVG export documentation.
