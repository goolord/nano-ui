# nano-ui-svg

Render `DrawOp` sequences from [nano-ui](https://github.com/goolord/nano-ui) canvases, custom widgets, and `nano-ui-diagrams` into SVG documents or `<g>` fragments.

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
