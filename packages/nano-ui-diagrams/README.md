# nano-ui-diagrams

Charts and [diagrams](https://diagrams.github.io/) drawings for
[nano-ui](https://github.com/goolord/nano-ui).

`diagram` places a diagrams-lib `Diagram` in a nano-ui layout. `NanoUI.Plot`
builds line, bar, scatter, area, and step charts with axes, legends, and hover
lookup, and thins out long series before drawing them.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import NanoUI
import NanoUI.Plot

temperatures :: Chart
temperatures =
  withTitle "Temperature" . withLegend LegendBottom $
    chart
      [ line "Indoor" [(0, 20), (1, 21.5), (2, 22), (3, 21)]
      , line "Outdoor" [(0, 8), (1, 11), (2, 14), (3, 12)]
      ]

view :: NanoUI ()
view = column $ do
  resp <- plot (minH 240 . fillW) temperatures
  case plotHover resp of
    Just h -> label (T.pack (show (hoverDataX h, hoverDataY h)))
    Nothing -> pure ()
```

The Plots tab of `nano-ui-sdl-demo` in `nano-ui-demo` shows more charts and a
diagram.

## Build and use

Use GHC 9.14 and add `nano-ui`, `nano-ui-diagrams`, and `text` to your
application's `build-depends`. Add `diagrams-lib` if you construct diagrams
directly. The example uses `GHC2024` and `OverloadedStrings`. This package
does not open a window; run the view with a nano-ui backend.

Start with `NanoUI.Plot` for charts and `NanoUI.Diagrams` for drawings.
`NanoUI.Plot.Types` describes series, axis options, and hover results.
The scale and tessellation modules expose lower-level geometry operations.

Chart data uses axis coordinates. The layout modifier passed to `plot`
sets its size in logical pixels. `plotHover` is `Nothing` when there is no
matching point; the returned data coordinates can be used in a tooltip or
status label. Keep a chart value outside the per-frame view when its data
and options do not change.

Run `cabal test nano-ui-diagrams-test` for diagram conversion, tessellation,
and chart tests. These tests do not require a window.
