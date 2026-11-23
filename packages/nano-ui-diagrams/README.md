# nano-ui-diagrams

Charts and [diagrams](https://diagrams.github.io/) vector graphics for [nano-ui](https://github.com/goolord/nano-ui).

`diagram` embeds any `diagrams-lib` `Diagram` inside a `nano-ui` layout, and `NanoUI.Plot` draws line, bar, scatter, area, and step charts with automatic axes, legends, hover inspection, and series decimation for large datasets.

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

-- Allocate cache <- newPlotCache once during setup.
view :: PlotCache -> NanoUI ()
view cache = column $ do
  resp <- plot cache (minH 240 . fillW) temperatures
  case plotHover resp of
    Just h -> label (T.pack (show (hoverDataX h, hoverDataY h)))
    Nothing -> pure ()
```

See the Plots tab of `nano-ui-sdl-demo` in `nano-ui-demo` for more chart types and a live diagram example.

## Usage and modules

Target GHC 9.10 through 9.14 with `GHC2024` and `OverloadedStrings`, and add `nano-ui`, `nano-ui-diagrams`, and `text` to `build-depends` (plus `diagrams-lib` if you construct `Diagram` values directly). This package is backend-agnostic; pair it with any `nano-ui` backend to display a window.

- `NanoUI.Plot`: High-level chart constructors, axes, legends, and `plot`.
- `NanoUI.Diagrams`: Re-exports `NanoUI.Plot` along with the `diagram` widget and theme color helpers.
- `NanoUI.Plot.Types`: Series types, axis configuration, and `PlotHover` results.
- `NanoUI.Plot.Scale` and `NanoUI.Plot.Decimate`: Domain calculation, tick generation, and decimation helpers (underlying diagram paths are filled and stroked with `NanoUI.Path`).

Chart points use data-space coordinates, while the layout modifier passed to `plot` sizes the widget in logical pixels. `plotHover` returns `Nothing` when the pointer is outside the plot area or no point matches; use `hoverDataX`/`hoverDataY` for readouts or tooltips. For static charts, define the `Chart` value outside the per-frame view so `PlotCache` can skip rebuilds immediately.

Run `cabal test nano-ui-diagrams-test` to execute diagram conversion, tessellation, and chart tests headlessly (no window required).

## Documentation

- [The nano-ui Book](https://goolord.github.io/nano-ui/book/packages/index.html#charts-and-diagrams): Visual examples of every chart series, style, legend, hover readout, and diagram widget.
