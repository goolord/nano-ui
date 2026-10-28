# nano-ui-diagrams: Charts & Plots

`nano-ui-diagrams` integrates high-level plotting primitives (`NanoUI.Plot`) and general 2D vector drawing from `diagrams-lib` into `nano-ui`.

## Chart Types

The `NanoUI.Plot` module provides ready-to-use chart widgets:
- **Line Plots**: Multi-series line charts with customizable strokes, point markers, and grid lines.
- **Bar Charts**: Grouped and stacked bar charts.
- **Scatter Plots**: 2D scatter plots with confidence intervals.
- **Area Charts**: Shaded area graphs for timelines and volume distributions.

```haskell
import NanoUI
import NanoUI.Plot

cpuChart :: [Double] -> NanoUI ()
cpuChart samples = linePlot (fixedWH 400 200) $ do
  plotSeries "CPU %" samples
  plotYRange 0 100
  plotGridLines True
```

Charts automatically adapt their background, grid lines, and axis label colors to the active `Theme`.
