# Changelog

## Unreleased

### Changed

- Draw ops are a `SmallArray DrawOp` from `primitive` instead of a boxed
  `Vector` in `diagramOps`, `diagramTextOps`, `diagramFrame` and
  `labelFitScale`.
- `CategoryY` holds its labels and values apart, as
  `CategoryY (SmallArray Text) (PrimArray Double)`. `bar` and `barVec` build
  it as before. Plot series stay unboxed vectors.
- `chartDiagram`, `hitTestChartCached` and `nearestPlotHover` take the
  chart's domains and decimated points, computed once per chart
  (`seriesDomains`, `seriesPoints`), instead of recomputing them from the
  `Chart`. Hover lookup reuses what the chart was drawn from.
- `uniformHeight` is replaced by `letterbox`, which returns the drawn size
  and offset of a diagram fitted into a box.
- `NanoUI.Diagrams.Widget` exports `frameInner`.

### Removed

- `chartXDomain` and `chartYDomain` from `NanoUI.Plot.Chrome` (use
  `seriesDomains`), `diagramPointAtWithExtents` from `NanoUI.Plot.Hit`, and
  `bezierTolerance` from `NanoUI.Diagrams.Tessellation`.

## 0.1.0.0

First release.
