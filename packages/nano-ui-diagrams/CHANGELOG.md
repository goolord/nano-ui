# Changelog

## Unreleased

### Changed

- Builds with GHC 9.10 through 9.14 (`base >=4.20 && <4.23`).
- Plots and diagrams are anti-aliased. A filled path is one `FillPolygon`
  and a stroked one one `StrokePolyline`, instead of hard-edged
  `FillTriangle`s whose corners each snapped to the pixel grid, so lines
  and markers no longer come out jagged. Strokes take the style's line cap,
  join, miter limit and dashing.
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
- Paths are filled and stroked by the core's canvas path code
  (`NanoUI.Path`): a convex fill is fanned, every chord of a flattened curve
  stays within half a unit of it, and a level rectangle is one rect op.
- A path's loops are filled together by the style's fill rule, so a loop
  inside another (an annulus, a glyph's counter) is a hole in it where the
  rule says so, instead of each loop being filled on its own over the
  others. A path is filled whole before it is stroked.

### Removed

- `chartXDomain` and `chartYDomain` from `NanoUI.Plot.Chrome` (use
  `seriesDomains`), `diagramPointAtWithExtents` from `NanoUI.Plot.Hit`, and
  the `NanoUI.Diagrams.Tessellation` module.

## 0.1.0.0

First release.
