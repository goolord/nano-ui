# Changelog

## 0.2.0.0 -- Unreleased

### Changed

- Widgets and view functions have `NanoUI` types instead of
  `Ui :> es => Eff es`, and the package no longer depends on `effectful-core`.
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
- An area series is filled from its points and the two ends of its
  baseline, instead of a baseline point under every sample: half the
  vertices painted each frame, and a quarter of the triangulation work when
  the chart is rebuilt.
- A plot checks whether its chart changed without boxing each point, skips
  the check for a chart kept across frames, and no longer derives its plot
  style every frame to compare. The demo's four plots allocate 106 KB a
  frame instead of 210 KB, and a kept 8000-point chart 92 KB instead of
  687 KB.

### Fixed

- An area series and diamond and triangle markers are drawn where their
  data is. Each polygon was drawn from the plot's origin instead of its
  first point, which moved an area by its first sample's offset from the
  corner and a marker by about its radius.
- An area series whose data crosses its baseline fills each side, one
  polygon per run between crossings, where it left gaps or spilled.

### Removed

- `chartXDomain` and `chartYDomain` from `NanoUI.Plot.Chrome` (use
  `seriesDomains`), `diagramPointAtWithExtents` from `NanoUI.Plot.Hit`, and
  the `NanoUI.Diagrams.Internal.Tessellation` module.

## 0.1.0.0

First release.
