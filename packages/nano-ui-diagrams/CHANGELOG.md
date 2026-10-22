# Changelog

## 0.2.0.0 -- Unreleased

### Added

- `niceTickLabels` in `NanoUI.Plot.Scale`: ticks with labels that carry the
  digits their step needs. `domainBoundsBy` and `settleDomain`, the two
  halves of `domainExtentBy`, merge several series' bounds before the
  fallback for an empty or constant domain is applied once.
- `PlotResponse` has a `HasResponse` instance, so the `resp*` accessors,
  `tooltip` and `contextMenu` take a chart's response directly.

### Changed

- **Breaking:** plot widgets take an explicit `PlotCache`, allocated once
  with `newPlotCache`. Cached chart values no longer use dynamic widget slots.

- Widgets and view functions have `NanoUI` types instead of
  `Ui :> es => Eff es`, and the package no longer depends on `effectful-core`.
- Builds with GHC 9.10 through 9.14 (`base >=4.20 && <4.23`).
- The library imports `NanoUI.Internal` modules, which change without
  notice, so it allows only nano-ui 0.2.0.x (`>=0.2.0 && <0.2.1`).
- A bar chart's x axis is labelled with its category names, and its bars
  stand on zero on the chart's y axis. Each bar series was scaled to its own
  largest value from the bottom of the box, so a bar's top disagreed with the
  axis and a negative bar fell below the box.
- Under default decimation, scatter series and lines with markers keep every
  point up to 2000 and are thinned to 2000 past that; line, area and step
  series are thinned as before. A hundred markers drew 64.
- Tick labels carry the digits their step needs, so fine steps and large
  offsets, such as a Unix time axis, no longer label every tick alike.
  `niceTicks` makes each tick its index times the step, so zero is exact and
  rounding does not build up along the axis. `formatTick` no
  longer rounds small values to a 1e-10 grid first: 1.5e-10 reads
  `1.500e-10`, not `2.000e-10`, and smaller values are not 0.
- Hover picks the point nearest the pointer on screen rather than in data
  units, where the axis with the larger span dominated.
- Series bounds are merged before the empty and constant fallbacks and the
  5% padding, so an empty or constant series no longer widens a
  multi-series chart's shared domain. An empty chart's axes are padded too
  (-0.05 to 1.05).
- A plot with a growing width keeps the caller's fixed height, a minimum
  above 260 px, or a growing height; it capped them at 260 px. So
  `fillW . minH 300` is now 300 px tall.
- Plots and diagrams are anti-aliased. A filled path is one `FillPolygon`
  and a stroked one one `StrokePolyline`, instead of hard-edged
  `FillTriangle`s whose corners each snapped to the pixel grid, so lines
  and markers no longer come out jagged. Strokes take the style's line cap,
  join, miter limit and dashing.
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

- An area series' y domain includes its baseline, so the fill no longer
  runs below the axis or squeezes the data, and a growing-width plot keeps
  a fixed height below 260 px exactly, as it did above.
- Values that are not finite (NaN, infinities) are left out of chart
  domains, drawing and hover, and `domainExtent` and `domainExtentBy` skip
  them. A NaN dropped the domain's earlier minimum, and an infinity made
  every point NaN. `hoverPointIdx` counts only the points drawn.
- A plot whose chart holds NaN, in its data, an area's baseline or a stroke
  or bar width, no longer rebuilds the chart and asks for a frame every
  frame: `SeriesData` and `SeriesKind` compare a NaN equal to the same NaN.
- Step series rise to their last point instead of stopping at the run before
  it. `withMarker` draws markers on line series, which it stored and nothing
  drew.
- A top legend (`LegendTop`) gets a row of its own under the title, instead
  of crossing the title or spilling above the drawing.
- Translucent diagram colours are no longer darkened (alpha was applied
  twice), and the style's `opacity`, `fillOpacity` and `strokeOpacity` take
  effect.
- `fitLayout` sizes a diagram with no width, such as `vrule`, by the
  caller's box: its height is no longer infinite under `fixedW`, and
  `fixedWH` keeps its width. A `vrule` with the default layout is 160 wide.
- An area series and diamond and triangle markers are drawn where their
  data is. Each polygon was drawn from the plot's origin instead of its
  first point, which moved an area by its first sample's offset from the
  corner and a marker by about its radius.
- An area series whose data crosses its baseline fills each side, one
  polygon per run between crossings, where it left gaps or spilled.

### Removed

- **Breaking:** the public `NanoUI.Diagrams.Tessellation` module
  (`triangulatePolygon`, `fillPolygon`, `strokePolyline`, `flattenCubic`).
  The backend fills and strokes through nano-ui's paths; build shapes with
  `NanoUI.Path` (`polygon`, `polyline`, `cubicTo`) and draw them with
  `drawPath` and `drawStrokePath`.

## 0.1.0.0

First release.
