# Changelog

## 0.2.0.0 -- Unreleased

### Breaking changes

- Plot widgets now take an explicit `PlotCache` allocated once with `newPlotCache`.
- Views and widgets use the concrete `NanoUI` monad instead of `Ui :> es => Eff es`, dropping the `effectful-core` dependency.
- Removed `NanoUI.Diagrams.Tessellation`. Build vector paths with `NanoUI.Path` and draw them through `drawPath` / `drawStrokePath`.

### Added

- `niceTickLabels`, `domainBoundsBy`, and `settleDomain` in `NanoUI.Plot.Scale`.
- `HasResponse` instance for `PlotResponse`, allowing `resp*`, `tooltip`, and `contextMenu` directly on chart responses.

### Changed

- Charts and diagrams render anti-aliased paths (`FillPolygon` and `StrokePolyline`) with full line cap, join, miter limit, dashing, and even-odd/non-zero fill rule support.
- Bar charts label the x axis by category name and anchor bars at y = 0.
- Default decimation preserves up to 2000 scatter/marker points; hover selects the nearest point in screen pixels; tick labels keep exact precision for fine steps and large offsets.
- Unchanged charts skip re-boxing and redundant style comparisons, cutting per-frame allocation by half or more on retained multi-thousand-point plots.
- Builds with GHC 9.10 through 9.14 (`base >=4.20 && <4.23`).

### Fixed

- Non-finite values (NaN, infinities) are ignored in domains, drawing, and hover without triggering continuous redraws.
- Area series now include their baseline in the y domain, fill cleanly across baseline crossings, and align with data coordinates (along with diamond and triangle markers).
- Fixed step series final segments, `withMarker` on line series, `LegendTop` placement, translucent diagram opacity, `fitLayout` on zero-width diagrams, and growing-width plots preserving caller height constraints.

## 0.1.0.0

First release.
