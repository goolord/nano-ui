# Changelog

## 0.1.0.0 -- Unreleased

- `NanoUI.Draw.FrameSvg` with `frameToSvg`: renders a whole painted frame of
  any view from its `DrawData`, draw-op caches, and raster spans — widget
  chrome as clip-cut rects and triangles, drawings as vectors, and text as
  fitted `<text>` elements. `FrameSvgConfig` sets precision, the span font
  family, and image URI resolution.

- Initial release: `NanoUI.Draw.Svg` with `SvgConfig`, `defaultSvgConfig`,
  `renderDrawOpsSvg`, and `drawOpsToSvg` for rendering `SmallArray DrawOp`
  values to SVG.
