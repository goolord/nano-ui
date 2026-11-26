# Changelog

## 0.2.0.0 -- Unreleased

Initial release.

- `NanoUI.Draw.Svg` (`SvgConfig`, `defaultSvgConfig`, `renderDrawOpsSvg`, `drawOpsToSvg`) exports `SmallArray DrawOp` draw lists to standalone SVG.
- `NanoUI.Draw.FrameSvg` (`FrameSvgConfig`, `defaultFrameSvgConfig`, `enableSvgExport`, `frameToSvg`) exports complete painted `nano-ui` frames to SVG, including clip-cut geometry, vector drawings, fitted `<text>` elements, image URI resolution, and native `<g opacity>` output for opacity shaders.
- Builds with GHC 9.10 through 9.14 (`base >=4.20 && <4.23`).
