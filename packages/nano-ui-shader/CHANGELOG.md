# Changelog

## 0.2.0.0 -- Unreleased

Initial release.

- Pure-Haskell typed fragment shader EDSL (`NanoUI.Shader`, `NanoUI.Shader.Expr`, `NanoUI.Shader.Compose`, `NanoUI.Shader.Effects`, `NanoUI.Shader.Raw`) with `OverloadedRecordDot` uniform projections, vector swizzles, and `Generic` std140 uniform packing.
- Multi-target compiler from lowered SSA IR to GLSL 150 (OpenGL 3.2), GLSL ES 300 (WebGL2), SPIR-V 1.0 (Vulkan), Metal Shading Language (MSL), and a pure-Haskell CPU evaluator for headless differential testing.
- Algebraic single-pass shader fusion (`composeColor`, `composeDistort`, `distortThenColor`) and multi-pass chaining (`chainLayer`).
- 21 built-in leaf surface fills (`meshGradient4`, `radialGlow`, `sdfRoundedBox`, `stripes`, `frostedGlass`, and more) and subtree effects (`grayscale`, `tint`, `groupOpacity`, `gaussianBlur`, `dropShadow`, `waveDistort`, and more).
- Builds with GHC 9.10 through 9.14 (`base >=4.20 && <4.23`).
