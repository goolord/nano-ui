# Changelog

## 0.2.0.0 -- Unreleased

- Initial release of the typed fragment shader EDSL (`NanoUI.Shader`, `NanoUI.Shader.Expr`, `NanoUI.Shader.Compose`, `NanoUI.Shader.Effects`).
- Compiles lowered SSA IR to GLSL 150, GLSL ES 300, SPIR-V, MSL, and a pure-Haskell CPU reference evaluator.
- Includes `OverloadedRecordDot` uniform field projections and vector swizzles, `Generic` std140 packing, algebraic pass fusion (`composeColor`, `composeDistort`, `distortThenColor`, `chainLayer`), and 21 built-in surface fills and subtree effects.
