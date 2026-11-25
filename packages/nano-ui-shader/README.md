# nano-ui-shader

Typed fragment shader EDSL and multi-target compiler for [nano-ui](https://github.com/goolord/nano-ui). Write surface fills, color effects, distortion warps, and multi-pass layer effects once in Haskell, fuse compatible passes algebraically, and compile directly to GLSL 150 (OpenGL 3.2), GLSL ES 300 (WebGL2), SPIR-V (Vulkan / SDL3 GPU), MSL (Metal), and a CPU reference evaluator.

```haskell
{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, OverloadedRecordDot #-}

import GHC.Generics (Generic)
import NanoUI
import NanoUI.Shader

data PulseParams = PulseParams
  { ppColor :: !Color
  , ppPhase :: !Float
  , ppRings :: !Float
  }
  deriving (Eq, Generic, Uniforms)

pulseRing :: Shader 'Surface PulseParams
pulseRing = surfaceShader $ \u ctx ->
  let p = ctx.uv *^ 2.0 - vec2 1.0 1.0
      wave = 0.5 + 0.5 * sin (vLength p * u.ppRings - u.ppPhase)
      half = ctx.size *^ 0.5
      edge = sdRoundedBox (ctx.pos - half) half ctx.cornerRadius
      aa = 0.75 / maxE ctx.scale 1.0
      mask = 1.0 - smoothstep (negate aa) aa edge
   in vec4_31 (u.ppColor.rgb *^ wave) (u.ppColor.a * mask)
```

Define shaders at top level so compilation happens once; uniform updates pack into std140 bytes and drive damage diffing without recompiling GPU programs. Render leaf fills with `shaderBox` or `shaderConfigured` (zero offscreen targets), and wrap interactive subtrees with `withShader` or `withShader'`.

## Two tiers and pass fusion

- **Tier 1 (`'Surface`):** Single-pass leaf fills drawn directly into the main render target (`meshGradient4`, `radialGlow`, `sdfRoundedBox`, `colorWheelHsv`, `noiseGrain`, `checkerboard`).
- **Tier 2 (`'ColorEffect`, `'DistortionEffect`, `'LayerEffect`):** Subtree post-processing over pooled offscreen targets that keeps child hit-testing, focus, and scrolling intact (`groupOpacity`, `grayscale`, `tint`, `brightnessContrast`, `colorMatrix`, `vignette`, `scanlines`, `waveDistort`, `rippleDistort`, `pixelate`, `boxBlur`, `gaussianBlur`, `dropShadow`, `outerGlow`, `chromaticAberration`).

Compatible Tier 2 passes fuse into a single GPU program with `composeColor`, `composeDistort`, and `distortThenColor`. Multi-pass chains combine with `chainLayer`.

## EDSL and authoring

Enable `DataKinds` and `OverloadedRecordDot` to project uniform record fields (`u.ppColor.rgb`) and vector swizzles (`ctx.uv.yx`). Scalar-vector operators are `(*^)`, `(^*)`, `(/^)`, `(+^)`, and `(-^)`; branchless selection is `selectE`. Use the monadic variants (`surfaceShaderM`, `colorEffectM`, `distortionEffectM`, `layerEffectM`) when a shader needs `bind` sharing or bounded `forRange` loops.

For hand-written backend shader sources without this EDSL, import `NanoUI.Shader.Raw` from `nano-ui`.

## Fallback and testing

Query live GPU support inside a view with `askShaderCapabilities`. When GPU shaders are unavailable (for instance under SDL's software renderer), `shaderConfigured` paints `scFallback` and `withShader` renders its children without the effect.

Run the headless test suite to verify algebraic fusion, std140 packing, GLSL/MSL/SPIR-V generation, and CPU reference parity:

```sh
cabal test nano-ui-shader-test
```

Run the interactive gallery example with `cabal run nano-ui-example-shaders`.

## Documentation

- [The nano-ui Book: Shaders](https://goolord.github.io/nano-ui/book/advanced/shaders.html): Surface fills, subtree effects, pass fusion, and backend fallback details.
