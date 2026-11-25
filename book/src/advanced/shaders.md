# Shaders

`nano-ui` supports typed, composable GPU fragment shaders on layout nodes across all three GPU backends: SDL 3.4+ (Vulkan/Metal via SPIR-V and MSL), RGFW (OpenGL 3.2 via GLSL 150), and the browser backend (WebGL2 via GLSL ES 300).

The core `nano-ui` package defines the shader types, std140 uniform packing (`Uniforms`), and the two widget entry points (`shaderBox` and `withShader`). The companion `nano-ui-shader` package adds a pure-Haskell typed fragment EDSL, compilers for all four GPU formats plus a CPU reference evaluator, algebraic pass fusion, and 21 standard effects.

## Two tiers

Every shader has a type-level kind (`ShaderKind`) that separates leaf fills from subtree post-processing:

- **`Surface` (Tier 1):** Draws a single layout leaf (`shaderBox`, `shaderConfigured`) in one draw call with zero offscreen render targets. Use this for gradients, procedural textures, SDF shapes, and color wheels.
- **`ColorEffect`, `DistortionEffect`, and `LayerEffect` (Tier 2):** Wrap any widget subtree (`withShader`) by rendering its children into a pooled 64px-bucketed offscreen texture and compositing the result through one or more shader passes. Controls inside the subtree remain interactive.

Passing a `Surface` shader to `withShader` is a compile-time type error (`IsNodeEffect`).

## Leaf surface shaders

Call `shaderBox` for a quick leaf fill or `shaderConfigured` when you need a corner radius, tint, or fallback color for backends without GPU shaders (such as SDL's software renderer):

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Shaders.hs:surface_basic}}
```

Define shaders as top-level bindings so the EDSL compiles once per shader, not on every frame. Changing the uniform value re-packs only the uniform bytes and triggers a local damage repaint on that node. Leaving the uniform unchanged produces zero damage, so idle windows remain asleep.

## Custom shaders and uniforms

Uniform records derive `Uniforms` via `GHC.Generics`. `nano-ui` packs fields to exact std140 alignment, zero-pads every block to a 16-byte multiple, and hashes the packed bytes for damage diffing:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Shaders.hs:custom_uniforms}}
```

Supported field types are `Float`, `Int32`, `Word32`, `Bool`, `V2`, `V3`, `V4`, `Color` (unpacked to normalized RGBA `vec4`), `Rect`, `Size`, tuples, and nested `Generic` records.

With `OverloadedRecordDot`, both uniform fields (`u.ppColor`) and GLSL-style vector swizzles (`u.ppColor.rgb`, `ctx.uv.xy`) resolve at compile time with no Template Haskell:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Shaders.hs:custom_surface}}
```

Every fragment shader receives a `FragCtx` containing `ctx.uv` (normalized `[0, 1]` over the node rect), `ctx.pos` (logical pixels from the node's top-left corner), `ctx.size` (logical width and height), `ctx.origin` (window-space logical origin), `ctx.scale` (device pixels per logical unit), `ctx.cornerRadius`, and `ctx.color` (vertex tint).

## Subtree effects and pass fusion

Wrap any view with `withShader` to apply a post-processing effect to the whole rendered subtree:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Shaders.hs:subtree_effect}}
```

`gaussianBlur` runs two separable 1D passes (horizontal then vertical) by pinging between pooled offscreen targets. Its bleed margin (`3 * sigma` pixels on each side) expands the offscreen target, cull test, and partial-damage region automatically so soft halos never clip at the widget edge.

Compatible effects fuse into a single GPU pass with zero extra offscreen targets:

- `composeColor :: Shader 'ColorEffect u1 -> Shader 'ColorEffect u2 -> Shader 'ColorEffect (u1, u2)`
- `composeDistort :: Shader 'DistortionEffect u1 -> Shader 'DistortionEffect u2 -> Shader 'DistortionEffect (u1, u2)`
- `distortThenColor :: Shader 'DistortionEffect u1 -> Shader 'ColorEffect u2 -> Shader 'LayerEffect (u1, u2)`
- `chainLayer` chains arbitrary multi-pass effects explicitly.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Shaders.hs:fused_effect}}
```

## Bounded loops in `FragM`

Iterative kernels (blurs, shadows, multi-octave noise) use the `FragM` builder (`surfaceShaderM`, `colorEffectM`, `distortionEffectM`, `layerEffectM`) with explicit `bind` and structured `forRange` loops:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Shaders.hs:blur_loop}}
```

The first argument to `layerEffectM` reports the maximum sample offset in logical pixels (`V2 marginX marginY`) for the current uniform value.

## Built-in effects

`NanoUI.Shader` ships 21 ready-to-use shaders:

| Category | Shaders |
| --- | --- |
| `Surface` | `meshGradient4`, `radialGlow`, `sdfRoundedBox`, `colorWheelHsv`, `noiseGrain`, `checkerboard` |
| `ColorEffect` | `groupOpacity`, `grayscale`, `tint`, `brightnessContrast`, `colorMatrix`, `vignette`, `scanlines` |
| `DistortionEffect` | `waveDistort`, `rippleDistort`, `pixelate` |
| `LayerEffect` | `boxBlur`, `gaussianBlur`, `dropShadow`, `outerGlow`, `chromaticAberration` |

## Capabilities, fallbacks, and SVG export

On SDL 3.4+, `RenderDriverAuto` selects SDL's `"gpu"` render driver when available (falling back cleanly to standard drivers or software rendering when unavailable). Inspect live GPU support inside a view with `askShaderCapabilities`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Shaders.hs:capabilities}}
```

When a backend lacks GPU shader support:

- `shaderBox` / `shaderConfigured` draws a flat rounded rectangle in `scFallback` (using `scCornerRadius`).
- `withShader` renders its children directly without offscreen redirection.
- `nano-ui-svg` exports `withShader groupOpacity alpha` as a native SVG `<g opacity="...">` group.

For a complete runnable program covering both tiers and animated uniforms, see `packages/nano-ui-demo/examples/Shaders.hs` (`cabal run nano-ui-example-shaders`).
