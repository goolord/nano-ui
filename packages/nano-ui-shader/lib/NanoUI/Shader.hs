-- | Public facade for the @nano-ui-shader@ typed fragment shader EDSL,
-- multi-target compiler (GLSL 150, GLSL ES 300, SPIR-V 1.0, MSL, CPU),
-- algebraic effect fusion, and standard effect library.
module NanoUI.Shader
  ( -- * Core shader types (re-exported from @nano-ui@)
    Shader
  , ShaderKind (..)
  , IsNodeEffect
  , ShaderCapabilities (..)
  , ShaderConfig (..)
  , defaultShaderConfig
  , contramapShader
  , fixUniform
  , Uniforms (..)
  , V3 (..)
  , V4 (..)
    -- * Compiling shaders
  , surfaceShader
  , surfaceShaderM
  , colorEffect
  , colorEffectM
  , distortionEffect
  , distortionEffectM
  , layerEffect
  , layerEffectM
    -- * Algebraic fusion and multi-pass chaining
  , composeColor
  , composeDistort
  , distortThenColor
  , chainLayer
    -- * Expressions and monadic builder
  , Expr
  , FragM
  , liftExpr
  , bind
  , LoopAcc (..)
  , forRange
  , GpuExpr (..)
  , GpuFloat
  , GpuVec
  , GpuSigned
  , GpuMinMax
  , GpuOrd
  , GpuEq
    -- * Uniform access
  , ExprU
  , UniformLeaf
  , uVal
  , uFst
  , uSnd
  , uFst3
  , uSnd3
  , uThd3
    -- * Fragment context and layer sampling
  , FragCtx (..)
  , Layer
  , sampleLayer
  , sampleLayerUv
    -- * Literals and vector construction
  , floatE
  , intE
  , uintE
  , boolE
  , colorE
  , v2E
  , v3E
  , v4E
  , vec2
  , vec3
  , vec4
  , vec3_21
  , vec4_22
  , vec4_31
  , rgba
  , splat2
  , splat3
  , splat4
  , floatToInt
  , intToFloat
    -- * Scalar and vector math
  , absE
  , signE
  , floorE
  , ceilE
  , fractE
  , modE
  , minE
  , maxE
  , clampE
  , clamp01E
  , mix
  , stepE
  , smoothstep
  , powE
  , atan2E
  , dot
  , cross
  , vLength
  , vDistance
  , normalize
  , (*^)
  , (^*)
  , (/^)
  , (+^)
  , (-^)
    -- * Comparisons and branchless selection
  , (.<)
  , (.<=)
  , (.>)
  , (.>=)
  , (.==)
  , (./=)
  , (.&&)
  , (.||)
  , notE
  , select
    -- * 2D SDF, color, and noise helpers
  , sdCircle
  , sdRoundedBox
  , luminanceE
  , withAlphaE
  , hsvToRgbE
  , hash21
  , valueNoise2D
    -- * Standard library effects
  , module NanoUI.Shader.Effects
  ) where

import NanoUI.Internal.Shader
  ( IsNodeEffect
  , Shader
  , ShaderCapabilities (..)
  , ShaderConfig (..)
  , ShaderKind (..)
  , contramapShader
  , defaultShaderConfig
  , fixUniform
  )
import NanoUI.Internal.Uniforms (Uniforms (..), V3 (..), V4 (..))
import NanoUI.Shader.Compose
  ( chainLayer
  , colorEffect
  , colorEffectM
  , composeColor
  , composeDistort
  , distortThenColor
  , distortionEffect
  , distortionEffectM
  , layerEffect
  , layerEffectM
  , surfaceShader
  , surfaceShaderM
  )
import NanoUI.Shader.Effects
import NanoUI.Shader.Expr
  ( Expr
  , ExprU
  , FragCtx (..)
  , FragM
  , GpuEq
  , GpuExpr (..)
  , GpuFloat
  , GpuMinMax
  , GpuOrd
  , GpuSigned
  , GpuVec
  , Layer
  , LoopAcc (..)
  , UniformLeaf
  , absE
  , atan2E
  , bind
  , boolE
  , ceilE
  , clamp01E
  , clampE
  , colorE
  , cross
  , dot
  , floatE
  , floatToInt
  , floorE
  , forRange
  , fractE
  , hash21
  , hsvToRgbE
  , intE
  , intToFloat
  , liftExpr
  , luminanceE
  , maxE
  , minE
  , mix
  , modE
  , normalize
  , notE
  , powE
  , rgba
  , sampleLayer
  , sampleLayerUv
  , sdCircle
  , sdRoundedBox
  , select
  , signE
  , smoothstep
  , splat2
  , splat3
  , splat4
  , stepE
  , uFst
  , uFst3
  , uSnd
  , uSnd3
  , uThd3
  , uVal
  , uintE
  , v2E
  , v3E
  , v4E
  , vDistance
  , vLength
  , valueNoise2D
  , vec2
  , vec3
  , vec3_21
  , vec4
  , vec4_22
  , vec4_31
  , withAlphaE
  , (*^)
  , (^*)
  , (/^)
  , (+^)
  , (-^)
  , (.&&)
  , (./=)
  , (.<)
  , (.<=)
  , (.==)
  , (.>)
  , (.>=)
  , (.||)
  )
import NanoUI.Shader.Swizzle ()
