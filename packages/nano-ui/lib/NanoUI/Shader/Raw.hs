-- | Low-level shader compilation payloads, std140 uniform layouts, raw
-- backend shader constructors, and CPU evaluation hooks.
--
-- Application code authoring shaders in typed Haskell should use
-- @nano-ui-shader@ ("NanoUI.Shader") together with 'NanoUI.shaderBox' and
-- 'NanoUI.withShader' from "NanoUI". Import this module when embedding
-- hand-written GLSL, MSL, or SPIR-V shader programs directly, or inspecting
-- compiled shader metadata.
module NanoUI.Shader.Raw
  ( -- * Uniform layout
    GpuType (..)
  , UniformField (..)
  , uniformLayout
  , uniformSize

    -- * Raw programs
  , ShaderProgram (..)
  , emptyShaderProgram
  , rawShader
  , rawLayerShader

    -- * CPU evaluation
  , CpuFragCtx (..)
  , CpuShaderEval
  ) where

import NanoUI.Internal.Shader
  ( CpuFragCtx (..)
  , CpuShaderEval
  , ShaderProgram (..)
  , emptyShaderProgram
  , rawLayerShader
  , rawShader
  )
import NanoUI.Internal.Uniforms
  ( GpuType (..)
  , UniformField (..)
  , uniformLayout
  , uniformSize
  )
