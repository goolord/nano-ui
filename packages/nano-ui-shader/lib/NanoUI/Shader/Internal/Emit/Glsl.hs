{-# LANGUAGE StrictData #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Pure-Haskell GLSL 150 (OpenGL 3.2, @nano-ui-rgfw@) and GLSL ES 300
-- (WebGL2, @nano-ui-sdl-web@) fragment shader emitters for 'ShaderIR'.
module NanoUI.Shader.Internal.Emit.Glsl
  ( GlslTarget (..)
  , emitGlsl
  , emitGlsl150
  , emitGlslEs300
  ) where

import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TB
import NanoUI.Internal.Uniforms (GpuType (..), UniformField (..))
import NanoUI.Shader.Internal.Emit.CLike (CLikeDialect (..), emitCLikeNodes, intB, nodeVar)
import NanoUI.Shader.Internal.IR

-- | GLSL dialect target.
data GlslTarget
  = Glsl150
  -- ^ OpenGL 3.2 core (@#version 150@, varying @vUV@).
  | GlslEs300
  -- ^ WebGL2 (@#version 300 es@, @precision highp float;@, varying @vUv@).
  deriving (Eq, Show)

-- | Emit an OpenGL 3.2 core GLSL 150 fragment shader from 'ShaderIR'.
emitGlsl150 :: ShaderIR -> Text
emitGlsl150 = emitGlsl Glsl150

-- | Emit a WebGL2 GLSL ES 300 fragment shader from 'ShaderIR'.
emitGlslEs300 :: ShaderIR -> Text
emitGlslEs300 = emitGlsl GlslEs300

-- | Emit GLSL source text for @target@ from 'ShaderIR'.
emitGlsl :: GlslTarget -> ShaderIR -> Text
emitGlsl target ir = TL.toStrict . TB.toLazyText $ mconcat
  [ header
  , uniformBlock (irUniforms ir)
  , if irNeedsInput ir
      then
        "uniform sampler2D uInput;\n"
          <> "vec4 nanoSampleInput(vec2 uv) {\n"
          <> "  vec4 s = texture(uInput, uv);\n"
          <> "  return vec4(min(s.rgb / max(s.a, 1e-5), vec3(1.0)), s.a);\n"
          <> "}\n"
      else ""
  , "void main() {\n"
  , emitCLikeNodes (glslDialect target) 0 (irNodes ir)
  , "  fragColor = " <> nodeVar 0 (irOutput ir) <> ";\n"
  , "}\n"
  ]
  where
    header = case target of
      Glsl150 ->
        "#version 150\n"
          <> "in vec4 vColor;\n"
          <> "in vec2 vUV;\n"
          <> "out vec4 fragColor;\n"
      GlslEs300 ->
        "#version 300 es\n"
          <> "precision highp float;\n"
          <> "in vec4 vColor;\n"
          <> "in vec2 vUv;\n"
          <> "out vec4 fragColor;\n"

glslDialect :: GlslTarget -> CLikeDialect
glslDialect target =
  CLikeDialect
    { cdType = glslType
    , cdSysField = \case
        SysRect -> "uRect"
        SysUvRect -> "uUvRect"
        SysMeta -> "uMeta"
        SysVColor -> "vColor"
        SysVUv -> case target of
          Glsl150 -> "vUV"
          GlslEs300 -> "vUv"
    , cdUniform = \i gt -> case gt of
        GBool -> "(u_" <> intB i <> " != 0)"
        _ -> "u_" <> intB i
    , cdMod = \a b -> "mod(" <> a <> ", " <> b <> ")"
    , cdAtan2 = \a b -> "atan(" <> a <> ", " <> b <> ")"
    , cdSampleInput = \uv -> "nanoSampleInput(" <> uv <> ")"
    }

uniformBlock :: [UniformField] -> Builder
uniformBlock fields =
  "layout(std140) uniform NanoShaderBlock {\n"
    <> "  vec4 uRect;\n"
    <> "  vec4 uUvRect;\n"
    <> "  vec4 uMeta;\n"
    <> mconcat (zipWith emitField [0 :: Int ..] fields)
    <> "};\n"
  where
    emitField i uf =
      "  " <> glslUniformType (ufType uf) <> " u_" <> intB i <> ";\n"

glslUniformType :: GpuType -> Builder
glslUniformType gt = case gt of
  GFloat -> "float"
  GInt32 -> "int"
  GUInt32 -> "uint"
  GBool -> "int"
  GVec2 -> "vec2"
  GVec3 -> "vec3"
  GVec4 -> "vec4"

glslType :: IRType -> Builder
glslType ty = case ty of
  TFloat -> "float"
  TInt -> "int"
  TUInt -> "uint"
  TBool -> "bool"
  TVec2 -> "vec2"
  TVec3 -> "vec3"
  TVec4 -> "vec4"
