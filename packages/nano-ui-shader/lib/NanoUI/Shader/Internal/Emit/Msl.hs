{-# LANGUAGE StrictData #-}

-- | Pure-Haskell Metal Shading Language (MSL) fragment shader emitter for
-- 'ShaderIR', matching SDL 3.4's @SDL_GPURenderState@ Metal interface.
--
-- User uniform fields are emitted with explicit @float _padN@ spacers and
-- @packed_float3@ for @GVec3@ so every field's byte offset matches the core's
-- std140 byte layout exactly.
module NanoUI.Shader.Internal.Emit.Msl
  ( emitMsl
  ) where

import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TB
import NanoUI.Internal.Uniforms (GpuType (..), UniformField (..), gpuTypeSize)
import NanoUI.Shader.Internal.Emit.CLike (CLikeDialect (..), emitCLikeNodes, intB, nodeVar)
import NanoUI.Shader.Internal.IR

-- | Emit an MSL fragment shader with entrypoint @main0@ for SDL 3.4 Metal.
emitMsl :: ShaderIR -> Text
emitMsl ir = TL.toStrict . TB.toLazyText $ mconcat
  [ "#include <metal_stdlib>\n"
  , "using namespace metal;\n"
  , "struct FragIn {\n"
  , "  float4 v_color [[user(locn0)]];\n"
  , "  float2 v_uv [[user(locn1)]];\n"
  , "};\n"
  , mslUniformStruct (irUniforms ir) (irUniformBytes ir)
  , if irNeedsInput ir
      then
        "inline float4 nanoSampleInput(texture2d<float> tex, sampler samp, float2 uv) {\n"
          <> "  float4 s = tex.sample(samp, uv);\n"
          <> "  return float4(min(s.rgb / max(s.a, 1e-5f), float3(1.0f)), s.a);\n"
          <> "}\n"
      else ""
  , "fragment float4 main0(\n"
  , "  FragIn in [[stage_in]],\n"
  , "  constant NanoShaderBlock& u [[buffer(0)]]"
  , if irNeedsInput ir
      then ",\n  texture2d<float> uInput [[texture(0)]],\n  sampler uSampler [[sampler(0)]]\n"
      else "\n"
  , ") {\n"
  , emitCLikeNodes mslDialect 0 (irNodes ir)
  , "  return " <> nodeVar 0 (irOutput ir) <> ";\n"
  , "}\n"
  ]

mslDialect :: CLikeDialect
mslDialect =
  CLikeDialect
    { cdType = mslType
    , cdSysField = \case
        SysRect -> "u.uRect"
        SysUvRect -> "u.uUvRect"
        SysMeta -> "u.uMeta"
        SysVColor -> "in.v_color"
        SysVUv -> "in.v_uv"
    , cdUniform = \i gt -> case gt of
        GBool -> "(u.u_" <> intB i <> " != 0)"
        GVec3 -> "float3(u.u_" <> intB i <> ")"
        _ -> "u.u_" <> intB i
    , cdMod = \a b -> "(" <> a <> " - " <> b <> " * floor(" <> a <> " / " <> b <> "))"
    , cdAtan2 = \a b -> "atan2(" <> a <> ", " <> b <> ")"
    , cdSampleInput = \uv -> "nanoSampleInput(uInput, uSampler, " <> uv <> ")"
    }

-- | Emit @NanoShaderBlock@ with explicit 4-byte padding fields so every user
-- uniform lands at its exact std140 byte offset.
mslUniformStruct :: [UniformField] -> Int -> Builder
mslUniformStruct fields totalUserBytes =
  "struct NanoShaderBlock {\n"
    <> "  float4 uRect;\n"
    <> "  float4 uUvRect;\n"
    <> "  float4 uMeta;\n"
    <> go 0 0 fields
    <> "};\n"
  where
    go :: Int -> Int -> [UniformField] -> Builder
    go !cursor !idx [] = padTo cursor totalUserBytes idx
    go !cursor !idx (uf : rest) =
      let target = ufOffset uf
          pads = padTo cursor target idx
          decl = "  " <> mslUniformType (ufType uf) <> " u_" <> intB idx <> ";\n"
          nextCur = target + gpuTypeSize (ufType uf)
       in pads <> decl <> go nextCur (idx + 1) rest

    padTo :: Int -> Int -> Int -> Builder
    padTo cur end tag
      | cur >= end = mempty
      | otherwise =
          let wordsN = (end - cur) `quot` 4
           in mconcat
                [ "  float _pad_" <> intB tag <> "_" <> intB k <> ";\n"
                | k <- [0 .. wordsN - 1]
                ]

mslUniformType :: GpuType -> Builder
mslUniformType gt = case gt of
  GFloat -> "float"
  GInt32 -> "int"
  GUInt32 -> "uint"
  GBool -> "int"
  GVec2 -> "float2"
  GVec3 -> "packed_float3"
  GVec4 -> "float4"

mslType :: IRType -> Builder
mslType ty = case ty of
  TFloat -> "float"
  TInt -> "int"
  TUInt -> "uint"
  TBool -> "bool"
  TVec2 -> "float2"
  TVec3 -> "float3"
  TVec4 -> "float4"
