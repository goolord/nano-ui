{-# LANGUAGE StrictData #-}

-- | Pure-Haskell Metal Shading Language (MSL) fragment shader emitter for
-- 'ShaderIR', matching SDL 3.4's @SDL_GPURenderState@ Metal interface.
--
-- User uniform fields are emitted with explicit @float _padN@ spacers and
-- @packed_float3@ for @GVec3@ so every field's byte offset matches the core's
-- std140 byte layout exactly.
module NanoUI.Shader.Emit.Msl
  ( emitMsl
  ) where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TB
import Data.Vector qualified as V
import Data.Word (Word32)
import GHC.Float (castWord32ToFloat)
import NanoUI.Internal.Uniforms (GpuType (..), UniformField (..), gpuTypeSize)
import NanoUI.Shader.IR
import Numeric (showFFloat)

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
  , "fragment float4 main0(\n"
  , "  FragIn in [[stage_in]],\n"
  , "  constant NanoShaderBlock& u [[buffer(0)]]"
  , if irNeedsInput ir
      then ",\n  texture2d<float> uInput [[texture(0)]],\n  sampler uSampler [[sampler(0)]]\n"
      else "\n"
  , ") {\n"
  , emitNodes 0 (irNodes ir)
  , "  return " <> nodeVar 0 (irOutput ir) <> ";\n"
  , "}\n"
  ]

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

emitNodes :: Int -> V.Vector (IRType, ExprOp) -> Builder
emitNodes depth nodes =
  V.ifoldl' (\acc i (ty, op) -> acc <> emitStmt depth i ty op) mempty nodes

emitStmt :: Int -> Int -> IRType -> ExprOp -> Builder
emitStmt depth idx ty op = case op of
  OpLoop lb -> emitLoopStmt depth idx lb
  _ ->
    indent depth
      <> mslType ty
      <> " "
      <> nodeVar depth (NodeId idx)
      <> " = "
      <> emitExpr depth ty op
      <> ";\n"

emitLoopStmt :: Int -> Int -> LoopBody -> Builder
emitLoopStmt depth idx lb =
  let lid = lbLoopId lb
      ind = indent depth
      initDecls =
        mconcat
          [ ind
              <> mslType t
              <> " "
              <> loopStateVar lid slot
              <> " = "
              <> nodeVar depth initId
              <> ";\n"
          | (slot, (t, initId)) <- zip [0 :: Int ..] (zip (lbStateTypes lb) (lbInits lb))
          ]
      innerDepth = depth + 1
      indIn = indent innerDepth
      iv = loopIdxVar lid
      bodyStmts = emitNodes innerDepth (lbBodyNodes lb)
      updates =
        mconcat
          [ indIn
              <> loopStateVar lid slot
              <> " = "
              <> nodeVar innerDepth nextId
              <> ";\n"
          | (slot, nextId) <- zip [0 :: Int ..] (lbNextStates lb)
          ]
      aliasDecls =
        mconcat
          [ ind
              <> mslType t
              <> " "
              <> loopOutVar depth idx slot
              <> " = "
              <> loopStateVar lid slot
              <> ";\n"
          | (slot, t) <- zip [0 :: Int ..] (lbStateTypes lb)
          ]
   in initDecls
        <> ind
        <> "for (int "
        <> iv
        <> " = "
        <> int32B (lbStart lb)
        <> "; "
        <> iv
        <> " < "
        <> int32B (lbEnd lb)
        <> "; "
        <> iv
        <> "++) {\n"
        <> bodyStmts
        <> updates
        <> ind
        <> "}\n"
        <> aliasDecls

emitExpr :: Int -> IRType -> ExprOp -> Builder
emitExpr depth ty op =
  let ref = nodeVar depth
   in case op of
        OpLitFloat bits -> floatB (castWord32ToFloat bits)
        OpLitInt n -> int32B n
        OpLitUInt w -> word32B w <> "u"
        OpLitBool b -> if b then "true" else "false"
        OpSys sf -> case sf of
          SysRect -> "u.uRect"
          SysUvRect -> "u.uUvRect"
          SysMeta -> "u.uMeta"
          SysVColor -> "in.v_color"
          SysVUv -> "in.v_uv"
        OpUniform i gt -> case gt of
          GBool -> "(u.u_" <> intB i <> " != 0)"
          GVec3 -> "float3(u.u_" <> intB i <> ")"
          _ -> "u.u_" <> intB i
        OpConstruct ct args ->
          mslType ct <> "(" <> sepComma (map ref args) <> ")"
        OpSwizzle v idxs ->
          ref v <> "." <> mconcat (map compChar idxs)
        OpUnary uop a -> case uop of
          UNeg -> "(-" <> ref a <> ")"
          UAbs -> "abs(" <> ref a <> ")"
          UFloor -> "floor(" <> ref a <> ")"
          UCeil -> "ceil(" <> ref a <> ")"
          UFract -> "fract(" <> ref a <> ")"
          USqrt -> "sqrt(" <> ref a <> ")"
          USin -> "sin(" <> ref a <> ")"
          UCos -> "cos(" <> ref a <> ")"
          UTan -> "tan(" <> ref a <> ")"
          UExp -> "exp(" <> ref a <> ")"
          ULog -> "log(" <> ref a <> ")"
          ULog2 -> "log2(" <> ref a <> ")"
          USign -> "sign(" <> ref a <> ")"
          UNot -> "(!" <> ref a <> ")"
          UFloatToInt -> "int(" <> ref a <> ")"
          UIntToFloat -> "float(" <> ref a <> ")"
        OpBinary bop a b -> case bop of
          BAdd -> "(" <> ref a <> " + " <> ref b <> ")"
          BSub -> "(" <> ref a <> " - " <> ref b <> ")"
          BMul -> "(" <> ref a <> " * " <> ref b <> ")"
          BDiv -> "(" <> ref a <> " / " <> ref b <> ")"
          BMod -> "(" <> ref a <> " - " <> ref b <> " * floor(" <> ref a <> " / " <> ref b <> "))"
          BMin -> "min(" <> ref a <> ", " <> ref b <> ")"
          BMax -> "max(" <> ref a <> ", " <> ref b <> ")"
          BPow -> "pow(" <> ref a <> ", " <> ref b <> ")"
          BAtan2 -> "atan2(" <> ref a <> ", " <> ref b <> ")"
          BStep -> "step(" <> ref a <> ", " <> ref b <> ")"
          BEq -> "(" <> ref a <> " == " <> ref b <> ")"
          BNeq -> "(" <> ref a <> " != " <> ref b <> ")"
          BLt -> "(" <> ref a <> " < " <> ref b <> ")"
          BLe -> "(" <> ref a <> " <= " <> ref b <> ")"
          BGt -> "(" <> ref a <> " > " <> ref b <> ")"
          BGe -> "(" <> ref a <> " >= " <> ref b <> ")"
          BAnd -> "(" <> ref a <> " && " <> ref b <> ")"
          BOr -> "(" <> ref a <> " || " <> ref b <> ")"
          BDot -> "dot(" <> ref a <> ", " <> ref b <> ")"
          BCross -> "cross(" <> ref a <> ", " <> ref b <> ")"
        OpTernary top a b c -> case top of
          TMix -> "mix(" <> ref a <> ", " <> ref b <> ", " <> ref c <> ")"
          TClamp -> "clamp(" <> ref a <> ", " <> ref b <> ", " <> ref c <> ")"
          TSmoothstep -> "smoothstep(" <> ref a <> ", " <> ref b <> ", " <> ref c <> ")"
          TSelect -> "(" <> ref a <> " ? " <> ref b <> " : " <> ref c <> ")"
        OpLength a -> "length(" <> ref a <> ")"
        OpNormalize a -> "normalize(" <> ref a <> ")"
        OpSampleInput uv -> "uInput.sample(uSampler, " <> ref uv <> ")"
        OpOuterRef d nid -> nodeVar d nid
        OpLoopVar lid -> loopIdxVar lid
        OpLoopState lid slot -> loopStateVar lid slot
        OpLoop _ -> mslType ty <> "(0)"
        OpLoopResult (NodeId lidx) slot -> loopOutVar depth lidx slot

nodeVar :: Int -> NodeId -> Builder
nodeVar depth (NodeId i) = "v" <> intB depth <> "_" <> intB i

loopIdxVar :: Int -> Builder
loopIdxVar lid = "i_" <> intB lid

loopStateVar :: Int -> Int -> Builder
loopStateVar lid slot = "ls_" <> intB lid <> "_" <> intB slot

loopOutVar :: Int -> Int -> Int -> Builder
loopOutVar depth lidx slot = "lo" <> intB depth <> "_" <> intB lidx <> "_" <> intB slot

compChar :: Int -> Builder
compChar 0 = "x"
compChar 1 = "y"
compChar 2 = "z"
compChar _ = "w"

indent :: Int -> Builder
indent d = TB.fromText (T.replicate (2 * (d + 1)) " ")

sepComma :: [Builder] -> Builder
sepComma [] = mempty
sepComma [x] = x
sepComma (x : xs) = x <> ", " <> sepComma xs

intB :: Int -> Builder
intB = TB.fromString . show

int32B :: Int32 -> Builder
int32B = TB.fromString . show

word32B :: Word32 -> Builder
word32B = TB.fromString . show

floatB :: Float -> Builder
floatB f
  | isNaN f = "0.0"
  | isInfinite f = if f > 0 then "1e38" else "-1e38"
  | otherwise =
      let s = showFFloat Nothing f ""
       in TB.fromString (if '.' `elem` s || 'e' `elem` s || 'E' `elem` s then s else s ++ ".0")
