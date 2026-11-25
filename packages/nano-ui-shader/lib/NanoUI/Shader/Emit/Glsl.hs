{-# LANGUAGE StrictData #-}

-- | Pure-Haskell GLSL 150 (OpenGL 3.2, @nano-ui-rgfw@) and GLSL ES 300
-- (WebGL2, @nano-ui-sdl-web@) fragment shader emitters for 'ShaderIR'.
module NanoUI.Shader.Emit.Glsl
  ( GlslTarget (..)
  , emitGlsl
  , emitGlsl150
  , emitGlslEs300
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
import NanoUI.Internal.Uniforms (GpuType (..), UniformField (..))
import NanoUI.Shader.IR
import Numeric (showFFloat)

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
  , if irNeedsInput ir then "uniform sampler2D uInput;\n" else ""
  , "void main() {\n"
  , emitNodes target 0 (irNodes ir)
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

emitNodes :: GlslTarget -> Int -> V.Vector (IRType, ExprOp) -> Builder
emitNodes target depth nodes =
  V.ifoldl' (\acc i (ty, op) -> acc <> emitStmt target depth i ty op) mempty nodes

emitStmt :: GlslTarget -> Int -> Int -> IRType -> ExprOp -> Builder
emitStmt target depth idx ty op = case op of
  OpLoop lb -> emitLoopStmt target depth idx lb
  _ ->
    indent depth
      <> glslType ty
      <> " "
      <> nodeVar depth (NodeId idx)
      <> " = "
      <> emitExpr target depth ty op
      <> ";\n"

emitLoopStmt :: GlslTarget -> Int -> Int -> LoopBody -> Builder
emitLoopStmt target depth idx lb =
  let lid = lbLoopId lb
      ind = indent depth
      initDecls =
        mconcat
          [ ind
              <> glslType t
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
      bodyStmts = emitNodes target innerDepth (lbBodyNodes lb)
      updates =
        mconcat
          [ indIn
              <> loopStateVar lid slot
              <> " = "
              <> nodeVar innerDepth nextId
              <> ";\n"
          | (slot, nextId) <- zip [0 :: Int ..] (lbNextStates lb)
          ]
      -- Bind the loop's node id to slot 0 when non-empty for uniform naming.
      aliasDecls =
        mconcat
          [ ind
              <> glslType t
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

emitExpr :: GlslTarget -> Int -> IRType -> ExprOp -> Builder
emitExpr target depth ty op =
  let ref = nodeVar depth
   in case op of
        OpLitFloat bits -> floatB (castWord32ToFloat bits)
        OpLitInt n -> int32B n
        OpLitUInt w -> word32B w <> "u"
        OpLitBool b -> if b then "true" else "false"
        OpSys sf -> case sf of
          SysRect -> "uRect"
          SysUvRect -> "uUvRect"
          SysMeta -> "uMeta"
          SysVColor -> "vColor"
          SysVUv -> case target of
            Glsl150 -> "vUV"
            GlslEs300 -> "vUv"
        OpUniform i gt -> case gt of
          GBool -> "(u_" <> intB i <> " != 0)"
          _ -> "u_" <> intB i
        OpConstruct ct args ->
          glslType ct <> "(" <> sepComma (map ref args) <> ")"
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
          BMod -> "mod(" <> ref a <> ", " <> ref b <> ")"
          BMin -> "min(" <> ref a <> ", " <> ref b <> ")"
          BMax -> "max(" <> ref a <> ", " <> ref b <> ")"
          BPow -> "pow(" <> ref a <> ", " <> ref b <> ")"
          BAtan2 -> "atan(" <> ref a <> ", " <> ref b <> ")"
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
        OpSampleInput uv -> "texture(uInput, " <> ref uv <> ")"
        OpOuterRef d nid -> nodeVar d nid
        OpLoopVar lid -> loopIdxVar lid
        OpLoopState lid slot -> loopStateVar lid slot
        OpLoop _ -> glslType ty <> "(0)"
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
