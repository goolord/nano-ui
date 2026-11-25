{-# LANGUAGE StrictData #-}

-- | Shared C-like statement, loop, and expression emitter parameterized by a
-- small 'CLikeDialect' for GLSL 150, GLSL ES 300, and Metal Shading Language.
module NanoUI.Shader.Internal.Emit.CLike
  ( CLikeDialect (..)
  , emitCLikeNodes
  , nodeVar
  , intB
  ) where

import Data.Int (Int32)
import Data.Text qualified as T
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TB
import Data.Vector qualified as V
import Data.Word (Word32)
import GHC.Float (castWord32ToFloat)
import NanoUI.Internal.Uniforms (GpuType)
import NanoUI.Shader.Internal.IR
import Numeric (showFFloat)

-- | Dialect differences between GLSL and MSL fragment shaders.
data CLikeDialect = CLikeDialect
  { cdType :: IRType -> Builder
  , cdSysField :: SysField -> Builder
  , cdUniform :: Int -> GpuType -> Builder
  , cdMod :: Builder -> Builder -> Builder
  , cdAtan2 :: Builder -> Builder -> Builder
  , cdSampleInput :: Builder -> Builder
  }

-- | Emit a topologically ordered SSA block at nesting @depth@.
emitCLikeNodes :: CLikeDialect -> Int -> V.Vector (IRType, ExprOp) -> Builder
emitCLikeNodes d depth =
  V.ifoldl' (\acc i (ty, op) -> acc <> emitStmt d depth i ty op) mempty

emitStmt :: CLikeDialect -> Int -> Int -> IRType -> ExprOp -> Builder
emitStmt d depth idx ty op = case op of
  OpLoop lb -> emitLoopStmt d depth idx lb
  _ ->
    indent depth
      <> cdType d ty
      <> " "
      <> nodeVar depth (NodeId idx)
      <> " = "
      <> emitExpr d depth ty op
      <> ";\n"

emitLoopStmt :: CLikeDialect -> Int -> Int -> LoopBody -> Builder
emitLoopStmt d depth idx lb =
  let lid = lbLoopId lb
      ind = indent depth
      initDecls =
        mconcat
          [ ind
              <> cdType d t
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
      bodyStmts = emitCLikeNodes d innerDepth (lbBodyNodes lb)
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
              <> cdType d t
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

emitExpr :: CLikeDialect -> Int -> IRType -> ExprOp -> Builder
emitExpr d depth ty op =
  let ref = nodeVar depth
      call1 fn a = fn <> "(" <> ref a <> ")"
      call2 fn a b = fn <> "(" <> ref a <> ", " <> ref b <> ")"
      call3 fn a b c = fn <> "(" <> ref a <> ", " <> ref b <> ", " <> ref c <> ")"
      infix2 sym a b = "(" <> ref a <> sym <> ref b <> ")"
   in case op of
        OpLitFloat bits -> floatB (castWord32ToFloat bits)
        OpLitInt n -> int32B n
        OpLitUInt w -> word32B w <> "u"
        OpLitBool b -> if b then "true" else "false"
        OpSys sf -> cdSysField d sf
        OpUniform i gt -> cdUniform d i gt
        OpConstruct ct args -> cdType d ct <> "(" <> sepComma (map ref args) <> ")"
        OpSwizzle v idxs -> ref v <> "." <> mconcat (map compChar idxs)
        OpUnary uop a -> case uop of
          UNeg -> "(-" <> ref a <> ")"
          UNot -> "(!" <> ref a <> ")"
          UAbs -> call1 "abs" a
          UFloor -> call1 "floor" a
          UCeil -> call1 "ceil" a
          UFract -> call1 "fract" a
          USqrt -> call1 "sqrt" a
          USin -> call1 "sin" a
          UCos -> call1 "cos" a
          UTan -> call1 "tan" a
          UExp -> call1 "exp" a
          ULog -> call1 "log" a
          ULog2 -> call1 "log2" a
          USign -> call1 "sign" a
          UFloatToInt -> call1 "int" a
          UIntToFloat -> call1 "float" a
        OpBinary bop a b -> case bop of
          BAdd -> infix2 " + " a b
          BSub -> infix2 " - " a b
          BMul -> infix2 " * " a b
          BDiv -> infix2 " / " a b
          BMod -> cdMod d (ref a) (ref b)
          BMin -> call2 "min" a b
          BMax -> call2 "max" a b
          BPow -> call2 "pow" a b
          BAtan2 -> cdAtan2 d (ref a) (ref b)
          BStep -> call2 "step" a b
          BEq -> infix2 " == " a b
          BNeq -> infix2 " != " a b
          BLt -> infix2 " < " a b
          BLe -> infix2 " <= " a b
          BGt -> infix2 " > " a b
          BGe -> infix2 " >= " a b
          BAnd -> infix2 " && " a b
          BOr -> infix2 " || " a b
          BDot -> call2 "dot" a b
          BCross -> call2 "cross" a b
        OpTernary top a b c -> case top of
          TMix -> call3 "mix" a b c
          TClamp -> call3 "clamp" a b c
          TSmoothstep -> call3 "smoothstep" a b c
          TSelect -> "(" <> ref a <> " ? " <> ref b <> " : " <> ref c <> ")"
        OpLength a -> call1 "length" a
        OpNormalize a -> call1 "normalize" a
        OpSampleInput uv -> cdSampleInput d (ref uv)
        OpOuterRef outerD nid -> nodeVar outerD nid
        OpLoopVar lid -> loopIdxVar lid
        OpLoopState lid slot -> loopStateVar lid slot
        OpLoop _ -> cdType d ty <> "(0)"
        OpLoopResult (NodeId lidx) slot -> loopOutVar depth lidx slot

-- | Local variable name for an SSA 'NodeId' at loop nesting @depth@.
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

-- | Render an 'Int' to a 'Builder'.
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
