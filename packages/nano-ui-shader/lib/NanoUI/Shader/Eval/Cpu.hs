{-# LANGUAGE StrictData #-}

-- | Pure-Haskell CPU reference evaluator for 'ShaderIR'.
--
-- Evaluates the exact same lowered SSA IR emitted to GLSL, MSL, and SPIR-V,
-- enabling headless golden testing, CPU-vs-GPU differential pixel checks, and
-- optional SVG raster fallbacks.
module NanoUI.Shader.Eval.Cpu
  ( evalShaderIR
  , evalShaderIRColor
  ) where

import Data.Bits (shiftR, (.&.))
import Data.Int (Int32)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Primitive.ByteArray (ByteArray, indexByteArray)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32)
import GHC.Float (castWord32ToFloat)
import NanoUI.Internal.Shader (CpuFragCtx (..))
import NanoUI (Color (..), V2 (..), colorRGBA)
import NanoUI.Internal.Uniforms (GpuType (..), UniformField (..))
import NanoUI.Shader.IR

data Val
  = VF {-# UNPACK #-} !Float
  | VI {-# UNPACK #-} !Int32
  | VU {-# UNPACK #-} !Word32
  | VB !Bool
  | VV2 {-# UNPACK #-} !Float {-# UNPACK #-} !Float
  | VV3 {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float
  | VV4 {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float
  | VTuple ![Val]

-- | Evaluate a 'ShaderIR' at fragment context @ctx@ with packed std140 user
-- uniform bytes @uBytes@ and input sampler @sampleTex@ (taking local logical
-- pixel coordinates), returning straight-alpha RGBA floats in @[0, 1]@.
evalShaderIR ::
  ShaderIR ->
  ByteArray ->
  CpuFragCtx ->
  (V2 -> Color) ->
  (Float, Float, Float, Float)
evalShaderIR ir uBytes ctx sampleTex =
  let fields = V.fromList (irUniforms ir)
      vals = evalScope fields uBytes ctx sampleTex [] IM.empty IM.empty (irNodes ir)
   in case vals V.! unNodeId (irOutput ir) of
        VV4 r g b a -> (r, g, b, a)
        _ -> (0, 0, 0, 0)

clamp01 :: Float -> Float
clamp01 x = max 0 (min 1 x)

-- | Like 'evalShaderIR', quantizing the result to an 8-bit straight-alpha
-- 'Color' (clamped to @[0, 1]@).
evalShaderIRColor :: ShaderIR -> ByteArray -> CpuFragCtx -> (V2 -> Color) -> Color
evalShaderIRColor ir uBytes ctx sampleTex =
  let (r, g, b, a) = evalShaderIR ir uBytes ctx sampleTex
      toByte x = round (255 * clamp01 x)
   in colorRGBA (toByte r) (toByte g) (toByte b) (toByte a)

evalScope ::
  Vector UniformField ->
  ByteArray ->
  CpuFragCtx ->
  (V2 -> Color) ->
  [Vector Val] ->
  IntMap Int32 ->
  IntMap [Val] ->
  Vector (IRType, ExprOp) ->
  Vector Val
evalScope fields uBytes ctx sampleTex outerEnvs loopVars loopStates nodes =
  V.constructN (V.length nodes) $ \prefix ->
    let !idx = V.length prefix
        (_, op) = nodes V.! idx
     in evalOp fields uBytes ctx sampleTex outerEnvs loopVars loopStates prefix op

evalOp ::
  Vector UniformField ->
  ByteArray ->
  CpuFragCtx ->
  (V2 -> Color) ->
  [Vector Val] ->
  IntMap Int32 ->
  IntMap [Val] ->
  Vector Val ->
  ExprOp ->
  Val
evalOp fields uBytes ctx sampleTex outerEnvs loopVars loopStates env op =
  let at (NodeId i) = env V.! i
   in case op of
        OpLitFloat bits -> VF (castWord32ToFloat bits)
        OpLitInt n -> VI n
        OpLitUInt w -> VU w
        OpLitBool b -> VB b
        OpSys sf -> case sf of
          SysRect -> let V2 w h = cfcSize ctx in VV4 0 0 w h
          SysUvRect -> VV4 0 0 1 1
          SysMeta -> let V2 w h = cfcSize ctx in VV4 (cfcScale ctx) (cfcCornerRadius ctx) w h
          SysVColor -> colorToVal (cfcColor ctx)
          SysVUv -> let V2 u v = cfcUv ctx in VV2 u v
        OpUniform i gt ->
          let off = ufOffset (fields V.! i)
              wAt o = indexByteArray uBytes (o `shiftR` 2) :: Word32
              fAt o = castWord32ToFloat (wAt o)
           in case gt of
                GFloat -> VF (fAt off)
                GInt32 -> VI (fromIntegral (wAt off))
                GUInt32 -> VU (wAt off)
                GBool -> VB (wAt off /= 0)
                GVec2 -> VV2 (fAt off) (fAt (off + 4))
                GVec3 -> VV3 (fAt off) (fAt (off + 4)) (fAt (off + 8))
                GVec4 -> VV4 (fAt off) (fAt (off + 4)) (fAt (off + 8)) (fAt (off + 12))
        OpConstruct ct args ->
          let comps = concatMap (valFloats . at) args
           in case (ct, comps) of
                (TVec2, [x]) -> VV2 x x
                (TVec2, x : y : _) -> VV2 x y
                (TVec3, [x]) -> VV3 x x x
                (TVec3, x : y : z : _) -> VV3 x y z
                (TVec4, [x]) -> VV4 x x x x
                (TVec4, x : y : z : w : _) -> VV4 x y z w
                _ -> VF 0
        OpSwizzle v idxs ->
          let fs = valFloats (at v)
              pick k = if k < length fs then fs !! k else 0
           in case map pick idxs of
                [x] -> VF x
                [x, y] -> VV2 x y
                [x, y, z] -> VV3 x y z
                [x, y, z, w] -> VV4 x y z w
                _ -> VF 0
        OpUnary uop a -> evalUnary uop (at a)
        OpBinary bop a b -> evalBinary bop (at a) (at b)
        OpTernary top a b c -> evalTernary top (at a) (at b) (at c)
        OpLength a ->
          let fs = valFloats (at a)
           in VF (sqrt (sum (map (\x -> x * x) fs)))
        OpNormalize a ->
          let v = at a
              fs = valFloats v
              len = sqrt (sum (map (\x -> x * x) fs))
              inv = if len > 0 then 1 / len else 0
           in mapFloatVal (* inv) v
        OpSampleInput uvNode ->
          let (u, v) = case at uvNode of
                VV2 x y -> (x, y)
                _ -> (0, 0)
              V2 w h = cfcSize ctx
           in colorToVal (sampleTex (V2 (u * w) (v * h)))
        OpOuterRef d (NodeId nid) ->
          -- outerEnvs is stored outermost-first via append at loop entry.
          (outerEnvs !! d) V.! nid
        OpLoopVar lid -> VI (IM.findWithDefault 0 lid loopVars)
        OpLoopState lid slot -> (IM.findWithDefault [] lid loopStates) !! slot
        OpLoop lb ->
          let lid = lbLoopId lb
              initVals = map at (lbInits lb)
              envsWithMe = outerEnvs ++ [env]
              step !acc !i =
                let lv' = IM.insert lid i loopVars
                    ls' = IM.insert lid acc loopStates
                    bodyVals = evalScope fields uBytes ctx sampleTex envsWithMe lv' ls' (lbBodyNodes lb)
                 in map (\(NodeId nid) -> bodyVals V.! nid) (lbNextStates lb)
              finalVals = foldl' step initVals [lbStart lb .. lbEnd lb - 1]
           in VTuple finalVals
        OpLoopResult ln slot -> case at ln of
          VTuple vs -> vs !! slot
          other -> other

colorToVal :: Color -> Val
colorToVal (Color w) =
  let inv = 1.0 / 255.0 :: Float
      r = fromIntegral ((w `shiftR` 24) .&. 0xFF) * inv
      g = fromIntegral ((w `shiftR` 16) .&. 0xFF) * inv
      b = fromIntegral ((w `shiftR` 8) .&. 0xFF) * inv
      a = fromIntegral (w .&. 0xFF) * inv
   in VV4 r g b a

valFloats :: Val -> [Float]
valFloats v = case v of
  VF x -> [x]
  VV2 x y -> [x, y]
  VV3 x y z -> [x, y, z]
  VV4 x y z w -> [x, y, z, w]
  VI n -> [fromIntegral n]
  VU w -> [fromIntegral w]
  VB b -> [if b then 1 else 0]
  VTuple _ -> []

mapFloatVal :: (Float -> Float) -> Val -> Val
mapFloatVal f v = case v of
  VF x -> VF (f x)
  VV2 x y -> VV2 (f x) (f y)
  VV3 x y z -> VV3 (f x) (f y) (f z)
  VV4 x y z w -> VV4 (f x) (f y) (f z) (f w)
  other -> other

zipFloatVal :: (Float -> Float -> Float) -> Val -> Val -> Val
zipFloatVal f a b = case (a, b) of
  (VF x, VF y) -> VF (f x y)
  (VV2 x1 y1, VV2 x2 y2) -> VV2 (f x1 x2) (f y1 y2)
  (VV3 x1 y1 z1, VV3 x2 y2 z2) -> VV3 (f x1 x2) (f y1 y2) (f z1 z2)
  (VV4 x1 y1 z1 w1, VV4 x2 y2 z2 w2) -> VV4 (f x1 x2) (f y1 y2) (f z1 z2) (f w1 w2)
  (VV2 x1 y1, VF s) -> VV2 (f x1 s) (f y1 s)
  (VV3 x1 y1 z1, VF s) -> VV3 (f x1 s) (f y1 s) (f z1 s)
  (VV4 x1 y1 z1 w1, VF s) -> VV4 (f x1 s) (f y1 s) (f z1 s) (f w1 s)
  (VF s, VV2 x2 y2) -> VV2 (f s x2) (f s y2)
  (VF s, VV3 x2 y2 z2) -> VV3 (f s x2) (f s y2) (f s z2)
  (VF s, VV4 x2 y2 z2 w2) -> VV4 (f s x2) (f s y2) (f s z2) (f s w2)
  _ -> a

evalUnary :: UnOp -> Val -> Val
evalUnary uop v = case uop of
  UNeg -> case v of
    VI n -> VI (negate n)
    VU w -> VU (negate w)
    _ -> mapFloatVal negate v
  UAbs -> case v of
    VI n -> VI (abs n)
    VU w -> VU w
    _ -> mapFloatVal abs v
  UFloor -> mapFloatVal (fromIntegral . (floor :: Float -> Int)) v
  UCeil -> mapFloatVal (fromIntegral . (ceiling :: Float -> Int)) v
  UFract -> mapFloatVal (\x -> x - fromIntegral (floor x :: Int)) v
  USqrt -> mapFloatVal sqrt v
  USin -> mapFloatVal sin v
  UCos -> mapFloatVal cos v
  UTan -> mapFloatVal tan v
  UExp -> mapFloatVal exp v
  ULog -> mapFloatVal log v
  ULog2 -> mapFloatVal (logBase 2) v
  USign -> case v of
    VI n -> VI (signum n)
    VU w -> VU (if w == 0 then 0 else 1)
    _ -> mapFloatVal signum v
  UNot -> case v of
    VB b -> VB (not b)
    _ -> VB False
  UFloatToInt -> case v of
    VF x -> VI (truncate x)
    _ -> VI 0
  UIntToFloat -> case v of
    VI n -> VF (fromIntegral n)
    VU w -> VF (fromIntegral w)
    _ -> VF 0

evalBinary :: BinOp -> Val -> Val -> Val
evalBinary bop a b = case (bop, a, b) of
  (BAdd, VI x, VI y) -> VI (x + y)
  (BSub, VI x, VI y) -> VI (x - y)
  (BMul, VI x, VI y) -> VI (x * y)
  (BDiv, VI x, VI y) -> VI (if y == 0 then 0 else x `quot` y)
  (BAdd, VU x, VU y) -> VU (x + y)
  (BSub, VU x, VU y) -> VU (x - y)
  (BMul, VU x, VU y) -> VU (x * y)
  (BDiv, VU x, VU y) -> VU (if y == 0 then 0 else x `quot` y)
  (BAdd, _, _) -> zipFloatVal (+) a b
  (BSub, _, _) -> zipFloatVal (-) a b
  (BMul, _, _) -> zipFloatVal (*) a b
  (BDiv, _, _) -> zipFloatVal (/) a b
  (BMod, _, _) -> zipFloatVal (\x y -> x - y * fromIntegral (floor (x / y) :: Int)) a b
  (BMin, VI x, VI y) -> VI (min x y)
  (BMax, VI x, VI y) -> VI (max x y)
  (BMin, VU x, VU y) -> VU (min x y)
  (BMax, VU x, VU y) -> VU (max x y)
  (BMin, _, _) -> zipFloatVal min a b
  (BMax, _, _) -> zipFloatVal max a b
  (BPow, _, _) -> zipFloatVal (**) a b
  (BAtan2, VF y, VF x) -> VF (atan2 y x)
  (BStep, VF e, VF x) -> VF (if x < e then 0 else 1)
  (BEq, VF x, VF y) -> VB (x == y)
  (BEq, VI x, VI y) -> VB (x == y)
  (BEq, VU x, VU y) -> VB (x == y)
  (BEq, VB x, VB y) -> VB (x == y)
  (BNeq, VF x, VF y) -> VB (x /= y)
  (BNeq, VI x, VI y) -> VB (x /= y)
  (BNeq, VU x, VU y) -> VB (x /= y)
  (BNeq, VB x, VB y) -> VB (x /= y)
  (BLt, VF x, VF y) -> VB (x < y)
  (BLt, VI x, VI y) -> VB (x < y)
  (BLt, VU x, VU y) -> VB (x < y)
  (BLe, VF x, VF y) -> VB (x <= y)
  (BLe, VI x, VI y) -> VB (x <= y)
  (BLe, VU x, VU y) -> VB (x <= y)
  (BGt, VF x, VF y) -> VB (x > y)
  (BGt, VI x, VI y) -> VB (x > y)
  (BGt, VU x, VU y) -> VB (x > y)
  (BGe, VF x, VF y) -> VB (x >= y)
  (BGe, VI x, VI y) -> VB (x >= y)
  (BGe, VU x, VU y) -> VB (x >= y)
  (BAnd, VB x, VB y) -> VB (x && y)
  (BOr, VB x, VB y) -> VB (x || y)
  (BDot, _, _) -> VF (sum (zipWith (*) (valFloats a) (valFloats b)))
  (BCross, VV3 x1 y1 z1, VV3 x2 y2 z2) ->
    VV3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)
  _ -> VF 0

evalTernary :: TriOp -> Val -> Val -> Val -> Val
evalTernary top a b c = case top of
  TMix -> case c of
    VF t -> zipFloatVal (\x y -> x * (1 - t) + y * t) a b
    _ -> a
  TClamp -> case (a, b, c) of
    (VI x, VI lo, VI hi) -> VI (min hi (max lo x))
    (VU x, VU lo, VU hi) -> VU (min hi (max lo x))
    _ ->
      let lo = zipFloatVal max a b
       in zipFloatVal min lo c
  TSmoothstep -> case (a, b, c) of
    (VF e0, VF e1, VF x) ->
      let t = clamp01 ((x - e0) / (e1 - e0))
       in VF (t * t * (3 - 2 * t))
    _ -> VF 0
  TSelect -> case a of
    VB cond -> if cond then b else c
    _ -> c
