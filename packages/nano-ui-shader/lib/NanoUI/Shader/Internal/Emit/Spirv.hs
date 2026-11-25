{-# LANGUAGE StrictData #-}

-- | Pure-Haskell SPIR-V 1.0 binary fragment shader emitter for 'ShaderIR',
-- matching SDL 3.4's @SDL_GPURenderState@ Vulkan interface:
--
-- * Fragment inputs: @location 0@ (@vec4 v_color@), @location 1@ (@vec2 v_uv@)
-- * Fragment output: @location 0@ (@vec4 fragColor@)
-- * Sampled input texture (when 'irNeedsInput'): @DescriptorSet 2, Binding 0@
-- * Std140 uniform block: @DescriptorSet 3, Binding 0@
-- * Entry point: @"main"@ with @OriginUpperLeft@
module NanoUI.Shader.Internal.Emit.Spirv
  ( emitSpirv
  , emitSpirvWords
  ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int32)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32, Word8)
import GHC.Float (castFloatToWord32)
import NanoUI.Internal.Uniforms (GpuType (..), UniformField (..))
import NanoUI.Shader.Internal.IR

-- ---------------------------------------------------------------------------
-- SPIR-V 1.0 and GLSL.std.450 constants (Khronos specification)

spvMagic, spvVersion10 :: Word32
spvMagic = 0x07230203
spvVersion10 = 0x00010000

-- Core opcodes
opExtInstImport, opExtInst, opMemoryModel, opEntryPoint, opExecutionMode, opCapability :: Word32
(opExtInstImport, opExtInst, opMemoryModel, opEntryPoint, opExecutionMode, opCapability) = (11, 12, 14, 15, 16, 17)

opTypeVoid, opTypeBool, opTypeInt, opTypeFloat, opTypeVector, opTypeImage, opTypeSampledImage, opTypeStruct, opTypePointer, opTypeFunction :: Word32
(opTypeVoid, opTypeBool, opTypeInt, opTypeFloat, opTypeVector, opTypeImage, opTypeSampledImage, opTypeStruct, opTypePointer, opTypeFunction) = (19, 20, 21, 22, 23, 25, 27, 30, 32, 33)

opConstantTrue, opConstantFalse, opConstant, opFunction, opFunctionEnd, opVariable, opLoad, opStore, opAccessChain, opDecorate, opMemberDecorate :: Word32
(opConstantTrue, opConstantFalse, opConstant, opFunction, opFunctionEnd, opVariable, opLoad, opStore, opAccessChain, opDecorate, opMemberDecorate) = (41, 42, 43, 54, 56, 59, 61, 62, 65, 71, 72)

opVectorShuffle, opCompositeConstruct, opCompositeExtract, opImageSampleImplicitLod, opConvertFToS, opConvertSToF :: Word32
(opVectorShuffle, opCompositeConstruct, opCompositeExtract, opImageSampleImplicitLod, opConvertFToS, opConvertSToF) = (79, 80, 81, 87, 110, 111)

opSNegate, opFNegate, opIAdd, opFAdd, opISub, opFSub, opIMul, opFMul, opUDiv, opSDiv, opFDiv, opFMod, opDot :: Word32
(opSNegate, opFNegate, opIAdd, opFAdd, opISub, opFSub, opIMul, opFMul, opUDiv, opSDiv, opFDiv, opFMod, opDot) = (126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 141, 148)

opLogicalEqual, opLogicalNotEqual, opLogicalOr, opLogicalAnd, opLogicalNot, opSelect :: Word32
(opLogicalEqual, opLogicalNotEqual, opLogicalOr, opLogicalAnd, opLogicalNot, opSelect) = (164, 165, 166, 167, 168, 169)

opIEqual, opINotEqual, opUGreaterThan, opSGreaterThan, opUGreaterThanEqual, opSGreaterThanEqual, opULessThan, opSLessThan, opULessThanEqual, opSLessThanEqual :: Word32
(opIEqual, opINotEqual, opUGreaterThan, opSGreaterThan, opUGreaterThanEqual, opSGreaterThanEqual, opULessThan, opSLessThan, opULessThanEqual, opSLessThanEqual) = (170, 171, 172, 173, 174, 175, 176, 177, 178, 179)

opFOrdEqual, opFOrdNotEqual, opFOrdLessThan, opFOrdGreaterThan, opFOrdLessThanEqual, opFOrdGreaterThanEqual :: Word32
(opFOrdEqual, opFOrdNotEqual, opFOrdLessThan, opFOrdGreaterThan, opFOrdLessThanEqual, opFOrdGreaterThanEqual) = (180, 182, 184, 186, 188, 190)

opLoopMerge, opLabel, opBranch, opBranchConditional, opReturn :: Word32
(opLoopMerge, opLabel, opBranch, opBranchConditional, opReturn) = (246, 248, 249, 250, 253)

-- Enums
capShader, execModelFragment, execModeOriginUpperLeft :: Word32
(capShader, execModelFragment, execModeOriginUpperLeft) = (1, 4, 7)

decBlock, decLocation, decBinding, decDescriptorSet, decOffset :: Word32
(decBlock, decLocation, decBinding, decDescriptorSet, decOffset) = (2, 30, 33, 34, 35)

scUniformConstant, scInput, scUniform, scOutput, scFunction :: Word32
(scUniformConstant, scInput, scUniform, scOutput, scFunction) = (0, 1, 2, 3, 7)

-- GLSL.std.450 extended instructions
glslFAbs, glslSAbs, glslFSign, glslSSign, glslFloor, glslCeil, glslFract :: Word32
(glslFAbs, glslSAbs, glslFSign, glslSSign, glslFloor, glslCeil, glslFract) = (4, 5, 6, 7, 8, 9, 10)

glslSin, glslCos, glslTan, glslAtan2, glslPow, glslExp, glslLog, glslLog2, glslSqrt :: Word32
(glslSin, glslCos, glslTan, glslAtan2, glslPow, glslExp, glslLog, glslLog2, glslSqrt) = (13, 14, 15, 25, 26, 27, 28, 30, 31)

glslFMin, glslUMin, glslSMin, glslFMax, glslUMax, glslSMax, glslFClamp, glslUClamp, glslSClamp, glslFMix, glslStep, glslSmoothStep, glslLength, glslCross, glslNormalize :: Word32
(glslFMin, glslUMin, glslSMin, glslFMax, glslUMax, glslSMax, glslFClamp, glslUClamp, glslSClamp, glslFMix, glslStep, glslSmoothStep, glslLength, glslCross, glslNormalize) = (37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 48, 49, 66, 68, 69)

-- ---------------------------------------------------------------------------
-- Builder state & on-demand type/constant memoization

data TypeKey
  = TKVoid
  | TKFnVoid
  | TKIR !IRType
  | TKBVec !Int
  | TKImage2D
  | TKSampledImage2D
  | TKPtr !Word32 !TypeKey
  | TKUboPtr !Word32
  deriving (Eq, Ord)

data ConstKey
  = CKFloat !Word32
  | CKInt !Int32
  | CKUInt !Word32
  | CKBool !Bool
  deriving (Eq, Ord)

data SpvEnv = SpvEnv
  { seNextId :: !Word32
  , seExtGlsl :: !Word32
  , seMainId :: !Word32
  , seEntryLabel :: !Word32
  , seVColorVar :: !Word32
  , seVUvVar :: !Word32
  , seFragOutVar :: !Word32
  , seUboVar :: !Word32
  , seTexVar :: !(Maybe Word32)
  , seTypes :: !(Map TypeKey Word32)
  , seTypeInsts :: ![[Word32]]
  -- ^ Reversed OpType* instructions emitted in section 9 (before constants).
  , seConsts :: !(Map ConstKey Word32)
  , seConstInsts :: ![[Word32]]
  -- ^ Reversed OpConstant* instructions emitted in section 9.
  , seFuncVars :: ![[Word32]]
  -- ^ Reversed function-scope OpVariable instructions emitted at entry.
  , seBody :: ![[Word32]]
  -- ^ Reversed instructions in main's body.
  }

-- | Emit a complete SPIR-V 1.0 binary fragment shader as little-endian bytes.
emitSpirv :: ShaderIR -> ByteString
emitSpirv = BSL.toStrict . BSB.toLazyByteString . foldMap BSB.word32LE . emitSpirvWords

-- | Emit a complete SPIR-V 1.0 binary fragment shader as 32-bit words.
emitSpirvWords :: ShaderIR -> [Word32]
emitSpirvWords ir =
  let (env0, decWords, globalWords) = initModule ir
      (outId, env1) = emitMain ir env0
      (tVoid, env2) = getType TKVoid env1
      (tFnVoid, env3) = getType TKFnVoid env2
      header = [spvMagic, spvVersion10, 0, seNextId env3, 0]
      caps = [inst opCapability [capShader]]
      extImports = [inst opExtInstImport (seExtGlsl env3 : encodeStr "GLSL.std.450")]
      memModel = [inst opMemoryModel [0, 1]]
      entryPts =
        [ inst opEntryPoint $
            [execModelFragment, seMainId env3]
              ++ encodeStr "main"
              ++ [seVColorVar env3, seVUvVar env3, seFragOutVar env3]
        ]
      execModes = [inst opExecutionMode [seMainId env3, execModeOriginUpperLeft]]
      fnStart =
        [ inst opFunction [tVoid, seMainId env3, 0, tFnVoid]
        , inst opLabel [seEntryLabel env3]
        ]
      fnEnd =
        [ inst opStore [seFragOutVar env3, outId]
        , inst opReturn []
        , inst opFunctionEnd []
        ]
   in concat $
        [header]
          ++ caps
          ++ extImports
          ++ memModel
          ++ entryPts
          ++ execModes
          ++ decWords
          ++ reverse (seTypeInsts env3)
          ++ reverse (seConstInsts env3)
          ++ globalWords
          ++ fnStart
          ++ reverse (seFuncVars env3)
          ++ reverse (seBody env3)
          ++ fnEnd

inst :: Word32 -> [Word32] -> [Word32]
inst opcode operands =
  let !wc = fromIntegral (1 + length operands) :: Word32
   in ((wc `shiftL` 16) .|. opcode) : operands

encodeStr :: String -> [Word32]
encodeStr s =
  let bytes = BSL.unpack (BSL.fromStrict (TE.encodeUtf8 (T.pack s))) ++ [0 :: Word8]
      padLen = (4 - (length bytes `rem` 4)) `rem` 4
      pack4 (a : b : c : d : rest) =
        let !w = fromIntegral a .|. (fromIntegral b `shiftL` 8) .|. (fromIntegral c `shiftL` 16) .|. (fromIntegral d `shiftL` 24)
         in w : pack4 rest
      pack4 _ = []
   in pack4 (bytes ++ replicate padLen 0)

allocId :: SpvEnv -> (Word32, SpvEnv)
allocId e = let !i = seNextId e in (i, e {seNextId = i + 1})

emitBody :: [Word32] -> SpvEnv -> SpvEnv
emitBody w e = e {seBody = w : seBody e}

emitRes :: Word32 -> Word32 -> [Word32] -> SpvEnv -> (Word32, SpvEnv)
emitRes opcode tyId args e =
  let (r, e1) = allocId e
   in (r, emitBody (inst opcode (tyId : r : args)) e1)

emitExt :: Word32 -> Word32 -> [Word32] -> SpvEnv -> (Word32, SpvEnv)
emitExt tyId extOp args e = emitRes opExtInst tyId (seExtGlsl e : extOp : args) e

emitLoad :: Word32 -> Word32 -> SpvEnv -> (Word32, SpvEnv)
emitLoad tyId ptrId = emitRes opLoad tyId [ptrId]

emitFuncVar :: IRType -> SpvEnv -> (Word32, SpvEnv)
emitFuncVar ty e0 =
  let (ptrTy, e1) = getType (TKPtr scFunction (TKIR ty)) e0
      (vid, e2) = allocId e1
   in (vid, e2 {seFuncVars = inst opVariable [ptrTy, vid, scFunction] : seFuncVars e2})

-- | Memoized on-demand SPIR-V type allocation.
getType :: TypeKey -> SpvEnv -> (Word32, SpvEnv)
getType key e = case Map.lookup key (seTypes e) of
  Just tid -> (tid, e)
  Nothing ->
    let record tid w e' = (tid, e' {seTypes = Map.insert key tid (seTypes e'), seTypeInsts = w : seTypeInsts e'})
        allocWith build e' = let (tid, e1) = allocId e' in record tid (build tid) e1
     in case key of
          TKVoid -> allocWith (\tid -> inst opTypeVoid [tid]) e
          TKFnVoid ->
            let (tv, e1) = getType TKVoid e
             in allocWith (\tid -> inst opTypeFunction [tid, tv]) e1
          TKIR TBool -> allocWith (\tid -> inst opTypeBool [tid]) e
          TKIR TInt -> allocWith (\tid -> inst opTypeInt [tid, 32, 1]) e
          TKIR TUInt -> allocWith (\tid -> inst opTypeInt [tid, 32, 0]) e
          TKIR TFloat -> allocWith (\tid -> inst opTypeFloat [tid, 32]) e
          TKIR TVec2 -> vecType TFloat 2 e
          TKIR TVec3 -> vecType TFloat 3 e
          TKIR TVec4 -> vecType TFloat 4 e
          TKBVec n -> vecType TBool (fromIntegral n) e
          TKImage2D ->
            let (tf, e1) = getType (TKIR TFloat) e
             in allocWith (\tid -> inst opTypeImage [tid, tf, 1, 0, 0, 0, 1, 0]) e1
          TKSampledImage2D ->
            let (img, e1) = getType TKImage2D e
             in allocWith (\tid -> inst opTypeSampledImage [tid, img]) e1
          TKPtr sc inner ->
            let (innerTy, e1) = getType inner e
             in allocWith (\tid -> inst opTypePointer [tid, sc, innerTy]) e1
          TKUboPtr structId ->
            allocWith (\tid -> inst opTypePointer [tid, scUniform, structId]) e
  where
    vecType elemTy n e0 =
      let (et, e1) = getType (TKIR elemTy) e0
          (tid, e2) = allocId e1
       in (tid, e2 {seTypes = Map.insert key tid (seTypes e2), seTypeInsts = inst opTypeVector [tid, et, n] : seTypeInsts e2})

irTypeId :: IRType -> SpvEnv -> (Word32, SpvEnv)
irTypeId = getType . TKIR

uboFieldIR :: GpuType -> IRType
uboFieldIR gt = case gt of
  GFloat -> TFloat
  GInt32 -> TInt
  GUInt32 -> TUInt
  GBool -> TUInt
  GVec2 -> TVec2
  GVec3 -> TVec3
  GVec4 -> TVec4

constWord :: ConstKey -> SpvEnv -> (Word32, SpvEnv)
constWord key e = case Map.lookup key (seConsts e) of
  Just cid -> (cid, e)
  Nothing ->
    let (ty, build) = case key of
          CKFloat bits -> (TFloat, \t c -> inst opConstant [t, c, bits])
          CKInt n -> (TInt, \t c -> inst opConstant [t, c, fromIntegral n])
          CKUInt u -> (TUInt, \t c -> inst opConstant [t, c, u])
          CKBool True -> (TBool, \t c -> inst opConstantTrue [t, c])
          CKBool False -> (TBool, \t c -> inst opConstantFalse [t, c])
        (tid, e1) = irTypeId ty e
        (cid, e2) = allocId e1
     in (cid, e2 {seConsts = Map.insert key cid (seConsts e2), seConstInsts = build tid cid : seConstInsts e2})

constInt :: Int32 -> SpvEnv -> (Word32, SpvEnv)
constInt = constWord . CKInt

constFloat :: Float -> SpvEnv -> (Word32, SpvEnv)
constFloat = constWord . CKFloat . castFloatToWord32

-- ---------------------------------------------------------------------------
-- Module setup

initModule :: ShaderIR -> (SpvEnv, [[Word32]], [[Word32]])
initModule ir =
  let base0 = SpvEnv 1 0 0 0 0 0 0 0 Nothing Map.empty [] Map.empty [] [] []
      (extGlsl, e1) = allocId base0
      (mainId, e2) = allocId e1
      (entryLabel, e3) = allocId e2
      (vColorVar, e4) = allocId e3
      (vUvVar, e5) = allocId e4
      (fragOutVar, e6) = allocId e5
      (uboVar, e7) = allocId e6
      (pInVec4, e8) = getType (TKPtr scInput (TKIR TVec4)) e7
      (pInVec2, e9) = getType (TKPtr scInput (TKIR TVec2)) e8
      (pOutVec4, e10) = getType (TKPtr scOutput (TKIR TVec4)) e9
      (tVec4, e11) = irTypeId TVec4 e10
      (userTys, e12) = mapAccumEnv (\st uf -> irTypeId (uboFieldIR (ufType uf)) st) e11 (irUniforms ir)
      (uboStruct, e13) = allocId e12
      e14 = e13 {seTypeInsts = inst opTypeStruct (uboStruct : tVec4 : tVec4 : tVec4 : userTys) : seTypeInsts e13}
      (pUbo, e15) = getType (TKUboPtr uboStruct) e14
      (mTexVar, texDecs, texGlobals, e16)
        | irNeedsInput ir =
            let (pSamp, s1) = getType (TKPtr scUniformConstant TKSampledImage2D) e15
                (tVar, s2) = allocId s1
             in ( Just tVar
                , [inst opDecorate [tVar, decDescriptorSet, 2], inst opDecorate [tVar, decBinding, 0]]
                , [inst opVariable [pSamp, tVar, scUniformConstant]]
                , s2
                )
        | otherwise = (Nothing, [], [], e15)
      decs =
        [ inst opDecorate [vColorVar, decLocation, 0]
        , inst opDecorate [vUvVar, decLocation, 1]
        , inst opDecorate [fragOutVar, decLocation, 0]
        , inst opDecorate [uboStruct, decBlock]
        , inst opMemberDecorate [uboStruct, 0, decOffset, 0]
        , inst opMemberDecorate [uboStruct, 1, decOffset, 16]
        , inst opMemberDecorate [uboStruct, 2, decOffset, 32]
        ]
          ++ [ inst opMemberDecorate [uboStruct, fromIntegral (3 + i), decOffset, fromIntegral (48 + ufOffset uf)]
             | (i, uf) <- zip [0 :: Int ..] (irUniforms ir)
             ]
          ++ [inst opDecorate [uboVar, decDescriptorSet, 3], inst opDecorate [uboVar, decBinding, 0]]
          ++ texDecs
      globals =
        [ inst opVariable [pInVec4, vColorVar, scInput]
        , inst opVariable [pInVec2, vUvVar, scInput]
        , inst opVariable [pOutVec4, fragOutVar, scOutput]
        , inst opVariable [pUbo, uboVar, scUniform]
        ]
          ++ texGlobals
      envFinal =
        e16
          { seExtGlsl = extGlsl
          , seMainId = mainId
          , seEntryLabel = entryLabel
          , seVColorVar = vColorVar
          , seVUvVar = vUvVar
          , seFragOutVar = fragOutVar
          , seUboVar = uboVar
          , seTexVar = mTexVar
          }
   in (envFinal, decs, globals)

mapAccumEnv :: (SpvEnv -> a -> (b, SpvEnv)) -> SpvEnv -> [a] -> ([b], SpvEnv)
mapAccumEnv _ e [] = ([], e)
mapAccumEnv f e (x : xs) =
  let (y, e1) = f e x
      (ys, e2) = mapAccumEnv f e1 xs
   in (y : ys, e2)

-- ---------------------------------------------------------------------------
-- SSA block emission

data ScopeCtx = ScopeCtx
  { scFrames :: ![Vector (IRType, Word32)]
  , scLoopVars :: !(Map Int Word32)
  , scLoopStates :: !(Map (Int, Int) (IRType, Word32))
  , scLoopResults :: !(Map Int [(IRType, Word32)])
  }

emitMain :: ShaderIR -> SpvEnv -> (Word32, SpvEnv)
emitMain ir env0 =
  let (vals, env1) = emitBlock (ScopeCtx [] Map.empty Map.empty Map.empty) (irNodes ir) env0
   in (snd (vals V.! unNodeId (irOutput ir)), env1)

emitBlock :: ScopeCtx -> Vector (IRType, ExprOp) -> SpvEnv -> (Vector (IRType, Word32), SpvEnv)
emitBlock scope nodes = go 0 IM.empty Map.empty
  where
    !n = V.length nodes
    go !i !acc !loopRes !e
      | i >= n = (V.generate n (acc IM.!), e)
      | otherwise =
          let (ty, op) = nodes V.! i
              scope' = scope {scLoopResults = loopRes}
           in case op of
                OpLoop lb ->
                  let (resList, e1) = emitLoopSpv scope' i acc lb e
                      firstVal = case resList of ((_, v) : _) -> v; [] -> 0
                   in go (i + 1) (IM.insert i (ty, firstVal) acc) (Map.insert i resList loopRes) e1
                _ ->
                  let (vid, e1) = emitOpSpv scope' acc ty op e
                   in go (i + 1) (IM.insert i (ty, vid) acc) loopRes e1

emitLoopSpv ::
  ScopeCtx ->
  Int ->
  IntMap (IRType, Word32) ->
  LoopBody ->
  SpvEnv ->
  ([(IRType, Word32)], SpvEnv)
emitLoopSpv scope outerCount curVals lb e0 =
  let lid = lbLoopId lb
      (tInt, e1) = irTypeId TInt e0
      (tBool, e2) = irTypeId TBool e1
      (iVar, e3) = emitFuncVar TInt e2
      (accVars, e4) = mapAccumEnv (flip emitFuncVar) e3 (lbStateTypes lb)
      (startConst, e5) = constInt (lbStart lb) e4
      (endConst, e6) = constInt (lbEnd lb) e5
      (oneConst, e7) = constInt 1 e6
      e8 =
        foldl'
          (\e (vPtr, initNid) -> emitBody (inst opStore [vPtr, snd (curVals IM.! unNodeId initNid)]) e)
          (emitBody (inst opStore [iVar, startConst]) e7)
          (zip accVars (lbInits lb))
      (headerLbl, e9) = allocId e8
      (bodyLbl, e10) = allocId e9
      (contLbl, e11) = allocId e10
      (mergeLbl, e12) = allocId e11
      -- Header
      e13 = emitBody (inst opLabel [headerLbl]) . emitBody (inst opBranch [headerLbl]) $ e12
      (iVal, e14) = emitLoad tInt iVar e13
      (condId, e15) = emitRes opSLessThan tBool [iVal, endConst] e14
      e16 =
        emitBody (inst opLabel [bodyLbl])
          . emitBody (inst opBranchConditional [condId, bodyLbl, mergeLbl])
          . emitBody (inst opLoopMerge [mergeLbl, contLbl, 0])
          $ e15
      -- Body
      (loadedStates, e17) = loadAccVars e16 (zip (lbStateTypes lb) accVars)
      outerVec = V.generate outerCount (curVals IM.!)
      innerScope =
        ScopeCtx
          { scFrames = scFrames scope ++ [outerVec]
          , scLoopVars = Map.insert lid iVal (scLoopVars scope)
          , scLoopStates = foldl' (\m (slot, st) -> Map.insert (lid, slot) st m) (scLoopStates scope) (zip [0 :: Int ..] loadedStates)
          , scLoopResults = Map.empty
          }
      (bodyVals, e18) = emitBlock innerScope (lbBodyNodes lb) e17
      e19 =
        emitBody (inst opLabel [contLbl])
          . emitBody (inst opBranch [contLbl])
          $ foldl'
            (\e (vPtr, nextNid) -> emitBody (inst opStore [vPtr, snd (bodyVals V.! unNodeId nextNid)]) e)
            e18
            (zip accVars (lbNextStates lb))
      -- Continue
      (iCur, e20) = emitLoad tInt iVar e19
      (iNext, e21) = emitRes opIAdd tInt [iCur, oneConst] e20
      e22 =
        emitBody (inst opLabel [mergeLbl])
          . emitBody (inst opBranch [headerLbl])
          . emitBody (inst opStore [iVar, iNext])
          $ e21
   in loadAccVars e22 (zip (lbStateTypes lb) accVars)
  where
    loadAccVars = mapAccumEnv $ \e (t, vPtr) ->
      let (tid, e1) = irTypeId t e
          (val, e2) = emitLoad tid vPtr e1
       in ((t, val), e2)

emitUboMember :: Int32 -> GpuType -> SpvEnv -> (Word32, SpvEnv)
emitUboMember memberIdx gt e0 =
  let fieldIR = uboFieldIR gt
      (idxConst, e1) = constInt memberIdx e0
      (ptrTy, e2) = getType (TKPtr scUniform (TKIR fieldIR)) e1
      (ptrId, e3) = emitRes opAccessChain ptrTy [seUboVar e2, idxConst] e2
      (valTy, e4) = irTypeId fieldIR e3
      (rawId, e5) = emitLoad valTy ptrId e4
   in case gt of
        GBool ->
          let (zeroU, e6) = constWord (CKUInt 0) e5
              (tBool, e7) = irTypeId TBool e6
           in emitRes opINotEqual tBool [rawId, zeroU] e7
        _ -> (rawId, e5)

-- ---------------------------------------------------------------------------
-- Individual instruction lowering

emitOpSpv :: ScopeCtx -> IntMap (IRType, Word32) -> IRType -> ExprOp -> SpvEnv -> (Word32, SpvEnv)
emitOpSpv scope vals ty op e0 = case op of
  OpLitFloat bits -> constWord (CKFloat bits) e0
  OpLitInt n -> constWord (CKInt n) e0
  OpLitUInt u -> constWord (CKUInt u) e0
  OpLitBool b -> constWord (CKBool b) e0
  OpSys sf -> case sf of
    SysRect -> emitUboMember 0 GVec4 e0
    SysUvRect -> emitUboMember 1 GVec4 e0
    SysMeta -> emitUboMember 2 GVec4 e0
    SysVColor -> let (t4, e1) = irTypeId TVec4 e0 in emitLoad t4 (seVColorVar e1) e1
    SysVUv -> let (t2, e1) = irTypeId TVec2 e0 in emitLoad t2 (seVUvVar e1) e1
  OpUniform idx gt -> emitUboMember (fromIntegral (3 + idx)) gt e0
  OpOuterRef depth nid -> (snd ((scFrames scope !! depth) V.! unNodeId nid), e0)
  OpLoopVar lid -> (scLoopVars scope Map.! lid, e0)
  OpLoopState lid slot -> (snd (scLoopStates scope Map.! (lid, slot)), e0)
  OpLoopResult loopNid slot -> (snd ((scLoopResults scope Map.! unNodeId loopNid) !! slot), e0)
  OpLoop _ -> (0, e0)
  OpConstruct outTy args -> emitConstruct vals outTy args e0
  OpSwizzle src cs -> emitSwizzle vals ty src cs e0
  OpUnary uo a -> emitUnary vals ty uo a e0
  OpBinary bo a b -> emitBinary vals ty bo a b e0
  OpTernary to a b c -> emitTernary vals ty to a b c e0
  OpLength a ->
    let (tf, e1) = irTypeId TFloat e0
     in emitExt tf glslLength [snd (vals IM.! unNodeId a)] e1
  OpNormalize a ->
    let (tyId, e1) = irTypeId ty e0
     in emitExt tyId glslNormalize [snd (vals IM.! unNodeId a)] e1
  OpSampleInput uvNid ->
    case seTexVar e0 of
      Just texVar ->
        let (_, uvId) = vals IM.! unNodeId uvNid
            (sampTy, e1) = getType TKSampledImage2D e0
            (tF, e2) = irTypeId TFloat e1
            (tV3, e3) = irTypeId TVec3 e2
            (tV4, e4) = irTypeId TVec4 e3
            (sampId, e5) = emitLoad sampTy texVar e4
            (rawId, e6) = emitRes opImageSampleImplicitLod tV4 [sampId, uvId] e5
            -- Offscreen targets hold premultiplied RGBA; un-premultiply to straight alpha.
            (aId, e7) = emitRes opCompositeExtract tF [rawId, 3] e6
            (epsId, e8) = constFloat 1.0e-5 e7
            (denomId, e9) = emitExt tF glslFMax [aId, epsId] e8
            (denom3Id, e10) = emitRes opCompositeConstruct tV3 [denomId, denomId, denomId] e9
            (rgbId, e11) = emitRes opVectorShuffle tV3 [rawId, rawId, 0, 1, 2] e10
            (divId, e12) = emitRes opFDiv tV3 [rgbId, denom3Id] e11
            (oneId, e13) = constFloat 1.0 e12
            (one3Id, e14) = emitRes opCompositeConstruct tV3 [oneId, oneId, oneId] e13
            (clampedId, e15) = emitExt tV3 glslFMin [divId, one3Id] e14
         in emitRes opCompositeConstruct tV4 [clampedId, aId] e15
      Nothing ->
        let (tV4, e1) = irTypeId TVec4 e0
            (z, e2) = constFloat 0 e1
         in emitRes opCompositeConstruct tV4 [z, z, z, z] e2

-- | Flatten vector arguments to scalar components and handle scalar splat.
emitConstruct :: IntMap (IRType, Word32) -> IRType -> [NodeId] -> SpvEnv -> (Word32, SpvEnv)
emitConstruct vals outTy args e0 =
  let resolved = [vals IM.! unNodeId a | a <- args]
      !outComps = irTypeComps outTy
      (tyId, e1) = irTypeId outTy e0
   in case resolved of
        [(TFloat, sId)] | outComps > 1 ->
          emitRes opCompositeConstruct tyId (replicate outComps sId) e1
        _ ->
          let (scalars, e2) = flattenScalars resolved e1
           in emitRes opCompositeConstruct tyId scalars e2
  where
    flattenScalars [] e = ([], e)
    flattenScalars ((t, vid) : rest) e
      | irTypeComps t == 1 =
          let (rs, e') = flattenScalars rest e
           in (vid : rs, e')
      | otherwise =
          let (tF, e1) = irTypeId TFloat e
              (cs, e2) = mapAccumEnv (\st j -> emitRes opCompositeExtract tF [vid, fromIntegral j] st) e1 [0 .. irTypeComps t - 1]
              (rs, e3) = flattenScalars rest e2
           in (cs ++ rs, e3)

emitSwizzle :: IntMap (IRType, Word32) -> IRType -> NodeId -> [Int] -> SpvEnv -> (Word32, SpvEnv)
emitSwizzle vals outTy src cs e0 =
  let (srcTy, srcId) = vals IM.! unNodeId src
      (tyId, e1) = irTypeId outTy e0
   in case cs of
        [c]
          | irTypeComps srcTy == 1 -> (srcId, e0)
          | otherwise -> emitRes opCompositeExtract tyId [srcId, fromIntegral c] e1
        _
          | irTypeComps srcTy == 1 -> emitRes opCompositeConstruct tyId (replicate (length cs) srcId) e1
          | otherwise -> emitRes opVectorShuffle tyId (srcId : srcId : map fromIntegral cs) e1

-- | Promote a scalar TFloat to match a target vector type via OpCompositeConstruct.
splatTo :: IRType -> (IRType, Word32) -> SpvEnv -> (Word32, SpvEnv)
splatTo targetTy (srcTy, srcId) e
  | srcTy == targetTy = (srcId, e)
  | srcTy == TFloat && irTypeComps targetTy > 1 =
      let (tyId, e1) = irTypeId targetTy e
       in emitRes opCompositeConstruct tyId (replicate (irTypeComps targetTy) srcId) e1
  | otherwise = (srcId, e)

-- | Select signed-int, unsigned-int, or float SPIR-V opcode for @opTy@.
numPick :: IRType -> Word32 -> Word32 -> Word32 -> Word32
numPick TInt s _ _ = s
numPick TUInt _ u _ = u
numPick _ _ _ f = f

emitUnary :: IntMap (IRType, Word32) -> IRType -> UnOp -> NodeId -> SpvEnv -> (Word32, SpvEnv)
emitUnary vals ty uo a e0 =
  let (aTy, aId) = vals IM.! unNodeId a
      (tyId, e1) = irTypeId ty e0
      ext extOp = emitExt tyId extOp [aId] e1
      core opcode = emitRes opcode tyId [aId] e1
   in case uo of
        UNeg -> core (numPick aTy opSNegate opSNegate opFNegate)
        UAbs -> ext (if aTy == TInt then glslSAbs else glslFAbs)
        USign -> ext (if aTy == TInt then glslSSign else glslFSign)
        UNot -> core opLogicalNot
        UFloatToInt -> core opConvertFToS
        UIntToFloat -> core opConvertSToF
        UFloor -> ext glslFloor
        UCeil -> ext glslCeil
        UFract -> ext glslFract
        USqrt -> ext glslSqrt
        USin -> ext glslSin
        UCos -> ext glslCos
        UTan -> ext glslTan
        UExp -> ext glslExp
        ULog -> ext glslLog
        ULog2 -> ext glslLog2

emitBinary :: IntMap (IRType, Word32) -> IRType -> BinOp -> NodeId -> NodeId -> SpvEnv -> (Word32, SpvEnv)
emitBinary vals ty bo a b e0 =
  let av@(aTy, aRaw) = vals IM.! unNodeId a
      bv@(bTy, bRaw) = vals IM.! unNodeId b
      opTy = if irTypeComps aTy >= irTypeComps bTy then aTy else bTy
      (aId, e1) = splatTo opTy av e0
      (bId, e2) = splatTo opTy bv e1
      (tyId, e3) = irTypeId ty e2
      core opcode = emitRes opcode tyId [aId, bId] e3
      ext extOp = emitExt tyId extOp [aId, bId] e3
      eqOp bOp iOp fOp = if opTy == TBool then bOp else numPick opTy iOp iOp fOp
   in case bo of
        BAdd -> core (numPick opTy opIAdd opIAdd opFAdd)
        BSub -> core (numPick opTy opISub opISub opFSub)
        BMul -> core (numPick opTy opIMul opIMul opFMul)
        BDiv -> core (numPick opTy opSDiv opUDiv opFDiv)
        BMod -> core opFMod
        BMin -> ext (numPick opTy glslSMin glslUMin glslFMin)
        BMax -> ext (numPick opTy glslSMax glslUMax glslFMax)
        BPow -> ext glslPow
        BAtan2 -> ext glslAtan2
        BStep -> ext glslStep
        BEq -> core (eqOp opLogicalEqual opIEqual opFOrdEqual)
        BNeq -> core (eqOp opLogicalNotEqual opINotEqual opFOrdNotEqual)
        BLt -> core (numPick opTy opSLessThan opULessThan opFOrdLessThan)
        BLe -> core (numPick opTy opSLessThanEqual opULessThanEqual opFOrdLessThanEqual)
        BGt -> core (numPick opTy opSGreaterThan opUGreaterThan opFOrdGreaterThan)
        BGe -> core (numPick opTy opSGreaterThanEqual opUGreaterThanEqual opFOrdGreaterThanEqual)
        BAnd -> core opLogicalAnd
        BOr -> core opLogicalOr
        BDot ->
          let (tF, eF) = irTypeId TFloat e0
           in emitRes opDot tF [aRaw, bRaw] eF
        BCross ->
          let (tOut, eOut) = irTypeId ty e0
           in emitExt tOut glslCross [aRaw, bRaw] eOut

emitTernary :: IntMap (IRType, Word32) -> IRType -> TriOp -> NodeId -> NodeId -> NodeId -> SpvEnv -> (Word32, SpvEnv)
emitTernary vals ty to a b c e0 =
  let av = vals IM.! unNodeId a
      bv = vals IM.! unNodeId b
      cv = vals IM.! unNodeId c
      (tyId, e1) = irTypeId ty e0
      splatExt extOp =
        let (aId, s1) = splatTo ty av e1
            (bId, s2) = splatTo ty bv s1
            (cId, s3) = splatTo ty cv s2
         in emitExt tyId extOp [aId, bId, cId] s3
   in case to of
        TMix -> splatExt glslFMix
        TClamp -> splatExt (numPick ty glslSClamp glslUClamp glslFClamp)
        TSmoothstep -> splatExt glslSmoothStep
        TSelect ->
          let (_, condRaw) = av
              (bId, e2) = splatTo ty bv e1
              (cId, e3) = splatTo ty cv e2
              -- SPIR-V 1.0 requires a boolean vector condition for vector OpSelect.
              (condId, e4)
                | irTypeComps ty > 1 =
                    let !n = irTypeComps ty
                        (bvTy, s1) = getType (TKBVec n) e3
                     in emitRes opCompositeConstruct bvTy (replicate n condRaw) s1
                | otherwise = (condRaw, e3)
           in emitRes opSelect tyId [condId, bId, cId] e4
