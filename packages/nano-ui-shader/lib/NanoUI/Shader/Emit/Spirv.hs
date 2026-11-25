{-# LANGUAGE StrictData #-}

-- | Pure-Haskell SPIR-V 1.0 binary fragment shader emitter for 'ShaderIR',
-- matching SDL 3.4's @SDL_GPURenderState@ Vulkan interface:
--
-- * Fragment inputs: @location 0@ (@vec4 v_color@), @location 1@ (@vec2 v_uv@)
-- * Fragment output: @location 0@ (@vec4 fragColor@)
-- * Sampled input texture (when 'irNeedsInput'): @DescriptorSet 2, Binding 0@
-- * Std140 uniform block: @DescriptorSet 3, Binding 0@
-- * Entry point: @"main"@ with @OriginUpperLeft@
module NanoUI.Shader.Emit.Spirv
  ( emitSpirv
  , emitSpirvWords
  ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text.Encoding qualified as TE
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32, Word8)
import GHC.Float (castFloatToWord32)
import NanoUI.Internal.Uniforms (GpuType (..), UniformField (..))
import NanoUI.Shader.IR

-- ---------------------------------------------------------------------------
-- SPIR-V 1.0 and GLSL.std.450 constants (Khronos specification)

spvMagic :: Word32
spvMagic = 0x07230203

spvVersion10 :: Word32
spvVersion10 = 0x00010000

-- Opcodes
opExtInstImport, opExtInst, opMemoryModel, opEntryPoint, opExecutionMode :: Word32
opExtInstImport = 11
opExtInst = 12
opMemoryModel = 14
opEntryPoint = 15
opExecutionMode = 16

opCapability :: Word32
opCapability = 17

opTypeVoid, opTypeBool, opTypeInt, opTypeFloat, opTypeVector :: Word32
opTypeVoid = 19
opTypeBool = 20
opTypeInt = 21
opTypeFloat = 22
opTypeVector = 23

opTypeImage, opTypeSampledImage, opTypeStruct, opTypePointer, opTypeFunction :: Word32
opTypeImage = 25
opTypeSampledImage = 27
opTypeStruct = 30
opTypePointer = 32
opTypeFunction = 33

opConstantTrue, opConstantFalse, opConstant :: Word32
opConstantTrue = 41
opConstantFalse = 42
opConstant = 43

opFunction, opFunctionEnd, opVariable, opLoad, opStore, opAccessChain :: Word32
opFunction = 54
opFunctionEnd = 56
opVariable = 59
opLoad = 61
opStore = 62
opAccessChain = 65

opDecorate, opMemberDecorate :: Word32
opDecorate = 71
opMemberDecorate = 72

opVectorShuffle, opCompositeConstruct, opCompositeExtract, opImageSampleImplicitLod :: Word32
opVectorShuffle = 79
opCompositeConstruct = 80
opCompositeExtract = 81
opImageSampleImplicitLod = 87

opConvertFToS, opConvertSToF :: Word32
opConvertFToS = 110
opConvertSToF = 111

opSNegate, opFNegate, opIAdd, opFAdd, opISub, opFSub, opIMul, opFMul, opSDiv, opFDiv, opFMod :: Word32
opSNegate = 126
opFNegate = 127
opIAdd = 128
opFAdd = 129
opISub = 130
opFSub = 131
opIMul = 132
opFMul = 133
opSDiv = 135
opFDiv = 136
opFMod = 141

opDot :: Word32
opDot = 148

opLogicalEqual, opLogicalNotEqual, opLogicalOr, opLogicalAnd, opLogicalNot, opSelect :: Word32
opLogicalEqual = 164
opLogicalNotEqual = 165
opLogicalOr = 166
opLogicalAnd = 167
opLogicalNot = 168
opSelect = 169

opIEqual, opINotEqual, opUGreaterThan, opSGreaterThan, opUGreaterThanEqual, opSGreaterThanEqual :: Word32
opIEqual = 170
opINotEqual = 171
opUGreaterThan = 172
opSGreaterThan = 173
opUGreaterThanEqual = 174
opSGreaterThanEqual = 175

opULessThan, opSLessThan, opULessThanEqual, opSLessThanEqual :: Word32
opULessThan = 176
opSLessThan = 177
opULessThanEqual = 178
opSLessThanEqual = 179

opFOrdEqual, opFOrdNotEqual, opFOrdLessThan, opFOrdGreaterThan, opFOrdLessThanEqual, opFOrdGreaterThanEqual :: Word32
opFOrdEqual = 180
opFOrdNotEqual = 182
opFOrdLessThan = 184
opFOrdGreaterThan = 186
opFOrdLessThanEqual = 188
opFOrdGreaterThanEqual = 190

opLoopMerge, opLabel, opBranch, opBranchConditional, opReturn :: Word32
opLoopMerge = 246
opLabel = 248
opBranch = 249
opBranchConditional = 250
opReturn = 253

-- Enums
capShader, execModelFragment, execModeOriginUpperLeft :: Word32
capShader = 1
execModelFragment = 4
execModeOriginUpperLeft = 7

decBlock, decLocation, decBinding, decDescriptorSet, decOffset :: Word32
decBlock = 2
decLocation = 30
decBinding = 33
decDescriptorSet = 34
decOffset = 35

scUniformConstant, scInput, scUniform, scOutput, scFunction :: Word32
scUniformConstant = 0
scInput = 1
scUniform = 2
scOutput = 3
scFunction = 7

-- GLSL.std.450 extended instructions
glslFAbs, glslSAbs, glslFSign, glslSSign, glslFloor, glslCeil, glslFract :: Word32
glslFAbs = 4
glslSAbs = 5
glslFSign = 6
glslSSign = 7
glslFloor = 8
glslCeil = 9
glslFract = 10

glslSin, glslCos, glslTan, glslAtan2, glslPow, glslExp, glslLog, glslLog2, glslSqrt :: Word32
glslSin = 13
glslCos = 14
glslTan = 15
glslAtan2 = 25
glslPow = 26
glslExp = 27
glslLog = 28
glslLog2 = 30
glslSqrt = 31

glslFMin, glslUMin, glslSMin, glslFMax, glslUMax, glslSMax :: Word32
glslFMin = 37
glslUMin = 38
glslSMin = 39
glslFMax = 40
glslUMax = 41
glslSMax = 42

glslFClamp, glslSClamp, glslFMix, glslStep, glslSmoothStep, glslLength, glslCross, glslNormalize :: Word32
glslFClamp = 43
glslSClamp = 45
glslFMix = 46
glslStep = 48
glslSmoothStep = 49
glslLength = 66
glslCross = 68
glslNormalize = 69

-- ---------------------------------------------------------------------------
-- Builder state

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
  , seTVoid :: !Word32
  , seTFnVoid :: !Word32
  , seTBool :: !Word32
  , seTInt :: !Word32
  , seTUInt :: !Word32
  , seTFloat :: !Word32
  , seTVec2 :: !Word32
  , seTVec3 :: !Word32
  , seTVec4 :: !Word32
  , seTBVec2 :: !Word32
  , seTBVec3 :: !Word32
  , seTBVec4 :: !Word32
  , seUboStruct :: !Word32
  , seVColorVar :: !Word32
  , seVUvVar :: !Word32
  , seFragOutVar :: !Word32
  , seUboVar :: !Word32
  , seTexVar :: !(Maybe Word32)
  , seTSampledImg :: !(Maybe Word32)
  , sePtrUniform :: !(Map GpuType Word32)
  , sePtrFunc :: !(Map IRType Word32)
  , seConsts :: !(Map ConstKey Word32)
  , seConstInsts :: ![[Word32]]
  -- ^ Reversed list of OpConstant* instructions emitted in section 9.
  , seFuncVars :: ![[Word32]]
  -- ^ Reversed list of function-scope OpVariable instructions emitted at entry.
  , seBody :: ![[Word32]]
  -- ^ Reversed list of instructions in main's body.
  }

-- | Emit a complete SPIR-V 1.0 binary fragment shader as little-endian bytes.
emitSpirv :: ShaderIR -> ByteString
emitSpirv ir =
  BSL.toStrict . BSB.toLazyByteString . foldMap BSB.word32LE $ emitSpirvWords ir

-- | Emit a complete SPIR-V 1.0 binary fragment shader as 32-bit words.
emitSpirvWords :: ShaderIR -> [Word32]
emitSpirvWords ir =
  let (env0, typeDeclWords, decWords, globalWords) = initModule ir
      (outId, env1) = emitMain ir env0
      bound = seNextId env1
      header = [spvMagic, spvVersion10, 0, bound, 0]
      caps = [inst opCapability [capShader]]
      extImports = [inst opExtInstImport (seExtGlsl env1 : encodeStr "GLSL.std.450")]
      memModel = [inst opMemoryModel [0, 1]]
      entryPts =
        [ inst opEntryPoint $
            [execModelFragment, seMainId env1]
              ++ encodeStr "main"
              ++ [seVColorVar env1, seVUvVar env1, seFragOutVar env1]
        ]
      execModes = [inst opExecutionMode [seMainId env1, execModeOriginUpperLeft]]
      consts = reverse (seConstInsts env1)
      fnStart =
        [ inst opFunction [seTVoid env1, seMainId env1, 0, seTFnVoid env1]
        , inst opLabel [seEntryLabel env1]
        ]
      fnVars = reverse (seFuncVars env1)
      fnBody = reverse (seBody env1)
      fnEnd =
        [ inst opStore [seFragOutVar env1, outId]
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
          ++ typeDeclWords
          ++ consts
          ++ globalWords
          ++ fnStart
          ++ fnVars
          ++ fnBody
          ++ fnEnd

inst :: Word32 -> [Word32] -> [Word32]
inst opcode operands =
  let !wc = fromIntegral (1 + length operands) :: Word32
   in ((wc `shiftL` 16) .|. opcode) : operands

encodeStr :: String -> [Word32]
encodeStr s =
  let bs = TE.encodeUtf8 (T.pack s)
      bytes = BSL.unpack (BSL.fromStrict bs) ++ [0 :: Word8]
      padLen = (4 - (length bytes `rem` 4)) `rem` 4
      padded = bytes ++ replicate padLen 0
      pack4 (a : b : c : d : rest) =
        let !w =
              fromIntegral a
                .|. (fromIntegral b `shiftL` 8)
                .|. (fromIntegral c `shiftL` 16)
                .|. (fromIntegral d `shiftL` 24)
         in w : pack4 rest
      pack4 _ = []
   in pack4 padded

allocId :: SpvEnv -> (Word32, SpvEnv)
allocId e =
  let !i = seNextId e
   in (i, e {seNextId = i + 1})

emitBody :: [Word32] -> SpvEnv -> SpvEnv
emitBody w e = e {seBody = w : seBody e}

emitFuncVar :: Word32 -> SpvEnv -> (Word32, SpvEnv)
emitFuncVar ptrTy e =
  let (vid, e1) = allocId e
      w = inst opVariable [ptrTy, vid, scFunction]
   in (vid, e1 {seFuncVars = w : seFuncVars e1})

constWord :: ConstKey -> SpvEnv -> (Word32, SpvEnv)
constWord key e =
  case Map.lookup key (seConsts e) of
    Just cid -> (cid, e)
    Nothing ->
      let (cid, e1) = allocId e
          w = case key of
            CKFloat bits -> inst opConstant [seTFloat e1, cid, bits]
            CKInt n -> inst opConstant [seTInt e1, cid, fromIntegral n]
            CKUInt u -> inst opConstant [seTUInt e1, cid, u]
            CKBool True -> inst opConstantTrue [seTBool e1, cid]
            CKBool False -> inst opConstantFalse [seTBool e1, cid]
       in (cid, e1 {seConsts = Map.insert key cid (seConsts e1), seConstInsts = w : seConstInsts e1})

constInt :: Int32 -> SpvEnv -> (Word32, SpvEnv)
constInt = constWord . CKInt

constFloat :: Float -> SpvEnv -> (Word32, SpvEnv)
constFloat = constWord . CKFloat . castFloatToWord32

irTypeId :: SpvEnv -> IRType -> Word32
irTypeId e ty = case ty of
  TFloat -> seTFloat e
  TInt -> seTInt e
  TUInt -> seTUInt e
  TBool -> seTBool e
  TVec2 -> seTVec2 e
  TVec3 -> seTVec3 e
  TVec4 -> seTVec4 e

uboFieldTypeId :: SpvEnv -> GpuType -> Word32
uboFieldTypeId e gt = case gt of
  GFloat -> seTFloat e
  GInt32 -> seTInt e
  GUInt32 -> seTUInt e
  GBool -> seTUInt e
  GVec2 -> seTVec2 e
  GVec3 -> seTVec3 e
  GVec4 -> seTVec4 e

-- ---------------------------------------------------------------------------
-- Module type, decoration, and global setup

initModule :: ShaderIR -> (SpvEnv, [[Word32]], [[Word32]], [[Word32]])
initModule ir =
  let -- Fixed deterministic ID allocation for core types and globals.
      extGlsl = 1
      mainId = 2
      entryLabel = 3
      tVoid = 4
      tFnVoid = 5
      tBool = 6
      tInt = 7
      tUInt = 8
      tFloat = 9
      tVec2 = 10
      tVec3 = 11
      tVec4 = 12
      tBVec2 = 13
      tBVec3 = 14
      tBVec4 = 15
      pInVec4 = 16
      pInVec2 = 17
      pOutVec4 = 18
      uboStruct = 19
      pUniformStruct = 20
      pUniformFloat = 21
      pUniformInt = 22
      pUniformUInt = 23
      pUniformVec2 = 24
      pUniformVec3 = 25
      pUniformVec4 = 26
      pFuncFloat = 27
      pFuncInt = 28
      pFuncUInt = 29
      pFuncBool = 30
      pFuncVec2 = 31
      pFuncVec3 = 32
      pFuncVec4 = 33
      vColorVar = 34
      vUvVar = 35
      fragOutVar = 36
      uboVar = 37
      (next0, texTypes, texDecs, texGlobals, mTexVar, mSampledTy) =
        if irNeedsInput ir
          then
            let tImg = 38
                tSamp = 39
                pSamp = 40
                tVar = 41
             in ( 42
                , [ inst opTypeImage [tImg, tFloat, 1, 0, 0, 0, 1, 0]
                  , inst opTypeSampledImage [tSamp, tImg]
                  , inst opTypePointer [pSamp, scUniformConstant, tSamp]
                  ]
                , [ inst opDecorate [tVar, decDescriptorSet, 2]
                  , inst opDecorate [tVar, decBinding, 0]
                  ]
                , [inst opVariable [pSamp, tVar, scUniformConstant]]
                , Just tVar
                , Just tSamp
                )
          else (38, [], [], [], Nothing, Nothing)

      userTypes = map (uboFieldTypeId envBase . ufType) (irUniforms ir)
      uboMembers = [tVec4, tVec4, tVec4] ++ userTypes

      typeDecls =
        [ inst opTypeVoid [tVoid]
        , inst opTypeFunction [tFnVoid, tVoid]
        , inst opTypeBool [tBool]
        , inst opTypeInt [tInt, 32, 1]
        , inst opTypeInt [tUInt, 32, 0]
        , inst opTypeFloat [tFloat, 32]
        , inst opTypeVector [tVec2, tFloat, 2]
        , inst opTypeVector [tVec3, tFloat, 3]
        , inst opTypeVector [tVec4, tFloat, 4]
        , inst opTypeVector [tBVec2, tBool, 2]
        , inst opTypeVector [tBVec3, tBool, 3]
        , inst opTypeVector [tBVec4, tBool, 4]
        , inst opTypePointer [pInVec4, scInput, tVec4]
        , inst opTypePointer [pInVec2, scInput, tVec2]
        , inst opTypePointer [pOutVec4, scOutput, tVec4]
        , inst opTypeStruct (uboStruct : uboMembers)
        , inst opTypePointer [pUniformStruct, scUniform, uboStruct]
        , inst opTypePointer [pUniformFloat, scUniform, tFloat]
        , inst opTypePointer [pUniformInt, scUniform, tInt]
        , inst opTypePointer [pUniformUInt, scUniform, tUInt]
        , inst opTypePointer [pUniformVec2, scUniform, tVec2]
        , inst opTypePointer [pUniformVec3, scUniform, tVec3]
        , inst opTypePointer [pUniformVec4, scUniform, tVec4]
        , inst opTypePointer [pFuncFloat, scFunction, tFloat]
        , inst opTypePointer [pFuncInt, scFunction, tInt]
        , inst opTypePointer [pFuncUInt, scFunction, tUInt]
        , inst opTypePointer [pFuncBool, scFunction, tBool]
        , inst opTypePointer [pFuncVec2, scFunction, tVec2]
        , inst opTypePointer [pFuncVec3, scFunction, tVec3]
        , inst opTypePointer [pFuncVec4, scFunction, tVec4]
        ]
          ++ texTypes

      memberOffsetDecs =
        [ inst opMemberDecorate [uboStruct, 0, decOffset, 0]
        , inst opMemberDecorate [uboStruct, 1, decOffset, 16]
        , inst opMemberDecorate [uboStruct, 2, decOffset, 32]
        ]
          ++ [ inst opMemberDecorate [uboStruct, fromIntegral (3 + i), decOffset, fromIntegral (48 + ufOffset uf)]
             | (i, uf) <- zip [0 :: Int ..] (irUniforms ir)
             ]

      decs =
        [ inst opDecorate [vColorVar, decLocation, 0]
        , inst opDecorate [vUvVar, decLocation, 1]
        , inst opDecorate [fragOutVar, decLocation, 0]
        , inst opDecorate [uboStruct, decBlock]
        ]
          ++ memberOffsetDecs
          ++ [ inst opDecorate [uboVar, decDescriptorSet, 3]
             , inst opDecorate [uboVar, decBinding, 0]
             ]
          ++ texDecs

      globals =
        [ inst opVariable [pInVec4, vColorVar, scInput]
        , inst opVariable [pInVec2, vUvVar, scInput]
        , inst opVariable [pOutVec4, fragOutVar, scOutput]
        , inst opVariable [pUniformStruct, uboVar, scUniform]
        ]
          ++ texGlobals

      envBase =
        SpvEnv
          { seNextId = next0
          , seExtGlsl = extGlsl
          , seMainId = mainId
          , seEntryLabel = entryLabel
          , seTVoid = tVoid
          , seTFnVoid = tFnVoid
          , seTBool = tBool
          , seTInt = tInt
          , seTUInt = tUInt
          , seTFloat = tFloat
          , seTVec2 = tVec2
          , seTVec3 = tVec3
          , seTVec4 = tVec4
          , seTBVec2 = tBVec2
          , seTBVec3 = tBVec3
          , seTBVec4 = tBVec4
          , seUboStruct = uboStruct
          , seVColorVar = vColorVar
          , seVUvVar = vUvVar
          , seFragOutVar = fragOutVar
          , seUboVar = uboVar
          , seTexVar = mTexVar
          , seTSampledImg = mSampledTy
          , sePtrUniform =
              Map.fromList
                [ (GFloat, pUniformFloat)
                , (GInt32, pUniformInt)
                , (GUInt32, pUniformUInt)
                , (GBool, pUniformUInt)
                , (GVec2, pUniformVec2)
                , (GVec3, pUniformVec3)
                , (GVec4, pUniformVec4)
                ]
          , sePtrFunc =
              Map.fromList
                [ (TFloat, pFuncFloat)
                , (TInt, pFuncInt)
                , (TUInt, pFuncUInt)
                , (TBool, pFuncBool)
                , (TVec2, pFuncVec2)
                , (TVec3, pFuncVec3)
                , (TVec4, pFuncVec4)
                ]
          , seConsts = Map.empty
          , seConstInsts = []
          , seFuncVars = []
          , seBody = []
          }
   in (envBase, typeDecls, decs, globals)

-- ---------------------------------------------------------------------------
-- SSA block emission

data ScopeCtx = ScopeCtx
  { scFrames :: ![Vector (IRType, Word32)]
  -- ^ Outer scope frames from depth 0 upwards.
  , scLoopVars :: !(Map Int Word32)
  -- ^ Current induction variable SSA ID per active loop ID.
  , scLoopStates :: !(Map (Int, Int) (IRType, Word32))
  -- ^ Current iteration accumulator SSA ID per (loopId, slot).
  , scLoopResults :: !(Map Int [(IRType, Word32)])
  -- ^ Final accumulator SSA IDs keyed by OpLoop's NodeId index in current frame.
  }

emitMain :: ShaderIR -> SpvEnv -> (Word32, SpvEnv)
emitMain ir env0 =
  let emptyScope = ScopeCtx [] Map.empty Map.empty Map.empty
      (vals, env1) = emitBlock emptyScope (irNodes ir) env0
      (_, outId) = vals V.! unNodeId (irOutput ir)
   in (outId, env1)

emitBlock :: ScopeCtx -> Vector (IRType, ExprOp) -> SpvEnv -> (Vector (IRType, Word32), SpvEnv)
emitBlock scope nodes env0 = go 0 [] Map.empty env0
  where
    !n = V.length nodes
    go !i !acc !loopRes !e
      | i >= n = (V.fromList (reverse acc), e)
      | otherwise =
          let (ty, op) = nodes V.! i
              curVals = V.fromList (reverse acc)
              scope' = scope {scLoopResults = loopRes}
           in case op of
                OpLoop lb ->
                  let (resList, e1) = emitLoopSpv scope' curVals lb e
                      firstVal = case resList of
                        ((_, v) : _) -> v
                        [] -> 0
                   in go (i + 1) ((ty, firstVal) : acc) (Map.insert i resList loopRes) e1
                _ ->
                  let (vid, e1) = emitOpSpv scope' curVals ty op e
                   in go (i + 1) ((ty, vid) : acc) loopRes e1

emitLoopSpv ::
  ScopeCtx ->
  Vector (IRType, Word32) ->
  LoopBody ->
  SpvEnv ->
  ([(IRType, Word32)], SpvEnv)
emitLoopSpv scope curVals lb e0 =
  let lid = lbLoopId lb
      -- Allocate function-scope variables for the loop counter and accumulators.
      (iVar, e1) = emitFuncVar (sePtrFunc e0 Map.! TInt) e0
      (accVars, e2) = allocAccVars (lbStateTypes lb) e1
      (startConst, e3) = constInt (lbStart lb) e2
      (endConst, e4) = constInt (lbEnd lb) e3
      (oneConst, e5) = constInt 1 e4
      -- Initialize counter and accumulators before branching to loop header.
      e6 = emitBody (inst opStore [iVar, startConst]) e5
      e7 =
        foldl'
          (\e (vPtr, initNid) -> emitBody (inst opStore [vPtr, snd (curVals V.! unNodeId initNid)]) e)
          e6
          (zip accVars (lbInits lb))
      (headerLbl, e8) = allocId e7
      (bodyLbl, e9) = allocId e8
      (contLbl, e10) = allocId e9
      (mergeLbl, e11) = allocId e10
      -- Branch to header.
      e12 = emitBody (inst opBranch [headerLbl]) e11
      -- Header: load i, check i < end, OpLoopMerge, conditional branch.
      e13 = emitBody (inst opLabel [headerLbl]) e12
      (iVal, e14) = emitLoad (seTInt e13) iVar e13
      (condId, e15) = allocId e14
      e16 =
        emitBody (inst opBranchConditional [condId, bodyLbl, mergeLbl])
          . emitBody (inst opLoopMerge [mergeLbl, contLbl, 0])
          . emitBody (inst opSLessThan [seTBool e15, condId, iVal, endConst])
          $ e15
      -- Body: load accumulators, emit body nodes, store updated accumulators.
      e17 = emitBody (inst opLabel [bodyLbl]) e16
      (loadedStates, e18) = loadAccVars (zip (lbStateTypes lb) accVars) e17
      innerScope =
        ScopeCtx
          { scFrames = scFrames scope ++ [curVals]
          , scLoopVars = Map.insert lid iVal (scLoopVars scope)
          , scLoopStates =
              foldl'
                (\m (slot, st) -> Map.insert (lid, slot) st m)
                (scLoopStates scope)
                (zip [0 :: Int ..] loadedStates)
          , scLoopResults = Map.empty
          }
      (bodyVals, e19) = emitBlock innerScope (lbBodyNodes lb) e18
      e20 =
        foldl'
          (\e (vPtr, nextNid) -> emitBody (inst opStore [vPtr, snd (bodyVals V.! unNodeId nextNid)]) e)
          e19
          (zip accVars (lbNextStates lb))
      e21 = emitBody (inst opBranch [contLbl]) e20
      -- Continue block: increment i and branch back to header.
      e22 = emitBody (inst opLabel [contLbl]) e21
      (iCur, e23) = emitLoad (seTInt e22) iVar e22
      (iNext, e24) = allocId e23
      e25 =
        emitBody (inst opBranch [headerLbl])
          . emitBody (inst opStore [iVar, iNext])
          . emitBody (inst opIAdd [seTInt e24, iNext, iCur, oneConst])
          $ e24
      -- Merge block: load final accumulator values.
      e26 = emitBody (inst opLabel [mergeLbl]) e25
      (finalStates, e27) = loadAccVars (zip (lbStateTypes lb) accVars) e26
   in (finalStates, e27)
  where
    allocAccVars [] e = ([], e)
    allocAccVars (t : ts) e =
      let (v, e') = emitFuncVar (sePtrFunc e Map.! t) e
          (vs, e'') = allocAccVars ts e'
       in (v : vs, e'')

    loadAccVars [] e = ([], e)
    loadAccVars ((t, vPtr) : rest) e =
      let (val, e') = emitLoad (irTypeId e t) vPtr e
          (vals, e'') = loadAccVars rest e'
       in ((t, val) : vals, e'')

emitLoad :: Word32 -> Word32 -> SpvEnv -> (Word32, SpvEnv)
emitLoad tyId ptrId e =
  let (resId, e1) = allocId e
   in (resId, emitBody (inst opLoad [tyId, resId, ptrId]) e1)

emitUboMember :: Int32 -> GpuType -> SpvEnv -> (Word32, SpvEnv)
emitUboMember memberIdx gt e0 =
  let (idxConst, e1) = constInt memberIdx e0
      ptrTy = sePtrUniform e1 Map.! gt
      (ptrId, e2) = allocId e1
      e3 = emitBody (inst opAccessChain [ptrTy, ptrId, seUboVar e2, idxConst]) e2
      valTy = uboFieldTypeId e3 gt
      (rawId, e4) = emitLoad valTy ptrId e3
   in case gt of
        GBool ->
          let (zeroU, e5) = constWord (CKUInt 0) e4
              (bId, e6) = allocId e5
           in (bId, emitBody (inst opINotEqual [seTBool e6, bId, rawId, zeroU]) e6)
        _ -> (rawId, e4)

-- ---------------------------------------------------------------------------
-- Individual instruction lowering

emitOpSpv :: ScopeCtx -> Vector (IRType, Word32) -> IRType -> ExprOp -> SpvEnv -> (Word32, SpvEnv)
emitOpSpv scope vals ty op e0 = case op of
  OpLitFloat bits -> constWord (CKFloat bits) e0
  OpLitInt n -> constWord (CKInt n) e0
  OpLitUInt u -> constWord (CKUInt u) e0
  OpLitBool b -> constWord (CKBool b) e0
  OpSys sf -> case sf of
    SysRect -> emitUboMember 0 GVec4 e0
    SysUvRect -> emitUboMember 1 GVec4 e0
    SysMeta -> emitUboMember 2 GVec4 e0
    SysVColor -> emitLoad (seTVec4 e0) (seVColorVar e0) e0
    SysVUv -> emitLoad (seTVec2 e0) (seVUvVar e0) e0
  OpUniform idx gt -> emitUboMember (fromIntegral (3 + idx)) gt e0
  OpOuterRef depth nid ->
    let frame = scFrames scope !! depth
     in (snd (frame V.! unNodeId nid), e0)
  OpLoopVar lid -> (scLoopVars scope Map.! lid, e0)
  OpLoopState lid slot -> (snd (scLoopStates scope Map.! (lid, slot)), e0)
  OpLoopResult loopNid slot ->
    let resList = scLoopResults scope Map.! unNodeId loopNid
     in (snd (resList !! slot), e0)
  OpLoop _ -> (0, e0)
  OpConstruct outTy args -> emitConstruct vals outTy args e0
  OpSwizzle src cs -> emitSwizzle vals ty src cs e0
  OpUnary uo a -> emitUnary vals ty uo a e0
  OpBinary bo a b -> emitBinary vals ty bo a b e0
  OpTernary to a b c -> emitTernary vals ty to a b c e0
  OpLength a ->
    let (_, aId) = vals V.! unNodeId a
     in emitGlslExt (seTFloat e0) glslLength [aId] e0
  OpNormalize a ->
    let (_, aId) = vals V.! unNodeId a
     in emitGlslExt (irTypeId e0 ty) glslNormalize [aId] e0
  OpSampleInput uvNid ->
    case (seTexVar e0, seTSampledImg e0) of
      (Just texVar, Just sampTy) ->
        let (_, uvId) = vals V.! unNodeId uvNid
            (sampId, e1) = emitLoad sampTy texVar e0
            (resId, e2) = allocId e1
         in (resId, emitBody (inst opImageSampleImplicitLod [seTVec4 e2, resId, sampId, uvId]) e2)
      _ ->
        let (z, e1) = constFloat 0 e0
            (resId, e2) = allocId e1
         in (resId, emitBody (inst opCompositeConstruct [seTVec4 e2, resId, z, z, z, z]) e2)

emitGlslExt :: Word32 -> Word32 -> [Word32] -> SpvEnv -> (Word32, SpvEnv)
emitGlslExt resTy extOp args e0 =
  let (resId, e1) = allocId e0
   in (resId, emitBody (inst opExtInst ([resTy, resId, seExtGlsl e1, extOp] ++ args)) e1)

-- | Flatten vector arguments to scalar components and handle scalar splat.
emitConstruct :: Vector (IRType, Word32) -> IRType -> [NodeId] -> SpvEnv -> (Word32, SpvEnv)
emitConstruct vals outTy args e0 =
  let resolved = [vals V.! unNodeId a | a <- args]
      !outComps = irTypeComps outTy
   in case resolved of
        [(TFloat, sId)] | outComps > 1 ->
          let (resId, e1) = allocId e0
           in (resId, emitBody (inst opCompositeConstruct (irTypeId e1 outTy : resId : replicate outComps sId)) e1)
        _ ->
          let (scalars, e1) = flattenScalars resolved e0
              (resId, e2) = allocId e1
           in (resId, emitBody (inst opCompositeConstruct (irTypeId e2 outTy : resId : scalars)) e2)
  where
    flattenScalars [] e = ([], e)
    flattenScalars ((t, vid) : rest) e
      | irTypeComps t == 1 =
          let (rs, e') = flattenScalars rest e
           in (vid : rs, e')
      | otherwise =
          let (cs, e1) = extractAll vid (irTypeComps t) e
              (rs, e2) = flattenScalars rest e1
           in (cs ++ rs, e2)

    extractAll vid k e = go 0 e
      where
        go !j !st
          | j >= k = ([], st)
          | otherwise =
              let (cid, st1) = allocId st
                  st2 = emitBody (inst opCompositeExtract [seTFloat st1, cid, vid, fromIntegral j]) st1
                  (more, st3) = go (j + 1) st2
               in (cid : more, st3)

emitSwizzle :: Vector (IRType, Word32) -> IRType -> NodeId -> [Int] -> SpvEnv -> (Word32, SpvEnv)
emitSwizzle vals outTy src cs e0 =
  let (srcTy, srcId) = vals V.! unNodeId src
   in case cs of
        [c]
          | irTypeComps srcTy == 1 -> (srcId, e0)
          | otherwise ->
              let (resId, e1) = allocId e0
               in (resId, emitBody (inst opCompositeExtract [irTypeId e1 outTy, resId, srcId, fromIntegral c]) e1)
        _
          | irTypeComps srcTy == 1 ->
              let (resId, e1) = allocId e0
               in (resId, emitBody (inst opCompositeConstruct (irTypeId e1 outTy : resId : replicate (length cs) srcId)) e1)
          | otherwise ->
              let (resId, e1) = allocId e0
               in (resId, emitBody (inst opVectorShuffle ([irTypeId e1 outTy, resId, srcId, srcId] ++ map fromIntegral cs)) e1)

-- | Promote a scalar TFloat to match a target vector type via OpCompositeConstruct.
splatTo :: IRType -> (IRType, Word32) -> SpvEnv -> (Word32, SpvEnv)
splatTo targetTy (srcTy, srcId) e
  | srcTy == targetTy = (srcId, e)
  | srcTy == TFloat && irTypeComps targetTy > 1 =
      let (resId, e1) = allocId e
       in (resId, emitBody (inst opCompositeConstruct (irTypeId e1 targetTy : resId : replicate (irTypeComps targetTy) srcId)) e1)
  | otherwise = (srcId, e)

-- | Common promoted type for a binary operand pair.
commonVecTy :: IRType -> IRType -> IRType
commonVecTy a b
  | irTypeComps a >= irTypeComps b = a
  | otherwise = b

emitUnary :: Vector (IRType, Word32) -> IRType -> UnOp -> NodeId -> SpvEnv -> (Word32, SpvEnv)
emitUnary vals ty uo a e0 =
  let (aTy, aId) = vals V.! unNodeId a
      tyId = irTypeId e0 ty
      emit1 opcode =
        let (resId, e1) = allocId e0
         in (resId, emitBody (inst opcode [tyId, resId, aId]) e1)
   in case uo of
        UNeg -> emit1 (if aTy == TInt then opSNegate else opFNegate)
        UAbs -> emitGlslExt tyId (if aTy == TInt then glslSAbs else glslFAbs) [aId] e0
        UFloor -> emitGlslExt tyId glslFloor [aId] e0
        UCeil -> emitGlslExt tyId glslCeil [aId] e0
        UFract -> emitGlslExt tyId glslFract [aId] e0
        USqrt -> emitGlslExt tyId glslSqrt [aId] e0
        USin -> emitGlslExt tyId glslSin [aId] e0
        UCos -> emitGlslExt tyId glslCos [aId] e0
        UTan -> emitGlslExt tyId glslTan [aId] e0
        UExp -> emitGlslExt tyId glslExp [aId] e0
        ULog -> emitGlslExt tyId glslLog [aId] e0
        ULog2 -> emitGlslExt tyId glslLog2 [aId] e0
        USign -> emitGlslExt tyId (if aTy == TInt then glslSSign else glslFSign) [aId] e0
        UNot -> emit1 opLogicalNot
        UFloatToInt -> emit1 opConvertFToS
        UIntToFloat -> emit1 opConvertSToF

emitBinary :: Vector (IRType, Word32) -> IRType -> BinOp -> NodeId -> NodeId -> SpvEnv -> (Word32, SpvEnv)
emitBinary vals ty bo a b e0 =
  let av@(aTy, aRaw) = vals V.! unNodeId a
      bv@(bTy, bRaw) = vals V.! unNodeId b
      opTy = commonVecTy aTy bTy
      (aId, e1) = splatTo opTy av e0
      (bId, e2) = splatTo opTy bv e1
      tyId = irTypeId e2 ty
      emit2 opcode =
        let (resId, e3) = allocId e2
         in (resId, emitBody (inst opcode [tyId, resId, aId, bId]) e3)
      isInt = opTy == TInt
      isUInt = opTy == TUInt
      isBool = opTy == TBool
   in case bo of
        BAdd -> emit2 (if isInt || isUInt then opIAdd else opFAdd)
        BSub -> emit2 (if isInt || isUInt then opISub else opFSub)
        BMul -> emit2 (if isInt || isUInt then opIMul else opFMul)
        BDiv -> emit2 (if isInt then opSDiv else opFDiv)
        BMod -> emit2 opFMod
        BMin -> emitGlslExt tyId (if isInt then glslSMin else if isUInt then glslUMin else glslFMin) [aId, bId] e2
        BMax -> emitGlslExt tyId (if isInt then glslSMax else if isUInt then glslUMax else glslFMax) [aId, bId] e2
        BPow -> emitGlslExt tyId glslPow [aId, bId] e2
        BAtan2 -> emitGlslExt tyId glslAtan2 [aId, bId] e2
        BStep -> emitGlslExt tyId glslStep [aId, bId] e2
        BEq -> emit2 (if isBool then opLogicalEqual else if isInt || isUInt then opIEqual else opFOrdEqual)
        BNeq -> emit2 (if isBool then opLogicalNotEqual else if isInt || isUInt then opINotEqual else opFOrdNotEqual)
        BLt -> emit2 (if isInt then opSLessThan else if isUInt then opULessThan else opFOrdLessThan)
        BLe -> emit2 (if isInt then opSLessThanEqual else if isUInt then opULessThanEqual else opFOrdLessThanEqual)
        BGt -> emit2 (if isInt then opSGreaterThan else if isUInt then opUGreaterThan else opFOrdGreaterThan)
        BGe -> emit2 (if isInt then opSGreaterThanEqual else if isUInt then opUGreaterThanEqual else opFOrdGreaterThanEqual)
        BAnd -> emit2 opLogicalAnd
        BOr -> emit2 opLogicalOr
        BDot ->
          let (resId, e3) = allocId e0
           in (resId, emitBody (inst opDot [seTFloat e3, resId, aRaw, bRaw]) e3)
        BCross -> emitGlslExt tyId glslCross [aRaw, bRaw] e0

emitTernary :: Vector (IRType, Word32) -> IRType -> TriOp -> NodeId -> NodeId -> NodeId -> SpvEnv -> (Word32, SpvEnv)
emitTernary vals ty to a b c e0 =
  let av = vals V.! unNodeId a
      bv = vals V.! unNodeId b
      cv = vals V.! unNodeId c
      tyId = irTypeId e0 ty
   in case to of
        TMix ->
          let (aId, e1) = splatTo ty av e0
              (bId, e2) = splatTo ty bv e1
              (cId, e3) = splatTo ty cv e2
           in emitGlslExt tyId glslFMix [aId, bId, cId] e3
        TClamp ->
          let (aId, e1) = splatTo ty av e0
              (bId, e2) = splatTo ty bv e1
              (cId, e3) = splatTo ty cv e2
              extOp = if ty == TInt then glslSClamp else glslFClamp
           in emitGlslExt tyId extOp [aId, bId, cId] e3
        TSmoothstep ->
          let (aId, e1) = splatTo ty av e0
              (bId, e2) = splatTo ty bv e1
              (cId, e3) = splatTo ty cv e2
           in emitGlslExt tyId glslSmoothStep [aId, bId, cId] e3
        TSelect ->
          let (_, condRaw) = av
              (bId, e1) = splatTo ty bv e0
              (cId, e2) = splatTo ty cv e1
              -- SPIR-V 1.0 requires a boolean vector condition for vector OpSelect.
              (condId, e3) = case ty of
                TVec2 -> splatBool (seTBVec2 e2) 2 condRaw e2
                TVec3 -> splatBool (seTBVec3 e2) 3 condRaw e2
                TVec4 -> splatBool (seTBVec4 e2) 4 condRaw e2
                _ -> (condRaw, e2)
              (resId, e4) = allocId e3
           in (resId, emitBody (inst opSelect [tyId, resId, condId, bId, cId]) e4)
  where
    splatBool bvTy n condRaw e =
      let (vid, e1) = allocId e
       in (vid, emitBody (inst opCompositeConstruct (bvTy : vid : replicate n condRaw)) e1)
