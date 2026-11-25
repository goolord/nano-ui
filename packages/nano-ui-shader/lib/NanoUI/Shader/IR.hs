{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

-- | Typed SSA intermediate representation for fragment shaders, with pure
-- structural hash-consing (global value numbering / CSE).
--
-- Every backend emitter (GLSL 150, GLSL ES 300, SPIR-V, MSL) and the CPU
-- reference evaluator consume 'ShaderIR' directly.
module NanoUI.Shader.IR
  ( NodeId (..)
  , IRType (..)
  , gpuToIRType
  , irTypeComps
  , SysField (..)
  , UnOp (..)
  , BinOp (..)
  , TriOp (..)
  , LoopBody (..)
  , ExprOp (..)
  , ShaderIR (..)
  , IRBuilder
  , emptyIRBuilder
  , runIRBuilder
  , emitNode
  , emitLoop
  , hashShaderIR
  ) where

import Data.Bits (shiftL, xor, (.|.))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable (..))
import Data.Int (Int32)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import NanoUI.Internal.Uniforms (GpuType (..), UniformField (..))

-- | Index into 'irNodes' (0-based, strictly topologically ordered).
newtype NodeId = NodeId {unNodeId :: Int}
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | Scalar and vector value types supported in 'ShaderIR'.
data IRType
  = TFloat
  | TInt
  | TUInt
  | TBool
  | TVec2
  | TVec3
  | TVec4
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

-- | Map a uniform's 'GpuType' to its 'IRType'.
gpuToIRType :: GpuType -> IRType
gpuToIRType gt = case gt of
  GFloat -> TFloat
  GInt32 -> TInt
  GUInt32 -> TUInt
  GBool -> TBool
  GVec2 -> TVec2
  GVec3 -> TVec3
  GVec4 -> TVec4

-- | Component count of an 'IRType' (1 for scalars, 2..4 for vectors).
irTypeComps :: IRType -> Int
irTypeComps t = case t of
  TFloat -> 1
  TInt -> 1
  TUInt -> 1
  TBool -> 1
  TVec2 -> 2
  TVec3 -> 3
  TVec4 -> 4

-- | Built-in system inputs from the 48-byte uniform header and vertex stage.
data SysField
  = SysRect
  -- ^ @uRect@ (@vec4@): node @(x, y, w, h)@ in logical pixels.
  | SysUvRect
  -- ^ @uUvRect@ (@vec4@): node @(u0, v0, uSpan, vSpan)@ in source target UVs.
  | SysMeta
  -- ^ @uMeta@ (@vec4@): @(dpiScale, cornerRadius, targetW, targetH)@.
  | SysVColor
  -- ^ Interpolated vertex color (@vec4@).
  | SysVUv
  -- ^ Interpolated vertex texture coordinate (@vec2@).
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

-- | Unary scalar or componentwise vector operations.
data UnOp
  = UNeg
  | UAbs
  | UFloor
  | UCeil
  | UFract
  | USqrt
  | USin
  | UCos
  | UTan
  | UExp
  | ULog
  | ULog2
  | USign
  | UNot
  | UFloatToInt
  | UIntToFloat
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

-- | Binary operations (arithmetic, math, comparisons, vector ops).
data BinOp
  = BAdd
  | BSub
  | BMul
  | BDiv
  | BMod
  | BMin
  | BMax
  | BPow
  | BAtan2
  | BStep
  | BEq
  | BNeq
  | BLt
  | BLe
  | BGt
  | BGe
  | BAnd
  | BOr
  | BDot
  | BCross
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

-- | Ternary operations (@mix@, @clamp@, @smoothstep@, branchless @select@).
data TriOp
  = TMix
  | TClamp
  | TSmoothstep
  | TSelect
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

-- | A self-contained bounded loop body whose SSA nodes may reference
-- 'OpLoopVar', 'OpLoopState', and outer 'NodeId's via 'OpOuterRef'.
data LoopBody = LoopBody
  { lbLoopId :: !Int
  , lbStart :: !Int32
  , lbEnd :: !Int32
  , lbInits :: ![NodeId]
  -- ^ Initial accumulator values from the enclosing scope.
  , lbStateTypes :: ![IRType]
  -- ^ Types of each loop accumulator slot (same length as 'lbInits').
  , lbBodyNodes :: !(Vector (IRType, ExprOp))
  -- ^ Local SSA nodes evaluated on every iteration.
  , lbNextStates :: ![NodeId]
  -- ^ Indices into 'lbBodyNodes' yielding the next iteration's accumulators.
  }
  deriving (Eq, Show, Generic)

instance Hashable LoopBody where
  hashWithSalt s lb =
    s
      `hashWithSalt` lbLoopId lb
      `hashWithSalt` lbStart lb
      `hashWithSalt` lbEnd lb
      `hashWithSalt` lbInits lb
      `hashWithSalt` lbStateTypes lb
      `hashWithSalt` V.toList (lbBodyNodes lb)
      `hashWithSalt` lbNextStates lb

-- | One SSA instruction in 'ShaderIR'. Float literals hash by IEEE 754 bit
-- pattern so @-0.0@ and @0.0@ remain distinct and hashing is exact.
data ExprOp
  = OpLitFloat !Word32
  -- ^ IEEE 754 bit pattern of a 32-bit float literal ('castFloatToWord32').
  | OpLitInt !Int32
  | OpLitUInt !Word32
  | OpLitBool !Bool
  | OpSys !SysField
  | OpUniform !Int !GpuType
  -- ^ Index into 'irUniforms' and its GPU type.
  | OpConstruct !IRType ![NodeId]
  -- ^ Construct a vector from scalar/vector components (or splat one scalar).
  | OpSwizzle !NodeId ![Int]
  -- ^ Extract 1..4 components (each in @0..3@).
  | OpUnary !UnOp !NodeId
  | OpBinary !BinOp !NodeId !NodeId
  | OpTernary !TriOp !NodeId !NodeId !NodeId
  | OpLength !NodeId
  | OpNormalize !NodeId
  | OpSampleInput !NodeId
  -- ^ Sample @uInput@ at target UV (@TVec2@), returning @TVec4@.
  | OpOuterRef !Int !NodeId
  -- ^ Inside a 'LoopBody', references node 'NodeId' from enclosing scope depth @d@.
  | OpLoopVar !Int
  -- ^ Inside a 'LoopBody', reads the induction variable @i :: TInt@.
  | OpLoopState !Int !Int
  -- ^ Inside a 'LoopBody', reads loop @loopId@'s accumulator slot @k@.
  | OpLoop !LoopBody
  -- ^ Executes a bounded loop and binds its final accumulator tuple.
  | OpLoopResult !NodeId !Int
  -- ^ Projects accumulator slot @k@ from an 'OpLoop' node.
  deriving (Eq, Show, Generic)

instance Hashable ExprOp where
  hashWithSalt s op = case op of
    OpLitFloat w -> s `hashWithSalt` (0 :: Int) `hashWithSalt` w
    OpLitInt n -> s `hashWithSalt` (1 :: Int) `hashWithSalt` n
    OpLitUInt w -> s `hashWithSalt` (2 :: Int) `hashWithSalt` w
    OpLitBool b -> s `hashWithSalt` (3 :: Int) `hashWithSalt` b
    OpSys sf -> s `hashWithSalt` (4 :: Int) `hashWithSalt` sf
    OpUniform idx gt -> s `hashWithSalt` (5 :: Int) `hashWithSalt` idx `hashWithSalt` fromEnum gt
    OpConstruct t args -> s `hashWithSalt` (6 :: Int) `hashWithSalt` t `hashWithSalt` args
    OpSwizzle a cs -> s `hashWithSalt` (7 :: Int) `hashWithSalt` a `hashWithSalt` cs
    OpUnary uo a -> s `hashWithSalt` (8 :: Int) `hashWithSalt` uo `hashWithSalt` a
    OpBinary bo a b -> s `hashWithSalt` (9 :: Int) `hashWithSalt` bo `hashWithSalt` a `hashWithSalt` b
    OpTernary to a b c -> s `hashWithSalt` (10 :: Int) `hashWithSalt` to `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c
    OpLength a -> s `hashWithSalt` (11 :: Int) `hashWithSalt` a
    OpNormalize a -> s `hashWithSalt` (12 :: Int) `hashWithSalt` a
    OpSampleInput a -> s `hashWithSalt` (13 :: Int) `hashWithSalt` a
    OpOuterRef d n -> s `hashWithSalt` (14 :: Int) `hashWithSalt` d `hashWithSalt` n
    OpLoopVar lid -> s `hashWithSalt` (15 :: Int) `hashWithSalt` lid
    OpLoopState lid k -> s `hashWithSalt` (16 :: Int) `hashWithSalt` lid `hashWithSalt` k
    OpLoop lb -> s `hashWithSalt` (17 :: Int) `hashWithSalt` lb
    OpLoopResult n k -> s `hashWithSalt` (18 :: Int) `hashWithSalt` n `hashWithSalt` k

-- | Complete, lowered fragment shader IR ready for code generation or CPU
-- evaluation.
data ShaderIR = ShaderIR
  { irNodes :: !(Vector (IRType, ExprOp))
  -- ^ Topologically ordered SSA nodes.
  , irOutput :: !NodeId
  -- ^ Final output color node (type 'TVec4').
  , irUniforms :: ![UniformField]
  -- ^ Flattened std140 user uniform fields (relative to user block start).
  , irUniformBytes :: !Int
  -- ^ Padded std140 byte count for user uniforms (multiple of 16; 0 for none).
  , irNeedsInput :: !Bool
  -- ^ Whether any instruction samples @uInput@.
  }
  deriving (Eq, Show)

-- | Pure hash-consing SSA DAG builder.
data IRBuilder = IRBuilder
  { irbNodes :: ![(IRType, ExprOp)]
  -- ^ Emitted nodes in reverse order.
  , irbCount :: !Int
  , irbMemo :: !(HashMap (IRType, ExprOp) NodeId)
  -- ^ Structural hash-consing table for global value numbering (CSE).
  , irbNeedsInput :: !Bool
  , irbNextLoopId :: !Int
  }

-- | Start an empty builder.
emptyIRBuilder :: IRBuilder
emptyIRBuilder =
  IRBuilder
    { irbNodes = []
    , irbCount = 0
    , irbMemo = HM.empty
    , irbNeedsInput = False
    , irbNextLoopId = 0
    }

-- | Finalize a builder into a 'ShaderIR'.
runIRBuilder :: [UniformField] -> Int -> IRBuilder -> NodeId -> ShaderIR
runIRBuilder fields uBytes b outId =
  ShaderIR
    { irNodes = V.fromList (reverse (irbNodes b))
    , irOutput = outId
    , irUniforms = fields
    , irUniformBytes = uBytes
    , irNeedsInput = irbNeedsInput b
    }

-- | Emit an SSA node with pure structural hash-consing (CSE).
emitNode :: IRType -> ExprOp -> IRBuilder -> (NodeId, IRBuilder)
emitNode ty op b =
  let !b0 = case op of
        OpSampleInput _ -> b {irbNeedsInput = True}
        _ -> b
      key = (ty, op)
   in case HM.lookup key (irbMemo b0) of
        Just existing -> (existing, b0)
        Nothing ->
          let !nid = NodeId (irbCount b0)
              !b' =
                b0
                  { irbNodes = key : irbNodes b0
                  , irbCount = irbCount b0 + 1
                  , irbMemo = HM.insert key nid (irbMemo b0)
                  }
           in (nid, b')

-- | Emit a bounded loop node and allocate its unique loop identifier.
emitLoop ::
  Int32 ->
  Int32 ->
  [NodeId] ->
  [IRType] ->
  (Int -> IRBuilder -> ([NodeId], IRBuilder)) ->
  IRBuilder ->
  (NodeId, IRBuilder)
emitLoop start end inits stateTypes buildBody b =
  let !lid = irbNextLoopId b
      sub0 = emptyIRBuilder {irbNextLoopId = lid + 1}
      (nexts, sub1) = buildBody lid sub0
      lb =
        LoopBody
          { lbLoopId = lid
          , lbStart = start
          , lbEnd = end
          , lbInits = inits
          , lbStateTypes = stateTypes
          , lbBodyNodes = V.fromList (reverse (irbNodes sub1))
          , lbNextStates = nexts
          }
      !b1 =
        b
          { irbNeedsInput = irbNeedsInput b || irbNeedsInput sub1
          , irbNextLoopId = irbNextLoopId sub1
          }
   in emitNode (case stateTypes of (t : _) -> t; [] -> TFloat) (OpLoop lb) b1

-- | Deterministic non-zero 64-bit structural hash of a lowered 'ShaderIR'.
-- Hashing the IR rather than compiled backend strings keeps 'ShaderProgram'
-- fields lazy so each backend only compiles the target format it consumes.
hashShaderIR :: ShaderIR -> Word64
hashShaderIR ir =
  let hashWith s =
        s
          `hashWithSalt` irNeedsInput ir
          `hashWithSalt` irUniformBytes ir
          `hashWithSalt` irOutput ir
          `hashWithSalt` [(fromEnum (ufType f), ufOffset f) | f <- irUniforms ir]
          `hashWithSalt` V.toList (irNodes ir)
      !h1 = fromIntegral (hashWith (0x517cc1b7 :: Int)) :: Word64
      !h2 = fromIntegral (hashWith (0x27d4eb2f :: Int)) :: Word64
      !h = (h1 `shiftL` 32) .|. (h2 `xor` (h1 * 0x9e3779b97f4a7c15))
   in if h == 0 then 1 else h
