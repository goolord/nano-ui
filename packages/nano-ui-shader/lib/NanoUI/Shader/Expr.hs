{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Pure-Haskell typed fragment shader EDSL.
--
-- Simple shaders are pure functions @ExprU u -> FragCtx -> Expr V4@ lowered
-- through structural hash-consing (CSE). Iterative shaders (blur kernels,
-- multi-octave noise) can use 'FragM' for explicit SSA 'bind' and bounded
-- 'forRange' loops.
module NanoUI.Shader.Expr
  ( -- * Expressions and monadic builder
    Expr
  , runExpr
  , FragM
  , runFragM
  , liftExpr
  , bind
  , LoopAcc (..)
  , forRange
  , GpuExpr (..)
  , GpuFloat
  , GpuVec
  , GpuSigned
  , GpuMinMax
  , GpuOrd
  , GpuEq
    -- * Uniform access
  , ExprU (..)
  , UniformLeaf (..)
  , uVal
  , uFst
  , uSnd
  , uFst3
  , uSnd3
  , uThd3
    -- * Built-in fragment context and layer sampling
  , FragCtx (..)
  , mkFragCtx
  , Layer
  , mkLayer
  , sampleLayer
  , sampleLayerUv
    -- * Literals and vector construction
  , floatE
  , intE
  , uintE
  , boolE
  , colorE
  , v2E
  , v3E
  , v4E
  , vec2
  , vec3
  , vec4
  , vec3_21
  , vec4_22
  , vec4_31
  , rgba
  , splat2
  , splat3
  , splat4
  , floatToInt
  , intToFloat
    -- * Scalar and vector math
  , absE
  , signE
  , floorE
  , ceilE
  , fractE
  , modE
  , minE
  , maxE
  , clampE
  , clamp01E
  , mix
  , stepE
  , smoothstep
  , powE
  , atan2E
  , dot
  , cross
  , vLength
  , vDistance
  , normalize
  , (*^)
  , (^*)
  , (/^)
  , (+^)
  , (-^)
    -- * Comparisons and branchless selection
  , (.<)
  , (.<=)
  , (.>)
  , (.>=)
  , (.==)
  , (./=)
  , (.&&)
  , (.||)
  , notE
  , selectE
    -- * Common 2D SDF, color, and noise helpers
  , sdCircle
  , sdRoundedBox
  , luminanceE
  , withAlphaE
  , hsvToRgbE
  , hash21
  , valueNoise2D
  ) where

import Data.Bits (shiftR, (.&.))
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import GHC.Float (castFloatToWord32)
import GHC.Generics
  ( (:*:) (..)
  , C1
  , D1
  , Generic (..)
  , K1 (..)
  , Meta (MetaSel)
  , S1
  , U1
  )
import GHC.Records (HasField (..))
import GHC.TypeLits
  ( ErrorMessage (..)
  , KnownNat
  , Nat
  , Symbol
  , TypeError
  , natVal
  , type (+)
  )
import NanoUI (Color (..), Rect, Size, V2 (..))
import NanoUI.Internal.Uniforms (GpuType (..), V3 (..), V4 (..))
import NanoUI.Shader.Internal.IR
import NanoUI.Shader.Internal.Swizzle (Swizzle (..))

-- | Typed fragment shader expression lowered into 'ShaderIR' at lexical scope
-- depth @d@.
newtype Expr a = Expr (Int -> IRBuilder -> (NodeId, IRBuilder))

-- | Lower an 'Expr' into an 'IRBuilder' at scope depth @d@.
runExpr :: Int -> Expr a -> IRBuilder -> (NodeId, IRBuilder)
runExpr d (Expr f) = f d

-- | Monadic fragment builder for explicit SSA 'bind' and bounded 'forRange'
-- loops.
newtype FragM a = FragM (Int -> IRBuilder -> (a, IRBuilder))

instance Functor FragM where
  fmap f (FragM g) = FragM $ \d b ->
    let (a, b') = g d b
     in (f a, b')

instance Applicative FragM where
  pure a = FragM $ \_ b -> (a, b)
  FragM ff <*> FragM fa = FragM $ \d b ->
    let (f, b1) = ff d b
        (a, b2) = fa d b1
     in (f a, b2)

instance Monad FragM where
  FragM fa >>= k = FragM $ \d b ->
    let (a, b1) = fa d b
        FragM fb = k a
     in fb d b1

-- | Lower a 'FragM' action returning an @Expr V4@ at scope depth 0.
runFragM :: FragM (Expr V4) -> IRBuilder -> (NodeId, IRBuilder)
runFragM (FragM f) b0 =
  let (e, b1) = f 0 b0
   in runExpr 0 e b1

-- | Lift a pure 'Expr' into 'FragM'.
liftExpr :: Expr a -> FragM (Expr a)
liftExpr = pure

-- | Mapping from Haskell shader expression types to 'IRType'.
class GpuExpr a where
  exprIRType :: Proxy a -> IRType

instance GpuExpr Float where exprIRType _ = TFloat
instance GpuExpr Int32 where exprIRType _ = TInt
instance GpuExpr Word32 where exprIRType _ = TUInt
instance GpuExpr Bool where exprIRType _ = TBool
instance GpuExpr V2 where exprIRType _ = TVec2
instance GpuExpr V3 where exprIRType _ = TVec3
instance GpuExpr V4 where exprIRType _ = TVec4

-- | Floating-point scalar and vector expression types.
class GpuExpr a => GpuFloat a
instance GpuFloat Float
instance GpuFloat V2
instance GpuFloat V3
instance GpuFloat V4

-- | Floating-point vector expression types (@V2@, @V3@, @V4@).
class GpuFloat a => GpuVec a
instance GpuVec V2
instance GpuVec V3
instance GpuVec V4

-- | Signed expression types supporting 'absE' and 'signE'.
class GpuExpr a => GpuSigned a
instance GpuSigned Float
instance GpuSigned V2
instance GpuSigned V3
instance GpuSigned V4
instance GpuSigned Int32

-- | Numeric expression types supporting 'minE', 'maxE', and 'clampE'.
class GpuExpr a => GpuMinMax a
instance GpuMinMax Float
instance GpuMinMax V2
instance GpuMinMax V3
instance GpuMinMax V4
instance GpuMinMax Int32
instance GpuMinMax Word32

-- | Ordered scalar expression types supporting @.<@, @.<=@, @.>@, and @.>=@.
class GpuExpr a => GpuOrd a
instance GpuOrd Float
instance GpuOrd Int32
instance GpuOrd Word32

-- | Scalar expression types supporting @.==@ and @./=@.
class GpuExpr a => GpuEq a
instance GpuEq Float
instance GpuEq Int32
instance GpuEq Word32
instance GpuEq Bool

-- | Reference an already-emitted SSA node from scope depth @defDepth@.
refNode :: forall a. GpuExpr a => Int -> NodeId -> Expr a
refNode defDepth nid = Expr $ \curDepth b ->
  if curDepth == defDepth
    then (nid, b)
    else emitNode (exprIRType (Proxy @a)) (OpOuterRef defDepth nid) b

-- | Emit @e@ once in the current scope and return a reference expression that
-- reuses that SSA value everywhere (including inside nested 'forRange' loops).
bind :: forall a. GpuExpr a => Expr a -> FragM (Expr a)
bind e = FragM $ \d b ->
  let (nid, b') = runExpr d e b
   in (refNode d nid, b')

-- | Values that can be carried across iterations of a bounded GPU 'forRange'
-- loop (single 'Expr's or tuples of them).
class LoopAcc s where
  loopAccTypes :: Proxy s -> [IRType]
  loopAccEmit :: Int -> s -> IRBuilder -> ([NodeId], IRBuilder)
  loopAccState :: Int -> Int -> (s, Int)
  loopAccProject :: Int -> NodeId -> Int -> IRBuilder -> (s, Int, IRBuilder)

instance GpuExpr a => LoopAcc (Expr a) where
  loopAccTypes _ = [exprIRType (Proxy @a)]
  loopAccEmit d e b =
    let (nid, b') = runExpr d e b
     in ([nid], b')
  loopAccState lid slot =
    let e = Expr $ \_ b -> emitNode (exprIRType (Proxy @a)) (OpLoopState lid slot) b
     in (e, slot + 1)
  loopAccProject defDepth loopNode slot b0 =
    let (nid, b1) = emitNode (exprIRType (Proxy @a)) (OpLoopResult loopNode slot) b0
     in (refNode defDepth nid, slot + 1, b1)

instance (LoopAcc a, LoopAcc b) => LoopAcc (a, b) where
  loopAccTypes _ = loopAccTypes (Proxy @a) <> loopAccTypes (Proxy @b)
  loopAccEmit d (a, b) ir0 =
    let (na, ir1) = loopAccEmit d a ir0
        (nb, ir2) = loopAccEmit d b ir1
     in (na <> nb, ir2)
  loopAccState lid slot0 =
    let (a, slot1) = loopAccState lid slot0
        (b, slot2) = loopAccState lid slot1
     in ((a, b), slot2)
  loopAccProject defDepth loopNode slot0 b0 =
    let (a, slot1, b1) = loopAccProject defDepth loopNode slot0 b0
        (b, slot2, b2) = loopAccProject defDepth loopNode slot1 b1
     in ((a, b), slot2, b2)

instance (LoopAcc a, LoopAcc b, LoopAcc c) => LoopAcc (a, b, c) where
  loopAccTypes _ = loopAccTypes (Proxy @a) <> loopAccTypes (Proxy @b) <> loopAccTypes (Proxy @c)
  loopAccEmit d (a, b, c) ir0 =
    let (na, ir1) = loopAccEmit d a ir0
        (nb, ir2) = loopAccEmit d b ir1
        (nc, ir3) = loopAccEmit d c ir2
     in (na <> nb <> nc, ir3)
  loopAccState lid slot0 =
    let (a, slot1) = loopAccState lid slot0
        (b, slot2) = loopAccState lid slot1
        (c, slot3) = loopAccState lid slot2
     in ((a, b, c), slot3)
  loopAccProject defDepth loopNode slot0 b0 =
    let (a, slot1, b1) = loopAccProject defDepth loopNode slot0 b0
        (b, slot2, b2) = loopAccProject defDepth loopNode slot1 b1
        (c, slot3, b3) = loopAccProject defDepth loopNode slot2 b2
     in ((a, b, c), slot3, b3)

-- | Bounded GPU loop from @start@ (inclusive) to @end@ (exclusive), threading
-- accumulator state @s@. Emits a structured loop in GLSL, MSL, and SPIR-V.
forRange ::
  forall s.
  LoopAcc s =>
  Int32 ->
  Int32 ->
  s ->
  (Expr Int32 -> s -> FragM s) ->
  FragM s
forRange start end initState stepFn = FragM $ \d b0 ->
  let (initIds, b1) = loopAccEmit d initState b0
      tys = loopAccTypes (Proxy @s)
      innerDepth = d + 1
      (loopNode, b2) = emitLoop start end initIds tys (\lid sub0 ->
        let loopVar = Expr $ \_ sb -> emitNode TInt (OpLoopVar lid) sb
            (stIn, _) = loopAccState lid 0
            FragM bodyAct = stepFn loopVar stIn
            (stOut, sub1) = bodyAct innerDepth sub0
         in loopAccEmit innerDepth stOut sub1
        ) b1
      (resultState, _, b3) = loopAccProject d loopNode 0 b2
   in (resultState, b3)

-- ---------------------------------------------------------------------------
-- OverloadedRecordDot swizzling on Expr V2 / V3 / V4
-- ---------------------------------------------------------------------------

instance (GpuExpr r, Swizzle s v r) => HasField s (Expr v) (Expr r) where
  getField v = Expr $ \d b ->
    let (vid, b1) = runExpr d v b
        idxs = swizzleIndices (Proxy @s) (Proxy @v)
     in emitNode (exprIRType (Proxy @r)) (OpSwizzle vid idxs) b1

-- ---------------------------------------------------------------------------
-- Typed uniform handle ExprU u with OverloadedRecordDot field projection
-- ---------------------------------------------------------------------------

-- | Handle to a shader's uniform record @u@ inside an EDSL body. Carries the
-- starting flattened field index (0 for standalone shaders; offset for fused
-- shaders).
newtype ExprU u = ExprU Int

-- | Mapping from a single primitive Haskell uniform field type to its EDSL
-- expression type and 'GpuType'.
class GpuExpr r => UniformLeaf u r | u -> r where
  leafGpuType :: Proxy u -> GpuType

instance UniformLeaf Float Float where leafGpuType _ = GFloat
instance UniformLeaf Int32 Int32 where leafGpuType _ = GInt32
instance UniformLeaf Word32 Word32 where leafGpuType _ = GUInt32
instance UniformLeaf Bool Bool where leafGpuType _ = GBool
instance UniformLeaf V2 V2 where leafGpuType _ = GVec2
instance UniformLeaf V3 V3 where leafGpuType _ = GVec3
instance UniformLeaf V4 V4 where leafGpuType _ = GVec4
instance UniformLeaf Color V4 where leafGpuType _ = GVec4
instance UniformLeaf Rect V4 where leafGpuType _ = GVec4
instance UniformLeaf Size V2 where leafGpuType _ = GVec2

-- | Read a primitive uniform directly (for shaders parameterized by a single
-- @Float@, @Color@, @V2@, etc., rather than a record).
uVal :: forall u r. UniformLeaf u r => ExprU u -> Expr r
uVal (ExprU base) = Expr $ \_ b ->
  emitNode (exprIRType (Proxy @r)) (OpUniform base (leafGpuType (Proxy @u))) b

-- | Project the first component of a paired uniform handle.
uFst :: ExprU (a, b) -> ExprU a
uFst (ExprU base) = ExprU base

-- | Project the second component of a paired uniform handle.
uSnd :: forall a b. KnownNat (UniformLeafCount a) => ExprU (a, b) -> ExprU b
uSnd (ExprU base) = ExprU (base + fromIntegral (natVal (Proxy @(UniformLeafCount a))))

-- | Project the first component of a 3-tuple uniform handle.
uFst3 :: ExprU (a, b, c) -> ExprU a
uFst3 (ExprU base) = ExprU base

-- | Project the second component of a 3-tuple uniform handle.
uSnd3 :: forall a b c. KnownNat (UniformLeafCount a) => ExprU (a, b, c) -> ExprU b
uSnd3 (ExprU base) = ExprU (base + fromIntegral (natVal (Proxy @(UniformLeafCount a))))

-- | Project the third component of a 3-tuple uniform handle.
uThd3 ::
  forall a b c.
  (KnownNat (UniformLeafCount a), KnownNat (UniformLeafCount b)) =>
  ExprU (a, b, c) ->
  ExprU c
uThd3 (ExprU base) =
  ExprU
    ( base
        + fromIntegral (natVal (Proxy @(UniformLeafCount a)))
        + fromIntegral (natVal (Proxy @(UniformLeafCount b)))
    )

-- | Type-level count of flattened primitive uniform fields in @a@.
type family UniformLeafCount (a :: Type) :: Nat where
  UniformLeafCount () = 0
  UniformLeafCount Float = 1
  UniformLeafCount Int32 = 1
  UniformLeafCount Word32 = 1
  UniformLeafCount Bool = 1
  UniformLeafCount V2 = 1
  UniformLeafCount V3 = 1
  UniformLeafCount V4 = 1
  UniformLeafCount Color = 1
  UniformLeafCount Rect = 1
  UniformLeafCount Size = 1
  UniformLeafCount (a, b) = UniformLeafCount a + UniformLeafCount b
  UniformLeafCount (a, b, c) = UniformLeafCount a + UniformLeafCount b + UniformLeafCount c
  UniformLeafCount r = GFieldCount (Rep r)

type family GFieldCount (f :: Type -> Type) :: Nat where
  GFieldCount U1 = 0
  GFieldCount (D1 _ f) = GFieldCount f
  GFieldCount (C1 _ f) = GFieldCount f
  GFieldCount (S1 _ f) = GFieldCount f
  GFieldCount (f :*: g) = GFieldCount f + GFieldCount g
  GFieldCount (K1 _ a) = UniformLeafCount a

-- | Type-level lookup of record field @name@ inside @Rep u@, returning its
-- flattened field index and Haskell field type.
type family GLookupField (name :: Symbol) (f :: Type -> Type) (off :: Nat) :: Maybe (Nat, Type) where
  GLookupField name (D1 _ f) off = GLookupField name f off
  GLookupField name (C1 _ f) off = GLookupField name f off
  GLookupField name (S1 ('MetaSel ('Just name) _ _ _) (K1 _ a)) off = 'Just '(off, a)
  GLookupField _ (S1 _ _) _ = 'Nothing
  GLookupField name (f :*: g) off =
    AltMaybe (GLookupField name f off) (GLookupField name g (off + GFieldCount f))
  GLookupField _ U1 _ = 'Nothing

type family AltMaybe (a :: Maybe k) (b :: Maybe k) :: Maybe k where
  AltMaybe ('Just x) _ = 'Just x
  AltMaybe 'Nothing y = y

type family RequireField (u :: Type) (name :: Symbol) (m :: Maybe (Nat, Type)) :: (Nat, Type) where
  RequireField _ _ ('Just hit) = hit
  RequireField u name 'Nothing =
    TypeError
      ( 'Text "Uniform record "
          ':<>: 'ShowType u
          ':<>: 'Text " has no field '"
          ':<>: 'Text name
          ':<>: 'Text "'."
      )

type family IsUniformLeaf (a :: Type) :: Bool where
  IsUniformLeaf Float = 'True
  IsUniformLeaf Int32 = 'True
  IsUniformLeaf Word32 = 'True
  IsUniformLeaf Bool = 'True
  IsUniformLeaf V2 = 'True
  IsUniformLeaf V3 = 'True
  IsUniformLeaf V4 = 'True
  IsUniformLeaf Color = 'True
  IsUniformLeaf Rect = 'True
  IsUniformLeaf Size = 'True
  IsUniformLeaf _ = 'False

class ProjectUniform (isLeaf :: Bool) (fieldTy :: Type) (res :: Type) | isLeaf fieldTy -> res where
  projectUniform :: Proxy isLeaf -> Proxy fieldTy -> Int -> res

instance UniformLeaf fieldTy r => ProjectUniform 'True fieldTy (Expr r) where
  projectUniform _ _ fieldIdx = Expr $ \_ b ->
    emitNode (exprIRType (Proxy @r)) (OpUniform fieldIdx (leafGpuType (Proxy @fieldTy))) b

instance ProjectUniform 'False fieldTy (ExprU fieldTy) where
  projectUniform _ _ = ExprU

instance
  ( '(idx, fieldTy) ~ RequireField u name (GLookupField name (Rep u) 0)
  , KnownNat idx
  , isLeaf ~ IsUniformLeaf fieldTy
  , ProjectUniform isLeaf fieldTy res
  ) =>
  HasField name (ExprU u) res
  where
  getField (ExprU base) =
    projectUniform (Proxy @isLeaf) (Proxy @fieldTy) (base + fromIntegral (natVal (Proxy @idx)))

-- ---------------------------------------------------------------------------
-- Fragment context and input layer sampling
-- ---------------------------------------------------------------------------

-- | Built-in per-fragment inputs provided to every shader.
data FragCtx = FragCtx
  { uv :: !(Expr V2)
  -- ^ Normalized coordinates in @[0, 1] x [0, 1]@ over the node's layout rect
  -- (may extend outside @[0, 1]@ inside a 'LayerEffect' bleed margin).
  , pos :: !(Expr V2)
  -- ^ Local logical pixel coordinates relative to the node's top-left corner.
  , size :: !(Expr V2)
  -- ^ Node width and height in logical pixels.
  , origin :: !(Expr V2)
  -- ^ Node top-left origin in logical window pixels.
  , scale :: !(Expr Float)
  -- ^ Device pixels per logical layout unit.
  , cornerRadius :: !(Expr Float)
  -- ^ Corner radius in logical pixels from 'NanoUI.ShaderConfig'.
  , color :: !(Expr V4)
  -- ^ Vertex tint color.
  }

sysExpr :: forall a. GpuExpr a => SysField -> Expr a
sysExpr sf = Expr $ \_ b -> emitNode (exprIRType (Proxy @a)) (OpSys sf) b

-- | Construct the standard 'FragCtx' from the system uniform header and vertex
-- varyings.
mkFragCtx :: FragCtx
mkFragCtx =
  let rect = sysExpr @V4 SysRect
      uvRect = sysExpr @V4 SysUvRect
      meta = sysExpr @V4 SysMeta
      vUv = sysExpr @V2 SysVUv
      vCol = sysExpr @V4 SysVColor
      uvOrigin = swiz2 [0, 1] uvRect
      uvSpan = swiz2 [2, 3] uvRect
      nodeSize = swiz2 [2, 3] rect
      nodeOrigin = swiz2 [0, 1] rect
      localUv = (vUv - uvOrigin) / uvSpan
      localPos = localUv * nodeSize
   in FragCtx
        { uv = localUv
        , pos = localPos
        , size = nodeSize
        , origin = nodeOrigin
        , scale = swiz1 0 meta
        , cornerRadius = swiz1 1 meta
        , color = vCol
        }

swiz1 :: Int -> Expr v -> Expr Float
swiz1 i v = Expr $ \d b ->
  let (vid, b1) = runExpr d v b
   in emitNode TFloat (OpSwizzle vid [i]) b1

swiz2 :: [Int] -> Expr v -> Expr V2
swiz2 is v = Expr $ \d b ->
  let (vid, b1) = runExpr d v b
   in emitNode TVec2 (OpSwizzle vid is) b1

-- | Handle to the rendered offscreen subtree texture inside a 'LayerEffect'.
data Layer = Layer
  { layerUvOrigin :: !(Expr V2)
  , layerUvSpan :: !(Expr V2)
  , layerNodeSize :: !(Expr V2)
  }

-- | Construct the default 'Layer' handle backed by @uInput@.
mkLayer :: Layer
mkLayer =
  let rect = sysExpr @V4 SysRect
      uvRect = sysExpr @V4 SysUvRect
   in Layer
        { layerUvOrigin = swiz2 [0, 1] uvRect
        , layerUvSpan = swiz2 [2, 3] uvRect
        , layerNodeSize = swiz2 [2, 3] rect
        }

-- | Sample the offscreen subtree texture at local logical pixel coordinates
-- (where @(0, 0)@ is the node's top-left corner).
sampleLayer :: Layer -> Expr V2 -> Expr V4
sampleLayer l localPos =
  let normUv = localPos / maxE (vec2 1e-6 1e-6) (layerNodeSize l)
   in sampleLayerUv l normUv

-- | Sample the offscreen subtree texture at node-normalized UV coordinates.
sampleLayerUv :: Layer -> Expr V2 -> Expr V4
sampleLayerUv l normUv =
  let texUv = layerUvOrigin l + normUv * layerUvSpan l
   in Expr $ \d b ->
        let (uid, b1) = runExpr d texUv b
         in emitNode TVec4 (OpSampleInput uid) b1

-- ---------------------------------------------------------------------------
-- Literals and vector construction
-- ---------------------------------------------------------------------------

-- | Float literal expression.
floatE :: Float -> Expr Float
floatE x = Expr $ \_ b -> emitNode TFloat (OpLitFloat (castFloatToWord32 x)) b

-- | Signed 32-bit integer literal expression.
intE :: Int32 -> Expr Int32
intE n = Expr $ \_ b -> emitNode TInt (OpLitInt n) b

-- | Unsigned 32-bit integer literal expression.
uintE :: Word32 -> Expr Word32
uintE w = Expr $ \_ b -> emitNode TUInt (OpLitUInt w) b

-- | Boolean literal expression.
boolE :: Bool -> Expr Bool
boolE c = Expr $ \_ b -> emitNode TBool (OpLitBool c) b

-- | Lift a constant 'Color' to a straight-alpha @Expr V4@ in @[0, 1]@.
colorE :: Color -> Expr V4
colorE (Color w) =
  let inv = 1.0 / 255.0 :: Float
      r = fromIntegral ((w `shiftR` 24) .&. 0xFF) * inv
      g = fromIntegral ((w `shiftR` 16) .&. 0xFF) * inv
      b = fromIntegral ((w `shiftR` 8) .&. 0xFF) * inv
      a = fromIntegral (w .&. 0xFF) * inv
   in vec4 (floatE r) (floatE g) (floatE b) (floatE a)

-- | Lift a constant 'V2'.
v2E :: V2 -> Expr V2
v2E (V2 x y) = vec2 (floatE x) (floatE y)

-- | Lift a constant 'V3'.
v3E :: V3 -> Expr V3
v3E (V3 x y z) = vec3 (floatE x) (floatE y) (floatE z)

-- | Lift a constant 'V4'.
v4E :: V4 -> Expr V4
v4E (V4 x y z w) = vec4 (floatE x) (floatE y) (floatE z) (floatE w)

-- | Construct a 2-vector from two scalars.
vec2 :: Expr Float -> Expr Float -> Expr V2
vec2 x y = Expr $ \d b0 ->
  let (xid, b1) = runExpr d x b0
      (yid, b2) = runExpr d y b1
   in emitNode TVec2 (OpConstruct TVec2 [xid, yid]) b2

-- | Construct a 3-vector from three scalars.
vec3 :: Expr Float -> Expr Float -> Expr Float -> Expr V3
vec3 x y z = Expr $ \d b0 ->
  let (xid, b1) = runExpr d x b0
      (yid, b2) = runExpr d y b1
      (zid, b3) = runExpr d z b2
   in emitNode TVec3 (OpConstruct TVec3 [xid, yid, zid]) b3

-- | Construct a 4-vector from four scalars.
vec4 :: Expr Float -> Expr Float -> Expr Float -> Expr Float -> Expr V4
vec4 x y z w = Expr $ \d b0 ->
  let (xid, b1) = runExpr d x b0
      (yid, b2) = runExpr d y b1
      (zid, b3) = runExpr d z b2
      (wid, b4) = runExpr d w b3
   in emitNode TVec4 (OpConstruct TVec4 [xid, yid, zid, wid]) b4

-- | Construct a 3-vector from a @V2@ and a trailing scalar.
vec3_21 :: Expr V2 -> Expr Float -> Expr V3
vec3_21 xy z = vec3 (swiz1 0 xy) (swiz1 1 xy) z

-- | Construct a 4-vector from two @V2@s.
vec4_22 :: Expr V2 -> Expr V2 -> Expr V4
vec4_22 xy zw = vec4 (swiz1 0 xy) (swiz1 1 xy) (swiz1 0 zw) (swiz1 1 zw)

-- | Construct a 4-vector from a @V3@ and a trailing scalar.
vec4_31 :: Expr V3 -> Expr Float -> Expr V4
vec4_31 rgb a = vec4 (swiz1 0 rgb) (swiz1 1 rgb) (swiz1 2 rgb) a

-- | Construct an RGBA color from RGB @V3@ and alpha.
rgba :: Expr V3 -> Expr Float -> Expr V4
rgba = vec4_31

-- | Broadcast a scalar across a @V2@.
splat2 :: Expr Float -> Expr V2
splat2 x = vec2 x x

-- | Broadcast a scalar across a @V3@.
splat3 :: Expr Float -> Expr V3
splat3 x = vec3 x x x

-- | Broadcast a scalar across a @V4@.
splat4 :: Expr Float -> Expr V4
splat4 x = vec4 x x x x

-- | Truncate a float expression to signed 32-bit integer.
floatToInt :: Expr Float -> Expr Int32
floatToInt = unOpE UFloatToInt

-- | Convert a signed 32-bit integer expression to float.
intToFloat :: Expr Int32 -> Expr Float
intToFloat = unOpE UIntToFloat

-- ---------------------------------------------------------------------------
-- Numeric instances and math operations
-- ---------------------------------------------------------------------------

unOpE :: forall a b. GpuExpr b => UnOp -> Expr a -> Expr b
unOpE op x = Expr $ \d b0 ->
  let (xid, b1) = runExpr d x b0
   in emitNode (exprIRType (Proxy @b)) (OpUnary op xid) b1

binOpE :: forall a b c. GpuExpr c => BinOp -> Expr a -> Expr b -> Expr c
binOpE op x y = Expr $ \d b0 ->
  let (xid, b1) = runExpr d x b0
      (yid, b2) = runExpr d y b1
   in emitNode (exprIRType (Proxy @c)) (OpBinary op xid yid) b2

triOpE :: forall a b c r. GpuExpr r => TriOp -> Expr a -> Expr b -> Expr c -> Expr r
triOpE op x y z = Expr $ \d b0 ->
  let (xid, b1) = runExpr d x b0
      (yid, b2) = runExpr d y b1
      (zid, b3) = runExpr d z b2
   in emitNode (exprIRType (Proxy @r)) (OpTernary op xid yid zid) b3

infixl 7 *^, ^*, /^
infixl 6 +^, -^

-- | Multiply a vector expression by a scalar float expression.
(*^) :: GpuVec v => Expr v -> Expr Float -> Expr v
(*^) = binOpE BMul

-- | Multiply a scalar float expression by a vector expression.
(^*) :: GpuVec v => Expr Float -> Expr v -> Expr v
(^*) = binOpE BMul

-- | Divide a vector expression by a scalar float expression.
(/^) :: GpuVec v => Expr v -> Expr Float -> Expr v
(/^) = binOpE BDiv

-- | Add a scalar float expression to every component of a vector expression.
(+^) :: GpuVec v => Expr v -> Expr Float -> Expr v
(+^) = binOpE BAdd

-- | Subtract a scalar float expression from every component of a vector expression.
(-^) :: GpuVec v => Expr v -> Expr Float -> Expr v
(-^) = binOpE BSub

instance Num (Expr Float) where
  (+) = binOpE BAdd
  (-) = binOpE BSub
  (*) = binOpE BMul
  negate = unOpE UNeg
  abs = unOpE UAbs
  signum = unOpE USign
  fromInteger = floatE . fromInteger

instance Fractional (Expr Float) where
  (/) = binOpE BDiv
  fromRational = floatE . fromRational

instance Floating (Expr Float) where
  pi = floatE pi
  exp = unOpE UExp
  log = unOpE ULog
  sqrt = unOpE USqrt
  (**) = binOpE BPow
  sin = unOpE USin
  cos = unOpE UCos
  tan = unOpE UTan
  asin x = atan2E x (sqrt (maxE (floatE 0) (1 - x * x)))
  acos x = floatE (pi / 2) - asin x
  atan x = atan2E x 1
  sinh x = (exp x - exp (negate x)) * 0.5
  cosh x = (exp x + exp (negate x)) * 0.5
  asinh x = log (x + sqrt (x * x + 1))
  acosh x = log (x + sqrt (x * x - 1))
  atanh x = 0.5 * log ((1 + x) / (1 - x))

instance Num (Expr Int32) where
  (+) = binOpE BAdd
  (-) = binOpE BSub
  (*) = binOpE BMul
  negate = unOpE UNeg
  abs = unOpE UAbs
  signum = unOpE USign
  fromInteger = intE . fromInteger

instance Num (Expr Word32) where
  (+) = binOpE BAdd
  (-) = binOpE BSub
  (*) = binOpE BMul
  negate = unOpE UNeg
  abs = id
  signum u = selectE (u .== uintE 0) (uintE 0) (uintE 1)
  fromInteger = uintE . fromInteger

instance Num (Expr V2) where
  (+) = binOpE BAdd
  (-) = binOpE BSub
  (*) = binOpE BMul
  negate = unOpE UNeg
  abs = unOpE UAbs
  signum = unOpE USign
  fromInteger = splat2 . floatE . fromInteger

instance Fractional (Expr V2) where
  (/) = binOpE BDiv
  fromRational = splat2 . floatE . fromRational

instance Num (Expr V3) where
  (+) = binOpE BAdd
  (-) = binOpE BSub
  (*) = binOpE BMul
  negate = unOpE UNeg
  abs = unOpE UAbs
  signum = unOpE USign
  fromInteger = splat3 . floatE . fromInteger

instance Fractional (Expr V3) where
  (/) = binOpE BDiv
  fromRational = splat3 . floatE . fromRational

instance Num (Expr V4) where
  (+) = binOpE BAdd
  (-) = binOpE BSub
  (*) = binOpE BMul
  negate = unOpE UNeg
  abs = unOpE UAbs
  signum = unOpE USign
  fromInteger = splat4 . floatE . fromInteger

instance Fractional (Expr V4) where
  (/) = binOpE BDiv
  fromRational = splat4 . floatE . fromRational

-- | Componentwise absolute value.
absE :: GpuSigned a => Expr a -> Expr a
absE = unOpE UAbs

-- | Componentwise sign (@-1@, @0@, or @1@).
signE :: GpuSigned a => Expr a -> Expr a
signE = unOpE USign

-- | Componentwise floor.
floorE :: GpuFloat a => Expr a -> Expr a
floorE = unOpE UFloor

-- | Componentwise ceil.
ceilE :: GpuFloat a => Expr a -> Expr a
ceilE = unOpE UCeil

-- | Componentwise fractional part (@x - floor x@).
fractE :: GpuFloat a => Expr a -> Expr a
fractE = unOpE UFract

-- | Floating-point modulo (@x - y * floor(x / y)@).
modE :: GpuFloat a => Expr a -> Expr a -> Expr a
modE = binOpE BMod

-- | Componentwise minimum.
minE :: GpuMinMax a => Expr a -> Expr a -> Expr a
minE = binOpE BMin

-- | Componentwise maximum.
maxE :: GpuMinMax a => Expr a -> Expr a -> Expr a
maxE = binOpE BMax

-- | Componentwise clamp to @[lo, hi]@.
clampE :: GpuMinMax a => Expr a -> Expr a -> Expr a -> Expr a
clampE x lo hi = triOpE TClamp x lo hi

-- | Clamp a float expression to @[0, 1]@.
clamp01E :: Expr Float -> Expr Float
clamp01E x = clampE x 0 1

-- | Linear interpolation @a * (1 - t) + b * t@.
mix :: GpuFloat a => Expr a -> Expr a -> Expr Float -> Expr a
mix = triOpE TMix

-- | Step function @stepE edge x@: @0@ when @x < edge@, else @1@.
stepE :: Expr Float -> Expr Float -> Expr Float
stepE = binOpE BStep

-- | Smooth Hermite interpolation between @edge0@ and @edge1@.
smoothstep :: Expr Float -> Expr Float -> Expr Float -> Expr Float
smoothstep = triOpE TSmoothstep

-- | Componentwise exponentiation.
powE :: GpuFloat a => Expr a -> Expr a -> Expr a
powE = binOpE BPow

-- | Two-argument arctangent @atan2 y x@.
atan2E :: Expr Float -> Expr Float -> Expr Float
atan2E = binOpE BAtan2

-- | Vector dot product.
dot :: forall a. GpuVec a => Expr a -> Expr a -> Expr Float
dot a b =
  let !_ = exprIRType (Proxy @a)
   in binOpE BDot a b

-- | 3D vector cross product.
cross :: Expr V3 -> Expr V3 -> Expr V3
cross = binOpE BCross

-- | Euclidean vector length.
vLength :: forall a. GpuVec a => Expr a -> Expr Float
vLength v =
  let !_ = exprIRType (Proxy @a)
   in Expr $ \d b0 ->
        let (vid, b1) = runExpr d v b0
         in emitNode TFloat (OpLength vid) b1

-- | Euclidean distance between two vectors.
vDistance :: (GpuVec a, Num (Expr a)) => Expr a -> Expr a -> Expr Float
vDistance a b = vLength (a - b)

-- | Unit-length vector in the direction of @v@.
normalize :: forall a. GpuVec a => Expr a -> Expr a
normalize v = Expr $ \d b0 ->
  let (vid, b1) = runExpr d v b0
   in emitNode (exprIRType (Proxy @a)) (OpNormalize vid) b1

-- ---------------------------------------------------------------------------
-- Comparisons and branchless selection
-- ---------------------------------------------------------------------------

infix 4 .<, .<=, .>, .>=, .==, ./=
infixr 3 .&&
infixr 2 .||

ordOp :: forall a. GpuOrd a => BinOp -> Expr a -> Expr a -> Expr Bool
ordOp op a b =
  let !_ = exprIRType (Proxy @a)
   in binOpE op a b

eqOp :: forall a. GpuEq a => BinOp -> Expr a -> Expr a -> Expr Bool
eqOp op a b =
  let !_ = exprIRType (Proxy @a)
   in binOpE op a b

-- | Less-than comparison.
(.<) :: GpuOrd a => Expr a -> Expr a -> Expr Bool
(.<) = ordOp BLt

-- | Less-than-or-equal comparison.
(.<=) :: GpuOrd a => Expr a -> Expr a -> Expr Bool
(.<=) = ordOp BLe

-- | Greater-than comparison.
(.>) :: GpuOrd a => Expr a -> Expr a -> Expr Bool
(.>) = ordOp BGt

-- | Greater-than-or-equal comparison.
(.>=) :: GpuOrd a => Expr a -> Expr a -> Expr Bool
(.>=) = ordOp BGe

-- | Equality comparison.
(.==) :: GpuEq a => Expr a -> Expr a -> Expr Bool
(.==) = eqOp BEq

-- | Inequality comparison.
(./=) :: GpuEq a => Expr a -> Expr a -> Expr Bool
(./=) = eqOp BNeq

-- | Logical conjunction.
(.&&) :: Expr Bool -> Expr Bool -> Expr Bool
(.&&) = binOpE BAnd

-- | Logical disjunction.
(.||) :: Expr Bool -> Expr Bool -> Expr Bool
(.||) = binOpE BOr

-- | Logical negation.
notE :: Expr Bool -> Expr Bool
notE = unOpE UNot

-- | Branchless conditional selection: @selectE cond whenTrue whenFalse@.
selectE :: GpuExpr a => Expr Bool -> Expr a -> Expr a -> Expr a
selectE = triOpE TSelect

-- ---------------------------------------------------------------------------
-- Common 2D SDF, color, and noise helpers
-- ---------------------------------------------------------------------------

-- | Signed distance from point @p@ to a circle of radius @r@ centered at 0.
sdCircle :: Expr V2 -> Expr Float -> Expr Float
sdCircle p r = vLength p - r

-- | Signed distance from point @p@ (centered at 0) to a rounded box of
-- half-extents @halfSize@ and corner radius @r@.
sdRoundedBox :: Expr V2 -> Expr V2 -> Expr Float -> Expr Float
sdRoundedBox p halfSize r =
  let q = absE p - halfSize + splat2 r
      outside = vLength (maxE q (vec2 0 0))
      inside = minE (maxE (swiz1 0 q) (swiz1 1 q)) 0
   in outside + inside - r

-- | Rec.709 perceptual luminance of an RGBA color.
luminanceE :: Expr V4 -> Expr Float
luminanceE col =
  let rgb = vec3 (swiz1 0 col) (swiz1 1 col) (swiz1 2 col)
   in dot rgb (vec3 0.2126 0.7152 0.0722)

-- | Replace a color's alpha channel.
withAlphaE :: Expr Float -> Expr V4 -> Expr V4
withAlphaE a col = vec4 (swiz1 0 col) (swiz1 1 col) (swiz1 2 col) a

-- | Convert HSV @(h in [0,1], s in [0,1], v in [0,1])@ to RGB @V3@.
hsvToRgbE :: Expr Float -> Expr Float -> Expr Float -> Expr V3
hsvToRgbE h s v =
  let k = vec3 1.0 (2.0 / 3.0) (1.0 / 3.0)
      p = absE (fractE (splat3 h + k) * 6.0 - splat3 3.0)
      clamped = clampE (p - splat3 1.0) (splat3 0) (splat3 1)
   in splat3 v * mix (splat3 1) clamped s

-- | Fast deterministic 2D-to-1D hash in @[0, 1)@.
hash21 :: Expr V2 -> Expr Float
hash21 p =
  let p1 = fractE (p * vec2 123.34 456.21)
      p2 = p1 + splat2 (dot p1 (p1 + vec2 45.32 45.32))
   in fractE (swiz1 0 p2 * swiz1 1 p2)

-- | Smooth 2D value noise in @[0, 1]@.
valueNoise2D :: Expr V2 -> Expr Float
valueNoise2D p =
  let i = floorE p
      f = fractE p
      u = f * f * (vec2 3 3 - f * vec2 2 2)
      a = hash21 i
      b = hash21 (i + vec2 1 0)
      c = hash21 (i + vec2 0 1)
      d = hash21 (i + vec2 1 1)
      ux = swiz1 0 u
      uy = swiz1 1 u
   in mix (mix a b ux) (mix c d ux) uy
