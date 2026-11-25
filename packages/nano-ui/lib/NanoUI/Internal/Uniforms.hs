{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}

-- | GPU uniform types and std140 byte-layout packing via 'GHC.Generics'.
--
-- A uniform record is defined once in Haskell and drives host-side byte
-- packing, paint-hash invalidation, and GPU uniform-block declarations across
-- GLSL 150, GLSL ES 300, SPIR-V, and MSL. Every packed block is zero-padded
-- to a 16-byte multiple so identical values always produce identical bytes
-- and hashes.
module NanoUI.Internal.Uniforms
  ( V3 (..)
  , V4 (..)
  , GpuType (..)
  , gpuTypeSize
  , alignUp
  , UniformField (..)
  , Uniforms (..)
  , GUniforms (..)
  , uniformLayout
  , uniformSize
  , packUniformBytes
  , hashUniformBytes
  , sysUniformBytes
  , pokeSysUniforms
  ) where

import Data.Bits (shiftR, xor, (.&.))
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Primitive.ByteArray
  ( ByteArray
  , MutableByteArray
  , fillByteArray
  , indexByteArray
  , newByteArray
  , sizeofByteArray
  , unsafeFreezeByteArray
  , writeByteArray
  )
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32, Word64)
import GHC.Exts (RealWorld)
import GHC.Float (castFloatToWord32)
import GHC.Generics
  ( (:*:) (..)
  , C1
  , D1
  , Generic (..)
  , K1 (..)
  , M1 (..)
  , S1
  , Selector (..)
  , U1 (..)
  )
import NanoUI.Internal.Types (Color (..), Rect (..), Size (..), V2 (..))

-- | A three-component float vector for GPU uniforms and shader math.
data V3 = V3
  { v3X :: {-# UNPACK #-} !Float
  , v3Y :: {-# UNPACK #-} !Float
  , v3Z :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show, Generic)

-- | A four-component float vector for GPU uniforms and shader math.
data V4 = V4
  { v4X :: {-# UNPACK #-} !Float
  , v4Y :: {-# UNPACK #-} !Float
  , v4Z :: {-# UNPACK #-} !Float
  , v4W :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show, Generic)

-- | Primitive GPU field types supported inside std140 uniform blocks.
data GpuType
  = GFloat
  | GInt32
  | GUInt32
  | GBool
  | GVec2
  | GVec3
  | GVec4
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Std140 byte size for a primitive GPU type.
{-# INLINE gpuTypeSize #-}
gpuTypeSize :: GpuType -> Int
gpuTypeSize gt = case gt of
  GFloat -> 4
  GInt32 -> 4
  GUInt32 -> 4
  GBool -> 4
  GVec2 -> 8
  GVec3 -> 12
  GVec4 -> 16

-- | Round @off@ upward to the next multiple of @align@ (a power of two).
{-# INLINE alignUp #-}
alignUp :: Int -> Int -> Int
alignUp align off =
  let !rem' = off `rem` align
   in if rem' == 0 then off else off + (align - rem')

-- | One flattened field of a std140 uniform block.
data UniformField = UniformField
  { ufName :: !Text
  , ufType :: !GpuType
  , ufOffset :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- | Write one 32-bit word at a 4-byte-aligned byte offset.
{-# INLINE writeWord32At #-}
writeWord32At :: MutableByteArray RealWorld -> Int -> Word32 -> IO ()
writeWord32At mba byteOff w = writeByteArray mba (byteOff `shiftR` 2) w

-- | Write one 32-bit float at a 4-byte-aligned byte offset.
{-# INLINE writeFloatAt #-}
writeFloatAt :: MutableByteArray RealWorld -> Int -> Float -> IO ()
writeFloatAt mba byteOff f = writeWord32At mba byteOff (castFloatToWord32 f)

-- | Join a prefix and a field name without leading underscores.
joinFieldName :: Text -> Text -> Text
joinFieldName pfx name
  | T.null pfx = name
  | T.null name = pfx
  | otherwise = pfx <> "_" <> name

-- | Types that can be packed into a GPU std140 uniform block.
--
-- Product records derive 'Uniforms' automatically via 'Generic':
--
-- > data GlowParams = GlowParams
-- >   { glowColor  :: Color
-- >   , glowRadius :: Float
-- >   }
-- >   deriving (Generic, Eq)
-- >   deriving anyclass (Uniforms)
class Eq u => Uniforms u where
  -- | Flatten @u@ into std140 fields starting at byte offset @off@, returning
  -- the next byte offset and the fields in declaration order.
  uniformFieldsAt :: Proxy u -> Text -> Int -> (Int, [UniformField])
  default uniformFieldsAt ::
    GUniforms (Rep u) =>
    Proxy u -> Text -> Int -> (Int, [UniformField])
  uniformFieldsAt _ = gUniformFieldsAt (Proxy :: Proxy (Rep u))

  -- | Pack @u@ into @mba@ at byte offset @base + off@ (with @base@ 16-byte
  -- aligned), returning the next byte offset relative to @base@.
  pokeUniformAt :: u -> MutableByteArray RealWorld -> Int -> Int -> IO Int
  default pokeUniformAt ::
    (Generic u, GUniforms (Rep u)) =>
    u -> MutableByteArray RealWorld -> Int -> Int -> IO Int
  pokeUniformAt u mba base off = gPokeUniformAt (from u) mba base off

-- | Generic derivation helper for product records.
class GUniforms (f :: Type -> Type) where
  gUniformFieldsAt :: Proxy f -> Text -> Int -> (Int, [UniformField])
  gPokeUniformAt :: f p -> MutableByteArray RealWorld -> Int -> Int -> IO Int

instance GUniforms U1 where
  gUniformFieldsAt _ _ off = (off, [])
  gPokeUniformAt U1 _ _ off = pure off

instance GUniforms f => GUniforms (D1 d f) where
  gUniformFieldsAt _ = gUniformFieldsAt (Proxy :: Proxy f)
  gPokeUniformAt (M1 x) = gPokeUniformAt x

instance GUniforms f => GUniforms (C1 c f) where
  gUniformFieldsAt _ = gUniformFieldsAt (Proxy :: Proxy f)
  gPokeUniformAt (M1 x) = gPokeUniformAt x

instance (Selector s, GUniforms f) => GUniforms (S1 s f) where
  gUniformFieldsAt _ pfx off =
    let sname = T.pack (selName (undefined :: S1 s f ()))
     in gUniformFieldsAt (Proxy :: Proxy f) (joinFieldName pfx sname) off
  gPokeUniformAt (M1 x) = gPokeUniformAt x

instance (GUniforms f, GUniforms g) => GUniforms (f :*: g) where
  gUniformFieldsAt _ pfx off =
    let (off1, fs1) = gUniformFieldsAt (Proxy :: Proxy f) pfx off
        (off2, fs2) = gUniformFieldsAt (Proxy :: Proxy g) pfx off1
     in (off2, fs1 <> fs2)
  gPokeUniformAt (x :*: y) mba base off = do
    off1 <- gPokeUniformAt x mba base off
    gPokeUniformAt y mba base off1

instance Uniforms a => GUniforms (K1 i a) where
  gUniformFieldsAt _ = uniformFieldsAt (Proxy :: Proxy a)
  gPokeUniformAt (K1 x) = pokeUniformAt x

instance Uniforms () where
  uniformFieldsAt _ _ off = (off, [])
  pokeUniformAt () _ _ off = pure off

instance Uniforms Float where
  uniformFieldsAt _ name off =
    let !o = alignUp 4 off
     in (o + 4, [UniformField name GFloat o])
  pokeUniformAt f mba base off = do
    let !o = alignUp 4 off
    writeFloatAt mba (base + o) f
    pure (o + 4)

instance Uniforms Int32 where
  uniformFieldsAt _ name off =
    let !o = alignUp 4 off
     in (o + 4, [UniformField name GInt32 o])
  pokeUniformAt n mba base off = do
    let !o = alignUp 4 off
    writeWord32At mba (base + o) (fromIntegral n)
    pure (o + 4)

instance Uniforms Word32 where
  uniformFieldsAt _ name off =
    let !o = alignUp 4 off
     in (o + 4, [UniformField name GUInt32 o])
  pokeUniformAt w mba base off = do
    let !o = alignUp 4 off
    writeWord32At mba (base + o) w
    pure (o + 4)

instance Uniforms Bool where
  uniformFieldsAt _ name off =
    let !o = alignUp 4 off
     in (o + 4, [UniformField name GBool o])
  pokeUniformAt b mba base off = do
    let !o = alignUp 4 off
    writeWord32At mba (base + o) (if b then 1 else 0)
    pure (o + 4)

instance Uniforms V2 where
  uniformFieldsAt _ name off =
    let !o = alignUp 8 off
     in (o + 8, [UniformField name GVec2 o])
  pokeUniformAt (V2 x y) mba base off = do
    let !o = alignUp 8 off
    writeFloatAt mba (base + o) x
    writeFloatAt mba (base + o + 4) y
    pure (o + 8)

instance Uniforms V3 where
  uniformFieldsAt _ name off =
    let !o = alignUp 16 off
     in (o + 12, [UniformField name GVec3 o])
  pokeUniformAt (V3 x y z) mba base off = do
    let !o = alignUp 16 off
    writeFloatAt mba (base + o) x
    writeFloatAt mba (base + o + 4) y
    writeFloatAt mba (base + o + 8) z
    pure (o + 12)

instance Uniforms V4 where
  uniformFieldsAt _ name off =
    let !o = alignUp 16 off
     in (o + 16, [UniformField name GVec4 o])
  pokeUniformAt (V4 x y z w) mba base off = do
    let !o = alignUp 16 off
    writeFloatAt mba (base + o) x
    writeFloatAt mba (base + o + 4) y
    writeFloatAt mba (base + o + 8) z
    writeFloatAt mba (base + o + 12) w
    pure (o + 16)

instance Uniforms Color where
  uniformFieldsAt _ name off =
    let !o = alignUp 16 off
     in (o + 16, [UniformField name GVec4 o])
  pokeUniformAt (Color w) mba base off = do
    let !o = alignUp 16 off
        !inv = 1.0 / 255.0 :: Float
        !r = fromIntegral ((w `shiftR` 24) .&. 0xFF) * inv
        !g = fromIntegral ((w `shiftR` 16) .&. 0xFF) * inv
        !b = fromIntegral ((w `shiftR` 8) .&. 0xFF) * inv
        !a = fromIntegral (w .&. 0xFF) * inv
    writeFloatAt mba (base + o) r
    writeFloatAt mba (base + o + 4) g
    writeFloatAt mba (base + o + 8) b
    writeFloatAt mba (base + o + 12) a
    pure (o + 16)

instance Uniforms Rect where
  uniformFieldsAt _ name off =
    let !o = alignUp 16 off
     in (o + 16, [UniformField name GVec4 o])
  pokeUniformAt (Rect x y w h) mba base off = do
    let !o = alignUp 16 off
    writeFloatAt mba (base + o) x
    writeFloatAt mba (base + o + 4) y
    writeFloatAt mba (base + o + 8) w
    writeFloatAt mba (base + o + 12) h
    pure (o + 16)

instance Uniforms Size where
  uniformFieldsAt _ name off =
    let !o = alignUp 8 off
     in (o + 8, [UniformField name GVec2 o])
  pokeUniformAt (Size w h) mba base off = do
    let !o = alignUp 8 off
    writeFloatAt mba (base + o) w
    writeFloatAt mba (base + o + 4) h
    pure (o + 8)

instance (Uniforms a, Uniforms b) => Uniforms (a, b) where
  uniformFieldsAt _ pfx off =
    let (off1, fs1) = uniformFieldsAt (Proxy :: Proxy a) (joinFieldName pfx "a") off
        (off2, fs2) = uniformFieldsAt (Proxy :: Proxy b) (joinFieldName pfx "b") off1
     in (off2, fs1 <> fs2)
  pokeUniformAt (a, b) mba base off = do
    off1 <- pokeUniformAt a mba base off
    pokeUniformAt b mba base off1

instance (Uniforms a, Uniforms b, Uniforms c) => Uniforms (a, b, c) where
  uniformFieldsAt _ pfx off =
    let (off1, fs1) = uniformFieldsAt (Proxy :: Proxy a) (joinFieldName pfx "a") off
        (off2, fs2) = uniformFieldsAt (Proxy :: Proxy b) (joinFieldName pfx "b") off1
        (off3, fs3) = uniformFieldsAt (Proxy :: Proxy c) (joinFieldName pfx "c") off2
     in (off3, fs1 <> fs2 <> fs3)
  pokeUniformAt (a, b, c) mba base off = do
    off1 <- pokeUniformAt a mba base off
    off2 <- pokeUniformAt b mba base off1
    pokeUniformAt c mba base off2

-- | Flattened std140 field descriptors for uniform type @u@.
uniformLayout :: Uniforms u => Proxy u -> [UniformField]
uniformLayout p = snd (uniformFieldsAt p "" 0)

-- | Padded std140 byte size for uniform type @u@ (always a multiple of 16;
-- @0@ for @()@).
uniformSize :: Uniforms u => Proxy u -> Int
uniformSize p =
  let !raw = fst (uniformFieldsAt p "" 0)
   in if raw == 0 then 0 else alignUp 16 raw

-- | Pack @u@ into a zero-padded immutable std140 'ByteArray'.
packUniformBytes :: forall u. Uniforms u => u -> IO ByteArray
packUniformBytes u = do
  let !sz = uniformSize (Proxy :: Proxy u)
  mba <- newByteArray sz
  if sz > 0
    then do
      fillByteArray mba 0 sz 0
      _ <- pokeUniformAt u mba 0 0
      unsafeFreezeByteArray mba
    else unsafeFreezeByteArray mba

-- | Deterministic 64-bit FNV-1a hash over the 32-bit words of a packed
-- uniform 'ByteArray'. Because 'packUniformBytes' zeroes padding bytes,
-- equal uniform values produce identical hashes.
hashUniformBytes :: ByteArray -> Word64
hashUniformBytes ba = go 0 0xcbf29ce484222325
  where
    !wordsN = sizeofByteArray ba `shiftR` 2
    go !i !h
      | i >= wordsN = h
      | otherwise =
          let !w = indexByteArray ba i :: Word32
              !h' = (h `xor` fromIntegral w) * 0x100000001b3
           in go (i + 1) h'

-- | Size in bytes of the system uniform header prepended to every shader
-- uniform block: three @vec4@ values (@uRect@, @uUvRect@, @uMeta@).
sysUniformBytes :: Int
sysUniformBytes = 48

-- | Write the 48-byte system uniform header at byte offset @off@ (which must
-- be 4-byte aligned):
--
-- * bytes 0..15: @uRect@ (@nodeX, nodeY, nodeW, nodeH@) in logical pixels
-- * bytes 16..31: @uUvRect@ (@u0, v0, uSpan, vSpan@) in source target UVs
-- * bytes 32..47: @uMeta@ (@dpiScale, cornerRadius, targetW, targetH@)
pokeSysUniforms ::
  Rect ->
  Rect ->
  Float ->
  Float ->
  Float ->
  Float ->
  MutableByteArray RealWorld ->
  Int ->
  IO ()
pokeSysUniforms (Rect rx ry rw rh) (Rect u0 v0 us vs) scale radius tw th mba off = do
  writeFloatAt mba off rx
  writeFloatAt mba (off + 4) ry
  writeFloatAt mba (off + 8) rw
  writeFloatAt mba (off + 12) rh
  writeFloatAt mba (off + 16) u0
  writeFloatAt mba (off + 20) v0
  writeFloatAt mba (off + 24) us
  writeFloatAt mba (off + 28) vs
  writeFloatAt mba (off + 32) scale
  writeFloatAt mba (off + 36) radius
  writeFloatAt mba (off + 40) tw
  writeFloatAt mba (off + 44) th
