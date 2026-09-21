-- | Vertex and index writers for the draw buffers. Each vertex is written with
-- two 128-bit GHC SIMD stores (FloatX4#) instead of eight scalar stores.
module NanoUI.Internal.SIMD
  ( pokeVertexSIMD
  , pokeQuadSIMD
  , pokeQuadGradientSIMD
  , concentricOffsetsSIMD
  ) where

import GHC.Ptr (Ptr (..))
import Foreign.Storable (pokeByteOff)
import GHC.Exts
  ( Float (F#)
  , Int (I#)
  , packFloatX4#
  , packWord32X4#
  , plusAddr#
  , writeFloatOffAddrAsFloatX4#
  , writeWord32OffAddrAsWord32X4#
  )
import GHC.Word (Word32 (W32#))
import GHC.IO (IO (..))
import Data.Word (Word8)

-- | Writes one 32-byte Vertex (8 floats) into memory using two 128-bit SIMD stores
-- instead of 8 scalar stores.
{-# INLINE pokeVertexSIMD #-}
pokeVertexSIMD ::
  Ptr Word8 ->
  Int ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
pokeVertexSIMD (Ptr addr#) (I# byteOff#) (F# px#) (F# py#) (F# r#) (F# g#) (F# b#) (F# a#) (F# u#) (F# v#) = IO $ \s0 ->
  -- Offsets are recomputed inline (the address add is a single lea) so the
  -- simplified body stays free of let bindings; the inspection test guards
  -- this with a NoAllocation obligation.
  case packFloatX4# (# px#, py#, r#, g# #) of
    v0# ->
      case packFloatX4# (# b#, a#, u#, v# #) of
        v1# ->
          case writeFloatOffAddrAsFloatX4# (plusAddr# addr# byteOff#) 0# v0# s0 of
            s1 -> case writeFloatOffAddrAsFloatX4# (plusAddr# (plusAddr# addr# byteOff#) 16#) 0# v1# s1 of
              s2 -> (# s2, () #)

-- | Vectorized Quad Poking: writes 4 vertices (128 bytes total) and 6 indices (24 bytes total)
-- with SIMD vector stores.
{-# INLINE pokeQuadSIMD #-}
pokeQuadSIMD ::
  Ptr Word8 ->
  Int ->
  Ptr Word8 ->
  Int ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Word32 ->
  IO ()
pokeQuadSIMD vertices vOffset indices iOffset x y w h u0 v0 u1 v1 r g b a baseIdx = do
  let x1 = x + w
      y1 = y + h
  pokeVertexSIMD vertices vOffset x y r g b a u0 v0
  pokeVertexSIMD vertices (vOffset + 32) x1 y r g b a u1 v0
  pokeVertexSIMD vertices (vOffset + 64) x1 y1 r g b a u1 v1
  pokeVertexSIMD vertices (vOffset + 96) x y1 r g b a u0 v1
  pokeQuadIndicesSIMD indices iOffset baseIdx

-- Six indices form the same two triangles for both solid and gradient quads.
{-# INLINE pokeQuadIndicesSIMD #-}
pokeQuadIndicesSIMD :: Ptr Word8 -> Int -> Word32 -> IO ()
pokeQuadIndicesSIMD (Ptr addr#) offset@(I# offset#) baseIdx = do
  let !(W32# b0#) = baseIdx
      !(W32# b1#) = baseIdx + 1
      !(W32# b2#) = baseIdx + 2
      !idxVec# = packWord32X4# (# b0#, b1#, b2#, b0# #)
  IO $ \s0 ->
    case writeWord32OffAddrAsWord32X4# (plusAddr# addr# offset#) 0# idxVec# s0 of
      s1 -> (# s1, () #)
  pokeByteOff (Ptr addr#) (offset + 16) (baseIdx + 2)
  pokeByteOff (Ptr addr#) (offset + 20) (baseIdx + 3)

-- | Vectorized Quad with 4 distinct corner colors (top-left, top-right, bottom-right, bottom-left)
{-# INLINE pokeQuadGradientSIMD #-}
pokeQuadGradientSIMD ::
  Ptr Word8 ->
  Int ->
  Ptr Word8 ->
  Int ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  (Float, Float, Float, Float) ->
  (Float, Float, Float, Float) ->
  (Float, Float, Float, Float) ->
  (Float, Float, Float, Float) ->
  Word32 ->
  IO ()
pokeQuadGradientSIMD
  vertices vOffset indices iOffset x y w h u v
  (r0, g0, b0, a0) (r1, g1, b1, a1)
  (r2, g2, b2, a2) (r3, g3, b3, a3) baseIdx = do
  let x1 = x + w
      y1 = y + h
  pokeVertexSIMD vertices vOffset x y r0 g0 b0 a0 u v
  pokeVertexSIMD vertices (vOffset + 32) x1 y r1 g1 b1 a1 u v
  pokeVertexSIMD vertices (vOffset + 64) x1 y1 r2 g2 b2 a2 u v
  pokeVertexSIMD vertices (vOffset + 96) x y1 r3 g3 b3 a3 u v
  pokeQuadIndicesSIMD indices iOffset baseIdx

-- | Evaluates 4 concentric arc positions:
-- xs = cx + radii * ct
-- ys = cy + radii * st
--
-- Scalar on purpose: GHC 9.14.1 miscompiles the broadcast/pack/unpack FloatX4#
-- version at -O2 once it is inlined into a loop (liberate-case computed the y
-- lane from cx), corrupting anti-aliased border vertices. The results are
-- bit-identical to the vector version, which also multiplied and added separately.
{-# INLINE concentricOffsetsSIMD #-}
concentricOffsetsSIMD ::
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  ((Float, Float), (Float, Float), (Float, Float), (Float, Float))
concentricOffsetsSIMD cx cy ct st r0 r1 r2 r3 =
  ( (cx + r0 * ct, cy + r0 * st)
  , (cx + r1 * ct, cy + r1 * st)
  , (cx + r2 * ct, cy + r2 * st)
  , (cx + r3 * ct, cy + r3 * st)
  )
