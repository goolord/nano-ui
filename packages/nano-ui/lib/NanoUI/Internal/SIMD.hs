-- | Vertex and index writers for the draw buffers. They write with scalar
-- stores, which measured no slower than packed 128-bit ones.
module NanoUI.Internal.SIMD
  ( pokeVertexSIMD
  , pokeQuadSIMD
  , pokeQuadGradientSIMD
  , pokeQuadIndices
  , concentricOffsetsSIMD
  , pokeQuadCornersSIMD
  ) where

import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import Data.Word (Word32, Word8)

-- | Writes one 32-byte Vertex (8 floats: position, colour, uv) into memory.
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
pokeVertexSIMD vp off px py r g b a u v = do
  pokeByteOff vp off px
  pokeByteOff vp (off + 4) py
  pokeByteOff vp (off + 8) r
  pokeByteOff vp (off + 12) g
  pokeByteOff vp (off + 16) b
  pokeByteOff vp (off + 20) a
  pokeByteOff vp (off + 24) u
  pokeByteOff vp (off + 28) v

-- | Quad Poking: writes 4 vertices (128 bytes total) and 6 indices (24 bytes total).
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
pokeQuadSIMD vertices vOffset indices iOffset x y w h =
  let x1 = x + w
      y1 = y + h
   in pokeQuadCornersSIMD vertices vOffset indices iOffset x y x1 y x1 y1 x y1

-- | Quad with arbitrary corners, for non-axis-aligned quads: 4 vertices and
-- 6 indices. The corners map to the texture's top-left, top-right,
-- bottom-right and bottom-left, in that order.
{-# INLINE pokeQuadCornersSIMD #-}
pokeQuadCornersSIMD ::
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
  Float ->
  Float ->
  Float ->
  Float ->
  Word32 ->
  IO ()
pokeQuadCornersSIMD vertices vOffset indices iOffset x0 y0 x1 y1 x2 y2 x3 y3 u0 v0 u1 v1 r g b a baseIdx = do
  pokeVertexSIMD vertices vOffset x0 y0 r g b a u0 v0
  pokeVertexSIMD vertices (vOffset + 32) x1 y1 r g b a u1 v0
  pokeVertexSIMD vertices (vOffset + 64) x2 y2 r g b a u1 v1
  pokeVertexSIMD vertices (vOffset + 96) x3 y3 r g b a u0 v1
  pokeQuadIndices indices iOffset baseIdx (baseIdx + 1) (baseIdx + 2) (baseIdx + 3)

-- | Writes the six indices of quad @a b c d@ (corners in order) as the two
-- triangles @a b c@ and @a c d@, with scalar stores.
{-# INLINE pokeQuadIndices #-}
pokeQuadIndices :: Ptr Word8 -> Int -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
pokeQuadIndices ip off a b c d = do
  pokeByteOff ip off a
  pokeByteOff ip (off + 4) b
  pokeByteOff ip (off + 8) c
  pokeByteOff ip (off + 12) a
  pokeByteOff ip (off + 16) c
  pokeByteOff ip (off + 20) d

-- | Quad with 4 distinct corner colors (top-left, top-right, bottom-right, bottom-left)
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
  pokeQuadIndices indices iOffset baseIdx (baseIdx + 1) (baseIdx + 2) (baseIdx + 3)

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
