{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}

-- | SIMD vector acceleration primitives for geometry lowering and rendering.
-- Uses GHC native SIMD primops (FloatX4#, Word32X4#) under AVX/SSE for high-throughput
-- vector stores and arithmetic, with zero FFI overhead.
module NanoUI.SIMD
  ( SIMD4Float (..)
  , SIMD4Word32 (..)
  , broadcastSIMD4Float
  , packSIMD4Float
  , plusSIMD4Float
  , timesSIMD4Float
  , fmaSIMD4Float
  , writeAddrSIMD4Float
  , readAddrSIMD4Float
  , packSIMD4Word32
  , writeAddrSIMD4Word32
  , readAddrSIMD4Word32
  , pokeVertexSIMD
  , pokeQuadSIMD
  , pokeQuadGradientSIMD
  , concentricOffsetsSIMD
  , strokeStripNormalsSIMD
  ) where

import GHC.Ptr (Ptr (..))
import Foreign.Storable (pokeByteOff)
import GHC.Exts
  ( Addr#
  , Float (F#)
  , FloatX4#
  , Int (I#)
  , Int#
  , Word32X4#
  , plusFloat#
  , broadcastFloatX4#
  , packFloatX4#
  , packWord32X4#
  , plusAddr#
  , plusFloatX4#
  , readFloatOffAddrAsFloatX4#
  , readWord32OffAddrAsWord32X4#
  , timesFloatX4#
  , unpackFloatX4#
  , writeFloatOffAddrAsFloatX4#
  , writeWord32OffAddrAsWord32X4#
  )
import GHC.Word (Word32 (W32#))
import GHC.IO (IO (..))
import Data.Word (Word8)

-- | 4-wide 32-bit float vector.
data SIMD4Float = SIMD4Float FloatX4#

-- | 4-wide 32-bit unsigned integer vector.
data SIMD4Word32 = SIMD4Word32 Word32X4#

{-# INLINE broadcastSIMD4Float #-}
broadcastSIMD4Float :: Float -> SIMD4Float
broadcastSIMD4Float (F# f#) = SIMD4Float (broadcastFloatX4# f#)

{-# INLINE packSIMD4Float #-}
packSIMD4Float :: Float -> Float -> Float -> Float -> SIMD4Float
packSIMD4Float (F# a#) (F# b#) (F# c#) (F# d#) =
  SIMD4Float (packFloatX4# (# a#, b#, c#, d# #))

{-# INLINE plusSIMD4Float #-}
plusSIMD4Float :: SIMD4Float -> SIMD4Float -> SIMD4Float
plusSIMD4Float (SIMD4Float a#) (SIMD4Float b#) =
  SIMD4Float (plusFloatX4# a# b#)

{-# INLINE timesSIMD4Float #-}
timesSIMD4Float :: SIMD4Float -> SIMD4Float -> SIMD4Float
timesSIMD4Float (SIMD4Float a#) (SIMD4Float b#) =
  SIMD4Float (timesFloatX4# a# b#)

{-# INLINE fmaSIMD4Float #-}
-- | Computes a * b + c using vector operations.
fmaSIMD4Float :: SIMD4Float -> SIMD4Float -> SIMD4Float -> SIMD4Float
fmaSIMD4Float a b c = plusSIMD4Float (timesSIMD4Float a b) c

{-# INLINE writeAddrSIMD4Float #-}
writeAddrSIMD4Float :: Addr# -> Int# -> SIMD4Float -> IO ()
writeAddrSIMD4Float addr# off# (SIMD4Float v#) = IO $ \s0 ->
  case writeFloatOffAddrAsFloatX4# addr# off# v# s0 of
    s1 -> (# s1, () #)

{-# INLINE readAddrSIMD4Float #-}
readAddrSIMD4Float :: Addr# -> Int# -> IO SIMD4Float
readAddrSIMD4Float addr# off# = IO $ \s0 ->
  case readFloatOffAddrAsFloatX4# addr# off# s0 of
    (# s1, v# #) -> (# s1, SIMD4Float v# #)

{-# INLINE packSIMD4Word32 #-}
packSIMD4Word32 :: Word32 -> Word32 -> Word32 -> Word32 -> SIMD4Word32
packSIMD4Word32 (W32# a#) (W32# b#) (W32# c#) (W32# d#) =
  SIMD4Word32 (packWord32X4# (# a#, b#, c#, d# #))

{-# INLINE writeAddrSIMD4Word32 #-}
writeAddrSIMD4Word32 :: Addr# -> Int# -> SIMD4Word32 -> IO ()
writeAddrSIMD4Word32 addr# off# (SIMD4Word32 v#) = IO $ \s0 ->
  case writeWord32OffAddrAsWord32X4# addr# off# v# s0 of
    s1 -> (# s1, () #)

{-# INLINE readAddrSIMD4Word32 #-}
readAddrSIMD4Word32 :: Addr# -> Int# -> IO SIMD4Word32
readAddrSIMD4Word32 addr# off# = IO $ \s0 ->
  case readWord32OffAddrAsWord32X4# addr# off# s0 of
    (# s1, v# #) -> (# s1, SIMD4Word32 v# #)

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
  let !target# = addr# `plusAddr#` byteOff#
      !v0# = packFloatX4# (# px#, py#, r#, g# #)
      !v1# = packFloatX4# (# b#, a#, u#, v# #)
   in case writeFloatOffAddrAsFloatX4# target# 0# v0# s0 of
        s1 -> case writeFloatOffAddrAsFloatX4# (target# `plusAddr#` 16#) 0# v1# s1 of
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
pokeQuadSIMD (Ptr vAddr#) (I# vOff#) (Ptr iAddr#) (I# iOff#) (F# x#) (F# y#) (F# w#) (F# h#) (F# u0#) (F# v0#) (F# u1#) (F# v1#) (F# r#) (F# g#) (F# b#) (F# a#) !baseIdx = do
  let !baseV# = vAddr# `plusAddr#` vOff#
      !baseI# = iAddr# `plusAddr#` iOff#
      !x1# = x# `plusFloat#` w#
      !y1# = y# `plusFloat#` h#

  -- Write 4 vertices (each vertex is 2x FloatX4# = 32 bytes)
  IO $ \s0 ->
    -- V0: (x, y, r, g) | (b, a, u0, v0)
    let !v0_lo# = packFloatX4# (# x#, y#, r#, g# #)
        !v0_hi# = packFloatX4# (# b#, a#, u0#, v0# #)
     in case writeFloatOffAddrAsFloatX4# baseV# 0# v0_lo# s0 of
          s1 -> case writeFloatOffAddrAsFloatX4# (baseV# `plusAddr#` 16#) 0# v0_hi# s1 of
            s2 ->
              -- V1: (x1, y, r, g) | (b, a, u1, v0)
              let !v1_lo# = packFloatX4# (# x1#, y#, r#, g# #)
                  !v1_hi# = packFloatX4# (# b#, a#, u1#, v0# #)
                  !v1_addr# = baseV# `plusAddr#` 32#
               in case writeFloatOffAddrAsFloatX4# v1_addr# 0# v1_lo# s2 of
                    s3 -> case writeFloatOffAddrAsFloatX4# (v1_addr# `plusAddr#` 16#) 0# v1_hi# s3 of
                      s4 ->
                        -- V2: (x1, y1, r, g) | (b, a, u1, v1)
                        let !v2_lo# = packFloatX4# (# x1#, y1#, r#, g# #)
                            !v2_hi# = packFloatX4# (# b#, a#, u1#, v1# #)
                            !v2_addr# = baseV# `plusAddr#` 64#
                         in case writeFloatOffAddrAsFloatX4# v2_addr# 0# v2_lo# s4 of
                              s5 -> case writeFloatOffAddrAsFloatX4# (v2_addr# `plusAddr#` 16#) 0# v2_hi# s5 of
                                s6 ->
                                  -- V3: (x, y1, r, g) | (b, a, u0, v1)
                                  let !v3_lo# = packFloatX4# (# x#, y1#, r#, g# #)
                                      !v3_hi# = packFloatX4# (# b#, a#, u0#, v1# #)
                                      !v3_addr# = baseV# `plusAddr#` 96#
                                   in case writeFloatOffAddrAsFloatX4# v3_addr# 0# v3_lo# s6 of
                                        s7 -> case writeFloatOffAddrAsFloatX4# (v3_addr# `plusAddr#` 16#) 0# v3_hi# s7 of
                                          s8 -> (# s8, () #)

  -- Write 6 quad indices: base, base+1, base+2, base, base+2, base+3
  -- First 4 indices in one 128-bit vector store
  let !(W32# b0#) = baseIdx
      !(W32# b1#) = baseIdx + 1
      !(W32# b2#) = baseIdx + 2
      !idxVec# = packWord32X4# (# b0#, b1#, b2#, b0# #)
  IO $ \s0 ->
    case writeWord32OffAddrAsWord32X4# baseI# 0# idxVec# s0 of
      s1 -> (# s1, () #)
  let !iOffInt = I# iOff#
  pokeByteOff (Ptr iAddr#) (iOffInt + 16) (baseIdx + 2)
  pokeByteOff (Ptr iAddr#) (iOffInt + 20) (baseIdx + 3)

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
  (Ptr vAddr#) (I# vOff#) (Ptr iAddr#) (I# iOff#)
  (F# x#) (F# y#) (F# w#) (F# h#)
  (F# u#) (F# v#)
  (F# r0#, F# g0#, F# b0#, F# a0#)
  (F# r1#, F# g1#, F# b1#, F# a1#)
  (F# r2#, F# g2#, F# b2#, F# a2#)
  (F# r3#, F# g3#, F# b3#, F# a3#)
  !baseIdx = do
  let !baseV# = vAddr# `plusAddr#` vOff#
      !x1# = x# `plusFloat#` w#
      !y1# = y# `plusFloat#` h#

  IO $ \s0 ->
    let !v0_lo# = packFloatX4# (# x#, y#, r0#, g0# #)
        !v0_hi# = packFloatX4# (# b0#, a0#, u#, v# #)
        !v1_lo# = packFloatX4# (# x1#, y#, r1#, g1# #)
        !v1_hi# = packFloatX4# (# b1#, a1#, u#, v# #)
        !v2_lo# = packFloatX4# (# x1#, y1#, r2#, g2# #)
        !v2_hi# = packFloatX4# (# b2#, a2#, u#, v# #)
        !v3_lo# = packFloatX4# (# x#, y1#, r3#, g3# #)
        !v3_hi# = packFloatX4# (# b3#, a3#, u#, v# #)
        !v1_addr# = baseV# `plusAddr#` 32#
        !v2_addr# = baseV# `plusAddr#` 64#
        !v3_addr# = baseV# `plusAddr#` 96#
     in case writeFloatOffAddrAsFloatX4# baseV# 0# v0_lo# s0 of
          s1 -> case writeFloatOffAddrAsFloatX4# (baseV# `plusAddr#` 16#) 0# v0_hi# s1 of
            s2 -> case writeFloatOffAddrAsFloatX4# v1_addr# 0# v1_lo# s2 of
              s3 -> case writeFloatOffAddrAsFloatX4# (v1_addr# `plusAddr#` 16#) 0# v1_hi# s3 of
                s4 -> case writeFloatOffAddrAsFloatX4# v2_addr# 0# v2_lo# s4 of
                  s5 -> case writeFloatOffAddrAsFloatX4# (v2_addr# `plusAddr#` 16#) 0# v2_hi# s5 of
                    s6 -> case writeFloatOffAddrAsFloatX4# v3_addr# 0# v3_lo# s6 of
                      s7 -> case writeFloatOffAddrAsFloatX4# (v3_addr# `plusAddr#` 16#) 0# v3_hi# s7 of
                        s8 -> (# s8, () #)

  let !(W32# q0#) = baseIdx
      !(W32# q1#) = baseIdx + 1
      !(W32# q2#) = baseIdx + 2
      !baseI# = iAddr# `plusAddr#` iOff#
      !idxVec# = packWord32X4# (# q0#, q1#, q2#, q0# #)
  IO $ \s0 ->
    case writeWord32OffAddrAsWord32X4# baseI# 0# idxVec# s0 of
      s1 -> (# s1, () #)
  let !iOffInt = I# iOff#
  pokeByteOff (Ptr iAddr#) (iOffInt + 16) (baseIdx + 2)
  pokeByteOff (Ptr iAddr#) (iOffInt + 20) (baseIdx + 3)

-- | Evaluates 4 concentric arc positions in parallel using vector math:
-- xs = cx + radii * ct
-- ys = cy + radii * st
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
concentricOffsetsSIMD
  (F# cx#) (F# cy#) (F# ct#) (F# st#)
  (F# r0#) (F# r1#) (F# r2#) (F# r3#) =
  let !cxVec# = broadcastFloatX4# cx#
      !cyVec# = broadcastFloatX4# cy#
      !ctVec# = broadcastFloatX4# ct#
      !stVec# = broadcastFloatX4# st#
      !radii# = packFloatX4# (# r0#, r1#, r2#, r3# #)
      !xs# = plusFloatX4# cxVec# (timesFloatX4# radii# ctVec#)
      !ys# = plusFloatX4# cyVec# (timesFloatX4# radii# stVec#)
   in case unpackFloatX4# xs# of
        (# x0#, x1#, x2#, x3# #) ->
          case unpackFloatX4# ys# of
            (# y0#, y1#, y2#, y3# #) ->
              ( (F# x0#, F# y0#)
              , (F# x1#, F# y1#)
              , (F# x2#, F# y2#)
              , (F# x3#, F# y3#)
              )

-- | Evaluates 4 coverage strip normal offsets in parallel:
-- xs = px + nx * offsets
-- ys = py + ny * offsets
{-# INLINE strokeStripNormalsSIMD #-}
strokeStripNormalsSIMD ::
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  ((Float, Float), (Float, Float), (Float, Float), (Float, Float))
strokeStripNormalsSIMD
  (F# px#) (F# py#) (F# nx#) (F# ny#)
  (F# o0#) (F# o1#) (F# o2#) (F# o3#) =
  let !pxVec# = broadcastFloatX4# px#
      !pyVec# = broadcastFloatX4# py#
      !nxVec# = broadcastFloatX4# nx#
      !nyVec# = broadcastFloatX4# ny#
      !offs# = packFloatX4# (# o0#, o1#, o2#, o3# #)
      !xs# = plusFloatX4# pxVec# (timesFloatX4# nxVec# offs#)
      !ys# = plusFloatX4# pyVec# (timesFloatX4# nyVec# offs#)
   in case unpackFloatX4# xs# of
        (# x0#, x1#, x2#, x3# #) ->
          case unpackFloatX4# ys# of
            (# y0#, y1#, y2#, y3# #) ->
              ( (F# x0#, F# y0#)
              , (F# x1#, F# y1#)
              , (F# x2#, F# y2#)
              , (F# x3#, F# y3#)
              )
