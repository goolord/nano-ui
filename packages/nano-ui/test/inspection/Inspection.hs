{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time optimization invariants for nano-ui's hot-path coding
-- patterns, checked by the inspection-testing plugin after the optimizer
-- runs. The plugin can only inspect bindings defined in this module, so
-- probes exercise the actual inline SIMD writers. If GHC stops optimizing
-- them, the build fails.
module Main
  ( main
  , solidQuadProbe
  , gradientQuadProbe
  ) where

import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr)

import Test.Inspection

import NanoUI.SIMD qualified as SIMD

main :: IO ()
main = putStrLn "inspection invariants hold"

-- Inspect actual library calls, not copies of the quad writers. Dynamic
-- offsets, coordinates, colors and indices prevent a constant-only probe from
-- hiding boxing or a failure to inline the shared vertex writer.
solidQuadProbe :: Ptr Word8 -> Ptr Word8 -> Int -> Float -> Word32 -> IO ()
solidQuadProbe vp ip offset x base =
  SIMD.pokeQuadSIMD vp offset ip offset x x x x 0 0 1 1 x x x 1 base

gradientQuadProbe :: Ptr Word8 -> Ptr Word8 -> Int -> Float -> Word32 -> IO ()
gradientQuadProbe vp ip offset x base =
  SIMD.pokeQuadGradientSIMD vp offset ip offset x x x x 0 0
    (x, 0, 0, 1) (0, x, 0, 1) (0, 0, x, 1) (x, x, x, 1) base

inspect $ 'solidQuadProbe `doesNotUse` 'SIMD.pokeQuadSIMD
inspect $ 'solidQuadProbe `doesNotUse` 'SIMD.pokeVertexSIMD
inspect $ hasNoTypeClasses 'solidQuadProbe
inspect $ 'solidQuadProbe `hasNoType` ''[]
-- NoAllocation cannot be used here: on GHC 9.14 unboxed-tuple construction
-- appears as a datacon application, which the plugin (conservatively) counts
-- as allocation. Guard the pattern with type-level checks instead: no boxed
-- tuples/pairs may appear in the inlined poke body.
inspect $ 'solidQuadProbe `hasNoType` ''(,)
inspect $ 'gradientQuadProbe `doesNotUse` 'SIMD.pokeQuadGradientSIMD
inspect $ 'gradientQuadProbe `doesNotUse` 'SIMD.pokeVertexSIMD
inspect $ hasNoTypeClasses 'gradientQuadProbe
inspect $ 'gradientQuadProbe `hasNoType` ''(,,,)
