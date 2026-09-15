{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time optimization invariants for nano-ui's hot-path coding
-- patterns, checked by the inspection-testing plugin after the optimizer
-- runs. The plugin can only inspect bindings defined in this module, so
-- probes exercise the actual inline SIMD writers and representative private
-- hashing patterns. If GHC stops optimizing them, the build fails.
module Main
  ( main
  , localHashWidgetId
  , localFnv1a
  , localMixIntKey
  , localPokeVertexSIMD
  , solidQuadProbe
  , gradientQuadProbe
  ) where

import Data.Bits (xor)
import Data.Char (ord)
import Data.Hashable (hash)
import Data.IORef (IORef)
import Data.Word (Word32, Word64, Word8)
import Foreign.Ptr (Ptr)

import Test.Inspection

import NanoUI.Id (WidgetId (..))
import NanoUI.SIMD qualified as SIMD

main :: IO ()
main = putStrLn "inspection invariants hold"

-- ---------------------------------------------------------------------------
-- Pattern 1: FNV-1a widget-id hashing must not retain class dictionaries.
-- (Copy of NanoUI.Id.hashWidgetId/fnv1a coding pattern.)
-- ---------------------------------------------------------------------------

localHashWidgetId :: WidgetId -> Word64
localHashWidgetId (WidgetId w) = w

localFnv1a :: String -> Word64
localFnv1a s = go 0xcbf29ce484222325 s
  where
    go !acc [] = acc
    go !acc (c : cs) =
      let !acc' = (fromIntegral (ord c) `xor` acc) * 0x00000100000001B3
       in go acc' cs

-- ---------------------------------------------------------------------------
-- Pattern 2: polymorphic key mixing must specialize away its Hashable dict.
-- (Copy of the widget-id key-mixing pattern, 'mixFnv' over a hashed key, for Int keys.)
-- ---------------------------------------------------------------------------

localMixIntKey :: WidgetId -> Int -> WidgetId
localMixIntKey (WidgetId base) k = WidgetId (mixFnv base (fromIntegral (hash k)))
  where
    mixFnv :: Word64 -> Word64 -> Word64
    mixFnv x y = (x `xor` y) * 1099511628211

-- ---------------------------------------------------------------------------
-- Pattern 3: the actual SIMD vertex writer must inline without boxed tuples.
-- ---------------------------------------------------------------------------

localPokeVertexSIMD ::
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
localPokeVertexSIMD ptr offset x y r g b a u v =
  -- A real call-site offset prevents GHC from replacing this probe with a
  -- top-level alias, which would make the type checks vacuous.
  SIMD.pokeVertexSIMD ptr (offset + 32) x y r g b a u v

-- ---------------------------------------------------------------------------
-- Obligations
-- ---------------------------------------------------------------------------

inspect $ hasNoTypeClasses 'localHashWidgetId
inspect $ hasNoTypeClasses 'localFnv1a
inspect $ hasNoTypeClasses 'localMixIntKey
-- NoAllocation cannot be used here: on GHC 9.14 unboxed-tuple construction
-- appears as a datacon application, which the plugin (conservatively) counts
-- as allocation. Guard the pattern with type-level checks instead: no boxed
-- tuples/pairs may appear in the poke body.
inspect $ 'localPokeVertexSIMD `hasNoType` ''(,)
inspect $ 'localPokeVertexSIMD `hasNoType` ''Data.IORef.IORef
inspect $ 'localPokeVertexSIMD `doesNotUse` 'SIMD.pokeVertexSIMD

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
inspect $ 'gradientQuadProbe `doesNotUse` 'SIMD.pokeQuadGradientSIMD
inspect $ 'gradientQuadProbe `doesNotUse` 'SIMD.pokeVertexSIMD
inspect $ hasNoTypeClasses 'gradientQuadProbe
inspect $ 'gradientQuadProbe `hasNoType` ''(,,,)
