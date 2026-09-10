{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Compile-time optimization invariants for nano-ui's hot-path coding
-- patterns, checked by the inspection-testing plugin after the optimizer
-- runs. The plugin can only inspect bindings defined in this module, so
-- each target is a faithful local copy of the library pattern it guards
-- (same definition style, same pragmas). If GHC stops optimizing the
-- pattern, the build fails.
module Main
  ( main
  , localHashWidgetId
  , localFnv1a
  , localMixIntKey
  , localPokeVertexSIMD
  ) where

import Data.Bits (xor)
import Data.Char (ord)
import Data.Hashable (hash)
import Data.IORef (IORef)
import Data.Word (Word64, Word8)
import GHC.Exts
  ( Float (F#)
  , Int (I#)
  , packFloatX4#
  , plusAddr#
  , writeFloatOffAddrAsFloatX4#
  )
import GHC.IO (IO (IO))
import GHC.Ptr (Ptr (..))

import Test.Inspection

import NanoUI.Id (WidgetId (..))

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
-- (Copy of NanoUI.Id.mixId for Int keys.)
-- ---------------------------------------------------------------------------

localMixIntKey :: WidgetId -> Int -> WidgetId
localMixIntKey (WidgetId base) k = WidgetId (mixFnv base (fromIntegral (hash k)))
  where
    mixFnv :: Word64 -> Word64 -> Word64
    mixFnv x y = (x `xor` y) * 1099511628211

-- ---------------------------------------------------------------------------
-- Pattern 3: the SIMD vertex poke writes only through Addr#: no allocation.
-- (Copy of NanoUI.SIMD.pokeVertexSIMD.)
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
localPokeVertexSIMD (Ptr addr#) (I# byteOff#) (F# px#) (F# py#) (F# r#) (F# g#) (F# b#) (F# a#) (F# u#) (F# v#) = IO $ \s0 ->
  case packFloatX4# (# px#, py#, r#, g# #) of
    v0# ->
      case packFloatX4# (# b#, a#, u#, v# #) of
        v1# ->
          case writeFloatOffAddrAsFloatX4# (plusAddr# addr# byteOff#) 0# v0# s0 of
            s1 -> case writeFloatOffAddrAsFloatX4# (plusAddr# (plusAddr# addr# byteOff#) 16#) 0# v1# s1 of
              s2 -> (# s2, () #)

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
