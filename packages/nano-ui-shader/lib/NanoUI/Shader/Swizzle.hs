{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type-level vector swizzling for 'OverloadedRecordDot' (@v.xy@, @col.bgr@,
-- @v.xyxy@) using GHC 9.10+ 'GHC.TypeLits.UnconsSymbol' and 'HasField', with
-- custom compile-time 'TypeError' diagnostics and zero Template Haskell.
module NanoUI.Shader.Swizzle
  ( VecDim
  , SwizzleIndices
  , SwizzleResult
  , KnownIndices (..)
  , Swizzle (..)
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits
  ( ErrorMessage (..)
  , KnownNat
  , Nat
  , Symbol
  , TypeError
  , UnconsSymbol
  , natVal
  , type (<=?)
  )
import NanoUI (V2)
import NanoUI.Internal.Uniforms (V3, V4)

-- | Component dimension of a GPU vector type.
type family VecDim (v :: Type) :: Nat where
  VecDim V2 = 2
  VecDim V3 = 3
  VecDim V4 = 4
  VecDim t =
    TypeError
      ( 'Text "Cannot swizzle value of type "
          ':<>: 'ShowType t
          ':<>: 'Text " (expected V2, V3, or V4)."
      )

-- | Map a swizzle character to @(componentIndex, channelGroup)@, where group 0
-- is @xyzw@ and group 1 is @rgba@.
type family CharComp (c :: Char) :: (Nat, Nat) where
  CharComp 'x' = '(0, 0)
  CharComp 'y' = '(1, 0)
  CharComp 'z' = '(2, 0)
  CharComp 'w' = '(3, 0)
  CharComp 'r' = '(0, 1)
  CharComp 'g' = '(1, 1)
  CharComp 'b' = '(2, 1)
  CharComp 'a' = '(3, 1)
  CharComp c =
    TypeError
      ( 'Text "Invalid swizzle component '"
          ':<>: 'ShowType c
          ':<>: 'Text "' (expected one of x, y, z, w, r, g, b, a)."
      )

-- | Bounds-check a component index against the source vector dimension.
type family CheckBound (dim :: Nat) (idx :: Nat) (c :: Char) :: Nat where
  CheckBound dim idx c = CheckBoundGo (idx <=? dim) dim idx c

type family CheckBoundGo (ok :: Bool) (dim :: Nat) (idx :: Nat) (c :: Char) :: Nat where
  CheckBoundGo 'True dim idx c =
    IfEq idx dim
      ( TypeError
          ( 'Text "Swizzle component '"
              ':<>: 'ShowType c
              ':<>: 'Text "' is out of range for a "
              ':<>: 'ShowType dim
              ':<>: 'Text "-component vector."
          )
      )
      idx
  CheckBoundGo 'False dim _ c =
    TypeError
      ( 'Text "Swizzle component '"
          ':<>: 'ShowType c
          ':<>: 'Text "' is out of range for a "
          ':<>: 'ShowType dim
          ':<>: 'Text "-component vector."
      )

type family IfEq (a :: Nat) (b :: Nat) (yes :: k) (no :: k) :: k where
  IfEq a a yes _ = yes
  IfEq _ _ _ no = no

-- | Ensure all components in a swizzle come from the same channel set
-- (@xyzw@ or @rgba@).
type family CheckGroup (expected :: Nat) (actual :: Nat) (s :: Symbol) :: () where
  CheckGroup g g _ = '()
  CheckGroup _ _ s =
    TypeError
      ( 'Text "Swizzle \""
          ':<>: 'Text s
          ':<>: 'Text "\" mixes xyzw and rgba component sets."
      )

-- | Parse a swizzle 'Symbol' into a list of 1..4 validated component indices.
type family SwizzleIndices (dim :: Nat) (s :: Symbol) :: [Nat] where
  SwizzleIndices dim s = ParseStart dim s (UnconsSymbol s)

type family ParseStart (dim :: Nat) (full :: Symbol) (m :: Maybe (Char, Symbol)) :: [Nat] where
  ParseStart _ full 'Nothing =
    TypeError ('Text "Empty swizzle: " ':<>: 'ShowType full)
  ParseStart dim full ('Just '(c, rest)) =
    ParsePair dim full c (CharComp c) rest

type family ParsePair (dim :: Nat) (full :: Symbol) (c :: Char) (p :: (Nat, Nat)) (rest :: Symbol) :: [Nat] where
  ParsePair dim full c '(idx, grp) rest =
    CheckBound dim idx c ': ParseRest dim full grp (UnconsSymbol rest)

type family ParseRest (dim :: Nat) (full :: Symbol) (grp :: Nat) (m :: Maybe (Char, Symbol)) :: [Nat] where
  ParseRest _ _ _ 'Nothing = '[]
  ParseRest dim full grp ('Just '(c, rest)) =
    ParseRestPair dim full grp c (CharComp c) rest

type family ParseRestPair (dim :: Nat) (full :: Symbol) (grp :: Nat) (c :: Char) (p :: (Nat, Nat)) (rest :: Symbol) :: [Nat] where
  ParseRestPair dim full grp c '(idx, g2) rest =
    SeqUnit (CheckGroup grp g2 full) (CheckBound dim idx c ': ParseRest dim full grp (UnconsSymbol rest))

type family SeqUnit (u :: ()) (x :: k) :: k where
  SeqUnit '() x = x

-- | Result vector/scalar type for a parsed swizzle index list of length 1..4.
type family SwizzleResult (xs :: [Nat]) :: Type where
  SwizzleResult '[_] = Float
  SwizzleResult '[_, _] = V2
  SwizzleResult '[_, _, _] = V3
  SwizzleResult '[_, _, _, _] = V4
  SwizzleResult xs =
    TypeError
      ( 'Text "Swizzle must select 1 to 4 components (got "
          ':<>: 'ShowType xs
          ':<>: 'Text ")."
      )

-- | Reify a type-level list of component indices to runtime @[Int]@.
class KnownIndices (xs :: [Nat]) where
  indicesVal :: Proxy xs -> [Int]

instance KnownIndices '[] where
  indicesVal _ = []

instance (KnownNat x, KnownIndices xs) => KnownIndices (x ': xs) where
  indicesVal _ = fromIntegral (natVal (Proxy @x)) : indicesVal (Proxy @xs)

-- | Helper constraint bundling swizzle validation, result type, and reification.
class (r ~ SwizzleResult (SwizzleIndices (VecDim v) s), KnownIndices (SwizzleIndices (VecDim v) s)) =>
  Swizzle (s :: Symbol) (v :: Type) (r :: Type) where
  swizzleIndices :: Proxy s -> Proxy v -> [Int]

instance
  ( xs ~ SwizzleIndices (VecDim v) s
  , r ~ SwizzleResult xs
  , KnownIndices xs
  ) =>
  Swizzle s v r
  where
  swizzleIndices _ _ = indicesVal (Proxy @xs)
