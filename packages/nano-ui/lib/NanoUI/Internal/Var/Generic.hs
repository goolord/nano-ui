{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Reading and replacing one record field through "GHC.Generics", for the
-- @v.field@ focus on a 'NanoUI.Var.Var'. The path to the field is found at
-- compile time, so with optimisation a focus compiles to a plain selector
-- and record update.
module NanoUI.Internal.Var.Generic
  ( Side (..)
  , FieldPath
  , GFocus (..)
  , PathIx (..)
  )
where

import Data.Kind (Type)
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)

-- | Which half of a product the field is in.
data Side = SL | SR

-- | The path to field @x@ in the representation of record @s@.
type family FieldPath (x :: Symbol) s (rep :: Type -> Type) :: [Side] where
  FieldPath x s (D1 _ (C1 _ f)) = Found x s (Search x f)
  FieldPath x s (D1 _ _) =
    TypeError
      ( 'Text "Cannot focus a Var on field ‘" ':<>: 'Text x ':<>: 'Text "’ of " ':<>: 'ShowType s ':<>: 'Text ":"
          ':$$: 'Text "only a type with exactly one constructor can be focused by field."
      )

type family Search (x :: Symbol) (f :: Type -> Type) :: Maybe [Side] where
  Search x (S1 ('MetaSel ('Just x) _ _ _) _) = 'Just '[]
  Search x (S1 _ _) = 'Nothing
  Search x (l :*: r) = Pick (Search x l) (Search x r)
  Search x U1 = 'Nothing

type family Pick (l :: Maybe [Side]) (r :: Maybe [Side]) :: Maybe [Side] where
  Pick ('Just p) _ = 'Just ('SL ': p)
  Pick 'Nothing ('Just p) = 'Just ('SR ': p)
  Pick 'Nothing 'Nothing = 'Nothing

type family Found (x :: Symbol) s (m :: Maybe [Side]) :: [Side] where
  Found _ _ ('Just p) = p
  Found x s 'Nothing =
    TypeError ('Text "Cannot focus a Var on field ‘" ':<>: 'Text x ':<>: 'Text "’: " ':<>: 'ShowType s ':<>: 'Text " has no such field.")

-- | Get and set the field at path @p@ of a representation.
class GFocus (p :: [Side]) (f :: Type -> Type) a | p f -> a where
  gget :: f x -> a
  gset :: a -> f x -> f x

instance GFocus '[] (M1 S m (K1 i a)) a where
  {-# INLINE gget #-}
  gget (M1 (K1 a)) = a
  {-# INLINE gset #-}
  gset a _ = M1 (K1 a)

instance GFocus p f a => GFocus p (M1 D m (M1 C c f)) a where
  {-# INLINE gget #-}
  gget (M1 (M1 f)) = gget @p f
  {-# INLINE gset #-}
  gset a (M1 (M1 f)) = M1 (M1 (gset @p a f))

instance GFocus p l a => GFocus ('SL ': p) (l :*: r) a where
  {-# INLINE gget #-}
  gget (l :*: _) = gget @p l
  {-# INLINE gset #-}
  gset a (l :*: r) = gset @p a l :*: r

instance GFocus p r a => GFocus ('SR ': p) (l :*: r) a where
  {-# INLINE gget #-}
  gget (_ :*: r) = gget @p r
  {-# INLINE gset #-}
  gset a (l :*: r) = l :*: gset @p a r

-- | A number for a path, unique among the fields of one record: the path's
-- sides read as binary digits after a leading one.
class PathIx (p :: [Side]) where
  pathIx :: Int -> Int

instance PathIx '[] where
  {-# INLINE pathIx #-}
  pathIx acc = acc

instance PathIx p => PathIx ('SL ': p) where
  {-# INLINE pathIx #-}
  pathIx acc = pathIx @p (2 * acc)

instance PathIx p => PathIx ('SR ': p) where
  {-# INLINE pathIx #-}
  pathIx acc = pathIx @p (2 * acc + 1)
