{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The 'Var' handle: a constant, or a cell plus a path into its value, with
-- the paths' reads and writes, the list-element index, and the positional
-- slot types. The view's reads and writes, and what they cost, live in
-- "NanoUI.Internal.Var".
module NanoUI.Internal.Var.Types
  ( Var (..)
  , Lens (..)
  , ElemCtx (..)
  , viewLens
  , overLens
  , peekLens
  , zoomVar
  , zoomVarL
  , fieldLens
  , newElemCtx
  , VarSlot (..)
  , SlotVar (..)
  )
where

import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics (Generic (..))
import GHC.Records (HasField (..))
import GHC.TypeLits (Symbol)
import NanoUI.Internal.Derived (SortCol)
import NanoUI.Internal.Equality (ptrEq)
import NanoUI.Internal.Var.Cell (Cell)
import NanoUI.Internal.Var.Generic (FieldPath, GFocus (..))

-- | State a widget can show and edit: a value held in a cell, or a constant.
--
-- * 'NanoUI.Var.newVar' and 'NanoUI.Var.useVar' make Vars that keep what
--   widgets write to them.
-- * @'Var' x@ is a constant. A widget given one shows @x@, and an edit to it
--   is gone on the next frame. A string literal is a constant too, so
--   @textInput "draft"@ works as written; a number is wrapped, as in
--   @slider 0 1 (Var 0.5)@.
--
-- @v.field@ focuses a Var on one field of its record, for any record with
-- one constructor and a 'Generic' instance.
data Var a where
  -- | A constant.
  Var :: a -> Var a
  VarRef :: {-# UNPACK #-} !(Cell s) -> !(Lens s a) -> Var a

-- | The path from a cell's value to the part a Var shows.
data Lens s a where
  Here :: Lens a a
  Field :: !(Lens s b) -> (b -> a) -> (a -> b -> b) -> Lens s a
  Elem :: !(ElemCtx s k a) -> !k -> a -> Lens s a

-- | One list's elements, found by key: the list as it was when the elements
-- were handed out, and an index of the current list built on first need.
data ElemCtx s k a where
  ElemCtx :: Hashable k => !(Lens s [a]) -> (a -> k) -> [a] -> !(IORef (Maybe ([a], HM.HashMap k a))) -> ElemCtx s k a

newElemCtx :: Hashable k => Lens s [a] -> (a -> k) -> [a] -> IO (ElemCtx s k a)
newElemCtx l key xs = ElemCtx l key xs <$> newIORef Nothing

-- | The part of @s@ a lens shows. An element whose key has left the list
-- shows the value it had when it was handed out.
viewLens :: Lens s a -> s -> a
viewLens Here s = s
viewLens (Field l g _) s = g (viewLens l s)
viewLens (Elem (ElemCtx l key _ _) k snap) s =
  fromMaybe snap (find ((== k) . key) (viewLens l s))

-- | Change the part of @s@ a lens shows. Changing an element whose key has
-- left the list changes nothing. Keys are unique, so only the elements
-- before the changed one are copied.
overLens :: Lens s a -> (a -> a) -> s -> s
overLens Here f s = f s
overLens (Field l g set) f s = overLens l (\b -> let !a = f (g b) in set a b) s
overLens (Elem (ElemCtx l key _ _) k _) f s = overLens l update s
  where
    update xs = case break ((== k) . key) xs of
      (before, x : after) -> let !y = f x in before ++ y : after
      (_, []) -> xs

-- | 'viewLens', using an element's index instead of a linear search: the
-- snapshot when the list is the one handed out, else an index of the
-- current list, built once per list. Parts are returned evaluated, so the
-- list compares by pointer with the one handed out.
peekLens :: Lens s a -> s -> IO a
peekLens Here s = pure s
peekLens (Field l g _) s = do
  b <- peekLens l s
  pure $! g b
peekLens (Elem (ElemCtx l key xs0 cache) k snap) s = do
  !xs <- peekLens l s
  if ptrEq xs xs0
    then pure snap
    else do
      cached <- readIORef cache
      index <- case cached of
        Just (ys, ix) | ptrEq ys xs -> pure ix
        _ -> do
          -- The first of equal keys wins, as in 'viewLens'.
          let !ix = HM.fromListWith (\_ old -> old) [(key x, x) | x <- xs]
          writeIORef cache (Just (xs, ix))
          pure ix
      pure (HM.findWithDefault snap k index)

-- | Focus a Var on part of its value, given how to read the part and how to
-- replace it. Writes through the result change that part of the whole.
--
-- > draft <- zoomVar fst (\a (_, b) -> (a, b)) pair
{-# INLINE zoomVar #-}
zoomVar :: (s -> a) -> (a -> s -> s) -> Var s -> Var a
zoomVar g _ (Var s) = Var (g s)
zoomVar g set (VarRef cell l) = VarRef cell (Field l g set)

-- | 'zoomVar' from a van Laarhoven lens, such as one from @lens@ or
-- @microlens@.
{-# INLINE zoomVarL #-}
zoomVarL :: (forall f. Functor f => (a -> f a) -> s -> f s) -> Var s -> Var a
zoomVarL l = zoomVar (getConst . l Const) (\a -> runIdentity . l (\_ -> Identity a))

instance (Generic s, GFocus (FieldPath x s (Rep s)) (Rep s) a) => HasField (x :: Symbol) (Var s) (Var a) where
  {-# INLINE getField #-}
  getField = uncurry zoomVar (fieldLens @x)

-- | How to read field @x@ of @s@ and how to replace it, found through
-- "GHC.Generics"; with optimisation, a selector and a record update.
{-# INLINE fieldLens #-}
fieldLens :: forall x s a. (Generic s, GFocus (FieldPath x s (Rep s)) (Rep s) a) => (s -> a, a -> s -> s)
fieldLens = (gget @(FieldPath x s (Rep s)) . from, \a -> to . gset @(FieldPath x s (Rep s)) a . from)

-- | A string literal is a constant.
instance IsString a => IsString (Var a) where
  fromString = Var . fromString

-- | A positional Var as the slot table keeps it, tagged with its type so a
-- slot that changes type starts over instead of being misread.
data SlotVar
  = SInt !(Var Int)
  | SDouble !(Var Double)
  | SFloat !(Var Float)
  | SBool !(Var Bool)
  | SText !(Var Text)
  | SEnum !(Var Int)
  | SSort !(Var SortCol)

-- | Types a positional 'NanoUI.Var.useVar' can hold. For an enumeration,
-- use 'NanoUI.Var.useEnumVar'; for any other type, 'NanoUI.Var.newVar'.
class VarSlot a where
  slotWrap :: Var a -> SlotVar
  slotMatch :: SlotVar -> Maybe (Var a)

instance VarSlot Int where
  slotWrap = SInt
  slotMatch = \case SInt v -> Just v; _ -> Nothing

instance VarSlot Double where
  slotWrap = SDouble
  slotMatch = \case SDouble v -> Just v; _ -> Nothing

instance VarSlot Float where
  slotWrap = SFloat
  slotMatch = \case SFloat v -> Just v; _ -> Nothing

instance VarSlot Bool where
  slotWrap = SBool
  slotMatch = \case SBool v -> Just v; _ -> Nothing

instance VarSlot Text where
  slotWrap = SText
  slotMatch = \case SText v -> Just v; _ -> Nothing

instance VarSlot SortCol where
  slotWrap = SSort
  slotMatch = \case SSort v -> Just v; _ -> Nothing
