{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The 'Var' handle: a constant, or a cell plus a path into its value, with
-- the paths' reads and writes, the list-element index, and the positional
-- slot types. The view's reads and writes, and what they cost, live in
-- "NanoUI.Internal.Var".
module NanoUI.Internal.Var.Types
  ( Var (..)
  , constVar
  , Lens (..)
  , ElemCtx (..)
  , viewLens
  , previewLens
  , overLensSig
  , peekLens
  , zoomVar
  , zoomVarL
  , fieldLens
  , newElemCtx
  , stepBit
  , lensSig
  , structSig
  , VarSlot (..)
  , SlotVar (..)
  )
where

import Data.Bits (bit, (.&.))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Typeable (TypeRep)
import Data.Word (Word64)
import GHC.Generics (Generic (..))
import GHC.Records (HasField (..))
import GHC.TypeLits (Symbol)
import NanoUI.Internal.Derived (SortCol)
import NanoUI.Internal.Equality (ptrEq)
import NanoUI.Internal.Var.Cell (Cell, Sig (..), allBits)
import NanoUI.Internal.Var.Generic (FieldPath, GFocus (..), PathIx (..))

-- | Mutable state handle (a cell and lens path) or read-only constant passed
-- to widgets.
--
-- * 'NanoUI.Var.newVar' and 'NanoUI.Var.useVar' create mutable 'Var's that
--   widgets mutate in place.
-- * @'constVar' x@ is a read-only constant; widget edits to it are discarded.
--
-- @v.field@ focuses a 'Var' on one field of its record, for any single-constructor
-- record with a 'Generic' instance.
data Var a where
  VarConst :: a -> Var a
  VarRef :: {-# UNPACK #-} !(Cell s) -> !(Lens s a) -> Var a

-- | Read-only 'Var' with constant value @x@; widget edits to it are ignored.
-- Use with primed widget variants to handle updates manually from the response:
--
-- > allDone <- all todoDone <$> readVar todos
-- > r <- checkbox' "All done" (constVar allDone)
-- > when (respChanged r) (modifyVar todos (map (\t -> t {todoDone = not allDone})))
constVar :: a -> Var a
constVar = VarConst

-- | The path from a cell's value to the part a Var shows. Each step after
-- 'Here' carries its bit for 'lensSig': a record field's or a list key's,
-- or 0 for a step to an unknown part.
data Lens s a where
  Here :: Lens a a
  Field :: {-# UNPACK #-} !Word64 -> !(Lens s b) -> (b -> a) -> (a -> b -> b) -> Lens s a
  Elem :: {-# UNPACK #-} !Word64 -> !(ElemCtx s k a) -> !k -> a -> Lens s a

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
viewLens (Field _ l g _) s = g (viewLens l s)
viewLens (Elem _ (ElemCtx l key _ _) k snap) s =
  fromMaybe snap (find ((== k) . key) (viewLens l s))

-- | The part of @s@ a lens shows, or 'Nothing' when its path goes through an
-- element whose key has left the list, which a write cannot reach.
previewLens :: Lens s a -> s -> Maybe a
previewLens Here s = Just s
previewLens (Field _ l g _) s = g <$> previewLens l s
previewLens (Elem _ (ElemCtx l key _ _) k _) s = find ((== k) . key) =<< previewLens l s

-- | Update the part of @s@ focused by the lens, returning the updated root and
-- the modified part signature ('lensSig'): the lens's own part or, when the
-- update changed a list element's key, the outermost affected list (including
-- keys and order). Updating an element whose key is no longer in the list is a
-- no-op. Keys are unique, so only the elements before the modified one are
-- copied.
overLensSig :: Lens s a -> (a -> a) -> s -> (s, Sig)
overLensSig l f s = case overPath l (\a -> (f a, Nothing)) s of
  (s', rekeyed) -> (s', fromMaybe (lensSig l) rekeyed)

-- | 'overLensSig' over a change that may itself report a rekeyed list.
overPath :: Lens s a -> (a -> (a, Maybe Sig)) -> s -> (s, Maybe Sig)
overPath Here f s = f s
overPath (Field _ l g set) f s = overPath l (\b -> case f (g b) of (!a, w) -> (set a b, w)) s
overPath (Elem _ (ElemCtx l key _ _) k _) f s = overPath l update s
  where
    update xs = case break ((== k) . key) xs of
      (before, x : after) -> case f x of
        (!y, w) -> (before ++ y : after, if key y == k then w else Just (lensSig l))
      (_, []) -> (xs, Nothing)

-- | 'viewLens', using an element's index instead of a linear search: the
-- snapshot when the list is the one handed out, else an index of the
-- current list, built once per list. Parts are returned evaluated, so the
-- list compares by pointer with the one handed out.
peekLens :: Lens s a -> s -> IO a
peekLens Here s = pure s
peekLens (Field _ l g _) s = do
  b <- peekLens l s
  pure $! g b
peekLens (Elem _ (ElemCtx l key xs0 cache) k snap) s = do
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

-- | The part of the cell a path touches: its first two steps narrow it,
-- and a step to an unknown part closes it.
lensSig :: Lens s a -> Sig
lensSig Here = Sig allBits allBits True
lensSig (Field step l _ _) = narrow step (lensSig l)
lensSig (Elem step (ElemCtx l _ _ _) _ _) = narrow step (lensSig l)

narrow :: Word64 -> Sig -> Sig
narrow step (Sig a b open)
  | step == 0 || not open = Sig a b False
  | a == allBits = Sig step allBits True
  | otherwise = Sig a step False

-- | The part of the cell that holds a list's keys and order, apart from
-- what its elements hold, for a reader that only lays out one row per key.
-- It takes a bit no field or key takes, at the list's level.
structSig :: Lens s [a] -> Sig
structSig l = case lensSig l of
  Sig a _ True
    | a == allBits -> Sig structBit allBits False
    | otherwise -> Sig a structBit False
  sig -> sig

-- | The bit only 'structSig' takes.
structBit :: Word64
structBit = bit 63

-- | A field's or key's bit, from a number such as a key's hash: any bit but
-- 'structBit'.
{-# INLINE stepBit #-}
stepBit :: Int -> Word64
stepBit n = let i = n .&. 63 in bit (if i == 63 then 62 else i)

-- | Focus a Var on part of its value, given how to read the part and how to
-- replace it. Writes through the result change that part of the whole.
--
-- > draft <- zoomVar fst (\a (_, b) -> (a, b)) pair
--
-- Where @v.field@ is known to touch one field, the part this shows is
-- unknown, so a write through it counts as a write to all of @v@ when
-- deciding what it costs.
{-# INLINE zoomVar #-}
zoomVar :: (s -> a) -> (a -> s -> s) -> Var s -> Var a
zoomVar = zoomStep 0

-- | 'zoomVar' with the step's bit for 'lensSig'.
{-# INLINE zoomStep #-}
zoomStep :: Word64 -> (s -> a) -> (a -> s -> s) -> Var s -> Var a
zoomStep _ g _ (VarConst s) = VarConst (g s)
zoomStep step g set (VarRef cell l) = VarRef cell (Field step l g set)

-- | 'zoomVar' from a van Laarhoven lens, such as one from @lens@ or
-- @microlens@.
{-# INLINE zoomVarL #-}
zoomVarL :: (forall f. Functor f => (a -> f a) -> s -> f s) -> Var s -> Var a
zoomVarL l = zoomVar (getConst . l Const) (\a -> runIdentity . l (\_ -> Identity a))

instance (Generic s, GFocus (FieldPath x s (Rep s)) (Rep s) a, PathIx (FieldPath x s (Rep s))) => HasField (x :: Symbol) (Var s) (Var a) where
  {-# INLINE getField #-}
  getField = uncurry (zoomStep (stepBit (pathIx @(FieldPath x s (Rep s)) 1))) (fieldLens @x)

-- | How to read field @x@ of @s@ and how to replace it, found through
-- "GHC.Generics"; with optimisation, a selector and a record update.
{-# INLINE fieldLens #-}
fieldLens :: forall x s a. (Generic s, GFocus (FieldPath x s (Rep s)) (Rep s) a) => (s -> a, a -> s -> s)
fieldLens = (gget @(FieldPath x s (Rep s)) . from, \a -> to . gset @(FieldPath x s (Rep s)) a . from)

-- | A positional Var as the slot table keeps it, tagged with its type so a
-- slot that changes type starts over instead of being misread.
data SlotVar
  = SInt !(Var Int)
  | SDouble !(Var Double)
  | SFloat !(Var Float)
  | SBool !(Var Bool)
  | SText !(Var Text)
  | -- | An enumeration's index, with the enumeration's type.
    SEnum !TypeRep !(Var Int)
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
