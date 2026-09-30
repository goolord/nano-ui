-- | Conservative physical-equality shortcuts for immutable values.
module NanoUI.Internal.Equality (ptrEq, eqByPtr) where

import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)

-- | True implies the closures are identical; False requires a real comparison.
-- Forces neither argument. Callers comparing evaluated values must force them
-- before calling, rather than comparing selector thunks.
{-# INLINE ptrEq #-}
ptrEq :: a -> b -> Bool
ptrEq a b = isTrue# (reallyUnsafePtrEquality# a b)

-- | Structural equality with a physical-equality fast path.
{-# INLINE eqByPtr #-}
eqByPtr :: Eq a => a -> a -> Bool
eqByPtr !a !b = ptrEq a b || a == b
