-- | Conservative physical-equality shortcuts for immutable values, and the
-- equalities inputs compare their values with.
module NanoUI.Internal.Equality (ptrEq, eqByPtr, sameFloat, sameDouble, sameEnum) where

import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import GHC.Float (castDoubleToWord64, castFloatToWord32)

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

-- | '==', except that a NaN equals the same NaN, so an input holding one
-- does not count as changed on every frame.
{-# INLINE sameFloat #-}
sameFloat :: Float -> Float -> Bool
sameFloat a b = a == b || castFloatToWord32 a == castFloatToWord32 b

{-# INLINE sameDouble #-}
sameDouble :: Double -> Double -> Bool
sameDouble a b = a == b || castDoubleToWord64 a == castDoubleToWord64 b

-- | Equality by index, for enumerations without an 'Eq' instance.
{-# INLINE sameEnum #-}
sameEnum :: Enum a => a -> a -> Bool
sameEnum a b = fromEnum a == fromEnum b
