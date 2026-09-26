{-# LANGUAGE StrictData #-}

-- | Widget ids and the id context they are derived from. See the
-- "Widget identity" section of "NanoUI" for how ids are assigned.
module NanoUI.Internal.Id
  ( WidgetId (..)
  , IdContext (..)
  , initialIdContext
  , idContextWidgetId
  , widgetId
  , hashWidgetId
  , mix64
  , mixFnv
  , scopeTag
  , enterScope
  , enterKeyed
  )
where

import Data.Bits (shiftR, xor)
import Data.Char (ord)
import Data.Hashable (Hashable)
import Data.Primitive.Types (Prim)
import Data.Word (Word64, Word8)
import GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack)

-- | Stable store and interaction identity. Zero is reserved for no widget.
newtype WidgetId = WidgetId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, Prim)

-- | Parent-path hash and the next sibling's position within that path.
data IdContext = IdContext
  { currentId :: {-# UNPACK #-} !Word64
  , siblingId :: {-# UNPACK #-} !Word64
  }
  deriving stock (Eq, Show)

-- | Root path and sibling position used at the start of each view pass.
initialIdContext :: IdContext
initialIdContext = IdContext 0x243F6A8885A308D3 0

-- | Id of the next sibling in this context. A zero hash becomes 1, so
-- @WidgetId 0@ never names a real widget.
{-# INLINE idContextWidgetId #-}
idContextWidgetId :: IdContext -> WidgetId
idContextWidgetId (IdContext cid sid) =
  let
    raw = mix64 cid sid
   in
    if raw == 0 then WidgetId 1 else WidgetId raw

-- | Hash salt distinguishing an ordinary child scope from a keyed scope.
scopeTag :: Word64
scopeTag = 0x9E3779B185EBCA87

keyedTag :: Word64
keyedTag = 0xC2B2AE3D27D4EB4F

-- | Return the advanced parent and a fresh child context derived from its
-- sibling position and the supplied tag.
{-# INLINE enterScope #-}
enterScope :: Word64 -> IdContext -> (IdContext, IdContext)
enterScope tag parent = enterChild (siblingId parent) tag parent

-- | Return the advanced parent and a child path derived from the key, not the
-- sibling position. Keys must be unique within the parent.
{-# INLINE enterKeyed #-}
enterKeyed :: Word64 -> IdContext -> (IdContext, IdContext)
enterKeyed tag = enterChild tag keyedTag

-- | Advance the parent's sibling counter and derive a child path from the
-- parent path, @seed@ and @tag@.
{-# INLINE enterChild #-}
enterChild :: Word64 -> Word64 -> IdContext -> (IdContext, IdContext)
enterChild seed tag (IdContext pid sib) = (IdContext pid (sib + 1), IdContext (mix64 (mix64 pid seed) tag) 0)

-- | Hash the call site's package, module, file, line, and column. Repeated
-- calls at one source location return the same id; this is not a list-item key.
-- Throws if the call stack is empty.
{-# INLINE widgetId #-}
widgetId :: HasCallStack => WidgetId
widgetId =
  let
    stack = getCallStack callStack
    loc = case stack of
      (_, loc') : _ -> loc'
      [] -> error "widgetId: empty CallStack"
   in
    hashSrcLoc loc

hashSrcLoc :: SrcLoc -> WidgetId
hashSrcLoc loc =
  WidgetId $
    fnv1a (srcLocPackage loc)
      `mixFnv` fnv1a (srcLocModule loc)
      `mixFnv` fnv1a (srcLocFile loc)
      `mixFnv` fromIntegral (srcLocStartLine loc)
      `mixFnv` fromIntegral (srcLocStartCol loc)

-- | Unwrap the id's existing hash without hashing it again.
{-# INLINE hashWidgetId #-}
hashWidgetId :: WidgetId -> Word64
hashWidgetId (WidgetId w) = w

-- | FNV-1a over the low byte of each character. Intended for identifier seeds,
-- not general Unicode hashing or cryptography.
{-# INLINE fnv1a #-}
fnv1a :: String -> Word64
fnv1a s =
  foldl'
    (\acc c -> (fromIntegral @Word8 @Word64 (fromIntegral (ord c)) `xor` acc) * 0x00000100000001B3)
    0xcbf29ce484222325
    s

-- | Mix two 64-bit identifier components with wrapping arithmetic.
-- This is a non-cryptographic hash combiner.
{-# INLINE mix64 #-}
mix64 :: Word64 -> Word64 -> Word64
mix64 x y =
  let
    z = x + (y * 0x9E3779B97F4A7C15)
    z1 = z `xor` (z `shiftR` 30)
    z2 = z1 * 0xBF58476D1CE4E5B9
    z3 = z2 `xor` (z2 `shiftR` 27)
   in
    z3 * 0x94D049BB133111EB

-- | Combine two hash words with an FNV xor-and-multiply step.
{-# INLINE mixFnv #-}
mixFnv :: Word64 -> Word64 -> Word64
mixFnv x y = (x `xor` y) * 1099511628211
