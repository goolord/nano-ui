{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module NanoUI.Id
  ( WidgetId (..)
  , IdContext (..)
  , initialIdContext
  , widgetId
  , hashWidgetId
  , hashSrcLoc
  , fnv1a
  , mix64
  , mixFnv
  , mixId
  , scopeTag
  , keyedTag
  , enterScope
  , enterKeyed
  )
where

import Data.Bits (shiftR, xor)
import Data.Char (ord)
import Data.Hashable (Hashable, hash)
import Data.Primitive.Types (Prim)
import Data.Word (Word64, Word8)
import GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack)

newtype WidgetId = WidgetId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, Prim)

data IdContext = IdContext
  { currentId :: {-# UNPACK #-} !Word64
  , siblingId :: {-# UNPACK #-} !Word64
  }
  deriving stock (Eq, Show)

initialIdContext :: IdContext
initialIdContext = IdContext 0x243F6A8885A308D3 0

scopeTag :: Word64
scopeTag = 0x9E3779B185EBCA87

keyedTag :: Word64
keyedTag = 0xC2B2AE3D27D4EB4F

{-# INLINE enterScope #-}
enterScope :: Word64 -> IdContext -> (IdContext, IdContext)
enterScope tag parent =
  let
    IdContext pid sib = parent
    child = IdContext (mix64 (mix64 pid sib) tag) 0
    parent' = parent {siblingId = sib + 1}
   in
    (parent', child)

{-# INLINE enterKeyed #-}
enterKeyed :: Word64 -> IdContext -> (IdContext, IdContext)
enterKeyed tag parent =
  let
    IdContext pid sid = parent
    child = IdContext (mix64 (mix64 pid tag) keyedTag) 0
    parent' = parent {siblingId = sid + 1}
   in
    (parent', child)

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

{-# INLINE hashSrcLoc #-}
hashSrcLoc :: SrcLoc -> WidgetId
hashSrcLoc
  ( SrcLoc
      { srcLocPackage
      , srcLocModule
      , srcLocFile
      , srcLocStartLine
      , srcLocStartCol
      }
    ) =
    WidgetId $
      fnv1a srcLocPackage
        `mixFnv` fnv1a srcLocModule
        `mixFnv` fnv1a srcLocFile
        `mixFnv` fromIntegral srcLocStartLine
        `mixFnv` fromIntegral srcLocStartCol

{-# INLINE hashWidgetId #-}
hashWidgetId :: WidgetId -> Word64
hashWidgetId (WidgetId w) = w

{-# INLINE mixId #-}
mixId :: Hashable k => WidgetId -> k -> WidgetId
mixId (WidgetId base) k = WidgetId (base `mixFnv` fromIntegral (hash k))

{-# INLINE fnv1a #-}
fnv1a :: String -> Word64
fnv1a s =
  foldl'
    (\acc c -> (fromIntegral @Word8 @Word64 (c2w c) `xor` acc) * 0x00000100000001B3)
    0xcbf29ce484222325
    s
 where
  c2w :: Char -> Word8
  c2w = fromIntegral . ord
  {-# INLINE c2w #-}

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

{-# INLINE mixFnv #-}
mixFnv :: Word64 -> Word64 -> Word64
mixFnv x y = (x `xor` y) * 1099511628211
