{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NanoUI.Id
  ( WidgetId (..)
  , widgetId
  , hashWidgetId
  , hashSrcLoc
  , fnv1a
  , mixId
  ) where

import Data.Bits (xor)
import Data.Hashable (Hashable, hash)
import Data.Word (Word64, Word8)
import GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack)
import Data.Char (ord)
import Data.Primitive.Types (Prim)

newtype WidgetId = WidgetId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Prim)

{-# INLINE widgetId #-}
widgetId :: HasCallStack => WidgetId
widgetId =
  let stack = getCallStack callStack
      loc = case stack of
        (_, loc') : _ -> loc'
        [] -> error "widgetId: empty CallStack"
   in hashSrcLoc loc

{-# INLINE hashSrcLoc #-}
hashSrcLoc :: SrcLoc -> WidgetId
hashSrcLoc (SrcLoc {srcLocPackage, srcLocModule, srcLocFile, srcLocStartLine, srcLocStartCol}) =
  WidgetId $
    fnv1a srcLocPackage
      `mix64` fnv1a srcLocModule
      `mix64` fnv1a srcLocFile
      `mix64` (fromIntegral srcLocStartLine)
      `mix64` (fromIntegral srcLocStartCol)

{-# INLINE hashWidgetId #-}
hashWidgetId :: WidgetId -> Word64
hashWidgetId (WidgetId w) = w

{-# INLINE mixId #-}
mixId :: Hashable k => WidgetId -> k -> WidgetId
mixId (WidgetId base) k = WidgetId (base `mix64` fromIntegral (hash k))

{-# INLINE fnv1a #-}
fnv1a :: String -> Word64
fnv1a s = foldl'
  ( \acc c -> (fromIntegral @Word8 @Word64 (c2w c) `xor` acc) * 0x00000100000001B3
  )
  0xcbf29ce484222325
  s
  where
  c2w :: Char -> Word8
  c2w = fromIntegral . ord
  {-# INLINE c2w #-}

{-# INLINE mix64 #-}
mix64 :: Word64 -> Word64 -> Word64
mix64 h k = (h `xor` k) * 1099511628211
