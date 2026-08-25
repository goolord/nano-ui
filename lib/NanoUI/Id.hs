module NanoUI.Id
  ( WidgetId (..)
  , widgetId
  , hashWidgetId
  , mixId
  ) where

import Data.Bits (xor)
import Data.Hashable (Hashable, hash)
import Data.Word (Word64)
import GHC.Stack (CallStack, HasCallStack, SrcLoc (..), callStack, getCallStack)

newtype WidgetId = WidgetId Word64
  deriving (Eq, Ord, Show)

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
fnv1a s = go 14695981039346656037 s
  where
    go :: Word64 -> String -> Word64
    go h [] = h
    go h (c : cs) = go ((h `xor` fromIntegral (fromEnum c)) * 1099511628211) cs

{-# INLINE mix64 #-}
mix64 :: Word64 -> Word64 -> Word64
mix64 h k = (h `xor` k) * 1099511628211
