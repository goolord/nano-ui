-- | Store immutable, read-heavy host data in a GHC compact region to reduce GC scanning.
module NanoUI.Internal.Compact
  ( Compact
  , compactHost
  , askCompact
  ) where

import GHC.Compact (Compact, compact, getCompact)
import NanoUI.Internal.Host (Host, setHost)
import NanoUI.Internal.Monad (NanoUI, askHost)

-- | Copy data into a compact region and retain it in an explicit typed slot.
-- GHC's 'compact' restrictions apply: values containing functions or mutable
-- objects cannot be compacted. This is not an FFI buffer-pinning operation.
compactHost :: Host (Compact a) -> a -> IO (Compact a)
compactHost owner a = compact a >>= \region -> region <$ setHost owner region

-- | Read a compacted value from its typed owner, or 'Nothing' if uninstalled.
askCompact :: Host (Compact a) -> NanoUI (Maybe a)
askCompact owner = fmap getCompact <$> askHost owner
