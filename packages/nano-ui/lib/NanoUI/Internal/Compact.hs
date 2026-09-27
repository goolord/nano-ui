-- | Store immutable, read-heavy host data in a GHC compact region to reduce GC scanning.
module NanoUI.Internal.Compact
  ( Compact
  , compactHost
  , askCompact
  ) where

import Data.Typeable (Typeable)
import GHC.Compact (Compact, compact, getCompact)
import NanoUI.Internal.Context (Context, setHost)
import NanoUI.Internal.Monad (NanoUI, askHost)

-- | Copy data into a compact region and store it by type in the context.
-- GHC's 'compact' restrictions apply: values containing functions or mutable
-- objects cannot be compacted. This is not an FFI buffer-pinning operation.
compactHost :: Typeable a => Context -> a -> IO (Compact a)
compactHost ctx a = compact a >>= \region -> region <$ setHost ctx region

-- | Read the compacted host value of the requested type, or 'Nothing' if absent.
askCompact :: forall a. Typeable a => NanoUI (Maybe a)
askCompact = fmap getCompact <$> askHost @(Compact a)
