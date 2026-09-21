{-# LANGUAGE TypeApplications #-}

-- | Store immutable, read-heavy host data in a GHC compact region to reduce GC scanning.
module NanoUI.Internal.Compact
  ( Compact
  , compactHost
  , askCompact
  ) where

import Data.Typeable (Typeable)
import Effectful (Eff, type (:>))
import GHC.Compact (Compact, compact, getCompact)
import NanoUI.Internal.Context (Context, setHost)
import NanoUI.Internal.Monad (Ui, askHost)

-- | Copy data into a compact region and store it by type in the context.
-- GHC's 'compact' restrictions apply: values containing functions or mutable
-- objects cannot be compacted. This is not an FFI buffer-pinning operation.
compactHost :: Typeable a => Context -> a -> IO (Compact a)
compactHost ctx a = do
  region <- compact a
  setHost ctx region
  pure region

-- | Read the compacted host value of the requested type, or 'Nothing' if absent.
askCompact :: forall a es. (Typeable a, Ui :> es) => Eff es (Maybe a)
askCompact = do
  region <- askHost @(Compact a)
  pure (fmap getCompact region)
