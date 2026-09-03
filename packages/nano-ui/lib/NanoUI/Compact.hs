{-# LANGUAGE TypeApplications #-}

module NanoUI.Compact
  ( Compact
  , compactHost
  , askCompact
  ) where

import Data.Typeable (Typeable)
import Effectful (Eff, type (:>))
import GHC.Compact (Compact, compact, getCompact)
import NanoUI.Context (Context, setHost)
import NanoUI.Monad (Ui, askHost)

-- Pin read-heavy app state so GC treats it as one block.
compactHost :: Typeable a => Context -> a -> IO (Compact a)
compactHost ctx a = do
  region <- compact a
  setHost ctx region
  pure region

askCompact :: forall a es. (Typeable a, Ui :> es) => Eff es (Maybe a)
askCompact = do
  region <- askHost @(Compact a)
  pure (fmap getCompact region)
