{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module NanoUI.Context.Host
  ( setHost
  , askHostIO
  ) where

import Data.Dynamic (fromDynamic, toDyn)
import Data.IORef (modifyIORef', readIORef)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeOf, typeRep)
import qualified Data.Map.Strict as Map
import NanoUI.Context.Internal (Context (..))

{-# INLINE setHost #-}
setHost :: Typeable a => Context -> a -> IO ()
setHost ctx a = modifyIORef' (ctxHost ctx) (Map.insert (typeOf a) (toDyn a))

{-# INLINE askHostIO #-}
askHostIO :: forall a. Typeable a => Context -> IO (Maybe a)
askHostIO ctx = do
  hosts <- readIORef (ctxHost ctx)
  pure (Map.lookup (typeRep (Proxy @a)) hosts >>= fromDynamic)
