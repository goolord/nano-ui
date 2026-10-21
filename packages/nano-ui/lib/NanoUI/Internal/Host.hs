-- | Explicit, typed host slots. Ownership is carried by a handle rather than
-- recovered from a runtime type in a context-wide heterogeneous map.
module NanoUI.Internal.Host (Host, newHost, setHost, clearHost, askHostIO, hostOrInit) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- | A single-session, UI-thread slot. An uninstalled host is absent.
newtype Host a = Host (IORef (Maybe a))

-- | A new slot with no value installed.
newHost :: IO (Host a)
newHost = Host <$> newIORef Nothing

-- | Install a value in the slot, replacing any already there.
{-# INLINE setHost #-}
setHost :: Host a -> a -> IO ()
setHost (Host ref) value = writeIORef ref (Just value)

clearHost :: Host a -> IO ()
clearHost (Host ref) = writeIORef ref Nothing

{-# INLINE askHostIO #-}
askHostIO :: Host a -> IO (Maybe a)
askHostIO (Host ref) = readIORef ref

-- | Lazily initialize a typed slot. Call serially on its owning UI thread.
hostOrInit :: Host a -> IO a -> IO a
hostOrInit owner acquire =
  askHostIO owner
    >>= maybe (acquire >>= \value -> value <$ setHost owner value) pure
