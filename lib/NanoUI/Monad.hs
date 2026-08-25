module NanoUI.Monad
  ( UI (..)
  , runUI
  , emit
  , withKey
  , currentId
  , askContext
  , askInput
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Bits (xor)
import Data.Hashable (Hashable, hash)
import Data.IORef (readIORef, writeIORef)
import Data.Word (Word64)
import GHC.Stack (CallStack, HasCallStack, callStack, getCallStack)
import NanoUI.Context (Context (..), FrameMsg (..), pushMessage)
import NanoUI.Id (WidgetId (..), fnv1a, hashSrcLoc, hashWidgetId)
import NanoUI.Input (Input)

newtype UI a = UI {unUI :: Context -> Input -> IO a}

instance Functor UI where
  fmap f (UI g) = UI (\ctx inp -> f <$> g ctx inp)

instance Applicative UI where
  pure a = UI (\_ _ -> pure a)
  UI fg <*> UI fa = UI (\ctx inp -> fg ctx inp >>= \g -> fa ctx inp >>= \a -> pure (g a))

instance Monad UI where
  UI m >>= f = UI (\ctx inp -> m ctx inp >>= \a -> unUI (f a) ctx inp)

instance MonadIO UI where
  liftIO act = UI (\_ _ -> act)

{-# INLINE runUI #-}
runUI :: Context -> Input -> UI a -> IO a
runUI ctx inp (UI m) = m ctx inp

{-# INLINE emit #-}
emit :: msg -> UI ()
emit msg = UI (\ctx _ -> pushMessage ctx (FrameMsg msg) >> pure ())

{-# INLINE withKey #-}
withKey :: Hashable k => k -> UI a -> UI a
withKey k (UI m) = UI (\ctx inp -> do
  old <- readIORef (ctxIdSalt ctx)
  writeIORef (ctxIdSalt ctx) (old `mix64` fromIntegral (hash k))
  r <- m ctx inp
  writeIORef (ctxIdSalt ctx) old
  pure r)

-- The whole stack is hashed, not just its head: the head always points at this
-- module, so distinct user call sites are only distinguishable by outer frames.
{-# INLINE currentId #-}
currentId :: HasCallStack => UI WidgetId
currentId = UI (\ctx _ -> do
  salt <- readIORef (ctxIdSalt ctx)
  let base = hashCallStack callStack
  pure (WidgetId (base `mix64` salt)))

hashCallStack :: CallStack -> Word64
hashCallStack cs =
  foldl
    (\acc (fn, loc) -> acc `mix64` fnv1a fn `mix64` hashWidgetId (hashSrcLoc loc))
    14695981039346656037
    (getCallStack cs)

{-# INLINE askContext #-}
askContext :: UI Context
askContext = UI (\ctx _ -> pure ctx)

{-# INLINE askInput #-}
askInput :: UI Input
askInput = UI (\_ inp -> pure inp)

{-# INLINE mix64 #-}
mix64 :: Word64 -> Word64 -> Word64
mix64 h k = (h `xor` k) * 1099511628211
