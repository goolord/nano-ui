module NanoUI.Monad
  ( UI (..)
  , runUI
  , emit
  , withKey
  , currentId
  , askContext
  , askInput
  ) where

import Data.Bits (xor)
import Data.Hashable (Hashable, hash)
import Data.IORef (readIORef, writeIORef)
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import NanoUI.Context (Context (..), FrameMsg (..), pushMessage)
import NanoUI.Id (WidgetId (..), hashWidgetId, widgetId)
import NanoUI.Input (Input)

newtype UI a = UI {unUI :: Context -> Input -> IO a}

instance Functor UI where
  fmap f (UI g) = UI (\ctx inp -> f <$> g ctx inp)

instance Applicative UI where
  pure a = UI (\_ _ -> pure a)
  UI fg <*> UI fa = UI (\ctx inp -> fg ctx inp >>= \g -> fa ctx inp >>= \a -> pure (g a))

instance Monad UI where
  UI m >>= f = UI (\ctx inp -> m ctx inp >>= \a -> unUI (f a) ctx inp)

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

{-# INLINE currentId #-}
currentId :: HasCallStack => UI WidgetId
currentId = UI (\ctx _ -> do
  salt <- readIORef (ctxIdSalt ctx)
  let base = hashWidgetId widgetId
  pure (WidgetId (base `mix64` salt)))

{-# INLINE askContext #-}
askContext :: UI Context
askContext = UI (\ctx _ -> pure ctx)

{-# INLINE askInput #-}
askInput :: UI Input
askInput = UI (\_ inp -> pure inp)

{-# INLINE mix64 #-}
mix64 :: Word64 -> Word64 -> Word64
mix64 h k = (h `xor` k) * 1099511628211
