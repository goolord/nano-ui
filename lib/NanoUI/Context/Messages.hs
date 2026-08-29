module NanoUI.Context.Messages
  ( pushMessage
  , drainMessages
  ) where

import Data.IORef (readIORef, writeIORef)
import NanoUI.Context.Internal (Context (..))
import NanoUI.Messages (FrameMsg)

{-# INLINE pushMessage #-}
pushMessage :: Context -> FrameMsg -> IO ()
pushMessage ctx msg = do
  msgs <- readIORef (ctxMessages ctx)
  writeIORef (ctxMessages ctx) (msg : msgs)

{-# INLINE drainMessages #-}
drainMessages :: Context -> IO [FrameMsg]
drainMessages ctx = do
  msgs <- readIORef (ctxMessages ctx)
  writeIORef (ctxMessages ctx) []
  pure (reverse msgs)
