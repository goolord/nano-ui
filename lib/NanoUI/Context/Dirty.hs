module NanoUI.Context.Dirty
  ( clearDirty
  , isDirty
  , setWakeLoop
  , takeDamage
  ) where

import Data.IORef (readIORef, writeIORef)
import NanoUI.Context.Internal (Context (..))
import NanoUI.Types (Damage (..))

{-# INLINE clearDirty #-}
clearDirty :: Context -> IO ()
clearDirty ctx = writeIORef (ctxDirty ctx) False

{-# INLINE setWakeLoop #-}
setWakeLoop :: Context -> IO () -> IO ()
setWakeLoop ctx wake = writeIORef (ctxWakeLoop ctx) (Just wake)

{-# INLINE isDirty #-}
isDirty :: Context -> IO Bool
isDirty ctx = readIORef (ctxDirty ctx)

{-# INLINE takeDamage #-}
takeDamage :: Context -> IO Damage
takeDamage ctx = readIORef (ctxDamage ctx)
