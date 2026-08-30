module NanoUI.Context.Focus
  ( getFocusId
  , getHotId
  , registerFocusable
  , getFocusables
  , getFocusablesPrim
  ) where

import Control.Monad (forM)
import Data.IORef (readIORef, writeIORef)
import Data.Primitive.PrimArray
  ( PrimArray
  , copyMutablePrimArray
  , freezePrimArray
  , newPrimArray
  , readPrimArray
  , writePrimArray
  )
import NanoUI.Context.Internal (Context (..))
import NanoUI.Id (WidgetId)

{-# INLINE getFocusId #-}
getFocusId :: Context -> IO WidgetId
getFocusId ctx = readIORef (ctxFocusId ctx)

{-# INLINE getHotId #-}
getHotId :: Context -> IO WidgetId
getHotId ctx = readIORef (ctxHotId ctx)

{-# INLINE registerFocusable #-}
registerFocusable :: Context -> WidgetId -> IO ()
registerFocusable ctx wid = do
  idx <- readIORef (ctxFocusablesCount ctx)
  cap <- readIORef (ctxFocusablesCap ctx)
  arr <- readIORef (ctxFocusables ctx)
  arr' <-
    if idx >= cap
      then do
        let newCap = max 16 (cap * 2)
        newArr <- newPrimArray newCap
        copyMutablePrimArray newArr 0 arr 0 idx
        writeIORef (ctxFocusables ctx) newArr
        writeIORef (ctxFocusablesCap ctx) newCap
        pure newArr
      else pure arr
  writePrimArray arr' idx wid
  writeIORef (ctxFocusablesCount ctx) (idx + 1)

{-# INLINE getFocusables #-}
getFocusables :: Context -> IO [WidgetId]
getFocusables ctx = do
  count <- readIORef (ctxFocusablesCount ctx)
  arr <- readIORef (ctxFocusables ctx)
  forM [0 .. count - 1] (readPrimArray arr)

{-# INLINE getFocusablesPrim #-}
getFocusablesPrim :: Context -> IO (PrimArray WidgetId)
getFocusablesPrim ctx = do
  count <- readIORef (ctxFocusablesCount ctx)
  arr <- readIORef (ctxFocusables ctx)
  freezePrimArray arr 0 count
