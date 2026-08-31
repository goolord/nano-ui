module NanoUI.Context.Store
  ( getStore
  , setStore
  , isDisabled
  , setDisabled
  , getScrollOffset
  , setScrollOffset
  ) where

import Control.Monad (when)
import Data.IORef (readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import NanoUI.Context.Internal (Context (..), intKey, markDirty)
import NanoUI.Id (WidgetId)
import NanoUI.Store
  ( WidgetStore (..)
  , boolInt
  , intBool
  , slotDisabled
  , slotKey
  )

{-# INLINE getStore #-}
getStore :: Context -> IO WidgetStore
getStore ctx = readIORef (ctxStore ctx)

{-# INLINE setStore #-}
setStore :: Context -> WidgetStore -> IO ()
setStore ctx store = do
  prev <- readIORef (ctxStore ctx)
  writeIORef (ctxStore ctx) store
  when (prev /= store) (markDirty ctx)

{-# INLINE isDisabled #-}
isDisabled :: Context -> WidgetId -> IO Bool
isDisabled ctx wid = do
  store <- getStore ctx
  let key = slotKey slotDisabled (intKey wid)
  pure (intBool (IM.findWithDefault 0 key (storeInt store)))

{-# INLINE setDisabled #-}
setDisabled :: Context -> WidgetId -> Bool -> IO ()
setDisabled ctx wid on = do
  store <- getStore ctx
  let key = slotKey slotDisabled (intKey wid)
  setStore
    ctx
    (store {storeInt = IM.insert key (boolInt on) (storeInt store)})

{-# INLINE getScrollOffset #-}
getScrollOffset :: Context -> WidgetId -> IO Float
getScrollOffset ctx wid = do
  store <- getStore ctx
  pure (IM.findWithDefault 0 (intKey wid) (storeFloat store))

{-# INLINE setScrollOffset #-}
setScrollOffset :: Context -> WidgetId -> Float -> IO ()
setScrollOffset ctx wid off = do
  store <- getStore ctx
  let key = intKey wid
      prev = IM.findWithDefault 0 key (storeFloat store)
  when (prev /= off) $ do
    setStore ctx (store {storeFloat = IM.insert key off (storeFloat store)})
    markDirty ctx
