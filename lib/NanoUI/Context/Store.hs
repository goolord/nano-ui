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
import NanoUI.Store (WidgetStore (..))

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
  pure (IM.findWithDefault False (intKey wid) (storeDisabled store))

{-# INLINE setDisabled #-}
setDisabled :: Context -> WidgetId -> Bool -> IO ()
setDisabled ctx wid on = do
  store <- getStore ctx
  setStore ctx (store {storeDisabled = IM.insert (intKey wid) on (storeDisabled store)})

{-# INLINE getScrollOffset #-}
getScrollOffset :: Context -> WidgetId -> IO Float
getScrollOffset ctx wid = do
  store <- getStore ctx
  pure (IM.findWithDefault 0 (intKey wid) (storeScroll store))

{-# INLINE setScrollOffset #-}
setScrollOffset :: Context -> WidgetId -> Float -> IO ()
setScrollOffset ctx wid off = do
  store <- getStore ctx
  let key = intKey wid
      prev = IM.findWithDefault 0 key (storeScroll store)
  when (prev /= off) $ do
    setStore ctx (store {storeScroll = IM.insert key off (storeScroll store)})
    markDirty ctx
