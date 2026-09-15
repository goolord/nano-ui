-- | Scroll offsets, links and configuration kept in the widget store.
module NanoUI.Context.Scroll
  ( getScrollOffset
  , setScrollOffset
  , getScrollOffset2D
  , setScrollOffset2D
  , setScrollConfig
  , linkScrollAxes
  ) where

import Control.Monad (when)
import Data.IntMap.Strict qualified as IM

import NanoUI.Context.Core (damageWidget, getStore, setStore)
import NanoUI.Context.Types (Context (..), intKey)
import NanoUI.Draw qualified as Draw
import NanoUI.Frame.Scroll.Geometry
  ( ScrollConfig
  , decodeScrollConfig
  , defaultScrollConfig
  , encodeScrollConfig
  , scrollConfigNative2D
  )
import NanoUI.Id (WidgetId)
import NanoUI.Store
  ( WidgetStore (..)
  , slotKey
  , slotScrollCfg
  , slotScrollCross
  , slotScrollLinkX
  , slotScrollLinkY
  , slotScrollOff
  , slotTextAreaScroll
  )
import NanoUI.Types (DamageBounds (..), V2 (..), onGrid, v2X, v2Y)

{-# INLINE snapScrollOffset #-}
snapScrollOffset :: Context -> Float -> IO Float
snapScrollOffset ctx v = do
  s <- Draw.getDrawSnapScale (ctxDrawArena ctx)
  pure (onGrid s v)

getScrollOffset :: Context -> WidgetId -> IO Float
getScrollOffset ctx wid = do
  s <- getStore ctx
  let key = intKey wid
      sKey = slotKey slotTextAreaScroll key
  off <-
    case IM.lookup sKey (storePoint s) of
      Just (_, sy) -> pure sy
      Nothing -> do
        cfg <- getScrollConfig ctx wid
        if scrollConfigNative2D cfg
          then v2Y <$> getScrollOffset2D ctx wid
          else pure (IM.findWithDefault 0 key (storeFloat s))
  snapScrollOffset ctx off

setScrollOffset :: Context -> WidgetId -> Float -> IO ()
setScrollOffset ctx wid off = do
  store <- getStore ctx
  let key = intKey wid
      sKey = slotKey slotTextAreaScroll key
  case IM.lookup sKey (storePoint store) of
    Just (sx, sy) ->
      when (sy /= off) $ do
        setStore ctx (store {storePoint = IM.insert sKey (sx, off) (storePoint store)})
        damageWidget ctx wid DamageSelf
    Nothing -> do
      cfg <- getScrollConfig ctx wid
      if scrollConfigNative2D cfg
        then do
          cur <- getScrollOffset2D ctx wid
          setScrollOffset2D ctx wid (V2 (v2X cur) off)
        else do
          let prev = IM.findWithDefault 0 key (storeFloat store)
          when (prev /= off) $ do
            let floats0 = IM.insert key off (storeFloat store)
                yKey = IM.findWithDefault 0 (slotKey slotScrollLinkY key) (storeInt store)
            if yKey == 0
              then setStore ctx (store {storeFloat = floats0})
              else do
                let offKey = slotKey slotScrollOff yKey
                    crossKey = slotKey slotScrollCross yKey
                    prevY = IM.findWithDefault 0 yKey floats0
                    floats1 = IM.insert yKey prevY $ IM.insert crossKey off floats0
                    points = IM.insert offKey (off, prevY) (storePoint store)
                setStore ctx (store {storeFloat = floats1, storePoint = points})

getScrollOffset2D :: Context -> WidgetId -> IO V2
getScrollOffset2D ctx wid = do
  s <- getStore ctx
  let widKey = intKey wid
      sKey = slotKey slotTextAreaScroll widKey
  v <-
    case IM.lookup sKey (storePoint s) of
      Just (sx, sy) -> pure (V2 sx sy)
      Nothing -> do
        let offKey = slotKey slotScrollOff widKey
            crossKey = slotKey slotScrollCross widKey
        case IM.lookup offKey (storePoint s) of
          Just (x, y) -> pure (V2 x y)
          Nothing ->
            pure
              ( V2
                  (IM.findWithDefault 0 crossKey (storeFloat s))
                  (IM.findWithDefault 0 widKey (storeFloat s))
              )
  sx <- snapScrollOffset ctx (v2X v)
  sy <- snapScrollOffset ctx (v2Y v)
  pure (V2 sx sy)

setScrollOffset2D :: Context -> WidgetId -> V2 -> IO ()
setScrollOffset2D ctx wid off = do
  store <- getStore ctx
  let widKey = intKey wid
      sKey = slotKey slotTextAreaScroll widKey
  -- Text areas only reach the first branch because `textAreaWith` seeds this
  -- slot at init; without the seed a freshly mounted editor falls through to
  -- the legacy container slots below and its offsets are never rendered.
  case IM.lookup sKey (storePoint store) of
    Just (sx, sy) -> do
      let sx' = v2X off
          sy' = v2Y off
      when (sx /= sx' || sy /= sy') $ do
        setStore ctx (store {storePoint = IM.insert sKey (sx', sy') (storePoint store)})
        damageWidget ctx wid DamageSelf
    Nothing -> do
      let offKey = slotKey slotScrollOff widKey
          crossKey = slotKey slotScrollCross widKey
          prev = IM.lookup offKey (storePoint store)
          next = (v2X off, v2Y off)
          prevY = IM.findWithDefault 0 widKey (storeFloat store)
          prevX = IM.findWithDefault 0 crossKey (storeFloat store)
          xLink = IM.findWithDefault 0 (slotKey slotScrollLinkX widKey) (storeInt store)
      when (prev /= Just next || prevY /= v2Y off || prevX /= v2X off) $ do
        let floats0 =
              IM.insert widKey (v2Y off) $
                IM.insert crossKey (v2X off) (storeFloat store)
            floats1 =
              if xLink == 0 then floats0 else IM.insert xLink (v2X off) floats0
        setStore ctx
          ( store
              { storePoint = IM.insert offKey next (storePoint store)
              , storeFloat = floats1
              }
          )

linkScrollAxes :: Context -> WidgetId -> WidgetId -> IO ()
linkScrollAxes ctx yWid xWid = do
  store <- getStore ctx
  let yKey = intKey yWid
      xKey = intKey xWid
      ints =
        IM.insert (slotKey slotScrollLinkX yKey) xKey $
          IM.insert (slotKey slotScrollLinkY xKey) yKey (storeInt store)
  setStore ctx (store {storeInt = ints})
  V2 x2 y <- getScrollOffset2D ctx yWid
  x1 <- do
    s <- getStore ctx
    pure (IM.findWithDefault 0 xKey (storeFloat s))
  let x = if x2 == 0 && x1 /= 0 then x1 else x2
  when (x /= x2 || x /= x1) $
    setScrollOffset2D ctx yWid (V2 x y)

getScrollConfig :: Context -> WidgetId -> IO ScrollConfig
getScrollConfig ctx wid = do
  s <- getStore ctx
  let cfgKey = slotKey slotScrollCfg (intKey wid)
      bits = IM.findWithDefault (encodeScrollConfig defaultScrollConfig) cfgKey (storeInt s)
  pure (decodeScrollConfig bits)

setScrollConfig :: Context -> WidgetId -> ScrollConfig -> IO ()
setScrollConfig ctx wid cfg = do
  store <- getStore ctx
  let cfgKey = slotKey slotScrollCfg (intKey wid)
      bits = encodeScrollConfig cfg
      prev = IM.findWithDefault (encodeScrollConfig defaultScrollConfig) cfgKey (storeInt store)
  when (prev /= bits) $
    setStore ctx (store {storeInt = IM.insert cfgKey bits (storeInt store)})
