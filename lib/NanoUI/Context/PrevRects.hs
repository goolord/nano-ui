module NanoUI.Context.PrevRects
  ( getPrevRectByKey
  , getPrevRect
  , setPrevRect
  ) where

import Data.IORef (readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import NanoUI.Context.Internal (Context (..), intKey)
import NanoUI.Id (WidgetId)
import NanoUI.Types (Rect)

{-# INLINE getPrevRectByKey #-}
getPrevRectByKey :: Context -> Int -> IO (Maybe Rect)
getPrevRectByKey ctx key =
  readIORef (ctxPrevRects ctx) >>= pure . IM.lookup key

{-# INLINE getPrevRect #-}
getPrevRect :: Context -> WidgetId -> IO (Maybe Rect)
getPrevRect ctx wid = getPrevRectByKey ctx (intKey wid)

{-# INLINE setPrevRect #-}
setPrevRect :: Context -> WidgetId -> Rect -> IO ()
setPrevRect ctx wid rect = do
  rects <- readIORef (ctxPrevRects ctx)
  writeIORef (ctxPrevRects ctx) (IM.insert (intKey wid) rect rects)
