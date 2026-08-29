module NanoUI.Context.Focusables
  ( registerFocusable
  , getFocusables
  ) where

import Data.IORef (readIORef)
import NanoUI.Context.Internal (Context (..), modifyIORefList)
import NanoUI.Id (WidgetId)

{-# INLINE registerFocusable #-}
registerFocusable :: Context -> WidgetId -> IO ()
registerFocusable ctx wid =
  modifyIORefList (ctxFocusables ctx) (wid :)

{-# INLINE getFocusables #-}
getFocusables :: Context -> IO [WidgetId]
getFocusables ctx = reverse <$> readIORef (ctxFocusables ctx)
