module NanoUI.Context.Focus
  ( getFocusId
  , getHotId
  ) where

import Data.IORef (readIORef)
import NanoUI.Context.Internal (Context (..))
import NanoUI.Id (WidgetId)

{-# INLINE getFocusId #-}
getFocusId :: Context -> IO WidgetId
getFocusId ctx = readIORef (ctxFocusId ctx)

{-# INLINE getHotId #-}
getHotId :: Context -> IO WidgetId
getHotId ctx = readIORef (ctxHotId ctx)
