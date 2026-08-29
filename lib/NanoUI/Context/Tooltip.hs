module NanoUI.Context.Tooltip
  ( clearTooltips
  , pushTooltip
  , readTooltips
  ) where

import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import NanoUI.Context.Internal
  ( Context (..)
  , PendingTooltip (..)
  , modifyIORefList
  )
import NanoUI.Id (WidgetId)
import NanoUI.Types (Rect)

{-# INLINE clearTooltips #-}
clearTooltips :: Context -> IO ()
clearTooltips ctx = writeIORef (ctxTooltips ctx) []

{-# INLINE pushTooltip #-}
pushTooltip :: Context -> WidgetId -> Rect -> Text -> IO ()
pushTooltip ctx wid rect txt =
  modifyIORefList (ctxTooltips ctx) (PendingTooltip wid rect txt :)

{-# INLINE readTooltips #-}
readTooltips :: Context -> IO [PendingTooltip]
readTooltips ctx = readIORef (ctxTooltips ctx)
