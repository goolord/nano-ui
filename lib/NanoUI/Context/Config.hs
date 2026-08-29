module NanoUI.Context.Config
  ( withFontMetrics
  , withMonoFontMetrics
  , withMeasureText
  , wrapMeasureCache
  , clearMeasureCache
  , withExternalText
  , withTheme
  , withIcons
  , withHostProfile
  , withClipboard
  , enableMeasureCache
  ) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import NanoUI.Context.Internal (Context (..))
import NanoUI.Font (FontMetrics, hasMonoFontMarker, measureText, stripWidgetMarkers)
import NanoUI.Host (HostProfile (..))
import NanoUI.Icons (IconSet, iconsFor)
import NanoUI.Style (Theme)

fontMetricsForText :: Context -> Text -> FontMetrics
fontMetricsForText ctx txt =
  if hasMonoFontMarker txt
    then ctxMonoFontMetrics ctx
    else ctxFontMetrics ctx

{-# INLINE withFontMetrics #-}
withFontMetrics :: Context -> FontMetrics -> Context
withFontMetrics ctx fm =
  ctx
    { ctxFontMetrics = fm
    , ctxMeasureText =
        \txt ->
          pure (measureText (ctxHostProfile ctx) (fontMetricsForText ctx {ctxFontMetrics = fm} txt) (stripWidgetMarkers txt))
    }

{-# INLINE withMonoFontMetrics #-}
withMonoFontMetrics :: Context -> FontMetrics -> Context
withMonoFontMetrics ctx monoFm = ctx {ctxMonoFontMetrics = monoFm}

{-# INLINE withMeasureText #-}
withMeasureText :: Context -> (Text -> IO (Float, Float)) -> Context
withMeasureText ctx measure = ctx {ctxMeasureText = measure}

{-# INLINE wrapMeasureCache #-}
wrapMeasureCache :: Float -> Context -> (Text -> IO (Float, Float)) -> Context
wrapMeasureCache scale ctx measure =
  case ctxMeasureCache ctx of
    Nothing -> ctx {ctxMeasureText = measure}
    Just cacheRef ->
      ctx
        { ctxMeasureText = \txt -> do
            let mono = hasMonoFontMarker txt
                key = (stripWidgetMarkers txt, mono, scale)
            cache <- readIORef cacheRef
            case Map.lookup key cache of
              Just wh -> pure wh
              Nothing -> do
                wh <- measure txt
                writeIORef cacheRef (Map.insert key wh cache)
                pure wh
        }

{-# INLINE clearMeasureCache #-}
clearMeasureCache :: Context -> IO ()
clearMeasureCache ctx =
  case ctxMeasureCache ctx of
    Nothing -> pure ()
    Just cacheRef -> writeIORef cacheRef Map.empty

{-# INLINE withExternalText #-}
withExternalText :: Context -> Bool -> Context
withExternalText ctx on = ctx {ctxExternalText = on}

{-# INLINE withTheme #-}
withTheme :: Context -> Theme -> Context
withTheme ctx theme = ctx {ctxTheme = theme}

{-# INLINE withIcons #-}
withIcons :: Context -> IconSet -> Context
withIcons ctx set = ctx {ctxIcons = iconsFor set}

{-# INLINE withHostProfile #-}
withHostProfile :: Context -> HostProfile -> Context
withHostProfile ctx host =
  let ctx' = ctx {ctxHostProfile = host}
   in ctx'
        { ctxMeasureText =
            \txt ->
              pure (measureText host (fontMetricsForText ctx' txt) (stripWidgetMarkers txt))
        }

{-# INLINE enableMeasureCache #-}
enableMeasureCache :: Context -> IO Context
enableMeasureCache ctx = do
  cacheRef <- newIORef Map.empty
  pure (ctx {ctxMeasureCache = Just cacheRef})

{-# INLINE withClipboard #-}
withClipboard :: Context -> IO (Maybe String) -> (String -> IO Bool) -> Context
withClipboard ctx get set =
  ctx {ctxClipboardGet = get, ctxClipboardSet = set}
