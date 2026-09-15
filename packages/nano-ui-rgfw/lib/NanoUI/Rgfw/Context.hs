module NanoUI.Rgfw.Context
  ( newRgfwContext
  , applyRgfwTheme
  , TextSpan
  , paintInLayerOrder
  ) where

import Data.Text (Text)
import NanoUI (Color, Rect)
import NanoUI.Context
  ( Context
  , setDrawExternalText
  , setDrawSquareGeometry
  , setTheme
  , withFontMetrics
  )
import NanoUI.Rgfw.Font.Cozette (cozetteMetrics)
import NanoUI.Rgfw.Theme (RgfwTheme, rgfwCoreTheme)
import NanoUI.Testing (Layer (..), newPixelContext)

-- | Pixel context configured for the RGFW renderers: Cozette metrics, square
-- geometry (every primitive reaches 'NanoUI.Rgfw.Render.renderArena' as a flat
-- quad or triangle), text left to the span stamper, and the core theme derived
-- from the RGFW palette. Sessions, tests and profiles share it so they render
-- the same frame.
newRgfwContext :: RgfwTheme -> IO Context
newRgfwContext theme = do
  ctx0 <- newPixelContext
  let ctx = withFontMetrics ctx0 cozetteMetrics
  setDrawSquareGeometry ctx True
  setDrawExternalText ctx True
  applyRgfwTheme ctx theme
  pure ctx

-- | Switch the core theme to an RGFW palette. A no-op when it is unchanged;
-- otherwise caches are dropped and the context is marked dirty.
applyRgfwTheme :: Context -> RgfwTheme -> IO ()
applyRgfwTheme ctx = setTheme ctx . rgfwCoreTheme

-- | A collected text span (see 'NanoUI.Testing.collectRasterSpans'): rect,
-- text, foreground, background, and clip, in logical pixels.
type TextSpan = (Rect, Text, Color, Color, Rect)

-- | The paint order both RGFW renderers follow: background and content
-- geometry, base text spans (after content, so scroll tracks cannot erase box
-- rules), overlay geometry, floating chrome (window scrollbars), then overlay
-- text spans.
paintInLayerOrder :: (Layer -> IO ()) -> IO () -> IO () -> IO ()
paintInLayerOrder geometry baseText overlayText = do
  geometry LayerBackground
  geometry LayerContent
  baseText
  geometry LayerOverlay
  geometry LayerChrome
  overlayText
