-- | Context setup for RGFW: Cozette metrics, square geometry, text drawn
-- from spans, and square themes.
module NanoUI.Rgfw.Context
  ( newRgfwContext
  , applyRgfwTheme
  , TextSpan
  , paintInLayerOrder
  ) where

import Data.Text (Text)
import NanoUI (Color, Rect, Theme, borderWidth, cornerRadius, everyStyle)
import NanoUI.Context
  ( Context
  , setDrawExternalText
  , setDrawSquareGeometry
  , setTheme
  , withFontMetrics
  )
import NanoUI.Rgfw.Font.Cozette (cozetteMetrics)
import NanoUI.Testing (Layer (..), newPixelContext)

-- | Pixel context configured for RGFW: Cozette metrics, square geometry
-- (every primitive reaches the renderer as a flat quad or triangle), text
-- left to the span stamper, and the given theme made square
-- ('applyRgfwTheme'). Sessions, tests and profiles share it so they render
-- the same frame.
newRgfwContext :: Theme -> IO Context
newRgfwContext theme = do
  ctx0 <- newPixelContext
  let ctx = withFontMetrics ctx0 cozetteMetrics
  setDrawSquareGeometry ctx True
  setDrawExternalText ctx True
  applyRgfwTheme ctx theme
  pure ctx

-- | Switch to a theme with every surface square: corner radius 0 and a 1px
-- border, so geometry matches hit boxes. A no-op when it is unchanged;
-- otherwise caches are dropped and the context is marked dirty.
applyRgfwTheme :: Context -> Theme -> IO ()
applyRgfwTheme ctx = setTheme ctx . everyStyle (cornerRadius 0 . borderWidth 1)

-- | A collected text span (see 'NanoUI.Testing.collectRasterSpans'): rect,
-- text, foreground, background, and clip, in logical pixels.
type TextSpan = (Rect, Text, Color, Color, Rect)

-- | The paint order the OpenGL renderer and the test suite's software
-- rasteriser follow: background and content
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
