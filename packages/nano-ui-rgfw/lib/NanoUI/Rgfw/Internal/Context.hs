-- | Context setup for RGFW: Cozette metrics, square geometry, text drawn
-- from spans, and square themes.
module NanoUI.Rgfw.Internal.Context
  ( newRgfwContext
  , applyRgfwTheme
  , TextSpan
  , paintInLayerOrder
  ) where

import Data.Text (Text)
import NanoUI (Color, Rect, Theme, borderWidth, cornerRadius, everyStyle)
import NanoUI.Backend (measureTextIO)
import NanoUI.Internal.Context
  ( Context
  , setDrawExternalText
  , setDrawSquareGeometry
  , setTheme
  , withFontMetrics
  , withFontResolver
  , withMonoFontMetrics
  )
import NanoUI.Rgfw.Internal.Font.Cozette (cozetteMetrics)
import NanoUI.Testing (Layer (..), newPixelContext)

-- | Pixel context configured for RGFW: Cozette metrics, square geometry
-- (every primitive reaches the renderer as a flat quad or triangle), text
-- left to the span stamper, and the given theme made square
-- ('applyRgfwTheme'). Sessions, tests and profiles share it so they render
-- the same frame.
--
-- The span stamper draws every span in Cozette's one 6x13 cell, so text of
-- every size and face, monospace included, is laid out in that cell: font
-- sizes change nothing here. Text in drawings and canvases (rich text,
-- Markdown, chart labels) reaches it as spans too, one per line, which
-- 'NanoUI.Testing.collectRasterSpans' returns after the widget text.
newRgfwContext :: Theme -> IO Context
newRgfwContext theme = do
  ctx0 <- newPixelContext
  -- The resolver goes last: the metric setters reinstall the default one,
  -- which scales the metrics to the size asked for.
  let ctx =
        withFontResolver
          (withMonoFontMetrics (withFontMetrics ctx0 cozetteMetrics) cozetteMetrics)
          (\_ _ _ _ -> pure (cozetteMetrics, False))
          (\_ _ _ _ -> measureTextIO cozetteMetrics)
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
-- text spans. Each text list holds drawing text too, the page's in the base
-- spans and floating panels' in the overlay ones, so it is drawn above
-- every shape of its layer, including its own drawing's later shapes.
paintInLayerOrder :: (Layer -> IO ()) -> IO () -> IO () -> IO ()
paintInLayerOrder geometry baseText overlayText = do
  geometry LayerBackground
  geometry LayerContent
  baseText
  geometry LayerOverlay
  geometry LayerChrome
  overlayText
