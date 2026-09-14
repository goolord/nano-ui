module NanoUI.Rgfw.Context
  ( newRgfwContext
  , applyRgfwTheme
  ) where

import NanoUI.Context
  ( Context
  , setDrawExternalText
  , setDrawSquareGeometry
  , setTheme
  , withFontMetrics
  )
import NanoUI.Rgfw.Font.Cozette (cozetteMetrics)
import NanoUI.Rgfw.Theme (RgfwTheme, rgfwCoreTheme)
import NanoUI.Testing (newPixelContext)

-- | Pixel context configured for the RGFW rasterizer: Cozette metrics, square
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
