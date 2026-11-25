{-# OPTIONS_HADDOCK not-home #-}

-- | What the browser session reports about itself: the core's frame timings
-- with the canvas, its pixel ratio, the UI font and the WebGL renderer.
module NanoUI.SdlWeb.Internal.Debug
  ( SdlWebDebugSnapshot (..)
  ) where

import Data.Text (Text)
import NanoUI.Internal.Debug (CoreDebugSnapshot)

-- | A sample of the session, refreshed at most four times a second while a
-- view asks for it. The core's window size is the canvas's CSS box in
-- layout units.
data SdlWebDebugSnapshot = SdlWebDebugSnapshot
  { dbgCore :: !CoreDebugSnapshot
  , dbgScale :: !Float
  -- ^ Drawing-buffer pixels per layout unit: the pixel ratio times the UI
  -- scale.
  , dbgPixelRatio :: !Float
  -- ^ The page's @devicePixelRatio@, which follows the browser's zoom.
  , dbgFont :: !String
  -- ^ The UI font: the bundled Inter, an installed family or a URL.
  , dbgRenderer :: !Text
  -- ^ The WebGL renderer, as the browser names it.
  , dbgRefreshHz :: !Int
  -- ^ The rate frames are paced at while something animates.
  }
  deriving (Eq, Show)
