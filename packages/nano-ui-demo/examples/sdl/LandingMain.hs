-- | The landing page's program in an SDL window. Run it with
-- @cabal run nano-ui-sdl-landing@.
module Main (main) where

import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

import Landing (landingView)

main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings = defaultWindowSettings {wsTitle = "nano-ui", wsSize = Size 1040 560}
      }
    (landingView True)
