-- | Native SDL3 entry point for the landing-page demo. Run with
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
