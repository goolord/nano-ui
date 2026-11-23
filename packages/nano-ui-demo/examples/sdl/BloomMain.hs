-- | "Bloom" in an SDL window. Run it with @cabal run nano-ui-example-bloom@.
module Main (main) where

import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

import Bloom (bloomView)

main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings = defaultWindowSettings {wsTitle = "Bloom", wsSize = Size 900 540}
      }
    bloomView
