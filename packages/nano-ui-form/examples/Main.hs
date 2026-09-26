module Main (main) where

import FormDemo (formDemoUi)
import NanoUI (Key (KeyEscape), Pressable (..), Size (..), WindowSettings (..), defaultWindowSettings)
import NanoUI.Backend.Sdl
  ( SdlOptions (..)
  , defaultSdlOptions
  , runSdlApp
  )

main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlWindowSettings = defaultWindowSettings {wsTitle = "nano-ui-form example", wsSize = Size 1100 800}
      , sdlAppShouldQuit = pressedOnceIn KeyEscape
      }
    formDemoUi
