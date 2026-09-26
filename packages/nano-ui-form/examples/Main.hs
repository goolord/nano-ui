module Main (main) where

import FormDemo (formDemoUi)
import NanoUI (Key (KeyEscape), Size (..), WindowSettings (..), defaultWindowSettings, inputKeys, inputKeysElem)
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
      , sdlAppShouldQuit = \inp -> inputKeysElem KeyEscape (inputKeys inp)
      }
    formDemoUi
