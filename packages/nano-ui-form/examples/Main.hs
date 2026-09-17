module Main (main) where

import FormDemo (formDemoUi)
import NanoUI (Key (KeyEscape), Size (..), inputKeys, inputKeysElem)
import NanoUI.Backend.Sdl
  ( SdlOptions (..)
  , defaultSdlOptions
  , runSdlApp
  )

main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlWindowTitle = "nano-ui-form example"
      , sdlWindowSize = Size 1100 800
      , sdlAppShouldQuit = \inp -> inputKeysElem KeyEscape (inputKeys inp)
      }
    formDemoUi
