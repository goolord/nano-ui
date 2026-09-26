module Main (main) where

import ChatDemo (chatDemoUi)
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
      { sdlWindowTitle = "nano-ui-markdown example"
      , sdlWindowSize = Size 900 760
      , sdlAppShouldQuit = \inp -> inputKeysElem KeyEscape (inputKeys inp)
      }
    chatDemoUi
