module Main (main) where

import ChatDemo (chatDemoUi)
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
      { sdlWindowSettings = defaultWindowSettings {wsTitle = "nano-ui-markdown example", wsSize = Size 900 760}
      , sdlAppShouldQuit = \inp -> inputKeysElem KeyEscape (inputKeys inp)
      }
    chatDemoUi
