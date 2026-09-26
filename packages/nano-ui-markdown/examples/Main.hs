module Main (main) where

import ChatDemo (chatDemoUi)
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
      { sdlWindowSettings = defaultWindowSettings {wsTitle = "nano-ui-markdown example", wsSize = Size 900 760}
      , sdlAppShouldQuit = pressedOnceIn KeyEscape
      }
    chatDemoUi
