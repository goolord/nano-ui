module Main (main) where

import FormDemo (formDemoUi)
import NanoUI.Form (newFormState)
import NanoUI (Key (KeyEscape), Pressable (..), Size (..), WindowSettings (..), defaultWindowSettings)
import NanoUI.Backend.Sdl
  ( SdlOptions (..)
  , defaultSdlOptions
  , runSdlApp
  )

main :: IO ()
main = do
  owner <- newFormState
  runSdlApp
    defaultSdlOptions
      { sdlWindowSettings = defaultWindowSettings {wsTitle = "nano-ui-form example", wsSize = Size 1100 800}
      , sdlAppShouldQuit = pressedOnceIn KeyEscape
      }
    (formDemoUi owner)
