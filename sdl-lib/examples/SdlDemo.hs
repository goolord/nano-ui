{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)
import SdlDemoUi (demoImages, demoUi)

main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = \inp -> KeyEscape `elem` inputKeys inp
      , sdlAppImages = demoImages
      }
    demoUi
