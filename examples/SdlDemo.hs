{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import NanoUI
import NanoUI.Backend.Sdl (runSdlAppWithQuit)
import SdlDemoUi (demoImages, demoUi)

main :: IO ()
main = do
  ctx <- newSdlContext
  ok <- registerImages ctx demoImages
  unless ok $ fail "registerImage failed"
  runSdlAppWithQuit
    ctx
    (\inp -> KeyEscape `elem` inputKeys inp)
    demoUi
