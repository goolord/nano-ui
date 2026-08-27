{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.IORef (newIORef, writeIORef)
import NanoUI
import NanoUI.Backend.Sdl (SdlEnv, runSdlAppWith)
import SdlDemoUi (demoImages, demoUi)

main :: IO ()
main = do
  ctx <- newSdlContext
  ok <- registerImages ctx demoImages
  unless ok $ fail "registerImage failed"
  envRef <- newIORef (Nothing :: Maybe SdlEnv)
  runSdlAppWith
    ctx
    (\env -> writeIORef envRef (Just env))
    (\inp -> KeyEscape `elem` inputKeys inp)
    (demoUi envRef)
