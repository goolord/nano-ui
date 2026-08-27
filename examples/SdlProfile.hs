module Main (main) where

import Control.Monad (replicateM_, unless, void)
import Data.IORef (newIORef, writeIORef)
import NanoUI
import NanoUI.Backend.Sdl (SdlEnv (..), sdlDrawFrame, syncDisplay, withSdlBench)
import SdlDemoUi (demoImages, demoUi)

-- Enough timed frames for a stable SDL profile without an interactive loop.
iterations :: Int
iterations = 400

-- Fixed input after the first syncDisplay: draw-only profile, no resize/DPI/mouse sync.
profileInput :: Input
profileInput =
  emptyInput
    { inputWindowSize = Size 800 600
    , inputMousePos = V2 400 300
    , inputMouseDown = True
    }

main :: IO ()
main = do
  ctx0 <- newSdlContext
  ok <- registerImages ctx0 demoImages
  unless ok $ fail "registerImage failed"
  envRef <- newIORef (Nothing :: Maybe SdlEnv)
  withSdlBench ctx0 $ \ctx sdlEnv -> do
    writeIORef envRef (Just sdlEnv)
    (ctx', inp) <- syncDisplay ctx sdlEnv profileInput
    let ui = demoUi envRef
    -- One warmup frame, then timed iterations.
    void (sdlDrawFrame ctx' ui sdlEnv inp False)
    replicateM_ iterations (void (sdlDrawFrame ctx' ui sdlEnv inp False))
  putStrLn ("profiled " ++ show iterations ++ " SDL demo frames (plus 1 warmup)")
