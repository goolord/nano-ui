module Main (main) where

import Control.Monad (replicateM_, void)
import Data.Foldable (foldlM)
import Data.Primitive.SmallArray (SmallArray)
import NanoUI
import NanoUI.Backend.Sdl (RgbaImage (..))
import NanoUI.Testing (Context, registerImage)
import NanoUI.Backend.Sdl (newSdlContext, sdlDrawFrame, syncDisplay, withSdlBench)
import SdlDemo (demoImages, demoUi)

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
  ok <- registerDemoImages ctx0 demoImages
  if not ok
    then fail "registerImage failed"
    else
      withSdlBench ctx0 $ \ctx sdlEnv -> do
        (ctx', inp) <- syncDisplay ctx sdlEnv profileInput
        void (sdlDrawFrame ctx' demoUi sdlEnv inp False)
        replicateM_ iterations (void (sdlDrawFrame ctx' demoUi sdlEnv inp False))
  putStrLn ("profiled " ++ show iterations ++ " SDL demo frames (plus 1 warmup)")

registerDemoImages :: Context -> SmallArray RgbaImage -> IO Bool
registerDemoImages ctx images =
  foldlM
    ( \ok img ->
        if ok
          then
            registerImage
              ctx
              (rgbaImageId img)
              (rgbaImageWidth img)
              (rgbaImageHeight img)
              (rgbaImagePixels img)
          else pure False
    )
    True
    images
