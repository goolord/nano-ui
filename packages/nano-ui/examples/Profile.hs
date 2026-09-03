module Main (main) where

import Control.Monad (replicateM_, void)
import NanoUI
import NanoUI.Testing (newContext, runFrame)

-- Enough frames for a stable time profile without an interactive window.
iterations :: Int
iterations = 3000

main :: IO ()
main = do
  ctx <- newContext
  let inp =
        emptyInput
          { inputWindowSize = Size 800 600
          , inputMousePos = V2 400 300
          , inputMouseDown = True
          }
      ui =
        column
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1, layoutGap = 8})
          ( do
              replicateM_ 12 $
                row (defaultLayout {layoutGap = 8, layoutWrap = True}) $
                  replicateM_ 8 (void (button "OK"))
              label "nano-ui profile loop"
          )
  replicateM_ iterations (void (runFrame ctx inp ui))
  putStrLn ("profiled " ++ show iterations ++ " frames")
