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
        columnWith
          (grow . gap 8)
          ( do
              replicateM_ 12 $
                gridWith 8 (gap 8) $
                  replicateM_ 8 (void (button "OK"))
              label "nano-ui profile loop"
          )
  replicateM_ iterations (void (runFrame ctx inp ui))
  putStrLn ("profiled " ++ show iterations ++ " frames")
