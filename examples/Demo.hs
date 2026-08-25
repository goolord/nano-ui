module Main where

import NanoUI

main :: IO ()
main = do
  ctx <- newContext
  let inp =
        emptyInput
          { inputWindowSize = Size 60 20
          , inputMousePos = V2 30 10
          , inputMousePressed = True
          , inputMouseDown = True
          }
  (_, msgs, drawData) <-
    runFrame
      ctx
      inp
      ( column
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1})
          ( do
              button "OK"
              button "Cancel"
              label "nano-ui demo"
          )
      )
  putStrLn "=== nano-ui ASCII demo ==="
  mapM_ putStrLn (renderASCII 60 20 drawData)
  putStrLn "--- messages ---"
  print (length msgs)
