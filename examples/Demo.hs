module Main (main) where

import NanoUI
import NanoUI.Testing (newContext, renderASCII, runFrame)

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
  (_, msgs, drawData, _) <-
    runFrame
      ctx
      inp
      ( column
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1})
          ( do
              (tabResp, _) <- tabs ("Controls" :: String)
                [ tab "Controls" "Controls" $ do
                    _ <- button "OK"
                    _ <- button "Cancel"
                    _ <- label "Controls Tab Content"
                    pure ()
                , tab "Settings" "Settings" $ do
                    _ <- label "Settings Tab Content"
                    pure ()
                , tab "Logs" "Logs" $ do
                    _ <- label "Logs Tab Content"
                    pure ()
                ]
              onClick tabResp (pure ())
              label "nano-ui demo"
          )
      )
  putStrLn "=== nano-ui ASCII demo ==="
  mapM_ putStrLn (renderASCII 60 20 drawData)
  putStrLn "--- messages ---"
  print (length msgs)
