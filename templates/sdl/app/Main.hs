module Main (main) where

import Data.Text qualified as T
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlWindowSettings =
          defaultWindowSettings
            { wsTitle = "my-app"
            , wsSize = Size 480 280
            }
      }
    appView

appView :: NanoUI ()
appView = columnWith (grow . padAll 24 . gap 16) $ do
  name <- useVar ("world" :: T.Text)
  count <- useVar (0 :: Int)

  heading "Hello, nano-ui"
  textInput name

  rowWith (tight . gap 8 . itemsCenter) $ do
    whenM (button "-") (modifyVar count (subtract 1))
    whenM (styled primary (button "+")) (modifyVar count (+ 1))
    n <- readVar count
    who <- readVar name
    let greeting = if T.null (T.strip who) then "world" else who
    label ("Hello, " <> greeting <> " (" <> T.pack (show n) <> ")")
