-- | Quickstart: the minimal counter application from the Quick Start guide.
--
-- Run with @cabal run nano-ui-example-quickstart@.
-- ANCHOR: quickstart
module Main (main) where

import Data.Text qualified as T
import NanoUI
import NanoUI.Backend.Sdl

main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlWindowSettings =
          defaultWindowSettings {wsTitle = "Hello nano-ui"}
      }
    appView

appView :: NanoUI ()
appView = columnWith (gap 12 . padAll 20 . alignCenter) $ do
  heading "Counter Demo"

  -- State at this call site; allocated on the first frame only.
  n <- useVar (0 :: Int)

  rowWith (gap 8 . alignMid) $ do
    whenM (button "-") (modifyVar n (subtract 1))

    value <- readVar n
    labelWith (fontMono . fontSize 18) (T.pack (show value))

    whenM (button "+") (modifyVar n (+ 1))

  whenM (button "Reset") (writeVar n 0)

-- ANCHOR_END: quickstart
