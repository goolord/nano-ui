-- | Hello: the smallest complete nano-ui app. Start here.
--
-- The view is a function that runs every frame. Nothing persists between
-- frames except the state you keep, and each widget call draws the widget
-- and reports what the user did with it this frame: 'button' returns 'True'
-- on the frame it is clicked.
--
-- State lives in a 'Var'. 'useVar' keeps one at its place in the view with
-- no setup. An input takes the Var it edits, so typing into 'textInput'
-- changes @name@ with nothing more to write. 'readVar' reads a Var where you
-- show it.
--
-- Next: State.hs (where state lives), Layout.hs (arranging widgets),
-- Components.hs (your own widgets), Forms.hs (validating and submitting).
--
-- Escape quits.
--
-- Run it with @cabal run nano-ui-example-hello@.
module Main (main) where

import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings = defaultWindowSettings {wsTitle = "Hello", wsSize = Size 480 300}
      }
    hello

hello :: NanoUI ()
hello = columnWith (padAll 20 . gap 12 . fillW) $ do
  heading "Hello, nano-ui"
  name <- useVar ""
  textInput name
  -- Read below the field that edits it, so the greeting shows this frame's
  -- typing.
  current <- readVar name
  label (if T.null current then "What's your name?" else "Hello, " <> current <> "!")
  clicks <- useVar (0 :: Int)
  rowWith (tight . gap 12 . alignMid) $ do
    whenM (button "Click me") (modifyVar clicks (+ 1))
    n <- readVar clicks
    labelWith alignMid ("Clicked " <> T.pack (show n) <> " times")
