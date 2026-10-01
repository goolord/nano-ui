-- | Hello: the smallest complete nano-ui app. Start here.
--
-- The view is a function that runs every frame. Nothing persists between
-- frames except the state you keep, and each widget call both draws the
-- widget and returns what the user did with it this frame: 'button' returns
-- 'True' on the frame it is clicked, 'textInput' returns the text after this
-- frame's typing.
--
-- Inputs are controlled: you pass the current value in and store the value
-- that comes back. Forget to store it and the edit is undone on the next
-- frame, because the next frame passes the old value in again.
--
-- 'useText' and 'useInt' keep a value between frames with no setup. They
-- are positional, so call them in the same order every frame.
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
  (name, setName) <- useText ""
  -- Store what the field returns, and use it right away for the greeting.
  name' <- textInput name
  setName name'
  label (if T.null name' then "What's your name?" else "Hello, " <> name' <> "!")
  (clicks, setClicks) <- useInt 0
  rowWith (tight . gap 12 . alignMid) $ do
    whenM (button "Click me") (setClicks (clicks + 1))
    labelWith alignMid ("Clicked " <> T.pack (show clicks) <> " times")
