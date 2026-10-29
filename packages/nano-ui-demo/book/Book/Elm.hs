-- | Code for the book's Elm-Style State chapter (@book/src/state/elm.md@).
-- The full example is @examples/Elm.hs@.
module Book.Elm where

import Data.Text (Text)
import NanoUI.Backend.Sdl (defaultSdlOptions, runSdlAppReduce)
import NanoUI.Emit

-- ANCHOR: emit
settingsForm :: Float -> NanoUIE Msg ()
settingsForm volume =
  column $ do
    label "Settings"
    withKey ("form" :: Text) $ do
      emitChanged (slider 0 100) volume SetVolume
      emitWhen (button "Save") Save
-- ANCHOR_END: emit

-- ANCHOR: run
main :: IO ()
main = runSdlAppReduce defaultSdlOptions update initialModel view
-- ANCHOR_END: run

-- Stand-ins for application code the snippets refer to.

data Msg = SetVolume Float | Save

newtype Model = Model Float
  deriving (Eq)

initialModel :: Model
initialModel = Model 40

update :: Msg -> Model -> Model
update (SetVolume v) _ = Model v
update Save m = m

view :: Model -> NanoUIE Msg ()
view (Model v) = settingsForm v
