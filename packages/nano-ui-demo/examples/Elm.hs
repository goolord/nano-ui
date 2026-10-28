{-# LANGUAGE OverloadedRecordDot #-}

-- | Elm: Elm-style reducer state using nano-ui-emit.
--
-- Run with @cabal run nano-ui-example-elm@.
module Main
  ( main
  , childView
  , parentView
  , ChildMsg (..)
  , ParentMsg (..)
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Backend.Sdl (defaultSdlOptions, runSdlAppReduce)
import NanoUI.Emit

-- ANCHOR: full_app
data Model = Model {counter :: Int, name :: Text}
  deriving Eq

data Msg = Increment | Decrement | SetName Text

view :: Model -> NanoUIE Msg ()
view m = column $ do
  heading "Profile"
  row $ do
    emitWhenE (button "-") Decrement
    label (T.pack (show m.counter))
    emitWhenE (button "+") Increment
  emitChangedE textInput m.name SetName

update :: Msg -> Model -> Model
update Increment m = m {counter = m.counter + 1}
update Decrement m = m {counter = m.counter - 1}
update (SetName n) m = m {name = n}

main :: IO ()
main = runSdlAppReduce defaultSdlOptions update (Model 0 "Ada") view

-- ANCHOR_END: full_app

-- ANCHOR: components
data ChildMsg = ChildIncrement | ChildReset

data ParentMsg = Child ChildMsg | SaveAll

childView :: Int -> NanoUIE ChildMsg ()
childView n = row $ do
  emitWhenE (button "+") ChildIncrement
  label (T.pack (show n))
  emitWhenE (button "Reset") ChildReset

parentView :: Int -> NanoUIE ParentMsg ()
parentView n = column $ do
  mapMessages Child (childView n)
  emitWhenE (button "Save All") SaveAll

-- ANCHOR_END: components
