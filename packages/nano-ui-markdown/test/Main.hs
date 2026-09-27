module Main (main) where

import Incremental qualified
import Parse qualified
import Render qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parsing" Parse.spec
  describe "incremental parsing" Incremental.spec
  describe "drawing" Render.spec
