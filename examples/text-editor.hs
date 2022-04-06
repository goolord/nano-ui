module Main where

import MyLib
import Graphics.Gloss
import Control.Monad.IO.Class

main :: IO ()
main = defaultMainWith defaultSettings $ do
  didSave <- button (Text "Save") 5.0 2.0
  if didSave
    then liftIO $ putStrLn "you saved!"
    else pure ()
