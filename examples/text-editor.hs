{-# LANGUAGE OverloadedStrings #-}

module Main where

import MyLib
import Graphics.Gloss
import Control.Monad.IO.Class
import Graphics.Text.TrueType (FontStyle(FontStyle), FontDescriptor (FontDescriptor))
import qualified Graphics.Text.TrueType as TT

firaCode = FontDescriptor "Fira Sans" (FontStyle False False)

main :: IO ()
main = defaultMain $ do
  didSaveText <- renderFont firaCode (TT.PointSize 16) "Save"
  didSave <- button [didSaveText] 75.0 30.0
  if didSave
    then liftIO $ putStrLn "you saved!"
    else pure ()
