{-# LANGUAGE OverloadedStrings #-}

module Main where

import NanoUI
import Graphics.Gloss hiding (text)
import Control.Monad.IO.Class
import Graphics.Text.TrueType (FontStyle(FontStyle), FontDescriptor (FontDescriptor))
import Polysemy.State
import qualified Graphics.Text.TrueType as TT
import Data.IORef

firaCode = FontDescriptor "Open Sans" (FontStyle False False)
text = renderFont firaCode (TT.PointSize 16)

main :: IO ()
main = defaultMain $ do
  didSaveText <- text "Save"
  didSave <- button (color white didSaveText) 75.0 30.0
  if didSave
    then liftIO $ putStrLn "you saved!"
    else pure ()
  mousePosRef <- cursorPos <$> get
  mousePos <- liftIO $ readIORef mousePosRef
  pictureI . color white . translate (-100.0) (-100.0) =<< text (show mousePos)
