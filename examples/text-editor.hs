{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import NanoUI
import Graphics.Gloss hiding (text)
import Control.Monad.IO.Class
import Control.Monad.Freer.Reader
import Data.IORef
import Control.Monad (when)
import Data.Foldable (for_)

main :: IO ()
main = do
  msgs <- newIORef []

  defaultMain $ do

    text "File name:"
    fileName <- input 0 "File name" ""
    text fileName
  
    didSaveText <- textP "Save"
    didSave <- button (PictureI $ color white didSaveText) 75.0 30.0

    when didSave $ liftIO (modifyIORef' msgs (fileName :))

    mousePosRef <- mouse <$> ask
    mousePos <- liftIO $ readIORef mousePosRef
    text (show mousePos)

    mapM_ text =<< liftIO (readIORef msgs)

