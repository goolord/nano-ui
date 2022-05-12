{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import NanoUI
import Graphics.Gloss hiding (text)
import Control.Monad.IO.Class
import Graphics.Text.TrueType (FontStyle(FontStyle), FontDescriptor (FontDescriptor))
import Control.Monad.Freer.Reader
import qualified Graphics.Text.TrueType as TT
import Data.IORef
import Control.Monad (when)

firaCode :: FontDescriptor
firaCode = FontDescriptor "Open Sans" (FontStyle False False)

text :: String -> GUIM Picture
text = renderFont firaCode (TT.PointSize 16)

main :: IO ()
main = do
  msgs <- newIORef []

  defaultMain $ do
  
    didSaveText <- text "Save"
    didSave <- button (PictureI $ color white didSaveText) 75.0 30.0

    when didSave $ liftIO (modifyIORef' msgs ("YOU MUST DIE!!!!" :))

    pics <- do
      msgs' <- liftIO (readIORef msgs)
      pics <- mapM text msgs'
      pure $ fmap (color white) pics
    mapM_ pictureI pics

    mousePosRef <- mouse <$> ask
    mousePos <- liftIO $ readIORef mousePosRef
    pictureI . color white =<< text (show mousePos)
