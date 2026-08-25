{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import NanoUI
import NanoUI.Backend.Sdl (runSdlAppWithQuit)
import qualified Data.Text as T

main :: IO ()
main = do
  ctx <- newContext
  lastClick <- newIORef ("" :: String)
  runSdlAppWithQuit ctx (\inp -> KeyEscape `elem` inputKeys inp) $
    column
      defaultLayout
        { layoutWidth = Grow 1
        , layoutHeight = Grow 1
        }
      ( panel
          defaultLayout
            { layoutWidth = Grow 1
            , layoutHeight = Grow 1
            }
          ( column defaultLayout $ do
              _ <- label "nano-ui SDL3"
              _ <- label ""
              ok <- button "OK"
              when (respClicked ok) $ do
                liftIO $ writeIORef lastClick "OK"
                emit ("button:OK" :: String)
              cancel <- button "Cancel"
              when (respClicked cancel) $ do
                liftIO $ writeIORef lastClick "Cancel"
                emit ("button:Cancel" :: String)
              (_, checked) <- checkbox "Feature" False
              (_, vol) <- slider "Volume" 0 100 50
              (_, name) <- textInput "Name" ""
              click <- liftIO $ readIORef lastClick
              _ <- label ""
              _ <-
                label
                  ( T.pack
                      ( "checked="
                          <> show checked
                          <> "  vol="
                          <> show (round vol :: Int)
                          <> "  name="
                          <> name
                          <> "  click="
                          <> click
                      )
                  )
              _ <- label "click widgets, type in Name, Esc quits"
              pure ()
          )
      )
