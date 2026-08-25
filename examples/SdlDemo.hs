{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import NanoUI
import NanoUI.Backend.Sdl (runSdlAppWithQuit)
import qualified Data.Text as T

outerPad :: Padding
outerPad = Padding 20 20 20 20

panelPad :: Padding
panelPad = Padding 24 24 24 24

contentGap :: Float
contentGap = 10

main :: IO ()
main = do
  ctx <- newSdlContext
  lastClick <- newIORef ("" :: String)
  runSdlAppWithQuit ctx (\inp -> KeyEscape `elem` inputKeys inp) $
    column
      defaultLayout
        { layoutWidth = Grow 1
        , layoutHeight = Grow 1
        , layoutPadding = outerPad
        , layoutGap = 0
        }
      ( panel
          defaultLayout
            { layoutWidth = Grow 1
            , layoutHeight = Grow 1
            , layoutPadding = panelPad
            , layoutGap = contentGap
            }
          (           column
              defaultLayout
                { layoutGap = contentGap
                , layoutWidth = Grow 1
                }
              $ do
                  _ <- label "nano-ui"
                  _ <- label "SDL3 demo"
                  _ <- separator
                  row
                    defaultLayout
                      { layoutGap = 8
                      }
                    $ do
                      ok <- button "OK"
                      when (respClicked ok) $ do
                        liftIO $ writeIORef lastClick "OK"
                        emit ("button:OK" :: String)
                      cancel <- button "Cancel"
                      when (respClicked cancel) $ do
                        liftIO $ writeIORef lastClick "Cancel"
                        emit ("button:Cancel" :: String)
                      pure ()
                  (_, checked) <- checkbox "Feature" False
                  (_, vol) <- slider (defaultLayout {layoutWidth = Grow 1}) "Volume" 0 100 50
                  (_, name) <- textInput "Name" ""
                  click <- liftIO $ readIORef lastClick
                  _ <- separator
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
                  _ <- label "Click widgets, type in Name, Esc quits"
                  pure ()
          )
      )
