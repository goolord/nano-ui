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
  aboutOpen <- newIORef False
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
                      about <- button "About"
                      when (respClicked about) $ liftIO $ writeIORef aboutOpen True
                      pure ()
                  (_, checked) <- checkbox "Feature" False
                  (_, _) <-
                    scrollArea
                      (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 80})
                      ( column defaultLayout $ do
                          _ <- label "Scrollable list"
                          mapM_ (\i -> label (T.pack ("Item " <> show (i :: Int)))) [1 .. 12]
                          pure ()
                      )
                  (_, vol) <- slider (defaultLayout {layoutWidth = Grow 1}) "Volume" 0 100 50
                  (_, quality) <- select "Quality" ["Low", "Medium", "High"] 1
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
                              <> "  quality="
                              <> show quality
                              <> "  name="
                              <> name
                              <> "  click="
                              <> click
                          )
                      )
                  _ <- label "Click widgets, type in Name. Esc closes About, then quits"
                  showAbout <- liftIO (readIORef aboutOpen)
                  (aboutResp, _) <-
                    modal showAbout "About" $ do
                      _ <- label "nano-ui SDL demo"
                      close <- button "Close"
                      when (respClicked close) $ liftIO $ writeIORef aboutOpen False
                      pure ()
                  when (respClicked aboutResp) $ liftIO $ writeIORef aboutOpen False
                  pure ()
          )
      )
