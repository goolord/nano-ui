module Main (main) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import NanoUI
import NanoUI.Backend.Term (runTermAppWithQuit)
import qualified Data.Text as T
-- Terminal cells are the layout unit here, so the pixel-oriented defaults
-- (4 units of padding and gap) would leave huge holes between rows.
compact :: Layout
compact =
  defaultLayout
    { layoutPadding = Padding 0 0 0 0
    , layoutGap = 0
    }

main :: IO ()
main = do
  ctx <- newTerminalContext
  lastClick <- newIORef ("" :: String)
  runTermAppWithQuit ctx (\inp -> KeyEscape `elem` inputKeys inp) $
    column
      compact
        { layoutWidth = Grow 1
        , layoutHeight = Grow 1
        }
      ( panel
          compact
            { layoutWidth = Grow 1
            , layoutHeight = Grow 1
            , layoutPadding = Padding 2 2 1 1
            }
          ( column compact $ do
              _ <- label "nano-ui terminal"
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
              (_, vol) <- slider defaultLayout "Volume" 0 100 50
              (_, quality) <- select "Quality" ["Low", "High"] 0
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
                          <> "  quality="
                          <> show quality
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
