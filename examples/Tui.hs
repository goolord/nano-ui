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
          $ do
            (_, _) <-
              scrollArea
                compact
                  { layoutWidth = Grow 1
                  , layoutHeight = Grow 1
                  }
                $ column compact $ do
              _ <- label "nano-ui terminal"
              _ <-
                labelEx
                  (compact {layoutMaxW = 28})
                  "This label wraps inside a max width so long lines break cleanly."
              _ <-
                row
                  (compact {layoutWrap = True, layoutWidth = Grow 1, layoutMaxW = 24})
                  ( do
                      _ <- label "[wrap]"
                      _ <- label "alpha"
                      _ <- label "beta"
                      _ <- label "gamma"
                      pure ()
                  )
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
              (_, vol) <- slider (defaultLayout {layoutWidth = Grow 1}) "Volume" 0 100 50
              (_, quality) <- select "Quality" ["Low", "High"] 0
              (_, name) <- textInput "Name" ""
              (_, _) <-
                scrollArea
                  ( compact
                      { layoutWidth = Grow 1
                      , layoutHeight = Fixed 4
                      }
                  )
                  ( column compact $ do
                      _ <- label "Scroll line 1"
                      _ <- label "Scroll line 2"
                      _ <- label "Scroll line 3"
                      _ <- label "Scroll line 4"
                      _ <- label "Scroll line 5"
                      _ <- label "Scroll line 6"
                      pure ()
                  )
              click <- liftIO $ readIORef lastClick
              _ <- label ""
              _ <-
                labelEx
                  (compact {layoutWidth = Grow 1})
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
              row
                compact
                  { layoutWrap = True
                  , layoutWidth = Grow 1
                  }
                $ do
                  _ <- image (compact {layoutWidth = Fixed 8, layoutHeight = Fixed 2}) (ImageId 1)
                  _ <- image (compact {layoutWidth = Fixed 8, layoutHeight = Fixed 2}) (ImageId 2)
                  _ <- image (compact {layoutWidth = Fixed 8, layoutHeight = Fixed 2}) (ImageId 3)
                  pure ()
              _ <- label "click widgets, type in Name, Esc quits"
              pure ()
            pure ()
      )
