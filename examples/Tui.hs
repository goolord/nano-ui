module Main (main) where

import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import NanoUI
import NanoUI.Backend.Term (runTermAppWithQuit)
import qualified Data.Text as T

-- Terminal cells are the layout unit. defaultLayout gap/pad are scaled down
-- in solve (see resolveLayoutGap). compact still avoids extra row spacing.
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
    tuiApp lastClick

tuiApp :: IORef String -> NanoUI ()
tuiApp lastClick = do
  (aboutOpen, setAbout) <- useFlag False
  (debugOpen, setDebug) <- useFlag False
  column
    compact
      { layoutWidth = Grow 1
      , layoutHeight = Grow 1
      }
    $ do
      void $
        panel
          compact
            { layoutWidth = Grow 1
            , layoutHeight = Grow 1
            , layoutPadding = Padding 2 2 1 1
            }
          $ scrollArea
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
                  checked <-
                    row
                      (compact {layoutWrap = True, layoutWidth = Grow 1})
                      $ do
                        ok <- button "OK"
                        when (respClicked ok) $ do
                          liftIO $ writeIORef lastClick "OK"
                          emit ("button:OK" :: String)
                        cancel <- button "Cancel"
                        when (respClicked cancel) $ do
                          liftIO $ writeIORef lastClick "Cancel"
                          emit ("button:Cancel" :: String)
                        (_, c) <- checkbox "Feature" False
                        pure c
                  row
                    (compact {layoutWrap = True, layoutWidth = Grow 1})
                    $ do
                      clickButton "About" (setAbout True)
                      clickButton "Debug" (setDebug True)
                  (_, vol) <- slider "Volume" 0 100 50
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
                  _ <- label "About opens a modal. Debug opens a floating window."
                  _ <- label "Esc closes About, then quits."
                  pure ()
      when debugOpen $ do
        (win, _) <- window True "Debug" (debugWindowBody setDebug)
        onClick win (setDebug False)
      (aboutResp, _) <-
        modal aboutOpen "About" $ do
          heading "nano-ui"
          muted "Immediate-mode GUI for Haskell."
          muted "Terminal backend demo."
          row (compact {layoutWidth = Grow 1}) $ do
            flex
            clickButton "Close" (setAbout False)
      onClick aboutResp (setAbout False)

debugWindowBody :: (Bool -> NanoUI ()) -> NanoUI ()
debugWindowBody setDebug = do
  heading "Debug"
  muted "Drag the title bar to move."
  muted "Click X or Close to dismiss."
  _ <- label "Floating window overlay."
  row (compact {layoutWidth = Grow 1}) $ do
    flex
    clickButton "Close" (setDebug False)
