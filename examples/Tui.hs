module Main (main) where

import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import NanoUI
import NanoUI.Backend.Term (runTermAppWithQuit)
import qualified Data.Text as T

-- Zero pad. defaultLayout gap (4) becomes 1 cell after resolveLayoutGap.
page :: Layout
page =
  defaultLayout
    { layoutPadding = Padding 0 0 0 0
    , layoutWidth = Grow 1
    , layoutHeight = Grow 1
    }

stack :: Layout
stack =
  page
    { layoutHeight = Fit
    , layoutGap = 4
    }

-- The framing panel, so it fills the terminal rather than fitting its content.
inset :: Layout
inset = stack {layoutPadding = Padding 4 4 4 4, layoutHeight = Grow 1}

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
  column page $ do
    void $
      panel inset $
        scrollArea page $
          column stack $ do
            heading "nano-ui"
            muted "Immediate-mode GUI for Haskell."
            sep
            checked <-
              row (stack {layoutWrap = True, layoutHeight = Fit}) $ do
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
            row (stack {layoutWrap = True, layoutHeight = Fit}) $ do
              clickButton "About" (setAbout True)
              clickButton "Debug" (setDebug True)
            sep
            (_, vol) <- slider "Volume" 0 100 50
            (_, quality) <- select "Quality" ["Low", "High"] 0
            (_, name) <- textInput "Name" ""
            sep
            muted "List"
            (_, _) <-
              scrollArea
                ( page
                    { layoutHeight = Fixed 5
                    }
                )
                $ column stack $ do
                    _ <- label "Scroll line 1"
                    _ <- label "Scroll line 2"
                    _ <- label "Scroll line 3"
                    _ <- label "Scroll line 4"
                    _ <- label "Scroll line 5"
                    _ <- label "Scroll line 6"
                    pure ()
            sep
            click <- liftIO $ readIORef lastClick
            _ <-
              labelEx (stack {layoutWidth = Grow 1}) $
                T.pack $
                  unwords
                    [ "checked=" <> show checked
                    , "vol=" <> show (round vol :: Int)
                    , "quality=" <> show quality
                    , "name=" <> name
                    , "click=" <> click
                    ]
            muted "About opens a dialog. Debug opens a window. Esc closes, then quits."
            pure ()
    when debugOpen $ do
      (win, _) <- window True "Debug" (debugWindowBody setDebug)
      onClick win (setDebug False)
    (aboutResp, _) <-
      modal aboutOpen "About" $ do
        heading "nano-ui"
        muted "Immediate-mode GUI for Haskell."
        muted "Terminal backend demo."
        row (stack {layoutWidth = Grow 1, layoutHeight = Fit}) $ do
          flex
          clickButton "Close" (setAbout False)
    onClick aboutResp (setAbout False)

debugWindowBody :: (Bool -> NanoUI ()) -> NanoUI ()
debugWindowBody setDebug = do
  heading "Debug"
  muted "Drag the title bar to move."
  muted "Click X or Close to dismiss."
  _ <- label "Floating window overlay."
  row (stack {layoutWidth = Grow 1, layoutHeight = Fit}) $ do
    flex
    clickButton "Close" (setDebug False)
