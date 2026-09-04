module Main (main) where

import Control.Monad (void, when)
import NanoUI
import NanoUI.Backend.Term
import NanoUI.Debug (CoreDebugSnapshot (..), formatCoreRtsRows)
import Data.Text (Text)
import Text.Printf (printf)
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

newtype ClickMsg = Clicked String
  deriving (Eq, Show)

newtype TuiClick = TuiClick {tuiClick :: String}
  deriving (Eq, Show)

updateClick :: ClickMsg -> TuiClick -> TuiClick
updateClick (Clicked s) _ = TuiClick s

main :: IO ()
main =
  runTermAppReduce
    defaultTermOptions
      { termAppShouldQuit = inputKeysElem KeyEscape . inputKeys
      }
    updateClick
    (TuiClick "")
    tuiApp

tuiApp :: TuiClick -> NanoUI ()
tuiApp st = do
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
              row (stack {layoutHeight = Fit}) $ do
                ok <- button "OK"
                when (respClicked ok) $ emit (Clicked "OK")
                cancel <- button "Cancel"
                when (respClicked cancel) $ emit (Clicked "Cancel")
                (_, c) <- checkbox "Feature" False
                pure c
            row (stack {layoutHeight = Fit}) $ do
              clickButton "About" (setAbout True)
              clickButton "Debug" (setDebug True)
            sep
            (curIdx, setTab) <- useTabIdx 0
            (tabResp, nextTab) <- tabs curIdx
              [ tab (0 :: Int) "Controls" $ do
                  (_, vol) <- slider "Volume" 0 100 50
                  (_, quality) <- select "Quality" ["Low", "High"] 0
                  (_, name) <- textInput "Name" ""
                  _ <- labelEx (stack {layoutWidth = Grow 1}) $
                    T.intercalate " "
                      [ "vol=" <> T.pack (show (round vol :: Int))
                      , "quality=" <> T.pack (show quality)
                      , "name=" <> name
                      ]
                  pure ()
              , tab 1 "Logs" $ do
                  muted "Log Stream"
                  (_, _) <-
                    scrollArea (page {layoutHeight = Fixed 4}) $ column stack $ do
                      _ <- label "Log line 1: system online"
                      _ <- label "Log line 2: arena reset"
                      _ <- label "Log line 3: ready"
                      pure ()
                  pure ()
              , tab 2 "Info" $ do
                  kv "Engine" "nano-ui immediate mode"
                  kv "Tabs" "Zero-cost inactive evaluation"
              ]
            onClick tabResp (setTab nextTab)
            sep
            let click = tuiClick st
            _ <-
              labelEx (stack {layoutWidth = Grow 1}) $
                T.intercalate
                  " "
                  [ "checked=" <> T.pack (show checked)
                  , "tab=" <> T.pack (show nextTab)
                  , "click=" <> T.pack click
                  ]
            muted "About opens a dialog. Debug opens a window. Esc closes, then quits."
            pure ()
    when debugOpen $ do
      snap <- askTermDebug
      (win, _) <- window True "Debug" (debugWindowBody snap)
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

debugWindowBody :: TermDebugSnapshot -> NanoUI ()
debugWindowBody snap = kvBlock (allDebugRows snap)

allDebugRows :: TermDebugSnapshot -> [(Text, Text)]
allDebugRows s =
  frameRows s ++ drawRows s ++ terminalRows s ++ rtsRows s

frameRows :: TermDebugSnapshot -> [(Text, Text)]
frameRows s =
  let c = dbgCore s
   in [ ("present", T.pack (printf "%.1f fps" (dbgPresentFps c)))
      , ("loop", T.pack (printf "%.1f fps" (dbgLoopFps c)))
      , ("frame", T.pack (printf "%.1f ms" (dbgFrameMs c)))
      , ("ui", T.pack (printf "%.1f ms" (dbgUiMs c)))
      , ("redraws", T.pack (printf "%d" (dbgRedraws s)))
      , ("blits", T.pack (printf "%d" (dbgBlits s)))
      , ("skips", T.pack (printf "%d" (dbgSkips c)))
      ]

drawRows :: TermDebugSnapshot -> [(Text, Text)]
drawRows s =
  let c = dbgCore s
   in [ ("verts", T.pack (printf "%d" (dbgVerts c)))
      , ("indices", T.pack (printf "%d" (dbgIndices c)))
      , ("cmds", T.pack (printf "%d" (dbgCmds c)))
      , ("nodes", T.pack (printf "%d" (dbgNodes s)))
      , ("base spans", T.pack (printf "%d" (dbgBaseSpans s)))
      , ("overlay spans", T.pack (printf "%d" (dbgOverlaySpans s)))
      ]

terminalRows :: TermDebugSnapshot -> [(Text, Text)]
terminalRows s =
  let c = dbgCore s
      (fr, fg, fb) = dbgThemeFg s
      (br, bg, bb) = dbgThemeBg s
   in [ ("size", T.pack (printf "%.0fx%.0f" (dbgWinW c) (dbgWinH c)))
      , ("mouse", T.pack (printf "%.0f, %.0f" (dbgMouseX c) (dbgMouseY c)))
      , ("theme fg", T.pack (printf "%d,%d,%d" fr fg fb))
      , ("theme bg", T.pack (printf "%d,%d,%d" br bg bb))
      ]

rtsRows :: TermDebugSnapshot -> [(Text, Text)]
rtsRows s = formatCoreRtsRows (dbgCore s)
