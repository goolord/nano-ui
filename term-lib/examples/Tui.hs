module Main (main) where

import Control.Monad (void, when)
import NanoUI
import NanoUI.Backend.Term (TermDebugSnapshot (..), TermOptions (..), askTermDebug, defaultTermOptions, runTermAppReduce)
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

data ClickMsg = Clicked String
  deriving (Eq, Show)

data TuiClick = TuiClick {tuiClick :: String}
  deriving (Eq, Show)

updateClick :: ClickMsg -> TuiClick -> TuiClick
updateClick (Clicked s) _ = TuiClick s

main :: IO ()
main =
  runTermAppReduce
    defaultTermOptions
      { termAppShouldQuit = \inp -> KeyEscape `elem` inputKeys inp
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
              row (stack {layoutWrap = True, layoutHeight = Fit}) $ do
                ok <- button "OK"
                when (respClicked ok) $ emit (Clicked "OK")
                cancel <- button "Cancel"
                when (respClicked cancel) $ emit (Clicked "Cancel")
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
            let click = tuiClick st
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

allDebugRows :: TermDebugSnapshot -> [(String, String)]
allDebugRows s =
  frameRows s ++ drawRows s ++ terminalRows s ++ rtsRows s

frameRows :: TermDebugSnapshot -> [(String, String)]
frameRows s =
  [ ("present", printf "%.1f fps" (dbgPresentFps s))
  , ("loop", printf "%.1f fps" (dbgLoopFps s))
  , ("frame", printf "%.1f ms" (dbgFrameMs s))
  , ("ui", printf "%.1f ms" (dbgUiMs s))
  , ("redraws", printf "%d" (dbgRedraws s))
  , ("blits", printf "%d" (dbgBlits s))
  , ("skips", printf "%d" (dbgSkips s))
  ]

drawRows :: TermDebugSnapshot -> [(String, String)]
drawRows s =
  [ ("verts", printf "%d" (dbgVerts s))
  , ("indices", printf "%d" (dbgIndices s))
  , ("cmds", printf "%d" (dbgCmds s))
  , ("nodes", printf "%d" (dbgNodes s))
  , ("base spans", printf "%d" (dbgBaseSpans s))
  , ("overlay spans", printf "%d" (dbgOverlaySpans s))
  ]

terminalRows :: TermDebugSnapshot -> [(String, String)]
terminalRows s =
  let (fr, fg, fb) = dbgThemeFg s
      (br, bg, bb) = dbgThemeBg s
   in
    [ ("size", printf "%.0fx%.0f" (dbgWinW s) (dbgWinH s))
    , ("mouse", printf "%.0f, %.0f" (dbgMouseX s) (dbgMouseY s))
    , ("theme fg", printf "%d,%d,%d" fr fg fb)
    , ("theme bg", printf "%d,%d,%d" br bg bb)
    ]

rtsRows :: TermDebugSnapshot -> [(String, String)]
rtsRows s
  | not (dbgRtsOn s) =
      [ ("rts", "stats off (need +RTS -T)")
      , ("haskell", printf "%d cap / %d cpu" (dbgCaps s) (dbgCpus s))
      ]
  | otherwise =
      [ ("haskell", printf "%d cap / %d cpu" (dbgCaps s) (dbgCpus s))
      , ("gc total", printf "%d" (dbgGcs s))
      , ("gc major", printf "%d" (dbgMajorGcs s))
      , ("last gen", printf "%d" (dbgLastGcGen s))
      , ("last gc", printf "%.2f ms" (dbgLastGcMs s))
      , ("heap live", printf "%.1f MiB" (dbgLiveMb s))
      , ("heap alloc", printf "%.1f MiB" (dbgAllocMb s))
      , ("copied", printf "%.1f MiB" (dbgCopiedMb s))
      , ("rss max", printf "%.1f MiB" (dbgMaxMemMb s))
      , ("gc time", printf "%.1f%%" (dbgGcPct s))
      ]
