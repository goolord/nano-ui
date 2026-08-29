module Main (main) where

import Control.Monad (void, when)
import NanoUI
import NanoUI.Backend.Term (TermDebugSnapshot (..), TermOptions (..), askTermDebug, defaultTermOptions, runTermAppReduce)
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
      { termAppShouldQuit = \inp -> inputKeysElem KeyEscape (inputKeys inp)
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
                T.intercalate
                  " "
                  [ "checked=" <> T.pack (show checked)
                  , "vol=" <> T.pack (show (round vol :: Int))
                  , "quality=" <> T.pack (show quality)
                  , "name=" <> name
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
  [ ("present", T.pack (printf "%.1f fps" (dbgPresentFps s)))
  , ("loop", T.pack (printf "%.1f fps" (dbgLoopFps s)))
  , ("frame", T.pack (printf "%.1f ms" (dbgFrameMs s)))
  , ("ui", T.pack (printf "%.1f ms" (dbgUiMs s)))
  , ("redraws", T.pack (printf "%d" (dbgRedraws s)))
  , ("blits", T.pack (printf "%d" (dbgBlits s)))
  , ("skips", T.pack (printf "%d" (dbgSkips s)))
  ]

drawRows :: TermDebugSnapshot -> [(Text, Text)]
drawRows s =
  [ ("verts", T.pack (printf "%d" (dbgVerts s)))
  , ("indices", T.pack (printf "%d" (dbgIndices s)))
  , ("cmds", T.pack (printf "%d" (dbgCmds s)))
  , ("nodes", T.pack (printf "%d" (dbgNodes s)))
  , ("base spans", T.pack (printf "%d" (dbgBaseSpans s)))
  , ("overlay spans", T.pack (printf "%d" (dbgOverlaySpans s)))
  ]

terminalRows :: TermDebugSnapshot -> [(Text, Text)]
terminalRows s =
  let (fr, fg, fb) = dbgThemeFg s
      (br, bg, bb) = dbgThemeBg s
   in
    [ ("size", T.pack (printf "%.0fx%.0f" (dbgWinW s) (dbgWinH s)))
    , ("mouse", T.pack (printf "%.0f, %.0f" (dbgMouseX s) (dbgMouseY s)))
    , ("theme fg", T.pack (printf "%d,%d,%d" fr fg fb))
    , ("theme bg", T.pack (printf "%d,%d,%d" br bg bb))
    ]

rtsRows :: TermDebugSnapshot -> [(Text, Text)]
rtsRows s
  | not (dbgRtsOn s) =
      [ ("rts", "stats off (need +RTS -T)")
      , ("haskell", T.pack (printf "%d cap / %d cpu" (dbgCaps s) (dbgCpus s)))
      ]
  | otherwise =
      [ ("haskell", T.pack (printf "%d cap / %d cpu" (dbgCaps s) (dbgCpus s)))
      , ("gc total", T.pack (printf "%d" (dbgGcs s)))
      , ("gc major", T.pack (printf "%d" (dbgMajorGcs s)))
      , ("last gen", T.pack (printf "%d" (dbgLastGcGen s)))
      , ("last gc", T.pack (printf "%.2f ms" (dbgLastGcMs s)))
      , ("heap live", T.pack (printf "%.1f MiB" (dbgLiveMb s)))
      , ("heap alloc", T.pack (printf "%.1f MiB" (dbgAllocMb s)))
      , ("copied", T.pack (printf "%.1f MiB" (dbgCopiedMb s)))
      , ("rss max", T.pack (printf "%.1f MiB" (dbgMaxMemMb s)))
      , ("gc time", T.pack (printf "%.1f%%" (dbgGcPct s)))
      ]
