{-# LANGUAGE OverloadedStrings #-}

module SdlDemoUi
  ( demoImages
  , demoUi
  ) where

import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef)
import NanoUI
import NanoUI.Backend.Sdl
  ( SdlDebugSnapshot (..)
  , SdlEnv
  , emptySdlDebug
  , readSdlDebugEnv
  )
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Text.Printf (printf)

card :: Layout
card = minW 280 . padXY 16 14 . gap 8 . fillW $ defaultLayout

demoImages :: [(ImageId, Int, Int, BS.ByteString)]
demoImages =
  [ (ImageId 1, 32, 32, swatchPixels)
  , (ImageId 2, 32, 32, checkerPixels)
  , (ImageId 3, 32, 32, stripePixels)
  ]

demoUi :: IORef (Maybe SdlEnv) -> UI ()
demoUi envRef = do
  (click, setClick) <- useText ""
  (aboutOpen, setAbout) <- useFlag False
  (debugOpen, setDebug) <- useFlag False
  column (padAll 12 . gap 8 . grow $ defaultLayout) $ do
    panel (padXY 16 12 . gap 8 . fillW $ defaultLayout) $
      row (tight . gap 16 . alignMid . fillW $ defaultLayout) $ do
        label_ "nano-ui SDL3 demo"
        flex
        row (tight . gap 8 . alignMid $ defaultLayout) $ do
          clickButton "OK" (setClick "OK")
          clickButton "Cancel" (setClick "Cancel")
          clickButton "About" (setAbout True)
          clickButton "Debug" (setDebug (not debugOpen))
    scroll (tight (grow defaultLayout)) $
      row (tight . gap 8 . wrap . fillW $ defaultLayout) $ do
        panel card $ do
          label_ "Controls"
          (_, checked) <- checkbox "Feature" False
          (_, vol) <- slider "Volume" 0 100 50
          (_, quality) <- select "Quality" ["Low", "Medium", "High"] 1
          (_, name) <- textInput "Name" ""
          sep
          label_ "List"
          scroll (padAll 8 . fixedH 136 . fillW $ defaultLayout) $
            column (tight . gap 0 . fillW $ defaultLayout) $
              mapM_ (label_ . T.pack . ("Item " <>) . show) [1 .. 12 :: Int]
          sep
          label_ (T.pack ("Feature  " <> if checked then "on" else "off"))
          label_ (T.pack ("Volume   " <> show (round vol :: Int)))
          label_ (T.pack ("Quality  " <> show quality))
          label_ (T.pack ("Name     " <> if null name then "-" else name))
          label_ (T.pack ("Clicked  " <> if null click then "-" else click))
        panel card $ do
          label_ "Gallery"
          row (tight . gap 12 . wrap $ defaultLayout) $ do
            thumb (ImageId 1) "Swatch"
            thumb (ImageId 2) "Checker"
            thumb (ImageId 3) "Stripe"
          sep
          copy "Click widgets or type in Name."
          copy "Esc closes About, then quits."
  when debugOpen $ do
    snap <-
      liftIO $
        readIORef envRef >>= \m ->
          case m of
            Nothing -> pure emptySdlDebug
            Just env -> readSdlDebugEnv env
    (win, _) <- window True "Debug" (debugBody snap)
    onClick win (setDebug False)
  (aboutResp, _) <-
    modal aboutOpen "About" $ do
      copy "Immediate-mode GUI for Haskell."
      copy "Esc closes this dialog, then the app."
      row (gap 8 (fillW defaultLayout)) $ do
        flex
        clickButton "Close" (setAbout False)
  onClick aboutResp (setAbout False)

debugLabelW, debugValueW, debugPanelW :: Float
debugLabelW = 92
debugValueW = 260
debugPanelW = debugLabelW + 12 + debugValueW

fixedW :: Float -> Layout -> Layout
fixedW w l = l {layoutWidth = Fixed w, layoutMinW = w, layoutMaxW = w}

debugPanelLayout, debugRowLayout, debugLabelLayout, debugValueLayout :: Layout
debugPanelLayout = tight . fixedW debugPanelW . gap 2 $ defaultLayout
debugRowLayout = tight . gap 12 . alignMid . fillW $ defaultLayout
debugLabelLayout = tight . fixedW debugLabelW $ defaultLayout
debugValueLayout = tight . fixedW debugValueW $ defaultLayout

debugBody :: SdlDebugSnapshot -> UI ()
debugBody s =
  column debugPanelLayout $
    mapM_ (\(i, entry) -> withKey i (debugRow entry)) (zip [0 :: Int ..] (debugRows s))

debugRow :: (String, String) -> UI ()
debugRow (lbl, val) =
  void $
    row debugRowLayout $ do
      void (labelEx debugLabelLayout (monoFontMarker <> T.pack lbl))
      void (labelEx debugValueLayout (monoFontMarker <> T.pack val))

clipField :: Int -> String -> String
clipField n s =
  if length s > n
    then take (max 0 (n - 3)) s ++ "..."
    else s

debugRows :: SdlDebugSnapshot -> [(String, String)]
debugRows s =
  [ ("present", printf "%7.1f fps" (dbgPresentFps s))
  , ("loop", printf "%7.1f fps" (dbgLoopFps s))
  , ("frame", printf "%8.1f ms" (dbgFrameMs s))
  , ("ui", printf "%8.1f ms" (dbgUiMs s))
  , ("draws", printf "%8d" (dbgPresents s))
  , ("skips", printf "%8d" (dbgSkips s))
  , ("verts", printf "%8d" (dbgVerts s))
  , ("indices", printf "%8d" (dbgIndices s))
  , ("cmds", printf "%8d" (dbgCmds s))
  , ("window", printf "%4.0fx%-4.0f" (dbgWinW s) (dbgWinH s))
  , ("scale", printf "%6.2f" (dbgScale s))
  , ("mouse", printf "%7.0f, %-7.0f" (dbgMouseX s) (dbgMouseY s))
  , ("renderer", clipField 36 (dbgRenderer s <> "  vsync on"))
  , ("font", clipField 36 (dbgFontPath s))
  , ("haskell", printf "%2d cap / %2d cpu" (dbgCaps s) (dbgCpus s))
  ]
    ++ rtsRows s

rtsRows :: SdlDebugSnapshot -> [(String, String)]
rtsRows s
  | not (dbgRtsOn s) = [("rts", "stats off (need +RTS -T)")]
  | otherwise =
      [ ("gc total", printf "%8d" (dbgGcs s))
      , ("gc major", printf "%8d" (dbgMajorGcs s))
      , ("last gen", printf "%8d" (dbgLastGcGen s))
      , ("last gc", printf "%8.2f ms" (dbgLastGcMs s))
      , ("heap live", printf "%8.1f MiB" (dbgLiveMb s))
      , ("heap alloc", printf "%8.1f MiB" (dbgAllocMb s))
      , ("copied", printf "%8.1f MiB" (dbgCopiedMb s))
      , ("rss max", printf "%8.1f MiB" (dbgMaxMemMb s))
      , ("gc time", printf "%7.1f%%" (dbgGcPct s))
      ]

copy :: T.Text -> UI ()
copy txt = void (labelEx (fillW defaultLayout) txt)

thumb :: ImageId -> T.Text -> UI ()
thumb iid caption =
  column (tight . gap 6 $ defaultLayout) $ do
    image_ (fixedWH 80 80 defaultLayout) iid
    label_ caption

swatchPixels, checkerPixels, stripePixels :: BS.ByteString
swatchPixels =
  BS.pack
    [ chan
    | y <- [0 .. 31] :: [Int]
    , x <- [0 .. 31] :: [Int]
    , chan <-
        [ fromIntegral (x * 255 `div` 31)
        , fromIntegral (y * 255 `div` 31)
        , 180
        , 255
        ]
    ]

checkerPixels =
  BS.pack
    [ chan
    | y <- [0 .. 31] :: [Int]
    , x <- [0 .. 31] :: [Int]
    , let on = (x `div` 8 + y `div` 8) `mod` 2 == 0
    , chan <-
        if on
          then [240, 200, 80, 255]
          else [40, 50, 70, 255]
    ]

stripePixels =
  BS.pack
    [ chan
    | y <- [0 .. 31] :: [Int]
    , x <- [0 .. 31] :: [Int]
    , let on = (x + y) `mod` 8 < 4
    , chan <-
        if on
          then [80, 200, 220, 255]
          else [20, 30, 90, 255]
    ]
