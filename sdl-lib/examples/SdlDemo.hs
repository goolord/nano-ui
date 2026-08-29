{-# LANGUAGE OverloadedStrings #-}

module SdlDemo
    ( main
    , demoImages
    , demoUi
    ) where

import Control.Monad (when)
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList)
import Data.Text (Text)
import NanoUI
import NanoUI.Backend.Sdl (RgbaImage (..), SdlDebugSnapshot (..), askSdlDebug, SdlOptions (..), defaultSdlOptions, runSdlApp)
import Text.Printf (printf)
import qualified Data.ByteString as BS
import qualified Data.Text as T

main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = \inp -> KeyEscape `elem` inputKeys inp
      , sdlAppImages = demoImages
      }
    demoUi

------------------------------------------------------------------

demoImages :: SmallArray RgbaImage
demoImages =
  smallArrayFromList
    [ RgbaImage (ImageId 1) 32 32 swatchPixels
    , RgbaImage (ImageId 2) 32 32 checkerPixels
    , RgbaImage (ImageId 3) 32 32 stripePixels
    ]

demoUi :: NanoUI ()
demoUi = do
  (click, setClick) <- useText ""
  (aboutOpen, setAbout) <- useFlag False
  (debugOpen, setDebug) <- useFlag False
  scroll (tight (grow defaultLayout)) $
    column (padAll 8 . gap 8 . fillW $ defaultLayout) $ do
      panel (padXY 14 10 . gap 8 . fillW $ defaultLayout) $
        toolbar $ do
          column (tight . gap 4 $ defaultLayout) $ do
            heading "nano-ui"
            muted "SDL3 demo"
          flex
          clickButton "OK" (setClick "OK")
          clickButton "Cancel" (setClick "Cancel")
          clickButton "About" (setAbout True)
          clickButton "Debug" (setDebug (not debugOpen))
      row (tight . gap 8 . wrap . fillW $ defaultLayout) $ do
        card $ do
          heading "Controls"
          (_, checked) <- checkbox "Feature" False
          (_, vol) <- slider "Volume" 0 100 50
          (_, quality) <- select "Quality" ["Low", "Medium", "High"] 1
          (_, name) <- textInput "Name" ""
          sep
          heading "List"
          scroll (padAll 6 . fixedH 136 . fillW $ defaultLayout) $
            column (tight . gap 0 . fillW $ defaultLayout) $
              mapM_ (label_ . T.pack . ("Item " <>) . show) [1 .. 12 :: Int]
          sep
          heading "State"
          kv "Feature" (onOff checked)
          kv "Volume" (T.pack (show (round vol :: Int)))
          kv "Quality" (T.pack (show quality))
          kv "Name" (orDash name)
          kv "Clicked" (orDash click)
        card $ do
          heading "Gallery"
          row (tight . gap 10 . wrap $ defaultLayout) $ do
            thumb (ImageId 1) "Swatch"
            thumb (ImageId 2) "Checker"
            thumb (ImageId 3) "Stripe"
          sep
          muted "Click widgets or type in Name."
          muted "Esc closes About, then quits."
  when debugOpen $ do
    snap <- askSdlDebug
    (win, _) <- window True "Debug" (debugBody snap)
    onClick win (setDebug False)
  (aboutResp, _) <-
    modal aboutOpen "About" $ do
      heading "nano-ui"
      muted "Immediate-mode GUI for Haskell."
      muted "Esc closes this dialog, then the app."
      row (gap 6 (fillW defaultLayout)) $ do
        flex
        clickButton "Close" (setAbout False)
  onClick aboutResp (setAbout False)

onOff :: Bool -> T.Text
onOff True = "on"
onOff False = "off"

orDash :: String -> T.Text
orDash "" = "-"
orDash s = T.pack s

debugBody :: SdlDebugSnapshot -> NanoUI ()
debugBody s = do
  debugSection "Frame" (frameRows s)
  sep
  debugSection "Draw" (drawRows s)
  sep
  debugSection "Display" (displayRows s)
  sep
  debugSection "Runtime" (rtsRows s)

debugSection :: T.Text -> SmallArray (T.Text, T.Text) -> NanoUI ()
debugSection title rows = do
  heading title
  mapM_ (\(k, v) -> kv k (monoFontMarker <> v)) rows

clipField :: Int -> Text -> Text
clipField n s =
  if T.length s > n
    then T.take (max 0 (n - 3)) s <> "..."
    else s

frameRows :: SdlDebugSnapshot -> SmallArray (T.Text, T.Text)
frameRows s =
  smallArrayFromList
    [ ("present", T.pack (printf "%.1f fps" (dbgPresentFps s)))
    , ("loop", T.pack (printf "%.1f fps" (dbgLoopFps s)))
    , ("frame", T.pack (printf "%.1f ms" (dbgFrameMs s)))
    , ("ui", T.pack (printf "%.1f ms" (dbgUiMs s)))
    , ("draws", T.pack (printf "%d" (dbgPresents s)))
    , ("skips", T.pack (printf "%d" (dbgSkips s)))
    ]

drawRows :: SdlDebugSnapshot -> SmallArray (T.Text, T.Text)
drawRows s =
  smallArrayFromList
    [ ("verts", T.pack (printf "%d" (dbgVerts s)))
    , ("indices", T.pack (printf "%d" (dbgIndices s)))
    , ("cmds", T.pack (printf "%d" (dbgCmds s)))
    ]

displayRows :: SdlDebugSnapshot -> SmallArray (T.Text, T.Text)
displayRows s =
  smallArrayFromList
    [ ("window", T.pack (printf "%.0fx%.0f" (dbgWinW s) (dbgWinH s)))
    , ("scale", T.pack (printf "%.2f" (dbgScale s)))
    , ("mouse", T.pack (printf "%.0f, %.0f" (dbgMouseX s) (dbgMouseY s)))
    , ( "renderer"
      , clipField 36 (dbgRenderer s <> if dbgVsync s then "  vsync on" else "  vsync off")
      )
    , ("font", clipField 36 (T.pack (dbgFontPath s)))
    ]

rtsRows :: SdlDebugSnapshot -> SmallArray (T.Text, T.Text)
rtsRows s
  | not (dbgRtsOn s) =
      smallArrayFromList
        [ ("rts", "stats off (need +RTS -T)")
        , ("haskell", T.pack (printf "%d cap / %d cpu" (dbgCaps s) (dbgCpus s)))
        ]
  | otherwise =
      smallArrayFromList
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

thumb :: ImageId -> T.Text -> NanoUI ()
thumb iid caption =
  column (tight . gap 6 $ defaultLayout) $ do
    image_ (fixedWH 88 88 defaultLayout) iid
    muted caption

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
