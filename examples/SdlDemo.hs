{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import NanoUI
import qualified Data.ByteString as BS
import NanoUI.Backend.Sdl
  ( SdlDebugSnapshot (..)
  , SdlEnv
  , emptySdlDebug
  , readSdlDebugEnv
  , runSdlAppWith
  )
import qualified Data.Text as T
import Text.Printf (printf)

card :: Layout
card = minW 280 . padXY 16 14 . gap 8 . fillW $ defaultLayout

main :: IO ()
main = do
  ctx <- newSdlContext
  ok <-
    registerImages
      ctx
      [ (ImageId 1, 32, 32, swatchPixels)
      , (ImageId 2, 32, 32, checkerPixels)
      , (ImageId 3, 32, 32, stripePixels)
      ]
  unless ok $ fail "registerImage failed"
  envRef <- newIORef (Nothing :: Maybe SdlEnv)
  runSdlAppWith
    ctx
    (\env -> writeIORef envRef (Just env))
    (\inp -> KeyEscape `elem` inputKeys inp)
    $ do
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

debugBody :: SdlDebugSnapshot -> UI ()
debugBody s =
  mapM_ (\(i, line) -> withKey i (label_ (T.pack line))) (zip [0 :: Int ..] (debugLines s))

debugLines :: SdlDebugSnapshot -> [String]
debugLines s =
  [ printf "present  %.1f fps   loop %.1f fps" (dbgPresentFps s) (dbgLoopFps s)
  , printf "frame    %.2f ms    ui %.2f ms" (dbgFrameMs s) (dbgUiMs s)
  , printf "draws    %d   skips %d" (dbgPresents s) (dbgSkips s)
  , printf "mesh     %d verts  %d idx  %d cmds" (dbgVerts s) (dbgIndices s) (dbgCmds s)
  , printf
      "window   %.0fx%.0f  scale %.2f"
      (dbgWinW s)
      (dbgWinH s)
      (dbgScale s)
  , printf "mouse    %.0f, %.0f" (dbgMouseX s) (dbgMouseY s)
  , "renderer " <> dbgRenderer s <> "  vsync on"
  , "font     " <> dbgFontPath s
  , printf "haskell  %d cap / %d cpu" (dbgCaps s) (dbgCpus s)
  ]
    ++ rtsLines s

rtsLines :: SdlDebugSnapshot -> [String]
rtsLines s
  | not (dbgRtsOn s) = ["rts      stats off (need +RTS -T)"]
  | otherwise =
      [ printf "gc       %d total  %d major  last gen %d (%.2f ms)" (dbgGcs s) (dbgMajorGcs s) (dbgLastGcGen s) (dbgLastGcMs s)
      , printf
          "heap     live %.1f MiB  alloc %.1f MiB  copied %.1f MiB"
          (dbgLiveMb s)
          (dbgAllocMb s)
          (dbgCopiedMb s)
      , printf "rss max  %.1f MiB   gc time %.1f%%" (dbgMaxMemMb s) (dbgGcPct s)
      ]

copy :: T.Text -> UI ()
copy txt = void (labelEx (fillW defaultLayout) txt)

thumb :: ImageId -> T.Text -> UI ()
thumb iid caption =
  column (tight . gap 6 $ defaultLayout) $ do
    image_ (fixedWH 80 80 defaultLayout) iid
    label_ caption

swatchPixels :: BS.ByteString
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

checkerPixels :: BS.ByteString
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

stripePixels :: BS.ByteString
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
