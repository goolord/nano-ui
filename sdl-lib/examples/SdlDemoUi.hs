{-# LANGUAGE OverloadedStrings #-}

module SdlDemoUi
  ( demoImages
  , demoUi
  ) where

import NanoUI
import NanoUI.Backend.Sdl (RgbaImage (..))
import qualified Data.ByteString as BS
import qualified Data.Text as T

demoImages :: [RgbaImage]
demoImages =
  [ RgbaImage (ImageId 1) 32 32 swatchPixels
  , RgbaImage (ImageId 2) 32 32 checkerPixels
  , RgbaImage (ImageId 3) 32 32 stripePixels
  ]

demoUi :: NanoUI ()
demoUi = do
  (click, setClick) <- useText ""
  (aboutOpen, setAbout) <- useFlag False
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
