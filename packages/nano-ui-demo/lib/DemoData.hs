-- | Demo data shared by the SDL demo, profile harness, and UI tests.
--
-- Everything here is plain data: people records, the showcase tree, chart
-- datasets, and generated images.
module DemoData
  ( DemoPerson (..)
  , demoPeople
  , peopleColumns
  , colPeople
  , demoTree
  , sineCosineChart
  , weeklyBars
  , demoSwatches
  , demoLandscape
  ) where

import NanoUI
import NanoUI.Diagrams
import qualified Data.ByteString as BS
import qualified Data.Text as T

data DemoPerson = DemoPerson
  { demoPersonName :: !T.Text
  , demoPersonDept :: !T.Text
  , demoPersonAge :: !Int
  , demoPersonCity :: !T.Text
  , demoPersonRole :: !T.Text
  }
  deriving (Eq, Show)

demoPeople :: [DemoPerson]
demoPeople =
  [ DemoPerson "David" "Eng" 63 "Austin" "Staff"
  , DemoPerson "Ava" "Design" 34 "Berlin" "Lead"
  , DemoPerson "Sonia" "Eng" 12 "Lisbon" "Intern"
  , DemoPerson "Maya" "Ops" 41 "Tokyo" "Manager"
  , DemoPerson "Leo" "Design" 28 "Paris" "IC"
  , DemoPerson "Noah" "Eng" 37 "Seoul" "Staff"
  , DemoPerson "Iris" "Ops" 19 "Austin" "IC"
  , DemoPerson "Jules" "Sales" 45 "London" "Manager"
  , DemoPerson "Priya" "Eng" 31 "Bengaluru" "Lead"
  , DemoPerson "Chen" "Design" 26 "Shanghai" "IC"
  , DemoPerson "Omar" "Ops" 52 "Cairo" "Lead"
  , DemoPerson "Elena" "Sales" 39 "Madrid" "Staff"
  , DemoPerson "Kai" "Eng" 23 "Oslo" "IC"
  , DemoPerson "Ruth" "Ops" 47 "Boston" "Staff"
  ]

-- | Table columns as header and field text.
peopleColumns :: [(T.Text, DemoPerson -> T.Text)]
peopleColumns =
  [ ("Name", demoPersonName)
  , ("Dept", demoPersonDept)
  , ("Age", T.pack . show . demoPersonAge)
  , ("City", demoPersonCity)
  , ("Role", demoPersonRole)
  ]

colPeople :: Colonnade Headed DemoPerson T.Text
colPeople = foldMap (uncurry headed) peopleColumns

demoTree :: [TreeItem]
demoTree =
  [ TreeItem
      "src"
      [ TreeItem "Main.hs" []
      , TreeItem
          "NanoUI"
          [ TreeItem "Widgets.hs" []
          , TreeItem "Frame.hs" []
          ]
      ]
  , TreeItem
      "test"
      [ TreeItem "Main.hs" []
      ]
  , TreeItem "README.md" []
  ]

sineCosineChart :: Chart
sineCosineChart =
  withDecimate True $
    withGrid GridBoth $
      withLegend LegendRight $
        withYAxis "y" $
          withXAxis "x" $
            chart
              [ line "sin(x)" [(x, sin x) | x <- [0, 0.05 .. (2 * pi)]]
              , line "cos(x)" [(x, cos x) | x <- [0, 0.05 .. (2 * pi)]]
              ]

weeklyBars :: [(T.Text, Double)]
weeklyBars =
  [ ("Mon", 2)
  , ("Tue", 5)
  , ("Wed", 4)
  , ("Thu", 7)
  , ("Fri", 3)
  ]

-- | Captioned 32x32 RGBA images.
demoSwatches :: [(T.Text, BS.ByteString)]
demoSwatches =
  [ ("Swatch", square (\x y -> (x * 255 `div` 31, y * 255 `div` 31, 180)))
  , ("Checker", square (\x y -> if even (x `div` 8 + y `div` 8) then (240, 200, 80) else (40, 50, 70)))
  , ("Stripe", square (\x _ -> if even (x `div` 4) then (80, 160, 220) else (30, 40, 60)))
  ]
  where
    square = opaqueImage 32 32

-- | A 96x48 landscape (sky, hills, sun on the right). It is wide and
-- asymmetric so each content fit and rotation looks different.
demoLandscape :: BS.ByteString
demoLandscape = opaqueImage 96 48 pixel
  where
    pixel x y
      | sun = (255, 214, 92)
      | y >= hill = (60 + x, 140 - (y - hill) * 2, 70)
      | otherwise = (90 + y * 2, 150 + y, 230)
      where
        hill = 30 + round (6 * sin (fromIntegral x / 9 :: Double))
        sun = (x - 70) ^ (2 :: Int) + (y - 14) ^ (2 :: Int) < 64

-- | A @w@ x @h@ opaque RGBA image, rows top to bottom, built from a per-pixel
-- RGB function.
opaqueImage :: Int -> Int -> (Int -> Int -> (Int, Int, Int)) -> BS.ByteString
opaqueImage w h pixel =
  BS.pack
    [ chan
    | y <- [0 .. h - 1]
    , x <- [0 .. w - 1]
    , let (r, g, b) = pixel x y
    , chan <- [fromIntegral r, fromIntegral g, fromIntegral b, 255]
    ]
