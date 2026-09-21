-- | Demo data shared by the SDL demo, profile harness, and UI tests.
--
-- Everything here is plain data: people records, the showcase tree, chart
-- datasets, and generated images.
module DemoData
  ( DemoPerson (..)
  , demoPeople
  , colPeople
  , demoTree
  , sineCosineChart
  , weeklyBars
  , demoSwatches
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

-- | Table columns: one headed cell per field.
colPeople :: Colonnade Headed DemoPerson T.Text
colPeople =
  mconcat
    [ headed "Name" demoPersonName
    , headed "Dept" demoPersonDept
    , headed "Age" (T.pack . show . demoPersonAge)
    , headed "City" demoPersonCity
    , headed "Role" demoPersonRole
    ]

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

-- | Captioned 32x32 RGBA images, rows top to bottom.
demoSwatches :: [(T.Text, BS.ByteString)]
demoSwatches =
  [ ( "Swatch"
    , BS.pack
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
    )
  , ( "Checker"
    , BS.pack
        [ chan
        | y <- [0 .. 31] :: [Int]
        , x <- [0 .. 31] :: [Int]
        , chan <-
            if (x `div` 8 + y `div` 8) `mod` 2 == 0
              then [240, 200, 80, 255]
              else [40, 50, 70, 255]
        ]
    )
  , ( "Stripe"
    , BS.pack
        [ chan
        | _y <- [0 .. 31] :: [Int]
        , x <- [0 .. 31] :: [Int]
        , chan <-
            if (x `div` 4) `mod` 2 == 0
              then [80, 160, 220, 255]
              else [30, 40, 60, 255]
        ]
    )
  ]
