{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Monad (replicateM_, void, forM_)
import Data.Foldable (foldlM)
import Data.Primitive.SmallArray (SmallArray)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats (..), getRTSStats)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering), hFlush)
import System.Mem (performGC)
import Text.Printf (printf)
import qualified Data.Text as T

import NanoUI
import NanoUI.Backend.Sdl (RgbaImage (..), newSdlContext, sdlDrawFrame, syncDisplay, withSdlBench)
import NanoUI.Diagrams
import NanoUI.Testing
  ( Context
  , collectTextSpans
  , registerImage
  , runFrame
  , drawVertexCount
  , drawIndexCount
  , drawCmdCount
  )
import NanoUI.Testing.Harness (findExact)
import SdlDemo
  ( demoImages
  , demoUi
  )

iterations :: Int
iterations = 40

profileInput :: Input
profileInput =
  emptyInput
    { inputWindowSize = Size 1280 800
    , inputMousePos = V2 640 400
    , inputMouseDown = False
    }

profileInputActive :: Input
profileInputActive =
  emptyInput
    { inputWindowSize = Size 1280 800
    , inputMousePos = V2 640 400
    , inputMouseDown = True
    }

measureBench :: String -> Int -> IO () -> IO ()
measureBench name iters action = do
  -- Warmup
  replicateM_ 5 action
  performGC
  s0 <- getRTSStats
  t0 <- getMonotonicTimeNSec
  replicateM_ iters action
  t1 <- getMonotonicTimeNSec
  performGC
  s1 <- getRTSStats
  let totalMs = (fromIntegral (t1 - t0) / 1e6) :: Double
      avgMs = totalMs / fromIntegral iters
      totalAlloc = fromIntegral (allocated_bytes s1 - allocated_bytes s0) :: Double
      avgAllocKb = (totalAlloc / fromIntegral iters) / 1024.0
  printf "%-32s : %8.3f ms/frame  |  %8.1f KB alloc/frame\n" name avgMs avgAllocKb
  hFlush stdout

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "================================================================================"
  putStrLn "              NANO-UI SDL DEMO PROFILING & PERFORMANCE AUDIT                    "
  putStrLn "================================================================================"
  putStrLn ""
  ctx0 <- newSdlContext
  ok <- registerDemoImages ctx0 demoImages
  if not ok
    then fail "registerImage failed"
    else withSdlBench ctx0 $ \ctx sdlEnv -> do
      (ctx', inp) <- syncDisplay ctx sdlEnv profileInput
      (_, inpAct) <- syncDisplay ctx sdlEnv profileInputActive

      putStrLn "--- 1. FULL DEMO UI (Controls Tab, Idle vs Active Mouse) ---"
      measureBench "Full DemoUi (Idle, SDL Present)" iterations $
        void (sdlDrawFrame ctx' demoUi sdlEnv inp False)

      measureBench "Full DemoUi (Active, SDL Present)" iterations $
        void (sdlDrawFrame ctx' demoUi sdlEnv inpAct False)

      measureBench "Full DemoUi (runFrame only, No SDL)" iterations $
        void (runFrame ctx' inp demoUi)

      -- Inspect draw data
      (_, _, dd, _) <- runFrame ctx' inp demoUi
      printf "  -> Vertices: %d, Indices: %d, DrawCmds: %d\n\n"
        (drawVertexCount dd) (drawIndexCount dd) (drawCmdCount dd)

      putStrLn "--- 1b. DEMO UI WITH DEBUG WINDOW OPEN ---"
      spansLatest <- collectTextSpans ctx'
      case findExact "Debug" spansLatest of
        Nothing -> putStrLn "  Debug button not found\n"
        Just (V2 dbgX dbgY) -> do
          let clickDbg = do
                let dInp = inp { inputMousePos = V2 dbgX dbgY, inputMouseDown = True, inputMousePressed = True }
                void (sdlDrawFrame ctx' demoUi sdlEnv dInp False)
                let uInp = inp { inputMousePos = V2 dbgX dbgY, inputMouseDown = False }
                void (sdlDrawFrame ctx' demoUi sdlEnv uInp False)
          clickDbg
          measureBench "Full DemoUi (Debug Open, SDL Present)" iterations $
            void (sdlDrawFrame ctx' demoUi sdlEnv inp False)
          measureBench "Full DemoUi (Debug Open, runFrame)" iterations $
            void (runFrame ctx' inp demoUi)
          (_, _, ddDbg, _) <- runFrame ctx' inp demoUi
          printf "  -> Debug Vertices: %d, Indices: %d, DrawCmds: %d\n\n"
            (drawVertexCount ddDbg) (drawIndexCount ddDbg) (drawCmdCount ddDbg)
          clickDbg

      putStrLn "--- 2. DEMO TABS IN ISOLATION (Full runFrame + draw) ---"
      measureBench "Tab: Controls" iterations $
        void (runFrame ctx' inp tabControlsUi)

      measureBench "Tab: List (Tree + Items)" iterations $
        void (runFrame ctx' inp tabListUi)

      measureBench "Tab: Table (14 rows x 5 cols)" iterations $
        void (runFrame ctx' inp tabTableUi)
      measureBench "Tab: Table (SDL Present)" iterations $
        void (sdlDrawFrame ctx' tabTableUi sdlEnv inp False)
      (_, _, ddTable, _) <- runFrame ctx' inp tabTableUi
      printf "  -> Table DrawCmds: %d (Vertices: %d, Indices: %d)\n"
        (drawCmdCount ddTable) (drawVertexCount ddTable) (drawIndexCount ddTable)

      measureBench "Tab: Plots (4 Charts + Diagram)" iterations $
        void (runFrame ctx' inp tabPlotsUi)

      measureBench "Tab: Diagnostics" iterations $
        void (runFrame ctx' inp tabDiagnosticsUi)
      putStrLn ""

      putStrLn "--- 3. WIDGET MICROBENCHMARKS (100 widgets in container, runFrame) ---"
      measureBench "100x Button" iterations $
        void (runFrame ctx' inp benchButtons)

      measureBench "100x Checkbox" iterations $
        void (runFrame ctx' inp benchCheckboxes)

      measureBench "100x Slider" iterations $
        void (runFrame ctx' inp benchSliders)

      measureBench "100x Radio Button" iterations $
        void (runFrame ctx' inp benchRadios)

      measureBench "100x Select (Dropdown)" iterations $
        void (runFrame ctx' inp benchSelects)

      measureBench "100x TextInput" iterations $
        void (runFrame ctx' inp benchTextInputs)

      measureBench "20x TextArea" iterations $
        void (runFrame ctx' inp benchTextAreas)

      measureBench "20x ColorPicker" iterations $
        void (runFrame ctx' inp benchColorPickers)

      measureBench "100x Label (Plain Text)" iterations $
        void (runFrame ctx' inp benchLabels)

      measureBench "100x Box (Solid Rects)" iterations $
        void (runFrame ctx' inp benchBoxes)

      measureBench "100x Images (Atlas Quads)" iterations $
        void (runFrame ctx' inp benchImages)

      measureBench "50x Nested Rows & Cols" iterations $
        void (runFrame ctx' inp benchContainers)
      putStrLn ""

      putStrLn "--- 4. SCALING BENCHMARKS ---"
      measureBench "Table: 50 rows x 5 cols" iterations $
        void (runFrame ctx' inp benchLargeTable)

      measureBench "Table: 200 rows x 5 cols" iterations $
        void (runFrame ctx' inp benchHugeTable)

      measureBench "Tree: 50 items (unfolded)" iterations $
        void (runFrame ctx' inp benchLargeTree)

      measureBench "Plot: Line chart (500 pts)" iterations $
        void (runFrame ctx' inp benchLargeChart)
      putStrLn ""
      putStrLn "================================================================================"
      putStrLn "Profiling complete."

registerDemoImages :: Context -> SmallArray RgbaImage -> IO Bool
registerDemoImages ctx images =
  foldlM
    ( \ok img ->
        if ok
          then
            registerImage
              ctx
              (rgbaImageId img)
              (rgbaImageWidth img)
              (rgbaImageHeight img)
              (rgbaImagePixels img)
          else pure False
    )
    True
    images

--------------------------------------------------------------------------------
-- Isolated Tab UIs
--------------------------------------------------------------------------------

tabControlsUi :: NanoUI ()
tabControlsUi = column (tight . gap 8 . fillW $ defaultLayout) $ do
  heading "Controls"
  void $ checkbox "Feature" False
  void $ slider "Volume" 0 100 50
  let qualities = ["Low", "Medium", "High"]
  void $ select "Quality" qualities 1
  void $ colorPicker "Accent" (colorRGBA 204 102 102 255)
  void $ radioFieldset "Theme" ["Light", "Dark", "System"] 1
  void $ textInput "Name" ""
  void $ textArea "Notes" "Edit me.\nSecond line."
  row (tight . gap 8 . fillW $ defaultLayout) $ do
    btnTip <- button "Hover for Tooltip"
    tooltip btnTip "This is a floating tooltip widget!"
    btnMenu <- button "Right-click Menu"
    void $ contextMenu btnMenu $ do
      menuHeader "Context Menu"
      void $ menuItemWithShortcut "Cut" "Ctrl+X"
      void $ menuItemWithShortcut "Copy" "Ctrl+C"
      void $ menuItemWithShortcut "Paste" "Ctrl+V"

tabListUi :: NanoUI ()
tabListUi = column (tight . gap 8 . fillW $ defaultLayout) $ do
  heading "Tree"
  let demoTree =
        [ TreeItem "src"
            [ TreeItem "Main.hs" []
            , TreeItem "NanoUI"
                [ TreeItem "Widgets.hs" []
                , TreeItem "Frame.hs" []
                ]
            ]
        , TreeItem "test" [TreeItem "Main.hs" []]
        , TreeItem "README.md" []
        ]
  scroll2D (fixedH 300 . fillW $ defaultLayout) $ do
    void $ tree "demo" demoTree 0
  heading "Items"
  scroll2D (padAll 6 . fixedH 136 . fillW $ defaultLayout) $
    column (tight . gap 0 . fillW $ defaultLayout) $
      forM_ [1 .. 12 :: Int] $ \i ->
        void $ labelEx (tight . fillW $ defaultLayout) (T.pack ("Item " <> show i))

data Person = Person !T.Text !T.Text !Int !T.Text !T.Text deriving (Eq, Show)

peopleCols :: Colonnade Headed Person T.Text
peopleCols =
  mconcat
    [ headed "Name" (\(Person n _ _ _ _) -> n)
    , headed "Dept" (\(Person _ d _ _ _) -> d)
    , headed "Age" (\(Person _ _ a _ _) -> T.pack (show a))
    , headed "City" (\(Person _ _ _ c _) -> c)
    , headed "Role" (\(Person _ _ _ _ r) -> r)
    ]

samplePeople :: [Person]
samplePeople =
  [ Person "David" "Eng" 63 "Austin" "Staff"
  , Person "Ava" "Design" 34 "Berlin" "Lead"
  , Person "Sonia" "Eng" 12 "Lisbon" "Intern"
  , Person "Maya" "Ops" 41 "Tokyo" "Manager"
  , Person "Leo" "Design" 28 "Paris" "IC"
  , Person "Noah" "Eng" 37 "Seoul" "Staff"
  , Person "Iris" "Ops" 19 "Austin" "IC"
  , Person "Jules" "Sales" 45 "London" "Manager"
  , Person "Priya" "Eng" 31 "Bengaluru" "Lead"
  , Person "Chen" "Design" 26 "Shanghai" "IC"
  , Person "Omar" "Ops" 52 "Cairo" "Lead"
  , Person "Elena" "Sales" 39 "Madrid" "Staff"
  , Person "Kai" "Eng" 23 "Oslo" "IC"
  , Person "Ruth" "Ops" 47 "Boston" "Staff"
  ]

tabTableUi :: NanoUI ()
tabTableUi = column (tight . gap 8 . fillW $ defaultLayout) $ do
  heading "Table"
  void $
    tableCfg
      defaultTableCfg
      (tight . fillW . fixedH 280 $ defaultLayout {layoutGap = 0})
      "people"
      peopleCols
      samplePeople
      (SortCol 0 SortAsc)

tabPlotsUi :: NanoUI ()
tabPlotsUi = column (tight . gap 8 . fillW $ defaultLayout) $ do
  heading "Plots"
  let sineCos =
        withDecimate True $
          withGrid GridBoth $
            withLegend LegendRight $
              chart
                [ line "sin(x)" [(x, sin x) | x <- [0, 0.05 .. (2 * pi)]]
                , line "cos(x)" [(x, cos x) | x <- [0, 0.05 .. (2 * pi)]]
                ]
  void $ plot (fillW . fixedH 120 $ defaultLayout) sineCos
  let bars = [("Mon", 2), ("Tue", 5), ("Wed", 4), ("Thu", 7), ("Fri", 3)]
  void $ barChart (fillW . fixedH 120 $ defaultLayout) bars

tabDiagnosticsUi :: NanoUI ()
tabDiagnosticsUi = column (tight . gap 4 . fillW $ defaultLayout) $ do
  heading "Diagnostics"
  kv "Renderer" "SDL3 Pinned Vertex Arena"
  kv "Evaluation" "Zero-Cost Inactive Tabs"
  kv "State" "SrcLoc Preserved"
  kv "Alloc" "Optimized"

--------------------------------------------------------------------------------
-- Widget Microbenchmarks
--------------------------------------------------------------------------------

benchButtons :: NanoUI ()
benchButtons = column (tight . gap 2 . fillW $ defaultLayout) $
  forM_ [1 .. 100 :: Int] $ \i ->
    void $ button (T.pack ("Button " <> show i))

benchCheckboxes :: NanoUI ()
benchCheckboxes = column (tight . gap 2 . fillW $ defaultLayout) $
  forM_ [1 .. 100 :: Int] $ \i ->
    void $ checkbox (T.pack ("Checkbox " <> show i)) (even i)

benchSliders :: NanoUI ()
benchSliders = column (tight . gap 2 . fillW $ defaultLayout) $
  forM_ [1 .. 100 :: Int] $ \i ->
    void $ slider (T.pack ("Slider " <> show i)) 0 100 (fromIntegral i)

benchRadios :: NanoUI ()
benchRadios = column (tight . gap 2 . fillW $ defaultLayout) $
  forM_ [1 .. 100 :: Int] $ \i ->
    void $ radioFieldset (T.pack ("Radio " <> show i)) ["A", "B", "C"] (i `mod` 3)

benchSelects :: NanoUI ()
benchSelects = column (tight . gap 2 . fillW $ defaultLayout) $
  forM_ [1 .. 100 :: Int] $ \i ->
    void $ select (T.pack ("Select " <> show i)) ["Option 1", "Option 2", "Option 3"] (i `mod` 3)

benchTextInputs :: NanoUI ()
benchTextInputs = column (tight . gap 2 . fillW $ defaultLayout) $
  forM_ [1 .. 100 :: Int] $ \i ->
    void $ textInput (T.pack ("Input " <> show i)) "Hello World"

benchTextAreas :: NanoUI ()
benchTextAreas = column (tight . gap 4 . fillW $ defaultLayout) $
  forM_ [1 .. 20 :: Int] $ \i ->
    void $ textArea (T.pack ("Area " <> show i)) "Line 1\nLine 2\nLine 3"

benchColorPickers :: NanoUI ()
benchColorPickers = column (tight . gap 4 . fillW $ defaultLayout) $
  forM_ [1 .. 20 :: Int] $ \_ ->
    void $ colorPicker "Pick" (colorRGBA 100 150 200 255)

benchLabels :: NanoUI ()
benchLabels = column (tight . gap 2 . fillW $ defaultLayout) $
  forM_ [1 .. 100 :: Int] $ \i ->
    label (T.pack ("This is label text number " <> show i))

benchBoxes :: NanoUI ()
benchBoxes = row (tight . gap 2 . wrap . fillW $ defaultLayout) $
  forM_ [1 .. 100 :: Int] $ \i ->
    box (fixedWH 20 20 defaultLayout) (colorRGBA (fromIntegral (i * 2)) 120 200 255)

benchImages :: NanoUI ()
benchImages = row (tight . gap 2 . wrap . fillW $ defaultLayout) $
  forM_ [1 .. 100 :: Int] $ \i ->
    image_ (fixedWH 24 24 defaultLayout) (ImageId (1 + i `mod` 3))

benchContainers :: NanoUI ()
benchContainers = column (tight . gap 2 . fillW $ defaultLayout) $
  forM_ [1 .. 50 :: Int] $ \_ ->
    row (tight . gap 2 . fillW $ defaultLayout) $ do
      box (fixedWH 10 10 defaultLayout) (colorRGBA 255 0 0 255)
      box (fixedWH 10 10 defaultLayout) (colorRGBA 0 255 0 255)
      box (fixedWH 10 10 defaultLayout) (colorRGBA 0 0 255 255)

--------------------------------------------------------------------------------
-- Scaling Benchmarks
--------------------------------------------------------------------------------

benchLargeTable :: NanoUI ()
benchLargeTable =
  let rows = [Person (T.pack ("Name " <> show i)) (T.pack ("Dept " <> show (i `mod` 5))) (20 + i) "City" "Role" | i <- [1 .. 50 :: Int]]
   in void $ tableCfg defaultTableCfg (tight . fillW . fixedH 400 $ defaultLayout) "bigTable" peopleCols rows (SortCol 0 SortAsc)

benchHugeTable :: NanoUI ()
benchHugeTable =
  let rows = [Person (T.pack ("Name " <> show i)) (T.pack ("Dept " <> show (i `mod` 5))) (20 + i) "City" "Role" | i <- [1 .. 200 :: Int]]
   in void $ tableCfg defaultTableCfg (tight . fillW . fixedH 400 $ defaultLayout) "hugeTable" peopleCols rows (SortCol 0 SortAsc)

benchLargeTree :: NanoUI ()
benchLargeTree =
  let treeNodes = [TreeItem (T.pack ("Branch " <> show i)) [TreeItem (T.pack ("Leaf " <> show i <> "." <> show j)) [] | j <- [1 .. 5 :: Int]] | i <- [1 .. 10 :: Int]]
   in void $ tree "bigTree" treeNodes 0

benchLargeChart :: NanoUI ()
benchLargeChart =
  let pts = [(x, sin x * cos (x * 0.5)) | x <- [0.0, 0.02 .. 10.0 :: Double]]
      c = withGrid GridBoth $ chart [line "f(x)" pts]
   in void $ plot (fillW . fixedH 200 $ defaultLayout) c
