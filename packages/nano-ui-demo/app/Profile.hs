module Main (main) where

import Control.Monad (forM, forM_, replicateM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray, smallArrayFromList)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats (..), getRTSStats)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import System.Mem (performGC)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Text.Printf (printf)
import qualified Data.Text as T

import NanoUI
import NanoUI.Backend (Damage (..), emptyInput)
import NanoUI.Backend.Sdl
  ( SdlEnv (..)
  , sdlDrawFrame
  , syncDisplay
  , withSdlBench
  )
import NanoUI.Internal.Debug (debugRefreshDue, emptyCoreDebugSnapshot, isDebugActive, newDebugSampler, refreshDebugSnapshot)
import NanoUI.Diagrams
import NanoUI.Internal.Context (ctxNodeArena)
import NanoUI.Internal.Layout.Arena (NodeType (NodeButton), findNodeRevM, getNodeType, getRect, getText)
import NanoUI.Testing
  ( Context
  , newPixelContext
  , collectTextSpans
  , debugPanelOpen
  , runFrame
  , takeDamage
  , drawVertexCount
  , drawIndexCount
  , drawCmdCount
  )
import NanoUI.Testing.Harness (findExact)
import NanoUI.Testing.Harness qualified as Harness
import DemoData
  ( DemoPerson (..)
  , colPeople
  , demoPeople
  , demoTree
  , demoSwatches
  , sineCosineChart
  , weeklyBars
  )
import SdlDemo (demoUi)

iterations :: Int
iterations = 40

profileInput :: Input
profileInput =
  emptyInput
    { inputWindowSize = Size 1280 800
    , inputMousePos = V2 640 400
    , inputMouseDown = False
    }

-- | Mean wall time and allocation per run after a short warmup. The clock
-- stops before the second GC, which only brings the allocation counter current.
measureBench :: String -> IO () -> IO ()
measureBench name action = do
  requested <- lookupEnv "NANO_PROFILE_ITERATIONS"
  let runs = max 1 (maybe iterations id (requested >>= readMaybe))
  replicateM_ 5 action
  performGC
  s0 <- getRTSStats
  t0 <- getMonotonicTimeNSec
  replicateM_ runs action
  t1 <- getMonotonicTimeNSec
  performGC
  s1 <- getRTSStats
  let perIter :: Double -> Double
      perIter total = total / fromIntegral runs
  printf "%-32s : %11.6f ms/frame  |  %11.3f KB alloc/frame\n" name
    (perIter (fromIntegral (t1 - t0) / 1e6))
    (perIter (fromIntegral (allocated_bytes s1 - allocated_bytes s0) / 1024))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "================================================================================"
  putStrLn "              NANO-UI SDL DEMO PROFILING & PERFORMANCE AUDIT                    "
  putStrLn "================================================================================"
  putStrLn ""
  ctx0 <- newPixelContext
  withSdlBench ctx0 $ \ctx sdlEnv -> do
    (ctx', inp) <- syncDisplay ctx sdlEnv profileInput
    (_, inpAct) <- syncDisplay ctx sdlEnv profileInput {inputMouseDown = True}
    let drawDemo frameInp = void (sdlDrawFrame ctx' demoUi sdlEnv frameInp False)
        runFrames = mapM_ (\(name, ui) -> measureBench name (void (runFrame ctx' inp ui)))

    putStrLn "--- 1. FULL DEMO UI (Controls Tab, Idle vs Active Mouse) ---"
    measureBench "Full DemoUi (Idle, SDL Present)" (drawDemo inp)
    measureBench "Full DemoUi (Active, SDL Present)" (drawDemo inpAct)
    measureBench "Full DemoUi (runFrame only, No SDL)" $
      void (runFrame ctx' inp demoUi)

    (_, _, dd, _) <- runFrame ctx' inp demoUi
    printf "  -> Vertices: %d, Indices: %d, DrawCmds: %d\n\n"
      (drawVertexCount dd) (drawIndexCount dd) (drawCmdCount dd)

    putStrLn "--- 1b. DEMO UI WITH DEBUG WINDOW OPEN ---"
    spansLatest <- collectTextSpans ctx'
    case findExact "Debug" spansLatest of
      Nothing -> putStrLn "  Debug button not found\n"
      Just _ -> do
        alreadyOpen <- debugPanelOpen ctx'
        -- Live stats can move the toolbar between workloads, so find the
        -- button again before clicking it.
        unless alreadyOpen $ do
          currentSpans <- collectTextSpans ctx'
          case findExact "Debug" currentSpans of
            Nothing -> fail "Debug button missing during profiling"
            Just pos -> Harness.clickPos drawDemo inp pos
        opened <- debugPanelOpen ctx'
        unless opened (fail "Debug Open workload did not open the debug window")
        measureBench "Full DemoUi (Debug Open, SDL Present)" (drawDemo inp)
        measureBench "Full DemoUi (Debug Open, runFrame)" $
          void (runFrame ctx' inp demoUi)
        (_, _, ddDbg, _) <- runFrame ctx' inp demoUi
        printf "  -> Debug Vertices: %d, Indices: %d, DrawCmds: %d\n\n"
          (drawVertexCount ddDbg) (drawIndexCount ddDbg) (drawCmdCount ddDbg)

        putStrLn "--- 5. FLOATING WINDOW STEADY-STATE ---"
        measureBench "Debug Open, ForceFull replay" $
          void (sdlDrawFrame ctx' demoUi sdlEnv inp True)
        churnCtx <- newPixelContext
        churnCounter <- newIORef (0 :: Int)
        void (runFrame churnCtx inp (churnWindowUi 0))
        (f0, c0, e0) <- countDamageKinds churnCtx iterations (churnFrame churnCtx inp churnCounter)
        printf "  -> Churn damage over %d frames: Full=%d Clip=%d Empty=%d\n"
          iterations f0 c0 e0
        measureBench "Win content churn (text 1..9ch)" $
          churnFrame churnCtx inp churnCounter
        sweepCounter <- newIORef (0 :: Int)
        measureBench "Debug Open, hover sweep" $
          hoverSweepFrame ctx' demoUi sdlEnv inp sweepCounter
        putStrLn "--- 5b. IDLE CADENCE (debug gating) ---"
        cadRef <- newDebugSampler
        let cadence = do
              active <- isDebugActive cadRef
              due <- debugRefreshDue cadRef
              pure (show active, if active && due then 0 :: Int else if active then 250 else -1)
        (active0, wait0) <- cadence
        printf "  plain window, no stats query : active=%-5s waitTimeout=%-3d (blocks until the next event)\n" active0 wait0
        snapRef <- newIORef emptyCoreDebugSnapshot
        _ <- refreshDebugSnapshot cadRef snapRef pure
        (active1, wait1) <- cadence
        printf "  stats window queried         : active=%-5s waitTimeout=%-3d (4 Hz HUD refresh sustained)\n" active1 wait1
        putStrLn ""
        -- The floating debug window can occlude the toolbar after it grows.
        -- Its title-bar close button is the unlabelled NodeButton in this UI.
        let arena = ctxNodeArena ctx'
        closeNode <- findNodeRevM arena $ \i -> do
          nt <- getNodeType arena i
          if nt == NodeButton then T.null <$> getText arena i else pure False
        case closeNode of
          Nothing -> fail "Debug close button missing during profiling"
          Just i -> do
            (x, y, w, h) <- getRect arena i
            Harness.clickPos drawDemo inp (Harness.spanCenter (Rect x y w h))
        stillOpen <- debugPanelOpen ctx'
        when stillOpen (fail "Debug window did not close after profiling")

    putStrLn "--- 2. DEMO TABS IN ISOLATION (Full runFrame + draw) ---"
    runFrames
      [ ("Tab: Controls", tabControlsUi)
      , ("Tab: List (Tree + Items)", tabListUi)
      , ("Tab: Table (14 rows x 5 cols)", tabTableUi)
      ]
    measureBench "Tab: Table (SDL Present)" $
      void (sdlDrawFrame ctx' tabTableUi sdlEnv inp False)
    (_, _, ddTable, _) <- runFrame ctx' inp tabTableUi
    printf "  -> Table DrawCmds: %d (Vertices: %d, Indices: %d)\n"
      (drawCmdCount ddTable) (drawVertexCount ddTable) (drawIndexCount ddTable)
    runFrames
      [ ("Tab: Plots (4 Charts + Diagram)", tabPlotsUi)
      , ("Tab: Diagnostics", tabDiagnosticsUi)
      ]
    putStrLn ""

    (images, _, _, _) <- runFrame ctx' inp $
      fmap smallArrayFromList $ forM demoSwatches $ \(_, pixels) -> do
        iid <- freshImageId
        ok <- registerImageRgba iid 32 32 pixels
        unless ok (liftIO (fail "registerImageRgba failed"))
        pure iid

    putStrLn "--- 3. WIDGET MICROBENCHMARKS (100 widgets in container, runFrame) ---"
    runFrames
      [ ("100x Button", benchButtons)
      , ("100x Checkbox", benchCheckboxes)
      , ("100x Slider", benchSliders)
      , ("100x Radio Button", benchRadios)
      , ("100x Select (Dropdown)", benchSelects)
      , ("100x TextInput", benchTextInputs)
      , ("20x TextArea", benchTextAreas)
      , ("20x ColorPicker", benchColorPickers)
      , ("100x Label (Plain Text)", benchLabels)
      , ("100x Box (Solid Rects)", benchBoxes)
      , ("100x Images (Atlas Quads)", benchImages images)
      , ("50x Nested Rows & Cols", benchContainers)
      ]
    putStrLn ""

    putStrLn "--- 4. SCALING BENCHMARKS ---"
    runFrames
      [ ("Table: 50 rows x 5 cols", benchTable "bigTable" (tablePeople 50))
      , ("Table: 200 rows x 5 cols", benchTable "hugeTable" (tablePeople 200))
      , ("Tree: 50 items (unfolded)", benchLargeTree)
      , ("Plot: Line chart (500 pts)", benchLargeChart)
      , ("Table: 2000 rows x 5 cols", benchTable "giantTable" (tablePeople 2000))
      ]
    -- Rows built inside the frame, as a view that filters or maps them would.
    rowCount <- newIORef (2000 :: Int)
    measureBench "Table: 2000 rows, rebuilt rows" $ do
      n <- readIORef rowCount
      void (runFrame ctx' inp (benchTable "freshTable" (tablePeople n)))
    -- A label that changes every frame misses the whole-layout cache, so the
    -- paragraphs beside it are wrapped again each frame.
    wrapCounter <- newIORef (0 :: Int)
    measureBench "Wrap: 20 paragraphs, live label" $ do
      k <- readIORef wrapCounter
      modifyIORef' wrapCounter (+ 1)
      void (runFrame ctx' inp (benchWrap 480 k))
    -- A width that changes every frame, as when dragging a window's edge:
    -- every candidate line is new text to measure.
    resizeCounter <- newIORef (0 :: Int)
    measureBench "Wrap: 20 paragraphs, resizing" $ do
      k <- readIORef resizeCounter
      modifyIORef' resizeCounter (+ 1)
      void (runFrame ctx' inp (benchWrap (400 + fromIntegral (k `mod` 200)) 0))
    -- Two small changes in opposite corners: their bounding box is most of
    -- the window, but they repaint little.
    cornerCounter <- newIORef (0 :: Int)
    measureBench "Wrap: 20 paragraphs, corner labels" $ do
      k <- readIORef cornerCounter
      modifyIORef' cornerCounter (+ 1)
      void (sdlDrawFrame ctx' (benchCorners k) sdlEnv inp False)
    -- The bound for one line: a text area holding a single long line, painted
    -- in full each frame as a horizontal scroll or a resize would.
    forM_ [4000, 20000 :: Int] $ \n ->
      measureBench ("Long line: " <> show n <> " chars, full") $
        void (sdlDrawFrame ctx' (benchLongLine n) sdlEnv inp True)
    putStrLn ""
    putStrLn "================================================================================"
    putStrLn "Profiling complete."

churnWindowUi :: Int -> NanoUI ()
churnWindowUi k = do
  _ <- button "Outside"
  void $ fst <$> window True "Churn" (columnWith (tight . gap 4 . minW 300 . fillW) $ do
    void $ kvMono "value" (T.pack (replicate (1 + (k `mod` 9)) 'M'))
    label "static row"
    )

churnFrame :: Context -> Input -> IORef Int -> IO ()
churnFrame ctx ninp counter = do
  k <- readIORef counter
  modifyIORef' counter (+1)
  void (runFrame ctx ninp (churnWindowUi k))

hoverSweepFrame :: Context -> NanoUI () -> SdlEnv -> Input -> IORef Int -> IO ()
hoverSweepFrame ctx ui env ninp counter = do
  i <- readIORef counter
  modifyIORef' counter (+1)
  let m = V2 1130 (110 + fromIntegral (i `mod` 300))
  _ <- sdlDrawFrame ctx ui env ninp { inputMousePos = m } False
  pure ()

countDamageKinds :: Context -> Int -> IO () -> IO (Int, Int, Int)
countDamageKinds ctx n act = go n (0, 0, 0)
  where
    go k (f, c, empty)
      | k <= 0 = pure (f, c, empty)
      | otherwise = do
          act
          dmg <- takeDamage ctx
          case dmg of
            DamageFull -> go (k - 1) (f + 1, c, empty)
            DamageClip (Rect _ _ w h)
              | w <= 0 || h <= 0 -> go (k - 1) (f, c, empty + 1)
              | otherwise -> go (k - 1) (f, c + 1, empty)

--------------------------------------------------------------------------------
-- Isolated Tab UIs
--------------------------------------------------------------------------------

tabControlsUi :: NanoUI ()
tabControlsUi = columnWith (tight . gap 8 . fillW) $ do
  heading "Controls"
  void $ checkbox "Feature" False
  label "Volume"
  void $ slider 0 100 50
  let qualities = ["Low", "Medium", "High"]
  label "Quality"
  void $ select qualities 1
  label "Accent"
  void $ colorPicker (colorRGBA 204 102 102 255)
  muted "Theme"
  void $ radio ["Light", "Dark", "System"] 1
  muted "Name"
  void $ textInputConfigured defaultTextInputConfig {ticPlaceholder = "Enter name"} ""
  muted "Notes"
  void $ textArea "Edit me.\nSecond line."
  rowWith (tight . gap 8 . fillW) $ do
    btnTip <- button' "Hover for Tooltip"
    tooltip btnTip "This is a floating tooltip widget!"
    btnMenu <- button' "Right-click Menu"
    void $ contextMenu btnMenu $ do
      menuHeader "Context Menu"
      void $ menuItemShortcut "Cut" "Ctrl+X"
      void $ menuItemShortcut "Copy" "Ctrl+C"
      void $ menuItemShortcut "Paste" "Ctrl+V"

tabListUi :: NanoUI ()
tabListUi = columnWith (tight . gap 8 . fillW) $ do
  heading "Tree"
  scroll2DWith (fixedH 300 . fillW) $ do
    void $ tree "demo" demoTree 0
  heading "Items"
  scroll2DWith (padAll 6 . fixedH 136 . fillW) $
    columnWith (tight . gap 0 . fillW) $
      forM_ [1 .. 12 :: Int] $ \i ->
        labelWith (tight . fillW) (T.pack ("Item " <> show i))

tabTableUi :: NanoUI ()
tabTableUi = columnWith (tight . gap 8 . fillW) $ do
  heading "Table"
  void $
    tableWith
      (fixedH 280)
      "people"
      colPeople
      demoPeople
      (SortCol 0 SortAsc)

tabPlotsUi :: NanoUI ()
tabPlotsUi = columnWith (tight . gap 8 . fillW) $ do
  heading "Plots"
  void $ plot (fillW . fixedH 120) sineCosineChart
  void $ barChart (fillW . fixedH 120) weeklyBars

tabDiagnosticsUi :: NanoUI ()
tabDiagnosticsUi = columnWith (tight . gap 4 . fillW) $ do
  heading "Diagnostics"
  kv "Renderer" "SDL3 Pinned Vertex Arena"
  kv "Evaluation" "Zero-Cost Inactive Tabs"
  kv "State" "SrcLoc Preserved"
  kv "Alloc" "Optimized"

--------------------------------------------------------------------------------
-- Widget Microbenchmarks
--------------------------------------------------------------------------------

benchButtons :: NanoUI ()
benchButtons = columnWith (tight . gap 2 . fillW) $
  forM_ [1 .. 100 :: Int] $ \i ->
    void $ button (T.pack ("Button " <> show i))

benchCheckboxes :: NanoUI ()
benchCheckboxes = columnWith (tight . gap 2 . fillW) $
  forM_ [1 .. 100 :: Int] $ \i ->
    void $ checkbox (T.pack ("Checkbox " <> show i)) (even i)

benchSliders :: NanoUI ()
benchSliders = columnWith (tight . gap 2 . fillW) $
  forM_ [1 .. 100 :: Int] $ \i -> do
    label (T.pack ("Slider " <> show i))
    void $ slider 0 100 (fromIntegral i)

benchRadios :: NanoUI ()
benchRadios = columnWith (tight . gap 2 . fillW) $
  forM_ [1 .. 100 :: Int] $ \i -> do
    muted (T.pack ("Radio " <> show i))
    void $ radio ["A", "B", "C"] (i `mod` 3)

benchSelects :: NanoUI ()
benchSelects = columnWith (tight . gap 2 . fillW) $
  forM_ [1 .. 100 :: Int] $ \i -> do
    label (T.pack ("Select " <> show i))
    void $ select ["Option 1", "Option 2", "Option 3"] (i `mod` 3)

benchTextInputs :: NanoUI ()
benchTextInputs = columnWith (tight . gap 2 . fillW) $
  forM_ [1 .. 100 :: Int] $ \i -> do
    muted (T.pack ("Input " <> show i))
    void $ textInput "Hello World"

benchTextAreas :: NanoUI ()
benchTextAreas = columnWith (tight . gap 4 . fillW) $
  forM_ [1 .. 20 :: Int] $ \_ ->
    void $ textArea "Line 1\nLine 2\nLine 3"

benchColorPickers :: NanoUI ()
benchColorPickers = columnWith (tight . gap 4 . fillW) $
  forM_ [1 .. 20 :: Int] $ \_ -> do
    label "Pick"
    void $ colorPicker (colorRGBA 100 150 200 255)

benchLabels :: NanoUI ()
benchLabels = columnWith (tight . gap 2 . fillW) $
  forM_ [1 .. 100 :: Int] $ \i ->
    label (T.pack ("This is label text number " <> show i))

benchBoxes :: NanoUI ()
benchBoxes = gridWith 10 (tight . gap 2 . fillW) $
  forM_ [1 .. 100 :: Int] $ \i ->
    box (fixedWH 20 20) (colorRGBA (fromIntegral (i * 2)) 120 200 255)

benchImages :: SmallArray ImageId -> NanoUI ()
benchImages images = gridWith 10 (tight . gap 2 . fillW) $
  forM_ [1 .. 100 :: Int] $ \i ->
    image (fixedWH 24 24) (indexSmallArray images (i `mod` sizeofSmallArray images))

benchContainers :: NanoUI ()
benchContainers = columnWith (tight . gap 2 . fillW) $
  forM_ [1 .. 50 :: Int] $ \_ ->
    rowWith (tight . gap 2 . fillW) $ do
      box (fixedWH 10 10) (colorRGBA 255 0 0 255)
      box (fixedWH 10 10) (colorRGBA 0 255 0 255)
      box (fixedWH 10 10) (colorRGBA 0 0 255 255)

--------------------------------------------------------------------------------
-- Scaling Benchmarks
--------------------------------------------------------------------------------

-- | A sortable table. Build the rows outside the frame.
benchTable :: T.Text -> [DemoPerson] -> NanoUI ()
benchTable key rows = void $ tableWith (fixedH 400 . gap 8) key colPeople rows (SortCol 0 SortAsc)

tablePeople :: Int -> [DemoPerson]
tablePeople n = [DemoPerson (T.pack ("Name " <> show i)) (T.pack ("Dept " <> show (i `mod` 5))) (20 + i) "City" "Role" | i <- [1 .. n]]

benchWrap :: Float -> Int -> NanoUI ()
benchWrap width k = columnWith (tight . gap 4 . fixedW width) $ do
  label (T.pack ("frame " <> show k))
  forM_ wrapParagraphs label

-- | A text area whose one line is @n@ characters long.
benchLongLine :: Int -> NanoUI ()
benchLongLine n = void $ textAreaWith (fixedWH 600 200) (longLines !! (if n > 4000 then 1 else 0))

longLines :: [T.Text]
longLines = [T.replicate (n `quot` 10) "abcdefghi " | n <- [4000, 20000 :: Int]]

-- | The paragraphs between a label in the top-left corner and one in the
-- bottom-right, both changing every frame.
benchCorners :: Int -> NanoUI ()
benchCorners k = columnWith (tight . gap 4 . fillW . fillH) $ do
  label (T.pack ("frame " <> show k))
  forM_ wrapParagraphs label
  flex
  rowWith (tight . fillW) $ do
    flex
    label (T.pack ("frame " <> show k))

-- | Twenty paragraphs of 120 words, each several lines at 400 to 600 pixels.
wrapParagraphs :: [T.Text]
wrapParagraphs =
  [ T.unwords [ws !! ((i * 7 + j * 3) `mod` length ws) | j <- [0 .. 119 :: Int]]
  | i <- [1 .. 20 :: Int]
  ]
  where
    ws = T.words "the quick brown fox jumps over a lazy dog while seven wizards quietly hex bold nymphs and pack my box with five dozen liquor jugs"

benchLargeTree :: NanoUI ()
benchLargeTree =
  let treeNodes = [TreeItem (T.pack ("Branch " <> show i)) [TreeItem (T.pack ("Leaf " <> show i <> "." <> show j)) [] | j <- [1 .. 5 :: Int]] | i <- [1 .. 10 :: Int]]
   in void $ tree "bigTree" treeNodes 0

benchLargeChart :: NanoUI ()
benchLargeChart =
  let pts = [(x, sin x * cos (x * 0.5)) | x <- [0.0, 0.02 .. 10.0 :: Double]]
      c = withGrid GridBoth $ chart [line "f(x)" pts]
   in void $ plot (fillW . fixedH 200) c
