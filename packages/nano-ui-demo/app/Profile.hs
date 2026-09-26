module Main (main) where

import Control.Monad (forM, forM_, replicateM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Primitive.SmallArray (indexSmallArray, sizeofSmallArray, smallArrayFromList)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats (..), getRTSStats)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import System.Mem (performGC)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Text.Printf (printf)
import qualified Data.ByteString as BS
import qualified Data.Text as T

import NanoUI
import NanoUI.Backend (Damage (..), applyMouseButton, emptyInput)
import NanoUI.Backend.Sdl (sdlDrawFrame, syncDisplay, withSdlBench)
import NanoUI.Internal.Debug (debugCadence, newDebugSampler, refreshDebugSnapshot)
import NanoUI.Diagrams
import NanoUI.Internal.Context (ctxNodeArena)
import NanoUI.Internal.Layout.Arena (NodeType (NodeButton), findNodeRevM, getNodeType, getNodeRect, getText)
import NanoUI.Shortcut
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
    (_, inpAct) <- syncDisplay ctx sdlEnv profileInput {inputButtonsHeld = buttonsFromList [MouseLeft]}
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
    -- Live stats can move the toolbar between workloads, so find the button
    -- after them.
    spansLatest <- collectTextSpans ctx'
    case findExact "Debug" spansLatest of
      Nothing -> putStrLn "  Debug button not found\n"
      Just pos -> do
        alreadyOpen <- debugPanelOpen ctx'
        unless alreadyOpen (Harness.clickPos drawDemo inp pos)
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
        nextChurn <- newCounter 0
        let churnFrame = nextChurn >>= \k -> void (runFrame churnCtx inp (churnWindowUi k))
        void (runFrame churnCtx inp (churnWindowUi 0))
        (f0, c0, e0) <- countDamageKinds churnCtx iterations churnFrame
        printf "  -> Churn damage over %d frames: Full=%d Clip=%d Empty=%d\n"
          iterations f0 c0 e0
        measureBench "Win content churn (text 1..9ch)" churnFrame
        nextSweep <- newCounter 0
        measureBench "Debug Open, hover sweep" $ do
          i <- nextSweep
          void (sdlDrawFrame ctx' demoUi sdlEnv inp {inputMousePos = V2 1130 (110 + fromIntegral (i `mod` 300))} False)
        putStrLn "--- 5b. IDLE CADENCE (debug gating) ---"
        cadRef <- newDebugSampler
        let cadence = do
              (active, due) <- debugCadence cadRef
              pure (show active, if due then 0 :: Int else if active then 250 else -1)
        (active0, wait0) <- cadence
        printf "  plain window, no stats query : active=%-5s waitTimeout=%-3d (blocks until the next event)\n" active0 wait0
        _ <- refreshDebugSnapshot cadRef pure
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
            Rect x y w h <- getNodeRect arena i
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
      smallArrayFromList <$> forM demoSwatches (\(_, pixels) -> registerFresh 32 32 pixels)

    putStrLn "--- 3. WIDGET MICROBENCHMARKS (100 widgets in container, runFrame) ---"
    runFrames
      [ ("100x Button", repeated 2 100 $ \i -> void $ button (numbered "Button " i))
      , ("100x Checkbox", repeated 2 100 $ \i -> void $ checkbox (numbered "Checkbox " i) (even i))
      , ("100x Slider", repeated 2 100 $ \i -> label (numbered "Slider " i) >> void (slider 0 100 (fromIntegral i)))
      , ("100x Radio Button", repeated 2 100 $ \i -> muted (numbered "Radio " i) >> void (radio ["A", "B", "C"] (i `mod` 3)))
      , ( "100x Select (Dropdown)"
        , repeated 2 100 $ \i -> label (numbered "Select " i) >> void (select ["Option 1", "Option 2", "Option 3"] (i `mod` 3))
        )
      , ("100x TextInput", repeated 2 100 $ \i -> muted (numbered "Input " i) >> void (textInput "Hello World"))
      , ("20x TextArea", repeated 4 20 $ \_ -> void $ textArea "Line 1\nLine 2\nLine 3")
      , ("20x ColorPicker", repeated 4 20 $ \_ -> label "Pick" >> void (colorPicker (colorRGBA 100 150 200 255)))
      , ("100x Label (Plain Text)", repeated 2 100 $ \i -> label (numbered "This is label text number " i))
      , ( "100x Box (Solid Rects)"
        , gridWith 10 (tight . gap 2 . fillW) $
            forM_ [1 .. 100 :: Int] $ \i -> box (fixedWH 20 20) (colorRGBA (fromIntegral (i * 2)) 120 200 255)
        )
      , ( "100x Images (Atlas Quads)"
        , gridWith 10 (tight . gap 2 . fillW) $
            forM_ [1 .. 100 :: Int] $ \i -> image (fixedWH 24 24) (indexSmallArray images (i `mod` sizeofSmallArray images))
        )
      , ( "50x Nested Rows & Cols"
        , repeated 2 50 $ \_ -> rowWith (tight . gap 2 . fillW) $
            forM_ [colorRGBA 255 0 0 255, colorRGBA 0 255 0 255, colorRGBA 0 0 255 255] (box (fixedWH 10 10))
        )
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
    nextWrap <- newCounter 0
    measureBench "Wrap: 20 paragraphs, live label" $
      nextWrap >>= \k -> void (runFrame ctx' inp (benchWrap 480 k))
    -- A width that changes every frame, as when dragging a window's edge:
    -- every candidate line is new text to measure.
    nextResize <- newCounter 0
    measureBench "Wrap: 20 paragraphs, resizing" $
      nextResize >>= \k -> void (runFrame ctx' inp (benchWrap (400 + fromIntegral (k `mod` 200)) 0))
    -- Two small changes in opposite corners: their bounding box is most of
    -- the window, but they repaint little.
    nextCorner <- newCounter 0
    measureBench "Wrap: 20 paragraphs, corner labels" $
      nextCorner >>= \k -> void (sdlDrawFrame ctx' (benchCorners k) sdlEnv inp False)
    -- Dragging the divider of two panes that each hold the paragraphs: every
    -- frame shares the width out again and wraps both panes at new widths.
    let dividerAt k = inp {inputMousePos = V2 (400 + fromIntegral (k `mod` 40 - 20)) 300, inputButtonsHeld = buttonsFromList [MouseLeft]}
    replicateM_ 3 (void (runFrame ctx' inp benchSplit))
    void (runFrame ctx' (dividerAt 20) {inputButtonsPressed = buttonsFromList [MouseLeft]} benchSplit)
    nextSplit <- newCounter 1
    measureBench "Wrap: split drag, 2x20 paragraphs" $
      nextSplit >>= \k -> void (runFrame ctx' (dividerAt k) benchSplit)
    void (runFrame ctx' (applyMouseButton MouseLeft False inp) benchSplit)
    -- More glyphs than one atlas page holds, all painted every frame: ten
    -- sizes of 280 characters each.
    measureBench "Text: 10 sizes of 280 glyphs, > 1 atlas page" $
      void (sdlDrawFrame ctx' benchGlyphSizes sdlEnv inp True)
    -- One small image changing every frame in an atlas holding 12 large
    -- ones, as a live thumbnail does: getting it to the GPU is the cost.
    void $ runFrame ctx' inp $ forM_ [1 .. 12 :: Int] $ \i ->
      registerFresh 1024 256 (BS.replicate (1024 * 256 * 4) (fromIntegral i))
    (liveImage, _, _, _) <- runFrame ctx' inp freshImageId
    let livePixels k = BS.replicate (64 * 64 * 4) (if even k then 40 else 200)
    nextLive <- newCounter 0
    measureBench "Images: one 64x64 changing, 12 1024x256" $
      nextLive >>= \k -> void (sdlDrawFrame ctx' (benchLiveImage liveImage (livePixels k)) sdlEnv inp False)
    -- The bound for one line: a text area holding a single long line, painted
    -- in full each frame as a horizontal scroll or a resize would.
    forM_ [4000, 20000 :: Int] $ \n -> do
      let txt = T.replicate (n `quot` 10) "abcdefghi "
      measureBench ("Long line: " <> show n <> " chars, full") $
        void (sdlDrawFrame ctx' (benchLongLine txt) sdlEnv inp True)
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

-- | Each run of the action returns the next number, counting from @k0@.
newCounter :: Int -> IO (IO Int)
newCounter k0 = do
  ref <- newIORef k0
  pure (atomicModifyIORef' ref (\k -> (k + 1, k)))

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
      void $ menuItemShortcut "Cut" (ctrl <> key 'x')
      void $ menuItemShortcut "Copy" (ctrl <> key 'c')
      void $ menuItemShortcut "Paste" (ctrl <> key 'v')

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

-- | @n@ widgets, numbered from 1, in a column with the given gap.
repeated :: Float -> Int -> (Int -> NanoUI ()) -> NanoUI ()
repeated g n widget = columnWith (tight . gap g . fillW) (forM_ [1 .. n] widget)

numbered :: String -> Int -> T.Text
numbered prefix i = T.pack (prefix <> show i)

-- | Register a @w@ by @h@ RGBA image under a fresh id for the rest of the
-- run, as a benchmark's setup does.
registerFresh :: Int -> Int -> BS.ByteString -> NanoUI ImageId
registerFresh w h pixels = do
  iid <- freshImageId
  ok <- registerImageRgba iid w h pixels
  if ok then pure iid else liftIO (fail "registerImageRgba failed")

--------------------------------------------------------------------------------
-- Scaling Benchmarks
--------------------------------------------------------------------------------

-- | A sortable table. Build the rows outside the frame.
benchTable :: T.Text -> [DemoPerson] -> NanoUI ()
benchTable tableKey rows = void $ tableWith (fixedH 400 . gap 8) tableKey colPeople rows (SortCol 0 SortAsc)

tablePeople :: Int -> [DemoPerson]
tablePeople n = [DemoPerson (T.pack ("Name " <> show i)) (T.pack ("Dept " <> show (i `mod` 5))) (20 + i) "City" "Role" | i <- [1 .. n]]

benchWrap :: Float -> Int -> NanoUI ()
benchWrap width k = columnWith (tight . gap 4 . fixedW width) $ do
  label (T.pack ("frame " <> show k))
  forM_ wrapParagraphs label

-- | Printable ASCII and the Latin-1, Greek and Cyrillic letters at ten sizes
-- from 30 to 120, in two columns.
benchGlyphSizes :: NanoUI ()
benchGlyphSizes = rowWith (tight . fillW) $
  forM_ [[30, 50 .. 110], [40, 60 .. 120]] $ \sizes ->
    columnWith (tight . fillW) $
      forM_ sizes $ \s -> labelWith (fontSize s) glyphs
  where
    glyphs = T.pack (['!' .. '~'] ++ ['\192' .. '\255'] ++ ['\x391' .. '\x3C9'] ++ ['\x410' .. '\x44F'])

-- | An image given new pixels every frame.
benchLiveImage :: ImageId -> BS.ByteString -> NanoUI ()
benchLiveImage iid pixels = do
  ok <- registerImageRgba iid 64 64 pixels
  unless ok (liftIO (fail "registerImageRgba failed"))
  image (fixedWH 64 64) iid

-- | Two panes side by side, each holding the paragraphs.
benchSplit :: NanoUI ()
benchSplit =
  void $
    paneGrid
      defaultPaneGridConfig
        { pgLayout = fillW . fillH
        , pgInitial = Just (Split 3 AxisV 0.5 (Pane 1) (Pane 2))
        , pgViewPane = \_ _ -> do
            columnWith (tight . gap 4 . fillW) (forM_ wrapParagraphs label)
            pure (PaneView "P" False Nothing)
        }

-- | A text area holding one line.
benchLongLine :: T.Text -> NanoUI ()
benchLongLine txt = void $ textAreaWith (fixedWH 600 200) txt

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
