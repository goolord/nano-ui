module Cases.Table (tests) where

import Spec
import Data.Bits ((.&.))
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (nub, sort, sortOn, tails)
import Data.Ord (Down (..))
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Primitive.SmallArray qualified as SA
import Data.String (IsString)
import NanoUI.Internal.Context (ctxNodeArena)
import NanoUI.Internal.Layout.Arena
  ( NodeIdx
  , NodeArena
  , findNodeM
  , foldNodesM
  , getClipRect
  , getDirection
  , getNodeType
  , getParent
  , getNodeRect
  , getScrollContentW
  , getStyleIdx
  , getText
  , getWidgetId
  )
import NanoUI.Testing.Sim
import Text.Read (readMaybe)

tests :: [Spec]
tests =
  [ spec "table-sort" runTableSortTest
  , spec "table-widget-cells" runTableWidgetCellsTest
  , spec "table-sort-mark-damage" runTableSortMarkDamageTest
  , spec "table-live-rows" runTableLiveRowsTest
  , spec "table-reorder" runTableReorderTest
  , spec "table-hide-columns" runTableHideColumnsTest
  , spec "table-frozen" runTableFrozenTest
  , pixelSpec "table-shared-scroll-metrics" runTableSharedScrollMetricsTest
  , pixelSpec "page-wheel-above-table" runPageWheelAboveTableTest
  , pixelSpec "table-scroll-reveal" runTableScrollRevealTest
  , spec "table-wrap" runTableWrapTest
  , spec "table-column-widths" runTableColumnWidthsTest
  , spec "table-narrow-columns" runTableNarrowColumnsTest
  , spec "table-rules-tile" runTableRulesTileTest
  , pixelSpec "table-resize-overflow" runTableResizeOverflowTest
  , spec "table-col-resize" runTableColResizeTest
  , pixelSpec "table-hbar-reach" runTableHBarReachTest
  , spec "table-squeeze" runTableSqueezeTest
  ]

at :: Float -> Float -> Target
at x y = At (V2 x y)

-- | The cursor where the pointer is now.
cursorNow :: Sim a UiCursorKind
cursorNow = do
  ctx <- simContext
  liftIO . uiCursorKind ctx =<< getInput

-- | The cursor with the pointer moved onto the target.
cursorAt :: Target -> Sim a UiCursorKind
cursorAt t = moveTo t >> cursorNow

-- | A table sorted ascending on its first column, given everything but the sort.
sortedTable :: (Var SortCol -> NanoUI ()) -> NanoUI ()
sortedTable build = build =<< useVar (SortCol 0 SortAsc)

-- | Fruit names, a widget cell sorted by its count, and a button that logs
-- the fruit it eats.
mixedCols :: IORef [Text] -> Colonnade Headed (Text, Int) Cell
mixedCols eaten =
  mconcat
    [ headed "Fruit" (textCell . fst)
    , headed "Count" (\(_, n) -> cellSort (T.pack (show n)) (cellWidget (label (T.pack (show n)))))
    , headed "Button" (widgetCell (\(f, _) -> whenM (button ("Eat " <> f)) (liftIO (modifyIORef' eaten (f :)))))
    ]

fruits :: [(Text, Int)]
fruits = [("banana", 2), ("apple", 1), ("cherry", 3)]

-- | Rows sort stably by a column's text either way, by a widget cell's sort
-- key, and stay as given with no column to sort by.
runTableSortTest :: Context -> IORef Int -> IO ()
runTableSortTest _ failed = do
  let byKey = headed "Key" fst
      rows = [("b", 1), ("a", 2), ("b", 3), ("a", 4)] :: [(Text, Int)]
  assertEq failed [2, 4, 1, 3] (map snd (sortRows byKey (SortCol 0 SortAsc) (SA.smallArrayFromList rows)))
  assertEq failed [1, 3, 2, 4] (map snd (sortRows byKey (SortCol 0 SortDesc) rows))
  eaten <- newIORef []
  forM_ [(SortAsc, [1, 2, 3]), (SortDesc, [3, 2, 1])] $ \(dir, counts) -> do
    assertEq failed rows (sortRows (mempty :: Colonnade Headed (Text, Int) Text) (SortCol 0 dir) rows)
    assertEq failed counts (map snd (sortRows (mixedCols eaten) (SortCol 1 dir) fruits))

-- | Widget cells draw their widgets and take clicks, on the row they belong
-- to once a sort moves it. Each widget cell's panel carries its row's stripe
-- in its style index (even rows 1, odd rows 2) rather than a theme scope.
runTableWidgetCellsTest :: Context -> IORef Int -> IO ()
runTableWidgetCellsTest ctx failed = do
  eaten <- newIORef []
  sortV <- newVar (SortCol 0 SortAsc)
  runSim ctx failed (withInputOff 500 300) (table "mixed" (mixedCols eaten) fruits sortV) $ do
    mapM_ expect ["Fruit", "Count", "Button", "banana", "Eat cherry"]
    let na = ctxNodeArena ctx
        stripe acc i = do
          nt <- getNodeType na i
          if nt == NodePanel then (: acc) . (\si -> (si `div` 16) .&. 0x0F) <$> getStyleIdx na i else pure acc
    striped <- filter (/= 0) <$> liftIO (foldNodesM na stripe [])
    checkEq (IS.fromList [1, 2], 6) (IS.fromList striped, length striped)
    _ <- click "Eat apple"
    liftIO (writeVarIO sortV (SortCol 1 SortDesc))
    _ <- settle
    ys <- mapM (fmap rectY . rectOf) ["cherry", "banana", "apple"]
    checkEq (sort ys) ys
    _ <- click "Eat apple"
    checkEq ["apple", "apple"] =<< liftIO (readIORef eaten)

-- | The first three of 'demoPeopleCols'.
threeCols :: Colonnade Headed (T.Text, T.Text, T.Text, T.Text, T.Text) T.Text
threeCols = mconcat [headed "Name" (\(a, _, _, _, _) -> a), headed "Dept" (\(_, b, _, _, _) -> b), headed "Age" (\(_, _, c, _, _) -> c)]

-- | A sort set from outside the table moves its header mark, and the frame
-- repaints the headers that lose and gain it: no rect or text change covers
-- the mark, and an empty table has no rows to re-sort.
runTableSortMarkDamageTest :: Context -> IORef Int -> IO ()
runTableSortMarkDamageTest ctx failed = do
  sortV <- newVar (SortCol 0 SortAsc)
  let inp = withInputOff 400 240
      ui = table "marks" threeCols ([] :: [(T.Text, T.Text, T.Text, T.Text, T.Text)]) sortV
  spans0 <- run2Frames ctx inp ui >> runFrame ctx inp ui >> collectTextSpans ctx
  writeVarIO sortV (SortCol 1 SortAsc)
  _ <- runFrame ctx inp ui
  dmg <- takeDamage ctx
  forM_ ["Name", "Dept"] $ \h ->
    assertJust failed (findHeader h spans0) $ \(V2 x y) -> assert failed (damageCovers dmg (Rect x y 1 1))

-- | A row of 'runTableLiveRowsTest'; its middle column starts numeric.
data LiveRow = LiveRow Text Text Text

liveCols :: Colonnade Headed LiveRow Text
liveCols = mconcat [headed "Name" (\(LiveRow a _ _) -> a), headed "Qty" (\(LiveRow _ b _) -> b), headed "Note" (\(LiveRow _ _ c) -> c)]

-- | A table given its rows again each frame, a few of them changed, lays out
-- as a fresh one given the same rows: widths follow changed cells, added and
-- removed rows and a column that stops and starts being numeric (and so
-- changes font), whichever of its cells are not numbers, and the order
-- follows changed sort keys.
runTableLiveRowsTest :: Context -> IORef Int -> IO ()
runTableLiveRowsTest ctx0 failed = do
  let fonts c = withMonoFontMetrics c (monospaceMetrics 10)
      inp0 = (withInput 600 300) {inputMousePos = V2 590 290}
      laidOut c rows = warmup2 c inp0 (sortedTable (table "live" liveCols rows)) >> collectTextSpans c
      setAt i x xs = take i xs ++ x : drop (i + 1) xs
      rows0 = [LiveRow (T.pack [n]) (T.pack (show i)) "note" | (i, n) <- zip [1 :: Int ..] "dbeac"]
      edits =
        [ setAt 1 (LiveRow "a much longer name" "2" "x")
        , (++ [LiveRow "added" "40" "y"])
        , setAt 2 (LiveRow "e" "n/a" "z")
        , setAt 1 (LiveRow "b" "2" "x")
        , init
        , setAt 0 (LiveRow "zz" "1" "w")
        , setAt 2 (LiveRow "e" "3" "z")
        , setAt 3 (LiveRow "a" "n/a" "note")
        , setAt 1 (LiveRow "b" "none" "x")
        , setAt 3 (LiveRow "a" "4" "note")
        , setAt 3 (LiveRow "a" "four" "note")
        , take 1
        , (++ [LiveRow "y" "2" "note"])
        , setAt 1 (LiveRow "b" "2" "a much longer note")
        ]
      live = fonts ctx0
  void (laidOut live rows0)
  foldM_
    ( \rows edit -> do
        let rows' = edit rows
        fresh <- newContext >>= \c -> laidOut (fonts c) rows'
        laidOut live rows' >>= assertEq failed fresh
        pure rows'
    )
    rows0
    edits

-- | A header dragged onto another moves its column there, onto a column
-- boundary inside its resize zone too, which shows no resize arrow while the
-- column is held. A click is not a reorder, even past the drag threshold in
-- absolute x, and a released drag does not stay latched. A scrim over the
-- table blocks both reordering and resizing.
runTableReorderTest :: Context -> IORef Int -> IO ()
runTableReorderTest _ failed = do
  scrim <- newIORef False
  let ui = columnWith (fillW . fillH) $ do
        r <- simpleTable ["First", "Second", "Third"] [["a", "b", "c"], ["short"], []]
        covered <- liftIO (readIORef scrim)
        when covered (panelWith (pinAt 0 0 . pointer PointerBlock . fixedWH 500 240) (pure ()))
        pure r
      order = tableColOrder <$> settle
      onTable script = newPixelContext >>= \c -> runSim c failed (withInputOff 500 240) ui (script c)
  onTable $ \_ -> do
    first <- Within <$> rectOf "First"
    _ <- click first
    checkEq [0, 1, 2] =<< order
    third <- rectOf "Third"
    _ <- drag first (Within third) 1
    checkEq [1, 2, 0] =<< order
    _ <- moveTo first
    checkEq [1, 2, 0] =<< order
  onTable $ \c -> do
    [first, Rect sx sy sw sh, _] <- liftIO (headerButtonRects c)
    _ <- press (Within first)
    replicateM_ 2 (moveTo (at (sx + sw - 1) (sy + sh / 2)))
    check . (/= UiCursorEwResize) =<< cursorNow
    _ <- release
    checkEq [1, 0, 2] =<< order
  writeIORef scrim True
  onTable $ \c -> do
    before@(Rect fx fy fw fh : _) <- liftIO (headerButtonRects c)
    forM_ [V2 (fx + fw / 2) (fy + fh / 2), V2 (fx + fw - 1) (fy + fh / 2)] $ \p@(V2 x y) -> do
      _ <- drag (At p) (at (x + 120) y) 1
      checkEq [0, 1, 2] =<< order
      checkEq before =<< liftIO (headerButtonRects c)

-- | A right-click on a header hides its column while another stays visible,
-- whatever out-of-range indices the hidden set holds, and with the
-- stretching column hidden the others share its space. Two simple tables
-- side by side keep their own hidden columns.
runTableHideColumnsTest :: Context -> IORef Int -> IO ()
runTableHideColumnsTest ctx failed = do
  let cfg = defaultTableConfig {tableColSizes = [ColStretch, ColContent, ColContent], tableHidden = IS.singleton 5}
      ui = tableConfigured' cfg id "hide" threeCols tableFillRows =<< useVar (SortCol 0 SortAsc)
      rightEdge = maybe 0 (\r -> rectX r + rectW r) . listToMaybe . reverse
      headers = liftIO (headerButtonRects ctx)
      hideFirst = do
        first : _ <- headers
        _ <- rightClick (Within first)
        tableHiddenIndices <$> settle
  runSim ctx failed (withInputOff 500 240) ui $ do
    filled <- rightEdge <$> headers
    checkEq [0, 5] =<< hideFirst
    shared <- rightEdge <$> headers
    checkGt shared (filled - 1)
    checkEq [0, 1, 5] =<< hideFirst
    -- The last column stays.
    checkEq [0, 1, 5] =<< hideFirst
  c <- newContext
  let pair = columnWith fillW ((,) <$> simpleTable ["A", "B"] [["a1", "b1"]] <*> simpleTable ["X", "Y"] [["x1", "y1"]])
  runSim c failed (withInputOff 500 300) pair $ do
    (a, x) <- rightClick "A" >> settle
    checkEq ([0], []) (tableHiddenIndices a, tableHiddenIndices x)

-- | A sideways wheel over a frozen column scrolls the body across, also when
-- the rows fit and the frozen pane, which shares the body's id, has nothing
-- of its own to scroll. Only edges in view resize: an unfrozen column's edge
-- scrolled under the frozen one has no resize zone there.
runTableFrozenTest :: Context -> IORef Int -> IO ()
runTableFrozenTest ctx failed = do
  let cfg = defaultTableConfig {tableFreezeCols = 1, tableColSizes = [ColFixed 150, ColFixed 100, ColFixed 500]}
      ui = sortedTable (tableConfigured cfg (fixedH 200) "frozen" threeCols tableFillRows)
  runSim ctx failed (withInputOff 400 240) ui $ do
    Rect fx fy fw fh : _ <- liftIO (headerButtonRects ctx)
    _ <- wheel (at (fx + fw / 2) (fy + fh + 10)) (V2 1 0)
    V2 offX _ <- liftIO (bodyOffset ctx)
    checkGt offX 0
    Just wid <- liftIO (tableBodyScrollWid ctx)
    liftIO (setScrollOffset2D ctx wid (V2 120 0))
    [Rect _ _ frozenW _, Rect hx hy hw hh, _] <- settle >> liftIO (headerButtonRects ctx)
    check (hx + hw < fx + frozenW)
    check . (/= UiCursorEwResize) =<< cursorAt (at (hx + hw - 1) (hy + hh / 2))

-- | A table with frozen columns builds two scroll nodes under one widget id.
-- Only one may publish the body's geometry, or each frame would rewrite the
-- store twice for as long as the table is on screen: the pane owning both
-- bars publishes, and what it publishes holds still.
runTableSharedScrollMetricsTest :: Context -> IORef Int -> IO ()
runTableSharedScrollMetricsTest ctx failed = do
  let inp0 = (withInput 320 220) {inputMousePos = V2 40 80}
      ui = sortedTable (tableConfigured defaultTableConfig {tableFreezeCols = 1} (fixedH 150) "people" tableScrollCols tableScrollRows)
  replicateM_ 3 (runFrame ctx inp0 ui)
  assertJustM failed (tableBodyScrollWid ctx) $ \wid -> do
    before <- getScrollMetrics ctx wid
    assertEq failed (Just ScrollAxisXY) (fmap scrollAxes before)
    _ <- runFrame ctx inp0 ui
    assertEq failed before =<< getScrollMetrics ctx wid

-- | The N of a "row-N" label.
rowLabelIndex :: T.Text -> Maybe Int
rowLabelIndex t = readMaybe (T.unpack (T.takeWhile (/= ' ') (T.drop 4 t)))

-- | Wheeling with the pointer well above a table nested in a page scrolls
-- the page only: hit rects that drifted by the page's offset made the wheel
-- grab the table's phantom rect and scroll the table instead.
runPageWheelAboveTableTest :: Context -> IORef Int -> IO ()
runPageWheelAboveTableTest ctx failed = do
  let ui = scrollArea (fillW . fixedH 200 . gap 0) $ do
        mapM_ (\i -> label (T.pack ("head " <> show (i :: Int)))) [1 .. 10]
        sortedTable (tableWith (fixedH 120) "people" tableScrollCols tableScrollRows)
  runSim ctx failed (withInput 320 220) ui $ do
    (page, _) <- result
    replicateM_ 2 (wheel (at 160 80) (V2 0 5))
    off <- liftIO (getScrollOffset ctx page)
    checkGt off 0
    check . isJust =<< findSpan "row-1"

-- | Scrolling materializes the revealed rows in the frame the offset lands.
-- The offset applies after the view runs, so a frame that only moved the
-- old rows left the revealed strip without geometry.
runTableScrollRevealTest :: Context -> IORef Int -> IO ()
runTableScrollRevealTest ctx failed = do
  -- A one-line step keeps the rows the scroll lands on in ascending label
  -- order (the labels sort as text, so "row-2" comes after "row-19").
  setScrollTuning ctx defaultScrollTuning {scrollWheelStep = 20}
  let ui = sortedTable (tableWith (fixedH 150) "people" tableScrollCols tableScrollRows)
      rowSpans = (\ss -> [(r, t) | (r, t, _, _, _) <- ss, "row-" `T.isPrefixOf` t]) <$> spans
      bottomRow = listToMaybe . (\rs -> [n | (_, t) <- sortOn (Down . rectY . fst) rs, Just n <- [rowLabelIndex t]]) <$> rowSpans
  runSim ctx failed ((withInput 320 220) {inputMousePos = V2 40 80}) ui $ do
    lo <- bottomRow
    _ <- frameWith (\i -> i {inputScroll = V2 0 6})
    hi <- bottomRow
    checkJust ((,) <$> lo <*> hi) $ \(l, h) -> checkGt h l
    -- Real rows fill the revealed band, not a sliver of a stale one.
    check . (>= 3) . length =<< rowSpans

-- | When one cell wraps to several lines the whole row grows, and every cell
-- in it stretches to that height, so stripes and borders span the row. The
-- body starts right below the header: a scrollbar lane reserved with no
-- horizontal overflow opens a dead gap. A wrapping cell is as tall as the
-- lines paint draws in it: the solve wraps inside the cell's inset, as paint
-- does (two words fit a line of the 90px cell, but not inside its inset).
runTableWrapTest :: Context -> IORef Int -> IO ()
runTableWrapTest ctx failed = do
  let wrapCols = headed "Name" fst <> headed "Notes" snd
      lorem = T.unwords (replicate 24 "lorem")
      cfg = defaultTableConfig {tableColSizes = [ColFixed 280, ColFixed 90]}
      rows = [("row-" <> T.pack (show (i :: Int)), lorem) | i <- [1 .. 8]]
      na = ctxNodeArena ctx
  _ <- warmup2 ctx (withInputOff 700 400) (sortedTable (tableConfigured cfg id "wrap-stretch" wrapCols rows))
  -- Body cells: below the header band, and wider than 50px.
  cells <- foldNodesM na (\acc i -> do
    nt <- getNodeType na i
    Rect _ y w h <- getNodeRect na i
    pure (if nt == NodeText && y > 25 && w > 50 then (round y, [h]) : acc else acc)) []
  let rowBands = IM.toAscList (IM.fromListWith (++) cells)
  forM_ rowBands $ \(_, hs) -> assertEq failed 1 (length (nub hs))
  assert failed (any ((> 40) . maximum . snd) rowBands)
  assert failed (minimum (map fst rowBands) < 40)
  c <- newContext
  let notes = T.unwords (replicate 12 "abc")
      narrow = defaultTableConfig {tableColSizes = [ColFixed 120, ColFixed 90]}
      cna = ctxNodeArena c
  _ <- warmup2 c (withInputOff 400 400) (sortedTable (tableConfigured narrow id "wrap-lines" wrapCols [("row", notes)]))
  ss <- collectTextSpans c
  assertJustM failed (findNodeM cna (\i -> (== notes) <$> getText cna i)) $ \cell -> do
    Rect cx cy cw ch <- getNodeRect cna cell
    let lns = [r | (r, t, _, _, _) <- ss, "abc" `T.isPrefixOf` t, rectX r >= cx, rectX r < cx + cw, rectY r >= cy]
    assertEq failed 12 (length lns)
    assert failed (sum (map rectH lns) <= ch)

-- | The rects of the first spans holding each of @needles@ once @ui@ is laid
-- out; a missing one fails the test.
spanRects :: Context -> Input -> NanoUI a -> [T.Text] -> IO [Rect]
spanRects ctx inp ui needles = do
  ss <- warmup2 ctx inp ui >> collectTextSpans ctx
  forM needles $ \n -> maybe (fail ("no span " <> show n)) pure (spanRect n ss)

-- | A content-sized first column fits its longest cell, and in a fit-width
-- 2D table vertical overflow must not shrink it either. Content, stretch and
-- fixed columns fill the table's width. On a pixel host Age (right) and City
-- (left) do not sit on their shared grid line, and a plain table hands its
-- slack to the columns, so the last one reaches the far side.
runTableColumnWidthsTest :: Context -> IORef Int -> IO ()
runTableColumnWidthsTest ctx failed = do
  let firstCol = sortedTable (tableConfigured defaultTableConfig {tableColSizes = [ColContent, ColStretch]} id "people" tableScrollCols tableFirstColRows)
  [Rect cn _ cw _, Rect vx _ _ _] <- spanRects ctx ((withInput 400 200) {inputMousePos = V2 60 80}) firstCol ["long-first-col", "val-1"]
  assertGt failed cw 50
  assert failed (vx > cn + cw - 2)
  fitHost <- newPixelContext
  let fitLayout = fixedH 100 . (\l -> l {layoutWidth = Fit})
      fit = sortedTable (tableWith fitLayout "people" tableScrollCols tableFirstColRows)
  [Rect _ _ fw fh] <- spanRects fitHost ((withInput 280 180) {inputMousePos = V2 40 60}) fit ["long-first-col"]
  assertGt failed fw 50
  assert failed (fh < 40)
  fillHost <- newContext
  let sizes = [ColContent, ColStretch, ColFixed 64, ColStretch, ColContent]
      filled = sortedTable (tableConfigured defaultTableConfig {tableColSizes = sizes} id "people" demoPeopleCols tableFillRows)
      inp = (withInput 500 200) {inputMousePos = V2 200 80}
  [Rect nx _ _ _, Rect dx _ _ _, Rect rx _ rw _, Rect mx _ mw _] <- spanRects fillHost inp filled ["Name", "David", "Role", "Manager"]
  assert failed (abs (nx - dx) <= 1)
  assertGt failed (rx + rw) 380
  assertGt failed mw 50
  assert failed (mx >= rx - 2)
  pixel <- newPixelContext
  let plainTable = sortedTable (table "people" demoPeopleCols tableFillRows)
  [Rect hx _ _ _, Rect px _ _ _, Rect ax _ aw _, Rect tx _ _ _] <- spanRects pixel inp {inputWindowSize = Size 500 240} plainTable ["Name", "David", "63", "Austin"]
  assert failed (abs (hx - px) <= 1)
  assertGt failed px 4
  assertGt failed (tx - (ax + aw)) 10
  -- The slack check needs all five columns in view, which the pixel font's
  -- wider glyphs do not fit in 500px.
  plain <- newContext
  [Rect lx _ lw _] <- spanRects plain inp plainTable ["Role"]
  assertGt failed (lx + lw) 420

tableFirstColRows :: [(T.Text, T.Text)]
tableFirstColRows = ("long-first-col", "short") : take 8 tableScrollRows

-- | A header narrower than its text truncates with an ellipsis rather than
-- running into the next column, and its text is its label alone: the sort
-- mark's lane is an inset, not blank text. A stretch column flexes down to
-- its floor without forcing a sideways scroll, long text or not. A stretch
-- column also absorbs the vertical bar's lane (stronghead's leak suspects
-- table): the fixed columns fit the narrowed viewport, so the stretch
-- column compresses rather than surface a horizontal bar, and the last
-- column sits clear of the lane.
runTableNarrowColumnsTest :: Context -> IORef Int -> IO ()
runTableNarrowColumnsTest ctx failed = do
  let pairs a b = headed a fst <> headed b snd :: Colonnade Headed (Text, Text) Text
      truncated = sortedTable (tableConfigured defaultTableConfig {tableColSizes = [ColFixed 140, ColFixed 100]} id "trunc" (pairs "Long Column Title" "Next") [("val1", "val2")])
  ss <- warmup2 ctx (withInputOff 400 200) truncated >> collectTextSpans ctx
  let texts = [t | (_, t, _, _, _) <- ss]
      starting p = [r | (r, t, _, _, _) <- ss, p `T.isPrefixOf` t]
  assert failed (any ("..." `T.isInfixOf`) texts)
  assert failed ("Next" `elem` texts)
  case (starting "Long", starting "Next") of
    (Rect hx _ hw _ : _, Rect nx _ _ _ : _) -> assert failed (hx + hw <= nx)
    _ -> assert failed False
  floored <- newContext
  let stretchCfg = defaultTableConfig {tableColSizes = [ColStretch, ColFixed 60]}
  _ <- warmup2 floored (withInputOff 300 200) (sortedTable (tableConfigured stretchCfg id "stretch" (pairs "Stretch" "Fixed") [(T.replicate 100 "verylongtext", "123")]))
  scrolls <- nodesOfType NodeScrollContainer floored
  ranges <- mapM (fmap (fmap (v2X . scrollRange)) . getScrollMetrics floored . fst) scrolls
  assert failed (all (maybe True (<= 0)) ranges)
  lane <- newContext
  let cfg = defaultTableConfig {tableColSizes = [ColStretch, ColFixed 100, ColFixed 100, ColFixed 200]}
      -- Fixed columns plus rules (403) fit the narrowed viewport; the
      -- stretch column's old 40px floor (443 in all) did not.
      ui = sortedTable (tableConfigured cfg (fixedH (9 * 29 + 6)) "wereSuspects" (suspectCols textCell) (suspectRows 10))
  _ <- warmup2 lane ((withInput 455 240) {inputMousePos = V2 200 120}) ui
  assertJustM failed (findNodeM (ctxNodeArena lane) (isBodyScroller lane)) $ \i -> do
    Rect bx _ bw _ <- getNodeRect (ctxNodeArena lane) i
    assertJustM failed (bodyMetrics lane) $ \sm -> do
      assertGt failed (v2Y (scrollRange sm)) 0
      assertEq failed 0 (v2X (scrollRange sm))
      assertJustM failed (listToMaybe . reverse <$> headerButtonRects lane) $ \(Rect hx _ hw _) -> do
        assert failed (hx + hw <= bx + rectW (scrollViewport sm) + 0.5)
        assertGt failed (hx + hw) (bx + bw - 24)

-- | The columns of stronghead's leak suspects table, each cell made by @cell@.
suspectCols :: IsString c => (Text -> c) -> Colonnade Headed (Text, Text, Text, Text) c
suspectCols cell =
  mconcat
    [ headed "leak suspect" (\(a, _, _, _) -> cell a)
    , headed "peak before" (\(_, b, _, _) -> cell b)
    , headed "peak now" (\(_, _, c, _) -> cell c)
    , headed "status" (\(_, _, _, d) -> cell d)
    ]

suspectRows :: Int -> [(Text, Text, Text, Text)]
suspectRows n = [(T.pack ("suspect-" <> show k), "19.1 MB", "20.0 MB", "still there") | k <- [1 .. n]]

-- | Every row of cells and column rules tiles: each child's far edge is the
-- next one's origin. A cell that reaches a pixel past it paints over the
-- rule there, and the column's grid line vanishes down the whole table. At
-- fractional scales and fractional fixed widths too, whose sizes do not
-- round to whole device pixels.
runTableRulesTileTest :: Context -> IORef Int -> IO ()
runTableRulesTileTest _ failed =
  forM_ [(s, m, w) | s <- [1, 1.25, 1.5, 2], m <- [ColStretch, ColFixed 97.3], w <- [470, 473 .. 530]] $ \(scale, middle, winW) -> do
    base <- newContext
    let ctx = withFontMetrics base ((monospaceMetrics 12) {fmSnapScale = scale})
        cfg = defaultTableConfig {tableColSizes = [ColContent, ColFixed 61.7, middle, ColContent, ColContent]}
        na = ctxNodeArena ctx
    warmup2 ctx (withInputOff winW 240) (sortedTable (tableConfigured cfg id "people" demoPeopleCols tableFillRows))
    -- Each parent's children as (has a rule, child extents), in one pass.
    byParent <- foldNodesM na (\acc i -> do
      parent <- getParent na i
      if parent < 0 then pure acc else do
        nt <- getNodeType na i
        Rect x _ w _ <- getNodeRect na i
        let merge (r1, e1) (r2, e2) = (r1 || r2, e1 ++ e2)
        pure (IM.insertWith merge parent (nt == NodeSeparator, [(x, w)]) acc)) IM.empty
    rows <- filterM (fmap (== DirRow) . getDirection na) [p | (p, (True, _)) <- IM.toList byParent]
    assertGt failed (length rows) 0
    forM_ rows $ \rowIdx -> do
      let edges = sortOn fst (maybe [] snd (IM.lookup rowIdx byParent))
      assertEq failed [] [(x0, w0, x1) | ((x0, w0), (x1, _)) <- zip edges (drop 1 edges), abs (x0 + w0 - x1) > 1.0e-3]

-- | Resizing a column past the pane's right edge overflows the body. On
-- every drag frame the header row stays inside its scroller's clip, with no
-- frame where the horizontal bar's lane covers the header's bottom half, and
-- once the drag settles the body's horizontal bar drags the shared offset
-- both ways.
runTableResizeOverflowTest :: Context -> IORef Int -> IO ()
runTableResizeOverflowTest ctx failed = do
  -- Five rows put the body just inside the vertical bar's toggle band: the
  -- bar appears exactly when the header lane spacer does.
  let ui = sortedTable (tableWith (fixedH 180) "resize-lane" tableScrollCols (take 5 tableScrollRows))
      offX = (\(V2 x _) -> x) <$> liftIO (bodyOffset ctx)
  runSim ctx failed ((withInput 400 300) {inputMousePos = V2 30 30}) ui $ do
    Rect hx hy hw hh : _ <- liftIO (headerButtonRects ctx)
    let edgeX = hx + hw
        headerY = hy + hh / 2
    _ <- press (at (edgeX - 2) headerY)
    -- Well past the pane's edge (lane and bar appear), then back and forth
    -- inside the bar's gutter band, where viewport and lane flag disagree.
    forM_ [160, 320, 300, 290, 310, 300] $ \dx -> do
      _ <- moveTo (at (edgeX + dx) headerY)
      Just (Rect _ cy _ ch) <- liftIO (headerScrollerClip ctx)
      Rect _ y _ h : _ <- liftIO (headerButtonRects ctx)
      check (y + h <= cy + ch + 0.5)
    _ <- release >> settle
    Just (Rect bx by bw bh) <- liftIO (bodyScrollerRect ctx)
    let barY = by + bh - scrollBarWidth / 2
    _ <- press (at (bx + bw * 0.3) barY)
    off1 <- offX
    off2 <- moveTo (at (bx + bw * 0.95) barY) >> offX
    checkGt off2 off1
    off3 <- moveTo (at (bx + bw * 0.2) barY) >> offX
    checkLt off3 off2

-- | Content column of the header row scroller (the Row-direction one).
headerScrollerClip :: Context -> IO (Maybe Rect)
headerScrollerClip ctx = do
  let na = ctxNodeArena ctx
  found <- findNodeM na $ \i -> do
    nt <- getNodeType na i
    if nt == NodeScrollContainer then (== DirRow) <$> getDirection na i else pure False
  maybe (pure Nothing) (getClipRect na) found

-- | In the demo's page structure (page scroller, card panel, five columns),
-- every column boundary raises the resize cursor and resizes when grabbed,
-- on the header cell and down in the column body alike. The arrow stays for
-- the whole drag, off the edge too, and goes once the button is let go.
runTableColResizeTest :: Context -> IORef Int -> IO ()
runTableColResizeTest _ failed =
  forM_ [(inBody, k) | inBody <- [False, True], k <- [0 .. 4]] $ \(inBody, k) -> do
    -- A context per edge: widening earlier columns can scroll the last edge
    -- out of view, where it deliberately cannot start a resize.
    ctx <- newPixelContext
    let ui = scrollWith (tight . grow) . columnWith (padAll 6 . gap 6 . fillW) . card $
          sortedTable (tableWith (fixedH 280) "people" demoPeopleCols demoPeopleRows)
        widthNow = rectW . (!! k) <$> liftIO (headerButtonRects ctx)
    runSim ctx failed ((withInput 1200 500) {inputMousePos = V2 400 100}) ui $ do
      bodyBot <- liftIO (tableBodyBottom ctx)
      Rect hx hy hw hh : _ <- drop k <$> liftIO (headerButtonRects ctx)
      check (bodyBot > hy + hh + 20)
      let edgeX = hx + hw - 2
          grabY = if inBody then (hy + hh + bodyBot) / 2 else hy + hh / 2
      check . (/= UiCursorEwResize) =<< cursorAt (at (hx + hw / 2) grabY)
      checkEq UiCursorEwResize =<< cursorAt (at edgeX grabY)
      before <- widthNow
      _ <- press (at edgeX grabY)
      replicateM_ 3 (moveTo (at (edgeX + 60) grabY))
      after <- widthNow
      checkGt after (before + 30)
      checkEq UiCursorEwResize =<< cursorAt (at (edgeX + 60) 490)
      _ <- release
      check . (/= UiCursorEwResize) =<< cursorNow

demoPeopleCols :: Colonnade Headed (T.Text, T.Text, T.Text, T.Text, T.Text) T.Text
demoPeopleCols =
  mconcat
    [ headed "Name" (\(a, _, _, _, _) -> a)
    , headed "Dept" (\(_, b, _, _, _) -> b)
    , headed "Age" (\(_, _, c, _, _) -> c)
    , headed "City" (\(_, _, _, d, _) -> d)
    , headed "Role" (\(_, _, _, _, e) -> e)
    ]

demoPeopleRows :: [(T.Text, T.Text, T.Text, T.Text, T.Text)]
demoPeopleRows =
  [ (T.pack n, T.pack d, T.pack (show a), T.pack c, T.pack r)
  | (n, d, a, c, r) <-
      [ ("David", "Eng", 63 :: Int, "Austin", "Staff")
      , ("Ava", "Design", 34, "Berlin", "Lead")
      , ("Sonia", "Eng", 12, "Lisbon", "Intern")
      , ("Maya", "Ops", 41, "Tokyo", "Manager")
      , ("Leo", "Design", 28, "Paris", "IC")
      , ("Noah", "Eng", 37, "Seoul", "Staff")
      , ("Iris", "Ops", 19, "Austin", "IC")
      , ("Jules", "Sales", 45, "London", "Manager")
      , ("Priya", "Eng", 31, "Bengaluru", "Lead")
      , ("Chen", "Design", 26, "Shanghai", "IC")
      , ("Omar", "Ops", 52, "Cairo", "Lead")
      , ("Elena", "Sales", 39, "Madrid", "Staff")
      , ("Kai", "Eng", 23, "Oslo", "IC")
      , ("Ruth", "Ops", 47, "Boston", "Staff")
      ]
  ]

tableFillRows :: [(T.Text, T.Text, T.Text, T.Text, T.Text)]
tableFillRows = [demoPeopleRows !! i | i <- [0, 3, 9]]

-- | Name/value rows: @row-i@, @val-i@.
tableScrollCols :: Colonnade Headed (T.Text, T.Text) T.Text
tableScrollCols = headed "Name" fst <> headed "Value" snd

tableScrollRows :: [(T.Text, T.Text)]
tableScrollRows = [("row-" <> n, "val-" <> n) | i <- [1 .. 20 :: Int], let n = T.pack (show i)]

-- | All table-header button rects, left to right.
headerButtonRects :: Context -> IO [Rect]
headerButtonRects ctx = do
  let na = ctxNodeArena ctx
  rects <- foldNodesM na (\acc i -> do
    header <- isHeaderButton na i
    if header then (: acc) <$> getNodeRect na i else pure acc) []
  pure (sortOn rectX rects)

isHeaderButton :: NodeArena -> NodeIdx -> IO Bool
isHeaderButton na i = do
  nt <- getNodeType na i
  if nt == NodeButton then (\si -> si .&. 0x80000000 /= 0) <$> getStyleIdx na i else pure False

-- | Bottom edge of the table pane: from the first header button, walk up to
-- the enclosing panel and return its bottom Y.
tableBodyBottom :: Context -> IO Float
tableBodyBottom ctx = do
  let na = ctxNodeArena ctx
      walkUp i
        | i < 0 = pure 0
        | otherwise = do
            nt <- getNodeType na i
            if nt /= NodePanel
              then getParent na i >>= walkUp
              else (\(Rect _ py _ ph) -> py + ph) <$> getNodeRect na i
  findNodeM na (isHeaderButton na) >>= maybe (pure 0) walkUp

-- | The body (unfrozen) v-scroller: a Column-direction 2D scroller with
-- style bits "both policies Auto, clamp set".
isBodyScroller :: Context -> NodeIdx -> IO Bool
isBodyScroller ctx i = do
  let na = ctxNodeArena ctx
  nt <- getNodeType na i
  d <- getDirection na i
  si <- getStyleIdx na i
  pure (nt == NodeScrollContainer && d == DirColumn && si .&. 3 == 0 && (si `div` 4) .&. 3 == 0 && si .&. 16 /= 0)

bodyScrollerRect :: Context -> IO (Maybe Rect)
bodyScrollerRect ctx = traverse (getNodeRect (ctxNodeArena ctx)) =<< findNodeM (ctxNodeArena ctx) (isBodyScroller ctx)

-- | The body scroller's 2D offset.
bodyOffset :: Context -> IO V2
bodyOffset ctx = do
  let na = ctxNodeArena ctx
  found <- findNodeM na (isBodyScroller ctx)
  maybe (pure (V2 0 0)) (getScrollOffset2D ctx <=< getWidgetId na) found

-- | The body scroller's published metrics.
bodyMetrics :: Context -> IO (Maybe ScrollMetrics)
bodyMetrics ctx = do
  let na = ctxNodeArena ctx
  found <- findNodeM na (isBodyScroller ctx)
  maybe (pure Nothing) (getScrollMetrics ctx <=< getWidgetId na) found

-- | The widget id shared by the table body's panes: the id of the first scroll
-- container the arena holds that another scroll container repeats.
tableBodyScrollWid :: Context -> IO (Maybe WidgetId)
tableBodyScrollWid ctx = do
  wids <- map fst <$> nodesOfType NodeScrollContainer ctx
  pure (listToMaybe [w | w : rest <- tails wids, w `elem` rest])

-- | At the end of a sideways scroll the last column clears the vertical
-- bar's lane rather than stopping under it: the bar narrows the horizontal
-- viewport, so the reachable range counts the lane. Header and body cells
-- scroll in lockstep.
runTableHBarReachTest :: Context -> IORef Int -> IO ()
runTableHBarReachTest ctx failed = do
  let inp0 = (withInput 700 320) {inputMousePos = V2 300 160}
      cfg = defaultTableConfig {tableColSizes = [ColFixed 500, ColFixed 500]}
      ui = sortedTable (tableConfigured cfg (fixedH 200) "people" tableScrollCols tableScrollRows)
      na = ctxNodeArena ctx
  _ <- warmup2 ctx inp0 ui
  assertJustM failed (findNodeM na (isBodyScroller ctx)) $ \i -> do
    body@(Rect bx _ bw _) <- getNodeRect na i
    contentW <- getScrollContentW na i
    assertGt failed contentW bw
    replicateM_ 20 (runFrame ctx inp0 {inputMousePos = spanCenter body, inputScroll = V2 50 0} ui)
    V2 offX _ <- bodyOffset ctx
    assertGt failed offX (contentW - bw)
    assertJustM failed (listToMaybe . reverse <$> headerButtonRects ctx) $ \(Rect hx _ hw _) -> do
      assert failed (hx + hw <= bx + bw + 0.5)
      assertGt failed (hx + hw) (bx + bw - 24)
    ss <- collectTextSpans ctx
    let xOf needle = listToMaybe [rectX r | (r, t, _, _, _) <- ss, needle `T.isInfixOf` t]
    assertJust failed ((,) <$> xOf "Value" <*> xOf "val-") $ \(headerX, cellX) -> assert failed (abs (headerX - cellX) <= 1)

-- | 'tableSqueeze': pinned columns too wide for the body compress in the
-- frame the table first appears, none under the floor, instead of scrolling
-- sideways (without the flag the same table scrolls; see
-- 'runTableHBarReachTest'). Below two floors and the rule the table scrolls
-- again. A squeezed table with a leading stretch column keeps it visible in
-- a pane narrower than the fixed columns, and shows no vertical bar when
-- every row fits. In a pane grid split it clips to its pane, so both
-- dividers stay grabbable and resize pane and table.
runTableSqueezeTest :: Context -> IORef Int -> IO ()
runTableSqueezeTest ctx failed = do
  let cfg = defaultTableConfig {tableColSizes = [ColFixed 500, ColFixed 500], tableSqueeze = True}
      ui = sortedTable (tableConfigured cfg (fixedH 200) "squeeze" tableScrollCols tableScrollRows)
      inp0 = (withInput 700 320) {inputMousePos = V2 300 160}
      rangeX c = fmap (v2X . scrollRange) <$> bodyMetrics c
  _ <- runFrame ctx inp0 ui
  -- The first frame already has the squeezed widths.
  assertEq failed 2 . length =<< headerButtonRects ctx
  assert failed . all ((<= 351) . rectW) =<< headerButtonRects ctx
  _ <- warmup2 ctx inp0 ui
  assertJustM failed (bodyScrollerRect ctx) $ \(Rect _ _ bw _) -> do
    assertEq failed (Just 0) =<< rangeX ctx
    -- The split leaves room for the vertical bar's lane, as the rows
    -- overflow, so each half is a little under half the body.
    hdrs <- headerButtonRects ctx
    assert failed (all (\r -> rectW r > bw / 2 - 12 && rectW r <= bw / 2 + 1) hdrs)
  tiny <- newContext
  _ <- warmup2 tiny ((withInput 74 120) {inputMousePos = V2 30 60}) ui
  assertJustM failed (rangeX tiny) $ \r -> assertGt failed r 0
  let suspectCfg = defaultTableConfig {tableColSizes = [ColStretch, ColFixed 100, ColFixed 100, ColFixed 200], tableSqueeze = True}
      cell txt = cellSort txt (cellWidget (rowWith (tight . fillW . fillH . alignMid) (labelWith fillW txt)))
      suspects layout name n = sortedTable (tableConfigured suspectCfg layout name (suspectCols cell) (suspectRows n))
      inPane = columnWith (fillW . gap 8) $ do
        labelWith (fontSizeScale 0.92) "Header above table"
        suspects (maxH (9 * 29 + 6)) "wereSuspects" 4
  forM_ [300, 380, 440] $ \winW -> do
    c <- newContext
    let c15 = withFontMetrics c ((ctxFontMetrics c) {fmLineHeight = 20, fmAscent = 15})
    _ <- warmup2 c15 ((withInput winW 360) {inputMousePos = V2 100 100}) inPane
    hdrs <- headerButtonRects c15
    assertEq failed 4 (length hdrs)
    assert failed (all ((> 39.5) . rectW) hdrs)
    assertJustM failed (bodyMetrics c15) $ \sm -> assertEq failed (V2 0 0) (scrollRange sm)
  -- Panes 2 (the table) and 3 below pane 1, as in stronghead's heap pane.
  pg <- newContext
  let heapTree = Split 100 AxisH 0.5 (Pane 1) (Split 101 AxisV 0.5 (Pane 2) (Pane 3))
      pane pid _ = PaneView "P" True <$ if pid == 2 then suspects (grow . tight) "paneTable" 30 else label "other"
      gridCfg = defaultPaneGridConfig {pgLayout = tight . fillW . fillH, pgMinSize = 40, pgSpacing = 4}
      heapPane = columnWith (tight . fillW . fillH) (paneGrid gridCfg {pgTree = InitialTree (Just heapTree), pgViewPane = pane})
      headers = liftIO (headerButtonRects pg)
      topOf = map (round . rectY) . take 1
  runSim pg failed ((withInput 800 600) {inputMousePos = V2 100 100}) heapPane $ do
    before <- headers
    checkEq (4, [308 :: Int]) (length before, topOf before)
    -- The vertical divider (x 400, y 450) left to x 300, then the horizontal
    -- one (x 400, y 300) down to y 400.
    _ <- drag (at 400 450) (at 300 450) 1 >> settle
    _ <- drag (at 400 300) (at 400 400) 1 >> settle
    after <- headers
    checkEq (4, [408]) (length after, topOf after)
    check (all ((> 39.5) . rectW) after)
    check (all (\r -> rectX r + rectW r <= 302) (take 1 (reverse after)))
