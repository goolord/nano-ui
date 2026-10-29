{-# OPTIONS_GHC -O0 #-}
module Cases.Table (tests) where

import Spec
import Data.Bits ((.&.))
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (nub, sort, sortOn)
import Data.Ord (Down (..))
import Data.Maybe (listToMaybe, mapMaybe)
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
  , pixelSimSpec "page-wheel-above-table" 320 220 pageAboveTableUi pageWheelAboveTable
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

-- | A table sorted ascending on its first column, given everything but the sort.
sortedTable :: (Var SortCol -> NanoUI ()) -> NanoUI ()
sortedTable build = build =<< useVar (SortCol 0 SortAsc)

-- | A sorted table of @rows@ with config @cfg@, laid out by @layout@.
tableOf :: ToCell c => TableConfig -> (Layout -> Layout) -> Colonnade Headed r c -> [r] -> NanoUI ()
tableOf cfg layout cols rows = sortedTable (tableConfigured cfg layout "t" cols rows)

-- | A sorted table with these column sizes.
sizedTable :: ToCell c => [ColSize] -> Colonnade Headed r c -> [r] -> NanoUI ()
sizedTable sizes = tableOf defaultTableConfig {tableColSizes = sizes} id

type Person = (Text, Text, Text, Text, Text)

-- | The first @n@ columns of the demo's people table.
peopleCols :: Int -> Colonnade Headed Person Text
peopleCols n =
  mconcat . take n $
    [ headed "Name" (\(a, _, _, _, _) -> a)
    , headed "Dept" (\(_, b, _, _, _) -> b)
    , headed "Age" (\(_, _, c, _, _) -> c)
    , headed "City" (\(_, _, _, d, _) -> d)
    , headed "Role" (\(_, _, _, _, e) -> e)
    ]

people :: [Person]
people =
  [ ("David", "Eng", "63", "Austin", "Staff")
  , ("Maya", "Ops", "41", "Tokyo", "Manager")
  , ("Chen", "Design", "26", "Shanghai", "IC")
  ]

-- | Name/value rows: @row-i@, @val-i@.
scrollCols :: Colonnade Headed (Text, Text) Text
scrollCols = headed "Name" fst <> headed "Value" snd

scrollRows :: [(Text, Text)]
scrollRows = [("row-" <> n, "val-" <> n) | i <- [1 .. 20 :: Int], let n = T.pack (show i)]

-- | Fruit names, a widget cell sorted by its count, and a button that logs
-- the fruit it eats.
mixedCols :: IORef [Text] -> Colonnade Headed (Text, Int) Cell
mixedCols eaten =
  mconcat
    [ headed "Fruit" (textCell . fst)
    , headed "Count" (\(_, n) -> cellSort (T.pack (show n)) (cellWidget (label (T.pack (show n)))))
    , headed "Button" (widgetCell (\(f, _) -> whenM (button ("Eat " <> f)) (eat f)))
    ]
  where
    eat f = liftIO (modifyIORef' eaten (f :))

fruits :: [(Text, Int)]
fruits = [("banana", 2), ("apple", 1), ("cherry", 3)]

-- | Rows sort stably by a column's text, and stay as given with no column
-- to sort by. (Widget cells sort by their key in "table-widget-cells".)
runTableSortTest :: Context -> IORef Int -> IO ()
runTableSortTest _ failed = do
  let byKey = headed "Key" fst
      rows = [("b", 1), ("a", 2), ("b", 3), ("a", 4)] :: [(Text, Int)]
  assertEq failed [2, 4, 1, 3] (map snd (sortRows byKey (SortCol 0 SortAsc) (SA.smallArrayFromList rows)))
  assertEq failed [1, 3, 2, 4] (map snd (sortRows byKey (SortCol 0 SortDesc) rows))
  forM_ [SortAsc, SortDesc] $ \dir ->
    assertEq failed rows (sortRows (mempty :: Colonnade Headed (Text, Int) Text) (SortCol 0 dir) rows)

-- | Widget cells take clicks on the row they belong to, after a sort too, and
-- carry their row's stripe in their panel's style index (even rows 1, odd 2).
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
    writeV sortV (SortCol 1 SortDesc)
    _ <- settle
    ys <- mapM (fmap rectY . rectOf) ["cherry", "banana", "apple"]
    checkEq (sort ys) ys
    _ <- click "Eat apple"
    checkEq ["apple", "apple"] =<< liftIO (readIORef eaten)

-- | A sort set from outside repaints the headers that lose and gain the mark,
-- which no rect or text change covers (an empty table has no rows to re-sort).
runTableSortMarkDamageTest :: Context -> IORef Int -> IO ()
runTableSortMarkDamageTest ctx failed = do
  sortV <- newVar (SortCol 0 SortAsc)
  let inp = withInputOff 400 240
      ui = table "marks" (peopleCols 3) ([] :: [Person]) sortV
  spans0 <- run2Frames ctx inp ui >> runFrame ctx inp ui >> collectTextSpans ctx
  writeVarIO sortV (SortCol 1 SortAsc)
  _ <- runFrame ctx inp ui
  dmg <- takeDamage ctx
  forM_ ["Name", "Dept"] $ \h ->
    assertJust failed (findHeader h spans0) $ \(V2 x y) -> assert failed (damageCovers dmg (Rect x y 1 1))

-- | A row of 'runTableLiveRowsTest'; its middle column starts numeric.
data LiveRow = LiveRow Text Text Text

-- | A table given its rows again each frame, a few changed, lays out as a
-- fresh one given the same rows: widths, row counts, a column that stops and
-- starts being numeric (and so changes font), and the order all follow.
runTableLiveRowsTest :: Context -> IORef Int -> IO ()
runTableLiveRowsTest ctx0 failed = do
  let cols = mconcat [headed h (\(LiveRow a b c) -> [a, b, c] !! k) | (k, h) <- zip [0 ..] names]
      names = ["Name", "Qty", "Note"]
      fonts c = withMonoFontMetrics c (monospaceMetrics 10)
      inp0 = (withInput 600 300) {inputMousePos = V2 590 290}
      laidOut c rows = warmup2 c inp0 (sortedTable (table "live" cols rows)) >> collectTextSpans c
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

-- | A header dropped on another, or on a boundary's resize zone (no resize
-- arrow while held), moves its column there. A click does not, a released
-- drag does not stay latched, and a scrim blocks reordering and resizing.
runTableReorderTest :: Context -> IORef Int -> IO ()
runTableReorderTest _ failed = do
  scrim <- newIORef False
  let ui = columnWith (fillW . fillH) $ do
        r <- simpleTable ["First", "Second", "Third"] [["a", "b", "c"], ["short"], []]
        covered <- liftIO (readIORef scrim)
        when covered (panelWith (pinAt 0 0 . pointer PointerBlock . fixedWH 500 240) (pure ()))
        pure r
      order = tableColOrder <$> settle
      onTable script = newPixelContext >>= \c -> runSim c failed (withInputOff 500 240) ui script
  onTable $ do
    first <- Within <$> rectOf "First"
    _ <- click first
    checkEq [0, 1, 2] =<< order
    third <- rectOf "Third"
    _ <- drag first (Within third) 1
    checkEq [1, 2, 0] =<< order
    _ <- moveTo first
    checkEq [1, 2, 0] =<< order
  onTable $ do
    [first, Rect sx sy sw sh, _] <- headers
    _ <- press (Within first)
    replicateM_ 2 (moveTo (at (sx + sw - 1) (sy + sh / 2)))
    check . (/= UiCursorEwResize) =<< pointerCursor
    _ <- release
    checkEq [1, 0, 2] =<< order
  writeIORef scrim True
  onTable $ do
    before@(Rect fx fy fw fh : _) <- headers
    forM_ [V2 (fx + fw / 2) (fy + fh / 2), V2 (fx + fw - 1) (fy + fh / 2)] $ \p@(V2 x y) -> do
      _ <- drag (At p) (at (x + 120) y) 1
      checkEq [0, 1, 2] =<< order
      checkEq before =<< headers

-- | A right-click on a header hides its column while another stays visible,
-- whatever out-of-range indices the hidden set holds; the others share a
-- hidden stretch column's space. Side by side tables keep their own.
runTableHideColumnsTest :: Context -> IORef Int -> IO ()
runTableHideColumnsTest ctx failed = do
  let sizes = [ColStretch, ColContent, ColContent]
      cfg = defaultTableConfig {tableColSizes = sizes, tableHidden = IS.singleton 5}
      ui = tableConfigured' cfg id "hide" (peopleCols 3) people =<< useVar (SortCol 0 SortAsc)
      rightEdge = maybe 0 (\r -> rectX r + rectW r) . listToMaybe . reverse
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
  let pair = columnWith fillW ((,) <$> simpleTable ["A", "B"] [["a1"]] <*> simpleTable ["X", "Y"] [["x1"]])
  runSim c failed (withInputOff 500 300) pair $ do
    (a, x) <- rightClick "A" >> settle
    checkEq ([0], []) (tableHiddenIndices a, tableHiddenIndices x)

-- | A sideways wheel over a frozen column scrolls the body, though the rows
-- fit and the frozen pane sharing its id has nothing to scroll. A column
-- edge scrolled under the frozen column has no resize zone there. Of the
-- two scroll nodes the table builds under one id, only the pane owning both
-- bars publishes metrics, and they hold still (two publishers rewrote the
-- store every frame).
runTableFrozenTest :: Context -> IORef Int -> IO ()
runTableFrozenTest ctx failed = do
  let cfg = defaultTableConfig {tableFreezeCols = 1, tableColSizes = map ColFixed [150, 100, 500]}
  runSim ctx failed (withInputOff 400 240) (tableOf cfg (fixedH 200) (peopleCols 3) people) $ do
    Just wid <- onContext bodyWid
    metrics <- onContext (`getScrollMetrics` wid)
    checkEq (Just ScrollAxisXY) (scrollAxes <$> metrics)
    checkEq metrics =<< (frame >> onContext (`getScrollMetrics` wid))
    Rect fx fy fw fh : _ <- headers
    _ <- wheel (at (fx + fw / 2) (fy + fh + 10)) (V2 1 0)
    V2 offX _ <- scrollOffset2DOf wid
    checkGt offX 0
    onContext (\c -> setScrollOffset2D c wid (V2 120 0))
    [Rect _ _ frozenW _, Rect hx hy hw hh, _] <- settle >> headers
    check (hx + hw < fx + frozenW)
    check . (/= UiCursorEwResize) =<< cursorAt (at (hx + hw - 1) (hy + hh / 2))

pageAboveTableUi :: NanoUI (WidgetId, ())
pageAboveTableUi = scrollArea (fillW . fixedH 200 . gap 0) $ do
  mapM_ (\i -> label (T.pack ("head " <> show (i :: Int)))) [1 .. 10]
  tableOf defaultTableConfig (fixedH 120) scrollCols scrollRows

-- | The wheel well above a table in a page scrolls the page: hit rects that
-- drifted by the page's offset made it scroll the table's phantom rect.
pageWheelAboveTable :: Sim (WidgetId, ()) ()
pageWheelAboveTable = do
  (page, _) <- result
  replicateM_ 2 (wheel (at 160 80) (V2 0 5))
  off <- scrollOffsetOf page
  checkGt off 0
  check =<< labelShown "row-1"

-- | Scrolling builds the revealed rows in the frame the offset lands (it
-- applies after the view runs, which left the revealed strip empty).
runTableScrollRevealTest :: Context -> IORef Int -> IO ()
runTableScrollRevealTest ctx failed = do
  -- A one-line step keeps the rows the scroll lands on in ascending label
  -- order (the labels sort as text, so "row-2" comes after "row-19").
  setScrollTuning ctx defaultScrollTuning {scrollWheelStep = 20}
  let rowSpans = (\ss -> [(r, t) | (r, t, _, _, _) <- ss, "row-" `T.isPrefixOf` t]) <$> spans
      -- The N of the lowest "row-N" label.
      rowIndex t = readMaybe (T.unpack (T.takeWhile (/= ' ') (T.drop 4 t))) :: Maybe Int
      bottomRow = listToMaybe . mapMaybe (rowIndex . snd) . sortOn (Down . rectY . fst) <$> rowSpans
      ui = tableOf defaultTableConfig (fixedH 150) scrollCols scrollRows
  runSim ctx failed ((withInput 320 220) {inputMousePos = V2 40 80}) ui $ do
    lo <- bottomRow
    hi <- frameWith (\i -> i {inputScroll = V2 0 6}) >> bottomRow
    checkJust ((,) <$> lo <*> hi) $ \(l, h) -> checkGt h l
    -- Real rows fill the revealed band, not a sliver of a stale one.
    check . (>= 3) . length =<< rowSpans

-- | A row with a wrapping cell grows and all its cells stretch to it, right
-- below the header (no dead lane gap). A wrapping cell is as tall as the
-- lines paint draws in it: two words fit the 90px cell, not its inset.
runTableWrapTest :: Context -> IORef Int -> IO ()
runTableWrapTest ctx failed = do
  let wrapCols = headed "Name" fst <> headed "Notes" snd
      lorem = T.unwords (replicate 24 "lorem")
      na = ctxNodeArena ctx
      wrapped = [(T.pack (show i), lorem) | i <- [1 .. 8 :: Int]]
  _ <- warmup2 ctx (withInputOff 700 400) (sizedTable [ColFixed 280, ColFixed 90] wrapCols wrapped)
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
      cna = ctxNodeArena c
  _ <- warmup2 c (withInputOff 400 400) (sizedTable [ColFixed 120, ColFixed 90] wrapCols [("row", notes)])
  ss <- collectTextSpans c
  assertJustM failed (findNodeM cna (\i -> (== notes) <$> getText cna i)) $ \cell -> do
    Rect cx cy cw ch <- getNodeRect cna cell
    let lns = [r | (r@(Rect x y _ _), t, _, _, _) <- ss, "abc" `T.isPrefixOf` t, x >= cx, x < cx + cw, y >= cy]
    assertEq failed 12 (length lns)
    assert failed (sum (map rectH lns) <= ch)

-- | The rects of the first spans holding each of @needles@ once @ui@ is laid
-- out on @ctx@ in a @w@ by @h@ window; a missing one fails the test.
spanRects :: Context -> Float -> Float -> NanoUI a -> [Text] -> IO [Rect]
spanRects ctx w h ui needles = do
  ss <- warmup2 ctx (withInput w h) {inputMousePos = V2 40 60} ui >> collectTextSpans ctx
  forM needles $ \n -> maybe (fail ("no span " <> show n)) pure (spanRect n ss)

-- | A content-sized first column fits its longest cell, in a fit-width table
-- with vertical overflow too. Mixed sizes fill the width. Age (right) and
-- City (left) keep off their shared rule, and slack goes to the columns.
runTableColumnWidthsTest :: Context -> IORef Int -> IO ()
runTableColumnWidthsTest ctx failed = do
  let firstRows = ("long-first-col", "short") : take 8 scrollRows
      firstCol = sizedTable [ColContent, ColStretch] scrollCols firstRows
  [Rect cn _ cw _, Rect vx _ _ _] <- spanRects ctx 400 200 firstCol ["long-first-col", "val-1"]
  assertGt failed cw 50
  assert failed (vx > cn + cw - 2)
  fitHost <- newPixelContext
  let fit = tableOf defaultTableConfig (fixedH 100 . (\l -> l {layoutWidth = Fit})) scrollCols firstRows
  [Rect _ _ fw fh] <- spanRects fitHost 280 180 fit ["long-first-col"]
  assertGt failed fw 50
  assert failed (fh < 40)
  fillHost <- newContext
  let filled = sizedTable [ColContent, ColStretch, ColFixed 64, ColStretch, ColContent] (peopleCols 5) people
  [Rect nx _ _ _, Rect dx _ _ _, Rect rx _ rw _, Rect mx _ mw _] <-
    spanRects fillHost 500 200 filled ["Name", "David", "Role", "Manager"]
  assert failed (abs (nx - dx) <= 1)
  assertGt failed (rx + rw) 380
  assertGt failed mw 50
  assert failed (mx >= rx - 2)
  pixel <- newPixelContext
  let plainTable = sortedTable (table "people" (peopleCols 5) people)
  [Rect hx _ _ _, Rect px _ _ _, Rect ax _ aw _, Rect tx _ _ _] <-
    spanRects pixel 500 240 plainTable ["Name", "David", "63", "Austin"]
  assert failed (abs (hx - px) <= 1)
  assertGt failed px 4
  assertGt failed (tx - (ax + aw)) 10
  -- The slack check needs all five columns in view, which the pixel font's
  -- wider glyphs do not fit in 500px.
  plain <- newContext
  [Rect lx _ lw _] <- spanRects plain 500 200 plainTable ["Role"]
  assertGt failed (lx + lw) 420

-- | A narrow header truncates with an ellipsis short of the next column, its
-- text its label alone (the sort mark's lane is an inset). A stretch column
-- flexes to its floor rather than scroll sideways, and absorbs the vertical
-- bar's lane (stronghead's leak suspects), its last column clear of it.
runTableNarrowColumnsTest :: Context -> IORef Int -> IO ()
runTableNarrowColumnsTest ctx failed = do
  let pairs a b = headed a fst <> headed b snd :: Colonnade Headed (Text, Text) Text
      truncated = sizedTable [ColFixed 140, ColFixed 100] (pairs "Long Column Title" "Next") [("val1", "val2")]
  ss <- warmup2 ctx (withInputOff 400 200) truncated >> collectTextSpans ctx
  let texts = [t | (_, t, _, _, _) <- ss]
      starting p = [r | (r, t, _, _, _) <- ss, p `T.isPrefixOf` t]
  assert failed (any ("..." `T.isInfixOf`) texts)
  assert failed ("Next" `elem` texts)
  case (starting "Long", starting "Next") of
    (Rect hx _ hw _ : _, Rect nx _ _ _ : _) -> assert failed (hx + hw <= nx)
    _ -> assert failed False
  floored <- newContext
  let stretched = sizedTable [ColStretch, ColFixed 60] (pairs "Stretch" "Fixed") [(T.replicate 400 "x", "1")]
  _ <- warmup2 floored (withInputOff 300 200) stretched
  scrolls <- nodesOfType NodeScrollContainer floored
  ranges <- mapM (fmap (fmap (v2X . scrollRange)) . getScrollMetrics floored . fst) scrolls
  assert failed (all (maybe True (<= 0)) ranges)
  lane <- newContext
  -- Fixed columns plus rules (403) fit the narrowed viewport; the stretch
  -- column's old 40px floor (443 in all) did not.
  let suspects =
        tableOf defaultTableConfig {tableColSizes = suspectSizes} (fixedH 267) (suspectCols textCell) $
          suspectRows 10
  _ <- warmup2 lane ((withInput 455 240) {inputMousePos = V2 200 120}) suspects
  assertJustM failed (bodyNode lane) $ \i -> do
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

suspectSizes :: [ColSize]
suspectSizes = [ColStretch, ColFixed 100, ColFixed 100, ColFixed 200]

suspectRows :: Int -> [(Text, Text, Text, Text)]
suspectRows n = [(T.pack ("suspect-" <> show k), "19.1 MB", "20.0 MB", "still there") | k <- [1 .. n]]

-- | Cells and column rules tile: each child's far edge is the next one's
-- origin, at fractional scales and widths too. A cell a pixel past it paints
-- over the rule, and the grid line vanishes down the whole table.
runTableRulesTileTest :: Context -> IORef Int -> IO ()
runTableRulesTileTest _ failed =
  forM_ [(s, m, w) | s <- [1, 1.25, 1.5, 2], m <- [ColStretch, ColFixed 97.3], w <- [470, 473 .. 530]] $
    \(scale, mid, winW) -> do
      base <- newContext
      let ctx = withFontMetrics base ((monospaceMetrics 12) {fmSnapScale = scale})
          na = ctxNodeArena ctx
          merge (r1, e1) (r2, e2) = (r1 || r2, e1 ++ e2)
          sizes = [ColContent, ColFixed 61.7, mid, ColContent, ColContent]
      warmup2 ctx (withInputOff winW 240) (sizedTable sizes (peopleCols 5) people)
      -- Each parent's children as (has a rule, child extents), in one pass.
      byParent <- foldNodesM na (\acc i -> do
        parent <- getParent na i
        nt <- getNodeType na i
        Rect x _ w _ <- getNodeRect na i
        let entry = (nt == NodeSeparator, [(x, w)])
        pure (if parent < 0 then acc else IM.insertWith merge parent entry acc)) IM.empty
      rows <- filterM (fmap (== DirRow) . getDirection na) [p | (p, (True, _)) <- IM.toList byParent]
      assertGt failed (length rows) 0
      forM_ rows $ \rowIdx -> do
        let edges = sortOn fst (maybe [] snd (IM.lookup rowIdx byParent))
            gaps = [(x0, w0, x1) | ((x0, w0), (x1, _)) <- zip edges (drop 1 edges), abs (x0 + w0 - x1) > 1e-3]
        assertEq failed [] gaps

-- | A column resized past the pane's edge overflows the body: on no drag
-- frame does the bar's lane cover the header, and afterwards the body's
-- horizontal bar drags the shared offset both ways.
runTableResizeOverflowTest :: Context -> IORef Int -> IO ()
runTableResizeOverflowTest ctx failed = do
  -- Five rows put the body just inside the vertical bar's toggle band: the
  -- bar appears exactly when the header lane spacer does.
  let ui = tableOf defaultTableConfig (fixedH 180) scrollCols (take 5 scrollRows)
      offX = (\(V2 x _) -> x) <$> onContext bodyOffset
  runSim ctx failed ((withInput 400 300) {inputMousePos = V2 30 30}) ui $ do
    Rect hx hy hw hh : _ <- headers
    let edgeX = hx + hw
        headerY = hy + hh / 2
    _ <- press (at (edgeX - 2) headerY)
    -- Well past the pane's edge (lane and bar appear), then back and forth
    -- inside the bar's gutter band, where viewport and lane flag disagree.
    forM_ [160, 320, 300, 290, 310, 300] $ \dx -> do
      _ <- moveTo (at (edgeX + dx) headerY)
      Just (Rect _ cy _ ch) <- onContext headerScrollerClip
      Rect _ y _ h : _ <- headers
      check (y + h <= cy + ch + 0.5)
    _ <- release >> settle
    Just (Rect bx by bw bh) <- onContext bodyScrollerRect
    let barY = by + bh - scrollBarWidth / 2
    _ <- press (at (bx + bw * 0.3) barY)
    off1 <- offX
    off2 <- moveTo (at (bx + bw * 0.95) barY) >> offX
    checkGt off2 off1
    off3 <- moveTo (at (bx + bw * 0.2) barY) >> offX
    checkLt off3 off2

-- | The content clip of the header row's scroller (the Row-direction one).
headerScrollerClip :: Context -> IO (Maybe Rect)
headerScrollerClip ctx = do
  let na = ctxNodeArena ctx
      isRowScroller i = (== (NodeScrollContainer, DirRow)) <$> ((,) <$> getNodeType na i <*> getDirection na i)
  maybe (pure Nothing) (getClipRect na) =<< findNodeM na isRowScroller

-- | In the demo's page structure, every column boundary shows the resize
-- arrow and resizes when grabbed, on the header and in the body alike; the
-- arrow stays for the whole drag and goes on release.
runTableColResizeTest :: Context -> IORef Int -> IO ()
runTableColResizeTest _ failed =
  forM_ [(inBody, k) | inBody <- [False, True], k <- [0 .. 4]] $ \(inBody, k) -> do
    -- A context per edge: widening earlier columns can scroll the last edge
    -- out of view, where it deliberately cannot start a resize.
    ctx <- newPixelContext
    let ui = scrollWith (tight . grow) . columnWith (padAll 6 . gap 6 . fillW) . card $
          tableOf defaultTableConfig (fixedH 280) (peopleCols 5) (take 14 (cycle people))
        widthNow = rectW . (!! k) <$> headers
    runSim ctx failed ((withInput 1200 500) {inputMousePos = V2 400 100}) ui $ do
      bodyBot <- onContext tableBodyBottom
      Rect hx hy hw hh : _ <- drop k <$> headers
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
      check . (/= UiCursorEwResize) =<< pointerCursor

-- | 'headerButtonRects' in a script.
headers :: Sim a [Rect]
headers = onContext headerButtonRects

-- | All table-header button rects, left to right.
headerButtonRects :: Context -> IO [Rect]
headerButtonRects ctx = do
  let na = ctxNodeArena ctx
      collect acc i = isHeaderButton na i >>= \h -> if h then (: acc) <$> getNodeRect na i else pure acc
  sortOn rectX <$> foldNodesM na collect []

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

-- | The body (unfrozen) v-scroller's node: a Column-direction 2D scroller
-- with style bits "both policies Auto, clamp set".
bodyNode :: Context -> IO (Maybe NodeIdx)
bodyNode ctx = findNodeM na $ \i -> do
  nt <- getNodeType na i
  d <- getDirection na i
  si <- getStyleIdx na i
  pure (nt == NodeScrollContainer && d == DirColumn && si .&. 15 == 0 && si .&. 16 /= 0)
  where
    na = ctxNodeArena ctx

bodyScrollerRect :: Context -> IO (Maybe Rect)
bodyScrollerRect ctx = traverse (getNodeRect (ctxNodeArena ctx)) =<< bodyNode ctx

bodyWid :: Context -> IO (Maybe WidgetId)
bodyWid ctx = traverse (getWidgetId (ctxNodeArena ctx)) =<< bodyNode ctx

-- | The body scroller's 2D offset.
bodyOffset :: Context -> IO V2
bodyOffset ctx = maybe (pure (V2 0 0)) (getScrollOffset2D ctx) =<< bodyWid ctx

-- | The body scroller's published metrics.
bodyMetrics :: Context -> IO (Maybe ScrollMetrics)
bodyMetrics ctx = maybe (pure Nothing) (getScrollMetrics ctx) =<< bodyWid ctx

-- | At the end of a sideways scroll the last column clears the vertical
-- bar's lane, which the reachable range counts. Header and cells move as one.
runTableHBarReachTest :: Context -> IORef Int -> IO ()
runTableHBarReachTest ctx failed = do
  let inp0 = (withInput 700 320) {inputMousePos = V2 300 160}
      ui = tableOf (wideCols False) (fixedH 200) scrollCols scrollRows
      na = ctxNodeArena ctx
  _ <- warmup2 ctx inp0 ui
  assertJustM failed (bodyNode ctx) $ \i -> do
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
    assertJust failed ((,) <$> xOf "Value" <*> xOf "val-") $ \(headerX, cellX) ->
      assert failed (abs (headerX - cellX) <= 1)

-- | Two 500px columns: squeezed or not.
wideCols :: Bool -> TableConfig
wideCols squeeze = defaultTableConfig {tableColSizes = [ColFixed 500, ColFixed 500], tableSqueeze = squeeze}

-- | 'tableSqueeze': fixed columns too wide for the body compress from the
-- first frame, none under the floor, until two floors and the rule no longer
-- fit. A leading stretch column stays visible, with no vertical bar when the
-- rows fit, and in a pane grid split the table clips to its pane, leaving
-- both dividers grabbable.
runTableSqueezeTest :: Context -> IORef Int -> IO ()
runTableSqueezeTest ctx failed = do
  let ui = tableOf (wideCols True) (fixedH 200) scrollCols scrollRows
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
  let cell txt = cellSort txt (cellWidget (rowWith (tight . fillW . fillH . alignMid) (labelWith fillW txt)))
      squeezed = defaultTableConfig {tableColSizes = suspectSizes, tableSqueeze = True}
      suspects layout n = tableOf squeezed layout (suspectCols cell) (suspectRows n)
      inPane = columnWith (fillW . gap 8) $ do
        labelWith (fontSizeScale 0.92) "Header above table"
        suspects (maxH (9 * 29 + 6)) 4
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
      pane pid _ = PaneView "P" True <$ if pid == 2 then suspects (grow . tight) 30 else label "other"
      gridCfg = defaultPaneGridConfig {pgLayout = tight . fillW . fillH, pgMinSize = 40, pgSpacing = 4}
      heapPane = columnWith (tight . fillW . fillH) $
        paneGrid gridCfg {pgTree = InitialTree (Just heapTree), pgViewPane = pane}
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
