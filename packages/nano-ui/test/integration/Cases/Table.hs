module Cases.Table (tests) where

import Spec
import Data.Bits ((.&.))
import Data.IntMap.Strict qualified as IM
import Data.List (sortOn, tails)
import Data.Ord (Down (..))
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Primitive.SmallArray qualified as SA
import NanoUI.Internal.Context (ctxNodeArena)
import NanoUI.Internal.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
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
  , getWidgetId
  )
import Text.Read (readMaybe)

tests :: [Spec]
tests =
  [ spec "table-sort" runTableSortTest
  , pixelSpec "table-reorder" runTableReorderTest
  , pixelSpec "table-scroll-reveal" runTableScrollRevealTest
  , pixelSpec "table-shared-scroll-metrics" runTableSharedScrollMetricsTest
  , pixelSpec "page-wheel-above-table" runPageWheelAboveTableTest
  , pixelSpec "table-wrap-row-stretch" runTableWrapRowStretchTest
  , spec "table-first-col" runTableFirstColWidthTest
  , spec "table-fill-width" runTableFillWidthTest
  , pixelSpec "table-cell-pad" runTableCellPadTest
  , spec "table-rules-tile" runTableRulesTileTest
  , pixelSpec "table-resize-overflow" runTableResizeOverflowTest
  , spec "table-col-resize-body" runTableColResizeDemoReproTest
  , pixelSpec "table-hbar-reach" runTableHBarReachTest
  ]

-- | A table sorted ascending on its first column, given everything but the sort.
sortedTable :: (SortCol -> NanoUI TableResponse) -> NanoUI ()
sortedTable build = void (build . fst =<< useTableSort (SortCol 0 SortAsc))

runTableSortTest :: Context -> IORef Int -> IO ()
runTableSortTest _ failed = do
  let
    columns = headed "Key" fst
    rows = [("b", 1), ("a", 2), ("b", 3), ("a", 4)] :: [(Text, Int)]
  assertEq
    failed
    [2, 4, 1, 3]
    (map snd (sortRows columns (SortCol 0 SortAsc) (SA.smallArrayFromList rows)))
  assertEq
    failed
    [1, 3, 2, 4]
    (map snd (sortRows columns (SortCol 0 SortDesc) rows))
  assertEq failed rows (sortRows mempty (SortCol 0 SortAsc) rows)
  assertEq failed rows (sortRows mempty (SortCol 0 SortDesc) rows)

runTableReorderTest :: Context -> IORef Int -> IO ()
runTableReorderTest ctx failed = do
  let
    input = withInputOff 500 240
    ui = simpleTable ["First", "Second", "Third"] (SA.smallArrayFromList [["a", "b", "c"], ["short"], []])
    draw inp = void (runFrame ctx inp ui)
    header name = do
      spans <- collectTextSpans ctx
      requireSpan "missing table header" (findHeader name spans)
  _ <- warmup2 ctx input ui
  first <- header "First"
  third <- header "Third"
  -- A click is not a reorder, even when its absolute x coordinate exceeds
  -- the drag threshold.
  clickPos draw input first
  clicked <- warmup2 ctx input ui
  assertEq failed [0, 1, 2] (tableColOrder clicked)
  dragPos draw input first third
  moved <- warmup2 ctx input ui
  assertEq failed [1, 0, 2] (tableColOrder moved)
  -- The released drag must not stay latched on subsequent frames.
  draw input {inputMousePos = first}
  settled <- warmup2 ctx input ui
  assertEq failed [1, 0, 2] (tableColOrder settled)

-- Row label nearest the bottom edge of the body viewport.
rowLabelIndex :: T.Text -> Maybe Int
rowLabelIndex t = readMaybe (T.unpack (T.takeWhile (/= ' ') (T.drop 4 t)))

-- Wheeling with the mouse parked well above a nested table must scroll the
-- page scroller only. Hit rects that drift by the page's scroll offset made
-- the wheel grab the table's phantom rect and scroll the table instead.
runPageWheelAboveTableTest :: Context -> IORef Int -> IO ()
runPageWheelAboveTableTest ctx failed = do
  let inp0 = withInput 320 220
      wheelAt = inp0 {inputMousePos = V2 160 80, inputScroll = V2 0 5}
      ui = scrollArea (fillW . fixedH 200 . gap 0) $ do
        mapM_ (\i -> label (T.pack ("head " <> show (i :: Int)))) [1 .. 10]
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        tableWith (fixedH 120) "people" tableScrollCols tableScrollRows tableSort
  (pageWid, _) <- warmup2 ctx inp0 ui
  _ <- runFrame ctx wheelAt ui
  _ <- runFrame ctx wheelAt ui
  pageOff <- getScrollOffset ctx pageWid
  assertGt failed pageOff 0
  spans <- collectTextSpans ctx
  let firstRowVisible = any ((== Just 1) . rowLabelIndex . sndOfSpan) spans
  assert failed firstRowVisible
  where
    sndOfSpan (_, t, _, _, _) = t

bottomRowIndex :: [(Rect, T.Text, a, b, c)] -> Maybe Int
bottomRowIndex spans =
  listToMaybe [n | (_, t) <- sortOn (Down . fst) [(rectY r, t) | (r, t, _, _, _) <- spans], "row-" `T.isPrefixOf` t, Just n <- [rowLabelIndex t]]

-- Scrolling must materialize the newly revealed row in the same frame the
-- offset lands. The scroll offset is applied after the UI pass, so a frame
-- that only translated the pre-scroll rows left the revealed strip without
-- geometry (stale pixels under the damage clip).
runTableScrollRevealTest :: Context -> IORef Int -> IO ()
runTableScrollRevealTest ctx failed = do
  let inp0 = (withInput 320 220) {inputMousePos = V2 40 80}
      ui = sortedTable (tableWith (fixedH 150) "people" tableScrollCols tableScrollRows)
  -- A step of one line keeps the scroll below short enough that the rows it
  -- lands on are still in ascending label order (the table sorts the labels
  -- as text, so "row-2" comes after "row-19").
  setScrollTuning ctx defaultScrollTuning {scrollWheelStep = 20}
  -- Three warmups so virtualization settles on the real viewport height.
  _ <- runFrame ctx inp0 ui
  _ <- warmup2 ctx inp0 ui
  spans0 <- collectTextSpans ctx
  -- Scroll six wheel lines (120px, several rows): the bottom visible row must
  -- advance because the revealed rows are materialized in the same frame.
  let scrollInp = inp0 {inputScroll = V2 0 6}
  _ <- runFrame ctx scrollInp ui
  spans1 <- collectTextSpans ctx
  assertJust failed ((,) <$> bottomRowIndex spans0 <*> bottomRowIndex spans1) $ \(lo, hi) -> do
      assert failed (hi > lo)
      -- The revealed band must be filled with real rows, not one clipped sliver
      -- of a stale row scrolling past the top edge.
      let
        visibleRows = [r | (r, t, _, _, _) <- spans1, "row-" `T.isPrefixOf` t]
      assert failed (length visibleRows >= 3)

-- When one cell wraps to several lines the whole row grows; every other
-- cell in the row must stretch to the same height so stripe backgrounds and
-- row borders span the full row instead of leaving a gap.
runTableWrapRowStretchTest :: Context -> IORef Int -> IO ()
runTableWrapRowStretchTest ctx failed = do
  let inp0 = (withInput 700 400) {inputMousePos = V2 (-40) (-40)}
      cfg = defaultTableConfig {tableColSizes = [ColFixed 280, ColFixed 90]}
      wrapCols = headed "Name" fst <> headed "Notes" snd
      rows =
        [ ("row-" <> T.pack (show (i :: Int)), T.unwords (replicate 24 "lorem"))
        | i <- [1 .. 8]
        ]
      ui = sortedTable (tableConfigured cfg id "wrap-stretch" wrapCols rows)
  _ <- warmup2 ctx inp0 ui
  let na = ctxNodeArena ctx
  cells <- foldNodesM na (\acc i -> do
    nt <- getNodeType na i
    if nt /= NodeText
      then pure acc
      else do
        Rect _ y w h <- getNodeRect na i
        -- Body cells sit below the header band and have real width (both
        -- columns are wider than 50px; the 90px Notes column wraps its long
        -- text and drives the row height).
        pure (if y > 25 && w > 50 then (y, [h]) : acc else acc)) []
  -- Cells of one row share the same top y; every row group must be uniform
  -- (all cells stretch to the row height).
  let rowBands = IM.toAscList (IM.fromListWith (++) [(round y, hs) | (y, hs) <- cells])
  forM_ rowBands $ \(_, hs) -> assert failed (length (dedup hs) == 1)
  -- And at least one row is actually wrapped (taller than the 28px minimum),
  -- otherwise the test asserts nothing.
  assert failed (any (\hs -> maximum hs > 40) (map snd rowBands))
  -- The body must start right below the header: an unconditional scrollbar
  -- lane reserve (shown even with no horizontal overflow) opens a dead gap.
  assert failed (minimum (map fst rowBands) < 40)
  where
    dedup = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- Resizing a column past the pane's right edge overflows the body. On every
-- drag frame the header row must stay inside the row scroller's clip, with
-- no frame where the horizontal bar lane covers the header's bottom half,
-- and once the drag settles the body scroller's
-- horizontal bar drags the shared offset both ways.
runTableResizeOverflowTest :: Context -> IORef Int -> IO ()
runTableResizeOverflowTest ctx failed = do
  let inp0 = (withInput 400 300) {inputMousePos = V2 30 30}
      -- Five rows put the body just inside the vertical bar's toggle band:
      -- the bar appears exactly when the header lane spacer appears, which is
      -- the sequence that could clip the header.
      rows = take 5 tableScrollRows
      ui = sortedTable (tableWith (fixedH 180) "resize-lane" tableScrollCols rows)
  _ <- warmup2 ctx inp0 ui
  assertJustM failed (headerButtonRect ctx) $ \(Rect hx hy hw hh) -> do
    let edgeX = hx + hw
        headerY = hy + hh / 2
        pressInp = pressAt inp0 (V2 (edgeX - 2) headerY)
        dragInp x = inp0 {inputMousePos = V2 x headerY, inputButtonsHeld = buttonsFromList [MouseLeft]}
        -- First drag well past the pane's right edge (lane + v-bar appear),
        -- then settle back inside the vertical-bar gutter band so the
        -- scroller viewport and the stale lane flag disagree across frames.
        steps = [edgeX + 160, edgeX + 320, edgeX + 300, edgeX + 290, edgeX + 310, edgeX + 300]
    _ <- runFrame ctx pressInp ui
    forM_ steps $ \x -> do
      _ <- runFrame ctx (dragInp x) ui
      mClip <- headerScrollerClip ctx
      mHdr <- headerButtonRect ctx
      assertJust failed ((,) <$> mClip <*> mHdr) $ \(Rect _ cy _ ch, Rect _ hy' _ hh') -> assert failed (hy' + hh' <= cy + ch + 0.5)
    -- Release the resize drag and let the layout settle.
    _ <- warmup2 ctx inp0 ui
    -- The bar lives at the bottom of the body scroller: pressing its track
    -- there jumps the shared horizontal offset, and dragging moves it.
    assertJustM failed (bodyScrollerRect ctx) $ \(Rect bx by bw bh) -> do
      let barY = by + bh - scrollBarWidth / 2
          barPress = pressAt inp0 (V2 (bx + bw * 0.3) barY)
          barDrag x = inp0 {inputMousePos = V2 x barY, inputButtonsHeld = buttonsFromList [MouseLeft]}
      _ <- runFrame ctx barPress ui
      V2 off1 _ <- bodyOffset ctx
      _ <- runFrame ctx (barDrag (bx + bw * 0.95)) ui
      V2 off2 _ <- bodyOffset ctx
      assertGt failed off2 off1
      _ <- runFrame ctx (barDrag (bx + bw * 0.2)) ui
      V2 off3 _ <- bodyOffset ctx
      assert failed (off3 < off2)

-- | Leftmost table-header button rect.
headerButtonRect :: Context -> IO (Maybe Rect)
headerButtonRect ctx = listToMaybe <$> headerButtonRects ctx

-- | Content clip of the header row scroller (the Row-direction one).
headerScrollerClip :: Context -> IO (Maybe Rect)
headerScrollerClip ctx = do
  let na = ctxNodeArena ctx
  found <- findNodeM na $ \i -> do
    nt <- getNodeType na i
    if nt == NodeScrollContainer then (== DirRow) <$> getDirection na i else pure False
  maybe (pure Nothing) (getClipRect na) found

isTableHeaderStyleIdx :: Int -> Bool
isTableHeaderStyleIdx si = si .&. 0x80000000 /= 0

-- | Name/value rows: @row-i@, @val-i@.
tableScrollCols :: Colonnade Headed (T.Text, T.Text) T.Text
tableScrollCols = headed "Name" fst <> headed "Value" snd

tableScrollRows :: [(T.Text, T.Text)]
tableScrollRows = [("row-" <> n, "val-" <> n) | i <- [1 .. 20 :: Int], let n = T.pack (show i)]

-- A content-sized first column fits its longest cell, and in a fit-width 2D
-- table vertical overflow must not shrink it either.
runTableFirstColWidthTest :: Context -> IORef Int -> IO ()
runTableFirstColWidthTest ctx failed = do
  let inp0 = (withInput 400 200) {inputMousePos = V2 60 80}
      cfg =
        defaultTableConfig
          { tableColSizes = [ColContent, ColStretch]
          }
      ui = sortedTable (tableConfigured cfg id "people" tableScrollCols tableFirstColRows)
  warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  let findLabel needle = spanRect needle spans
  assertJust failed ((,) <$> findLabel "long-first-col" <*> findLabel "val-1") $ \(Rect cn _ cw _, Rect vx _ _ _) -> do
      assertGt failed cw 50
      assert failed (vx > cn + cw - 2)
  pixel <- newPixelContext
  let fitInp = (withInput 280 180) {inputMousePos = V2 40 60}
      fitUi = sortedTable (tableWith (fixedH 100 . (\l -> l {layoutWidth = Fit})) "people" tableScrollCols tableFirstColRows)
  warmup2 pixel fitInp fitUi
  fitSpans <- collectTextSpans pixel
  assertJust failed (spanRect "long-first-col" fitSpans) $ \(Rect _ _ cw ch) -> do
    assertGt failed cw 50
    assert failed (ch < 40)

tableFirstColRows :: [(T.Text, T.Text)]
tableFirstColRows = ("long-first-col", "short") : take 8 tableScrollRows

runTableFillWidthTest :: Context -> IORef Int -> IO ()
runTableFillWidthTest _ failed = do
  ctx <- newContext
  let inp0 = (withInput 500 200) {inputMousePos = V2 200 80}
      cfg =
        defaultTableConfig
          { tableColSizes =
              [ ColContent
              , ColStretch
              , ColFixed 64
              , ColStretch
              , ColContent
              ]
          }
      ui = sortedTable (tableConfigured cfg id "people" demoPeopleCols tableFillRows)
  warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  let findLabel needle = spanRect needle spans
  case (findLabel "Name", findLabel "David", findLabel "Role", findLabel "Manager") of
    ( Just (Rect nx _ _ _)
      , Just (Rect cx _ _ _)
      , Just (Rect rx _ rw _)
      , Just (Rect mx _ mw _)
      ) -> do
      assert failed (abs (nx - cx) <= 1)
      assertGt failed (rx + rw) 380
      assertGt failed mw 50
      assert failed (mx >= rx - 2)
    _ -> assert failed False

-- Pixel host: Age (right) and City (left) must not sit on the shared grid
-- line. A plain table also hands its slack to the columns, so the last one
-- reaches the far side.
runTableCellPadTest :: Context -> IORef Int -> IO ()
runTableCellPadTest ctx failed = do
  let inp0 = (withInput 500 240) {inputMousePos = V2 200 80}
      ui = sortedTable (table "people" demoPeopleCols tableFillRows)
  warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  let findLabel needle = spanRect needle spans
  case (findLabel "Name", findLabel "David", findLabel "63", findLabel "Austin") of
    (Just (Rect hx _ _ _), Just (Rect nx _ _ _), Just (Rect ax _ aw _), Just (Rect cx _ _ _)) -> do
      assert failed (abs (hx - nx) <= 1)
      assertGt failed nx 4
      assertGt failed (cx - (ax + aw)) 10
    _ -> assert failed False
  -- The slack check needs all five columns in view, which the pixel font's
  -- wider glyphs do not fit in 500px; measure it on a plain host.
  plain <- newContext
  warmup2 plain ((withInput 500 200) {inputMousePos = V2 200 80}) ui
  plainSpans <- collectTextSpans plain
  assertJust failed (spanRect "Role" plainSpans) $ \(Rect rx _ rw _) -> assertGt failed (rx + rw) 420

tableFillRows :: [(T.Text, T.Text, T.Text, T.Text, T.Text)]
tableFillRows =
  [ ("David", "Eng", "63", "Austin", "Staff")
  , ("Maya", "Ops", "41", "Tokyo", "Manager")
  , ("Chen", "Design", "26", "Shanghai", "IC")
  ]

-- The demo's page structure (page scroller, card panel, five columns). Every
-- column boundary must raise the resize cursor and resize when grabbed, both
-- on the header cell and down in the column body.
runTableColResizeDemoReproTest :: Context -> IORef Int -> IO ()
runTableColResizeDemoReproTest _ failed =
  forM_ [False, True] $ \inBody -> do
    ctx <- newPixelContext
    let inp0 = (withInput 700 500) {inputMousePos = V2 400 100}
        ui =
          scrollWith (tight . grow) $
            columnWith (padAll 6 . gap 6 . fillW) $
              card $ do
                sortedTable (tableWith (fixedH 280) "people" demoPeopleCols demoPeopleRows)
    _ <- warmup2 ctx inp0 ui
    bodyBot <- tableBodyBottom ctx
    hdrs0 <- headerButtonRects ctx
    forM_ (zip [0 ..] hdrs0) $ \(k, _) -> do
      hdrs <- headerButtonRects ctx
      case drop k hdrs of
        Rect hx hy hw hh : _ | bodyBot > hy + hh + 20 -> do
          let edgeX = hx + hw - 2
              grabY = if inBody then (hy + hh + bodyBot) / 2 else hy + hh / 2
              hoverInp = inp0 {inputMousePos = V2 edgeX grabY}
          _ <- runFrame ctx hoverInp ui
          assertEq failed UiCursorEwResize =<< uiCursorKind ctx hoverInp
          -- Away from the edge, the header is not a resize zone.
          midKind <- uiCursorKind ctx inp0 {inputMousePos = V2 (hx + hw / 2) grabY}
          assert failed (midKind /= UiCursorEwResize)
          let pressInp = applyMouseButton MouseLeft True hoverInp
              dragInp x = inp0 {inputMousePos = V2 x grabY, inputButtonsHeld = buttonsFromList [MouseLeft]}
          before <- headerButtonRects ctx
          _ <- runFrame ctx pressInp ui
          _ <- runFrame ctx (dragInp (edgeX + 60)) ui
          _ <- runFrame ctx (dragInp (edgeX + 60)) ui
          _ <- runFrame ctx (dragInp (edgeX + 60)) ui
          after <- headerButtonRects ctx
          case (drop k before, drop k after) of
            (Rect _ _ wb _ : _, Rect _ _ wa _ : _) -> assertGt failed wa (wb + 30)
            _ -> assert failed False
          -- The arrow stays for the whole drag, off the edge too, and goes
          -- once the button is let go.
          let offInp = inp0 {inputMousePos = V2 (edgeX + 60) 490, inputButtonsHeld = buttonsFromList [MouseLeft]}
          assertEq failed UiCursorEwResize =<< uiCursorKind ctx offInp
          _ <- runFrame ctx (applyMouseButton MouseLeft False offInp) ui
          released <- uiCursorKind ctx offInp
          assert failed (released /= UiCursorEwResize)
        _ -> assert failed False

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

-- All table-header button rects, left to right.
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
  if nt == NodeButton then isTableHeaderStyleIdx <$> getStyleIdx na i else pure False

-- | Bottom edge of the table pane: from the first header button, walk up to
-- the enclosing panel and return its bottom Y.
tableBodyBottom :: Context -> IO Float
tableBodyBottom ctx = do
  let na = ctxNodeArena ctx
  let walkUp i
        | i < 0 = pure 0
        | otherwise = do
            nt <- getNodeType na i
            if nt /= NodePanel
              then getParent na i >>= walkUp
              else do
                Rect _ py _ ph <- getNodeRect na i
                pure (py + ph)
  findNodeM na (isHeaderButton na) >>= maybe (pure 0) walkUp

-- | The body (unfrozen) v-scroller: a Column-direction 2D scroller with
-- style bits "both policies Auto, clamp set" (shared predicate for the
-- rect / h-bar / offset helpers below).
isBodyScroller :: Context -> NodeIdx -> IO Bool
isBodyScroller ctx i = do
  let na = ctxNodeArena ctx
  nt <- getNodeType na i
  if nt /= NodeScrollContainer
    then pure False
    else do
      d <- getDirection na i
      if d /= DirColumn
        then pure False
        else do
          si <- getStyleIdx na i
          pure (si .&. 3 == 0 && (si `div` 4) .&. 3 == 0 && si .&. 16 /= 0)

bodyScrollerRect :: Context -> IO (Maybe Rect)
bodyScrollerRect ctx = do
  let na = ctxNodeArena ctx
  traverse (getNodeRect na) =<< findNodeM na (isBodyScroller ctx)

-- | The body scroller's 2D offset.
bodyOffset :: Context -> IO V2
bodyOffset ctx = do
  let na = ctxNodeArena ctx
  found <- findNodeM na (isBodyScroller ctx)
  maybe (pure (V2 0 0)) (getScrollOffset2D ctx <=< getWidgetId na) found

-- | Horizontal reach: at the end of the horizontal scroll the last column must
-- clear the vertical scrollbar lane, not stop with its right edge under the
-- lane. The body scroller's own vertical bar shrinks the horizontal viewport,
-- so the reachable range must account for the lane's width.
runTableHBarReachTest :: Context -> IORef Int -> IO ()
runTableHBarReachTest ctx failed = do
  let inp0 = (withInput 700 320) {inputMousePos = V2 300 160}
      cfg = defaultTableConfig {tableColSizes = [ColFixed 500, ColFixed 500]}
      ui = sortedTable (tableConfigured cfg (fixedH 200) "people" tableScrollCols tableScrollRows)
  _ <- warmup2 ctx inp0 ui
  assertJustM failed (bodyScrollerRect ctx) $ \(Rect bx by bw bh) -> do
    let na = ctxNodeArena ctx
    assertJustM failed (findNodeM na (isBodyScroller ctx)) $ \i -> do
      contentW <- getScrollContentW na i
      assertGt failed contentW bw
      let wheel = inp0 {inputMousePos = spanCenter (Rect bx by bw bh), inputScroll = V2 50 0}
      replicateM_ 20 (runFrame ctx wheel ui)
      V2 offX _ <- bodyOffset ctx
      -- Reached past the naive content - viewport range: the lane's width
      -- is now part of the reachable range.
      assertGt failed offX (contentW - bw)
      -- The rightmost header cell sits fully inside the body, left of the
      -- vertical lane.
      hdrs <- headerButtonRects ctx
      assertJust failed (listToMaybe (reverse hdrs)) $ \(Rect hx _ hw _) -> do
        assert failed (hx + hw <= bx + bw + 0.5)
        assertGt failed (hx + hw) (bx + bw - 24)
      -- Header and body cells scroll in lockstep.
      spans <- collectTextSpans ctx
      let xOf needle = listToMaybe [rectX r | (r, t, _, _, _) <- spans, needle `T.isInfixOf` t]
      assertJust failed ((,) <$> xOf "Value" <*> xOf "val-") $ \(headerX, cellX) -> assert failed (abs (headerX - cellX) <= 1)

-- A table with frozen columns builds two scroll nodes under one widget id.
-- Only one of them may publish the body's geometry: if both did, every frame
-- would rewrite the store with the other pane's viewport twice a frame, for
-- as long as the table is on screen. One publishes; what it publishes is the
-- body scroller, whole, and it holds still from frame to frame.
runTableSharedScrollMetricsTest :: Context -> IORef Int -> IO ()
runTableSharedScrollMetricsTest ctx failed = do
  let inp0 = (withInput 320 220) {inputMousePos = V2 40 80}
      cfg = defaultTableConfig {tableFreezeCols = 1}
      ui = sortedTable (tableConfigured cfg (fixedH 150) "people" tableScrollCols tableScrollRows)
  replicateM_ 3 (runFrame ctx inp0 ui)
  assertJustM failed (tableBodyScrollWid ctx) $ \wid -> do
    before <- getScrollMetrics ctx wid
    assert failed (isJust before)
    -- The pane that owns both scrollbars, not the frozen column's sliver.
    assertEq failed (Just ScrollAxisXY) (fmap scrollAxes before)
    _ <- runFrame ctx inp0 ui
    assertEq failed before =<< getScrollMetrics ctx wid

-- The widget id shared by the table body's panes: the id of the first scroll
-- container the arena holds that another scroll container repeats.
tableBodyScrollWid :: Context -> IO (Maybe WidgetId)
tableBodyScrollWid ctx = do
  let na = ctxNodeArena ctx
  wids <- reverse <$> foldNodesM na (\acc i -> do
    nt <- getNodeType na i
    if nt == NodeScrollContainer then (: acc) <$> getWidgetId na i else pure acc) []
  pure (listToMaybe [w | w : rest <- tails wids, w `elem` rest])


-- Every row of cells and column rules must tile: each child's far edge is the
-- next one's origin. A cell that reaches a pixel past it paints over the rule
-- there, and the column's grid line vanishes down the whole table. Covers
-- fractional scales and fractional fixed widths, whose sizes do not round to
-- whole device pixels.
runTableRulesTileTest :: Context -> IORef Int -> IO ()
runTableRulesTileTest _ failed =
  forM_ [1, 1.25, 1.5, 2] $ \scale ->
    forM_ [ColStretch, ColFixed 97.3] $ \middle ->
      forM_ [470, 473 .. 530] $ \winW -> do
        base <- newContext
        let ctx = withFontMetrics base ((monospaceMetrics 12) {fmSnapScale = scale})
            cfg = defaultTableConfig {tableColSizes = [ColContent, ColFixed 61.7, middle, ColContent, ColContent]}
            ui = sortedTable (tableConfigured cfg id "people" demoPeopleCols tableFillRows)
        warmup2 ctx (withInputOff winW 240) ui
        let na = ctxNodeArena ctx
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
              overlaps = [(x0, w0, x1) | ((x0, w0), (x1, _)) <- zip edges (drop 1 edges), abs (x0 + w0 - x1) > 1.0e-3]
          assertEq failed [] overlaps
