module Cases.Table
  ( runPageWheelAboveTableTest
  , runTableCellPadTest
  , runTableColResizeDemoReproTest
  , runTableFillWidthTest
  , runTableFirstColWidthTest
  , runTableHBarReachTest
  , runTableReorderTest
  , runTableResizeOverflowTest
  , runTableScrollRevealTest
  , runTableSortTest
  , runTableWrapRowStretchTest
  ) where

import Control.Monad (forM, forM_, replicateM_, void)
import Data.Bits ((.&.))
import Data.IORef (IORef)
import Data.IntMap.Strict qualified as IM
import Data.List (sortBy, sortOn)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import NanoUI
import NanoUI.Context (ctxNodeArena, getScrollOffset2D)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
  , arenaCount
  , getClipRect
  , getDirection
  , getNodeType
  , getParent
  , getRect
  , getScrollContentW
  , getStyleIdx
  , getWidgetId
  )
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertGt, withInput)
import NanoUI.Testing.Harness
  ( clickPos
  , dragPos
  , findHeader
  , requireSpan
  , warmup2
  , withInputOff
  )
import Text.Read (readMaybe)


runTableSortTest :: Context -> IORef Int -> IO ()
runTableSortTest _ failed = do
  let
    columns = headed "Key" fst
    rows = [("b", 1), ("a", 2), ("b", 3), ("a", 4)] :: [(Text, Int)]
  assertEq
    failed
    [2, 4, 1, 3]
    (map snd (sortRows columns (SortCol 0 SortAsc) (V.fromList rows)))
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
    ui = simpleTable ["First", "Second", "Third"] (V.fromList [["a", "b", "c"], ["short"], []])
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
runPageWheelAboveTableTest _ failed = do
  ctx <- newPixelContext
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
  listToMaybe
    [ n
    | (_, t, _, _, _) <-
        sortBy
          ( \(ra, _, _, _, _) (rb, _, _, _, _) ->
              compare (rectY rb) (rectY ra)
          )
          spans
    , "row-" `T.isPrefixOf` t
    , Just n <- [rowLabelIndex t]
    ]

-- Scrolling must materialize the newly revealed row in the same frame the
-- offset lands. The scroll offset is applied after the UI pass, so a frame
-- that only translated the pre-scroll rows left the revealed strip without
-- geometry (stale pixels under the damage clip).
runTableScrollRevealTest :: Context -> IORef Int -> IO ()
runTableScrollRevealTest _ failed = do
  ctx <- newPixelContext
  let inp0 = (withInput 320 220) {inputMousePos = V2 40 80}
      ui = do
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        void
          ( tableWith
              (fixedH 150)
              "people"
              tableScrollCols
              tableScrollRows
              tableSort
          )
  -- Three warmups so virtualization settles on the real viewport height.
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  spans0 <- collectTextSpans ctx
  -- Scroll six wheel lines (120px, several rows): the bottom visible row must
  -- advance because the revealed rows are materialized in the same frame.
  let scrollInp = inp0 {inputScroll = V2 0 6}
  _ <- runFrame ctx scrollInp ui
  spans1 <- collectTextSpans ctx
  case (bottomRowIndex spans0, bottomRowIndex spans1) of
    (Just lo, Just hi) -> do
      assert failed (hi > lo)
      -- The revealed band must be filled with real rows, not one clipped sliver
      -- of a stale row scrolling past the top edge.
      let
        visibleRows = [r | (r, t, _, _, _) <- spans1, "row-" `T.isPrefixOf` t]
      assert failed (length visibleRows >= 3)
    _ -> assert failed False

-- When one cell wraps to several lines the whole row grows; every other
-- cell in the row must stretch to the same height so stripe backgrounds and
-- row borders span the full row instead of leaving a gap.
runTableWrapRowStretchTest :: Context -> IORef Int -> IO ()
runTableWrapRowStretchTest _ failed = do
  ctx <- newPixelContext
  let inp0 = (withInput 700 400) {inputMousePos = V2 (-40) (-40)}
      cfg = defaultTableConfig {tableColSizes = [ColFixed 280, ColFixed 90]}
      wrapCols = headed "Name" fst <> headed "Notes" snd
      rows =
        [ ("row-" <> T.pack (show (i :: Int)), T.unwords (replicate 24 "lorem"))
        | i <- [1 .. 8]
        ]
      ui = do
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        void
          ( tableConfigured
              cfg
              id
              "wrap-stretch"
              wrapCols
              rows
              tableSort
          )
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  let na = ctxNodeArena ctx
  n <- arenaCount na
  cells <-
    fmap
      concat
      ( forM [0 .. n - 1] $ \i -> do
          nt <- getNodeType na i
          if nt == NodeText
            then do
              (_, y, w, h) <- getRect na i
              -- Body cells sit below the header band and have real width
              -- (both columns are wider than 50px; the 90px Notes column
              -- wraps its long text and drives the row height).
              pure [(y, [h]) | y > 25 && w > 50]
            else pure []
      )
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
-- drag frame the header row must stay inside the row scroller's clip (the
-- horizontal bar lane used to cover the header's bottom half for a frame,
-- flickering it mid-drag), and once the drag settles the body scroller's
-- horizontal bar drags the shared offset both ways.
runTableResizeOverflowTest :: Context -> IORef Int -> IO ()
runTableResizeOverflowTest ctx failed = do
  let inp0 = (withInput 400 300) {inputMousePos = V2 30 30}
      -- Five rows put the body just inside the vertical bar's toggle band:
      -- the bar appears exactly when the header lane spacer appears, which is
      -- the sequence that used to clip the header.
      rows = take 5 tableScrollRows
      ui = do
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        void
          ( tableWith
              (fixedH 180)
              "resize-lane"
              tableScrollCols
              rows
              tableSort
          )
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  hdr <- headerButtonRect ctx
  case hdr of
    Nothing -> assert failed False
    Just (Rect hx hy hw hh) -> do
      let edgeX = hx + hw
          headerY = hy + hh / 2
          pressInp = inp0 {inputMousePos = V2 (edgeX - 2) headerY, inputMouseDown = True, inputMousePressed = True}
          dragInp x = inp0 {inputMousePos = V2 x headerY, inputMouseDown = True}
          -- First drag well past the pane's right edge (lane + v-bar appear),
          -- then settle back inside the vertical-bar gutter band so the
          -- scroller viewport and the stale lane flag disagree across frames.
          steps = [edgeX + 160, edgeX + 320, edgeX + 300, edgeX + 290, edgeX + 310, edgeX + 300]
      _ <- runFrame ctx pressInp ui
      forM_ steps $ \x -> do
        _ <- runFrame ctx (dragInp x) ui
        mClip <- headerScrollerClip ctx
        mHdr <- headerButtonRect ctx
        case (mClip, mHdr) of
          (Just (Rect _ cy _ ch), Just (Rect _ hy' _ hh')) -> assert failed (hy' + hh' <= cy + ch + 0.5)
          _ -> assert failed False
      -- Release the resize drag and let the layout settle.
      _ <- runFrame ctx inp0 ui
      _ <- runFrame ctx inp0 ui
      -- The bar lives at the bottom of the body scroller: pressing its track
      -- there jumps the shared horizontal offset, and dragging moves it.
      mBody <- bodyScrollerRect ctx
      case mBody of
        Nothing -> assert failed False
        Just (Rect bx by bw bh) -> do
          let barY = by + bh - scrollBarWidth / 2
              barPress = inp0 {inputMousePos = V2 (bx + bw * 0.3) barY, inputMouseDown = True, inputMousePressed = True}
              barDrag x = inp0 {inputMousePos = V2 x barY, inputMouseDown = True}
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
headerButtonRect ctx = do
  let na = ctxNodeArena ctx
  n <- arenaCount na
  rects <-
    fmap
      catMaybes
      ( forM [0 .. n - 1] $ \i -> do
          nt <- getNodeType na i
          if nt /= NodeButton
            then pure Nothing
            else do
              si <- getStyleIdx na i
              if not (isTableHeaderStyleIdx si)
                then pure Nothing
                else do
                  (x, y, w, h) <- getRect na i
                  pure (Just (Rect x y w h))
      )
  pure (listToMaybe (sortOn rectX rects))

-- | Content clip of the header row scroller (the Row-direction one).
headerScrollerClip :: Context -> IO (Maybe Rect)
headerScrollerClip ctx = do
  let na = ctxNodeArena ctx
  n <- arenaCount na
  found <-
    fmap
      catMaybes
      ( forM [0 .. n - 1] $ \i -> do
          nt <- getNodeType na i
          if nt /= NodeScrollContainer
            then pure Nothing
            else do
              d <- getDirection na i
              if d /= DirRow
                then pure Nothing
                else Just <$> getClipRect na i
      )
  pure $ case found of
    c : _ -> c
    [] -> Nothing

isTableHeaderStyleIdx :: Int -> Bool
isTableHeaderStyleIdx si = si .&. 0x80000000 /= 0

tableScrollCols :: Colonnade Headed TableScrollRow T.Text
tableScrollCols =
  mconcat
    [ headed "Name" tableScrollName
    , headed "Value" tableScrollVal
    ]

data TableScrollRow = TableScrollRow
  { tableScrollName :: T.Text
  , tableScrollVal :: T.Text
  }

tableScrollRows :: [TableScrollRow]
tableScrollRows =
  [ TableScrollRow ("row-" <> T.pack (show (i :: Int))) ("val-" <> T.pack (show i))
  | i <- [1 .. 20]
  ]

-- A content-sized first column fits its longest cell, and in a fit-width 2D
-- table vertical overflow must not shrink it either.
runTableFirstColWidthTest :: Context -> IORef Int -> IO ()
runTableFirstColWidthTest ctx failed = do
  let inp0 = (withInput 400 200) {inputMousePos = V2 60 80}
      cfg =
        defaultTableConfig
          { tableColSizes = [ColContent, ColStretch]
          }
      ui = do
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        void
          ( tableConfigured
              cfg
              id
              "people"
              tableFirstColCols
              tableFirstColRows
              tableSort
          )
  warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  let findLabel needle =
        listToMaybe [(r, t) | (r, t, _, _, _) <- spans, needle `T.isInfixOf` t]
  case (findLabel "long-first-col", findLabel "val-1") of
    (Just (Rect cn _ cw _, _), Just (Rect vx _ _ _, _)) -> do
      assertGt failed cw 50
      assert failed (vx > cn + cw - 2)
    _ -> assert failed False
  pixel <- newPixelContext
  let fitInp = (withInput 280 180) {inputMousePos = V2 40 60}
      fitUi = do
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        void
          ( tableWith
              (fixedH 100 . (\l -> l {layoutWidth = Fit}))
              "people"
              tableFirstColCols
              tableFirstColRows
              tableSort
          )
  warmup2 pixel fitInp fitUi
  fitSpans <- collectTextSpans pixel
  case [r | (r, t, _, _, _) <- fitSpans, "long-first-col" `T.isInfixOf` t] of
    Rect _ _ cw ch : _ -> do
      assertGt failed cw 50
      assert failed (ch < 40)
    [] -> assert failed False

tableFirstColCols :: Colonnade Headed TableFirstColRow T.Text
tableFirstColCols =
  mconcat
    [ headed "Name" tableFirstColName
    , headed "Value" tableFirstColVal
    ]

data TableFirstColRow = TableFirstColRow
  { tableFirstColName :: T.Text
  , tableFirstColVal :: T.Text
  }

tableFirstColRows :: [TableFirstColRow]
tableFirstColRows =
  TableFirstColRow "long-first-col" "short"
    : [ TableFirstColRow ("row-" <> T.pack (show (i :: Int))) ("val-" <> T.pack (show i))
      | i <- [1 .. 8 :: Int]
      ]

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
      ui = do
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        void
          ( tableConfigured
              cfg
              id
              "people"
              tableFillCols
              tableFillRows
              tableSort
          )
  warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  let findLabel needle =
        listToMaybe [(r, t) | (r, t, _, _, _) <- spans, needle `T.isInfixOf` t]
  case (findLabel "Name", findLabel "David", findLabel "Role", findLabel "Manager") of
    ( Just (Rect nx _ _ _, _)
      , Just (Rect cx _ _ _, _)
      , Just (Rect rx _ rw _, _)
      , Just (Rect mx _ mw _, _)
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
      ui = do
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        void (table "people" tableFillCols tableFillRows tableSort)
  warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  let findLabel needle =
        listToMaybe [(r, t) | (r, t, _, _, _) <- spans, needle `T.isInfixOf` t]
  case (findLabel "Name", findLabel "David", findLabel "63", findLabel "Austin") of
    (Just (Rect hx _ _ _, _), Just (Rect nx _ _ _, _), Just (Rect ax _ aw _, _), Just (Rect cx _ _ _, _)) -> do
      assert failed (abs (hx - nx) <= 1)
      assertGt failed nx 4
      assertGt failed (cx - (ax + aw)) 10
    _ -> assert failed False
  -- The slack check needs all five columns in view, which the pixel font's
  -- wider glyphs do not fit in 500px; measure it on a plain host.
  plain <- newContext
  warmup2 plain ((withInput 500 200) {inputMousePos = V2 200 80}) ui
  plainSpans <- collectTextSpans plain
  case [r | (r, t, _, _, _) <- plainSpans, "Role" `T.isInfixOf` t] of
    Rect rx _ rw _ : _ -> assertGt failed (rx + rw) 420
    [] -> assert failed False

tableFillCols :: Colonnade Headed TableFillRow T.Text
tableFillCols =
  mconcat
    [ headed "Name" tableFillName
    , headed "Dept" tableFillDept
    , headed "Age" tableFillAge
    , headed "City" tableFillCity
    , headed "Role" tableFillRole
    ]

data TableFillRow = TableFillRow
  { tableFillName :: T.Text
  , tableFillDept :: T.Text
  , tableFillAge :: T.Text
  , tableFillCity :: T.Text
  , tableFillRole :: T.Text
  }

tableFillRows :: [TableFillRow]
tableFillRows =
  [ TableFillRow "David" "Eng" "63" "Austin" "Staff"
  , TableFillRow "Maya" "Ops" "41" "Tokyo" "Manager"
  , TableFillRow "Chen" "Design" "26" "Shanghai" "IC"
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
                (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
                void
                  ( tableWith
                      (fixedH 280)
                      "people"
                      demoPeopleCols
                      demoPeopleRows
                      tableSort
                  )
    _ <- runFrame ctx inp0 ui
    _ <- runFrame ctx inp0 ui
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
          kind <- uiCursorKind ctx hoverInp
          assertEq failed kind UiCursorEwResize
          let pressInp = hoverInp {inputMouseDown = True, inputMousePressed = True}
              dragInp x = inp0 {inputMousePos = V2 x grabY, inputMouseDown = True}
          before <- headerButtonRects ctx
          _ <- runFrame ctx pressInp ui
          _ <- runFrame ctx (dragInp (edgeX + 60)) ui
          _ <- runFrame ctx (dragInp (edgeX + 60)) ui
          _ <- runFrame ctx (dragInp (edgeX + 60)) ui
          after <- headerButtonRects ctx
          case (drop k before, drop k after) of
            (Rect _ _ wb _ : _, Rect _ _ wa _ : _) -> assertGt failed wa (wb + 30)
            _ -> assert failed False
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
  n <- arenaCount na
  rects <-
    fmap
      catMaybes
      ( forM [0 .. n - 1] $ \i -> do
          nt <- getNodeType na i
          if nt /= NodeButton
            then pure Nothing
            else do
              si <- getStyleIdx na i
              if not (isTableHeaderStyleIdx si)
                then pure Nothing
                else do
                  (x, y, w, h) <- getRect na i
                  pure (Just (Rect x y w h))
      )
  pure (sortOn rectX rects)

-- | Bottom edge of the table pane: from the first header button, walk up to
-- the enclosing panel and return its bottom Y.
tableBodyBottom :: Context -> IO Float
tableBodyBottom ctx = do
  let na = ctxNodeArena ctx
  n <- arenaCount na
  let findBtn i
        | i >= n = pure Nothing
        | otherwise = do
            nt <- getNodeType na i
            if nt /= NodeButton
              then findBtn (i + 1)
              else do
                si <- getStyleIdx na i
                if not (isTableHeaderStyleIdx si)
                  then findBtn (i + 1)
                  else pure (Just i)
      walkUp i
        | i < 0 = pure 0
        | otherwise = do
            nt <- getNodeType na i
            if nt /= NodePanel
              then getParent na i >>= walkUp
              else do
                (_, py, _, ph) <- getRect na i
                pure (py + ph)
  findBtn 0 >>= maybe (pure 0) walkUp

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
  n <- arenaCount na
  finds <-
    fmap
      (concat @[])
      ( forM [0 .. n - 1] $ \i -> do
          hit <- isBodyScroller ctx i
          if not hit
            then pure []
            else do
              (x, y, w, h) <- getRect na i
              pure [Rect x y w h]
      )
  pure (listToMaybe finds)

-- | The body scroller's 2D offset.
bodyOffset :: Context -> IO V2
bodyOffset ctx = do
  let na = ctxNodeArena ctx
  n <- arenaCount na
  found <-
    fmap
      (concat @[])
      ( forM [0 .. n - 1] $ \i -> do
          hit <- isBodyScroller ctx i
          if not hit
            then pure []
            else do
              wid <- getWidgetId na i
              pure [wid]
      )
  case found of
    (wid : _) -> getScrollOffset2D ctx wid
    [] -> pure (V2 0 0)

-- | Horizontal reach: at the end of the horizontal scroll the last column must
-- clear the vertical scrollbar lane, not stop with its right edge under the
-- lane. The body scroller's own vertical bar shrinks the horizontal viewport,
-- so the reachable range must subtract that lane (regression: the range used
-- the full padding box, leaving the last column partly hidden).
runTableHBarReachTest :: Context -> IORef Int -> IO ()
runTableHBarReachTest _ failed = do
  ctx <- newPixelContext
  let inp0 = (withInput 700 320) {inputMousePos = V2 300 160}
      cfg = defaultTableConfig {tableColSizes = [ColFixed 500, ColFixed 500]}
      ui = do
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        void
          ( tableConfigured
              cfg
              (fixedH 200)
              "people"
              tableScrollCols
              tableScrollRows
              tableSort
          )
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  mBody <- bodyScrollerRect ctx
  case mBody of
    Nothing -> assert failed False
    Just (Rect bx by bw bh) -> do
      let na = ctxNodeArena ctx
      n <- arenaCount na
      contentWs <-
        fmap concat $
          forM [0 .. n - 1] $ \i -> do
            hit <- isBodyScroller ctx i
            if hit then (: []) <$> getScrollContentW na i else pure []
      case contentWs of
        [] -> assert failed False
        (contentW : _) -> do
          assertGt failed contentW bw
          let wheel = inp0 {inputMousePos = V2 (bx + bw / 2) (by + bh / 2), inputScroll = V2 50 0}
          replicateM_ 20 (runFrame ctx wheel ui)
          V2 offX _ <- bodyOffset ctx
          -- Reached past the naive content - viewport range: the lane's width
          -- is now part of the reachable range.
          assertGt failed offX (contentW - bw)
          -- The rightmost header cell sits fully inside the body, left of the
          -- vertical lane.
          hdrs <- headerButtonRects ctx
          case reverse hdrs of
            (Rect hx _ hw _ : _) -> do
              assert failed (hx + hw <= bx + bw + 0.5)
              assertGt failed (hx + hw) (bx + bw - 24)
            [] -> assert failed False
          -- Header and body cells scroll in lockstep.
          spans <- collectTextSpans ctx
          let xOf needle = listToMaybe [rectX r | (r, t, _, _, _) <- spans, needle `T.isInfixOf` t]
          case (xOf "Value", xOf "val-") of
            (Just headerX, Just cellX) -> assert failed (abs (headerX - cellX) <= 1)
            _ -> assert failed False
