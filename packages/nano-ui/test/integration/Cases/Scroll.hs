module Cases.Scroll
  ( runGrowScrollGutterTest
  , runNestedScrollFocusTest
  , runNestedScrollTest
  , runPanelGrowScrollGutterTest
  , runScrollBarGutterTest
  , runScrollButtonClickSdlTest
  , runScrollButtonClickTest
  , runScrollDamageTest
  , runScrollHitOffsetTest
  , runScrollHoverClipTest
  , runScrollTest
  , runScrollThumbCursorTest
  , runScrollTopClipTest
  , runTableScrollTest
  , runTableScrollRevealTest
  , runPageWheelAboveTableTest
  , runTableWrapRowStretchTest
  , runTableFirstColWidthTest
  , runTableFillWidthTest
  , runTableContentSlackTest
  , runTableCellPadTest
  , runTableFitScrollColWidthTest
  , runTableTabWrapRowTest
  , runTableResizeHeaderLaneTest
  , run2DPadFillOverflowTest
  , run2DPadOverflowScrollsTest
  , runTableColResizeCursorTest
  , runTableColResizeDemoReproTest
  , runTableHBarStableTest
  , runScrolledOutClickImmunityTest
  , runScrolledOutHoverImmunityTest
  , runScrolledOutCursorImmunityTest
  , runScrollChildDamageOffsetTest
  , run2DScrollWheelTest
  , runTable2DScrollSyncTest
  , runScrollLockstepProbeTest
  , runPageScrollBackdropCoverageTest
  , runTableHBarReachTest
  ) where

import Control.Monad (forM, forM_, replicateM, replicateM_, unless, void)
import Data.Bits ((.&.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.List (sort, sortBy, sortOn)
import Data.Maybe (catMaybes, listToMaybe)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peekElemOff)
import Data.Text qualified as T
import Text.Read (readMaybe)
import NanoUI
import NanoUI.Context (ctxNodeArena, getScrollOffset2D, setDrawSnapScale)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
  , arenaCount
  , getClipRect
  , getDirection
  , getNodeValue
  , getNodeType
  , getParent
  , getRect
  , getScrollContentW
  , getStyleIdx
  , getWidgetId
  )
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertGt, bump, withInput)
import NanoUI.Testing.Harness
  ( assertScrollGutter
  , assertScrollGutterPad
  , findGrabHover
  , runClickPair
  , spanXOf
  , spanYOf
  , spanLabelYs
  , warmup2
  , withInputOff
  )

runScrollThumbCursorTest :: Context -> IORef Int -> IO ()
runScrollThumbCursorTest ctx failed = do
  let inp0 = withInput 200 120
      ui = scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 80})
             (column (replicateM 8 (label "scroll line") >> pure ()))
  ((sid, ()), _, _, _) <- runFrame ctx inp0 ui >>= \_ -> runFrame ctx inp0 ui
  mrect <- getPrevRect ctx sid
  case mrect of
    Nothing -> assert failed False
    Just (Rect rx ry rw rh) -> do
      let barW = scrollBarWidth
          thumbX = rx + rw - scrollBarListExtra - barW / 2
          tryYs = [ry + rh * n / 8 | n <- [1 .. 7]]
      mHover <- findGrabHover ctx ui inp0 thumbX tryYs
      case mHover of
        Nothing -> assert failed False
        Just hover -> do
          kind <- uiCursorKind ctx hover
          assertEq failed kind UiCursorGrab
          let press = hover {inputMouseDown = True, inputMousePressed = True}
          _ <- runFrame ctx press ui
          grabbing <- cursorKindIs ctx press UiCursorGrabbing
          assert failed grabbing

runScrollBarGutterTest :: Context -> IORef Int -> IO ()
runScrollBarGutterTest ctx failed = do
  let inp0 = withInput 200 120
      ui = scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 60}) $ do
             r <- labelEx (fillW defaultLayout) "Wide"
             _ <- replicateM 8 (label "scroll line")
             pure r
  (sid, child) <- warmup2 ctx inp0 ui
  let endPad = padR (layoutPadding defaultLayout)
      gutter = scrollBarGutter (ctxHostProfile ctx) (ctxFontMetrics ctx) + scrollBarListExtra
  assertScrollGutterPad failed ctx sid child gutter endPad

runGrowScrollGutterTest :: Context -> IORef Int -> IO ()
runGrowScrollGutterTest ctx failed = do
  let inp0 = withInput 240 140
      ui = scrollArea (tight (grow defaultLayout)) $ do
             r <- labelEx (fillW defaultLayout) "Wide"
             _ <- replicateM 20 (label "scroll line")
             pure r
  (sid, child) <- warmup2 ctx inp0 ui
  let gutter = scrollBarGutter (ctxHostProfile ctx) (ctxFontMetrics ctx) + scrollBarPageExtra
  assertScrollGutter failed ctx sid child gutter

runPanelGrowScrollGutterTest :: Context -> IORef Int -> IO ()
runPanelGrowScrollGutterTest ctx failed = do
  let inp0 = withInput 240 140
      ui = panelWith grow $
             scrollArea (tight (grow defaultLayout)) $ do
                r <- labelEx (fillW defaultLayout) "Wide"
                _ <- replicateM 20 (label "scroll line")
                pure r
  (sid, child) <- warmup2 ctx inp0 ui
  let gutter = scrollBarGutter (ctxHostProfile ctx) (ctxFontMetrics ctx) + scrollBarListExtra
  assertScrollGutter failed ctx sid child gutter

runScrollDamageTest :: Context -> IORef Int -> IO ()
runScrollDamageTest _ failed = do
  ctx <- newContext
  let scrollUi =
        fmap fst $
          scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 60}) $
            column (replicateM 8 (label "scroll line") >> pure ())
      inp0 = withInputOff 200 120
  sid <- warmup2 ctx inp0 scrollUi
  let bumpUi = scrollUi >> uiIO (setScrollOffset ctx sid 24)
  _ <- runFrame ctx inp0 bumpUi
  dScroll <- takeDamage ctx
  case dScroll of
    DamageFull -> assert failed False
    DamageClip r -> assert failed (rectW r > 0 && rectH r > 0 && rectH r <= 60 + defaultDamageSlop * 2 && not (damageIsEmpty dScroll))

-- Ghosting guard: a grow×grow (page-level) scroll container paints no well,
-- so on clip frames the strip vacated by scrolled content has no covering
-- command and the retained texture would show stale pixels — a ghost of a
-- previous scroll position. Every frame must emit a full-viewport fill (the
-- window-color backdrop) so clip replay repaints the whole viewport.
runPageScrollBackdropCoverageTest :: Context -> IORef Int -> IO ()
runPageScrollBackdropCoverageTest _ failed = do
  ctx <- newContext
  let inp0 = withInputOff 300 220
      ui = fmap fst $
        scrollArea
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1})
          (column (replicateM 20 (label "scroll backdrop line") >> pure ()))
  sid <- warmup2 ctx inp0 ui
  setScrollOffset ctx sid 120
  _ <- runFrame ctx inp0 ui
  (_, _, draw, _) <- runFrame ctx inp0 ui
  mRect <- getPrevRect ctx sid
  case mRect of
    Nothing -> pure ()
    Just (Rect rx ry rw rh) -> do
      quads <- decodeQuads draw
      let covered =
            any
              (\(qx1, qy1, qx2, qy2, _, _) ->
                abs (qx1 - rx) <= 0.6
                  && abs (qy1 - ry) <= 0.6
                  && abs (qx2 - (rx + rw)) <= 0.6
                  && abs (qy2 - (ry + rh)) <= 0.6)
              quads
      assert failed covered

runTableScrollTest :: Context -> IORef Int -> IO ()
runTableScrollTest _ failed = do
  ctx <- newContext
  let inp0 = (withInput 320 120) {inputMousePos = V2 40 70}
      ui = do
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        void (table "people" tableScrollCols tableScrollRows tableSort)
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  spans0 <- collectTextSpans ctx
  let findLabel needle =
        listToMaybe [(r, t, fg, bg, c) | (r, t, fg, bg, c) <- spans0, needle `T.isInfixOf` t]
  case (findLabel "Name", findLabel "row-1", findLabel "val-1") of
    (Just (Rect nx _ _ _, _, _, _, _), Just (Rect cn _ _ _, _, _, _, _), Just (Rect cvx _ _ _, _, _, _, _)) -> do
      assert failed (abs (nx - cn) <= 1)
      assert failed (cvx > cn)
    _ -> assert failed False
  let scrollInp = inp0 {inputScroll = V2 0 1}
  _ <- runFrame ctx scrollInp ui
  spans1 <- collectTextSpans ctx
  assert failed (length spans1 >= length spans0 `div` 2)

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
      ui = scrollArea (fillW . fixedH 200 $ defaultLayout {layoutGap = 0}) $ do
        mapM_ (\i -> label (T.pack ("head " <> show (i :: Int)))) [1 .. 10]
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        tableCfg
          defaultTableCfg
          (tight . fillW . fixedH 120 $ defaultLayout {layoutGap = 0})
          "people"
          tableScrollCols
          tableScrollRows
          tableSort
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
          ( tableCfg
              defaultTableCfg
              (tight . fillW . fixedH 150 $ defaultLayout {layoutGap = 0})
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
      cfg = defaultTableCfg {tableColSizes = [ColFixed 280, ColFixed 90]}
      wrapCols = headed "Name" fst <> headed "Notes" snd
      rows =
        [ ("row-" <> T.pack (show (i :: Int)), T.unwords (replicate 24 "lorem"))
        | i <- [1 .. 8]
        ]
      ui = do
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        void
          ( tableCfg
              cfg
              (tight . fillW $ defaultLayout {layoutGap = 0})
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

-- Resizing a column across the pane's right edge must never let the
-- horizontal scrollbar lane cover the bottom of the header row. The lane
-- spacer and the scroller policy come from the same previous-frame overflow
-- flag, so the scroller's clip can't activate the lane on a frame where the
-- spacer hasn't landed (that used to cover the header's bottom half under the
-- bar track for a frame, flickering it mid-drag). Asserts, on every drag
-- frame, that the header row stays inside the row scroller's clip and that
-- the scroller reserves the lane exactly when the spacer is present.
runTableResizeHeaderLaneTest :: Context -> IORef Int -> IO ()
runTableResizeHeaderLaneTest _ failed = do
  ctx <- newPixelContext
  let inp0 = (withInput 400 300) {inputMousePos = V2 30 30}
      -- Five rows put the body just inside the vertical bar's toggle band:
      -- the bar appears exactly when the header lane spacer appears, which is
      -- the sequence that used to clip the header.
      rows = take 5 tableScrollRows
      ui = do
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        void
          ( tableCfg
              defaultTableCfg
              (tight . fillW . fixedH 180 $ defaultLayout {layoutGap = 0})
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
        (mClip, hasLaneSpacer, reserved) <- laneState ctx
        mHdr <- headerButtonRect ctx
        case (mClip, mHdr) of
          (Just clip, Just (Rect _ hy' _ hh')) -> do
            let Rect cy _ _ ch = clip
            -- The whole header row (all cells share its band) must stay
            -- inside the scroller's content clip on every drag frame.
            assert failed (hy' + hh' <= cy + ch + 0.5)
            -- And the lane reservation must track the spacer: while the
            -- spacer is in the content the policy reserves the lane, and
            -- while it is absent the policy must not (a live ScrollAuto
            -- gutter is what used to clip the header for a frame).
            assert failed (reserved == hasLaneSpacer)
          _ -> assert failed False

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

-- | Row scroller state: its content clip, whether the lane spacer is in the
-- content (the scroller is taller than the header line), and whether its
-- policy reserves the lane.
laneState :: Context -> IO (Maybe Rect, Bool, Bool)
laneState ctx = do
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
                else do
                  mC <- getClipRect na i
                  (_, _, _, sh) <- getRect na i
                  si <- getStyleIdx na i
                  -- Lane reserved: policyX = ScrollAlways (bits 0-1 = 1).
                  let reserved = si .&. 3 == 1
                  pure (Just (mC, sh > 35, reserved))
      )
  pure $ case found of
    (c, hasLaneSpacer, reserved) : _ -> (c, hasLaneSpacer, reserved)
    [] -> (Nothing, False, False)

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

runTableFirstColWidthTest :: Context -> IORef Int -> IO ()
runTableFirstColWidthTest _ failed = do
  ctx <- newContext
  let inp0 = (withInput 400 200) {inputMousePos = V2 60 80}
      cfg =
        defaultTableCfg
          { tableColSizes = [ColContent, ColStretch]
          }
      ui = do
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        void
          ( tableCfg
              cfg
              (tight . fillW $ defaultLayout {layoutGap = 0})
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

-- Fit-width 2D table: vertical overflow must not shrink the first column.
runTableFitScrollColWidthTest :: Context -> IORef Int -> IO ()
runTableFitScrollColWidthTest _ failed = do
  ctx <- newPixelContext
  let inp0 = (withInput 280 180) {inputMousePos = V2 40 60}
      ui = do
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        void
          ( tableCfg
              defaultTableCfg
              (tight . fixedH 100 $ defaultLayout {layoutGap = 0})
              "people"
              tableFirstColCols
              tableFirstColRows
              tableSort
          )
  warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  let findLabel needle =
        listToMaybe [(r, t) | (r, t, _, _, _) <- spans, needle `T.isInfixOf` t]
  case findLabel "long-first-col" of
    Just (Rect _ _ cw ch, _) -> do
      assertGt failed cw 50
      assert failed (ch < 40)
    _ -> assert failed False

-- Long Table-tab help must not inflate a wrap row into a stacked full-width card.
runTableTabWrapRowTest :: Context -> IORef Int -> IO ()
runTableTabWrapRowTest ctx failed = do
  let inp0 = withInput 1200 800
      ui = rowWith (tight . gap 8 . fillW) $ do
        card (void (label "State"))
        card $ do
          heading "Table"
          muted "Click a header to sort. Drag a header to reorder."
          muted "Drag a header edge to resize. Right-click a header to hide."
          (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
          void
            ( tableCfg
                defaultTableCfg
                (tight . fixedH 100 $ defaultLayout {layoutGap = 0})
                "people"
                tableFillCols
                tableFillRows
                tableSort
            )
  warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  case (spanXOf "State" spans, spanXOf "Table" spans, spanYOf "State" spans, spanYOf "Table" spans) of
    ([sx], [tx], [sy], [ty]) -> do
      assertGt failed tx (sx + 1)
      assert failed (abs (ty - sy) < 8)
    _ -> assert failed False

runTableFillWidthTest :: Context -> IORef Int -> IO ()
runTableFillWidthTest _ failed = do
  ctx <- newContext
  let inp0 = (withInput 500 200) {inputMousePos = V2 200 80}
      cfg =
        defaultTableCfg
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
          ( tableCfg
              cfg
              (tight . fillW $ defaultLayout {layoutGap = 0})
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

runTableContentSlackTest :: Context -> IORef Int -> IO ()
runTableContentSlackTest _ failed = do
  ctx <- newContext
  let inp0 = (withInput 500 200) {inputMousePos = V2 200 80}
      ui = do
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        void (table "people" tableFillCols tableFillRows tableSort)
  warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  let findLabel needle =
        listToMaybe [(r, t) | (r, t, _, _, _) <- spans, needle `T.isInfixOf` t]
  case (findLabel "David", findLabel "Role") of
    (Just _, Just (Rect rx _ rw _, _)) -> assertGt failed (rx + rw) 420
    _ -> assert failed False

-- Pixel host: Age (right) and City (left) must not sit on the shared grid line.
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

runScrollTopClipTest :: Context -> IORef Int -> IO ()
runScrollTopClipTest _ failed = do
  ctx <- newPixelContext
  cbRef <- newIORef Nothing
  let inp0 = withInputOff 400 160
      ui = do
        scrollWith (tight . grow) $
          columnWith (padAll 8 . gap 8 . fillW) $
            card $ do
              heading "Controls"
              (cb, _) <- checkbox "Feature" False
              _ <- slider 0 100 50
              mapM_ (\i -> void (label (T.pack ("pad line " <> show (i :: Int))))) [1 .. 16]
              uiIO $ writeIORef cbRef (Just cb)
              pure ()
      clipFits dmg = case dmg of
        DamageFull -> True
        DamageClip (Rect _ y _ h) -> y >= -1 && y + h <= 160 + 1
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  mCb <- readIORef cbRef
  case mCb of
    Nothing -> assert failed False
    Just cb -> do
      mR <- getPrevRect ctx (respId cb)
      case mR of
        Nothing -> assert failed False
        Just (Rect rx ry rw rh) -> do
          let hover = inp0 {inputMousePos = V2 (rx + rw / 2) (ry + rh / 2)}
          _ <- runFrame ctx hover ui
          dHover <- takeDamage ctx
          assert failed (clipFits dHover)

runScrollTest :: Context -> IORef Int -> IO ()
runScrollTest ctx failed = do
  let inp0 = (withInput 200 120) {inputMousePos = V2 20 20}
      ui = fmap fst $ scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 60}) $
             column (replicateM 8 (label "scroll line") >> pure ())
  _ <- runFrame ctx inp0 ui
  (sid, _, _, _) <- runFrame ctx inp0 ui
  off0 <- getScrollOffset ctx sid
  _ <- runFrame ctx (inp0 {inputScroll = V2 0 1}) ui
  off1 <- getScrollOffset ctx sid
  assertGt failed off1 off0

runNestedScrollTest :: Context -> IORef Int -> IO ()
runNestedScrollTest ctx failed = do
  let inp0 = withInput 200 200
      ui = scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 90}) $
             column $ do
               (inner, ()) <- scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 40}) $
                                column (mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 12])
               mapM_ (\i -> label (T.pack ("out " <> show (i :: Int)))) [1 .. 12]
               pure inner
  (outer, inner) <- warmup2 ctx inp0 ui
  mInner <- getPrevRect ctx inner
  mOuter <- getPrevRect ctx outer
  case (mInner, mOuter) of
    (Just (Rect ix iy iw ih), Just (Rect _ oy _ oh)) | iw > 0 && ih > 0 -> do
      let hoverInner = inp0 {inputMousePos = V2 (ix + iw / 2) (iy + ih / 2)}
          wheelInner = hoverInner {inputScroll = V2 0 1}
      offI0 <- getScrollOffset ctx inner
      offO0 <- getScrollOffset ctx outer
      _ <- runFrame ctx wheelInner ui
      offI1 <- getScrollOffset ctx inner
      offO1 <- getScrollOffset ctx outer
      assertGt failed offI1 offI0
      assertEq failed offO1 offO0
      let pumpInner = do
            before <- getScrollOffset ctx inner
            _ <- runFrame ctx wheelInner ui
            after <- getScrollOffset ctx inner
            if after > before then pumpInner else pure ()
      pumpInner
      offO2 <- getScrollOffset ctx outer
      assertEq failed offO2 offO1
      let hoverOuterY = min (oy + oh - 4) (iy + ih + 8)
          wheelOuter = inp0 {inputMousePos = V2 (ix + iw / 2) hoverOuterY, inputScroll = V2 0 1}
      offO3 <- getScrollOffset ctx outer
      _ <- runFrame ctx wheelOuter ui
      offO4 <- getScrollOffset ctx outer
      assertGt failed offO4 offO3
    _ -> assert failed False

runScrollHoverClipTest :: Context -> IORef Int -> IO ()
runScrollHoverClipTest ctx failed = do
  let inp0 = withInput 200 200
      ui = scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 80}) $
             column $ do
                mapM_ (\i -> label (T.pack ("out " <> show (i :: Int)))) [1 .. 10]
                (inner, ()) <- scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 36}) $
                                 column (mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 8])
                pure inner
  (_, inner) <- warmup2 ctx inp0 ui
  mInner <- getPrevRect ctx inner
  case mInner of
    Just (Rect ix iy iw ih) | iw > 0 && ih > 0 -> do
      let hoverHidden = inp0 {inputMousePos = V2 (ix + iw / 2) (iy + ih / 2), inputScroll = V2 0 1}
      offI0 <- getScrollOffset ctx inner
      _ <- runFrame ctx hoverHidden ui
      offI1 <- getScrollOffset ctx inner
      assert failed (offI1 <= offI0)
    _ -> assert failed False

runScrollButtonClickTest :: Context -> IORef Int -> IO ()
runScrollButtonClickTest ctx failed = do
  let inp0 = withInput 240 160
      ui = do
        (hit, setHit) <- useText ""
        (sid, resp) <- scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 80}) $
                         column $ do
                           mapM_ (\_ -> void (label "pad")) [(1 :: Int) .. 6]
                           b <- button' "Target"
                           onClick b (setHit "yes")
                           pure b
        pure (sid, hit, resp)
  (sid, hit0, _) <- warmup2 ctx inp0 ui
  assertEq failed hit0 ""
  mScroll <- getPrevRect ctx sid
  case mScroll of
    Just (Rect sx sy sw sh) -> do
      let wheel = inp0 {inputMousePos = V2 (sx + sw / 2) (sy + sh / 2), inputScroll = V2 0 1}
      forM_ [(1 :: Int) .. 8] $ \_ -> void (runFrame ctx wheel ui)
      off <- getScrollOffset ctx sid
      assertGt failed off 0
      ((_, _, resp1), _, _, _) <- runFrame ctx inp0 ui
      let Rect bx by bw bh = respRect resp1
      (_, hit1, _) <- runClickPair ctx inp0 ui (V2 (bx + bw / 2) (by + bh / 2))
      assertEq failed hit1 "yes"
    _ -> assert failed False

runScrollButtonClickSdlTest :: Context -> IORef Int -> IO ()
runScrollButtonClickSdlTest ctx failed = do
  let inp0 = withInput 640 120
      ui = do
        (hit, setHit) <- useText ""
        (sid, resp) <- scrollArea (tight (grow defaultLayout)) $
                         column $ do
                           mapM_ (\_ -> void (label "pad")) [(1 :: Int) .. 6]
                           b <- button' "Target"
                           onClick b (setHit "yes")
                           pure b
        pure (sid, hit, resp)
  (sid, hit0, _) <- warmup2 ctx inp0 ui
  assertEq failed hit0 ""
  mScroll <- getPrevRect ctx sid
  case mScroll of
    Just (Rect sx sy sw sh) -> do
      let wheel = inp0 {inputMousePos = V2 (sx + sw / 2) (sy + sh / 2), inputScroll = V2 0 1}
      forM_ [(1 :: Int) .. 8] $ \_ -> void (runFrame ctx wheel ui)
      off <- getScrollOffset ctx sid
      assertGt failed off 0
      ((_, _, resp1), _, _, _) <- runFrame ctx inp0 ui
      let Rect bx by bw bh = respRect resp1
      (_, hit1, _) <- runClickPair ctx inp0 ui (V2 (bx + bw / 2) (by + bh / 2))
      assertEq failed hit1 "yes"
    _ -> assert failed False

runScrollHitOffsetTest :: Context -> IORef Int -> IO ()
runScrollHitOffsetTest ctx failed = do
  let inp0 = withInput 200 200
      ui = scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 90}) $
             column $ do
               (inner, ()) <- scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 40}) $
                                column (mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 12])
               mapM_ (\i -> label (T.pack ("out " <> show (i :: Int)))) [1 .. 12]
               pure inner
  (_, inner) <- warmup2 ctx inp0 ui
  mInner0 <- getPrevRect ctx inner
  case mInner0 of
    Just (Rect ix iy iw ih) | iw > 0 && ih > 0 -> do
      let wheelInner = inp0 {inputMousePos = V2 (ix + iw / 2) (iy + ih / 2), inputScroll = V2 0 1}
      forM_ [(1 :: Int) .. 6] $ \_ -> void (runFrame ctx wheelInner ui)
      mInner1 <- getPrevRect ctx inner
      case mInner1 of
        Just (Rect ix1 iy1 iw1 _) -> do
          off0 <- getScrollOffset ctx inner
          assertGt failed off0 0
          let hoverAbove = inp0 {inputMousePos = V2 (ix1 + iw1 / 2) (iy1 - 6), inputScroll = V2 0 1}
          _ <- runFrame ctx hoverAbove ui
          off1 <- getScrollOffset ctx inner
          assert failed (off1 <= off0)
        _ -> assert failed False
    _ -> assert failed False

runNestedScrollFocusTest :: Context -> IORef Int -> IO ()
runNestedScrollFocusTest ctx failed = do
  let inp0 = withInput 240 220
      ui = scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 90}) $
             column $ do
               pair <- scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 50}) $
                         column $ do
                           b <- button' "In"
                           mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 10]
                           pure b
               mapM_ (\i -> label (T.pack ("out " <> show (i :: Int)))) [1 .. 10]
               pure pair
  (_, (inner, _)) <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  focus <- getFocusId ctx
  assert failed (focus /= WidgetId 0)
  offI0 <- getScrollOffset ctx inner
  -- Wheel events scroll only the scroller under the mouse; owning focus is not
  -- enough, so scrolling away from the inner scroller must not move it.
  let away = inp0 {inputMousePos = V2 230 210, inputScroll = V2 0 1}
  _ <- runFrame ctx away ui
  offI1 <- getScrollOffset ctx inner
  assertEq failed offI1 offI0

runScrolledOutClickImmunityTest :: Context -> IORef Int -> IO ()
runScrolledOutClickImmunityTest ctx failed = do
  let inp0 = withInput 240 160
      ui = do
        (hit, setHit) <- useText ""
        (sid, b) <- scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 8}) $
                      column $ do
                        mapM_ (\_ -> void (label "pad")) [(1 :: Int) .. 40]
                        btn <- button' "Target"
                        onClick btn (setHit "yes")
                        pure btn
        pure (sid, b, hit)
  (sid, b, hit0) <- warmup2 ctx inp0 ui
  assertEq failed hit0 ""
  mScroll <- getPrevRect ctx sid
  case mScroll of
    Just (Rect sx sy sw sh) -> do
      let wheel = inp0 {inputMousePos = V2 (sx + sw / 2) (sy + sh / 2), inputScroll = V2 0 1}
      forM_ [(1 :: Int) .. 80] $ \_ -> void (runFrame ctx wheel ui)
      mBtn <- getPrevRect ctx (respId b)
      case mBtn of
        Just (Rect bx by bw bh) -> do
          let ghost = inp0 {inputMousePos = V2 (bx + bw / 2) (by + bh / 2)}
          (_, _, hit1) <- runClickPair ctx ghost ui (V2 (bx + bw / 2) (by + bh / 2))
          assertEq failed hit1 ""
        _ -> assert failed False
    _ -> assert failed False

runScrolledOutHoverImmunityTest :: Context -> IORef Int -> IO ()
runScrolledOutHoverImmunityTest ctx failed = do
  let inp0 = withInput 240 160
      ui = do
        (sid, b) <- scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 8}) $
                      column $ do
                        mapM_ (\_ -> void (label "pad")) [(1 :: Int) .. 40]
                        btn <- button' "Target"
                        pure btn
        pure (sid, b)
  (sid, b) <- warmup2 ctx inp0 ui
  let target = respId b
  mScroll <- getPrevRect ctx sid
  case mScroll of
    Just (Rect sx sy sw sh) -> do
      let wheel = inp0 {inputMousePos = V2 (sx + sw / 2) (sy + sh / 2), inputScroll = V2 0 1}
      forM_ [(1 :: Int) .. 80] $ \_ -> void (runFrame ctx wheel ui)
      mBtn <- getPrevRect ctx target
      case mBtn of
        Just (Rect bx by bw bh) -> do
          let hover = inp0 {inputMousePos = V2 (bx + bw / 2) (by + bh / 2)}
          _ <- runFrame ctx hover ui
          hot <- getHotId ctx
          assert failed (hot /= target)
        _ -> assert failed False
    _ -> assert failed False

runScrolledOutCursorImmunityTest :: Context -> IORef Int -> IO ()
runScrolledOutCursorImmunityTest ctx failed = do
  let inp0 = withInput 240 160
      ui = do
        (sid, b) <- scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 8}) $
                      column $ do
                        mapM_ (\_ -> void (label "pad")) [(1 :: Int) .. 40]
                        btn <- button' "Target"
                        pure btn
        pure (sid, b)
  (sid, b) <- warmup2 ctx inp0 ui
  mScroll <- getPrevRect ctx sid
  case mScroll of
    Just (Rect sx sy sw sh) -> do
      let wheel = inp0 {inputMousePos = V2 (sx + sw / 2) (sy + sh / 2), inputScroll = V2 0 1}
      forM_ [(1 :: Int) .. 80] $ \_ -> void (runFrame ctx wheel ui)
      mBtn <- getPrevRect ctx (respId b)
      case mBtn of
        Just (Rect bx by bw bh) -> do
          let hover = inp0 {inputMousePos = V2 (bx + bw / 2) (by + bh / 2)}
          kind <- uiCursorKind ctx hover
          assertEq failed kind UiCursorDefault
        _ -> assert failed False
    _ -> assert failed False

runScrollChildDamageOffsetTest :: Context -> IORef Int -> IO ()
runScrollChildDamageOffsetTest ctx failed = do
  let inp0 = withInputOff 240 160
      scrollUi =
        fmap fst $
          scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 60}) $
            column (replicateM 8 (label "scroll line") >> pure ())
  sid <- warmup2 ctx inp0 scrollUi
  let bump20 = scrollUi >> uiIO (setScrollOffset ctx sid 20)
      bump40 = scrollUi >> uiIO (setScrollOffset ctx sid 40)
  _ <- runFrame ctx inp0 bump20
  d0 <- takeDamage ctx
  assert failed (case d0 of DamageClip r -> rectW r > 0 && rectH r > 0; DamageFull -> False)
  _ <- runFrame ctx inp0 bump40
  d1 <- takeDamage ctx
  assert failed (case d1 of DamageClip r -> rectW r > 0 && rectH r > 0; DamageFull -> False)

run2DScrollWheelTest :: Context -> IORef Int -> IO ()
run2DScrollWheelTest ctx failed = do
  let inp0 = withInput 240 200
      ui = scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 90}) $
             column $ do
               (inner, ()) <-
                 scrollArea
                   (defaultLayout {layoutWidth = Fixed 72, layoutHeight = Fixed 36, layoutDirection = Row})
                   (row (mapM_ (\i -> label (T.pack ("c" <> show (i :: Int)))) [1 .. 16]))
               mapM_ (\i -> label (T.pack ("r" <> show (i :: Int)))) [1 .. 12]
               pure inner
  (outer, inner) <- warmup2 ctx inp0 ui
  mInner <- getPrevRect ctx inner
  case mInner of
    Just (Rect ix iy iw ih) | iw > 0 && ih > 0 -> do
      let wheelX = inp0 {inputMousePos = V2 (ix + iw / 2) (iy + ih / 2), inputScroll = V2 1 0}
      offX0 <- getScrollOffset ctx inner
      _ <- runFrame ctx wheelX ui
      offX1 <- getScrollOffset ctx inner
      assertGt failed offX1 offX0
      mOuter <- getPrevRect ctx outer
      case mOuter of
        Just (Rect ox oy ow oh) -> do
          let wheelY = inp0 {inputMousePos = V2 (ox + ow / 2) (oy + oh - 4), inputScroll = V2 0 1}
          offY0 <- getScrollOffset ctx outer
          _ <- runFrame ctx wheelY ui
          offY1 <- getScrollOffset ctx outer
          assertGt failed offY1 offY0
        _ -> assert failed False
    _ -> assert failed False

runTable2DScrollSyncTest :: Context -> IORef Int -> IO ()
runTable2DScrollSyncTest _ failed = do
  ctx <- newContext
  let inp0 = (withInput 360 160) {inputMousePos = V2 60 80}
      ui = do
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        void (table "people" tableScrollCols tableScrollRows tableSort)
  warmup2 ctx inp0 ui
  spans0 <- collectTextSpans ctx
  let findIn spans needle =
        listToMaybe [(r, t) | (r, t, _, _, _) <- spans, needle `T.isInfixOf` t]
  case (findIn spans0 "Name", findIn spans0 "row-1") of
    (Just (Rect nx _ _ _, _), Just (Rect cn _ _ _, _)) -> do
      assert failed (abs (nx - cn) <= 1)
      let scrollInp = inp0 {inputScroll = V2 1 1}
      _ <- runFrame ctx scrollInp ui
      spans1 <- collectTextSpans ctx
      case (findIn spans1 "Name", findIn spans1 "row-1") of
        (Just (Rect nx1 _ _ _, _), Just (Rect cn1 _ _ _, _)) -> do
          assert failed (abs (nx1 - cn1) <= 1)
          assert failed (length spans1 >= 4)
        _ -> assert failed False
    _ -> assert failed False

-- | Diagnostic probe for the reported scroll stair-stepping artifact.
-- Steps a scroll container through fractional offsets at a simulated display
-- scale of 2 and decodes the final (snapped) vertex buffer, comparing the
-- per-row motion of text (glyph quads) against geometry (fill quads such as
-- separators). Asserts every row's text and the adjacent geometry moved by the
-- exact same delta on every transition (text/geometry lockstep).
runScrollLockstepProbeTest :: Context -> IORef Int -> IO ()
runScrollLockstepProbeTest ctx failed = do
  setDrawSnapScale ctx 2
  let inp0 = withInput 300 220
      rows =
        [ ("Feature", "Enabled")
        , ("Volume", "50")
        , ("Quality", "High")
        , ("Accent", "#3D7EFF")
        , ("Theme", "Tomorrow at Midnight Min")
        , ("Theme radio", "Theme radio value")
        , ("Name", "Ada Lovelace")
        , ("Notes", "short note")
        , ("Tree", "1 visible item")
        , ("Table sort", "Name")
        ]
      kvRow k v =
        row' (tight . gap 12 . alignMid . fillW $ defaultLayout) $ do
          void (labelEx (minW 88 (tight defaultLayout)) (T.pack k))
          void (labelEx (tight . fillW . alignEnd $ defaultLayout) (T.pack v))
      keys = map (T.pack . fst) rows
      ui =
        scrollArea
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 200})
          (column (mapM_ (\(k, v) -> kvRow k v >> separator) rows))
  _ <- runFrame ctx inp0 ui
  ((sid, ()), _, _, _) <- runFrame ctx inp0 ui
  let steps = [0.0, 0.3, 0.6, 1.0, 1.3, 1.7, 2.0, 2.4, 2.7, 3.1, 3.4, 3.8]
  yss <- forM steps $ \off -> do
    setScrollOffset ctx sid off
    _ <- runFrame ctx inp0 ui
    (_, _, draw, _) <- runFrame ctx inp0 ui
    snapped <- getScrollOffset ctx sid
    spans <- collectTextSpans ctx
    let keyYs = [listToMaybe (spanLabelYs k spans) | k <- keys]
    quads <- decodeQuads draw
    let fillTops =
          [ qy1
          | (qx1, qy1, qx2, _, u, v) <- quads
          , abs (u - whitePixelU) < 1.0e-6
          , abs (v - whitePixelV) < 1.0e-6
          , qx2 - qx1 > 60.0
          ]
    pure (off, snapped, keyYs, fillTops)
  putStrLn "=== scroll lockstep probe (scale=2, logical; device = 2x) ==="
  forM_ yss $ \(off, snapped, keyYs, fillTops) ->
    putStrLn $
      "off="
        ++ show off
        ++ " gsoff="
        ++ show snapped
        ++ " keyYs="
        ++ show keyYs
        ++ " fillTops="
        ++ show fillTops
  putStrLn "=== text-vs-geometry deltas per transition (logical) ==="
  forM_ (zip yss (drop 1 yss)) $ \((offA, _, keyA, fillA), (offB, _, keyB, fillB)) -> do
    let sKeyA = sort [y | Just y <- keyA]
        sKeyB = sort [y | Just y <- keyB]
        sFillA = sort (filter (> 1.0) fillA)
        sFillB = sort (filter (> 1.0) fillB)
        textDs = [x2 - x1 | (x1, x2) <- zip sKeyA sKeyB]
        fillDs = [x2 - x1 | (x1, x2) <- zip sFillA sFillB]
        n = min (length textDs) (length fillDs)
        t0 = case textDs of
          d : _ -> d
          [] -> 0
        f0 = case fillDs of
          d : _ -> d
          [] -> 0
        badText = take n [i | i <- textDs, abs (i - t0) > 1.0e-3]
        badFill = take n [i | i <- fillDs, abs (i - f0) > 1.0e-3]
        textUniform = null badText
        fillUniform = null badFill
        sync = length textDs == 0 || length fillDs == 0 || abs (t0 - f0) <= 1.0e-3
    putStrLn ("sKeyA=" ++ show sKeyA ++ " sFillA=" ++ show sFillA)
    putStrLn $
      "d("
        ++ show offA
        ++ " -> "
        ++ show offB
        ++ ") text="
        ++ show t0
        ++ " fill="
        ++ show f0
        ++ " textUniform="
        ++ show textUniform
        ++ " fillUniform="
        ++ show fillUniform
        ++ " sync="
        ++ show sync
    unless (textUniform && fillUniform && sync) $ do
      bump failed
      putStrLn ("  >>> NON-LOCKSTEP on transition " ++ show offA ++ " -> " ++ show offB ++ ": text=" ++ show badText ++ " fill=" ++ show badFill)
  return ()

-- | Decode the final (post-snap) quad list from a DrawData vertex buffer.
-- The harness emits each Quad as 4 consecutive vertices of 8 floats:
-- x, y, r, g, b, a, u, v at a 32 byte stride (vertexSize).
decodeQuads :: DrawData -> IO [(Float, Float, Float, Float, Float, Float)]
decodeQuads dd =
  withForeignPtr (drawVertices dd) $ \vp -> do
    let n = drawVertexCount dd `div` 4
        fptr = castPtr vp :: Ptr Float
    forM [0 .. n - 1] $ \q -> do
      let vBase = q * 8 * 4
      xs <- forM [0 .. 3] $ \k -> do
        let o = vBase + k * 8
        x <- peekElemOff fptr o
        y <- peekElemOff fptr (o + 1)
        u <- peekElemOff fptr (o + 6)
        v <- peekElemOff fptr (o + 7)
        pure (x, y, u, v)
      let x1 = minimum [x | (x, _, _, _) <- xs]
          y1 = minimum [y | (_, y, _, _) <- xs]
          x2 = maximum [x | (x, _, _, _) <- xs]
          y2 = maximum [y | (_, y, _, _) <- xs]
          (_u, _v) =
            case xs of
              (_, _, u, v) : _ -> (u, v)
              [] -> (0, 0)
      pure (x1, y1, x2, y2, _u, _v)

whitePixelU :: Float
whitePixelU = 1.5 / 1024.0

whitePixelV :: Float
whitePixelV = 1.5 / 1024.0

-- A padded 2D scroller whose fill-width child fits the viewport must not
-- report horizontal overflow: content size is measured from the content
-- origin (after the leading padding), not from the padding-box origin,
-- which double-counts the padding and makes a fitting child look padX
-- wider than the viewport every frame. Same for the main axis of a padded
-- 1D vertical scroller. Fitting content must not wheel-scroll either: the
-- trailing padding extends the scroll range only once an axis genuinely
-- overflows.
run2DPadFillOverflowTest :: Context -> IORef Int -> IO ()
run2DPadFillOverflowTest _ failed = do
  ctx <- newPixelContext
  let inp0 = withInput 320 240
      ui =
        scrollArea2D (padAll 6 . fixedH 168 . fillW $ defaultLayout) $
          columnWith (tight . fillW) $
            mapM_ (void . label) (map T.pack ["alpha", "beta", "gamma"])
  (wid, ()) <- warmup2 ctx inp0 ui
  mState <- scrollNodeState ctx wid True
  case mState of
    Nothing -> assert failed False
    Just (contentW, innerW) -> assert failed (contentW <= innerW + overflowEps)
  -- No phantom scroll range: wheeling must not move either axis.
  let wheel = inp0 {inputScroll = V2 5 5}
  _ <- runFrame ctx wheel ui
  V2 offX offY <- getScrollOffset2D ctx wid
  assert failed (offX == 0 && offY == 0)
  let ui1 = scrollArea (padAll 6 . fixedH 80 . fillW $ defaultLayout) (void (labelEx (tight defaultLayout) (T.pack "fits")))
  (wid1, ()) <- warmup2 ctx inp0 ui1
  mState1 <- scrollNodeState ctx wid1 False
  case mState1 of
    Nothing -> assert failed False
    Just (contentH, innerH) -> assert failed (contentH <= innerH + overflowEps)

-- Padding (padAll 6) used by the 2D pad tests, and the resulting reduction
-- of the scroller rect to the padded inner size.
padTestPx, padTestBoth :: Float
padTestPx = 6
padTestBoth = padTestPx * 2

-- Same overflow epsilon as scrollAxisOverflows / scrollAxisRange.
overflowEps :: Float
overflowEps = 0.5

scrollNodeState :: Context -> WidgetId -> Bool -> IO (Maybe (Float, Float))
scrollNodeState ctx wid is2D = do
  let na = ctxNodeArena ctx
  n <- arenaCount na
  finds <-
    fmap
      (concat @[])
      ( forM [0 .. n - 1] $ \i -> do
          nt <- getNodeType na i
          if nt /= NodeScrollContainer
            then pure []
            else do
              w' <- getWidgetId na i
              if w' /= wid
                then pure []
                else do
                  contentMain <-
                    if is2D
                      then getScrollContentW na i
                      else getNodeValue na i
                  (_, _, rw, rh) <- getRect na i
                  let inner = if is2D then rw - padTestBoth else rh - padTestBoth
                  pure [(contentMain, inner)]
      )
  pure (listToMaybe finds)

-- The other side of the pad fix: a padded 2D scroller whose child really is
-- wider and taller than the viewport must still report overflow on both
-- axes, wheel-scroll vertically, and let scrolling reach the trailing
-- padding at the end (the range extends past the last child by padB).
run2DPadOverflowScrollsTest :: Context -> IORef Int -> IO ()
run2DPadOverflowScrollsTest _ failed = do
  ctx <- newPixelContext
  let inp0 = (withInput 320 240) {inputMousePos = V2 100 100}
      ui =
        scrollArea2D (padAll 6 . fixedH 168 . fillW $ defaultLayout) $
          columnWith (tight . fillW) $ do
            void (labelEx (tight . fixedW 500 $ defaultLayout) (T.pack "wide child"))
            mapM_ (void . label) (map T.pack (replicate 30 "scroll line"))
  (wid, ()) <- warmup2 ctx inp0 ui
  mState <- scrollNodeState ctx wid True
  case mState of
    Nothing -> assert failed False
    Just (contentW, innerW) -> do
      -- The 500px child genuinely overflows the ~308px inner width.
      assertGt failed contentW (innerW + 40)
  mStateH <- scrollNodeState ctx wid False
  case mStateH of
    Nothing -> assert failed False
    Just (contentH, innerH) -> do
      assertGt failed contentH (innerH + 100)
      -- Scroll far past the end: the clamp must land on the trailing-pad
      -- extended range (content + padB - view), not the flush content - view,
      -- so the bottom padding is reachable. The horizontal bar is active
      -- (the 500px child overflows), so it takes its lane out of the vertical
      -- viewport: view = innerH - laneH.
      let laneH =
            scrollBarGutter (ctxHostProfile ctx) (ctxFontMetrics ctx)
              + scrollBarListExtra
          wheelDown = inp0 {inputScroll = V2 0 50}
      replicateM_ 40 (runFrame ctx wheelDown ui)
      V2 _ offEnd <- getScrollOffset2D ctx wid
      assert failed (abs (offEnd - (contentH + padTestPx - (innerH - laneH))) < 1.5)

-- Hovering the right edge of a table header button must raise the
-- horizontal-resize cursor, and pressing + dragging from there must actually
-- widen the column.
runTableColResizeCursorTest :: Context -> IORef Int -> IO ()
runTableColResizeCursorTest _ failed = do
  ctx <- newPixelContext
  let inp0 = (withInput 400 240) {inputMousePos = V2 30 30}
      ui = do
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        void
          ( tableCfg
              defaultTableCfg
              (tight . fillW . fixedH 180 $ defaultLayout {layoutGap = 0})
              "people"
              tableScrollCols
              (take 5 tableScrollRows)
              tableSort
          )
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  mhdr <- headerButtonRect ctx
  case mhdr of
    Nothing -> assert failed False
    Just (Rect hx hy hw hh) -> do
      let edgeX = hx + hw - 2
          hoverInp = inp0 {inputMousePos = V2 edgeX (hy + hh / 2)}
      _ <- runFrame ctx hoverInp ui
      kind <- uiCursorKind ctx hoverInp
      assertEq failed kind UiCursorEwResize
      let pressInp = hoverInp {inputMouseDown = True, inputMousePressed = True}
          dragInp x = inp0 {inputMousePos = V2 x (hy + hh / 2), inputMouseDown = True}
      _ <- runFrame ctx pressInp ui
      _ <- runFrame ctx (dragInp (edgeX + 60)) ui
      _ <- runFrame ctx (dragInp (edgeX + 60)) ui
      mhdr2 <- headerButtonRect ctx
      case mhdr2 of
        Nothing -> assert failed False
        Just (Rect _ _ hw2 _) -> assertGt failed hw2 (hw + 30)

-- The demo's page structure (page scroller, card panel, five columns). Every
-- column boundary must raise the resize cursor and resize when grabbed down
-- in the column BODY, not only on the header cell.
runTableColResizeDemoReproTest :: Context -> IORef Int -> IO ()
runTableColResizeDemoReproTest _ failed = do
  ctx <- newPixelContext
  let inp0 = (withInput 700 500) {inputMousePos = V2 400 100}
      ui =
        scrollWith (tight . grow) $
          columnWith (padAll 6 . gap 6 . fillW) $
            card $ do
              (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
              void
                ( tableCfg
                    defaultTableCfg
                    (tight . fillW . fixedH 280 $ defaultLayout {layoutGap = 0})
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
            bodyY = (hy + hh + bodyBot) / 2
            hoverInp = inp0 {inputMousePos = V2 edgeX bodyY}
        _ <- runFrame ctx hoverInp ui
        kind <- uiCursorKind ctx hoverInp
        assertEq failed kind UiCursorEwResize
        let pressInp = hoverInp {inputMouseDown = True, inputMousePressed = True}
            dragInp x = inp0 {inputMousePos = V2 x bodyY, inputMouseDown = True}
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

-- The table's horizontal scrollbar belongs to the body scroller: it spans
-- the table's bottom edge, appears stably while the columns overflow (no
-- appear/disappear flicker mid-drag or at rest), and is draggable (2D thumb
-- drag). The header scroller stays chrome-less (policyX Hidden) so no bar
-- ever sits under the header row.
runTableHBarStableTest :: Context -> IORef Int -> IO ()
runTableHBarStableTest _ failed = do
  ctx <- newPixelContext
  let inp0 = (withInput 400 240) {inputMousePos = V2 30 30}
      ui = do
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        void
          ( tableCfg
              defaultTableCfg
              (tight . fillW . fixedH 180 $ defaultLayout {layoutGap = 0})
              "people"
              tableScrollCols
              (take 5 tableScrollRows)
              tableSort
          )
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  mhdr <- headerButtonRect ctx
  case mhdr of
    Nothing -> assert failed False
    Just (Rect hx hy hw hh) -> do
      let edgeX = hx + hw - 2
          headerY = hy + hh / 2
          pressInp = inp0 {inputMousePos = V2 edgeX headerY, inputMouseDown = True, inputMousePressed = True}
          dragInp x = inp0 {inputMousePos = V2 x headerY, inputMouseDown = True}
      _ <- runFrame ctx pressInp ui
      -- Drag outward past the pane edge, then hold still.
      let steps = [edgeX + 40, edgeX + 80, edgeX + 120, edgeX + 160, edgeX + 200, edgeX + 200, edgeX + 200, edgeX + 200]
      states <- forM steps $ \x -> do
        _ <- runFrame ctx (dragInp x) ui
        bodyHBarActive ctx
      -- While the columns overflow, the bar must be active on every frame,
      -- including the held-still tail (the old header lane vanished at rest
      -- because its flag compared the content against a content-floored
      -- rect).
      let tail4 = drop 4 states
      case tail4 of
        (lastFlag : _) -> assert failed (all (== lastFlag) tail4 && lastFlag)
        [] -> assert failed False
      -- Release the resize drag and let the layout settle.
      _ <- runFrame ctx inp0 ui
      _ <- runFrame ctx inp0 ui
      -- The header scroller must never reserve or show a horizontal bar.
      hdrSi <- headerScrollerStyle ctx
      case hdrSi of
        Nothing -> assert failed False
        Just si -> assert failed (si .&. 3 == 3)
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

-- Width of the vertical-bar lane (bar + outer gap) reserved inside the body
-- scroller while the columns overflow; the h-bar overflow check compares
-- content width against the lane-shrunk view.
bodyVBarLaneW :: Float
bodyVBarLaneW = 6

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

-- | True when the body scroller's horizontal axis overflows (bar active).
bodyHBarActive :: Context -> IO Bool
bodyHBarActive ctx = do
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
              contentW <- getScrollContentW na i
              (_, _, w, _) <- getRect na i
              pure [contentW > w - bodyVBarLaneW + overflowEps]
      )
  pure (case finds of
    (b : _) -> b
    [] -> False)

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

-- | Style index of the header row scroller (the Row-direction one).
headerScrollerStyle :: Context -> IO (Maybe Int)
headerScrollerStyle ctx = do
  let na = ctxNodeArena ctx
  n <- arenaCount na
  finds <-
    fmap
      (concat @[])
      ( forM [0 .. n - 1] $ \i -> do
          nt <- getNodeType na i
          if nt /= NodeScrollContainer
            then pure []
            else do
              d <- getDirection na i
              if d /= DirRow
                then pure []
                else do
                  si <- getStyleIdx na i
                  pure [si]
      )
  pure (listToMaybe finds)



-- | Horizontal reach: at the end of the horizontal scroll the last column must
-- clear the vertical scrollbar lane, not stop with its right edge under the
-- lane. The body scroller's own vertical bar shrinks the horizontal viewport,
-- so the reachable range must subtract that lane (regression: the range used
-- the full padding box, leaving the last column partly hidden).
runTableHBarReachTest :: Context -> IORef Int -> IO ()
runTableHBarReachTest _ failed = do
  ctx <- newPixelContext
  let inp0 = (withInput 700 320) {inputMousePos = V2 300 160}
      cfg = defaultTableCfg {tableColSizes = [ColFixed 500, ColFixed 500]}
      ui = do
        (tableSort, _) <- useTableSort (SortCol 0 SortAsc)
        void
          ( tableCfg
              cfg
              (tight . fillW . fixedH 200 $ defaultLayout {layoutGap = 0})
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
