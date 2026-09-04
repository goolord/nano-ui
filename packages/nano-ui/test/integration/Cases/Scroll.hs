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
  , runTableFirstColWidthTest
  , runTableFillWidthTest
  , runTableContentSlackTest
  , runTableCellPadTest
  , runTableFitScrollColWidthTest
  , runTableTabWrapRowTest
  , runScrolledOutClickImmunityTest
  , runScrolledOutHoverImmunityTest
  , runScrolledOutCursorImmunityTest
  , runScrollChildDamageOffsetTest
  , run2DScrollWheelTest
  , runTable2DScrollSyncTest
  ) where

import Control.Monad (forM_, replicateM, void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertGt, withInput)
import NanoUI.Testing.Harness
  ( assertScrollGutter
  , assertScrollGutterPad
  , findGrabHover
  , runClickPair
  , spanXOf
  , spanYOf
  , warmup2
  , withInputOff
  )

runScrollThumbCursorTest :: Context -> IORef Int -> IO ()
runScrollThumbCursorTest ctx failed = do
  let inp0 = withInput 200 120
      ui = scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 80})
             (column defaultLayout (replicateM 8 (label "scroll line") >> pure ()))
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
      ui = panel (grow defaultLayout) $
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
            column defaultLayout (replicateM 8 (label "scroll line") >> pure ())
      inp0 = withInputOff 200 120
  sid <- warmup2 ctx inp0 scrollUi
  let bumpUi = scrollUi >> uiIO (setScrollOffset ctx sid 24)
  _ <- runFrame ctx inp0 bumpUi
  dScroll <- takeDamage ctx
  case dScroll of
    DamageFull -> assert failed False
    DamageClip r -> assert failed (rectW r > 0 && rectH r > 0 && rectH r <= 60 + defaultDamageSlop * 2 && not (damageIsEmpty dScroll))

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
      ui = row (tight . gap 8 . wrap . fillW $ defaultLayout) $ do
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
        scroll (tight (grow defaultLayout)) $
          column (padAll 8 . gap 8 . fillW $ defaultLayout) $
            card $ do
              heading "Controls"
              (cb, _) <- checkbox "Feature" False
              _ <- slider "Volume" 0 100 50
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
             column defaultLayout (replicateM 8 (label "scroll line") >> pure ())
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
             column defaultLayout $ do
               (inner, ()) <- scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 40}) $
                                column defaultLayout (mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 12])
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
             column defaultLayout $ do
               mapM_ (\i -> label (T.pack ("out " <> show (i :: Int)))) [1 .. 10]
               (inner, ()) <- scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 36}) $
                                column defaultLayout (mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 8])
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
                         column defaultLayout $ do
                           mapM_ (\_ -> void (label "pad")) [(1 :: Int) .. 6]
                           b <- button "Target"
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
                         column defaultLayout $ do
                           mapM_ (\_ -> void (label "pad")) [(1 :: Int) .. 6]
                           b <- button "Target"
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
             column defaultLayout $ do
               (inner, ()) <- scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 40}) $
                                column defaultLayout (mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 12])
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
             column defaultLayout $ do
               pair <- scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 50}) $
                         column defaultLayout $ do
                           b <- button "In"
                           mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 10]
                           pure b
               mapM_ (\i -> label (T.pack ("out " <> show (i :: Int)))) [1 .. 10]
               pure pair
  (_, (inner, _)) <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  focus <- getFocusId ctx
  assert failed (focus /= WidgetId 0)
  offI0 <- getScrollOffset ctx inner
  let away = inp0 {inputMousePos = V2 230 210, inputScroll = V2 0 1}
  _ <- runFrame ctx away ui
  offI1 <- getScrollOffset ctx inner
  assertGt failed offI1 offI0

runScrolledOutClickImmunityTest :: Context -> IORef Int -> IO ()
runScrolledOutClickImmunityTest ctx failed = do
  let inp0 = withInput 240 160
      ui = do
        (hit, setHit) <- useText ""
        (sid, b) <- scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 8}) $
                      column defaultLayout $ do
                        mapM_ (\_ -> void (label "pad")) [(1 :: Int) .. 40]
                        btn <- button "Target"
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
                      column defaultLayout $ do
                        mapM_ (\_ -> void (label "pad")) [(1 :: Int) .. 40]
                        btn <- button "Target"
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
                      column defaultLayout $ do
                        mapM_ (\_ -> void (label "pad")) [(1 :: Int) .. 40]
                        btn <- button "Target"
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
            column defaultLayout (replicateM 8 (label "scroll line") >> pure ())
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
             column defaultLayout $ do
               (inner, ()) <-
                 scrollArea
                   (defaultLayout {layoutWidth = Fixed 72, layoutHeight = Fixed 36, layoutDirection = Row})
                   (row defaultLayout (mapM_ (\i -> label (T.pack ("c" <> show (i :: Int)))) [1 .. 16]))
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


