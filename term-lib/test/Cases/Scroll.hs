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
  ) where

import Control.Monad (forM_, replicateM, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (bump, failWhen, withInput)
import NanoUI.Testing.Harness
  ( assertScrollGutter
  , assertScrollGutterPad
  , findGrabHover
  , runClickPair
  , warmup2
  , withInputOff
  )
runScrollThumbCursorTest :: Context -> IORef Int -> IO ()
runScrollThumbCursorTest ctx failed = do
  let
    inp0 = withInput 200 120
    ui =
      scrollArea
        (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 80})
        ( column defaultLayout $ do
            _ <- replicateM 8 (label "scroll line")
            pure ()
        )
  _ <- runFrame ctx inp0 ui
  ((sid, ()), _, _, _) <- runFrame ctx inp0 ui
  mrect <- getPrevRect ctx sid
  case mrect of
    Nothing -> bump failed
    Just (Rect rx ry rw rh) -> do
      let
        barW = scrollBarWidth
        thumbX = rx + rw - scrollBarListExtra - barW / 2
        tryYs = [ry + rh * n / 8 | n <- [1 .. 7]]
      mHover <- findGrabHover ctx ui inp0 thumbX tryYs
      case mHover of
        Nothing -> bump failed
        Just hover -> do
          kind <- uiCursorKind ctx hover
          when (kind /= UiCursorGrab) $ bump failed
          let
            press =
              hover
                { inputMouseDown = True
                , inputMousePressed = True
                }
          _ <- runFrame ctx press ui
          grabbing <- cursorKindIs ctx press UiCursorGrabbing
          failWhen failed (not grabbing)

runScrollBarGutterTest :: Context -> IORef Int -> IO ()
runScrollBarGutterTest ctx failed = do
  let
    inp0 = withInput 200 120
    ui = do
      (sid, child) <-
        scrollArea
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 60})
          ( do
              r <- labelEx (fillW defaultLayout) "Wide"
              _ <- replicateM 8 (label "scroll line")
              pure r
          )
      pure (sid, child)
  (sid, child) <- warmup2 ctx inp0 ui
  let
    endPad = padR (layoutPadding defaultLayout)
    gutter =
      scrollBarGutter (ctxHostProfile ctx) (ctxFontMetrics ctx) + scrollBarListExtra
  assertScrollGutterPad failed ctx sid child gutter endPad

runGrowScrollGutterTest :: Context -> IORef Int -> IO ()
runGrowScrollGutterTest ctx failed = do
  let
    inp0 = withInput 240 140
    ui = do
      (sid, child) <-
        scrollArea
          (tight (grow defaultLayout))
          ( do
              r <- labelEx (fillW defaultLayout) "Wide"
              _ <- replicateM 20 (label "scroll line")
              pure r
          )
      pure (sid, child)
  (sid, child) <- warmup2 ctx inp0 ui
  let
    gutter = scrollBarGutter (ctxHostProfile ctx) (ctxFontMetrics ctx) + scrollBarPageExtra
  assertScrollGutter failed ctx sid child gutter

runPanelGrowScrollGutterTest :: Context -> IORef Int -> IO ()
runPanelGrowScrollGutterTest ctx failed = do
  let
    inp0 = withInput 240 140
    ui = do
      (sid, child) <-
        panel (grow defaultLayout) $
          scrollArea (tight (grow defaultLayout)) $ do
            r <- labelEx (fillW defaultLayout) "Wide"
            _ <- replicateM 20 (label "scroll line")
            pure r
      pure (sid, child)
  (sid, child) <- warmup2 ctx inp0 ui
  let
    gutter =
      scrollBarGutter (ctxHostProfile ctx) (ctxFontMetrics ctx) + scrollBarListExtra
  assertScrollGutter failed ctx sid child gutter

runScrollDamageTest :: Context -> IORef Int -> IO ()
runScrollDamageTest _ failed = do
  ctx <- newContext
  let
    ui = do
      (sid, _) <-
        scrollArea
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 60})
          ( column defaultLayout $ do
              _ <- replicateM 8 (label "scroll line")
              pure ()
          )
      pure sid
    inp0 =
      withInputOff 200 120
  (_, _, _, _) <- runFrame ctx inp0 ui
  let
    inpHover = inp0 {inputMousePos = V2 20 20}
  _ <- runFrame ctx inpHover ui
  dHover <- takeDamage ctx
  case dHover of
    DamageFull -> bump failed
    DamageClip {} -> pure ()
  let
    inpScroll = inpHover {inputScroll = V2 0 1}
  _ <- runFrame ctx inpScroll ui
  dScroll <- takeDamage ctx
  when (dScroll /= DamageFull) $ bump failed

runTableScrollTest :: Context -> IORef Int -> IO ()
runTableScrollTest _ failed = do
  ctx <- newContext
  let
    inp0 = (withInput 320 120) {inputMousePos = V2 40 70}
    ui = do
      (readSort, _) <- useTableSort (SortCol 0 SortAsc)
      tableSort <- readSort
      void (table "people" tableScrollCols tableScrollRows tableSort)
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  spans0 <- collectTextSpans ctx
  let findLabel needle =
        listToMaybe [(r, t, fg, bg, c) | (r, t, fg, bg, c) <- spans0, needle `T.isInfixOf` t]
  case (findLabel "Name", findLabel "row-1", findLabel "val-1") of
    (Just (Rect nx _ _ _, _, _, _, _), Just (Rect cn _ _ _, _, _, _, _), Just (Rect cvx _ _ _, _, _, _, _)) -> do
      when (abs (nx - cn) > 1) $ do
        putStrLn ("table-scroll: x mismatch nx=" ++ show nx ++ " cn=" ++ show cn)
        bump failed
      when (cvx <= cn) $ bump failed
    _ -> do
      putStrLn "table-scroll: missing header/body spans"
      bump failed
  let scrollInp = inp0 {inputScroll = V2 0 1}
  _ <- runFrame ctx scrollInp ui
  spans1 <- collectTextSpans ctx
  when (length spans1 < length spans0 `div` 2) $ do
    putStrLn ("table-scroll: span drop " ++ show (length spans0) ++ " -> " ++ show (length spans1))
    bump failed

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

runScrollTopClipTest :: Context -> IORef Int -> IO ()
runScrollTopClipTest _ failed = do
  ctx <- newPixelContext
  cbRef <- newIORef Nothing
  let
    inp0 = withInputOff 400 160
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
    clipFits dmg =
      case dmg of
        DamageFull -> True
        DamageClip (Rect _ y _ h) -> y >= -1 && y + h <= 160 + 1
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  mCb <- readIORef cbRef
  case mCb of
    Nothing -> bump failed
    Just cb -> do
      mR <- getPrevRect ctx (respId cb)
      case mR of
        Nothing -> bump failed
        Just (Rect rx ry rw rh) -> do
          let hover = inp0 {inputMousePos = V2 (rx + rw / 2) (ry + rh / 2)}
          _ <- runFrame ctx hover ui
          dHover <- takeDamage ctx
          when (not (clipFits dHover)) $ bump failed

runScrollTest :: Context -> IORef Int -> IO ()
runScrollTest ctx failed = do
  let
    inp0 = (withInput 200 120) {inputMousePos = V2 20 20}
    ui = do
      (sid, _) <-
        scrollArea
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 60})
          ( column defaultLayout $ do
              _ <- replicateM 8 (label "scroll line")
              pure ()
          )
      pure sid
  _ <- runFrame ctx inp0 ui
  (sid, _, _, _) <- runFrame ctx inp0 ui
  off0 <- getScrollOffset ctx sid
  let
    inpScroll = inp0 {inputScroll = V2 0 1}
  (_, _, _, _) <- runFrame ctx inpScroll ui
  off1 <- getScrollOffset ctx sid
  when (off1 <= off0) $ bump failed

runNestedScrollTest :: Context -> IORef Int -> IO ()
runNestedScrollTest ctx failed = do
  let
    inp0 = withInput 200 200
    ui = do
      (outer, inner) <-
        scrollArea
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 90})
          ( column defaultLayout $ do
              (inner, ()) <-
                scrollArea
                  (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 40})
                  ( column defaultLayout $ do
                      mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 12]
                  )
              mapM_ (\i -> label (T.pack ("out " <> show (i :: Int)))) [1 .. 12]
              pure inner
          )
      pure (outer, inner)
  (outer, inner) <- warmup2 ctx inp0 ui
  mInner <- getPrevRect ctx inner
  mOuter <- getPrevRect ctx outer
  case (mInner, mOuter) of
    (Just (Rect ix iy iw ih), Just (Rect _ oy _ oh))
      | iw > 0 && ih > 0 -> do
          let
            hoverInner = inp0 {inputMousePos = V2 (ix + iw / 2) (iy + ih / 2)}
            wheelInner = hoverInner {inputScroll = V2 0 1}
          offI0 <- getScrollOffset ctx inner
          offO0 <- getScrollOffset ctx outer
          _ <- runFrame ctx wheelInner ui
          offI1 <- getScrollOffset ctx inner
          offO1 <- getScrollOffset ctx outer
          when (offI1 <= offI0) $ bump failed
          when (offO1 /= offO0) $ bump failed
          let
            pumpInner = do
              before <- getScrollOffset ctx inner
              _ <- runFrame ctx wheelInner ui
              after <- getScrollOffset ctx inner
              when (after > before) pumpInner
          pumpInner
          offO2 <- getScrollOffset ctx outer
          when (offO2 /= offO1) $ bump failed
          let
            hoverOuterY = min (oy + oh - 4) (iy + ih + 8)
            wheelOuter =
              inp0
                { inputMousePos = V2 (ix + iw / 2) hoverOuterY
                , inputScroll = V2 0 1
                }
          offO3 <- getScrollOffset ctx outer
          _ <- runFrame ctx wheelOuter ui
          offO4 <- getScrollOffset ctx outer
          when (offO4 <= offO3) $ bump failed
    _ -> bump failed

runScrollHoverClipTest :: Context -> IORef Int -> IO ()
runScrollHoverClipTest ctx failed = do
  let
    inp0 = withInput 200 200
    ui = do
      (outer, inner) <-
        scrollArea
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 80})
          ( column defaultLayout $ do
              mapM_ (\i -> label (T.pack ("out " <> show (i :: Int)))) [1 .. 10]
              (inner, ()) <-
                scrollArea
                  (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 36})
                  ( column defaultLayout $ do
                      mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 8]
                  )
              pure inner
          )
      pure (outer, inner)
  (_, inner) <- warmup2 ctx inp0 ui
  mInner <- getPrevRect ctx inner
  case mInner of
    Just (Rect ix iy iw ih)
      | iw > 0 && ih > 0 -> do
          let
            hoverHidden =
              inp0
                { inputMousePos = V2 (ix + iw / 2) (iy + ih / 2)
                , inputScroll = V2 0 1
                }
          offI0 <- getScrollOffset ctx inner
          _ <- runFrame ctx hoverHidden ui
          offI1 <- getScrollOffset ctx inner
          when (offI1 > offI0) $ bump failed
    _ -> bump failed

runScrollButtonClickTest :: Context -> IORef Int -> IO ()
runScrollButtonClickTest ctx failed = do
  let
    inp0 = withInput 240 160
    ui = do
      (readHit, setHit) <- useText ""
      (sid, resp) <-
        scrollArea
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 80})
          ( column defaultLayout $ do
              mapM_ (\_ -> void (label "pad")) [(1 :: Int) .. 12]
              b <- button "Target"
              onClick b (setHit "yes")
              pure b
          )
      hit <- readHit
      pure (sid, hit, resp)
  (sid, hit0, _) <- warmup2 ctx inp0 ui
  when (hit0 /= "") $ bump failed
  mScroll <- getPrevRect ctx sid
  case mScroll of
    Just (Rect sx sy sw sh) -> do
      let
        wheel =
          inp0
            { inputMousePos = V2 (sx + sw / 2) (sy + sh / 2)
            , inputScroll = V2 0 1
            }
      forM_ [(1 :: Int) .. 8] $ \_ -> void (runFrame ctx wheel ui)
      off <- getScrollOffset ctx sid
      when (off <= 0) $ bump failed
      ((_, _, resp1), _, _, _) <- runFrame ctx inp0 ui
      let
        Rect bx by bw bh = respRect resp1
        clickPos = V2 (bx + bw / 2) (by + bh / 2)
      (_, hit1, _) <- runClickPair ctx inp0 ui clickPos
      when (hit1 /= "yes") $ bump failed
    _ -> bump failed

runScrollButtonClickSdlTest :: Context -> IORef Int -> IO ()
runScrollButtonClickSdlTest ctx failed = do
  let
    inp0 = withInput 640 480
    ui = do
      (readHit, setHit) <- useText ""
      (sid, resp) <-
        scrollArea
          (tight (grow defaultLayout))
          ( column (padAll 8 . gap 8 . fillW $ defaultLayout) $ do
              panel (padXY 14 10 . gap 8 . fillW $ defaultLayout) $
                void (heading "nano-ui")
              card $ do
                heading "Controls"
                b <- button "Target"
                onClick b (setHit "yes")
                mapM_ (\_ -> void (label "pad")) [(1 :: Int) .. 40]
                pure b
          )
      hit <- readHit
      pure (sid, hit, resp)
  (sid, hit0, respBase) <- warmup2 ctx inp0 ui
  let y0 = rectY (respRect respBase)
  when (hit0 /= "") $ bump failed
  mScroll <- getPrevRect ctx sid
  case mScroll of
    Just (Rect sx sy sw sh) -> do
      let
        wheel =
          inp0
            { inputMousePos = V2 (sx + sw / 2) (sy + sh / 2)
            , inputScroll = V2 0 1
            }
      forM_ [(1 :: Int) .. 12] $ \_ -> void (runFrame ctx wheel ui)
      off <- getScrollOffset ctx sid
      when (off <= 0) $ bump failed
      ((_, _, resp1), _, _, _) <- runFrame ctx inp0 ui
      let y1 = rectY (respRect resp1)
      when (abs (y1 - (y0 - off)) > 2) $ bump failed
      let
        Rect bx by bw bh = respRect resp1
        clickPos = V2 (bx + bw / 2) (by + bh / 2)
      (_, hit1, _) <- runClickPair ctx inp0 ui clickPos
      when (hit1 /= "yes") $ do
        putStrLn $
          "click miss hit="
            ++ show hit1
            ++ " pos="
            ++ show clickPos
            ++ " rect="
            ++ show (respRect resp1)
        bump failed
    _ -> bump failed

runScrollHitOffsetTest :: Context -> IORef Int -> IO ()
runScrollHitOffsetTest ctx failed = do
  let
    inp0 = withInput 200 200
    ui = do
      (outer, inner) <-
        scrollArea
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 90})
          ( column defaultLayout $ do
              (inner, ()) <-
                scrollArea
                  (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 40})
                  ( column defaultLayout $ do
                      mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 12]
                  )
              mapM_ (\i -> label (T.pack ("out " <> show (i :: Int)))) [1 .. 12]
              pure inner
          )
      pure (outer, inner)
  (_, inner) <- warmup2 ctx inp0 ui
  mInner0 <- getPrevRect ctx inner
  case mInner0 of
    Just (Rect ix iy iw ih)
      | iw > 0 && ih > 0 -> do
          let
            wheelInner =
              inp0
                { inputMousePos = V2 (ix + iw / 2) (iy + ih / 2)
                , inputScroll = V2 0 1
                }
          forM_ [(1 :: Int) .. 6] $ \_ -> void (runFrame ctx wheelInner ui)
          mInner1 <- getPrevRect ctx inner
          case mInner1 of
            Just (Rect ix1 iy1 iw1 _) -> do
              off0 <- getScrollOffset ctx inner
              when (off0 <= 0) $ bump failed
              let
                hoverAbove =
                  inp0
                    { inputMousePos = V2 (ix1 + iw1 / 2) (iy1 - 6)
                    , inputScroll = V2 0 1
                    }
              _ <- runFrame ctx hoverAbove ui
              off1 <- getScrollOffset ctx inner
              when (off1 > off0) $ bump failed
            _ -> bump failed
    _ -> bump failed

runNestedScrollFocusTest :: Context -> IORef Int -> IO ()
runNestedScrollFocusTest ctx failed = do
  let
    inp0 = withInput 240 220
    ui = do
      (outer, (inner, btn)) <-
        scrollArea
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 90})
          ( column defaultLayout $ do
              pair <-
                scrollArea
                  (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 50})
                  ( column defaultLayout $ do
                      b <- button "In"
                      mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 10]
                      pure b
                  )
              mapM_ (\i -> label (T.pack ("out " <> show (i :: Int)))) [1 .. 10]
              pure pair
          )
      pure (outer, inner, btn)
  (_, inner, _) <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  focus <- getFocusId ctx
  when (focus == WidgetId 0) $ bump failed
  offI0 <- getScrollOffset ctx inner
  let
    away =
      inp0
        { inputMousePos = V2 230 210
        , inputScroll = V2 0 1
        }
  _ <- runFrame ctx away ui
  offI1 <- getScrollOffset ctx inner
  when (offI1 <= offI0) $ bump failed



