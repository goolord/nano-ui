module Cases.Tabs
  ( runTabsClosableTest
  , runTabsContentDamageTest
  , runTabsDamageTest
  , runTabsEmitTest
  , runTabsInPanelDamageTest
  , runTabsInteractionTest
  , runTabsLazinessTest
  , runTabsScrollTest
  , runTabsStatePersistenceTest
  , runPanelBodySwapDamageTest
  ) where

import Control.Monad (forM_, replicateM)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, run2Frames, withInput)
import NanoUI.Testing.Harness
  ( assertSpansHas
  , centerOf
  , clickPair
  , drawQuads
  , runClick
  , runClickPair
  , spansHas
  , withInputOff
  , warmup2
  )
import NanoUI.Context (Context (..))
import NanoUI.Layout.Arena (arenaCount, getRect, getText)

data DummyTab = TabA | TabB | TabC
  deriving (Eq, Show)

-- Ghosting guard: switching tabs inside a floating window must leave no
-- stale pixel behind. The mirror store write escalates the switch frame to
-- DamageFull today; if a refactor ever narrows it to a clip (layout-driven
-- churn inside panels suppresses keysChanged), diffNew renders the new
-- body's rects, so the clip still covers every row of the new tab.
runTabsInPanelDamageTest :: Context -> IORef Int -> IO ()
runTabsInPanelDamageTest _ failed = do
  ctx <- newContext
  let inp0 = withInput 400 300
      ui cur = window True "TabWin" (columnWith (fixedH 300) (tabs cur
        [ tab TabA "Alpha" (label_ "WIDE BODY ROW ONE")
        , tab TabB "Beta" (column (replicateM 5 (label "row line") >> pure ()))
        ]))
  _ <- warmup2 ctx inp0 (ui TabA)
  _ <- runFrame ctx inp0 (ui TabA)
  _ <- takeDamage ctx
  let spansBody txt = do
        allSpans <- collectOverlayTextSpans ctx inp0
        pure [(r, t) | (r, t, _, _, _) <- allSpans, txt `T.isInfixOf` t]
  spansA <- spansBody "WIDE BODY"
  assert failed (length spansA == 1)
  betaSpans <- collectOverlayTextSpans ctx inp0
  case [r | (r, t, _, _, _) <- betaSpans, "Beta" `T.isInfixOf` t] of
    (Rect bx by bw bh : _) -> do
      let (press, release) = clickPair inp0 (V2 (bx + bw / 2) (by + bh / 2))
      _ <- runFrame ctx press (ui TabA)
      ((_, mtab), _, _, _) <- runFrame ctx release (ui TabA)
      case mtab of
        Nothing -> assert failed False
        Just (_, nTab) -> do
          assert failed (nTab == TabB)
          dmg <- takeDamage ctx
          assert failed (not (damageIsEmpty dmg))
          let dmgR = case dmg of
                DamageFull -> fullWindowRect inp0
                DamageClip r -> r
          spansB <- spansBody "row line"
          assert failed (length spansB == 5)
          let Rect ddx ddy ddw ddh = dmgR
          forM_ spansB $ \(Rect rx ry rw rh, _) -> do
            assert failed (rx >= ddx && ry >= ddy && rx + rw <= ddx + ddw && ry + rh <= ddy + ddh)
          spansA2 <- spansBody "WIDE BODY"
          assert failed (null spansA2)
    _ -> assert failed False

fullWindowRect :: Input -> Rect
fullWindowRect inp =
  let Size w h = inputWindowSize inp
   in Rect 0 0 w h

-- Content replacement inside a floating window must repaint every pixel of
-- the new body: a clip that skipped any incoming row would leave the pane
-- rendering stale pixels from the previous body ("ghosting"). The stable
-- content slot and the incoming key rects (diffNew) must together cover all
-- five rows.
runPanelBodySwapDamageTest :: Context -> IORef Int -> IO ()
runPanelBodySwapDamageTest _ failed = do
  ctx <- newContext
  let inp0 = withInput 400 300
      uiA = window True "TabWin" (columnWith (fixedH 300) (label "WIDE BODY ROW ONE"))
      uiB = window True "TabWin" (columnWith (fixedH 300) (column (replicateM 5 (label "row line") >> pure ())))
  _ <- warmup2 ctx inp0 uiA
  _ <- runFrame ctx inp0 uiA
  _ <- takeDamage ctx
  _ <- runFrame ctx inp0 uiB
  dmg <- takeDamage ctx
  assert failed (not (damageIsEmpty dmg))
  let dmgR = case dmg of
        DamageFull -> fullWindowRect inp0
        DamageClip r -> r
      Rect ddx ddy ddw ddh = dmgR
  spans <- collectOverlayTextSpans ctx inp0
  let rows = [(r, t) | (r, t, _, _, _) <- spans, "row line" `T.isInfixOf` t]
  assert failed (length rows == 5)
  forM_ rows $ \(Rect rx ry rw rh, _) -> do
    assert failed (rx >= ddx && ry >= ddy && rx + rw <= ddx + ddw && ry + rh <= ddy + ddh)

runTabsLazinessTest :: Context -> IORef Int -> IO ()
runTabsLazinessTest ctx failed = do
  evalCountA <- newIORef (0 :: Int)
  evalCountB <- newIORef (0 :: Int)
  evalCountC <- newIORef (0 :: Int)
  let inp = withInput 200 100
      ui = tabs TabB
        [ tab TabA "A" (uiIO (modifyIORef' evalCountA (+ 1)) >> label_ "Body A")
        , tab TabB "B" (uiIO (modifyIORef' evalCountB (+ 1)) >> label_ "Body B")
        , tab TabC "C" (uiIO (modifyIORef' evalCountC (+ 1)) >> label_ "Body C")
        ]
  _ <- runFrame ctx inp ui
  cntA <- readIORef evalCountA
  cntB <- readIORef evalCountB
  cntC <- readIORef evalCountC
  assertEq failed cntA 0
  assertEq failed cntB 1
  assertEq failed cntC 0

runTabsInteractionTest :: Context -> IORef Int -> IO ()
runTabsInteractionTest ctx failed = do
  let inp0 = withInput 300 100
      ui curTab = tabs curTab
        [ tab TabA "Alpha" (label_ "Body A")
        , tab TabB "Beta" (label_ "Body B")
        ]
  ((_, active0), _, _, _) <- runFrame ctx inp0 (ui TabA)
  assertEq failed active0 TabA
  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, "Beta" `T.isInfixOf` txt] of
    (Rect bx by bw bh : _) -> do
      (resp1, active1) <- runClickPair ctx inp0 (ui TabA) (V2 (bx + bw / 2) (by + bh / 2))
      assert failed (respChanged resp1 && active1 == TabB)
    [] -> assert failed False

data TabMsg = MsgSelect DummyTab | MsgClose DummyTab
  deriving (Eq, Show)

runTabsEmitTest :: Context -> IORef Int -> IO ()
runTabsEmitTest ctx failed = do
  let inp0 = withInput 300 100
      ui curTab = tabsEmit MsgSelect curTab
        [ tab TabA "Alpha" (label_ "Body A")
        , tab TabB "Beta" (label_ "Body B")
        ]
  _ <- runFrame ctx inp0 (ui TabA)
  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, "Beta" `T.isInfixOf` txt] of
    (Rect bx by bw bh : _) -> do
      let (press, release) = clickPair inp0 (V2 (bx + bw / 2) (by + bh / 2))
      _ <- runFrame ctx press (ui TabA)
      (_, msgs, _, _) <- runFrame ctx release (ui TabA)
      assertEq failed (decodeMessages msgs :: [TabMsg]) [MsgSelect TabB]
    [] -> assert failed False

runTabsClosableTest :: Context -> IORef Int -> IO ()
runTabsClosableTest ctx failed = do
  let inp0 = withInput 300 100
      ui curTab = tabsEx TabUnderline TabTop curTab
        [ closableTab TabA "Alpha" (label_ "Body A")
        , closableTab TabB "Beta" (label_ "Body B")
        ]
  _ <- runFrame ctx inp0 (ui TabA)
  mClose <- findCloseButtonRect ctx
  case mClose of
    Just (Rect cx cy cw ch) -> do
      (tResp, activeTab) <- runClickPair ctx inp0 (ui TabA) (V2 (cx + cw / 2) (cy + ch / 2))
      assertEq failed (tabClosed tResp) (Just TabA)
      assertEq failed activeTab TabA
    Nothing -> assert failed False

findCloseButtonRect :: Context -> IO (Maybe Rect)
findCloseButtonRect ctx = do
  let na = ctxNodeArena ctx
  n <- arenaCount na
  let go i
        | i >= n = pure Nothing
        | otherwise = do
            txt <- getText na i
            if "\215" `T.isInfixOf` txt
              then do
                (x, y, w, h) <- getRect na i
                pure (Just (Rect x y w h))
              else go (i + 1)
  go 0

runTabsStatePersistenceTest :: Context -> IORef Int -> IO ()
runTabsStatePersistenceTest ctx failed = do
  let inp0 = withInput 300 100
      ui curTab = tabs curTab
        [ tab TabA "A" $
            withKey ("tab-a" :: T.Text) $
              withKey ("flag" :: T.Text) $ do
                (flag, setFlag) <- useFlag False
                whenM (button "ToggleA") (setFlag (not flag))
                label_ (if flag then "FlagIsOn" else "FlagIsOff")
        , tab TabB "B" (label_ "OtherTab")
        ]
  _ <- runFrame ctx inp0 (ui TabA)
  spans0 <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans0, "ToggleA" `T.isInfixOf` txt] of
    (Rect cx cy cw ch : _) -> do
      runClick ctx inp0 (ui TabA) (V2 (cx + cw / 2) (cy + ch / 2))
      _ <- runFrame ctx inp0 (ui TabA)
      spans1 <- collectTextSpans ctx
      assertSpansHas failed "FlagIsOn" spans1

      _ <- runFrame ctx inp0 (ui TabB)
      spans2 <- collectTextSpans ctx
      assertSpansHas failed "OtherTab" spans2
      assert failed (not (spansHas "FlagIsOn" spans2))

      _ <- runFrame ctx inp0 (ui TabA)
      spans3 <- collectTextSpans ctx
      assertSpansHas failed "FlagIsOn" spans3
    [] -> assert failed False

runTabsDamageTest :: Context -> IORef Int -> IO ()
runTabsDamageTest _ failed = do
  ctx <- newContext
  let inp0 = withInputOff 300 100
      ui curTab = tabs curTab
        [ tab TabA "Alpha" (label_ "Body A with some text")
        , tab TabB "Beta" (label_ "Body B different widgets")
        ]
  _ <- runFrame ctx inp0 (ui TabA)
  _ <- takeDamage ctx
  _ <- runFrame ctx inp0 (ui TabA)
  dIdle <- takeDamage ctx
  assert failed (dIdle /= DamageFull)
  spansIdle <- collectTextSpans ctx
  assertSpansHas failed "Body A" spansIdle

  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, "Beta" `T.isInfixOf` txt] of
    (Rect bx by bw bh : _) -> do
      let (press, release) = clickPair inp0 (V2 (bx + bw / 2) (by + bh / 2))
      _ <- runFrame ctx press (ui TabA)
      ((resp, newTab), _, _, _) <- runFrame ctx release (ui TabA)
      assert failed (respChanged resp && newTab == TabB)
      spansSwitch <- collectTextSpans ctx
      assertSpansHas failed "Body B" spansSwitch
      assert failed (not (spansHas "Body A" spansSwitch))
      dSwitch <- takeDamage ctx
      assertEq failed dSwitch DamageFull

      _ <- runFrame ctx inp0 (ui TabB)
      dTabB <- takeDamage ctx
      assertEq failed dTabB DamageFull

      _ <- runFrame ctx inp0 (ui TabB)
      dSettled <- takeDamage ctx
      assert failed (dSettled /= DamageFull)
    [] -> assert failed False

runTabsContentDamageTest :: Context -> IORef Int -> IO ()
runTabsContentDamageTest _ failed = do
  ctx <- newContext
  let inp0 = withInputOff 320 200
      ui = do
        (click, setClick) <- useText ""
        row $ do
          btn <- button' "OK"
          onClick btn (setClick "OK")
          _ <- tabs ("Controls" :: T.Text)
            [ tab "Controls" "Controls" $
                kv "Clicked" (if T.null click then "-" else click)
            ]
          pure btn
  _ <- runFrame ctx inp0 ui
  (btn, _, _, _) <- runFrame ctx inp0 ui
  let (press, release) = clickPair inp0 (centerOf btn)
  _ <- runFrame ctx press ui
  _ <- runFrame ctx release ui
  spans1 <- collectTextSpans ctx
  assertSpansHas failed "OK" spans1
  _ <- runFrame ctx inp0 ui
  spans2 <- collectTextSpans ctx
  assertSpansHas failed "OK" spans2

-- Too-wide tab strips scroll instead of overflowing. The framework scroll
-- container owns the clip, the offset and the damage; the strip adds
-- chevron buttons on the left and right and pages the same offset with
-- them. Acceptance: scrolling left and right works (buttons, up/down wheel
-- notches mapped onto the horizontal offset) as well as left+right
-- (horizontal wheel, applied by the framework scroller itself); the tab
-- headers look exactly as they did before the strip could scroll (no
-- scroller well behind them); and no scrollbar appears.
runTabsScrollTest :: Context -> IORef Int -> IO ()
runTabsScrollTest _ failed = do
  let labels = ["Controls", "Graphics", "Typography", "Diagnostics", "LongestTabName"]
      mkTabs cur = tabBar cur [tab (i :: Int) l () | (i, l) <- zip [0 ..] labels]
      arrowRect ctx ch = do
        spans <- collectTextSpans ctx
        pure [r | (r, t, _, _, _) <- spans, T.any (== ch) t]
      chevrons = ['\8250', '\8249']

  -- Wide bar: everything fits and no arrows are drawn.
  wide <- newContext
  let wideInp = withInput 900 120
  _ <- runFrame wide wideInp (mkTabs 0)
  _ <- runFrame wide wideInp (mkTabs 0)
  wideSpans <- collectTextSpans wide
  forM_ labels $ \l -> assert failed (spansHas l wideSpans)
  assert failed (not (T.any (`elem` chevrons) (T.concat [t | (_, t, _, _, _) <- wideSpans])))

  ctx <- newContext
  let inp = withInput 240 120
  _ <- runFrame ctx inp (mkTabs 0)
  _ <- runFrame ctx inp (mkTabs 0)
  -- The strip only pulls in its scroller once it has measured an overflow, so
  -- the arrow buttons show from the third frame on.
  _ <- runFrame ctx inp (mkTabs 0)
  spans0 <- collectTextSpans ctx
  assert failed (spansHas "Controls" spans0)
  assert failed (not (spansHas "LongestTabName" spans0))

  -- No scroller well: while the strip is scrollable it must not paint the
  -- input background, the input border, or any scrollbar track or thumb
  -- behind the headers. Only the tab buttons themselves (and the arrows)
  -- may paint in the bar. The checked region is the full window width and
  -- the bar's height (header 28 + 4 slack + 2 slop), derived from the input
  -- so a resize of the test window cannot silently shrink coverage.
  theme <- readIORef (ctxTheme ctx)
  (_, _, dd, _) <- run2Frames ctx inp (mkTabs 0)
  quads <- drawQuads dd
  let Size winW _ = inputWindowSize inp
      bar = Rect 0 0 winW 34
      inputStyle = themeInput theme
      forbidden =
        [ styleBg inputStyle
        , styleBorder inputStyle
        , scrollBarTrackColor inputStyle theme False
        , scrollBarThumbColor inputStyle theme False
        ]
      inBar = [(r, c) | (r, c) <- quads, isJust (rectIntersect r bar), c `elem` forbidden]
  assert failed (null inBar)

  -- Left and right buttons page the strip; the right arrow is pinned to the
  -- bar's far edge rather than trailing the last visible tab.
  mRight <- arrowRect ctx '\8250'
  case mRight of
    (Rect ax ay aw ah : _) -> do
      assert failed (ax + aw > 200)
      runClick ctx inp (mkTabs 0) (V2 (ax + aw / 2) (ay + ah / 2))
      _ <- runFrame ctx inp (mkTabs 0)
      spans1 <- collectTextSpans ctx
      assert failed (not (spansHas "Controls" spans1))
      mLeft <- arrowRect ctx '\8249'
      case mLeft of
        (Rect lx ly lw lh : _) -> do
          runClick ctx inp (mkTabs 0) (V2 (lx + lw / 2) (ly + lh / 2))
          _ <- runFrame ctx inp (mkTabs 0)
          spans2 <- collectTextSpans ctx
          assert failed (spansHas "Controls" spans2)
        _ -> assert failed False
    _ -> assert failed False

  -- Wheel up/down over the bar pages the window too (the strip maps the
  -- notches onto the horizontal offset).
  spans3 <- collectTextSpans ctx
  case [r | (r, t, _, _, _) <- spans3, "Controls" `T.isInfixOf` t] of
    (Rect cx cy cw ch : _) -> do
      let wheelDown = inp {inputMousePos = V2 (cx + cw / 2) (cy + ch / 2), inputScroll = V2 0 20}
      _ <- runFrame ctx wheelDown (mkTabs 0)
      _ <- runFrame ctx inp (mkTabs 0)
      spans4 <- collectTextSpans ctx
      assert failed (not (spansHas "Controls" spans4))

      -- Left+right wheel over the bar scrolls the same offset through the
      -- framework scroller. The vertical wheel pinned the offset at max, so
      -- wheeling right stays put (clamped at the end); wheeling left runs
      -- back to the start and re-shows the first tab, and further left
      -- notches clamp at zero instead of running past it. The deltas are
      -- coupled to the framework wheel step (scrollLineFor, 20px per notch
      -- on window hosts): V2 0 20 saturates at max, and +/-100 notches
      -- crosses the whole range regardless of the exact step.
      let wheelX d = inp {inputMousePos = V2 (cx + cw / 2) (cy + ch / 2), inputScroll = V2 d 0}
      _ <- runFrame ctx (wheelX 10) (mkTabs 0)
      _ <- runFrame ctx inp (mkTabs 0)
      spans5 <- collectTextSpans ctx
      assert failed (not (spansHas "Controls" spans5))
      _ <- runFrame ctx (wheelX (-100)) (mkTabs 0)
      _ <- runFrame ctx inp (mkTabs 0)
      spans6 <- collectTextSpans ctx
      assert failed (spansHas "Controls" spans6)
      _ <- runFrame ctx (wheelX (-100)) (mkTabs 0)
      _ <- runFrame ctx inp (mkTabs 0)
      spans7 <- collectTextSpans ctx
      assert failed (spansHas "Controls" spans7)
    _ -> assert failed False

  -- Tab-list changes recompute the reachable range: shrinking back under
  -- the width drops the scroller and both arrows; growing past it again
  -- re-engages them and re-clips the tail. (One header fits the 240-wide
  -- window; two would still overflow the ~180px viewport between arrows.)
  let shortLabels = ["Controls"]
      mkShort cur = tabBar cur [tab (i :: Int) l () | (i, l) <- zip [0 ..] shortLabels]
  _ <- runFrame ctx inp (mkShort 0)
  _ <- runFrame ctx inp (mkShort 0)
  _ <- runFrame ctx inp (mkShort 0)
  spansS <- collectTextSpans ctx
  forM_ shortLabels $ \l -> assert failed (spansHas l spansS)
  mRightS <- arrowRect ctx '\8250'
  mLeftS <- arrowRect ctx '\8249'
  assert failed (null mRightS && null mLeftS)
  _ <- runFrame ctx inp (mkTabs 0)
  _ <- runFrame ctx inp (mkTabs 0)
  _ <- runFrame ctx inp (mkTabs 0)
  spansG <- collectTextSpans ctx
  mRightG <- arrowRect ctx '\8250'
  assert failed (not (null mRightG))
  assert failed (not (spansHas "LongestTabName" spansG))
