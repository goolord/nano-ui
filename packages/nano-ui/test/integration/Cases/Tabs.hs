module Cases.Tabs
  ( runTabsClosableTest
  , runTabsDisabledTest
  , runTabsDamageTest
  , runTabsEmitTest
  , runTabsLazinessTest
  , runTabsScrollTest
  , runTabsStatePersistenceTest
  , runPanelBodySwapDamageTest
  , runTabResponseForwardingTest
  ) where

import Control.Monad (forM, forM_, replicateM)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Sequence qualified as Seq
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, run2Frames, withInput)
import NanoUI.Testing.Harness
  ( assertSpansHas
  , clickPair
  , drawQuads
  , hasText
  , runClick
  , spanCenter
  , warmup2
  , withInputOff
  )
import NanoUI.Context (Context (..))
import NanoUI.Emit qualified as Emit
import NanoUI.Layout.Arena (arenaCount, findNodeM, getRect, getText, getWidgetId)

data DummyTab = TabA | TabB | TabC
  deriving (Eq, Show)

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
runPanelBodySwapDamageTest ctx failed = do
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
      ui = tabs TabB $ Seq.fromList
        [ tab TabA "A" (uiIO (modifyIORef' evalCountA (+ 1)) >> label "Body A")
        , tab TabB "B" (uiIO (modifyIORef' evalCountB (+ 1)) >> label "Body B")
        , tab TabC "C" (uiIO (modifyIORef' evalCountC (+ 1)) >> label "Body C")
        ]
  _ <- runFrame ctx inp ui
  cntA <- readIORef evalCountA
  cntB <- readIORef evalCountB
  cntC <- readIORef evalCountC
  assertEq failed cntA 0
  assertEq failed cntB 1
  assertEq failed cntC 0

data TabMsg = MsgSelect DummyTab | MsgClose DummyTab
  deriving (Eq, Show)

runTabsEmitTest :: Context -> IORef Int -> IO ()
runTabsEmitTest ctx failed = do
  let inp0 = withInput 300 100
      ui curTab = Emit.tabs curTab
        [ tab TabA "Alpha" (label "Body A")
        , tab TabB "Beta" (label "Body B")
        ]
        MsgSelect
  _ <- runFrame ctx inp0 (ui TabA)
  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, "Beta" `T.isInfixOf` txt] of
    (r : _) -> do
      let (press, release) = clickPair inp0 (spanCenter r)
      _ <- runFrame ctx press (ui TabA)
      (_, msgs, _, _) <- runFrame ctx release (ui TabA)
      assertEq failed (decodeMessages msgs :: [TabMsg]) [MsgSelect TabB]
    [] -> assert failed False

-- Composite responses expose every flag of their widget response
-- (regression: TabResponse dropped respSubmitted).
runTabResponseForwardingTest :: Context -> IORef Int -> IO ()
runTabResponseForwardingTest _ failed = do
  let inner = mempty {rawRespSubmitted = True, rawRespRightPressed = True, rawRespChanged = True}
      tabResp = TabResponse inner Nothing TabA
      tableResp = TableResponse inner (SortCol 0 SortAsc) [] mempty
  assert failed (respSubmitted tabResp && respRightPressed tabResp && respChanged tabResp)
  assert failed (respSubmitted tableResp && respRightPressed tableResp && respChanged tableResp)

runTabsClosableTest :: Context -> IORef Int -> IO ()
runTabsClosableTest ctx failed = do
  let inp0 = withInput 300 100
      ui curTab = tabs' curTab
        [ closableTab TabA "Alpha" (label "Body A")
        , closableTab TabB "Beta" (label "Body B")
        ]
  _ <- runFrame ctx inp0 (ui TabA)
  mClose <- findCloseButtonRect ctx
  case mClose of
    Just r -> do
      tResp <- runClick ctx inp0 (ui TabA) (spanCenter r)
      assertEq failed (tabClosed tResp) (Just TabA)
      assertEq failed (tabActive tResp) TabA
    Nothing -> assert failed False

findCloseButtonRect :: Context -> IO (Maybe Rect)
findCloseButtonRect ctx = do
  let na = ctxNodeArena ctx
  found <- findNodeM na (fmap ("\215" `T.isInfixOf`) . getText na)
  forM found $ \i -> do
    (x, y, w, h) <- getRect na i
    pure (Rect x y w h)

-- The public disabled flag covers both the header and its close control,
-- including retained keyboard focus when an enabled tab becomes disabled.
runTabsDisabledTest :: Context -> IORef Int -> IO ()
runTabsDisabledTest _ failed = forM_ [TabTop, TabLeft] $ \orientation -> do
  ctx <- newContext
  let inp = withInputOff 400 240
      ui disabled = tabsConfigured' defaultTabsConfig {tabsOrientation = orientation} TabA
        [ (closableTab TabB "Disabled" (label "Body B")) {tabDisabled = disabled}
        , tab TabA "Enabled" (label "Body A")
        ]
      check response = do
        assertEq failed (tabActive response) TabA
        assertEq failed (tabClosed response) Nothing
        assert failed (not (respClicked response) && not (respChanged response))
  _ <- warmup2 ctx inp (ui False)
  let arena = ctxNodeArena ctx
  n <- arenaCount arena
  headers <- mapM (\i -> (,) <$> getText arena i <*> getWidgetId arena i) [0 .. n - 1]
  _ <- warmup2 ctx inp (ui True)
  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, txt == "Disabled"] of
    r : _ -> check =<< runClick ctx inp (ui True) (spanCenter r)
    [] -> assert failed False
  closeRect <- findCloseButtonRect ctx
  case closeRect of
    Just r -> check =<< runClick ctx inp (ui True) (spanCenter r)
    Nothing -> assert failed False
  forM_ [wid | (txt, wid) <- headers, txt == "Disabled" || txt == "\215"] $ \wid -> do
    writeIORef (ctxFocusId ctx) wid
    (result, _, _, _) <- runFrame ctx (inp {inputKeys = inputKeysFromList [KeyEnter]}) (ui True)
    check result
  -- Re-enabling the same header preserves its identity and restores activation.
  _ <- warmup2 ctx inp (ui False)
  spansEnabled <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spansEnabled, txt == "Disabled"] of
    r : _ -> do
      response <- runClick ctx inp (ui False) (spanCenter r)
      assertEq failed (tabActive response) TabB
    [] -> assert failed False

runTabsStatePersistenceTest :: Context -> IORef Int -> IO ()
runTabsStatePersistenceTest ctx failed = do
  let inp0 = withInput 300 100
      ui curTab = tabs curTab
        [ tab TabA "A" $
            withKey ("tab-a" :: T.Text) $
              withKey ("flag" :: T.Text) $ do
                (flag, setFlag) <- useFlag False
                whenM (button "ToggleA") (setFlag (not flag))
                label (if flag then "FlagIsOn" else "FlagIsOff")
        , tab TabB "B" (label "OtherTab")
        ]
  _ <- runFrame ctx inp0 (ui TabA)
  spans0 <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans0, "ToggleA" `T.isInfixOf` txt] of
    (r : _) -> do
      _ <- runClick ctx inp0 (ui TabA) (spanCenter r)
      _ <- runFrame ctx inp0 (ui TabA)
      spans1 <- collectTextSpans ctx
      assertSpansHas failed "FlagIsOn" spans1

      _ <- runFrame ctx inp0 (ui TabB)
      spans2 <- collectTextSpans ctx
      assertSpansHas failed "OtherTab" spans2
      assert failed (not (hasText "FlagIsOn" spans2))

      _ <- runFrame ctx inp0 (ui TabA)
      spans3 <- collectTextSpans ctx
      assertSpansHas failed "FlagIsOn" spans3
    [] -> assert failed False

runTabsDamageTest :: Context -> IORef Int -> IO ()
runTabsDamageTest ctx failed = do
  let inp0 = withInputOff 300 100
      ui curTab = tabs' curTab
        [ tab TabA "Alpha" (label "Body A with some text")
        , tab TabB "Beta" (label "Body B different widgets")
        ]
      covers dmg (Rect rx ry rw rh) = case dmg of
        DamageFull -> True
        DamageClip (Rect dx dy dw dh) -> rx >= dx && ry >= dy && rx + rw <= dx + dw && ry + rh <= dy + dh
  _ <- runFrame ctx inp0 (ui TabA)
  _ <- takeDamage ctx
  _ <- runFrame ctx inp0 (ui TabA)
  dIdle <- takeDamage ctx
  assert failed (dIdle /= DamageFull)
  spansIdle <- collectTextSpans ctx
  assertSpansHas failed "Body A" spansIdle

  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, "Beta" `T.isInfixOf` txt] of
    (beta : _) -> do
      let (press, release) = clickPair inp0 (spanCenter beta)
      _ <- runFrame ctx press (ui TabA)
      (resp, _, _, _) <- runFrame ctx release (ui TabA)
      assert failed (respChanged resp && tabActive resp == TabB)
      spansSwitch <- collectTextSpans ctx
      assertSpansHas failed "Body B" spansSwitch
      assert failed (not (hasText "Body A" spansSwitch))
      let bodyB = [r | (r, txt, _, _, _) <- spansSwitch, "Body B" `T.isInfixOf` txt]
      dSwitch <- takeDamage ctx
      assert failed (not (null bodyB) && all (covers dSwitch) bodyB)

      _ <- runFrame ctx inp0 (ui TabB)
      dTabB <- takeDamage ctx
      assert failed (all (covers dTabB) bodyB)

      _ <- runFrame ctx inp0 (ui TabB)
      dSettled <- takeDamage ctx
      assert failed (dSettled /= DamageFull)
    [] -> assert failed False

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
  forM_ labels $ \l -> assert failed (hasText l wideSpans)
  assert failed (not (T.any (`elem` chevrons) (T.concat [t | (_, t, _, _, _) <- wideSpans])))

  ctx <- newContext
  let inp = withInput 240 120
  _ <- runFrame ctx inp (mkTabs 0)
  _ <- runFrame ctx inp (mkTabs 0)
  -- The strip only pulls in its scroller once it has measured an overflow, so
  -- the arrow buttons show from the third frame on.
  _ <- runFrame ctx inp (mkTabs 0)
  spans0 <- collectTextSpans ctx
  assert failed (hasText "Controls" spans0)
  assert failed (not (hasText "LongestTabName" spans0))

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
      inputSurface = themeInput theme
      forbidden =
        [ styleBg inputSurface
        , styleBorder inputSurface
        , scrollBarTrackColor inputSurface theme
        , scrollBarThumbColor inputSurface theme
        ]
      inBar = [(r, c) | (r, c) <- quads, isJust (rectIntersect r bar), c `elem` forbidden]
  assert failed (null inBar)

  -- Left and right buttons page the strip; the right arrow is pinned to the
  -- bar's far edge rather than trailing the last visible tab.
  mRight <- arrowRect ctx '\8250'
  case mRight of
    (r : _) -> do
      assert failed (rectX r + rectW r > 200)
      _ <- runClick ctx inp (mkTabs 0) (spanCenter r)
      _ <- runFrame ctx inp (mkTabs 0)
      spans1 <- collectTextSpans ctx
      assert failed (not (hasText "Controls" spans1))
      mLeft <- arrowRect ctx '\8249'
      case mLeft of
        (left : _) -> do
          _ <- runClick ctx inp (mkTabs 0) (spanCenter left)
          _ <- runFrame ctx inp (mkTabs 0)
          spans2 <- collectTextSpans ctx
          assert failed (hasText "Controls" spans2)
        _ -> assert failed False
    _ -> assert failed False

  -- Wheel up/down over the bar pages the window too (the strip maps the
  -- notches onto the horizontal offset).
  spans3 <- collectTextSpans ctx
  case [r | (r, t, _, _, _) <- spans3, "Controls" `T.isInfixOf` t] of
    (Rect cx cy cw ch : _) -> do
      let wheelDown = inp {inputMousePos = spanCenter (Rect cx cy cw ch), inputScroll = V2 0 20}
      _ <- runFrame ctx wheelDown (mkTabs 0)
      _ <- runFrame ctx inp (mkTabs 0)
      spans4 <- collectTextSpans ctx
      assert failed (not (hasText "Controls" spans4))

      -- Left+right wheel over the bar scrolls the same offset through the
      -- framework scroller. The vertical wheel pinned the offset at max, so
      -- wheeling right stays put (clamped at the end); wheeling left runs
      -- back to the start and re-shows the first tab, and further left
      -- notches clamp at zero instead of running past it. The deltas are
      -- coupled to the framework wheel step (scrollLineFor, 20px per notch
      -- on window hosts): V2 0 20 saturates at max, and +/-100 notches
      -- crosses the whole range regardless of the exact step.
      let wheelX d = inp {inputMousePos = spanCenter (Rect cx cy cw ch), inputScroll = V2 d 0}
      _ <- runFrame ctx (wheelX 10) (mkTabs 0)
      _ <- runFrame ctx inp (mkTabs 0)
      spans5 <- collectTextSpans ctx
      assert failed (not (hasText "Controls" spans5))
      _ <- runFrame ctx (wheelX (-100)) (mkTabs 0)
      _ <- runFrame ctx inp (mkTabs 0)
      spans6 <- collectTextSpans ctx
      assert failed (hasText "Controls" spans6)
      _ <- runFrame ctx (wheelX (-100)) (mkTabs 0)
      _ <- runFrame ctx inp (mkTabs 0)
      spans7 <- collectTextSpans ctx
      assert failed (hasText "Controls" spans7)
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
  forM_ shortLabels $ \l -> assert failed (hasText l spansS)
  mRightS <- arrowRect ctx '\8250'
  mLeftS <- arrowRect ctx '\8249'
  assert failed (null mRightS && null mLeftS)
  _ <- runFrame ctx inp (mkTabs 0)
  _ <- runFrame ctx inp (mkTabs 0)
  _ <- runFrame ctx inp (mkTabs 0)
  spansG <- collectTextSpans ctx
  mRightG <- arrowRect ctx '\8250'
  assert failed (not (null mRightG))
  assert failed (not (hasText "LongestTabName" spansG))
