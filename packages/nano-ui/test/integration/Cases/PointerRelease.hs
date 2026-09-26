-- | A click belongs to the widget its press went down on. Dragging off a
-- widget and letting go over a neighbour must fire nothing.
module Cases.PointerRelease (tests) where

import Spec
import Data.Maybe (isJust, isNothing)
import Data.Text qualified as T

tests :: [Spec]
tests =
  [ spec "release-elsewhere" runReleaseElsewhereTest
  , spec "right-release-elsewhere" runRightReleaseElsewhereTest
  , spec "release-returns" runReleaseReturnsTest
  , spec "overlap-press" runOverlapPressTest
  , spec "release-moved-radio" runReleaseMovedRadioTest
  , spec "release-moved-tab" runReleaseMovedTabTest
  ]

-- | Press a control, then release where it moved to on the same frame. The
-- view tests the release against the rect it had before the move and misses
-- it; the frame posts the click from the solved rect, asks for the next frame,
-- and the control reports it there.
releaseAfterMove ::
  Context -> IORef Int -> T.Text -> (Float -> NanoUI a) -> (a -> Bool) -> IO ()
releaseAfterMove ctx failed lbl ui picked = do
  let inp0 = withInput 320 240
      shift = 100
  _ <- warmup2 ctx inp0 (ui 0)
  spans <- collectTextSpans ctx
  assertJust failed (spanRectOf lbl spans) $ \r -> do
    let V2 px py = spanCenter r
        press = pressAt inp0 (V2 px py)
        release = applyMouseButton MouseLeft False inp0 {inputMousePos = V2 px (py + shift)}
    _ <- runFrame ctx press (ui 0)
    (missed, _, _, dirty) <- runFrame ctx release (ui shift)
    assert failed (not (picked missed))
    assert failed dirty
    (next, _, _, _) <- runFrame ctx inp0 {inputMousePos = V2 px (py + shift)} (ui shift)
    assert failed (picked next)

runReleaseMovedRadioTest :: Context -> IORef Int -> IO ()
runReleaseMovedRadioTest ctx failed = do
  sel <- newIORef (0 :: Int)
  releaseAfterMove ctx failed "two"
    ( \d -> column $ do
        columnWith (fixedH d . fillW) (pure ())
        snd <$> held sel (radio' ["one", "two", "three"])
    )
    (== 1)

runReleaseMovedTabTest :: Context -> IORef Int -> IO ()
runReleaseMovedTabTest ctx failed = do
  cur <- newIORef (0 :: Int)
  releaseAfterMove ctx failed "Beta"
    ( \d -> column $ do
        columnWith (fixedH d . fillW) (pure ())
        snd <$> held cur (\i -> (\r -> (r, tabActive r)) <$> tabBar' i [tab 0 "Alpha" (), tab 1 "Beta" ()])
    )
    (== 1)

-- | Press one widget, drag onto another, release: neither one fires, this
-- frame or the next.
runReleaseElsewhereTest :: Context -> IORef Int -> IO ()
runReleaseElsewhereTest ctx failed = do
  -- The checkbox drives its own state across frames, so a stray toggle sticks
  -- and the value assertions below have something to catch.
  flag <- newIORef False
  let inp0 = withInput 320 240
      ui = column $ do
        a <- button' "Alpha"
        b <- button' "Beta"
        (cb, on) <- held flag (checkbox' "Flag")
        pure (a, b, cb, on)

  (a, b, cb, _) <- warmup2 ctx inp0 ui
  let at r = inp0 {inputMousePos = centerOf r}
      pressOn r = pressAt inp0 (centerOf r)
      dragTo r p = holdAt p (centerOf r)
      releaseOn r p = releaseAt (dragTo r p)
  -- Button to button.
  _ <- runFrame ctx (pressOn a) ui
  ((_, bDrag, _, _), _, _, _) <- runFrame ctx (dragTo b (pressOn a)) ui
  assert failed (not (respHovered bDrag) && not (respPressed bDrag))
  ((aUp, bUp, _, _), _, _, _) <- runFrame ctx (releaseOn b (pressOn a)) ui
  assert failed (not (respClicked aUp) && not (respClicked bUp))
  ((aNext, bNext, _, _), _, _, _) <- runFrame ctx (at b) ui
  assert failed (not (respClicked aNext) && not (respClicked bNext))

  -- Button to checkbox: the checkbox must not toggle.
  _ <- runFrame ctx (pressOn a) ui
  ((_, _, cbUp, _), _, _, _) <- runFrame ctx (releaseOn cb (pressOn a)) ui
  assert failed (not (respClicked cbUp))
  ((_, _, _, checked), _, _, _) <- runFrame ctx (at cb) ui
  assert failed (not checked)

  -- Checkbox to button: neither fires, and the box stays clear.
  _ <- runFrame ctx (pressOn cb) ui
  ((_, bOver, cbOff, _), _, _, _) <- runFrame ctx (releaseOn b (pressOn cb)) ui
  assert failed (not (respClicked bOver) && not (respClicked cbOff))
  ((_, _, _, stillOff), _, _, _) <- runFrame ctx (at b) ui
  assert failed (not stillOff)

-- | The same rule for the right button: a context menu opens where the right
-- press went down, not where it came up. Also covers the container path, since
-- the menu area's response comes from a container node rather than a leaf.
runRightReleaseElsewhereTest :: Context -> IORef Int -> IO ()
runRightReleaseElsewhereTest ctx failed = do
  let inp0 = withInput 320 240
      ui = column $ do
        a <- button' "Alpha"
        (lbl, menu) <-
          contextMenuArea (fixedH 60 . fillW) (label' "Area") (const (menuItem "Cut"))
        pure (a, lbl, menu)
  (a, lbl, _) <- warmup2 ctx inp0 ui
  let rightPressOn r = fst (rightClickPair inp0 (centerOf r))
      rightReleaseOn r p = snd (rightClickPair p (centerOf r))
  -- Right press on the button, release over the menu area: no menu.
  _ <- runFrame ctx (rightPressOn a) ui
  ((aUp, _, menuUp), _, _, _) <- runFrame ctx (rightReleaseOn lbl (rightPressOn a)) ui
  assert failed (not (respRightClicked aUp))
  assert failed (isNothing menuUp)

  -- Right press and release inside the area: the menu opens.
  _ <- runFrame ctx (rightPressOn lbl) ui
  ((_, _, menuSame), _, _, _) <- runFrame ctx (rightReleaseOn lbl (rightPressOn lbl)) ui
  assert failed (isJust menuSame)

-- | Leaving a widget mid-press and coming back still clicks it, and a plain
-- press-release on one widget is unaffected.
runReleaseReturnsTest :: Context -> IORef Int -> IO ()
runReleaseReturnsTest ctx failed = do
  let inp0 = withInput 320 240
      ui = column $ do
        a <- button' "Alpha"
        b <- button' "Beta"
        pure (a, b)
  (a, b) <- warmup2 ctx inp0 ui
  let pressOn r = pressAt inp0 (centerOf r)
      moveTo r p = holdAt p (centerOf r)
      releaseOn r p = releaseAt (moveTo r p)
  -- Straight click.
  _ <- runFrame ctx (pressOn a) ui
  ((aUp, _), _, _, _) <- runFrame ctx (releaseOn a (pressOn a)) ui
  assert failed (respClicked aUp)

  -- Wander off and back before letting go.
  _ <- runFrame ctx (pressOn b) ui
  _ <- runFrame ctx (moveTo a (pressOn b)) ui
  ((_, bBack), _, _, _) <- runFrame ctx (releaseOn b (pressOn b)) ui
  assert failed (respClicked bBack)
  void (runFrame ctx inp0 ui)

-- | Where two widgets overlap, a held press belongs to the one hover lights
-- up: the earlier sibling, which paints on top.
runOverlapPressTest :: Context -> IORef Int -> IO ()
runOverlapPressTest ctx failed = do
  let inp0 = withInput 320 240
      ui = rowWith (gap (-30)) $ do
        a <- buttonWith' (fixedW 80) "Alpha"
        b <- buttonWith' (fixedW 80) "Beta"
        pure (a, b)
  (a, _) <- warmup2 ctx inp0 ui
  let Rect ax ay aw ah = respRect a
      overlap = inp0 {inputMousePos = V2 (ax + aw - 10) (ay + ah / 2)}
      press = applyMouseButton MouseLeft True overlap
      held' = overlap {inputButtonsHeld = buttonsFromList [MouseLeft]}
  _ <- runFrame ctx overlap ui
  hot <- getHotId ctx
  assert failed (hot == respId a)
  _ <- runFrame ctx press ui
  ((aHeld, bHeld), _, _, _) <- runFrame ctx held' ui
  assert failed (respPressed aHeld && not (respPressed bHeld))
  void (runFrame ctx (applyMouseButton MouseLeft False overlap) ui)
  void (runFrame ctx inp0 ui)
