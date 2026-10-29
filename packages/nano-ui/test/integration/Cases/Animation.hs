module Cases.Animation (tests) where

import Spec
import Control.Concurrent (threadDelay)
import Data.Primitive.SmallArray (smallArrayFromList)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)

tests :: [Spec]
tests =
  [ spec "animation-values" runAnimationValuesTest
  , spec "animate-to-stays-settled" runAnimateToStaysSettledTest
  , spec "animation-damage" runAnimationDamageTest
  , spec "composite-animation-isolation" runCompositeAnimationIsolationTest
  , spec "button-hover-anim" runButtonHoverAnimTest
  , spec "spinner" runSpinnerTest
  , spec "animation-left-out" runAnimationLeftOutTest
  , spec "keep-animating-rest" runKeepAnimatingRestTest
  , spec "drawing-animation-end" runDrawingAnimationEndTest
  , spec "wake-after" runWakeAfterTest
  ]

-- | Tweens, delays and springs reach their values and let the loop sleep. A
-- fast spring at a low frame rate stays in range (a fixed 1/30 s step sent
-- it to NaN), and one toward 1500 lands (integrating its absolute position
-- froze it short, running for good). A retargeted spring carries on.
runAnimationValuesTest :: Context -> IORef Int -> IO ()
runAnimationValuesTest ctx failed = do
  let near tol want v = abs (v - want) <= tol
      frames n dt = replicateM_ n (runFrame ctx (withDelta 200 100 dt) (label "anim"))
      value = getAnimationValue ctx
  forM_ [(presetBouncy {springMass = 0.05}, 60), (SpringParams 2000 150 1, 30)] $ \(params, hz) -> do
    setAnimationValue ctx (WidgetId 404) 0
    startSpring ctx (WidgetId 404) params 1
    vals <- replicateM 120 (frames 1 (1 / hz) >> value (WidgetId 404))
    assert failed (all (\v -> v >= -0.5 && v <= 1.5) vals)
    assert failed . not =<< anyAnimating ctx
    assertEq failed 1 (last vals)
  let far = animateTo (Spring presetSmooth) 1500 <* label "far"
  replicateM_ 600 (runFrame ctx (withDelta 200 100 (1 / 60)) far)
  assert failed . not =<< anyAnimating ctx
  assertEq failed 1500 =<< evalUi ctx (withDelta 200 100 (1 / 60)) far
  let inp = withDelta 200 100 0.1
  startAnimation ctx (WidgetId 99) 0 1 0.25
  assert failed =<< needsRedraw ctx inp inp
  frames 4 0.1
  assert failed . near 0.01 1 =<< value (WidgetId 99)
  assert failed . not =<< anyAnimating ctx
  assert failed . not =<< needsRedraw ctx inp inp
  assert failed . not . (\(_, _, dirty) -> dirty) =<< runFrame ctx inp (label "anim")
  startAnimationEaseDelay ctx (WidgetId 202) 0 1 0.2 EaseLinear 0.15
  frames 1 0.1
  assert failed . near 0.01 0 =<< value (WidgetId 202)
  assert failed =<< anyAnimating ctx
  frames 1 0.1
  assert failed . near 0.03 0.25 =<< value (WidgetId 202)
  let staggered = do
        _ <- withKey ("lead" :: String) (animateTo (Tween EaseLinear 0.4 0) 1)
        withKey ("trail" :: String) (animateTo (Tween EaseLinear 0.4 0.08) 1) <* label "trail"
  replicateM_ 2 (runFrame ctx (withDelta 200 100 0.02) staggered)
  assert failed . (<= 0.01) =<< evalUi ctx (withDelta 200 100 0.02) staggered
  replicateM_ 9 (runFrame ctx (withDelta 200 100 0.02) staggered)
  assert failed . (>= 0.15) =<< evalUi ctx (withDelta 200 100 0.02) staggered
  startSpring ctx (WidgetId 402) presetBouncy 1
  frames 5 0.02
  v1 <- value (WidgetId 402)
  assert failed (v1 >= 0.02 && v1 <= 0.98)
  startSpring ctx (WidgetId 402) presetBouncy 0
  assert failed . near 0.02 v1 =<< value (WidgetId 402)
  assert failed =<< anyAnimating ctx
  startSpring ctx (WidgetId 403) presetStiff 1
  frames 1 2
  v <- value (WidgetId 403)
  assert failed (not (isNaN v || isInfinite v || v < 0 || v > 1.5))

-- | A settled 'animateTo' value with no node holds its target while read
-- (resting values were once dropped after 300 rectless frames), comes back
-- settled after a short skip, and starts over after a long one.
runAnimateToStaysSettledTest :: Context -> IORef Int -> IO ()
runAnimateToStaysSettledTest ctx failed = do
  let inp = withDelta 200 100 0.05
      ui shown = (if shown then withKey ("fade" :: String) (animateTo (Tween EaseLinear 0.2 0) 1) else pure 0) <* label "rest"
      near v x = abs (x - v) <= 0.001
  replicateM_ 10 (runFrame ctx inp (ui True))
  assert failed . not =<< anyAnimating ctx
  assert failed . all (near 1) =<< replicateM 400 (evalUi ctx inp (ui True))
  assert failed . not =<< anyAnimating ctx
  assert failed . not =<< needsRedraw ctx inp inp
  replicateM_ 10 (runFrame ctx inp (ui False))
  assert failed . near 1 =<< evalUi ctx inp (ui True)
  replicateM_ 700 (runFrame ctx inp (ui False))
  assert failed . near 0 =<< evalUi ctx inp (ui True)

-- | A tween moving a label repaints its old and new rects, also one that
-- starts and ends in a frame.
runAnimationDamageTest :: Context -> IORef Int -> IO ()
runAnimationDamageTest ctx0 failed = do
  ctx1 <- newContext
  forM_ [(ctx0, 0.4, 0.05), (ctx1, 0.2, 0.5)] $ \(ctx, dur, dt) -> do
    let inp = withDelta 200 100 dt
        ui target = do
          t <- animateTo (Tween EaseLinear dur 0) target
          row (spacer (Fixed (20 + 80 * t)) Fit >> label' "anim")
    lbl <- respId <$> warmup2 ctx inp (ui 0)
    assert failed . (/= DamageFull) =<< takeDamage ctx
    -- The frame that starts the tween, then one it moves the label on.
    _ <- runFrame ctx inp (ui 1)
    assertJustM failed (getPrevRect ctx lbl) $ \old -> do
      _ <- runFrame ctx inp (ui 1)
      dmg <- takeDamage ctx
      assertJustM failed (getPrevRect ctx lbl) $ \new -> do
        assertGt failed (rectX new) (rectX old)
        assert failed (damageCovers dmg old && damageCovers dmg new)

-- | Easing curves hit their ends and bend the right way, colour channels
-- clamp, and two vectors animating side by side keep their own channels
-- (indices alone collide), settling under tweens and springs.
runCompositeAnimationIsolationTest :: Context -> IORef Int -> IO ()
runCompositeAnimationIsolationTest _ failed = do
  let out = EaseCubicBezier 0 0 0.58 1
      snappy = EaseCubicBezier 0.33 0 0.2 1
  assert failed (abs (applyEase (EaseCubicBezier 0 0 1 1) 0.5 - 0.5) <= 0.01 && applyEase out 0.5 > 0.5)
  assert failed (abs (applyEase EaseInQuad 0.5 - 0.25) <= 0.01)
  assert failed (abs (applyEase snappy 0) <= 0.001 && abs (applyEase snappy 1 - 1) <= 0.001)
  let color = colorRGBA 17 80 190 255
  assertEq failed ([0, 1], V2 3 5) (traverseChannels (\i x -> ([i], x + fromIntegral i)) (V2 3 4))
  assertEq failed ([0, 1, 2, 3], color) (traverseChannels (\i x -> ([i], x)) color)
  assertEq failed (colorRGBA 0 255 255 255) (snd (traverseChannels (\i _ -> ([i], if i == 0 then -1 else 2)) color))
  forM_ [animateToA (Tween EaseLinear 0.2 0), animateToA (Spring presetSmooth)] $ \animateVector -> do
    ctx <- newContext
    let inp = withDelta 200 100 0.05
        ui = do
          a <- animateVector (V2 1 2)
          b <- animateVector (V2 (-1) (-2))
          (a, b) <$ label (T.pack (show (a, b)))
    replicateM_ 80 (runFrame ctx inp ui)
    (V2 ax ay, V2 bx by) <- evalUi ctx inp ui
    assert failed (all ((< 0.05) . abs) [ax - 1, ay - 2, bx + 1, by + 2])
    assert failed . not =<< anyAnimating ctx
    assert failed . not =<< needsRedraw ctx inp inp

-- | A button's hover eases in without dipping and stays on through a click.
runButtonHoverAnimTest :: Context -> IORef Int -> IO ()
runButtonHoverAnimTest ctx failed = do
  let inp0 = withDelta 200 100 0.016
      ui = column (button "Hover")
  _ <- runFrame ctx inp0 ui
  vals <- replicateM 5 (runFrame ctx inp0 {inputMousePos = V2 10 10} ui >> getHotId ctx >>= getAnimationValue ctx)
  assert failed (and (zipWith (\a b -> b + 0.001 >= a) vals (drop 1 vals)))
  assert failed (last vals >= 0.4)
  _ <- runClick ctx inp0 ui (V2 10 10)
  hot <- getHotId ctx
  assert failed (hashWidgetId hot /= 0)
  assert failed . (>= 0.99) =<< getAnimationValue ctx hot

-- | A spinner keeps the loop drawing, repaints only around itself, and turns.
runSpinnerTest :: Context -> IORef Int -> IO ()
runSpinnerTest ctx failed = do
  theme <- getTheme ctx
  let inp = withDelta 400 300 0.016
      ui = column $ do
        label "Loading a long label so the window has more than the spinner"
        spinnerWith' id 18
      arc qs = [q | (q, c) <- qs, c == themeAccent theme]
  _ <- runFrame ctx inp ui
  (resp, draw0, _) <- runFrame ctx inp ui
  assert failed =<< needsRedraw ctx inp inp
  quads0 <- drawQuads draw0
  assert failed (not (null (arc quads0)))
  threadDelay 60000
  (_, draw1, _) <- runFrame ctx inp ui
  takeDamage ctx >>= \case
    DamageClip r -> assert failed (rectW r < 80 && rectH r < 80 && rectIntersect r (respRect resp) /= Nothing)
    DamageFull -> assert failed False
  assert failed . (arc quads0 /=) . arc =<< drawQuads draw1

-- | An animation the view stops building stops drawing frames and runs again
-- when shown: a spinner's 'keepAnimating', and a long tween or undamped
-- spring behind a tab switch, which once drew for minutes or for good.
runAnimationLeftOutTest :: Context -> IORef Int -> IO ()
runAnimationLeftOutTest _ failed = do
  let tweenAndSpring = do
        _ <- animate (Tween EaseLinear 120 0) 0 1
        _ <- animateTo (Spring (SpringParams 180 0 1)) 1
        label "moving"
  forM_ [(column (label "Loading" >> spinner), column (label "Loaded")), (tweenAndSpring, label "other")] $ \(shown, other) -> do
    ctx <- newContext
    let inp = withDelta 400 300 0.016
    replicateM_ 5 (runFrame ctx inp shown)
    assert failed =<< anyAnimating ctx
    _ <- runFrame ctx inp other
    assert failed . not =<< anyAnimating ctx
    _ <- runFrame ctx inp other
    assert failed . not =<< needsRedraw ctx inp inp
    _ <- runFrame ctx inp shown
    assert failed =<< anyAnimating ctx

-- | A 'keepAnimating' lease is not the widget's value: a spring on the key
-- starts from the resting value, and a set keeps the lease and repaints.
runKeepAnimatingRestTest :: Context -> IORef Int -> IO ()
runKeepAnimatingRestTest ctx failed = do
  let inp = withDelta 200 100 0.05
      hover = inp {inputMousePos = V2 10 10}
      ui = column (button' "Go" >>= \b -> b <$ keepAnimating b)
  _ <- runFrame ctx inp ui
  wid <- respId . last <$> replicateM 10 (evalUi ctx hover ui)
  assertEq failed 1 =<< getAnimationValue ctx wid
  startSpring ctx wid presetSmooth 0.5
  assert failed . (> 0.99) =<< getAnimationValue ctx wid
  replicateM_ 60 (runFrame ctx hover ui)
  assertEq failed 0.5 =<< getAnimationValue ctx wid
  setAnimationValue ctx wid 0
  assertEq failed 0 =<< getAnimationValue ctx wid
  assert failed =<< anyAnimating ctx
  assert failed =<< needsRedraw ctx hover hover

-- | An unversioned drawing rebuilds on the frame its animation ends, which
-- once replayed the last animated frame's ops for good.
runDrawingAnimationEndTest :: Context -> IORef Int -> IO ()
runDrawingAnimationEndTest ctx failed = do
  let inp = withDelta 200 100 0.016
      running = colorRGBA 255 0 0 255
      stopped = colorRGBA 0 0 255 255
      ui on = column $ do
        d <- drawing (fixedWH 40 40) (\r -> smallArrayFromList [FillRect r (if on then running else stopped)])
        when on (keepAnimating d)
  replicateM_ 3 (runFrame ctx inp (ui True))
  (_, draw, _) <- runFrame ctx inp (ui False)
  quads <- map snd <$> drawQuads draw
  assert failed (stopped `elem` quads && running `notElem` quads)

-- | 'wakeAfter' schedules one frame, the earliest request winning.
runWakeAfterTest :: Context -> IORef Int -> IO ()
runWakeAfterTest ctx failed = do
  let inp = withDelta 200 100 0.016
      wakeWithin ui lo hi = do
        t <- getMonotonicTime
        _ <- runFrame ctx inp ui
        at <- getWakeAt ctx
        assert failed (at > t + lo && at < t + hi)
  wakeWithin (label "tick" >> wakeAfter 5) 4.5 6
  wakeWithin (wakeAfter 5 >> wakeAfter 1 >> wakeAfter 3 >> label "tick") 0.5 2
  _ <- runFrame ctx inp (label "tick")
  assertEq failed 0 =<< getWakeAt ctx
