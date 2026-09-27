module Cases.Animation (tests) where

import Spec
import Control.Concurrent (threadDelay)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)

tests :: [Spec]
tests =
  [ spec "animation-settle" runAnimationSettleTest
  , spec "animate-to-stays-settled" runAnimateToStaysSettledTest
  , spec "animation-damage" runAnimationDamageTest
  , spec "animation-stagger" runAnimationStaggerTest
  , spec "animation-bezier" runAnimationBezierTest
  , spec "animation-spring-retarget" runAnimationSpringRetargetTest
  , spec "animation-spring-dt" runAnimationSpringDtTest
  , spec "composite-animation-isolation" runCompositeAnimationIsolationTest
  , spec "button-hover-anim" runButtonHoverAnimTest
  , spec "spinner" runSpinnerTest
  , spec "keep-animating-lapse" runKeepAnimatingLapseTest
  , spec "wake-after" runWakeAfterTest
  ]

-- A started animation requests redraws, settles on its target, and then
-- leaves the context idle and clean.
runAnimationSettleTest :: Context -> IORef Int -> IO ()
runAnimationSettleTest ctx failed = do
  let inp = withDelta 100 100 0.1
      wid = WidgetId 99
  _ <- runFrame ctx inp (label "settle")
  startAnimation ctx wid 0 1 0.25
  need <- needsRedraw ctx inp inp
  assert failed need
  replicateM_ 4 (runFrame ctx inp (label "settle"))
  val <- getAnimationValue ctx wid
  assert failed (abs (val - 1) <= 0.01)
  live <- anyAnimating ctx
  assert failed (not live)
  needAfter <- needsRedraw ctx inp inp
  assert failed (not needAfter)
  (_, _, _, dirty) <- runFrame ctx inp (label "settle")
  assert failed (not dirty)

-- A settled 'animateTo' value holds its target for as long as the view reads
-- it. Its key has no node, and resting values were once dropped after 300
-- frames without a widget rect, so it read 0 again and animated back up. One
-- the view skips for a few frames comes back settled; one no view reads for
-- long enough is dropped, and animates in from 0 like a new one. The key
-- keeps the label from taking the value's id while it is hidden, which would
-- hold it through the label's rect.
runAnimateToStaysSettledTest :: Context -> IORef Int -> IO ()
runAnimateToStaysSettledTest ctx failed = do
  let inp = withDelta 200 100 0.05
      ui shown = do
        t <- if shown then withKey ("fade" :: String) (animateTo (Tween EaseLinear 0.2 0) 1) else pure 0
        label "rest"
        pure t
      near v x = abs (x - v) <= 0.001
  replicateM_ 10 (runFrame ctx inp (ui True))
  assert failed . not =<< anyAnimating ctx
  vals <- replicateM 400 (evalUi ctx inp (ui True))
  assert failed (all (near 1) vals)
  assert failed . not =<< anyAnimating ctx
  assert failed . not =<< needsRedraw ctx inp inp
  replicateM_ 10 (runFrame ctx inp (ui False))
  assert failed . near 1 =<< evalUi ctx inp (ui True)
  replicateM_ 700 (runFrame ctx inp (ui False))
  assert failed . near 0 =<< evalUi ctx inp (ui True)

runAnimationDamageTest :: Context -> IORef Int -> IO ()
runAnimationDamageTest _ failed = do
  ctx <- newContext
  let idleInp = withDelta 200 100 0
      idle = label "anim"
      tweenInp = idleInp {inputDeltaTime = 0.05}
      ui = do
        t <- animateTo (Tween EaseLinear 0.4 0) 1
        void (spacer (Fixed (20 + 80 * t)) Fit)
        label "anim"
      hasMove dmg = case dmg of
        DamageFull -> True
        DamageClip r -> rectW r > 0 && rectH r > 0
  _ <- warmup2 ctx idleInp idle
  dIdle <- takeDamage ctx
  assert failed (dIdle /= DamageFull)
  _ <- runFrame ctx tweenInp ui
  dMid <- takeDamage ctx
  assert failed (hasMove dMid)
  ctx2 <- newContext
  let fastInp = idleInp {inputDeltaTime = 0.5}
      uiFast = do
        t <- animateTo (Tween EaseLinear 0.2 0) 1
        void (spacer (Fixed (20 + 80 * t)) Fit)
        label "anim"
  _ <- warmup2 ctx2 idleInp idle
  _ <- runFrame ctx2 fastInp uiFast
  dFast <- takeDamage ctx2
  assert failed (hasMove dFast)

-- A delayed tween holds its start value until the delay elapses and then
-- eases from there; declarative tweens stagger the same way per key.
runAnimationStaggerTest :: Context -> IORef Int -> IO ()
runAnimationStaggerTest ctx failed = do
  let inp = withDelta 200 100 0.02
      wid = WidgetId 202
      slow = inp {inputDeltaTime = 0.1}
  startAnimationEaseDelay ctx wid 0 1 0.2 EaseLinear 0.15
  _ <- runFrame ctx slow (label "delay")
  v0 <- getAnimationValue ctx wid
  assert failed (abs v0 <= 0.01)
  live0 <- anyAnimating ctx
  assert failed live0
  _ <- runFrame ctx slow (label "delay")
  v1 <- getAnimationValue ctx wid
  assert failed (abs (v1 - 0.25) <= 0.03)
  let ui = do
        _ <- withKey ("lead" :: String) (animateTo (Tween EaseLinear 0.4 0) 1)
        t <- withKey ("trail" :: String) (animateTo (Tween EaseLinear 0.4 0.08) 1)
        label (T.pack ("t=" ++ show t))
      trailVal = do
        spans <- collectTextSpans ctx
        let shown = [txt | (_, txt, _, _, _) <- spans]
            tagged = [T.drop 2 txt | txt <- shown, "t=" `T.isPrefixOf` txt]
        case tagged of
          (raw : _) -> case reads (T.unpack raw) of
            [(n, "")] -> pure (n :: Float)
            _ -> assert failed False >> pure 0
          _ -> assert failed False >> pure 0
  replicateM_ 3 (runFrame ctx inp ui)
  early <- trailVal
  assert failed (early <= 0.01)
  replicateM_ 10 (runFrame ctx inp ui)
  late <- trailVal
  assert failed (late >= 0.15)

runAnimationBezierTest :: Context -> IORef Int -> IO ()
runAnimationBezierTest _ failed = do
  let lin = applyEase (EaseCubicBezier 0 0 1 1) 0.5
      out = applyEase (EaseCubicBezier 0 0 0.58 1) 0.5
  assert failed (abs (lin - 0.5) <= 0.01)
  assert failed (out > 0.5)
  assert failed (abs (applyEase EaseInQuad 0.5 - 0.25) <= 0.01)
  assert failed (abs (applyEase (EaseCubicBezier 0.33 0 0.2 1) 0) <= 0.001)
  assert failed (abs (applyEase (EaseCubicBezier 0.33 0 0.2 1) 1 - 1) <= 0.001)

runAnimationSpringRetargetTest :: Context -> IORef Int -> IO ()
runAnimationSpringRetargetTest ctx failed = do
  let inp = withDelta 100 100 0.02
      wid = WidgetId 402
  startSpring ctx wid presetBouncy 1
  replicateM_ 5 (runFrame ctx inp (label "retarget"))
  v1 <- getAnimationValue ctx wid
  assert failed (v1 >= 0.02 && v1 <= 0.98)
  startSpring ctx wid presetBouncy 0
  v2 <- getAnimationValue ctx wid
  assert failed (abs (v2 - v1) <= 0.02)
  live <- anyAnimating ctx
  assert failed live

runAnimationSpringDtTest :: Context -> IORef Int -> IO ()
runAnimationSpringDtTest ctx failed = do
  let inp = withDelta 100 100 2
      wid = WidgetId 403
  startSpring ctx wid presetStiff 1
  _ <- runFrame ctx inp (label "dt")
  val <- getAnimationValue ctx wid
  assert failed (not (isNaN val || isInfinite val || val < 0 || val > 1.5))

-- Each composite animation owns a scope; component indices alone are not
-- unique when two vectors animate side by side in the same parent. Tweens
-- and springs both settle and stop requesting redraws.
runCompositeAnimationIsolationTest :: Context -> IORef Int -> IO ()
runCompositeAnimationIsolationTest _ failed = do
  let
    (indices, vector) = traverseChannels (\i x -> ([i], x + fromIntegral i)) (V2 3 4)
    color = colorRGBA 17 80 190 255
    (rgbaIndices, roundtrip) = traverseChannels (\i x -> ([i], x)) color
    (_, clipped) = traverseChannels (\i _ -> ([i], if i == 0 then -1 else 2)) color
  assertEq failed [0, 1] indices
  assertEq failed (V2 3 5) vector
  assertEq failed [0, 1, 2, 3] rgbaIndices
  assertEq failed color roundtrip
  assertEq failed (colorRGBA 0 255 255 255) clipped
  forM_ [animateToA (Tween EaseLinear 0.2 0), animateToA (Spring presetSmooth)] $ \animateVector -> do
    ctx <- newContext
    let
      inp = withDelta 200 100 0.05
      ui = do
        a <- animateVector (V2 1 2)
        b <- animateVector (V2 (-1) (-2))
        label (T.pack (show (a, b)))
        pure (a, b)
    replicateM_ 80 (runFrame ctx inp ui)
    ((V2 ax ay, V2 bx by), _, _, _) <- runFrame ctx inp ui
    assert failed (abs (ax - 1) < 0.05 && abs (ay - 2) < 0.05)
    assert failed (abs (bx + 1) < 0.05 && abs (by + 2) < 0.05)
    live <- anyAnimating ctx
    assert failed (not live)
    need <- needsRedraw ctx inp inp
    assert failed (not need)

-- Hovering a button eases its highlight in without dipping, and a press and
-- release over it leaves the hover animation fully on.
runButtonHoverAnimTest :: Context -> IORef Int -> IO ()
runButtonHoverAnimTest ctx failed = do
  let inp0 = withDelta 200 100 0.016
      ui = column (button "Hover")
  _ <- runFrame ctx inp0 ui
  let inp1 = inp0 {inputMousePos = V2 10 10}
  vals <- replicateM 5 (runFrame ctx inp1 ui >> getHotId ctx >>= getAnimationValue ctx)
  let decreases = any (uncurry (\a b -> b + 0.001 < a)) (zip vals (drop 1 vals))
  assert failed (not decreases)
  assert failed (last vals >= 0.4)
  let (press, release) = clickPair inp0 (V2 10 10)
  _ <- runFrame ctx press ui
  _ <- runFrame ctx release ui
  hot <- getHotId ctx
  val <- getAnimationValue ctx hot
  assert failed (hashWidgetId hot /= 0)
  assert failed (val >= 0.99)

-- A spinner keeps the loop drawing, repaints only around itself, and turns.
runSpinnerTest :: Context -> IORef Int -> IO ()
runSpinnerTest ctx failed = do
  theme <- getTheme ctx
  let inp = withDelta 400 300 0.016
      ui = column $ do
        label "Loading a long label so the window has more than the spinner"
        spinnerWith' id 18
  _ <- runFrame ctx inp ui
  (resp, _, draw0, _) <- runFrame ctx inp ui
  _ <- takeDamage ctx
  need <- needsRedraw ctx inp inp
  assert failed need
  quads0 <- drawQuads draw0
  assert failed (any ((== themeAccent theme) . snd) quads0)
  threadDelay 60000
  (_, _, draw1, _) <- runFrame ctx inp ui
  dmg <- takeDamage ctx
  case dmg of
    DamageClip r -> assert failed (rectW r < 80 && rectH r < 80 && rectIntersect r (respRect resp) /= Nothing)
    DamageFull -> assert failed False
  quads1 <- drawQuads draw1
  let arc qs = [q | (q, c) <- qs, c == themeAccent theme]
  assert failed (arc quads0 /= arc quads1)

-- 'keepAnimating' holds the loop open only while it keeps being called. A
-- spinner that is no longer built must let the loop sleep: its animation
-- never ends by itself, so a view that showed one while loading would
-- otherwise run frames at the display rate for the life of the process.
runKeepAnimatingLapseTest :: Context -> IORef Int -> IO ()
runKeepAnimatingLapseTest ctx failed = do
  let inp = withDelta 400 300 0.016
      loading = column (label "Loading" >> spinner)
      loaded = column (label "Loaded")
  _ <- warmup2 ctx inp loading
  assert failed =<< anyAnimating ctx
  -- The first frame without the spinner ends its animation.
  _ <- runFrame ctx inp loaded
  assert failed . not =<< anyAnimating ctx
  _ <- runFrame ctx inp loaded
  _ <- takeDamage ctx
  assert failed . not =<< needsRedraw ctx inp inp
  -- Shown again, it runs again.
  _ <- runFrame ctx inp loading
  assert failed =<< anyAnimating ctx

-- 'wakeAfter' schedules one frame: the earliest request of a frame wins, and
-- a frame that does not ask again leaves nothing pending.
runWakeAfterTest :: Context -> IORef Int -> IO ()
runWakeAfterTest ctx failed = do
  let inp = withDelta 200 100 0.016
  t0 <- getMonotonicTime
  _ <- runFrame ctx inp (label "tick" >> wakeAfter 5)
  at0 <- getWakeAt ctx
  assert failed (at0 > t0 + 4.5 && at0 < t0 + 6)
  t1 <- getMonotonicTime
  _ <- runFrame ctx inp (wakeAfter 5 >> wakeAfter 1 >> wakeAfter 3 >> label "tick")
  at1 <- getWakeAt ctx
  assert failed (at1 > t1 + 0.5 && at1 < t1 + 2)
  _ <- runFrame ctx inp (label "tick")
  assertEq failed 0 =<< getWakeAt ctx
