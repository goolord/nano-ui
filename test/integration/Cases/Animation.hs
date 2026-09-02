module Cases.Animation
  ( runAnimationBezierTest
  , runAnimationDamageTest
  , runAnimationDelayTest
  , runAnimationEaseTest
  , runAnimationHoldTest
  , runAnimationIdleTest
  , runAnimationSettleTest
  , runAnimationSharedCtxTest
  , runAnimationSpringATest
  , runAnimationSpringDtTest
  , runAnimationSpringHoldTest
  , runAnimationSpringRetargetTest
  , runAnimationSpringTest
  , runAnimationStaggerTest
  ) where

import Control.Monad (replicateM_, void)
import Data.IORef (IORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert)
import NanoUI.Testing.Harness (withAnimCtx, withDelta)

runAnimationIdleTest :: Context -> IORef Int -> IO ()
runAnimationIdleTest _ failed =
  withAnimCtx 100 100 0.05
    (\ctx inp fl -> do
      _ <- runFrame ctx inp (label "anim")
      startAnimation ctx (WidgetId 42) 0 1 0.5
      need <- needsRedraw ctx inp inp
      assert fl need
    )
    failed

runAnimationSettleTest :: Context -> IORef Int -> IO ()
runAnimationSettleTest _ failed =
  withAnimCtx 100 100 0.1
    (\ctx inp fl -> do
      let wid = WidgetId 99
      startAnimation ctx wid 0 1 0.25
      replicateM_ 4 (runFrame ctx inp (label "settle"))
      val <- getAnimationValue ctx wid
      assert fl (abs (val - 1) <= 0.01)
      live <- anyAnimating ctx
      assert fl (not live)
      need <- needsRedraw ctx inp inp
      assert fl (not need)
    )
    failed

runAnimationEaseTest :: Context -> IORef Int -> IO ()
runAnimationEaseTest _ failed =
  withAnimCtx 100 100 0.5
    (\ctx inp fl -> do
      let wid = WidgetId 100
      startAnimationEase ctx wid 0 1 1 EaseOutCubic
      _ <- runFrame ctx inp (label "ease")
      val <- getAnimationValue ctx wid
      assert fl (val >= 0.8)
    )
    failed

runAnimationHoldTest :: Context -> IORef Int -> IO ()
runAnimationHoldTest _ failed = do
  ctx <- newContext
  let inp = withDelta 200 100 0.1
      ui = do
        t <- animateTo 1 0.2
        label_ (T.pack (show t))
  replicateM_ 6 (runFrame ctx inp ui)
  live <- anyAnimating ctx
  assert failed (not live)
  spans <- collectTextSpans ctx
  let shown = [txt | (_, txt, _, _, _) <- spans]
  assert failed (any (\t -> t == "1.0" || "1.0" `T.isPrefixOf` t) shown)
  startAnimationEase ctx (WidgetId 101) 0 1 0.2 EaseLinear
  replicateM_ 5 (runFrame ctx inp (label "hold"))
  val <- getAnimationValue ctx (WidgetId 101)
  assert failed (abs (val - 1) <= 0.01)
  replicateM_ 3 (runFrame ctx inp (label "hold"))
  val2 <- getAnimationValue ctx (WidgetId 101)
  assert failed (abs (val2 - 1) <= 0.01)
  live2 <- anyAnimating ctx
  assert failed (not live2)

runAnimationDamageTest :: Context -> IORef Int -> IO ()
runAnimationDamageTest _ failed = do
  ctx <- newContext
  let idleInp = withDelta 200 100 0
      idle = label_ "anim"
      tweenInp = idleInp {inputDeltaTime = 0.05}
      ui = do
        t <- animateTo 1 0.4
        void (spacer (Fixed (20 + 80 * t)) Fit)
        label_ "anim"
      hasMove dmg = case dmg of
        DamageFull -> True
        DamageClip r -> rectW r > 0 && rectH r > 0
  _ <- runFrame ctx idleInp idle
  _ <- runFrame ctx idleInp idle
  dIdle <- takeDamage ctx
  assert failed (dIdle /= DamageFull)
  _ <- runFrame ctx tweenInp ui
  dMid <- takeDamage ctx
  assert failed (hasMove dMid)
  ctx2 <- newContext
  let fastInp = idleInp {inputDeltaTime = 0.5}
      uiFast = do
        t <- animateTo 1 0.2
        void (spacer (Fixed (20 + 80 * t)) Fit)
        label_ "anim"
  _ <- runFrame ctx2 idleInp idle
  _ <- runFrame ctx2 idleInp idle
  _ <- runFrame ctx2 fastInp uiFast
  dFast <- takeDamage ctx2
  assert failed (hasMove dFast)

runAnimationDelayTest :: Context -> IORef Int -> IO ()
runAnimationDelayTest _ failed =
  withAnimCtx 100 100 0.1
    (\ctx inp0 fl -> do
      let wid = WidgetId 202
      startAnimationEaseDelay ctx wid 0 1 0.2 EaseLinear 0.15
      _ <- runFrame ctx inp0 (label "delay")
      v0 <- getAnimationValue ctx wid
      assert fl (abs v0 <= 0.01)
      live0 <- anyAnimating ctx
      assert fl live0
      _ <- runFrame ctx inp0 (label "delay")
      v1 <- getAnimationValue ctx wid
      assert fl (abs (v1 - 0.25) <= 0.03)
    )
    failed

runAnimationStaggerTest :: Context -> IORef Int -> IO ()
runAnimationStaggerTest _ failed =
  withAnimCtx 200 100 0.02
    (\ctx inp fl -> do
      let ui = do
            _ <- withKey ("lead" :: String) (animateToEaseDelay EaseLinear 1 0.4 0)
            t <- withKey ("trail" :: String) (animateToEaseDelay EaseLinear 1 0.4 0.08)
            label_ (T.pack ("t=" ++ show t))
          trailVal = do
            spans <- collectTextSpans ctx
            let shown = [txt | (_, txt, _, _, _) <- spans]
                tagged = [T.drop 2 txt | txt <- shown, "t=" `T.isPrefixOf` txt]
            case tagged of
              (raw : _) -> case reads (T.unpack raw) of
                [(n, "")] -> pure (n :: Float)
                _ -> assert fl False >> pure 0
              _ -> assert fl False >> pure 0
      replicateM_ 3 (runFrame ctx inp ui)
      early <- trailVal
      assert fl (early <= 0.01)
      replicateM_ 10 (runFrame ctx inp ui)
      late <- trailVal
      assert fl (late >= 0.15)
    )
    failed

runAnimationBezierTest :: Context -> IORef Int -> IO ()
runAnimationBezierTest _ failed = do
  let lin = applyEase (EaseCubicBezier 0 0 1 1) 0.5
      out = applyEase (EaseCubicBezier 0 0 0.58 1) 0.5
  assert failed (abs (lin - 0.5) <= 0.01)
  assert failed (out > 0.5)
  assert failed (abs (applyEase EaseInQuad 0.5 - 0.25) <= 0.01)
  assert failed (abs (applyEase (EaseCubicBezier 0.33 0 0.2 1) 0) <= 0.001)
  assert failed (abs (applyEase (EaseCubicBezier 0.33 0 0.2 1) 1 - 1) <= 0.001)

runAnimationSpringTest :: Context -> IORef Int -> IO ()
runAnimationSpringTest _ failed =
  withAnimCtx 100 100 0.05
    (\ctx inp fl -> do
      let wid = WidgetId 401
      startSpring ctx wid presetSmooth 1
      replicateM_ 80 (runFrame ctx inp (label "spring"))
      val <- getAnimationValue ctx wid
      assert fl (abs (val - 1) <= 0.02)
      live <- anyAnimating ctx
      assert fl (not live)
      need <- needsRedraw ctx inp inp
      assert fl (not need)
    )
    failed

runAnimationSpringRetargetTest :: Context -> IORef Int -> IO ()
runAnimationSpringRetargetTest _ failed =
  withAnimCtx 100 100 0.02
    (\ctx inp fl -> do
      let wid = WidgetId 402
      startSpring ctx wid presetBouncy 1
      replicateM_ 5 (runFrame ctx inp (label "retarget"))
      v1 <- getAnimationValue ctx wid
      assert fl (v1 >= 0.02 && v1 <= 0.98)
      startSpring ctx wid presetBouncy 0
      v2 <- getAnimationValue ctx wid
      assert fl (abs (v2 - v1) <= 0.02)
      live <- anyAnimating ctx
      assert fl live
    )
    failed

runAnimationSpringDtTest :: Context -> IORef Int -> IO ()
runAnimationSpringDtTest _ failed =
  withAnimCtx 100 100 2
    (\ctx inp fl -> do
      let wid = WidgetId 403
      startSpring ctx wid presetStiff 1
      _ <- runFrame ctx inp (label "dt")
      val <- getAnimationValue ctx wid
      assert fl (not (isNaN val || isInfinite val || val < 0 || val > 1.5))
    )
    failed

runAnimationSpringHoldTest :: Context -> IORef Int -> IO ()
runAnimationSpringHoldTest _ failed = do
  ctx <- newContext
  let inp = withDelta 200 100 0.05
      ui = do
        t <- animateToSpring presetSmooth 1
        label_ (T.pack (show t))
  replicateM_ 81 (runFrame ctx inp ui)
  live <- anyAnimating ctx
  assert failed (not live)
  spans <- collectTextSpans ctx
  let shown = [txt | (_, txt, _, _, _) <- spans]
  assert failed (any (\t -> t == "1.0" || "1.0" `T.isPrefixOf` t) shown)

runAnimationSpringATest :: Context -> IORef Int -> IO ()
runAnimationSpringATest _ failed = do
  ctx <- newContext
  let inp = withDelta 200 100 0.05
      ui = do
        V2 x y <- withKey ("vec" :: String) (animateToSpringA presetSmooth (V2 1 2))
        label_ (T.pack (show x ++ "," ++ show y))
  replicateM_ 80 (runFrame ctx inp ui)
  live <- anyAnimating ctx
  assert failed (not live)
  spans <- collectTextSpans ctx
  let shown = [txt | (_, txt, _, _, _) <- spans]
      ok t = case break (== ',') (T.unpack t) of
        (xs, ',' : ys) -> case (reads xs, reads ys) of
          ([(x, "")], [(y, "")]) -> abs (x - 1 :: Float) < 0.05 && abs (y - 2 :: Float) < 0.05
          _ -> False
        _ -> False
  assert failed (any ok shown)

runAnimationSharedCtxTest :: Context -> IORef Int -> IO ()
runAnimationSharedCtxTest ctx failed = do
  let inp = withDelta 80 80 0.1
      wid = WidgetId 777
  startAnimation ctx wid 0 1 0.1
  replicateM_ 3 (runFrame ctx inp (label "shared"))
  val <- getAnimationValue ctx wid
  assert failed (abs (val - 1) <= 0.01)
  need <- needsRedraw ctx inp inp
  assert failed (not need)
  (_, _, _, dirty) <- runFrame ctx inp (label "idle")
  assert failed (not dirty)
