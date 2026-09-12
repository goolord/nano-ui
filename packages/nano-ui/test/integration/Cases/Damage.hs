module Cases.Damage
  ( runDamageBoundsResolutionTest
  , runDamageBoundsMonoidTest
  , runExplicitDamageWidgetTest
  , runExplicitDamageRectTest
  , runExplicitDamageFullTest
  , runDamageQueueClearedPerFrameTest
  , runStateChangeDamageTest
  , runOrphanAnimationDamageSettlesTest
  ) where

import Control.Monad (when)
import Data.IORef (IORef)
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, withInput)
import NanoUI.Testing.Harness (warmup2)

runDamageBoundsResolutionTest :: Context -> IORef Int -> IO ()
runDamageBoundsResolutionTest _ failed = do
  let base = Rect 10 20 100 50
      rSelf = resolveDamageRect DamageSelf base
      rInflated = resolveDamageRect (DamageInflated 8.0) base
      rExact = resolveDamageRect (DamageExact (Rect 0 0 500 500)) base
      rCustom = resolveDamageRect (DamageCustom (\(Rect x y w h) -> Rect (x - 1) (y - 2) (w + 10) (h + 20))) base
      rNone = resolveDamageRect DamageNone base

  assertEq failed rSelf base
  assertEq failed rInflated (Rect 2 12 116 66)
  assertEq failed rExact (Rect 0 0 500 500)
  assertEq failed rCustom (Rect 9 18 110 70)
  assertEq failed rNone (Rect 0 0 0 0)

runDamageBoundsMonoidTest :: Context -> IORef Int -> IO ()
runDamageBoundsMonoidTest _ failed = do
  let base = Rect 10 20 100 50
      b1 = DamageInflated 4.0
      b2 = DamageInflated 8.0
      bUnion = b1 <> b2
      rUnion = resolveDamageRect bUnion base
      rExpected = rectUnion (Rect 6 16 108 58) (Rect 2 12 116 66)

  assertEq failed rUnion rExpected
  assertEq failed (resolveDamageRect mempty base) base

runExplicitDamageWidgetTest :: Context -> IORef Int -> IO ()
runExplicitDamageWidgetTest _ failed = do
  ctx <- newContext
  let inp = withInput 400 300
      ui = columnWith (padAll 20) $ do
        w1 <- button' "First"
        w2 <- button' "Second"
        pure (w1, w2)
  -- Warmup to establish solved layout rects
  _ <- runFrame ctx inp ui
  ((w1, _), _, _, _) <- runFrame ctx inp ui
  _ <- takeDamage ctx

  -- Queue explicit widget damage
  let testUi = columnWith (padAll 20) $ do
        w1' <- button' "First"
        w2' <- button' "Second"
        damageWidgetNow (respId w1') (DamageInflated sliderDamageSlop)
        pure (w1', w2')
  _ <- runFrame ctx inp testUi
  dmg <- takeDamage ctx
  let Rect x1 y1 w1Len h1Len = respRect w1
      expected = rectInflate sliderDamageSlop (Rect x1 y1 w1Len h1Len)
      approxEq (Rect a b c d) (Rect e f g h) =
        abs (a - e) < 0.05 && abs (b - f) < 0.05 && abs (c - g) < 0.05 && abs (d - h) < 0.05
  case dmg of
    DamageFull -> assert failed False
    DamageClip r -> assert failed (approxEq r expected)

runExplicitDamageRectTest :: Context -> IORef Int -> IO ()
runExplicitDamageRectTest _ failed = do
  ctx <- newContext
  let inp = withInput 400 300
      customRect = Rect 15 25 80 45
      ui = column (label "Hello")
      damagedUi = column $ do
        damageRectNow customRect
        label "Hello"
  _ <- warmup2 ctx inp ui
  _ <- takeDamage ctx

  _ <- runFrame ctx inp damagedUi
  dmg <- takeDamage ctx
  case dmg of
    DamageFull -> assert failed False
    DamageClip r -> assertEq failed r customRect

runExplicitDamageFullTest :: Context -> IORef Int -> IO ()
runExplicitDamageFullTest _ failed = do
  ctx <- newContext
  let inp = withInput 400 300
      ui = column (label "Hello")
      fullDamagedUi = column $ do
        damageFullNow
        label "Hello"
  _ <- warmup2 ctx inp ui
  _ <- takeDamage ctx

  _ <- runFrame ctx inp fullDamagedUi
  dmg <- takeDamage ctx
  assertEq failed dmg DamageFull

runDamageQueueClearedPerFrameTest :: Context -> IORef Int -> IO ()
runDamageQueueClearedPerFrameTest _ failed = do
  ctx <- newContext
  let inp = withInput 400 300
      ui = column (label "Static content")
  _ <- warmup2 ctx inp ui
  _ <- takeDamage ctx

  -- Explicit damage in this frame
  let damagedUi = column $ do
        damageRectNow (Rect 5 5 20 20)
        label "Static content"
  _ <- runFrame ctx inp damagedUi
  dmg1 <- takeDamage ctx
  case dmg1 of
    DamageClip r -> assertEq failed r (Rect 5 5 20 20)
    _ -> assert failed False

  -- Next frame without damage requests: damage is empty
  _ <- runFrame ctx inp ui
  dmg2 <- takeDamage ctx
  assert failed (damageIsEmpty dmg2)

runStateChangeDamageTest :: Context -> IORef Int -> IO ()
runStateChangeDamageTest _ failed = do
  ctx <- newContext
  let inp0 = withInput 400 300
      ui = do
        (name, setName) <- useText ""
        row $ do
          label_ ("Left pane: " <> name)
          (resp, typed) <- textInput "Name" ""
          when (respChanged resp) (setName typed)

  -- Warm up and focus textInput via Tab
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  _ <- takeDamage ctx

  -- Type a character into focused textInput
  _ <- runFrame ctx (inp0 {inputChars = "a"}) ui
  dmg <- takeDamage ctx
  assertEq failed dmg DamageFull

runOrphanAnimationDamageSettlesTest :: Context -> IORef Int -> IO ()
runOrphanAnimationDamageSettlesTest _ failed = do
  let winInp = withInput 400 300
      inp = winInp {inputDeltaTime = 0.05}
      withBar = columnWith (padAll 20) $ do
        barResp <- spacer (Fixed 40) (Fixed 20)
        pure barResp
      withoutBar = columnWith (padAll 20) (pure ())
  ctx <- newContext
  -- Warm up: the bar widget occupies a nonzero 40x20 rect in the arena.
  (barResp, _, _, _) <- runFrame ctx inp withBar
  _ <- takeDamage ctx
  let wid = respId barResp
  -- keepAnimating-style perpetual animation on an established widget.
  startAnimation ctx wid 0 1 1e9
  -- Widget present and animating => damage is a clip over it, not a
  -- whole-window repaint.
  _ <- runFrame ctx inp withBar
  dmgAnimated <- takeDamage ctx
  case dmgAnimated of
    DamageFull -> assert failed False
    DamageClip r -> assert failed (rectW r > 0 && rectH r > 0)
  -- Widget leaves the arena (tab switch). The first absent frame may repaint
  -- its old region.
  _ <- runFrame ctx inp withoutBar
  _ <- takeDamage ctx
  -- The perpetual animation is still live, but it must not force the whole
  -- window to repaint forever after its widget is gone.
  _ <- runFrame ctx inp withoutBar
  live <- anyAnimating ctx
  assert failed live
  dmgAbsent <- takeDamage ctx
  assert failed (damageIsEmpty dmgAbsent)
  -- Guard: a freshly started animation on a widget that has never been laid
  -- out still escalates to a full repaint for its first rect-less frame.
  ctx2 <- newContext
  startAnimation ctx2 (WidgetId 777) 0 1 0.3
  _ <- runFrame ctx2 winInp (label "bare")
  dmgFresh <- takeDamage ctx2
  assertEq failed dmgFresh DamageFull
  _ <- runFrame ctx2 winInp (label "bare")
  dmgFresh2 <- takeDamage ctx2
  assert failed (dmgFresh2 /= DamageFull)
