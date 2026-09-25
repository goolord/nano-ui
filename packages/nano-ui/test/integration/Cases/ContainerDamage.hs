module Cases.ContainerDamage (tests) where

import Spec

tests :: [Spec]
tests =
  [ spec "panel-shrink-damage-hovered" (runPanelResizeDamageTest True 100 60)
  , spec "panel-shrink-damage" (runPanelResizeDamageTest False 100 60)
  , spec "panel-grow-damage" (runPanelResizeDamageTest False 60 100)
  , spec "scroller-shrink-damage" (runScrollerResizeDamageTest 100 60)
  , spec "scroller-grow-damage" (runScrollerResizeDamageTest 60 100)
  , spec "panel-steady-no-damage" runPanelSteadyNoDamageTest
  ]

-- | A panel is a container without a widget id of its own, but it paints a
-- background and a border. When it changes width the frame repaints both
-- where it was and where it is, with the pointer on the button inside it
-- (whose hover fade keeps the frame clipped) or away from the window.
runPanelResizeDamageTest :: Bool -> Float -> Float -> Context -> IORef Int -> IO ()
runPanelResizeDamageTest hovered from to ctx failed = do
  let inp = if hovered then (withInput 300 200) {inputMousePos = V2 10 10} else withInputOff 300 200
      ui w = panelWith (fixedWH w 40) (void (buttonWith (fixedWH 30 20) ""))
  _ <- warmup2 ctx inp (ui from)
  old <- arenaRects ctx
  _ <- takeDamage ctx
  _ <- runFrame ctx inp (ui to)
  new <- arenaRects ctx
  dmg <- takeDamage ctx
  assert failed (Rect 0 0 from 40 `elem` old)
  assert failed (Rect 0 0 to 40 `elem` new)
  assert failed (damageCovers dmg (Rect 0 0 (max from to) 40))

-- | A scroller's well, border and bar lane lie outside its viewport, which is
-- the clip it gives its content. When it changes width under a hovered button
-- in it, the frame repaints its old and new rects whole, not only their parts
-- inside the viewport.
runScrollerResizeDamageTest :: Float -> Float -> Context -> IORef Int -> IO ()
runScrollerResizeDamageTest from to ctx failed = do
  let inp = (withInput 300 200) {inputMousePos = V2 10 10}
      ui w = column $ scrollWith (fixedWH w 60) $ replicateM_ 6 (buttonWith (fixedWH 30 20) "")
  _ <- warmup2 ctx inp (ui from)
  old <- arenaRects ctx
  _ <- takeDamage ctx
  _ <- runFrame ctx inp (ui to)
  new <- arenaRects ctx
  dmg <- takeDamage ctx
  assert failed (Rect 3 3 from 60 `elem` old)
  assert failed (Rect 3 3 to 60 `elem` new)
  assert failed (damageCovers dmg (Rect 3 3 (max from to) 60))

-- | A panel that stays put repaints nothing: tracking its rect adds no damage
-- to a frame where nothing changed.
runPanelSteadyNoDamageTest :: Context -> IORef Int -> IO ()
runPanelSteadyNoDamageTest ctx failed = do
  let inp = withInputOff 300 200
      ui = column $ do
        panelWith (fixedWH 100 40) (void (buttonWith (fixedWH 30 20) ""))
        panelWith (fixedWH 80 40) (label "second")
  _ <- warmup2 ctx inp ui
  _ <- takeDamage ctx
  _ <- runFrame ctx inp ui
  dmg <- takeDamage ctx
  assertEq failed (DamageClip (Rect 0 0 0 0)) dmg

damageCovers :: Damage -> Rect -> Bool
damageCovers DamageFull _ = True
damageCovers (DamageClip clip) r = covers clip r
