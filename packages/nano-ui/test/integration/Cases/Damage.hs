module Cases.Damage (tests) where

import Spec
import NanoUI.Shortcut
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Data.Primitive.SmallArray (smallArrayFromList)

tests :: [Spec]
tests =
  [ spec "damage-bounds-resolution" runDamageBoundsResolutionTest
  , spec "damage-widget-explicit" runExplicitDamageWidgetTest
  , spec "damage-queue-cleared" runDamageQueueClearedPerFrameTest
  , spec "damage-state-change" runStateChangeDamageTest
  , spec "damage-orphan-anim-settles" runOrphanAnimationDamageSettlesTest
  , spec "versioned-drawing-damage" runVersionedDrawingDamageTest
  , spec "clip-frame-backdrop" runClipFrameBackdropTest
  , spec "textarea-select-all-damage" runTextAreaSelectAllDamageTest
  , spec "damage-pieces-merge" runDamagePiecesMergeTest
  , spec "damage-pieces-far-labels" runFarLabelsDamagePiecesTest
  , spec "damage-hook-paint-change" runHookPaintChangeDamageTest
  , spec "damage-float-hook-paint-change" runFloatHookPaintChangeDamageTest
  , spec "panel-shrink-damage-hovered" (runPanelResizeDamageTest True 100 60)
  , spec "panel-shrink-damage" (runPanelResizeDamageTest False 100 60)
  , spec "panel-grow-damage" (runPanelResizeDamageTest False 60 100)
  , spec "scroller-shrink-damage" (runScrollerResizeDamageTest 100 60)
  , spec "scroller-grow-damage" (runScrollerResizeDamageTest 60 100)
  , spec "panel-steady-no-damage" runPanelSteadyNoDamageTest
  , spec "drawing-shrink-damage" runDrawingShrinkDamageTest
  , spec "hovered-button-shrink-damage" runHoveredButtonShrinkDamageTest
  , spec "root-overflow-grow-damage" (runRootOverflowDamageTest 40 20)
  , spec "root-overflow-shrink-damage" (runRootOverflowDamageTest 20 30)
  ]

-- | Far-apart rects stay apart, near ones merge, and a frame never has more
-- than four pieces or just one.
runDamagePiecesMergeTest :: Context -> IORef Int -> IO ()
runDamagePiecesMergeTest _ failed = do
  let a = Rect 0 0 20 10
      b = Rect 500 400 20 10
  assertEq failed (length (damagePieces [a, b])) 2
  -- Within the merge gap: one piece, which is the bounding box itself.
  assertEq failed (damagePieces [a, Rect 30 0 20 10]) []
  assertEq failed (damagePieces [a]) []
  assertEq failed (damagePieces []) []
  -- A third rect joining two pieces' gap merges them all.
  assertEq failed (damagePieces [a, Rect 40 0 20 10, Rect 20 0 20 10]) []
  let scattered = [Rect (fromIntegral i * 100) (fromIntegral i * 80) 10 10 | i <- [0 .. 7 :: Int]]
      pieces = damagePieces scattered
  assert failed (length pieces == 4)
  -- Every rect lies inside a piece, and the pieces are disjoint.
  assert failed (all (\r -> any (`covers` r) pieces) scattered)
  assert failed (and [rectIntersect p q == Nothing | (i, p) <- zip [0 :: Int ..] pieces, (j, q) <- zip [0 ..] pieces, i < j])

-- | Two labels changing in opposite corners repaint as two pieces, and each
-- draw command lies inside one of them.
runFarLabelsDamagePiecesTest :: Context -> IORef Int -> IO ()
runFarLabelsDamagePiecesTest ctx failed = do
  let inp = withInput 800 600
      ui k = columnWith (fillW . fillH) $ do
        label (tshow k)
        flex
        rowWith fillW $ flex >> label (tshow k)
      tshow = T.pack . show :: Int -> T.Text
  _ <- warmup2 ctx inp (ui 1)
  writeIORef (ctxPaintFull ctx) False
  (_, _, dd, _) <- runFrame ctx inp (ui 2)
  dmg <- takeDamage ctx
  pieces <- takeDamagePieces ctx
  assert failed (case dmg of DamageClip _ -> True; DamageFull -> False)
  assertEq failed (length pieces) 2
  let clipOf c = Rect (cmdClipX c) (cmdClipY c) (cmdClipW c) (cmdClipH c)
      inPiece c = any ((`covers` clipOf c) . rectInflate 1) pieces
  assert failed (not (null (drawCmdElems dd)) && all inPiece (drawCmdElems dd))
  writeIORef (ctxPaintFull ctx) True

-- | A new version on a versioned drawing repaints its rect. Paint rebuilds the
-- ops once the version moves, and nothing else damages them, so a clip frame
-- would otherwise keep the pixels it drew last time.
runVersionedDrawingDamageTest :: Context -> IORef Int -> IO ()
runVersionedDrawingDamageTest ctx failed = do
  let inp = withInput 400 300
      ui version = column $ do
        _ <- label "Other"
        drawingVersioned version (fixedWH 80 40) $ \r ->
          smallArrayFromList [FillRect r (colorRGBA 255 0 0 255)]
  resp <- warmup2 ctx inp (ui 1)
  _ <- takeDamage ctx
  _ <- runFrame ctx inp (ui 2)
  dmg <- takeDamage ctx
  assert failed (clipCovers dmg (respRect resp))

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
  assertEq failed (resolveDamageRect (DamageUnion (DamageInflated 4.0) (DamageInflated 8.0)) base)
    (rectUnion (Rect 6 16 108 58) (Rect 2 12 116 66))
  -- DamageNone is the identity of a union rather than a rect at the origin.
  assertEq failed (resolveDamageRect (DamageUnion DamageSelf DamageNone) base) base
  assertEq failed (resolveDamageRect (DamageUnion DamageNone (DamageExact base)) (Rect 0 0 0 0)) base

runExplicitDamageWidgetTest :: Context -> IORef Int -> IO ()
runExplicitDamageWidgetTest ctx failed = do
  let inp = withInput 400 300
      ui = columnWith (padAll 20) $ do
        w1 <- button' "First"
        w2 <- button' "Second"
        pure (w1, w2)
  -- Warmup to establish solved layout rects
  (w1, _) <- warmup2 ctx inp ui
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

runDamageQueueClearedPerFrameTest :: Context -> IORef Int -> IO ()
runDamageQueueClearedPerFrameTest ctx failed = do
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

  -- Explicit full-window damage, again only for its own frame
  let fullDamagedUi = column $ do
        damageFullNow
        label "Static content"
  _ <- runFrame ctx inp fullDamagedUi
  dmg3 <- takeDamage ctx
  assertEq failed dmg3 DamageFull
  _ <- runFrame ctx inp ui
  dmg4 <- takeDamage ctx
  assert failed (damageIsEmpty dmg4)

runStateChangeDamageTest :: Context -> IORef Int -> IO ()
runStateChangeDamageTest ctx failed = do
  let inp0 = withInput 400 300
      ui = do
        (name, setName) <- useText ""
        row $ do
          label ("Left pane: " <> name)
          setName =<< textInput name

  -- Warm up and focus textInput via Tab
  warmupFocused ctx inp0 ui
  _ <- takeDamage ctx

  -- Type a character into focused textInput
  _ <- runFrame ctx (inp0 {inputChars = "a"}) ui
  dmg <- takeDamage ctx
  assertEq failed dmg DamageFull

runOrphanAnimationDamageSettlesTest :: Context -> IORef Int -> IO ()
runOrphanAnimationDamageSettlesTest ctx failed = do
  let winInp = withInput 400 300
      inp = winInp {inputDeltaTime = 0.05}
      withBar = columnWith (padAll 20) $ do
        bar <- currentId
        spacer (Fixed 40) (Fixed 20)
        pure bar
      withoutBar = columnWith (padAll 20) (pure ())
  -- Warm up: the bar widget occupies a nonzero 40x20 rect in the arena.
  (wid, _, _, _) <- runFrame ctx inp withBar
  _ <- takeDamage ctx
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

-- | A clip frame repaints its region from the window backdrop, as a full frame
-- repaints from the cleared window. An idle menu-bar title has no fill, so
-- without the backdrop the hover highlight it just lost would stay in the
-- retain texture.
runClipFrameBackdropTest :: Context -> IORef Int -> IO ()
runClipFrameBackdropTest ctx failed = do
  writeIORef (ctxPaintFull ctx) False
  let inp0 = withInputOff 400 300
      ui = rowWith (tight . fillW . fixedH 28) $ do
        file <- menuButton' "File" False
        _ <- menuButton' "Edit" False
        pure file
  file <- warmup2 ctx inp0 ui
  _ <- runFrame ctx inp0 {inputMousePos = centerOf file} ui
  (_, _, draw, _) <- runFrame ctx inp0 ui
  dmg <- takeDamage ctx
  theme <- readIORef (ctxTheme ctx)
  quads <- drawQuads draw
  case dmg of
    DamageClip clip -> do
      assert failed (covers clip (respRect file))
      assertJust failed (listToMaybe quads) $ \(r, c) -> do
        assert failed (covers r clip)
        assertEq failed c (themeWindow theme)
    DamageFull -> assert failed False

-- | Ctrl+A repaints the text area on the frame that selects, rather than
-- leaving the highlight to a follow-up frame.
runTextAreaSelectAllDamageTest :: Context -> IORef Int -> IO ()
runTextAreaSelectAllDamageTest ctx failed = do
  -- Frame time lets the hover fade from the click finish; a live fade would
  -- damage the area anyway.
  let inp0 = (withInputOff 800 600) {inputDeltaTime = 0.5}
      ui = column $ do
        _ <- label "Notes"
        fst <$> textAreaWith' (fixedWH 200 80) "hello world"
  area <- warmup2 ctx inp0 ui
  _ <- runClick ctx inp0 ui (centerOf area)
  _ <- warmup2 ctx inp0 ui
  _ <- takeDamage ctx
  _ <- runFrame ctx (chordInp (ctrl <> key 'a') inp0) ui
  dmg <- takeDamage ctx
  assert failed (clipCovers dmg (respRect area))

-- | A local-hook write that changes a label's text and, elsewhere, a box's
-- colour repaints the whole window: the text diff alone would clip to the
-- label and leave the box showing its old colour, which no diff describes.
runHookPaintChangeDamageTest :: Context -> IORef Int -> IO ()
runHookPaintChangeDamageTest ctx failed = do
  let ui = do
        (on, setOn) <- useFlag False
        resp <- button' "Toggle"
        when (respClicked resp) (setOn (not on))
        -- Same length in a monospace font: the label keeps its rect, so only
        -- the text diff sees the change.
        void $ labelWith fontMono (if on then "one" else "two")
        box (fixedWH 40 40) (if on then colorRGBA 255 0 0 255 else colorRGBA 0 0 255 255)
        pure resp
      inp0 = withInputOff 320 240
  resp <- warmup2 ctx inp0 ui
  _ <- runClick ctx inp0 ui (centerOf resp)
  dmg <- takeDamage ctx
  assertEq failed dmg DamageFull

-- | A 'useFloat' write that only changes a box's colour repaints the whole
-- window. The hook's key names no widget and floats are outside the store
-- diff, so only the hook write itself says the box needs repainting.
runFloatHookPaintChangeDamageTest :: Context -> IORef Int -> IO ()
runFloatHookPaintChangeDamageTest ctx failed = do
  let ui = do
        (level, setLevel) <- useFloat 0
        resp <- button' "Raise"
        when (respClicked resp) (setLevel (level + 1))
        box (fixedWH 40 40) (if level > 0 then colorRGBA 255 0 0 255 else colorRGBA 0 0 255 255)
        pure resp
      inp0 = withInputOff 320 240
  resp <- warmup2 ctx inp0 ui
  _ <- runClick ctx inp0 ui (centerOf resp)
  dmg <- takeDamage ctx
  assertEq failed dmg DamageFull

-- | Run @warm@ frames of @ui from@ and one of @ui to@, which lay out a node at
-- @rect from@ and @rect to@ (as @holds@ checks): the last repaints both rects.
widthChange :: Int -> (Rect -> [Rect] -> Bool) -> Input -> (Float -> NanoUI a) -> (Float -> Rect) -> Float -> Float -> Context -> IORef Int -> IO ()
widthChange warm holds inp ui rect from to ctx failed = do
  replicateM_ warm (runFrame ctx inp (ui from))
  assert failed . holds (rect from) =<< arenaRects ctx
  _ <- takeDamage ctx
  _ <- runFrame ctx inp (ui to)
  assert failed . holds (rect to) =<< arenaRects ctx
  assert failed . (`damageCovers` rect (max from to)) =<< takeDamage ctx

-- | A panel, which paints but has no widget id, repaints its old and new rects,
-- with the pointer on its button (whose hover fade keeps the frame clipped) or away.
runPanelResizeDamageTest :: Bool -> Float -> Float -> Context -> IORef Int -> IO ()
runPanelResizeDamageTest hovered =
  widthChange 2 elem (withInput 300 200) {inputMousePos = if hovered then V2 10 10 else V2 (-10) (-10)}
    (\w -> panelWith (fixedWH w 40) (void (buttonWith (fixedWH 30 20) ""))) (\w -> Rect 0 0 w 40)

-- | A scroller repaints its old and new rects whole, well and bar lane included,
-- not only the viewport it clips its hovered content to.
runScrollerResizeDamageTest :: Float -> Float -> Context -> IORef Int -> IO ()
runScrollerResizeDamageTest =
  widthChange 2 elem (withInput 300 200) {inputMousePos = V2 10 10}
    (\w -> column (scrollWith (fixedWH w 60) (replicateM_ 6 (buttonWith (fixedWH 30 20) "")))) (\w -> Rect 3 3 w 60)

-- | A panel that stays put adds no damage to a frame where nothing changed.
runPanelSteadyNoDamageTest :: Context -> IORef Int -> IO ()
runPanelSteadyNoDamageTest ctx failed = do
  let ui = column (panelWith (fixedWH 100 40) (void (buttonWith (fixedWH 30 20) "")) >> panelWith (fixedWH 80 40) (label "second"))
  _ <- warmup2 ctx (withInputOff 300 200) ui >> takeDamage ctx
  _ <- runFrame ctx (withInputOff 300 200) ui
  assertEq failed (DamageClip (Rect 0 0 0 0)) =<< takeDamage ctx

-- | A shrinking root drawing repaints the strip it vacated, not clipping its old
-- rect to its new one; the pointer's hover fade keeps the frame clipped.
runDrawingShrinkDamageTest :: Context -> IORef Int -> IO ()
runDrawingShrinkDamageTest =
  widthChange 1 (\r -> (== [r])) (withInput 200 100) (`redDrawing` 20) (\w -> Rect 0 0 w 20) 40 20

-- | So does a hovered button shrinking with its column.
runHoveredButtonShrinkDamageTest :: Context -> IORef Int -> IO ()
runHoveredButtonShrinkDamageTest =
  widthChange 2 elem (withInput 200 100) {inputMousePos = V2 5 5} (\w -> column (buttonWith (fixedWH w 20) "")) (\w -> Rect 3 3 w 20) 40 20

-- | A drawing overflowing a short root repaints all it drew when it shrinks to
-- @dh@ and the root goes to @h@: the root clips to the window, not its own rect.
runRootOverflowDamageTest :: Float -> Float -> Context -> IORef Int -> IO ()
runRootOverflowDamageTest h dh ctx failed = do
  let inp = (withInput 200 100) {inputMousePos = V2 1 1}
      ui rootH drawH = columnWith (fixedWH 40 rootH . padAll 0) (redDrawing 40 drawH)
  _ <- warmup2 ctx inp (ui 20 40) >> takeDamage ctx
  _ <- runFrame ctx inp (ui h dh)
  assert failed . (`damageCovers` Rect 0 0 40 40) =<< takeDamage ctx

redDrawing :: Float -> Float -> NanoUI ()
redDrawing w h = void (drawing (fixedWH w h) (\r -> smallArrayFromList [FillRect r (colorRGBA 255 0 0 255)]))
