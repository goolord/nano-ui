module Cases.LayoutFlow (tests) where

import Spec
import Data.List (findIndex)
import NanoUI.Adornment qualified as A
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (GeomCol, StyleCol, TagCol, TreeCol, geomStride, getClipRect, styleStride, tagStride, treeStride)

tests :: [Spec]
tests =
  [ spec "layout-flow-rects" runLayoutFlowRectsTest
  , spec "stack-text-wrap" runStackTextWrapTest
  , spec "wrap-refit-in-column" runWrapRefitInColumnTest
  , spec "wrap-in-scroller" runWrapInScrollerTest
  , spec "pin-in-scroller" runPinInScrollerTest
  , spec "layer-paint-order" runLayerPaintOrderTest
  , spec "pin-outside-parent-paint" runPinOutsideParentPaintTest
  , spec "layer-hit-order" runLayerHitOrderTest
  , spec "stack-field-focus" runStackFieldFocusTest
  , spec "layout-flow-damage" runLayoutFlowDamageTest
  , spec "scroll-ignores-flow" runScrollIgnoresFlowTest
  , spec "arena-columns-fit-strides" runArenaColumnsFitStridesTest
  , spec "covered-widget-no-pointer" runCoveredWidgetNoPointerTest
  , spec "pointer-modes" runPointerModesTest
  , spec "pointer-covers-any-id" runPointerCoversAnyIdTest
  ]

red, green, blue, yellow :: Color
red = colorRGBA 255 0 0 255
green = colorRGBA 0 255 0 255
blue = colorRGBA 0 0 255 255
yellow = colorRGBA 255 255 0 255

input0 :: Input
input0 = withInputOff 400 300

-- | The rects of nodes @is@ in the last frame, in arena order.
rectsAt :: Context -> [Int] -> IO [Rect]
rectsAt ctx is = (\rs -> [r | (i, r) <- zip [0 ..] rs, i `elem` is]) <$> arenaRects ctx

-- | Every node's rect but the root's: a stack is as large as its largest child
-- plus padding and places each by its alignment; a wrapping row breaks lines
-- at its width with the gap within and the line gap between them, a child
-- wider than a line alone on one, and a grow child sharing only its line; a
-- wrapping column flows into columns; pinned children sit at their offsets,
-- out of the flow and of the parent's size.
runLayoutFlowRectsTest :: Context -> IORef Int -> IO ()
runLayoutFlowRectsTest ctx failed =
  forM_ cases $ \(ui, want) -> warmup2 ctx input0 ui >> arenaRects ctx >>= assertEq failed want . drop 1
  where
    wrapRow w = columnWith tight $ rowWith (wrap . fixedW w . tight . gap 10 . lineGap 5) $ do
      box (alignBottom . fixedWH 30 10) red
      forM_ [(30, 20), (30, 10), (50, 10), (120, 10), (20, 10)] $ \(bw, bh) -> box (fixedWH bw bh) red
    cases =
      [ ( columnWith tight $ stackWith (padAll 4) $ do
            box (fixedWH 100 60) red
            box (alignEnd . alignBottom . fixedWH 20 10) blue
            box (alignCenter . alignMid . fixedWH 10 10) blue
            box (fillW . fixedH 6) green
            box (alignEnd . percent 50 . fixedH 6) green
            box (fillH . fixedW 5) yellow
        , [Rect 0 0 108 68, Rect 4 4 100 60, Rect 84 54 20 10, Rect 49 29 10 10, Rect 4 4 100 6, Rect 54 4 50 6, Rect 4 4 5 60] )
      , (wrapRow 100, [Rect 0 0 100 65, Rect 0 10 30 10, Rect 40 0 30 20, Rect 0 25 30 10, Rect 40 25 50 10, Rect 0 40 120 10, Rect 0 55 20 10])
      , (wrapRow 200, [Rect 0 0 200 35, Rect 0 10 30 10, Rect 40 0 30 20, Rect 80 0 30 10, Rect 120 0 50 10, Rect 0 25 120 10, Rect 130 25 20 10])
      , ( columnWith tight $ rowWith (wrap . fixedW 100 . tight . gap 0) $ do
            box (fixedWH 60 10) red
            box (fillW . minW 30 . fixedH 10) blue
            box (fixedWH 70 10) green
        , [Rect 0 0 100 20, Rect 0 0 60 10, Rect 60 0 40 10, Rect 0 10 70 10] )
      , ( columnWith tight $ columnWith (wrap . fixedH 100 . tight . gap 0 . lineGap 5) $ replicateM_ 5 (box (fixedWH 20 40) red)
        , [Rect 0 0 70 100, Rect 0 0 20 40, Rect 0 40 20 40, Rect 25 0 20 40, Rect 25 40 20 40, Rect 50 0 20 40] )
      , ( columnWith (tight . gap 0) $ do
            rowWith (padAll 5 . gap 10) $ do
              box (fixedWH 40 40) red
              box (pinAt 30 12 . fixedWH 16 16) blue
              box (fixedWH 40 40) green
              box (pinAt 0 0 . fillW . fixedH 4) yellow
            box (fixedWH 10 10) yellow
        , [Rect 0 0 100 50, Rect 5 5 40 40, Rect 35 17 16 16, Rect 55 5 40 40, Rect 5 5 90 4, Rect 0 50 10 10] )
      ]

-- | A label in a fixed-width stack wraps to its width, and the stack and
-- what follows it make room for all its lines.
runStackTextWrapTest :: Context -> IORef Int -> IO ()
runStackTextWrapTest ctx failed = do
  _ <- warmup2 ctx input0 . columnWith tight $ do
    stackWith (fixedW 100 . tight) $ label "one two three four five six seven eight nine ten"
    label "x"
  arenaRects ctx >>= \case
    [_, s, long, short] ->
      mapM_ (assert failed)
        [ rectW long <= 100
        , rectH long >= 2 * rectH short
        , rectY s + rectH s >= rectY long + rectH long
        , rectY short >= rectY long + rectH long
        ]
    rs -> assertEq failed 4 (length rs)

-- | A wrapping row that nothing bounds wraps at its column's width, which
-- refits its height, and a wider window reflows it onto one line, repainting
-- everything.
runWrapRefitInColumnTest :: Context -> IORef Int -> IO ()
runWrapRefitInColumnTest ctx failed = do
  let ui = columnWith (tight . gap 0) $ do
        rowWith (wrap . tight . gap 0) (replicateM_ 10 (box (fixedWH 50 10) red))
        box (fixedWH 10 10) blue
  _ <- warmup2 ctx (withInputOff 220 300) ui
  rectsAt ctx [1, 11, 12] >>= assertEq failed [Rect 0 0 220 30, Rect 50 20 50 10, Rect 0 30 10 10]
  _ <- takeDamage ctx
  warmup ctx (withInputOff 600 300) ui
  takeDamage ctx >>= \d -> assert failed (case d of DamageFull -> True; DamageClip _ -> False)
  rectsAt ctx [1, 12] >>= assertEq failed [Rect 0 0 500 10, Rect 0 10 10 10]

-- | A wrapping row in a scroller wraps inside the viewport, and the scroller
-- reaches its last line.
runWrapInScrollerTest :: Context -> IORef Int -> IO ()
runWrapInScrollerTest ctx failed = do
  let ui = columnWith tight . scrollArea (fixedWH 120 50 . tight) $
        rowWith (wrap . fillW . tight . gap 0) (replicateM_ 12 (box (fixedWH 40 20) red))
      bottom b = rectY b + rectH b
  (sid, _) <- warmup2 ctx input0 ui
  r : boxes <- drop 2 <$> arenaRects ctx
  assertEq failed 12 (length boxes)
  assert failed (all (\b -> rectX b + rectW b <= 120) boxes && rectH r >= 80)
  assertEq failed (bottom r) (maximum (map bottom boxes))
  assertJustM failed (getScrollMetrics ctx sid) $ \m -> assertEq failed (rectH r - 50) (v2Y (scrollRange m))
  setScrollOffset ctx sid (rectH r - 50)
  warmup ctx input0 ui
  arenaRects ctx >>= assertEq failed 50 . bottom . last

-- | A pinned child in a scroller extends its range, scrolls with the content
-- and is clipped to the viewport.
runPinInScrollerTest :: Context -> IORef Int -> IO ()
runPinInScrollerTest ctx failed = do
  let ui = columnWith tight . scrollArea (fixedWH 100 50 . tight) $ do
        box (fixedWH 80 20) red
        box (pinAt 0 200 . fixedWH 30 30) blue
  (sid, _) <- warmup2 ctx input0 ui
  assertJustM failed (getScrollMetrics ctx sid) $ \m -> assertEq failed 180 (v2Y (scrollRange m))
  setScrollOffset ctx sid 180
  warmup ctx input0 ui
  rectsAt ctx [2, 3] >>= assertEq failed [Rect 0 (-180) 80 20, Rect 0 20 30 30]
  assertJustM failed (getClipRect (ctxNodeArena ctx) 3) (assert failed . covers (Rect 0 0 100 50))

-- | Paint draws the first colour before the second: a row's earlier child over
-- a later one, a stack's later child over an earlier one, and a pinned child
-- over its siblings, even one declared before it or, in a stack, after it.
runLayerPaintOrderTest :: Context -> IORef Int -> IO ()
runLayerPaintOrderTest ctx failed =
  forM_ cases $ \(ui, under, over) -> do
    quads <- drawQuads . snd =<< warmupDraw ctx input0 ui
    let at col = findIndex ((== col) . snd) quads
    assertJust failed ((,) <$> at under <*> at over) $ \(u, o) -> assert failed (u < o)
  where
    sq n = fixedWH n n
    cases =
      [ (rowWith tight (box (sq 60) green >> box (sq 20) yellow), yellow, green)
      , (stackWith tight (box (sq 60) red >> box (sq 20) blue), red, blue)
      , (rowWith tight (box (sq 40) red >> box (pinAt 10 10 . sq 10) blue), red, blue)
      , (stackWith tight (box (pinAt 5 5 . sq 10) blue >> box (sq 60) red), red, blue)
      ]

-- | Paint finds a pinned child outside its parent: under a stack of no size,
-- and on a frame that repaints only the pinned child.
runPinOutsideParentPaintTest :: Context -> IORef Int -> IO ()
runPinOutsideParentPaintTest ctx failed = do
  let ui version = columnWith (tight . gap 0 . fillW) $ do
        stackWith tight $ box (pinAt 10 10 . fixedWH 20 20) green
        rowWith (tight . fixedWH 50 20) $
          drawingVersioned version (pinAt 200 0 . fixedWH 20 20) $ \r ->
            runCanvas (drawRect r (if version == 1 then red else blue))
      drawn col draw = any ((== col) . snd) <$> drawQuads draw
  (_, full) <- warmupDraw ctx input0 (ui 1)
  forM_ [green, red] $ \col -> assert failed =<< drawn col full
  writeIORef (ctxPaintFull ctx) False
  _ <- takeDamage ctx
  (_, _, draw, _) <- runFrame ctx input0 (ui 2)
  takeDamage ctx >>= \d -> assert failed (case d of DamageClip _ -> True; DamageFull -> False)
  assert failed =<< drawn blue draw
  writeIORef (ctxPaintFull ctx) True

-- | The pointer is on the button drawn on top: a stack's later child, a
-- pinned child (in a stack too, declared first), and each button of a stack
-- pinned over an earlier one, while beside them the button under them has
-- it. Each case checks its buttons' rects, then that each point hovers and
-- clicks only the button at its index.
runLayerHitOrderTest :: Context -> IORef Int -> IO ()
runLayerHitOrderTest ctx failed =
  forM_ cases $ \(ui, check) -> do
    bs <- warmup2 ctx input0 ui
    let (rects, points) = check bs
    forM_ rects $ \(i, r) -> assertEq failed r (respRect (bs !! i))
    forM_ points $ \(p, want) -> do
      warmup ctx input0 {inputMousePos = p} ui
      getHotId ctx >>= assertEq failed (respId (bs !! want))
      clicked <- runClick ctx input0 {inputMousePos = p} ui p
      assertEq failed [i == want | i <- [0 .. length bs - 1]] (map respClicked clicked)
  where
    btn f w h = buttonWith' (f . fixedWH w h) "b"
    cases =
      [ (stack (sequence [btn id 120 40, btn id 120 40]), \bs -> ([(1, respRect (bs !! 0))], [(centerOf (bs !! 1), 1)]))
      , (rowWith tight (sequence [btn id 120 40, btn (pinAt 20 10) 40 20]), const ([(1, Rect 20 10 40 20)], [(V2 40 20, 1), (V2 100 20, 0)]))
      , (stackWith tight (sequence [btn (pinAt 10 10) 40 20, btn id 120 60]), const ([(0, Rect 10 10 40 20)], [(V2 30 20, 0)]))
      , ( rowWith tight ((:) <$> btn id 200 100 <*> stackWith (pinAt 10 10 . tight) (sequence [btn id 50 50, btn id 20 20]))
        , const ([(1, Rect 10 10 50 50), (2, Rect 10 10 20 20)], [(V2 15 15, 2), (V2 40 40, 1), (V2 150 50, 0)]) )
      ]

-- | A button stacked over a text field takes the press, keeps the field's
-- focus as it was and shows no text cursor, while the field's uncovered part
-- focuses it; a field stacked over a button takes focus, and the button does
-- not click.
runStackFieldFocusTest :: Context -> IORef Int -> IO ()
runStackFieldFocusTest ctx failed = do
  let sized w = defaultTextInputConfig {ticLayout = fixedW w (ticLayout defaultTextInputConfig)}
      field w t = fst <$> textInputConfigured' (sized w) t
      ui = column $ do
        (f, cover) <- stack $ (,) <$> field 200 "under" <*> buttonWith' (alignEnd . fixedWH 40 20) "x"
        (under, top) <- stack $ (,) <$> buttonWith' (fixedWH 200 30) "under" <*> field 100 "over"
        pure (f, cover, under, top)
  (field0, cover0, _, top0) <- warmup2 ctx input0 ui
  let press p = runClick ctx input0 {inputMousePos = p} ui p
      Rect fx fy _ fh = respRect field0
      onField = V2 (fx + 20) (fy + fh / 2)
      focusAfter p = press p >> getFocusId ctx
  cursorOver ctx input0 ui (centerOf cover0) >>= \k -> assert failed (k /= UiCursorText)
  cursorOver ctx input0 ui onField >>= assertEq failed UiCursorText
  (_, cover1, _, _) <- press (centerOf cover0)
  assert failed (respClicked cover1)
  getFocusId ctx >>= assertEq failed (WidgetId 0)
  focusAfter onField >>= assertEq failed (respId field0)
  focusAfter (centerOf cover0) >>= assertEq failed (respId field0)
  (_, _, under2, _) <- press (centerOf top0)
  assert failed (not (respClicked under2))
  getFocusId ctx >>= assertEq failed (respId top0)

-- | Changing only a pin's offset, whether a row wraps, or its line gap lays
-- the frame out again, and repaints where the moved children were and are.
runLayoutFlowDamageTest :: Context -> IORef Int -> IO ()
runLayoutFlowDamageTest ctx failed = do
  let ui x wrapped lg = columnWith (tight . gap 0) $ do
        rowWith (tight . fixedWH 200 60) $ do
          box (fixedWH 50 50) green
          box (pinAt x 10 . fixedWH 20 20) blue
        rowWith ((if wrapped then wrap else id) . lineGap lg . fixedW 100 . tight . gap 0) $ do
          box (fixedWH 70 10) red
          box (fixedWH 40 10) yellow
      step view moved repainted = do
        warmup ctx input0 view
        dmg <- takeDamage ctx
        rectsAt ctx (map fst moved) >>= assertEq failed (map snd moved)
        forM_ repainted $ assert failed . damageCovers dmg
  _ <- warmup2 ctx input0 (ui 10 False 0)
  writeIORef (ctxPaintFull ctx) False
  _ <- takeDamage ctx
  step (ui 120 False 0) [(3, Rect 120 10 20 20)] [Rect 10 10 20 20, Rect 120 10 20 20]
  -- The yellow box moves from the end of the row to a line of its own.
  step (ui 120 True 0) [(4, Rect 0 60 100 20), (6, Rect 0 70 40 10)] [Rect 70 60 40 10, Rect 0 70 40 10]
  step (ui 120 True 5) [(4, Rect 0 60 100 25), (6, Rect 0 75 40 10)] [Rect 0 75 40 10]
  writeIORef (ctxPaintFull ctx) True

-- | A scroll container lays its children out one after another even when
-- its layout asks for a stack or for wrapping, and scrolls over all of them.
runScrollIgnoresFlowTest :: Context -> IORef Int -> IO ()
runScrollIgnoresFlowTest ctx failed = do
  let stacked l = l {layoutDirection = Stack}
  (sid, _) <- warmup2 ctx input0 . columnWith tight . scrollArea2D (stacked . wrap . fixedWH 100 50 . tight . gap 0) $
    replicateM_ 3 (box (fixedWH 40 20) red)
  arenaRects ctx >>= assertEq failed [0, 20, 40] . map rectY . drop 2
  assertJustM failed (getScrollMetrics ctx sid) $ \m -> assertEq failed 10 (v2Y (scrollRange m))

-- | Every column of the arena's per-node rows fits the row's stride, or it
-- would write into the next node's row.
runArenaColumnsFitStridesTest :: Context -> IORef Int -> IO ()
runArenaColumnsFitStridesTest _ failed =
  mapM_ (assert failed)
    [ columns (minBound :: GeomCol) <= geomStride
    , columns (minBound :: StyleCol) <= styleStride
    , columns (minBound :: TagCol) <= tagStride
    , columns (minBound :: TreeCol) <= treeStride
    ]
  where
    columns :: (Enum c, Bounded c) => c -> Int
    columns c = fromEnum (maxBound `asTypeOf` c) + 1

-- | Where a pinned or stacked button is drawn over another widget, that
-- widget has no pointer there: not hovered, not pressed, its tooltip shut, a
-- slider not moved, a select not opened. Beside the button it has the
-- pointer, and the widgets the button is inside keep it.
runCoveredWidgetNoPointerTest :: Context -> IORef Int -> IO ()
runCoveredWidgetNoPointerTest ctx failed = do
  let inp = withInputOff 500 300
      at p = inp {inputMousePos = p}
      -- Hover @p@, then press, hold and release there: the press frame's
      -- result and the release frame's.
      pressThrough :: NanoUI a -> V2 -> IO (a, a)
      pressThrough ui p = do
        warmup ctx (at p) ui
        pressed <- evalUi ctx (pressAt inp p) ui
        warmup ctx (holdAt inp p) ui
        (pressed,) <$> evalUi ctx (releaseAt (holdAt inp p)) ui
  -- A drawing with a tooltip, under a pinned button.
  let covered = columnWith tight $ do
        area <- drawing (fixedWH 200 100) (const mempty)
        tooltipConfigured defaultTooltipConfig {tooltipDelay = 0} area "Canvas tip"
        (area,) <$> buttonWith' (pinAt 10 10 . fixedWH 60 30) "pin"
      tipShown p = warmup ctx (at p) covered >> hasText "Canvas tip" <$> collectOverlayTextSpans ctx (at p)
      beside = V2 150 70
  (area0, onPin) <- fmap centerOf <$> warmup2 ctx inp covered
  warmup ctx (at onPin) covered
  (area1, b1) <- evalUi ctx (at onPin) covered
  assert failed (not (respHovered area1) && respHovered b1)
  ((area2, b2), _) <- pressThrough covered onPin
  assert failed (not (respHovered area2 || respPressed area2) && respPressed b2)
  assert failed . not =<< tipShown onPin
  assert failed =<< tipShown beside
  ((area3, _), _) <- pressThrough covered beside
  assert failed (respPressed area3 && rectContains (respRect area0) beside)
  -- A slider under a pinned button stays where it was.
  valueRef <- newIORef (0 :: Float)
  let sliders = columnWith (tight . fixedW 300) $ do
        _ <- held valueRef (sliderWith' id 0 100)
        buttonWith' (pinAt 200 0 . fixedWH 60 20) "x"
  (_, b4) <- pressThrough sliders . centerOf =<< warmup2 ctx inp sliders
  assert failed (respClicked b4)
  readIORef valueRef >>= assertEq failed 0
  -- A select under a pinned button stays shut.
  let selects = columnWith tight $ do
        _ <- selectWith (fixedW 200) ["Alpha", "Beta", "Gamma"] 0
        buttonWith' (pinAt 120 0 . fixedWH 60 20) "y"
  onB5 <- centerOf <$> warmup2 ctx inp selects
  _ <- pressThrough selects onB5
  warmup ctx (at onB5) selects
  assert failed . not . hasText "Gamma" =<< collectOverlayTextSpans ctx (at onB5)
  -- A chip stacked over a drawing, with a control among its adornments. The
  -- chip and its stack keep the pointer on its label, and the control takes
  -- the click on itself; the drawing is neither hovered nor pressed.
  removes <- newIORef (0 :: Int)
  let remove = A.trailing (A.control (whenM (buttonWith tight "x") (uiIO (modifyIORef' removes (+ 1)))))
      layered = columnWith tight $ stackWith tight $ do
        under <- drawing (fixedWH 200 100) (const mempty)
        (under,) <$> buttonConfigured' defaultButtonConfig {bcAdornments = remove} "Chip"
  _ <- warmup2 ctx inp layered
  spans <- collectTextSpans ctx
  assertJust failed ((,) <$> spanRectOf "Chip" spans <*> spanRectOf "x" spans) $ \(chipR, xR) -> do
    warmup ctx (at (spanCenter chipR)) layered
    (under1, chip1) <- evalUi ctx (at (spanCenter chipR)) layered
    assert failed (respHovered chip1 && not (respHovered under1))
    ((under2, _), (_, chip3)) <- pressThrough layered (spanCenter xR)
    assert failed (not (respHovered under2 || respPressed under2) && not (respClicked chip3))
    readIORef removes >>= assertEq failed 1

-- | 'pointer' decides what a node drawn over others does with the pointer. A
-- pinned panel lets it through to the button beneath by default, and takes
-- it with 'PointerBlock': the button is neither hovered nor clicked, and
-- nothing is hot, while a control inside the panel still takes its presses
-- and the button beside the panel its own. A text field under a blocking
-- panel is not focused by a press on the panel, nor shows the text cursor
-- there. A drawing pinned over a
-- button takes the pointer by default and lets it through with
-- 'PointerPass', taking no hover itself.
runPointerModesTest :: Context -> IORef Int -> IO ()
runPointerModesTest ctx failed = do
  let over mode = columnWith tight $ do
        b <- buttonWith' (fixedWH 200 100) "under"
        inner <- panelWith (pointer mode . pinAt 10 10 . fixedWH 120 60) (buttonWith' (fixedWH 40 20) "in")
        pure (b, inner)
      onPanel = V2 100 60
      at p = input0 {inputMousePos = p}
      clickAt ui p = runClick ctx (at p) ui p
  forM_ [(PointerAuto, True), (PointerBlock, False)] $ \(mode, through) -> do
    (b0, inner0) <- warmup2 ctx input0 (over mode)
    warmup ctx (at onPanel) (over mode)
    (b1, _) <- evalUi ctx (at onPanel) (over mode)
    assertEq failed through (respHovered b1)
    getHotId ctx >>= assertEq failed (if through then respId b0 else WidgetId 0)
    (b2, _) <- clickAt (over mode) onPanel
    assertEq failed through (respClicked b2)
    (_, inner2) <- clickAt (over mode) (centerOf inner0)
    assert failed (respClicked inner2)
    (b3, _) <- clickAt (over mode) (V2 190 90)
    assert failed (respClicked b3)
  -- A press on a blocking panel over a text field leaves it unfocused.
  let field mode = columnWith tight $ do
        f <- fst <$> textInputConfigured' defaultTextInputConfig {ticLayout = fixedW 200 (ticLayout defaultTextInputConfig)} "text"
        panelWith (pointer mode . pinAt 60 0 . fixedWH 80 20) (pure ())
        pure f
  forM_ [(PointerAuto, True), (PointerBlock, False)] $ \(mode, focused) -> do
    f0 <- warmup2 ctx input0 (field mode)
    let Rect fx fy _ _ = respRect f0
        p = V2 (fx + 100) (fy + 10)
    cursorOver ctx input0 (field mode) p >>= assertEq failed focused . (== UiCursorText)
    _ <- runClick ctx (at p) (field mode) p
    getFocusId ctx >>= assertEq failed (if focused then respId f0 else WidgetId 0)
    _ <- runClick ctx (at (V2 390 290)) (field mode) (V2 390 290)
    pure ()
  -- A drawing over a button: it takes the pointer, or lets it through.
  let drawn mode = columnWith tight $ do
        b <- buttonWith' (fixedWH 200 100) "under"
        d <- drawing (pointer mode . pinAt 20 20 . fixedWH 100 50) (const mempty)
        pure (b, d)
      onDrawing = V2 60 40
  forM_ [(PointerAuto, False), (PointerPass, True)] $ \(mode, through) -> do
    _ <- warmup2 ctx input0 (drawn mode)
    warmup ctx (at onDrawing) (drawn mode)
    (b1, d1) <- evalUi ctx (at onDrawing) (drawn mode)
    assertEq failed (through, not through) (respHovered b1, respHovered d1)
    (b2, d2) <- clickAt (drawn mode) onDrawing
    assertEq failed (through, not through) (respClicked b2, respClicked d2)

-- | Where a control is drawn over a node with an id that is not a control,
-- such as a label, that node is covered too: no hover there, and its
-- tooltip stays shut.
runPointerCoversAnyIdTest :: Context -> IORef Int -> IO ()
runPointerCoversAnyIdTest ctx failed = do
  let ui = columnWith tight $ do
        l <- labelWith' (fixedWH 200 40) "Covered text"
        tooltipConfigured defaultTooltipConfig {tooltipDelay = 0} l "Label tip"
        _ <- buttonWith' (pinAt 100 0 . fixedWH 60 30) "on top"
        pure l
      at p = input0 {inputMousePos = p}
  _ <- warmup2 ctx input0 ui
  forM_ [(V2 130 15, False), (V2 40 15, True)] $ \(p, uncovered) -> do
    warmup ctx (at p) ui
    l <- evalUi ctx (at p) ui
    assertEq failed uncovered (respHovered l)
    warmup ctx (at p) ui
    assertEq failed uncovered . hasText "Label tip" =<< collectOverlayTextSpans ctx (at p)
