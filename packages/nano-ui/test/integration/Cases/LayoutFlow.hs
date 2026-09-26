module Cases.LayoutFlow (tests) where

import Spec
import Data.Primitive.SmallArray (smallArrayFromList)
import Data.List (findIndex)
import NanoUI.Adornment qualified as A
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (GeomCol, StyleCol, TagCol, TreeCol, geomStride, getClipRect, styleStride, tagStride, treeStride)

tests :: [Spec]
tests =
  [ spec "layout-flow-rects" runLayoutFlowRectsTest
  , spec "layers-text-wrap" runLayersTextWrapTest
  , spec "layered-modifier" runLayeredModifierTest
  , spec "wrap-line-align" runWrapLineAlignTest
  , spec "aspect-ratio" runAspectRatioTest
  , spec "wrap-refit-in-column" runWrapRefitInColumnTest
  , spec "wrap-in-scroller" runWrapInScrollerTest
  , spec "pin-in-scroller" runPinInScrollerTest
  , spec "layer-paint-order" runLayerPaintOrderTest
  , spec "pin-outside-parent-paint" runPinOutsideParentPaintTest
  , spec "layer-hit-order" runLayerHitOrderTest
  , spec "layers-field-focus" runLayersFieldFocusTest
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

-- | Non-root rects for each flow. Layers size to their largest child plus
-- padding and place children by alignment. A wrapping row breaks at its
-- width, using the gap within lines and the line gap between them; an
-- oversized child gets its own line and a grow child fills only its line. A
-- wrapping column flows into columns. Pinned children sit at their offset
-- from their aligned position and take no space in the flow or the parent.
runLayoutFlowRectsTest :: Context -> IORef Int -> IO ()
runLayoutFlowRectsTest ctx failed =
  forM_ cases $ \(ui, want) -> warmup2 ctx input0 ui >> arenaRects ctx >>= assertEq failed want . drop 1
  where
    wrapRow w = columnWith tight $ rowWith (wrap . fixedW w . tight . gap 10 . lineGap 5) $ do
      box (alignBottom . fixedWH 30 10) red
      forM_ [(30, 20), (30, 10), (50, 10), (120, 10), (20, 10)] $ \(bw, bh) -> box (fixedWH bw bh) red
    cases =
      [ ( columnWith tight $ layersWith (padAll 4) $ do
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
      , ( columnWith (tight . gap 0) $ columnWith (tight . fixedWH 200 100) $ do
            -- A button inset 16 from bottom-right, a badge overhanging
            -- top-right, a bar ending 10 short of the right edge, and a
            -- centred box shifted right.
            box (pinAt (-16) (-16) . alignEnd . alignBottom . fixedWH 40 40) red
            box (pinAt 6 (-6) . alignEnd . alignTop . fixedWH 12 12) blue
            box (pinAt (-10) 0 . alignEnd . fillW . fixedH 4) green
            box (pinAt 10 0 . alignCenter . alignMid . fixedWH 20 10) yellow
        , [Rect 0 0 200 100, Rect 144 44 40 40, Rect 194 (-6) 12 12, Rect 0 0 190 4, Rect 100 45 20 10] )
      ]

-- | 'layered' makes a panel or row stack its children like 'layers': each at
-- its aligned corner of the content box, the container sized to the largest.
-- Text in a layered row wraps as in a column.
runLayeredModifierTest :: Context -> IORef Int -> IO ()
runLayeredModifierTest ctx failed = do
  _ <- warmup2 ctx input0 . columnWith tight $ panelWith (layered . padAll 5) $ do
    box (fixedWH 60 40) red
    box (alignEnd . alignBottom . fixedWH 10 10) blue
  rectsAt ctx [1, 2, 3] >>= assertEq failed [Rect 0 0 70 50, Rect 5 5 60 40, Rect 55 35 10 10]
  _ <- warmup2 ctx input0 . columnWith tight $ rowWith (layered . tight . fixedW 60) $ do
    label "one two three four five six"
    box (alignEnd . fixedWH 10 10) blue
  arenaRects ctx >>= \case
    [_, r, txt, b] -> do
      assertEq failed (Rect 50 0 10 10) b
      assert failed (rectW txt <= 60 && rectH txt > rectH b && rectH r == rectH txt)
    rs -> assertEq failed 4 (length rs)

-- | 'lineAlign' places a wrapping row's lines at the start, centre or end of
-- its width (a column's, of its height). A line filled by a grow child does
-- not move.
runWrapLineAlignTest :: Context -> IORef Int -> IO ()
runWrapLineAlignTest ctx failed = do
  let chips a = columnWith tight $ rowWith (wrap . lineAlign a . fixedW 100 . tight . gap 10 . lineGap 0) $ do
        replicateM_ 4 (box (fixedWH 30 10) red)
        box (fillW . fixedH 10) blue
      xs = fmap (map rectX . drop 2) (arenaRects ctx)
  forM_ [(LinesStart, 0), (LinesCenter, 15), (LinesEnd, 30)] $ \(a, shift) -> do
    _ <- warmup2 ctx input0 (chips a)
    xs >>= assertEq failed [shift, shift + 40, shift, shift + 40, 0]
  _ <- warmup2 ctx input0 . columnWith tight . columnWith (wrap . lineAlign LinesEnd . fixedH 100 . tight . gap 0 . lineGap 5) $
    replicateM_ 3 (box (fixedWH 20 40) red)
  rectsAt ctx [2, 3, 4] >>= assertEq failed [Rect 0 20 20 40, Rect 0 60 20 40, Rect 25 60 20 40]

-- | With 'aspect', a fit height is width / ratio: when filling a column, with
-- a fixed width, sharing a row, and under a maximum. A fit width with a fixed
-- height is height * ratio. A panel keeps the ratio's height regardless of
-- its content.
runAspectRatioTest :: Context -> IORef Int -> IO ()
runAspectRatioTest ctx failed = do
  let sized ui = warmup2 ctx input0 (columnWith (tight . gap 0 . fixedW 300) ui) >> drop 1 <$> arenaRects ctx
  sized (box (fillW . aspect 2) red) >>= assertEq failed [Rect 0 0 300 150]
  sized (box (fixedW 80 . aspect 4) red) >>= assertEq failed [Rect 0 0 80 20]
  sized (box (fixedH 50 . aspect 2) red) >>= assertEq failed [Rect 0 0 100 50]
  sized (box (fillW . aspect 1 . maxH 50) red) >>= assertEq failed [Rect 0 0 300 50]
  sized (rowWith (tight . gap 0 . fillW) (box (fillW . aspect 2) red >> box (fixedWH 100 10) blue))
    >>= assertEq failed [Rect 0 0 300 100, Rect 0 0 200 100, Rect 200 0 100 10]
  sized (panelWith (tight . fillW . aspect 10) (box (fixedWH 20 100) red) >> box (fixedWH 10 10) blue)
    >>= assertEq failed [Rect 0 0 300 30, Rect 0 0 20 100, Rect 0 30 10 10]
  -- A non-positive ratio is ignored.
  sized (box (fixedWH 40 20 . aspect 0) red) >>= assertEq failed [Rect 0 0 40 20]

-- | A label in fixed-width layers wraps to that width, and the layers and
-- the widgets after them make room for every line.
runLayersTextWrapTest :: Context -> IORef Int -> IO ()
runLayersTextWrapTest ctx failed = do
  _ <- warmup2 ctx input0 . columnWith tight $ do
    layersWith (fixedW 100 . tight) $ label "one two three four five six seven eight nine ten"
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

-- | An unbounded wrapping row wraps at its column's width and refits its
-- height. Widening the window reflows it onto one line with a full repaint.
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

-- | A wrapping row in a scroller wraps inside the viewport, and the scroll
-- range reaches its last line.
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

-- | Each case's first colour is painted before its second. A row's earlier
-- child paints over a later one, a later layer over an earlier one, and a
-- pinned child over siblings declared before it, or after it among layers.
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
      , (layersWith tight (box (sq 60) red >> box (sq 20) blue), red, blue)
      , (rowWith tight (box (sq 40) red >> box (pinAt 10 10 . sq 10) blue), red, blue)
      , (layersWith tight (box (pinAt 5 5 . sq 10) blue >> box (sq 60) red), red, blue)
      ]

-- | A pinned child outside its parent is still painted: under zero-size
-- layers, and on a frame that repaints only that child.
runPinOutsideParentPaintTest :: Context -> IORef Int -> IO ()
runPinOutsideParentPaintTest ctx failed = do
  let ui version = columnWith (tight . gap 0 . fillW) $ do
        layersWith tight $ box (pinAt 10 10 . fixedWH 20 20) green
        rowWith (tight . fixedWH 50 20) $
          drawingVersioned version (pinAt 200 0 . fixedWH 20 20) $ \r ->
            smallArrayFromList [FillRect r (if version == 1 then red else blue)]
      drawn col draw = any ((== col) . snd) <$> drawQuads draw
  (_, full) <- warmupDraw ctx input0 (ui 1)
  forM_ [green, red] $ \col -> assert failed =<< drawn col full
  writeIORef (ctxPaintFull ctx) False
  _ <- takeDamage ctx
  (_, _, draw, _) <- runFrame ctx input0 (ui 2)
  takeDamage ctx >>= \d -> assert failed (case d of DamageClip _ -> True; DamageFull -> False)
  assert failed =<< drawn blue draw
  writeIORef (ctxPaintFull ctx) True

-- | The topmost button gets the pointer: a later layer, a pinned child (also
-- among layers when declared first), and each button in pinned layers. Next
-- to them, the button underneath gets it. Each case checks the rects, then
-- that each point hovers and clicks only the expected button.
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
      [ (layers (sequence [btn id 120 40, btn id 120 40]), \bs -> ([(1, respRect (bs !! 0))], [(centerOf (bs !! 1), 1)]))
      , (rowWith tight (sequence [btn id 120 40, btn (pinAt 20 10) 40 20]), const ([(1, Rect 20 10 40 20)], [(V2 40 20, 1), (V2 100 20, 0)]))
      , (layersWith tight (sequence [btn (pinAt 10 10) 40 20, btn id 120 60]), const ([(0, Rect 10 10 40 20)], [(V2 30 20, 0)]))
      , ( rowWith tight ((:) <$> btn id 200 100 <*> layersWith (pinAt 10 10 . tight) (sequence [btn id 50 50, btn id 20 20]))
        , const ([(1, Rect 10 10 50 50), (2, Rect 10 10 20 20)], [(V2 15 15, 2), (V2 40 40, 1), (V2 150 50, 0)]) )
      ]

-- | A button layered over a text field takes the press, leaves focus
-- unchanged and shows no text cursor; the field's uncovered part still
-- focuses it. A field layered over a button takes focus and the button does
-- not click.
runLayersFieldFocusTest :: Context -> IORef Int -> IO ()
runLayersFieldFocusTest ctx failed = do
  let sized w = defaultTextInputConfig {ticLayout = fixedW w (ticLayout defaultTextInputConfig)}
      field w t = fst <$> textInputConfigured' (sized w) t
      ui = column $ do
        (f, cover) <- layers $ (,) <$> field 200 "under" <*> buttonWith' (alignEnd . fixedWH 40 20) "x"
        (under, top) <- layers $ (,) <$> buttonWith' (fixedWH 200 30) "under" <*> field 100 "over"
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

-- | Changing only a pin offset, a row's wrap flag, or its line gap relayouts
-- the frame and repaints the moved children's old and new rects.
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
  -- The yellow box moves onto its own line.
  step (ui 120 True 0) [(4, Rect 0 60 100 20), (6, Rect 0 70 40 10)] [Rect 70 60 40 10, Rect 0 70 40 10]
  step (ui 120 True 5) [(4, Rect 0 60 100 25), (6, Rect 0 75 40 10)] [Rect 0 75 40 10]
  writeIORef (ctxPaintFull ctx) True

-- | A scroll container stacks its children in sequence even when its layout
-- asks for layers or wrapping, and scrolls over all of them.
runScrollIgnoresFlowTest :: Context -> IORef Int -> IO ()
runScrollIgnoresFlowTest ctx failed =
  forM_ [layered, wrap] $ \flow -> do
    (sid, _) <- warmup2 ctx input0 . columnWith tight . scrollArea2D (flow . fixedWH 100 50 . tight . gap 0) $
      replicateM_ 3 (box (fixedWH 40 20) red)
    arenaRects ctx >>= assertEq failed [0, 20, 40] . map rectY . drop 2
    assertJustM failed (getScrollMetrics ctx sid) $ \m -> assertEq failed 10 (v2Y (scrollRange m))

-- | Every arena column fits within its row stride; otherwise it would
-- overwrite the next node's row.
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

-- | A widget under a pinned or layered button gets no pointer there: no
-- hover, press, tooltip, slider movement, or select opening. Beside the
-- button it gets the pointer, and the button's ancestors keep it.
runCoveredWidgetNoPointerTest :: Context -> IORef Int -> IO ()
runCoveredWidgetNoPointerTest ctx failed = do
  let inp = withInputOff 500 300
      at p = inp {inputMousePos = p}
      -- Hover, press, hold and release at @p@; returns the press and
      -- release frames' results.
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
  -- A chip with a control adornment, layered over a drawing. The chip keeps
  -- the pointer on its label, the control takes its own click, and the
  -- drawing is neither hovered nor pressed.
  removes <- newIORef (0 :: Int)
  let remove = A.trailing (A.control (whenM (buttonWith tight "x") (uiIO (modifyIORef' removes (+ 1)))))
      chipOver = columnWith tight $ layersWith tight $ do
        under <- drawing (fixedWH 200 100) (const mempty)
        (under,) <$> buttonConfigured' defaultButtonConfig {bcAdornments = remove} "Chip"
  _ <- warmup2 ctx inp chipOver
  spans <- collectTextSpans ctx
  assertJust failed ((,) <$> spanRectOf "Chip" spans <*> spanRectOf "x" spans) $ \(chipR, xR) -> do
    warmup ctx (at (spanCenter chipR)) chipOver
    (under1, chip1) <- evalUi ctx (at (spanCenter chipR)) chipOver
    assert failed (respHovered chip1 && not (respHovered under1))
    ((under2, _), (_, chip3)) <- pressThrough chipOver (spanCenter xR)
    assert failed (not (respHovered under2 || respPressed under2) && not (respClicked chip3))
    readIORef removes >>= assertEq failed 1

-- | 'pointer' sets how an overlapping node treats the pointer. A pinned panel
-- passes it through by default. With 'PointerBlock' the button beneath is
-- not hovered or clicked and nothing is hot, but controls inside the panel
-- and the uncovered part of the button still work. A press on a blocking
-- panel does not focus the text field under it, and shows no text cursor. A
-- pinned drawing takes the pointer by default and passes it through with
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
  -- A blocking panel over a text field keeps a press from focusing it.
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
  -- A drawing over a button takes the pointer or passes it through.
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

-- | A control drawn over a non-control node with an id, such as a label,
-- covers it too: no hover there, and its tooltip stays closed.
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
