module Cases.LayoutFlow (tests) where

import Spec
import Data.List (findIndex)
import Data.Maybe (isJust)
import Data.Primitive.SmallArray (smallArrayFromList)
import NanoUI.Adornment qualified as A
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (GeomCol, StyleCol, TagCol, TreeCol, geomStride, getClipRect, styleStride, tagStride, treeStride)
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ spec "layout-flow-rects" runLayoutFlowRectsTest
  , spec "layout-text-sizes" runTextSizesTest
  , spec "wrap-refit-in-column" runWrapRefitInColumnTest
  , spec "flow-in-scroller" runFlowInScrollerTest
  , spec "layer-paint-order" runLayerPaintOrderTest
  , spec "layout-flow-damage" runLayoutFlowDamageTest
  , spec "arena-columns-fit-strides" runArenaColumnsFitStridesTest
  , spec "pointer-owner" runPointerOwnerTest
  , spec "covered-field-focus" runCoveredFieldFocusTest
  , spec "covered-widget-no-pointer" runCoveredWidgetNoPointerTest
  ]

red, green, blue, yellow :: Color
red = colorRGBA 255 0 0 255
green = colorRGBA 0 255 0 255
blue = colorRGBA 0 0 255 255
yellow = colorRGBA 255 255 0 255

input0 :: Input
input0 = withInputOff 400 300

-- | Every rect but the root's after the view settles.
layoutOf :: Context -> NanoUI a -> IO [Rect]
layoutOf ctx ui = warmup2 ctx input0 ui >> drop 1 <$> arenaRects ctx

-- | The rects of nodes @is@ in the last frame, in arena order.
rectsAt :: Context -> [Int] -> IO [Rect]
rectsAt ctx is = (\rs -> [r | (i, r) <- zip [0 ..] rs, i `elem` is]) <$> arenaRects ctx

-- | 'runSim' on 'input0'.
sim :: Context -> IORef Int -> NanoUI a -> Sim a r -> IO r
sim ctx failed = runSim ctx failed input0

-- | The cursor with the pointer on the target.
cursorAt :: Target -> Sim a UiCursorKind
cursorAt t = moveTo t >> ((,) <$> simContext <*> getInput) >>= liftIO . uncurry uiCursorKind

focused, hot :: Sim a WidgetId
focused = simContext >>= liftIO . getFocusId
hot = simContext >>= liftIO . getHotId

-- | Hover, press, hold and release on the target; the press and release results.
pressThrough :: Target -> Sim a (a, a)
pressThrough t = do
  _ <- moveTo t
  pressed <- press t
  _ <- frame
  (pressed,) <$> release

-- | Non-root rects of layers and 'layered', wrapping rows and columns with
-- 'lineAlign', pins (which take no space), 'aspect', 'center', 'flex', grow
-- children under a maximum, and an empty label.
runLayoutFlowRectsTest :: Context -> IORef Int -> IO ()
runLayoutFlowRectsTest ctx failed =
  forM_ cases $ \(ui, want) -> layoutOf ctx ui >>= assertEq failed want
  where
    wrapRow w = columnWith tight $ rowWith (wrap . fixedW w . tight . gap 10 . lineGap 5) $ do
      box (alignBottom . fixedWH 30 10) red
      forM_ [(30, 20), (30, 10), (50, 10), (120, 10), (20, 10)] $ \(bw, bh) -> box (fixedWH bw bh) red
    chips a = columnWith tight $ rowWith (wrap . lineAlign a . fixedW 100 . tight . gap 10 . lineGap 0) $ do
      replicateM_ 4 (box (fixedWH 30 10) red)
      box (fillW . fixedH 10) blue
    chipRects s = [Rect 0 0 100 30, Rect s 0 30 10, Rect (s + 40) 0 30 10, Rect s 10 30 10, Rect (s + 40) 10 30 10, Rect 0 20 100 10]
    in300 = columnWith (tight . gap 0 . fixedW 300)
    inBox = columnWith (tight . fixedW 200 . fixedH 100) . center
    ends container = container $ box (fixedWH 20 10) red >> flex >> box (fixedWH 20 10) blue
    cases =
      [ ( columnWith tight $ layersWith (padAll 4) $ do
            box (fixedWH 100 60) red
            box (alignEnd . alignBottom . fixedWH 20 10) blue
            box (alignCenter . alignMid . fixedWH 10 10) blue
            box (fillW . fixedH 6) green
            box (alignEnd . percent 50 . fixedH 6) green
            box (fillH . fixedW 5) yellow
        , [Rect 0 0 108 68, Rect 4 4 100 60, Rect 84 54 20 10, Rect 49 29 10 10, Rect 4 4 100 6, Rect 54 4 50 6, Rect 4 4 5 60] )
      , ( columnWith tight $ panelWith (layered . padAll 5) $ do
            box (fixedWH 60 40) red
            box (alignEnd . alignBottom . fixedWH 10 10) blue
        , [Rect 0 0 70 50, Rect 5 5 60 40, Rect 55 35 10 10] )
      , ( columnWith tight $ rowWith (layered . tight . fixedW 60) $ do
            label "one two three four five six"
            box (alignEnd . fixedWH 10 10) blue
        , [Rect 0 0 60 72, Rect 0 0 60 72, Rect 50 0 10 10] )
      , ( columnWith tight $ do
            layersWith (fixedW 100 . tight) $ label "one two three four five six seven eight nine ten"
            label "x"
        , [Rect 0 0 100 84, Rect 0 0 96 84, Rect 0 92 12 12] )
      , (wrapRow 100, [Rect 0 0 100 65, Rect 0 10 30 10, Rect 40 0 30 20, Rect 0 25 30 10, Rect 40 25 50 10, Rect 0 40 120 10, Rect 0 55 20 10])
      , (wrapRow 200, [Rect 0 0 200 35, Rect 0 10 30 10, Rect 40 0 30 20, Rect 80 0 30 10, Rect 120 0 50 10, Rect 0 25 120 10, Rect 130 25 20 10])
      , ( columnWith tight $ rowWith (wrap . fixedW 100 . tight . gap 0) $ do
            box (fixedWH 60 10) red
            box (fillW . minW 30 . fixedH 10) blue
            box (fixedWH 70 10) green
        , [Rect 0 0 100 20, Rect 0 0 60 10, Rect 60 0 40 10, Rect 0 10 70 10] )
      , (chips LinesStart, chipRects 0)
      , (chips LinesCenter, chipRects 15)
      , (chips LinesEnd, chipRects 30)
      , ( columnWith tight $ columnWith (wrap . fixedH 100 . tight . gap 0 . lineGap 5) $ replicateM_ 5 (box (fixedWH 20 40) red)
        , [Rect 0 0 70 100, Rect 0 0 20 40, Rect 0 40 20 40, Rect 25 0 20 40, Rect 25 40 20 40, Rect 50 0 20 40] )
      , ( columnWith tight . columnWith (wrap . lineAlign LinesEnd . fixedH 100 . tight . gap 0 . lineGap 5) $
            replicateM_ 3 (box (fixedWH 20 40) red)
        , [Rect 0 0 45 100, Rect 0 20 20 40, Rect 0 60 20 40, Rect 25 60 20 40] )
      , ( columnWith (tight . gap 0) $ do
            rowWith (padAll 5 . gap 10) $ do
              box (fixedWH 40 40) red
              box (pinAt 30 12 . fixedWH 16 16) blue
              box (fixedWH 40 40) green
              box (pinAt 0 0 . fillW . fixedH 4) yellow
            box (fixedWH 10 10) yellow
        , [Rect 0 0 100 50, Rect 5 5 40 40, Rect 35 17 16 16, Rect 55 5 40 40, Rect 5 5 90 4, Rect 0 50 10 10] )
      , ( columnWith (tight . gap 0) $ columnWith (tight . fixedWH 200 100) $ do
            -- Pinned past an aligned position: inset, overhanging, shortened, shifted.
            box (pinAt (-16) (-16) . alignEnd . alignBottom . fixedWH 40 40) red
            box (pinAt 6 (-6) . alignEnd . alignTop . fixedWH 12 12) blue
            box (pinAt (-10) 0 . alignEnd . fillW . fixedH 4) green
            box (pinAt 10 0 . alignCenter . alignMid . fixedWH 20 10) yellow
        , [Rect 0 0 200 100, Rect 144 44 40 40, Rect 194 (-6) 12 12, Rect 0 0 190 4, Rect 100 45 20 10] )
      , (in300 (box (fillW . aspect 2) red), [Rect 0 0 300 150])
      , (in300 (box (fixedW 80 . aspect 4) red), [Rect 0 0 80 20])
      , (in300 (box (fixedH 50 . aspect 2) red), [Rect 0 0 100 50])
      , (in300 (box (fillW . aspect 1 . maxH 50) red), [Rect 0 0 300 50])
      , (in300 (rowWith (tight . gap 0 . fillW) (box (fillW . aspect 2) red >> box (fixedWH 100 10) blue)), [Rect 0 0 300 100, Rect 0 0 200 100, Rect 200 0 100 10])
      , -- A panel keeps the ratio's height whatever its content.
        (in300 (panelWith (tight . fillW . aspect 10) (box (fixedWH 20 100) red) >> box (fixedWH 10 10) blue), [Rect 0 0 300 30, Rect 0 0 20 100, Rect 0 30 10 10])
      , -- A non-positive ratio is ignored.
        (in300 (box (fixedWH 40 20 . aspect 0) red), [Rect 0 0 40 20])
      , (inBox (box (fixedWH 20 10) red), [Rect 0 0 200 100, Rect 90 45 20 10, Rect 90 45 20 10])
      , -- The block of both boxes and the gap between them sits mid-height.
        ( inBox (box (fixedWH 40 10) red >> box (alignCenter . fixedWH 20 10) blue)
        , [Rect 0 0 200 100, Rect 80 36 40 28, Rect 80 36 40 10, Rect 90 54 20 10] )
      , (ends (columnWith (tight . gap 0 . fixedW 100 . fixedH 200)), [Rect 0 0 20 10, Rect 0 10 8 180, Rect 0 190 20 10])
      , (ends (rowWith (tight . gap 0 . fixedW 200 . fixedH 50)), [Rect 0 0 20 10, Rect 20 0 160 8, Rect 180 0 20 10])
      , ( rowWith (tight . gap 0 . fixedW 400 . fixedH 20) $ do
            box (fillW . maxW 100) red
            box fillW green
            box (minW 80) blue
        , [Rect 0 0 100 32, Rect 100 0 220 32, Rect 320 0 80 32] )
      , ( columnWith (tight . gap 0 . fixedW 20 . fixedH 300) (box (fillH . maxH 50) red >> box fillH green)
        , [Rect 0 0 20 50, Rect 0 50 20 250] )
      , -- A negative weight grows nothing and takes nothing from the others.
        ( rowWith (tight . gap 0 . fixedW 300 . fixedH 20) (box fillW red >> box (\l -> l {layoutWidth = Grow (-1)}) green)
        , [Rect 0 0 268 32, Rect 268 0 32 32] )
      , ( columnWith tight (columnWith tight (label "a" >> label "" >> label "b") >> label "after")
        , [Rect 0 0 12 52, Rect 0 0 12 12, Rect 0 20 0 12, Rect 0 40 12 12, Rect 0 60 60 12] )
      ]

-- | Labels with fixed or percentage sizes measure as they are placed, and
-- paint wraps where the solve placed them (each case names the label's rect
-- and the other spans). Fixed sizes outside a node's limits take the limit.
runTextSizesTest :: Context -> IORef Int -> IO ()
runTextSizesTest ctx failed = do
  forM_ cases $ \(ui, at, others, want) -> do
    rs <- layoutOf ctx ui
    assertEq failed want rs
    assertJust failed (lookup at (zip [0 :: Int ..] rs)) (assertPaintedLines failed ctx others)
  let narrow l = l {layoutWidth = Fixed 100, layoutHeight = Fixed 28}
      field f = textInputConfigured' defaultTextInputConfig {ticLayout = f (ticLayout defaultTextInputConfig)}
      custom = defaultCustomWidgetSpec {widgetLayout = narrow (minW 160 defaultLayout), widgetMeasure = Just (\_ _ -> (100, 28))}
      limits = rowWith tight $ do
        (a, _) <- withValue "" (field narrow)
        b <- button' "Go"
        (c, ()) <- customWidget custom
        d <- button' "End"
        (fixed, _) <- withValue "" (field (fixedWH 100 28))
        pure [a, b, c, d, fixed]
  map respRect <$> warmup2 ctx input0 limits
    >>= assertEq failed [Rect 0 0 160 28, Rect 168 0 48 16, Rect 224 0 160 28, Rect 392 0 60 16, Rect 460 0 100 28]
  where
    fixedLabel w = labelWith (\l -> l {layoutWidth = Fixed w})
    cases =
      [ ( columnWith tight $ do
            rowWith tight (labelWith (fixedWH 100 20 . alignEnd) "Name" >> void (button "Go"))
            fixedLabel 100 "a fixed label wide enough to wrap"
        , 3, ["Name", "Go"], [Rect 0 0 156 20, Rect 0 0 100 20, Rect 108 0 48 16, Rect 0 28 100 60] )
      , ( columnWith tight . columnWith tight $ do
            labelWith (\l -> (minW 200 l) {layoutWidth = Fixed 100}) "a fixed label wide enough to wrap twice over"
            label "after"
        , 1, ["after"], [Rect 0 0 200 56, Rect 0 0 200 36, Rect 0 44 60 12] )
      , ( columnWith tight $ do
            columnWith (tight . fixedW 400) $ do
              box (percent 50 . alignEnd . fixedH 10) red
              labelWith (percent 50) "percent sized text wraps at its own width"
            gridWith 2 (tight . fixedW 401 . gap 1) (box (percent 50 . fixedH 10) red >> box (fixedH 10) blue)
            rowWith (tight . fixedH 40) $ box (\l -> l {layoutWidth = Fixed 10, layoutHeight = Percent 50}) red
        , 2
        , []
        , [ Rect 0 0 400 54, Rect 200 0 200 10, Rect 0 18 200 36, Rect 0 62 401 10, Rect 0 62 100 10
          , Rect 201 62 32 10, Rect 0 80 10 40, Rect 0 80 10 20
          ] )
      ]

-- | An unbounded wrapping row wraps at its column's width and refits its
-- height. Widening the window reflows it onto one line with a full repaint.
runWrapRefitInColumnTest :: Context -> IORef Int -> IO ()
runWrapRefitInColumnTest ctx failed = do
  let ui = columnWith (tight . gap 0) $ do
        rowWith (wrap . tight . gap 0) (replicateM_ 10 (box (fixedWH 50 10) red))
        box (fixedWH 10 10) blue
  _ <- warmup2 ctx (withInputOff 220 300) ui
  rectsAt ctx [1, 11, 12] >>= assertEq failed [Rect 0 0 220 30, Rect 50 20 50 10, Rect 0 30 10 10]
  warmup ctx (withInputOff 600 300) ui
  takeDamage ctx >>= assertEq failed DamageFull
  rectsAt ctx [1, 12] >>= assertEq failed [Rect 0 0 500 10, Rect 0 10 10 10]

-- | A wrapping row or a pinned child in a scroller sets its range, and a pin
-- is clipped to the viewport. A scroller stacks its children whatever flow
-- its layout asks for.
runFlowInScrollerTest :: Context -> IORef Int -> IO ()
runFlowInScrollerTest ctx failed = do
  let scrolled w h body = columnWith tight (scrollArea (fixedWH w h . tight) body)
      rangeOf sid want = assertJustM failed (getScrollMetrics ctx sid) (assertEq failed want . v2Y . scrollRange)
      bottom b = rectY b + rectH b
      wrapped = scrolled 120 50 $ rowWith (wrap . fillW . tight . gap 0) (replicateM_ 12 (box (fixedWH 40 20) red))
  (sid, _) <- warmup2 ctx input0 wrapped
  r : boxes <- drop 2 <$> arenaRects ctx
  assertEq failed 12 (length boxes)
  assert failed (all (\b -> rectX b + rectW b <= 120) boxes && rectH r >= 80)
  assertEq failed (bottom r) (maximum (map bottom boxes))
  rangeOf sid (rectH r - 50)
  setScrollOffset ctx sid (rectH r - 50)
  warmup ctx input0 wrapped
  arenaRects ctx >>= assertEq failed 50 . bottom . last
  let pinned = scrolled 100 50 (box (fixedWH 80 20) red >> box (pinAt 0 200 . fixedWH 30 30) blue)
  (pid, _) <- warmup2 ctx input0 pinned
  rangeOf pid 180
  setScrollOffset ctx pid 180
  warmup ctx input0 pinned
  rectsAt ctx [2, 3] >>= assertEq failed [Rect 0 (-180) 80 20, Rect 0 20 30 30]
  assertJustM failed (getClipRect (ctxNodeArena ctx) 3) (assert failed . covers (Rect 0 0 100 50))
  fresh <- newContext
  forM_ [layered, wrap] $ \flow -> do
    (fid, _) <- warmup2 fresh input0 . columnWith tight . scrollArea2D (flow . fixedWH 100 50 . tight . gap 0) $
      replicateM_ 3 (box (fixedWH 40 20) red)
    arenaRects fresh >>= assertEq failed [0, 20, 40] . map rectY . drop 2
    assertJustM failed (getScrollMetrics fresh fid) (assertEq failed 10 . v2Y . scrollRange)

-- | Each case's first colour is painted before its second. A pinned child
-- outside its parent is still painted, also on a frame that repaints only it.
runLayerPaintOrderTest :: Context -> IORef Int -> IO ()
runLayerPaintOrderTest ctx failed = do
  forM_ cases $ \(ui, under, over) -> do
    quads <- drawQuads . snd =<< warmupDraw ctx input0 ui
    let at col = findIndex ((== col) . snd) quads
    assertJust failed ((,) <$> at under <*> at over) $ \(u, o) -> assert failed (u < o)
  let outside version = columnWith (tight . gap 0 . fillW) $ do
        layersWith tight $ box (pinAt 10 10 . fixedWH 20 20) green
        rowWith (tight . fixedWH 50 20) $
          drawingVersioned version (pinAt 200 0 . fixedWH 20 20) $ \r ->
            smallArrayFromList [FillRect r (if version == 1 then red else blue)]
      drawn draw = map snd <$> drawQuads draw
  full <- drawn . snd =<< warmupDraw ctx input0 (outside 1)
  assert failed (green `elem` full && red `elem` full)
  writeIORef (ctxPaintFull ctx) False
  (_, draw, _) <- runFrame ctx input0 (outside 2)
  takeDamage ctx >>= \d -> assert failed (d /= DamageFull)
  assert failed . elem blue =<< drawn draw
  where
    sq n = fixedWH n n
    cases =
      [ (rowWith tight (box (sq 60) green >> box (sq 20) yellow), yellow, green)
      , (layersWith tight (box (sq 60) red >> box (sq 20) blue), red, blue)
      , (rowWith tight (box (sq 40) red >> box (pinAt 10 10 . sq 10) blue), red, blue)
      , (layersWith tight (box (pinAt 5 5 . sq 10) blue >> box (sq 60) red), red, blue)
      ]

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
  step (ui 120 False 0) [(3, Rect 120 10 20 20)] [Rect 10 10 20 20, Rect 120 10 20 20]
  -- The yellow box moves onto its own line.
  step (ui 120 True 0) [(4, Rect 0 60 100 20), (6, Rect 0 70 40 10)] [Rect 70 60 40 10, Rect 0 70 40 10]
  step (ui 120 True 5) [(4, Rect 0 60 100 25), (6, Rect 0 75 40 10)] [Rect 0 75 40 10]

-- | Every arena column fits within its row stride; otherwise it would
-- overwrite the next node's row.
runArenaColumnsFitStridesTest :: Context -> IORef Int -> IO ()
runArenaColumnsFitStridesTest _ failed =
  assert failed $
    and
      [ columns (minBound :: GeomCol) <= geomStride
      , columns (minBound :: StyleCol) <= styleStride
      , columns (minBound :: TagCol) <= tagStride
      , columns (minBound :: TreeCol) <= treeStride
      ]
  where
    columns :: (Enum c, Bounded c) => c -> Int
    columns c = fromEnum (maxBound `asTypeOf` c) + 1

-- | Each point names the widget it hovers, makes hot and clicks, if any: the
-- topmost, by layer and pin, and under 'pointer' modes ('PointerBlock' over
-- the button, 'PointerPass' through a drawing).
runPointerOwnerTest :: Context -> IORef Int -> IO ()
runPointerOwnerTest ctx failed =
  forM_ cases $ \(ui, points) -> sim ctx failed ui $ do
    ws <- result
    let only want = [Just i == want | i <- [0 .. length ws - 1]]
    forM_ (points ws) $ \(t, want) -> do
      _ <- moveTo t
      hovered <- moveTo t
      checkEq (maybe (WidgetId 0) (respId . (ws !!)) want) =<< hot
      checkEq (only want) (map respHovered hovered)
      checkEq (only want) . map respClicked =<< click t
  where
    btn f w h = buttonWith' (f . fixedWH w h) "b"
    at x y = At (V2 x y)
    over inner = columnWith tight ((\a b -> [a, b]) <$> btn id 200 100 <*> inner)
    panelOver mode = over (panelWith (pointer mode . pinAt 10 10 . fixedWH 120 60) (btn id 40 20))
    drawingOver mode = over (drawing (pointer mode . pinAt 20 20 . fixedWH 100 50) (const mempty))
    cases =
      [ (layers (sequence [btn id 120 40, btn id 120 40]), const [(at 63 23, Just 1)])
      , (rowWith tight (sequence [btn id 120 40, btn (pinAt 20 10) 40 20]), const [(at 40 20, Just 1), (at 100 20, Just 0)])
      , (layersWith tight (sequence [btn (pinAt 10 10) 40 20, btn id 120 60]), const [(at 30 20, Just 0)])
      , ( rowWith tight ((:) <$> btn id 200 100 <*> layersWith (pinAt 10 10 . tight) (sequence [btn id 50 50, btn id 20 20]))
        , const [(at 15 15, Just 2), (at 40 40, Just 1), (at 150 50, Just 0)] )
      , (panelOver PointerAuto, \ws -> [(at 100 60, Just 0), (On (ws !! 1), Just 1), (at 190 90, Just 0)])
      , (panelOver PointerBlock, \ws -> [(at 100 60, Nothing), (On (ws !! 1), Just 1), (at 190 90, Just 0)])
      , ( columnWith tight . panelWith (pointer PointerBlock . fixedWH 300 200) $
            (: []) <$> btn id 200 100 <* box (pinAt 0 0 . fixedWH 150 80 . pointer PointerBlock) red
        , const [(at 100 60, Nothing), (at 190 90, Just 0)] )
      , (drawingOver PointerAuto, const [(at 60 40, Just 1)])
      , (drawingOver PointerPass, const [(at 60 40, Just 0)])
      , ( columnWith tight $ (\a b -> [a, b]) <$> drawing (fixedWH 200 100) (const mempty) <*> btn (pinAt 10 10) 60 30
        , const [(at 40 25, Just 1), (at 150 70, Just 0)] )
      ]

-- | Focus and the text cursor follow the topmost widget: a button or a
-- blocking panel over a field or select keeps focus as it was, while a
-- field over a button or another field takes it.
runCoveredFieldFocusTest :: Context -> IORef Int -> IO ()
runCoveredFieldFocusTest ctx failed = do
  let field f t = fst <$> withValue t (textInputConfigured' defaultTextInputConfig {ticLayout = f (ticLayout defaultTextInputConfig)})
      onLeft r = At (V2 (rectX (respRect r) + 20) (rectY (respRect r) + rectH (respRect r) / 2))
      focusAfter t = click t >> focused
      layeredUi = column $ do
        (f, cover) <- layers $ (,) <$> field (fixedW 200) "under" <*> buttonWith' (alignEnd . fixedWH 40 20) "x"
        (under, top) <- layers $ (,) <$> buttonWith' (fixedWH 200 30) "under" <*> field (fixedW 100) "over"
        pure (f, cover, under, top)
  sim ctx failed layeredUi $ do
    (f, cover, _, top) <- result
    check . (/= UiCursorText) =<< cursorAt (On cover)
    checkEq UiCursorText =<< cursorAt (onLeft f)
    (_, cover1, _, _) <- click (On cover)
    check (respClicked cover1)
    checkEq (WidgetId 0) =<< focused
    checkEq (respId f) =<< focusAfter (onLeft f)
    checkEq (respId f) =<< focusAfter (On cover)
    (_, _, under, _) <- click (On top)
    check (not (respClicked under))
    checkEq (respId top) =<< focused
  let pinnedOver = columnWith tight $ do
        cover <- buttonWith' (pinAt 100 0 . fixedWH 60 20) "x"
        f <- field (fixedW 200) "text"
        (s, over) <- layersWith tight $
          (,) <$> (fst <$> withValue 0 (selectWith' (fixedW 200) ["Alpha", "Beta"])) <*> buttonWith' (alignEnd . fixedWH 40 20) "y"
        pure (cover, f, s, over)
  newContext >>= \c -> runSim c failed input0 pinnedOver $ do
    (cover, f, _, over) <- result
    (cover1, _, _, _) <- click (On cover)
    check (respClicked cover1)
    checkEq (WidgetId 0) =<< focused
    checkEq (respId f) =<< focusAfter (onLeft f)
    checkEq (respId f) =<< focusAfter (On cover)
    (_, _, _, over1) <- click (On over)
    check (respClicked over1)
    checkEq (WidgetId 0) =<< focused
  let pinnedField = columnWith tight (field (pinAt 50 0 . fixedW 100) "pinned" <* field (fixedW 300) "under")
  newContext >>= \c -> runSim c failed input0 pinnedField $ do
    pinned <- result
    checkEq UiCursorText =<< cursorAt (On pinned)
    _ <- rightClick (On pinned)
    checkEq (respId pinned) =<< focused
  let blocked mode = columnWith tight $
        field (fixedW 200) "text" <* panelWith (pointer mode . pinAt 60 0 . fixedWH 80 20) (pure ())
  forM_ [(PointerAuto, True), (PointerBlock, False)] $ \(mode, through) -> sim ctx failed (blocked mode) $ do
    f <- result
    let p = At (V2 (rectX (respRect f) + 100) (rectY (respRect f) + 10))
    checkEq through . (== UiCursorText) =<< cursorAt p
    _ <- click p
    checkEq (if through then respId f else WidgetId 0) =<< focused
    void (click (At (V2 390 290)))

-- | A widget under a pinned button shows no tooltip, moves no slider and
-- opens no select. A chip's control adornment takes its own click.
runCoveredWidgetNoPointerTest :: Context -> IORef Int -> IO ()
runCoveredWidgetNoPointerTest ctx failed = do
  let tipped txt w = w <$ tooltipConfigured defaultTooltipConfig {tooltipDelay = 0} w txt
      tipShown txt t = moveTo t >> isJust <$> findSpan txt
      covered = columnWith tight $ do
        _ <- drawing (fixedWH 200 100) (const mempty) >>= tipped "Canvas tip"
        buttonWith' (pinAt 10 10 . fixedWH 60 30) "pin"
  sim ctx failed covered $ do
    pin <- On <$> result
    check . not =<< tipShown "Canvas tip" pin
    check =<< tipShown "Canvas tip" (At (V2 150 70))
  valueRef <- newVar (0 :: Float)
  let sliderUi = columnWith (tight . fixedW 300) $
        held valueRef (sliderWith' id 0 100) >> buttonWith' (pinAt 200 0 . fixedWH 60 20) "x"
  sim ctx failed sliderUi $ do
    (_, b) <- pressThrough . On =<< result
    check (respClicked b)
    checkEq 0 =<< liftIO (readVarIO valueRef)
  let selectUi = columnWith tight $
        withValue 0 (selectWith (fixedW 200) ["Alpha", "Beta", "Gamma"]) >> buttonWith' (pinAt 120 0 . fixedWH 60 20) "y"
  sim ctx failed selectUi $ do
    onB <- On <$> result
    _ <- pressThrough onB
    _ <- moveTo onB
    expectNot "Gamma"
  removes <- newIORef (0 :: Int)
  let remove = A.trailing (A.control (whenM (buttonWith tight "x") (liftIO (modifyIORef' removes (+ 1)))))
      chipOver = columnWith tight $ layersWith tight $ do
        under <- drawing (fixedWH 200 100) (const mempty)
        (under,) <$> buttonConfigured' defaultButtonConfig {bcAdornments = remove} "Chip"
  sim ctx failed chipOver $ do
    _ <- moveTo "Chip"
    (under1, chip1) <- moveTo "Chip"
    check (respHovered chip1 && not (respHovered under1))
    ((under2, _), (_, chip3)) <- pressThrough "x"
    check (not (respHovered under2 || respPressed under2) && not (respClicked chip3))
    checkEq 1 =<< liftIO (readIORef removes)
  let labelUi = columnWith tight $ do
        l <- labelWith' (fixedWH 200 40) "Covered text" >>= tipped "Label tip"
        l <$ buttonWith' (pinAt 100 0 . fixedWH 60 30) "on top"
  sim ctx failed labelUi $
    forM_ [(V2 130 15, False), (V2 40 15, True)] $ \(p, uncovered) -> do
      _ <- moveTo (At p)
      checkEq uncovered . respHovered =<< moveTo (At p)
      checkEq uncovered =<< tipShown "Label tip" (At p)
