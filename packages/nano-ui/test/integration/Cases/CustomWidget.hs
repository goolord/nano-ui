module Cases.CustomWidget (tests) where

import Spec
import Data.Primitive.SmallArray qualified as SA
import NanoUI.Internal.Context (Context (..))

tests :: [Spec]
tests =
  [ spec "custom-widget-measure" runCustomWidgetMeasureTest
  , spec "custom-widget-measure-parent" runCustomWidgetMeasureParentTest
  , spec "custom-widget-cursor" runCustomWidgetCursorTest
  , spec "custom-widget-interaction" runCustomWidgetInteractionTest
  , spec "custom-widget-queued-click" runCustomWidgetQueuedClickTest
  , spec "custom-widget-content-damage" runCustomWidgetContentDamageTest
  , spec "custom-widget-content-key" runCustomWidgetContentKeyTest
  , spec "custom-widget-knob" runReferenceKnobTest
  , spec "custom-widget-knob-covered" runKnobCoveredTest
  , spec "drop-target" runDropTargetTest
  ]

-- | Verifies custom intrinsic layout measurement via widgetMeasure hook, and
-- that the measurement reverts once the hook is gone.
runCustomWidgetMeasureTest :: Context -> IORef Int -> IO ()
runCustomWidgetMeasureTest ctx failed = do
  let inp = withInput 400 400
      ui measure = column $ do
        fst <$> customWidget defaultCustomWidgetSpec
          { widgetMeasure = measure
          , widgetLayout = defaultLayout
          }
  resp <- warmup2 ctx inp (ui (Just $ \_ _ -> (160, 48)))
  let r = respRect resp
  assert failed (rectW r == 160 && rectH r == 48)
  plain <- warmup2 ctx inp (ui Nothing)
  let Rect _ _ pw ph = respRect plain
  assertEq failed (pw, ph) (32, 32)
  -- Gaining the hook back changes the size too, with the arena unchanged.
  back <- warmup2 ctx inp (ui (Just $ \_ _ -> (160, 48)))
  let Rect _ _ bw bh = respRect back
  assertEq failed (bw, bh) (160, 48)

-- | A custom measure whose result changes with nothing in the arena changed
-- must resize the containers sized from it, not only the widget.
runCustomWidgetMeasureParentTest :: Context -> IORef Int -> IO ()
runCustomWidgetMeasureParentTest ctx failed = do
  let inp = withInput 400 400
      -- The inner column takes its width from the widget, and the label
      -- after it in the row sits where that width ends.
      ui w = column $ row $ do
        resp <- column $
          fst <$> customWidget defaultCustomWidgetSpec
            { widgetMeasure = Just (\_ _ -> (w, 48))
            , widgetLayout = defaultLayout
            }
        void $ label "after"
        pure resp
      rects = arenaRects
  void $ warmup2 ctx inp (ui 120)
  resp <- warmup2 ctx inp (ui 240)
  assertEq failed (rectW (respRect resp)) 240
  fresh <- newContext
  void $ warmup2 fresh inp (ui 240)
  grown <- rects ctx
  assertEq failed grown =<< rects fresh

-- | Verifies dynamic cursor resolution on custom widgets.
runCustomWidgetCursorTest :: Context -> IORef Int -> IO ()
runCustomWidgetCursorTest ctx failed = do
  let inp0 = withInput 300 300
      -- The right edge resizes, and the rest has no opinion but the scope's.
      ui = withCursorShape UiCursorHidden . column $ do
        fst <$> customWidget defaultCustomWidgetSpec
          { widgetLayout = fixedWH 80 80 defaultLayout
          , widgetCursor = Just $ \_ (Rect x _ w _) (V2 px _) ->
              if px > x + w - 10 then UiCursorEwResize else UiCursorDefault
          }
  resp <- warmup2 ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      at p = inp0 {inputMousePos = p}
      onEdge = V2 (rx + rw - 4) (ry + rh / 2)
  cursorOver ctx inp0 ui onEdge >>= assertEq failed UiCursorEwResize
  cursorOver ctx inp0 ui (centerOf resp) >>= assertEq failed UiCursorHidden
  -- A drag that went down on the edge keeps its shape off the widget, and
  -- leaves it once let go.
  let away = V2 (rx + rw + 50) (ry + rh + 50)
      dragged = holdAt inp0 away
  _ <- runFrame ctx (pressAt inp0 onEdge) ui
  _ <- runFrame ctx dragged ui
  uiCursorKind ctx dragged >>= assertEq failed UiCursorEwResize
  _ <- runFrame ctx (releaseAt dragged) ui
  _ <- runFrame ctx (at away) ui
  uiCursorKind ctx (at away) >>= assertEq failed UiCursorDefault
  assertEq failed UiCursorHidden (cursorFallback UiCursorHidden)

-- | Verifies interaction state propagation (hover, press, click) and CustomDrawContext.
runCustomWidgetInteractionTest :: Context -> IORef Int -> IO ()
runCustomWidgetInteractionTest ctx failed = do
  let inp0 = withInput 300 300
      ui = column $ do
        customWidget defaultCustomWidgetSpec
          { widgetLayout = fixedWH 80 40 defaultLayout
          , widgetInteract = \resp cdc _ -> (resp, (cdcHovered cdc, cdcPressed cdc))
          }
  (resp0, _) <- warmup2 ctx inp0 ui
  (respClick, (hovered, pressed)) <- runClick ctx inp0 ui (centerOf resp0)
  assert failed (respClicked respClick)
  assert failed hovered
  assert failed (not pressed)

-- | A queued post-layout click reaches the custom widget even when its
-- build-time pointer hit test misses.
runCustomWidgetQueuedClickTest :: Context -> IORef Int -> IO ()
runCustomWidgetQueuedClickTest ctx failed = do
  let inp0 = (withInput 300 300) {inputMousePos = V2 290 290}
      ui = column $ do
        fromCanvas <- canvas (fixedWH 80 40) (\_ -> pure ())
        fromSpec <- fst <$> customWidget defaultCustomWidgetSpec {widgetLayout = fixedWH 80 40 defaultLayout}
        pure [fromCanvas, fromSpec]
  warm <- warmup2 ctx inp0 ui
  forM_ (zip [0 :: Int ..] warm) $ \(i, resp0) -> do
    writeIORef (ctxClickedId ctx) (respId resp0)
    (resps, _, _, _) <- runFrame ctx inp0 ui
    assert failed (map respClicked resps == [j == i | j <- [0 .. length resps - 1]])

-- | A label, then an 80 by 40 custom widget with this content key and drawing.
afterLabel :: Int -> CustomDrawBuild -> NanoUI Response
afterLabel key draw = column $ do
  label "Other"
  fst <$> customWidget defaultCustomWidgetSpec {widgetLayout = fixedWH 80 40 defaultLayout, widgetContent = key, widgetDraw = draw}

-- | Run a frame and decode the quads it drew.
frameQuads :: Context -> Input -> NanoUI a -> IO [(Rect, Color)]
frameQuads ctx inp ui = runFrame ctx inp ui >>= \(_, _, draw, _) -> drawQuads draw

red, blue :: Color
red = colorRGBA 255 0 0 255
blue = colorRGBA 0 0 255 255

-- | An unkeyed custom drawing repaints when captured state changes, even
-- while its rectangle and hover/press state remain unchanged.
runCustomWidgetContentDamageTest :: Context -> IORef Int -> IO ()
runCustomWidgetContentDamageTest ctx failed = do
  let inp = withInputOff 400 300
      ui on = afterLabel 0 (\cdc r -> runCanvasFor cdc (drawRect r (if on then red else blue)))
  resp <- warmup2 ctx inp (ui False)
  _ <- takeDamage ctx
  quads <- frameQuads ctx inp (ui True)
  dmg <- takeDamage ctx
  assert failed (clipCovers dmg (respRect resp))
  assert failed (any ((== red) . snd) quads)
  assert failed (not (any ((== blue) . snd) quads))
  -- The built-in progress bar captures its fraction the same way.
  let bar frac = column (progressBarWith' id 12 frac)
  barResp <- warmup2 ctx inp (bar 0.2)
  _ <- takeDamage ctx
  _ <- runFrame ctx inp (bar 0.8)
  barDmg <- takeDamage ctx
  assert failed (clipCovers barDmg (respRect barResp))

-- | A content key is taken at its word: while it is unchanged the widget
-- neither rebuilds its ops nor repaints, a new key does both, and a theme
-- change rebuilds them even though the key did not move, since the ops can
-- read the theme.
runCustomWidgetContentKeyTest :: Context -> IORef Int -> IO ()
runCustomWidgetContentKeyTest ctx failed = do
  let inp = withInputOff 400 300
      ui key on = afterLabel key (\cdc r -> runCanvasFor cdc (drawRect r (if on then red else blue)))
  resp <- warmup2 ctx inp (ui 1 False)
  _ <- takeDamage ctx

  -- Same key, different captured state: the ops it already has stand.
  keptQuads <- frameQuads ctx inp (ui 1 True)
  keptDmg <- takeDamage ctx
  assert failed (any ((== blue) . snd) keptQuads)
  case keptDmg of
    DamageClip clip -> assert failed (not (covers clip (respRect resp)))
    DamageFull -> assert failed False

  -- A new key rebuilds and repaints.
  freshQuads <- frameQuads ctx inp (ui 2 True)
  freshDmg <- takeDamage ctx
  assert failed (any ((== red) . snd) freshQuads)
  assert failed (clipCovers freshDmg (respRect resp))

  -- Disabling the widget repaints it: the ops rebuild in their disabled form,
  -- and disabled is not one of the roles damage already follows.
  let grey = colorRGBA 128 128 128 255
      dimmable off = column $ do
        label "Other"
        disabledWhen off $ fst <$> customWidget defaultCustomWidgetSpec
          { widgetLayout = fixedWH 80 40 defaultLayout
          , widgetContent = 4
          , widgetDraw = \cdc r -> runCanvasFor cdc (drawRect r (if cdcDisabled cdc then grey else blue))
          }
  disabledCtx <- newContext
  dresp <- warmup2 disabledCtx inp (dimmable False)
  _ <- takeDamage disabledCtx
  disabledQuads <- frameQuads disabledCtx inp (dimmable True)
  disabledDmg <- takeDamage disabledCtx
  assert failed (any ((== grey) . snd) disabledQuads)
  case disabledDmg of
    DamageClip clip -> assert failed (covers clip (respRect dresp))
    DamageFull -> pure ()

  -- A keyed widget that only moved still draws at its new place: paint
  -- translates the ops it kept.
  let moved lead = column $ do
        spacer Fit (Fixed lead)
        fst <$> customWidget defaultCustomWidgetSpec
          { widgetLayout = fixedWH 80 40 defaultLayout
          , widgetContent = 5
          , widgetDraw = \cdc r -> runCanvasFor cdc (drawRect r red)
          }
  settled <- warmup2 ctx inp (moved 40)
  let Rect _ my _ _ = respRect settled
  _ <- warmup2 ctx inp (moved 10)
  movedQuads <- frameQuads ctx inp (moved 40)
  assert failed (any (\(Rect _ qy _ _, c) -> c == red && abs (qy - my) < 0.5) movedQuads)

  -- Swapping the theme rebuilds a keyed widget that draws from the theme,
  -- through either theme entry point.
  let accent2 = colorRGBA 7 8 9 255
      accent3 = colorRGBA 11 12 13 255
      themedUi = afterLabel 3 (\cdc r -> runCanvasFor cdc (drawRect r (themeAccent (cdcTheme cdc))))
  _ <- warmup2 ctx inp themedUi
  theme0 <- getTheme ctx
  setTheme ctx theme0 {themeAccent = accent2}
  assert failed . any ((== accent2) . snd) =<< frameQuads ctx inp themedUi
  ctx3 <- withTheme ctx theme0 {themeAccent = accent3}
  assert failed . any ((== accent3) . snd) =<< frameQuads ctx3 inp themedUi
  setTheme ctx theme0

-- | Verifies the reference rotary knob widget.
runReferenceKnobTest :: Context -> IORef Int -> IO ()
runReferenceKnobTest ctx failed = do
  let inp0 = withInput 300 300
      ui = column $ knobWith' id 36 0 100 25
  (resp0, val0) <- warmup2 ctx inp0 ui
  assert failed (val0 == 25)

  let V2 x y = centerOf resp0
      -- Dragging up raises the value.
      dragUp = holdAt inp0 (V2 x (y - 30))
  _ <- runFrame ctx (pressAt inp0 (V2 x y)) ui
  (respDragged, valDragged) <- evalUi ctx dragUp ui
  assert failed (valDragged > 25)
  assert failed (respChanged respDragged)
  void $ runFrame ctx (releaseAt dragUp) ui

-- | A knob under a button pinned over it neither drags nor turns there:
-- 'useDrag2DOn' and 'useWheelDeltaOn' go by the knob's response, not its
-- rect. Beside the button, both work.
runKnobCoveredTest :: Context -> IORef Int -> IO ()
runKnobCoveredTest ctx failed = do
  valueRef <- newIORef (0 :: Float)
  let inp0 = withInputOff 300 300
      ui = columnWith tight $ do
        k <- held valueRef (knobWith' id 60 0 100)
        _ <- buttonWith' (pinAt 0 0 . fixedWH 30 60) "x"
        pure k
      dragFrom p = do
        writeIORef valueRef 50
        warmup ctx inp0 {inputMousePos = p} ui
        _ <- runFrame ctx (pressAt inp0 p) ui
        let up = holdAt inp0 (V2 (v2X p) (v2Y p - 40))
        _ <- runFrame ctx up ui
        _ <- runFrame ctx (releaseAt up) ui
        readIORef valueRef
      wheelAt p = do
        writeIORef valueRef 50
        warmup ctx inp0 {inputMousePos = p} ui
        _ <- runFrame ctx inp0 {inputMousePos = p, inputScroll = V2 0 3} ui
        readIORef valueRef
  (k0, _) <- warmup2 ctx inp0 ui
  let Rect kx ky _ kh = respRect k0
      onButton = V2 (kx + 15) (ky + kh / 2)
      onKnob = V2 (kx + 45) (ky + kh / 2)
  dragFrom onButton >>= assertEq failed 50
  wheelAt onButton >>= assertEq failed 50
  dragFrom onKnob >>= assert failed . (> 50)
  wheelAt onKnob >>= assert failed . (/= 50)

-- | Verifies the composable drag-and-drop hook: hover, file, text, and bounds.
runDropTargetTest :: Context -> IORef Int -> IO ()
runDropTargetTest ctx failed = do
  let inp0 = withInput 300 300
      bounds = Rect 10 10 100 100
      dropPoint = V2 60 60
      ui = column (useDrop bounds)
      dropsInp ds = inp0 {inputDrops = SA.smallArrayFromList ds}
  _ <- warmup2 ctx inp0 ui

  let beginInp = dropsInp [DropEvent DropBegin Nothing ""]
  (tgtBegin, _, _, _) <- runFrame ctx beginInp ui
  assert failed (not (dropHovered tgtBegin))

  let hoverInp = dropsInp [DropEvent DropPosition (Just dropPoint) ""]
  (tgtHover, _, _, _) <- runFrame ctx hoverInp ui
  assert failed (dropHovered tgtHover)
  assert failed (dropPosition tgtHover == Just dropPoint)

  -- Payload coordinates are ignored; attribution follows the last drag position.
  let dropInp =
        dropsInp
          [ DropEvent DropFile (Just dropPoint) "/tmp/a.txt"
          , DropEvent DropText (Just dropPoint) "hello"
          ]
  (tgtDrop, _, _, _) <- runFrame ctx dropInp ui
  assert failed (dropReceived tgtDrop)
  assert failed (dropFiles tgtDrop == ["/tmp/a.txt"])
  assert failed (dropTexts tgtDrop == ["hello"])

  -- A drop whose coordinates SDL reports as (0,0) (no final position seen)
  -- still lands on the target the pointer was hovering.
  let originDrop =
        dropsInp
          [ DropEvent DropFile (Just (V2 0 0)) "/tmp/origin.txt"
          , DropEvent DropComplete Nothing ""
          ]
  (tgtOrigin, _, _, _) <- runFrame ctx originDrop ui
  assert failed (dropFiles tgtOrigin == ["/tmp/origin.txt"])

  -- The completing drop clears hover state.
  (tgtDone, _, _, _) <- runFrame ctx inp0 ui
  assert failed (not (dropHovered tgtDone))

  -- A fresh drag that moves outside the target no longer delivers to it.
  _ <- runFrame ctx (dropsInp [DropEvent DropBegin Nothing ""]) ui
  _ <- runFrame ctx (dropsInp [DropEvent DropPosition (Just (V2 250 250)) ""]) ui
  let outInp =
        dropsInp
          [ DropEvent DropFile (Just (V2 250 250)) "/tmp/out.txt"
          , DropEvent DropComplete Nothing ""
          ]
  (tgtOut, _, _, _) <- runFrame ctx outInp ui
  assert failed (not (dropReceived tgtOut))
  assert failed (null (dropFiles tgtOut))
