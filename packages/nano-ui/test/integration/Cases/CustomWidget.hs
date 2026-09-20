module Cases.CustomWidget
  ( runCustomWidgetMeasureTest
  , runCustomWidgetCursorTest
  , runCustomWidgetInteractionTest
  , runCustomWidgetQueuedClickTest
  , runCustomWidgetContentDamageTest
  , runCustomWidgetContentKeyTest
  , runReferenceKnobTest
  , runDropTargetTest
  ) where

import Control.Monad (forM_, void)
import Data.IORef (IORef, writeIORef)
import Data.Primitive.SmallArray qualified as SA
import NanoUI
import NanoUI.Context (Context (..))
import NanoUI.Testing
  ( UiCursorKind (..)
  , cursorKindIs
  , newContext
  , runFrame
  , takeDamage
  )
import NanoUI.Testing.Assert (assert, assertEq, withInput)
import NanoUI.Testing.Harness (centerOf, clickPair, clipCovers, covers, drawQuads, warmup2, withInputOff)

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

-- | Verifies dynamic cursor resolution on custom widgets.
runCustomWidgetCursorTest :: Context -> IORef Int -> IO ()
runCustomWidgetCursorTest ctx failed = do
  let inp0 = withInput 300 300
      ui = column $ do
        fst <$> customWidget defaultCustomWidgetSpec
          { widgetLayout = fixedWH 80 80 defaultLayout
          , widgetCursor = Just (\_ -> UiCursorNsResize)
          }
  resp <- warmup2 ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      hoverInp = inp0 { inputMousePos = centerOf resp }
  _ <- runFrame ctx hoverInp ui
  hoverOk <- cursorKindIs ctx hoverInp UiCursorNsResize
  assert failed hoverOk

  let outInp = inp0 { inputMousePos = V2 (rx + rw + 50) (ry + rh + 50) }
  _ <- runFrame ctx outInp ui
  outOk <- cursorKindIs ctx outInp UiCursorNsResize
  assert failed (not outOk)

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
  let pos = centerOf resp0
      (pressInp, releaseInp) = clickPair inp0 pos

  _ <- runFrame ctx pressInp ui
  ((respClick, (hovered, pressed)), _, _, _) <- runFrame ctx releaseInp ui
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

-- | An unkeyed custom drawing repaints when captured state changes, even
-- while its rectangle and hover/press state remain unchanged.
runCustomWidgetContentDamageTest :: Context -> IORef Int -> IO ()
runCustomWidgetContentDamageTest ctx failed = do
  let inp = withInputOff 400 300
      red = colorRGBA 255 0 0 255
      blue = colorRGBA 0 0 255 255
      ui on = column $ do
        label "Other"
        fst <$> customWidget defaultCustomWidgetSpec
          { widgetLayout = fixedWH 80 40 defaultLayout
          , widgetDraw = \_ r -> runCanvas (drawRect r (if on then red else blue))
          }
  resp <- warmup2 ctx inp (ui False)
  _ <- takeDamage ctx
  (_, _, draw, _) <- runFrame ctx inp (ui True)
  dmg <- takeDamage ctx
  assert failed (clipCovers dmg (respRect resp))
  quads <- drawQuads draw
  assert failed (any ((== red) . snd) quads)
  assert failed (not (any ((== blue) . snd) quads))
  -- The built-in progress bar captures its fraction the same way.
  let bar frac = column (progressBar' frac)
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
      red = colorRGBA 255 0 0 255
      blue = colorRGBA 0 0 255 255
      ui key on = column $ do
        label "Other"
        fst <$> customWidget defaultCustomWidgetSpec
          { widgetLayout = fixedWH 80 40 defaultLayout
          , widgetContent = key
          , widgetDraw = \_ r -> runCanvas (drawRect r (if on then red else blue))
          }
  resp <- warmup2 ctx inp (ui 1 False)
  _ <- takeDamage ctx

  -- Same key, different captured state: the ops it already has stand.
  (_, _, keptDraw, _) <- runFrame ctx inp (ui 1 True)
  keptDmg <- takeDamage ctx
  keptQuads <- drawQuads keptDraw
  assert failed (any ((== blue) . snd) keptQuads)
  case keptDmg of
    DamageClip clip -> assert failed (not (covers clip (respRect resp)))
    DamageFull -> assert failed False

  -- A new key rebuilds and repaints.
  (_, _, freshDraw, _) <- runFrame ctx inp (ui 2 True)
  freshDmg <- takeDamage ctx
  freshQuads <- drawQuads freshDraw
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
          , widgetDraw = \cdc r -> runCanvas (drawRect r (if cdcDisabled cdc then grey else blue))
          }
  disabledCtx <- newContext
  dresp <- warmup2 disabledCtx inp (dimmable False)
  _ <- takeDamage disabledCtx
  (_, _, disabledDraw, _) <- runFrame disabledCtx inp (dimmable True)
  disabledDmg <- takeDamage disabledCtx
  disabledQuads <- drawQuads disabledDraw
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
          , widgetDraw = \_ r -> runCanvas (drawRect r red)
          }
  settled <- warmup2 ctx inp (moved 40)
  let Rect _ my _ _ = respRect settled
  _ <- warmup2 ctx inp (moved 10)
  (_, _, movedDraw, _) <- runFrame ctx inp (moved 40)
  movedQuads <- drawQuads movedDraw
  assert failed (any (\(Rect _ qy _ _, c) -> c == red && abs (qy - my) < 0.5) movedQuads)

  -- Swapping the theme rebuilds a keyed widget that draws from the theme,
  -- through either theme entry point.
  let accent2 = colorRGBA 7 8 9 255
      accent3 = colorRGBA 11 12 13 255
      themedUi = column $ do
        label "Other"
        fst <$> customWidget defaultCustomWidgetSpec
          { widgetLayout = fixedWH 80 40 defaultLayout
          , widgetContent = 3
          , widgetDraw = \cdc r -> runCanvas (drawRect r (themeAccent (cdcTheme cdc)))
          }
  _ <- warmup2 ctx inp themedUi
  theme0 <- getTheme ctx
  setTheme ctx theme0 {themeAccent = accent2}
  (_, _, themedDraw, _) <- runFrame ctx inp themedUi
  themedQuads <- drawQuads themedDraw
  assert failed (any ((== accent2) . snd) themedQuads)
  ctx3 <- withTheme ctx theme0 {themeAccent = accent3}
  (_, _, withThemeDraw, _) <- runFrame ctx3 inp themedUi
  withThemeQuads <- drawQuads withThemeDraw
  assert failed (any ((== accent3) . snd) withThemeQuads)
  setTheme ctx theme0

-- | Verifies the reference rotary knob widget.
runReferenceKnobTest :: Context -> IORef Int -> IO ()
runReferenceKnobTest ctx failed = do
  let inp0 = withInput 300 300
      ui = column $ knob' 0 100 25
  (resp0, val0) <- warmup2 ctx inp0 ui
  assert failed (val0 == 25)

  let pos = centerOf resp0
      dragStart = inp0 { inputMousePos = pos, inputMouseDown = True, inputMousePressed = True }
      -- Drag upward (negative dy in screen coords) to increase knob value
      dragUp = inp0 { inputMousePos = V2 (v2X pos) (v2Y pos - 30), inputMouseDown = True, inputMousePressed = False }
      release = inp0 { inputMousePos = V2 (v2X pos) (v2Y pos - 30), inputMouseDown = False, inputMouseReleased = True }

  _ <- runFrame ctx dragStart ui
  ((respDragged, valDragged), _, _, _) <- runFrame ctx dragUp ui
  assert failed (valDragged > 25)
  assert failed (respChanged respDragged)
  void $ runFrame ctx release ui

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
