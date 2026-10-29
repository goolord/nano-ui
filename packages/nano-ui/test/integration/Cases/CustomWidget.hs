module Cases.CustomWidget (tests) where

import Spec
import Data.Primitive.SmallArray qualified as SA
import NanoUI.Internal.Context (Context (..))
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ spec "custom-widget-measure" runCustomWidgetMeasureTest
  , spec "custom-widget-pointer" runCustomWidgetPointerTest
  , spec "custom-widget-content" runCustomWidgetContentTest
  , spec "drop-target" runDropTargetTest
  ]

red, blue :: Color
red = colorRGBA 255 0 0 255
blue = colorRGBA 0 0 255 255

-- | A custom widget of this layout, content key and drawing.
custom :: (Layout -> Layout) -> Int -> CustomDrawBuild -> NanoUI Response
custom f key draw =
  fst <$> customWidget defaultCustomWidgetSpec {widgetLayout = f defaultLayout, widgetContent = key, widgetDraw = draw}

-- | A label, then an 80x40 custom widget with this content key and drawing.
afterLabel :: Int -> CustomDrawBuild -> NanoUI Response
afterLabel key draw = column (label "Other" >> custom (fixedWH 80 40) key draw)

-- | Draw a rect of the colour.
filled :: Color -> CustomDrawBuild
filled c cdc r = runCanvasFor cdc (drawRect r c)

-- | Run a frame and decode the quads it drew.
frameQuads :: Context -> Input -> NanoUI a -> IO [(Rect, Color)]
frameQuads ctx inp ui = runFrame ctx inp ui >>= \(_, draw, _) -> drawQuads draw

-- | 'widgetMeasure' sizes a widget and the containers sized from it, as a
-- fresh solve does, even with the arena unchanged. A progress bar takes the
-- caller's width mode.
runCustomWidgetMeasureTest :: Context -> IORef Int -> IO ()
runCustomWidgetMeasureTest ctx failed = do
  let inp = withInput 400 400
      measured m = fst <$> customWidget defaultCustomWidgetSpec {widgetMeasure = m, widgetLayout = defaultLayout}
      sizeOf ui = (\(Rect _ _ w h) -> (w, h)) . respRect <$> warmup2 ctx inp ui
  mapM (sizeOf . column . measured) [Just (\_ _ -> (160, 48)), Nothing, Just (\_ _ -> (160, 48))]
    >>= assertEq failed [(160, 48), (32, 32), (160, 48)]
  -- The inner column takes its width from the widget, and the label after
  -- it in the row sits where that width ends.
  let parent w = column . row $ column (measured (Just (\_ _ -> (w, 48)))) <* label "after"
  sizeOf (parent 120) >> sizeOf (parent 240) >>= assertEq failed 240 . fst
  grown <- arenaRects ctx
  assertEq failed grown =<< (newContext >>= \fresh -> warmup2 fresh inp (parent 240) >> arenaRects fresh)
  let bar f = rowWith (fixedW 300 . tight) (progressBarWith' f 12 0.3)
  (fullW, _) <- sizeOf (bar id)
  sizeOf (bar (percent 50)) >>= assertEq failed (fullW / 2, 12)

-- | 'widgetCursor' (kept through a drag off the widget), 'widgetInteract',
-- a queued post-layout click, and the reference knob, which a pinned button
-- covers for drags and the wheel ('useDrag2DOn' uses the response).
runCustomWidgetPointerTest :: Context -> IORef Int -> IO ()
runCustomWidgetPointerTest ctx failed = do
  let inp = withInput 300 300
      edged = withCursorShape UiCursorHidden . column $ fst <$> customWidget defaultCustomWidgetSpec
        { widgetLayout = fixedWH 80 80 defaultLayout
        , widgetCursor = Just $ \_ (Rect x _ w _) (V2 px _) -> if px > x + w - 10 then UiCursorEwResize else UiCursorDefault
        }
      cursor = getInput >>= liftIO . uiCursorKind ctx
  runSim ctx failed inp edged $ do
    Rect rx ry rw rh <- respRect <$> result
    let onEdge = At (V2 (rx + rw - 4) (ry + rh / 2))
        away = At (V2 (rx + rw + 50) (ry + rh + 50))
    checkEq UiCursorEwResize =<< (moveTo onEdge >> cursor)
    checkEq UiCursorHidden =<< (moveTo (Within (Rect rx ry rw rh)) >> cursor)
    _ <- press onEdge
    checkEq UiCursorEwResize =<< (moveTo away >> cursor)
    _ <- release
    checkEq UiCursorDefault =<< (moveTo away >> cursor)
    checkEq UiCursorHidden (cursorFallback UiCursorHidden)
  let interacting = column $ customWidget defaultCustomWidgetSpec
        { widgetLayout = fixedWH 80 40 defaultLayout
        , widgetInteract = \resp cdc _ -> (resp, (cdcHovered cdc, cdcPressed cdc))
        }
  runSim ctx failed inp interacting $ do
    (resp, (hovered, pressed)) <- click . On . fst =<< result
    check (respClicked resp && hovered && not pressed)
  let queued = column $ (\a b -> [a, b]) <$> canvas (fixedWH 80 40) (\_ -> pure ()) <*> custom (fixedWH 80 40) 0 (\_ _ -> mempty)
      away = inp {inputMousePos = V2 290 290}
  warm <- warmup2 ctx away queued
  forM_ (zip [0 :: Int ..] warm) $ \(i, resp0) -> do
    writeIORef (ctxClickedId ctx) (respId resp0)
    resps <- evalUi ctx away queued
    assertEq failed [j == i | j <- [0 .. length resps - 1]] (map respClicked resps)
  runSim ctx failed inp (column (withValue 25 (knobWith' id 36 0 100))) $ do
    (k, v0) <- result
    checkEq 25 v0
    let V2 x y = centerOf k
    _ <- press (On k)
    (dragged, v1) <- moveTo (At (V2 x (y - 30)))
    check (v1 > 25 && respChanged dragged)
  value <- newVar (0 :: Float)
  let covered = columnWith tight (fst <$> held value (knobWith' id 60 0 100) <* buttonWith' (pinAt 0 0 . fixedWH 30 60) "x")
  runSim ctx failed (withInputOff 300 300) covered $ do
    Rect kx ky _ kh <- respRect <$> result
    let moved how p = liftIO (writeVarIO value 50) >> how p >> liftIO (readVarIO value)
        dragUp (V2 px py) = moveTo (At (V2 px py)) >> drag (At (V2 px py)) (At (V2 px (py - 40))) 1
        wheelOn p = wheel (At p) (V2 0 3)
        (onButton, onKnob) = (V2 (kx + 15) (ky + kh / 2), V2 (kx + 45) (ky + kh / 2))
    checkEq [50, 50] =<< mapM (\how -> moved how onButton) [dragUp, wheelOn]
    check . (> 50) =<< moved dragUp onKnob
    check . (/= 50) =<< moved wheelOn onKnob

-- | An unkeyed drawing repaints when captured state changes. A content key
-- is taken at its word, except that disabling or a new theme rebuilds the
-- ops (a scope's theme without touching font metrics), and a widget that
-- only moved draws at its new place.
runCustomWidgetContentTest :: Context -> IORef Int -> IO ()
runCustomWidgetContentTest ctx failed = do
  let inp = withInputOff 400 300
      ui key on = afterLabel key (filled (if on then red else blue))
      colours c view = map snd <$> frameQuads c inp view
      repaintedIn c r = takeDamage c >>= assert failed . (`clipCovers` respRect r)
  resp <- warmup2 ctx inp (ui 0 False)
  colours ctx (ui 0 True) >>= \cs -> assert failed (red `elem` cs && blue `notElem` cs)
  repaintedIn ctx resp
  bar <- warmup2 ctx inp (column (progressBarWith' id 12 0.2))
  _ <- runFrame ctx inp (column (progressBarWith' id 12 0.8))
  repaintedIn ctx bar
  _ <- warmup2 ctx inp (ui 1 False)
  colours ctx (ui 1 True) >>= assert failed . elem blue
  takeDamage ctx >>= assert failed . not . (`damageCovers` respRect resp)
  colours ctx (ui 2 True) >>= assert failed . elem red
  repaintedIn ctx resp
  let grey = colorRGBA 128 128 128 255
      dimmable off = column $ do
        label "Other"
        disabledWhen off $ custom (fixedWH 80 40) 4 $ \cdc r -> filled (if cdcDisabled cdc then grey else blue) cdc r
  dimCtx <- newContext
  dimmed <- warmup2 dimCtx inp (dimmable False)
  colours dimCtx (dimmable True) >>= assert failed . elem grey
  takeDamage dimCtx >>= assert failed . (`damageCovers` respRect dimmed)
  let moved lead = column (spacer Fit (Fixed lead) >> custom (fixedWH 80 40) 5 (filled red))
  Rect _ my _ _ <- respRect <$> warmup2 ctx inp (moved 40)
  _ <- warmup2 ctx inp (moved 10)
  frameQuads ctx inp (moved 40) >>= assert failed . any (\(Rect _ qy _ _, c) -> c == red && abs (qy - my) < 0.5)
  let (accent2, accent3) = (colorRGBA 7 8 9 255, colorRGBA 11 12 13 255)
      themedUi = afterLabel 3 (\cdc -> filled (themeAccent (cdcTheme cdc)) cdc)
  _ <- warmup2 ctx inp themedUi
  theme0 <- getTheme ctx
  setTheme ctx theme0 {themeAccent = accent2}
  colours ctx themedUi >>= assert failed . elem accent2
  ctx3 <- withTheme ctx theme0 {themeAccent = accent3}
  colours ctx3 themedUi >>= assert failed . elem accent3
  setTheme ctx theme0
  let scoped c = styled (accentColor c) themedUi
  _ <- warmup2 ctx inp (scoped accent2)
  gen <- readIORef (ctxMetricGen ctx)
  colours ctx (scoped accent3) >>= assert failed . elem accent3
  assertEq failed gen =<< readIORef (ctxMetricGen ctx)

-- | 'useDrop' follows the last drag position (SDL may report a drop at
-- (0, 0)), clears it once complete, and gets nothing under a modal or a
-- window on top.
runDropTargetTest :: Context -> IORef Int -> IO ()
runDropTargetTest ctx failed = do
  let inp = withInput 300 300
      dropPoint = V2 60 60
      ui = column (useDrop (Rect 10 10 100 100))
      dropping ds = evalUi ctx inp {inputDrops = SA.smallArrayFromList ds} ui
      file p f = DropEvent DropFile (Just p) f
      (begin, complete) = (DropEvent DropBegin Nothing "", DropEvent DropComplete Nothing "")
  _ <- warmup2 ctx inp ui
  dropping [begin] >>= assert failed . not . dropHovered
  dropping [DropEvent DropPosition (Just dropPoint) ""] >>= \t ->
    assert failed (dropHovered t && dropPosition t == Just dropPoint)
  dropping [file dropPoint "/tmp/a.txt", DropEvent DropText (Just dropPoint) "hello"] >>= \t ->
    assertEq failed (True, ["/tmp/a.txt"], ["hello"]) (dropReceived t, dropFiles t, dropTexts t)
  dropping [file (V2 0 0) "/tmp/origin.txt", complete] >>= \t ->
    assertEq failed (["/tmp/origin.txt"], Just dropPoint) (dropFiles t, dropPosition t)
  dropping [] >>= \t -> assertEq failed (False, Nothing) (dropHovered t, dropPosition t)
  mapM_ dropping [[begin], [DropEvent DropPosition (Just (V2 250 250)) ""]]
  dropping [file (V2 250 250) "/tmp/out.txt", complete] >>= \t -> assertEq failed (False, []) (dropReceived t, dropFiles t)
  open <- newVar True
  let dropAt p =
        (withInput 400 400) {inputDrops = SA.smallArrayFromList [begin, DropEvent DropPosition (Just p) "", file p "a.txt", complete]}
      stacked overlay = (,) <$> useDrop (Rect 0 0 400 400) <*> overlay open (useDrop (Rect 0 0 400 400))
      files c overlay p = (\(page, (_, inner)) -> (dropFiles page, dropFiles <$> inner)) <$> evalUi c (dropAt p) (stacked overlay)
  (_, (onModal, _)) <- warmup2 ctx (withInput 400 400) (stacked (`modal'` "Modal"))
  assertEq failed ([], Just ["a.txt"]) =<< files ctx (`modal'` "Modal") (centerOf onModal)
  assertEq failed ([], Just []) =<< files ctx (`modal'` "Modal") (V2 2 2)
  c <- newContext
  (_, (onWindow, _)) <- warmup2 c (withInput 400 400) (stacked (`window'` "Window"))
  assertEq failed ([], Just ["a.txt"]) =<< files c (`window'` "Window") (centerOf onWindow)
