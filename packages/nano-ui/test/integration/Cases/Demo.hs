module Cases.Demo (tests) where

import Spec
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (NodeType (..), arenaCount, getNodeRect, getNodeType)
import NanoUI.Shortcut

tests :: [Spec]
tests =
  [ pixelSpec "color-picker-commit" runColorPickerCommitTest
  , pixelSpec "color-picker-rgba" runColorPickerRgbaTest
  , pixelSpec "color-picker-part-damage" runColorPickerPartDamageTest
  , pixelSpec "color-picker-drag-damage" runColorPickerDragDamageTest
  , pixelSpec "color-picker-edit" runColorPickerEditTest
  , pixelSpec "color-picker-change-once" runColorPickerChangeOnceTest
  , pixelSpec "color-picker-bar-keys" runColorPickerBarKeysTest
  , pixelSpec "color-picker-drag-after-field" runColorPickerDragAfterFieldTest
  , pixelSpec "controls-tab-height" runControlsTabHeightTest
  , pixelSpec "bounded-radio-offset" runBoundedRadioTest
  ]

data DemoTab
  = Controls
  | List
  | Diagnostics
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data DemoTheme
  = Light
  | Dark
  | System
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

newtype OffsetChoice = OffsetChoice Int
  deriving (Eq, Show)

instance Bounded OffsetChoice where
  minBound = OffsetChoice 10
  maxBound = OffsetChoice 12

instance Enum OffsetChoice where
  fromEnum (OffsetChoice n) = n
  toEnum = OffsetChoice

runBoundedRadioTest :: Context -> IORef Int -> IO ()
runBoundedRadioTest ctx failed = do
  let inp = withInputOff 300 160
      ui = boundedRadio' (T.pack . show)
  (_, initial) <- warmup2 ctx inp (ui (OffsetChoice 11))
  assertEq failed initial (OffsetChoice 11)
  spans <- collectTextSpans ctx
  assertJust failed (spanRect "OffsetChoice 12" spans) $ \r -> do
    (_, selected) <- runClick ctx inp (ui (OffsetChoice 11)) (spanCenter r)
    assertEq failed selected (OffsetChoice 12)
    assertEq failed selected . snd =<< evalUi ctx inp (ui selected)
    assertEq failed (OffsetChoice 10) . snd =<< evalUi ctx inp (ui (OffsetChoice 10))

runControlsTabHeightTest :: Context -> IORef Int -> IO ()
runControlsTabHeightTest ctx failed = do
  let
    inp0 =
      withInputOff 1280 800
    controlsBody dumpRef = do
      heading "Controls"
      (cb, _) <- checkbox' "Feature" False
      _ <- slider 0 100 50
      _ <- select ["Low", "Medium", "High"] 1
      (cp, _) <- colorPicker' (colorRGBA 204 102 102 255)
      _ <- boundedRadio (T.pack . show) Dark
      (ti, _) <- textInput' ""
      separator
      uiIO $ writeIORef dumpRef (Just (cb, cp, ti))
    demoPage dumpRef =
      scrollWith (tight . grow) $
        columnWith (padAll 8 . gap 8 . fillW) $
          rowWith (tight . gap 8 . fillW) $ do
            columnWith (tight . gap 8 . fillW) $ do
              card $ do
                heading "State"
                kv "Feature" "off"
                kv "Volume" "50"
                kv "Quality" "Medium"
                kv "Theme" (T.pack (show Dark))
                kv "Name" "-"
                kv "Clicked" "-"
              card $ do
                heading "Gallery"
                mapM_ (\i -> void (label (T.pack ("thumb line " <> show (i :: Int))))) [1 .. 8]
            card $ do
              (demoTab, setDemoTab) <- useEnum Controls
              setDemoTab
                =<< tabs
                  demoTab
                  [ tab t (T.pack (show t)) $ case t of
                      Controls -> controlsBody dumpRef
                      List -> heading "Tree"
                      Diagnostics -> heading "Diagnostics"
                  | t <- [minBound ..]
                  ]
    -- Heights of the checkbox, colour picker and text input, in that order.
    heights c dumpRef = do
      m <- readIORef dumpRef
      case m of
        Just (cb, cp, ti) -> mapM (fmap (fmap rectH) . getPrevRect c . respId) [cb, cp, ti]
        Nothing -> pure [Nothing, Nothing, Nothing]
    spanOf lbls spans =
      let
        match txt =
          any
            (\lbl -> txt == lbl || T.drop 1 txt == lbl || T.isSuffixOf lbl txt)
            lbls
        ys =
          [ (y, y + h)
          | (Rect _ y _ h, txt, _, _, _) <- spans
          , match txt
          ]
       in
        case ys of
          [] -> 0
          _ -> maximum (map snd ys) - minimum (map fst ys)
  dumpLone <- newIORef Nothing
  ctxLone <- newPixelContext
  _ <- runFrame ctxLone inp0 (columnWith (tight . fillW) (controlsBody dumpLone))
  lone <- heights ctxLone dumpLone
  dumpPage <- newIORef Nothing
  let page = demoPage dumpPage
  _ <- warmup2 ctx inp0 page
  page0 <- heights ctx dumpPage
  spans0 <- collectTextSpans ctx
  dumped <- readIORef dumpPage
  cbRect <- maybe (pure Nothing) (\(cb, _, _) -> getPrevRect ctx (respId cb)) dumped
  let hover = maybe inp0 (\r -> inp0 {inputMousePos = spanCenter r}) cbRect
  _ <- runFrame ctx hover page
  pageHover <- heights ctx dumpPage
  spansH <- collectTextSpans ctx
  let
    left0 = spanOf ["State", "Clicked", "Gallery"] spans0
    body0 = spanOf ["Controls"] spans0
    leftH = spanOf ["State", "Clicked", "Gallery"] spansH
    bodyH = spanOf ["Controls"] spansH
    tooTall pageH loneH = case (pageH, loneH) of
      (Just p, Just l) -> l >= 8 && p > l * 1.35
      _ -> True
    jumped a b = case (a, b) of
      (Just x, Just y) -> abs (x - y) > 1
      _ -> True
  assert failed (not (or (zipWith tooTall page0 lone)))
  assert failed (not (or (zipWith tooTall pageHover lone)))
  assert failed (not (or (zipWith jumped page0 pageHover)))
  -- Body must not fill the wrap-line height of the left column.
  assert failed (not (left0 > 80 && body0 > left0 * 0.92))
  assert failed (not (leftH > 80 && bodyH > leftH * 0.92))

-- A press on the SV field previews without committing, a held press keeps
-- sampling it without blanking the field or resetting it to white, and the
-- release commits.
runColorPickerCommitTest :: Context -> IORef Int -> IO ()
runColorPickerCommitTest ctx failed = do
  let initial = colorRGBA 204 102 102 255
  colorRef <- newIORef initial
  let inp0 = withInput 400 420
      ui = held colorRef colorPicker'
  (resp, _) <- warmup2 ctx inp0 ui
  let sv = colorPickerSvSquare (respRect resp)
      press = pressAt inp0 (V2 (rectX sv + rectW sv * 0.9) (rectY sv + 2))
      colors = (\st -> (widgetStoreBaseColor st (respId resp) initial, widgetStoreColor st (respId resp) initial)) <$> getStore ctx
  _ <- runFrame ctx press ui
  (baseDrag, drag) <- colors
  assertEq failed baseDrag initial
  assert failed (drag /= initial)
  _ <- runFrame ctx press {inputButtonsPressed = noButtons} ui
  assertEq failed drag . snd =<< colors
  _ <- runFrame ctx (releaseAt press) ui
  (baseDone, done) <- colors
  assertEq failed baseDone done

-- Moving the colour with an arrow key on the focused field moves the markers
-- on the hue and alpha bars and recolours the preview swatch, which are
-- sibling parts with their own rects. The picker's state is keyed on the
-- container that holds them, so the frame repaints a clip covering every part.
runColorPickerPartDamageTest :: Context -> IORef Int -> IO ()
runColorPickerPartDamageTest ctx failed = do
  colorRef <- newIORef (colorRGBA 204 102 102 255)
  let inp0 = withInput 400 1200
      ui = held colorRef colorPickerRGBA'
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (tabInp inp0) ui
  _ <- runFrame ctx inp0 ui
  _ <- takeDamage ctx
  _ <- runFrame ctx (keyInp KeyRight inp0) ui
  dmg <- takeDamage ctx
  let na = ctxNodeArena ctx
  n <- arenaCount na
  parts <- mapM (getNodeRect na) =<< filterM (fmap (== NodeColorPicker) . getNodeType na) [0 .. n - 1]
  -- The field, hue bar, alpha bar and preview.
  assertEq failed (length parts) 4
  assert failed (all (clipCovers dmg) parts)

-- A press that starts a drag on the SV field, and the release that ends it,
-- repaint a clip. The drag hooks' held flags are bookkeeping no node owns;
-- were they diffed like widget state they would escalate both frames to
-- DamageFull.
runColorPickerDragDamageTest :: Context -> IORef Int -> IO ()
runColorPickerDragDamageTest ctx failed = do
  colorRef <- newIORef (colorRGBA 204 102 102 255)
  let inp0 = withInput 400 1200
      ui = held colorRef colorPickerRGBA'
  (resp, _) <- warmup2 ctx inp0 ui
  _ <- takeDamage ctx
  let (press, release) = clickPair inp0 (centerOf resp)
  _ <- runFrame ctx press ui
  pressDmg <- takeDamage ctx
  assert failed (clipCovers pressDmg (respRect resp))
  _ <- runFrame ctx release ui
  releaseDmg <- takeDamage ctx
  assert failed (releaseDmg /= DamageFull)

runColorPickerRgbaTest :: Context -> IORef Int -> IO ()
runColorPickerRgbaTest ctx failed = do
  let inp0 = withInputOff 400 460
      ui = void (colorPickerRGBA (colorRGBA 204 102 102 128))
  _ <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  forM_ ["#cc666680", "128", "Current", "New"] $ \needle -> assertSpansHas failed needle spans

-- Typing in a channel field must recolour on the same frame (live edits). The
-- fields are numeric: Up steps the focused one, and letters are dropped.
runColorPickerEditTest :: Context -> IORef Int -> IO ()
runColorPickerEditTest ctx failed = do
  let inp0 = withInput 400 460
      initial = colorRGBA 204 102 102 255
      ui = colorPicker' initial
  _ <- warmup2 ctx inp0 ui
  -- Tab past the field and the hue bar to the R field.
  replicateM_ 3 (runFrame ctx (tabInp inp0) ui)
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyBackspace, KeyBackspace, KeyBackspace]}) ui
  ((_, col), _, _, _) <- runFrame ctx (inp0 {inputChars = "10"}) ui
  assertEq failed (colorR col) 10
  assertEq failed (colorG col) 102
  ((_, stepped), _, _, _) <- runFrame ctx (keyInp KeyUp inp0) ui
  assertEq failed (colorR stepped) 11
  ((_, lettered), _, _, _) <- runFrame ctx (inp0 {inputChars = "x"}) ui
  assertEq failed (colorR lettered) 11

-- A channel field that had focus must not pull the colour back while the
-- canvas is dragged: pressing the canvas takes focus away from the field.
runColorPickerDragAfterFieldTest :: Context -> IORef Int -> IO ()
runColorPickerDragAfterFieldTest ctx failed = do
  let initial = colorRGBA 204 102 102 255
  colorRef <- newIORef initial
  let inp0 = withInput 400 460
      ui = held colorRef colorPicker'
  (resp, _) <- warmup2 ctx inp0 ui
  -- Tab past the field and the hue bar to the R field.
  replicateM_ 3 (runFrame ctx (tabInp inp0) ui)
  let sv = colorPickerSvSquare (respRect resp)
      press = pressAt inp0 (V2 (rectX sv + 2) (rectY sv + 2))
      drag = holdAt press (V2 (rectX sv + rectW sv * 0.9) (rectY sv + rectH sv * 0.9))
  _ <- runFrame ctx press ui
  _ <- runFrame ctx drag ui
  ((_, col), _, _, _) <- runFrame ctx drag ui
  -- Low value keeps every channel dark; a field still writing R would leave
  -- it at 204.
  assert failed (colorR col < 60)

-- respChanged fires on the frame the colour moves and not on later frames. A key step
-- commits at once: the base colour follows the live one.
runColorPickerChangeOnceTest :: Context -> IORef Int -> IO ()
runColorPickerChangeOnceTest ctx failed = do
  let initial = colorRGBA 204 102 102 255
  colorRef <- newIORef initial
  let inp0 = withInput 400 420
      ui = held colorRef colorPicker'
      changed inp = (\((resp, _), _, _, _) -> respChanged resp) <$> runFrame ctx inp ui
  (resp, _) <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (tabInp inp0) ui
  assert failed =<< changed (keyInp KeyRight inp0)
  store <- getStore ctx
  let neu = widgetStoreColor store (respId resp) initial
  assert failed (neu /= initial)
  assertEq failed (widgetStoreBaseColor store (respId resp) initial) neu
  assertEq failed [False, False] =<< mapM changed [inp0, inp0]

-- The hue and alpha bars are focus stops after the field. An arrow moves a
-- bar's handle the way it is drawn (down raises the hue, up lowers the alpha),
-- Shift steps ten times as far, and Home / End jump to the bar's ends.
runColorPickerBarKeysTest :: Context -> IORef Int -> IO ()
runColorPickerBarKeysTest ctx failed = do
  let initial = colorRGBA 204 102 102 200
  colorRef <- newIORef initial
  let inp0 = withInput 440 460
      ui = held colorRef colorPickerRGBA'
      frame inp = snd <$> evalUi ctx inp ui
      press k = keyInp k inp0
  _ <- warmup2 ctx inp0 ui
  _ <- frame (press KeyTab)
  _ <- frame (press KeyTab)
  shifted <- frame (chordInp (shift <> key KeyDown) inp0)
  assert failed (colorG shifted > colorG initial + 10)
  home <- frame (press KeyHome)
  assert failed (colorG home <= colorG initial + 1)
  _ <- frame (press KeyTab)
  opaque <- frame (press KeyEnd)
  assertEq failed (colorA opaque) 255
  lowered <- frame (press KeyUp)
  assertEq failed (colorA lowered) 254
