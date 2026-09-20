module Cases.Demo
  ( runControlsTabHeightTest
  , runBoundedRadioTest
  , runColorPickerCommitTest
  , runColorPickerChangeOnceTest
  , runColorPickerBarKeysTest
  , runColorPickerRgbaTest
  , runColorPickerEditTest
  , runColorPickerDragAfterFieldTest
  ) where

import Control.Monad (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertJust, withInput)
import NanoUI.Testing.Harness (held, holdAt, keyInp, pressAt, releaseAt, runClick, spanCenter, spanRect, tabInp, warmup2, withInputOff)

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
    ((_, retained), _, _, _) <- runFrame ctx inp (ui selected)
    assertEq failed retained selected
    ((_, reset), _, _, _) <- runFrame ctx inp (ui (OffsetChoice 10))
    assertEq failed reset (OffsetChoice 10)

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
      packed c = colorToWord32 c
      ui = held colorRef colorPicker'
  (resp, _) <- warmup2 ctx inp0 ui
  let wid = respId resp
      sv = colorPickerSvSquare (respRect resp)
      pt = V2 (rectX sv + rectW sv * 0.9) (rectY sv + 2)
      press = pressAt inp0 pt
      release = releaseAt press
  _ <- runFrame ctx press ui
  storeDrag <- getStore ctx
  assertEq failed (packed (widgetStoreBaseColor storeDrag wid initial)) (packed initial)
  assert failed (packed (widgetStoreColor storeDrag wid initial) /= packed initial)
  _ <- runFrame ctx press {inputMousePressed = False} ui
  storeHold <- getStore ctx
  assertEq
    failed
    (packed (widgetStoreColor storeHold wid initial))
    (packed (widgetStoreColor storeDrag wid initial))
  _ <- runFrame ctx release ui
  storeDone <- getStore ctx
  assertEq
    failed
    (packed (widgetStoreBaseColor storeDone wid initial))
    (packed (widgetStoreColor storeDone wid initial))

runColorPickerRgbaTest :: Context -> IORef Int -> IO ()
runColorPickerRgbaTest ctx failed = do
  let inp0 = withInputOff 400 460
      ui = void (colorPickerRGBA (colorRGBA 204 102 102 128))
  _ <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  let has needle = any (\(_, t, _, _, _) -> needle `T.isInfixOf` t) spans
  assert failed (has "#cc666680")
  assert failed (has "128")
  assert failed (has "Current")
  assert failed (has "New")

-- Typing in a channel field must recolour on the same frame (live edits). The
-- fields are numeric: Up steps the focused one, and letters are dropped.
runColorPickerEditTest :: Context -> IORef Int -> IO ()
runColorPickerEditTest ctx failed = do
  let inp0 = withInput 400 460
      initial = colorRGBA 204 102 102 255
      ui = colorPicker' initial
  _ <- warmup2 ctx inp0 ui
  -- Tab past the field and the hue bar to the R field.
  _ <- runFrame ctx (tabInp inp0) ui
  _ <- runFrame ctx (tabInp inp0) ui
  _ <- runFrame ctx (tabInp inp0) ui
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
      tabKey = tabInp inp0
  (resp, _) <- warmup2 ctx inp0 ui
  -- Tab past the field and the hue bar to the R field.
  _ <- runFrame ctx tabKey ui
  _ <- runFrame ctx tabKey ui
  _ <- runFrame ctx tabKey ui
  let sv = colorPickerSvSquare (respRect resp)
      press = pressAt inp0 (V2 (rectX sv + 2) (rectY sv + 2))
      drag = holdAt press (V2 (rectX sv + rectW sv * 0.9) (rectY sv + rectH sv * 0.9))
  _ <- runFrame ctx press ui
  _ <- runFrame ctx drag ui
  ((_, col), _, _, _) <- runFrame ctx drag ui
  -- Low value keeps every channel dark; a field still writing R would leave
  -- it at 204.
  assert failed (colorR col < 60)

-- respChanged fires on the frame the colour moves and not on later frames
-- (regression: it compared the colour against the initial one). A key step
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
  moved <- changed (keyInp KeyRight inp0)
  assert failed moved
  store <- getStore ctx
  let wid = respId resp
      base = colorToWord32 (widgetStoreBaseColor store wid initial)
      neu = colorToWord32 (widgetStoreColor store wid initial)
  assert failed (neu /= colorToWord32 initial)
  assertEq failed base neu
  idle <- mapM changed [inp0, inp0]
  assertEq failed idle [False, False]

-- The hue and alpha bars are focus stops after the field. An arrow moves a
-- bar's handle the way it is drawn (down raises the hue, up lowers the alpha),
-- Shift steps ten times as far, and Home / End jump to the bar's ends.
runColorPickerBarKeysTest :: Context -> IORef Int -> IO ()
runColorPickerBarKeysTest ctx failed = do
  let initial = colorRGBA 204 102 102 200
  colorRef <- newIORef initial
  let inp0 = withInput 440 460
      ui = held colorRef colorPickerRGBA'
      frame inp = (\((_, c), _, _, _) -> c) <$> runFrame ctx inp ui
      key k = inp0 {inputKeys = inputKeysFromList [k]}
  _ <- warmup2 ctx inp0 ui
  _ <- frame (key KeyTab)
  _ <- frame (key KeyTab)
  shifted <- frame ((key KeyDown) {inputModifiers = Modifiers True False False})
  assert failed (colorG shifted > colorG initial + 10)
  home <- frame (key KeyHome)
  assert failed (colorG home <= colorG initial + 1)
  _ <- frame (key KeyTab)
  opaque <- frame (key KeyEnd)
  assertEq failed (colorA opaque) 255
  lowered <- frame (key KeyUp)
  assertEq failed (colorA lowered) 254
