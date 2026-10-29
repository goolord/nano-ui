module Cases.Demo (tests) where

import Spec
import Data.Hashable (Hashable (..))
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import NanoUI.Shortcut
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ pixelSpec "color-picker-pointer" runColorPickerPointerTest
  , pixelSpec "color-picker-keys" runColorPickerKeysTest
  , pixelSpec "controls-tab-height" runControlsTabHeightTest
  , pixelSpec "bounded-radio-offset" runBoundedRadioTest
  ]

data DemoTab
  = Controls
  | List
  | Diagnostics
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Hashable DemoTab where
  hashWithSalt salt = hashWithSalt salt . fromEnum


newtype OffsetChoice = OffsetChoice Int
  deriving (Eq, Show)

instance Bounded OffsetChoice where
  minBound = OffsetChoice 10
  maxBound = OffsetChoice 12

instance Enum OffsetChoice where
  fromEnum (OffsetChoice n) = n
  toEnum = OffsetChoice

-- | A bounded radio over an enum not starting at 0 shows, picks and follows its value.
runBoundedRadioTest :: Context -> IORef Int -> IO ()
runBoundedRadioTest ctx failed = do
  v <- newVar (OffsetChoice 11)
  runSim ctx failed (withInputOff 300 160) (snd <$> held v (boundedRadio' (T.pack . show))) $ do
    checkEq (OffsetChoice 11) =<< result
    checkEq (OffsetChoice 12) =<< click "OffsetChoice 12"
    checkEq (OffsetChoice 12) =<< frame
    liftIO (writeVarIO v (OffsetChoice 10))
    checkEq (OffsetChoice 10) =<< frame

-- | Beside a taller column, the demo's controls keep near their height alone, also
-- hovered, and the tab body does not stretch to the column's height.
runControlsTabHeightTest :: Context -> IORef Int -> IO ()
runControlsTabHeightTest ctx failed = do
  let inp0 = withInputOff 1280 800
      controlsBody dumpRef = do
        heading "Controls"
        (cb, _) <- withValue False (checkbox' "Feature")
        _ <- withValue 50 (slider 0 100)
        _ <- withValue 1 (select ["Low", "Medium", "High"])
        (cp, _) <- withValue (colorRGBA 204 102 102 255) colorPicker'
        _ <- withValue (OffsetChoice 11) (boundedRadio (T.pack . show))
        (ti, _) <- withValue "" textInput'
        separator
        liftIO $ writeIORef dumpRef [cb, cp, ti]
      state = [("Feature", "off"), ("Volume", "50"), ("Quality", "Medium"), ("Theme", "Dark")]
        ++ [("Name", "-"), ("Clicked", "-")]

      demoPage dumpRef =
        scrollWith (tight . grow) . columnWith (padAll 8 . gap 8 . fillW) . rowWith (tight . gap 8 . fillW) $ do
          columnWith (tight . gap 8 . fillW) $ do
            card (heading "State" >> mapM_ (uncurry kv) state)
            card (heading "Gallery" >> forM_ [1 .. 8 :: Int] (\i -> label (T.pack ("thumb line " <> show i))))
          card $ do
            demoTabVar <- useEnumVar Controls
            let body t = if t == Controls then controlsBody dumpRef else heading (T.pack (show t))
            tabs demoTabVar [tab t (T.pack (show t)) (body t) | t <- [minBound ..]]
      -- Heights of the checkbox, colour picker and text input, in that order.
      heights c dumpRef = readIORef dumpRef >>= mapM (fmap (fmap rectH) . getPrevRect c . respId)
      extent lbls ss = case [(y, y + h) | (Rect _ y _ h, txt, _, _, _) <- ss, any (`T.isSuffixOf` txt) lbls] of
        [] -> 0
        ys -> maximum (map snd ys) - minimum (map fst ys)
      tooTall pageH loneH = case (pageH, loneH) of
        (Just p, Just l) -> l >= 8 && p > l * 1.35
        _ -> True
      jumped a b = case (a, b) of
        (Just x, Just y) -> abs (x - y) > 1
        _ -> True
      -- The tab body must not fill the wrap-line height of the left column.
      stretched ss =
        let left = extent ["State", "Clicked", "Gallery"] ss
         in left > 80 && extent ["Controls"] ss > left * 0.92
  dumpLone <- newIORef []
  ctxLone <- newPixelContext
  _ <- runFrame ctxLone inp0 (columnWith (tight . fillW) (controlsBody dumpLone))
  lone <- heights ctxLone dumpLone
  dumpPage <- newIORef []
  let page = demoPage dumpPage
  _ <- warmup2 ctx inp0 page
  page0 <- heights ctx dumpPage
  spans0 <- collectTextSpans ctx
  cbRect <- readIORef dumpPage >>= maybe (pure Nothing) (getPrevRect ctx . respId) . listToMaybe
  _ <- runFrame ctx (maybe inp0 (\r -> inp0 {inputMousePos = spanCenter r}) cbRect) page
  pageHover <- heights ctx dumpPage
  spansH <- collectTextSpans ctx
  assertEq failed (3, 3) (length lone, length page0)
  assert failed (not (or (zipWith tooTall page0 lone ++ zipWith tooTall pageHover lone)))
  assert failed (not (or (zipWith jumped page0 pageHover)))
  assert failed (not (stretched spans0 || stretched spansH))

initial :: Color
initial = colorRGBA 204 102 102 255

-- | A script against a fresh picker (with alpha when @rgba@) bound to a 'Var' of @c@.
picker :: IORef Int -> Bool -> Float -> Color -> (Var Color -> Sim (Response, Color) ()) -> IO ()
picker failed rgba h c script = do
  ctx <- newPixelContext
  v <- newVar c
  runSim ctx failed (withInput 400 h) (held v (if rgba then colorPickerRGBA' else colorPicker')) (script v)

-- | The picker's committed ("current") and live colours.
storeColors :: WidgetId -> Sim a (Color, Color)
storeColors wid = do
  st <- simContext >>= liftIO . getStore
  pure (widgetStoreBaseColor st wid initial, widgetStoreColor st wid initial)

-- | A press on the SV field previews, a held one keeps sampling, and the release
-- commits; it takes focus from a channel field, which would pull the colour back.
-- The press and release repaint a clip, not everything.
runColorPickerPointerTest :: Context -> IORef Int -> IO ()
runColorPickerPointerTest _ failed = do
  picker failed False 420 initial $ \_ -> do
    (resp, _) <- result
    let Rect x y w _ = colorPickerSvSquare (respRect resp)
    _ <- press (At (V2 (x + w * 0.9) (y + 2)))
    (base, live) <- storeColors (respId resp)
    check (base == initial && live /= initial)
    _ <- frame
    checkEq live . snd =<< storeColors (respId resp)
    _ <- release
    uncurry checkEq =<< storeColors (respId resp)
  picker failed False 460 initial $ \_ -> do
    Rect x y w h <- colorPickerSvSquare . respRect . fst <$> result
    -- Tab past the field and the hue bar to the R field.
    pressTab 3
    _ <- press (At (V2 (x + 2) (y + 2))) >> moveTo (At (V2 (x + w * 0.9) (y + h * 0.9)))
    -- Low value keeps every channel dark; a field still writing R would leave it at 204.
    checkGt 60 . colorR . snd =<< frame
  picker failed True 1200 initial $ \_ -> do
    ctx <- simContext
    (resp, _) <- result
    _ <- press (On resp)
    check . (`clipCovers` respRect resp) =<< liftIO (takeDamage ctx)
    _ <- release
    check . (/= DamageFull) =<< liftIO (takeDamage ctx)

-- | An arrow on the field changes and commits once, opaque without an alpha bar;
-- channel fields edit live and numerically; bar keys move handles as drawn (Shift
-- ten times, Home and End to the ends); an arrow's repaint covers every part.

runColorPickerKeysTest :: Context -> IORef Int -> IO ()
runColorPickerKeysTest _ failed = do
  picker failed False 420 initial $ \_ -> do
    wid <- respId . fst <$> result
    pressTab 1
    check . respChanged . fst =<< pressKey KeyRight
    (base, live) <- storeColors wid
    check (live /= initial)
    checkEq base live
    checkEq [False, False] =<< replicateM 2 (respChanged . fst <$> frame)
  picker failed False 420 (colorRGBA 204 102 102 128) $ \v -> do
    pressTab 1 >> void (pressKey KeyRight)
    checkEq 255 . colorA =<< liftIO (readVarIO v)
  picker failed False 460 initial $ \_ -> do
    wid <- respId . fst <$> result
    pressTab 3
    _ <- pressKey KeyUp
    (base, live) <- storeColors wid
    checkEq (205, live) (colorR live, base)
    replicateM_ 3 (pressKey KeyBackspace)
    (_, typed) <- typeText "10"
    checkEq (10, 102) (colorR typed, colorG typed)
    checkEq 11 . colorR . snd =<< pressKey KeyUp
    checkEq 11 . colorR . snd =<< typeText "x"
  picker failed True 460 (colorRGBA 204 102 102 200) $ \_ -> do
    mapM_ expect ["#cc6666c8", "200", "Current", "New"]
    pressTab 2
    checkLt 112 . colorG . snd =<< chord (shift <> key KeyDown)
    checkGt 104 . colorG . snd =<< pressKey KeyHome
    _ <- pressKey KeyTab
    checkEq 255 . colorA . snd =<< pressKey KeyEnd
    checkEq 254 . colorA . snd =<< pressKey KeyUp
  picker failed True 1200 initial $ \_ -> do
    ctx <- simContext
    pressTab 1 >> void frame
    _ <- pressKey KeyRight
    dmg <- liftIO (takeDamage ctx)
    parts <- map snd <$> liftIO (nodesOfType NodeColorPicker ctx)
    checkEq 4 (length parts)
    check (all (clipCovers dmg) parts)
