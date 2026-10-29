{-# OPTIONS_GHC -O0 #-}
-- | Labels cut to their room, and toggle buttons.
module Cases.Labels (tests) where

import Data.Primitive.PrimArray (primArrayFromList)
import Data.Text qualified as T
import NanoUI.Testing.Sim
import Spec

tests :: [Spec]
tests =
  [ spec "label-truncates" runTruncateTest
  , spec "toggle-button" runToggleButtonTest
  ]

-- | The text spans drawn inside a widget's rect.
spansIn :: Context -> Rect -> IO [(Rect, T.Text)]
spansIn ctx r = (\ss -> [(s, t) | (s, t, _, _, _) <- ss, rectIntersect s r /= Nothing]) <$> collectTextSpans ctx

-- | Buttons, selects (short of the chevron) and 'truncateTextUi' end text
-- that does not fit in dots, measured by its shaped width; text that fits,
-- or overflows by under half a pixel, shows whole.
runTruncateTest :: Context -> IORef Int -> IO ()
runTruncateTest base failed = do
  let inp = withInputOff 400 200
      long = "A label much longer than the button"
      dotted = T.isSuffixOf "..."
      cutIn ctx r k = spansIn ctx r >>= \case
        [(s, t)] -> assert failed (dotted t) >> k s t
        cut -> assertEq failed 1 (length cut)
      ui = column $ do
        narrow <- buttonWith' (fixedW 90) long
        fits <- button' "Save"
        (sel, _) <- withValue 0 (selectWith' (fixedW 120) ["An option far too long for the field", "Short"])
        fm <- uiFontMetrics
        fitted <- truncateTextUi fm 60 "Something rather long"
        (,,,) narrow fits sel <$> ((,,) fitted <$> lineWidthUi fm fitted <*> truncateTextUi fm 400 "Short")
  (narrow, fits, sel, (fitted, fittedW, kept)) <- warmup2 base inp ui
  let Rect nx _ nw _ = respRect narrow
      Rect sx _ sw _ = respRect sel
  cutIn base (respRect narrow) $ \(Rect x _ w _) t ->
    assert failed (T.length t < T.length long && x >= nx && x + w <= nx + nw)
  assertEq failed ["Save"] . map snd =<< spansIn base (respRect fits)
  cutIn base (respRect sel) $ \(Rect x _ w _) _ -> assert failed (x + w <= sx + sw - 16)
  assert failed (dotted fitted && fittedW <= 60)
  assertEq failed "Short" kept
  -- Shaping sets a text 8 a character; unshaped advances are 10.
  let shaped t =
        let n = T.length t
         in ShapedText (8 * fromIntegral n) (8 * fromIntegral n) (primArrayFromList [8 * fromIntegral i | i <- [0 .. n]])
      fm = (monospaceMetrics 10) {fmBackend = Just (FontBackend prepare (\_ -> pure Nothing))}
      prepare t = pure fm {fmShape = \t' -> if t' == t then Just (shaped t) else Nothing}
      shapedCtx = withFontMetrics base fm
  resp <- warmup2 shapedCtx inp (column (buttonWith' (fixedW 90) long))
  cutIn shapedCtx (respRect resp) $ \r t -> assertEq failed (8 * fromIntegral (T.length t)) (rectW r)
  let slack = withFontMetrics base (monospaceMetrics 10.04)
  warmup slack inp (column (labelWith (maxW 80) "12345678"))
  assertEq failed ["12345678"] . map snd =<< spansIn slack (Rect 0 0 400 200)

-- | A click turns a toggle button on and fills it in its tone; another
-- turns it off.
runToggleButtonTest :: Context -> IORef Int -> IO ()
runToggleButtonTest ctx failed = do
  ref <- newVar False
  tint <- themeWarning <$> getTheme ctx
  let inp = withInputOff 300 200
      ui = column (toggleButton' Warning "M" ref)
  runSim ctx failed inp ui $ do
    resp <- result
    _ <- click (On resp)
    checkEq True =<< liftIO (readVarIO ref)
    (_, draw) <- liftIO (warmupDraw ctx inp ui)
    check . elem tint . fillsIn (respRect resp) =<< liftIO (drawQuads draw)
    _ <- click (On resp)
    checkEq False =<< liftIO (readVarIO ref)
