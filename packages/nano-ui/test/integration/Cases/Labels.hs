-- | Labels cut to their room, toggle buttons, and the colour helpers.
module Cases.Labels (tests) where

import Data.Primitive.PrimArray (primArrayFromList)
import Data.Text qualified as T
import Spec

tests :: [Spec]
tests =
  [ spec "button-label-truncates" runButtonTruncateTest
  , spec "select-label-truncates" runSelectTruncateTest
  , spec "truncated-label-shaped" runTruncatedLabelShapedTest
  , spec "label-width-slack" runLabelWidthSlackTest
  , spec "truncate-text-ui" runTruncateTextUiTest
  , spec "toggle-button" runToggleButtonTest
  , spec "color-helpers" runColorHelpersTest
  ]

-- | The text spans drawn inside a widget's rect.
spansIn :: Context -> Rect -> IO [(Rect, T.Text)]
spansIn ctx r = do
  spans <- collectTextSpans ctx
  pure [(s, t) | (s, t, _, _, _) <- spans, rectIntersect s r /= Nothing]

-- | A button narrower than its label ends the label in dots, inside the
-- button; one that fits its label shows all of it.
runButtonTruncateTest :: Context -> IORef Int -> IO ()
runButtonTruncateTest ctx failed = do
  let
    inp = withInputOff 400 200
    long = "A label much longer than the button"
    ui = column $ (,) <$> buttonWith' (fixedW 90) long <*> button' "Save"
  (narrow, fits) <- warmup2 ctx inp ui
  cut <- spansIn ctx (respRect narrow)
  case cut of
    [(r, t)] -> do
      assert failed ("..." `T.isSuffixOf` t && T.length t < T.length long)
      assert
        failed
        ( rectX r >= rectX (respRect narrow)
            && rectX r + rectW r <= rectX (respRect narrow) + rectW (respRect narrow)
        )
    _ -> assertEq failed (length cut) 1
  assertEq failed ["Save"] . map snd =<< spansIn ctx (respRect fits)

-- | A select's label stops short of its chevron, ending in dots.
runSelectTruncateTest :: Context -> IORef Int -> IO ()
runSelectTruncateTest ctx failed = do
  let
    inp = withInputOff 400 200
    long = "An option far too long for the field"
  (resp, _) <-
    warmup2 ctx inp (column (withValue 0 (selectWith' (fixedW 120) [long, "Short"])))
  cut <- spansIn ctx (respRect resp)
  let
    Rect x _ w _ = respRect resp
  case cut of
    [(r, t)] -> do
      assert failed ("..." `T.isSuffixOf` t)
      -- Clear of the chevron at the right end.
      assert failed (rectX r + rectW r <= x + w - 16)
    _ -> assertEq failed (length cut) 1

-- | A button's label cut short is placed by its own shaped width, not by
-- the advances of what is left of the whole label.
runTruncatedLabelShapedTest :: Context -> IORef Int -> IO ()
runTruncatedLabelShapedTest base failed = do
  let
    -- Shaping sets a text 8 a character; unshaped advances are 10.
    shaped t =
      let n = T.length t
       in ShapedText (8 * fromIntegral n) (8 * fromIntegral n) (primArrayFromList [8 * fromIntegral i | i <- [0 .. n]])
    fm = (monospaceMetrics 10) {fmBackend = Just (FontBackend prepare (\_ -> pure Nothing))}
    prepare t = pure fm {fmShape = \t' -> if t' == t then Just (shaped t) else Nothing}
    ctx = withFontMetrics base fm
  resp <- warmup2 ctx (withInputOff 400 200) (column (buttonWith' (fixedW 90) "A label much longer than the button"))
  cut <- spansIn ctx (respRect resp)
  case cut of
    [(r, t)] -> do
      assert failed ("..." `T.isSuffixOf` t)
      assertEq failed (8 * fromIntegral (T.length t)) (rectW r)
    _ -> assertEq failed (length cut) 1

-- | A label that layout keeps on one line, less than half a pixel wider
-- than its room, shows whole rather than cut short.
runLabelWidthSlackTest :: Context -> IORef Int -> IO ()
runLabelWidthSlackTest base failed = do
  let
    ctx = withFontMetrics base (monospaceMetrics 10.04)
  warmup ctx (withInputOff 400 200) (column (labelWith (maxW 80) "12345678"))
  assertEq failed ["12345678"] . map snd =<< spansIn ctx (Rect 0 0 400 200)

-- | 'truncateTextUi' fits text to a width with dots and leaves text that
-- fits alone.
runTruncateTextUiTest :: Context -> IORef Int -> IO ()
runTruncateTextUiTest ctx failed = do
  let
    inp = withInputOff 400 200
  (fitted, kept, width) <- evalUi ctx inp $ do
    fm <- uiFontMetrics
    a <- truncateTextUi fm 60 "Something rather long"
    b <- truncateTextUi fm 400 "Short"
    w <- lineWidthUi fm a
    pure (a, b, w)
  assert failed ("..." `T.isSuffixOf` fitted)
  assert failed (width <= 60)
  assertEq failed kept "Short"

-- | A click turns a toggle button on and fills it in its tone; another
-- turns it off.
runToggleButtonTest :: Context -> IORef Int -> IO ()
runToggleButtonTest ctx failed = do
  ref <- newVar False
  let
    inp = withInputOff 300 200
    ui = column (held ref (toggleButton' Warning "M"))
  (resp, _) <- warmup2 ctx inp ui
  _ <- runClick ctx inp ui (centerOf resp)
  assertEq failed True =<< readVarIO ref
  tint <- themeWarning <$> getTheme ctx
  (_, draw) <- warmupDraw ctx inp ui
  quads <- drawQuads draw
  assert failed (tint `elem` fillsIn (respRect resp) quads)
  _ <- runClick ctx inp ui (centerOf resp)
  assertEq failed False =<< readVarIO ref

runColorHelpersTest :: Context -> IORef Int -> IO ()
runColorHelpersTest _ failed = do
  assertEq failed (colorRGB 1 2 3) (colorRGBA 1 2 3 255)
  assertEq failed (withAlpha (colorRGB 10 20 30) 0.5) (colorRGBA 10 20 30 128)
  assertEq failed (withAlpha colorWhite 2) colorWhite
  assertEq failed (withAlpha colorBlack (-1)) (colorRGBA 0 0 0 0)
  assertEq failed colorTransparent (colorRGBA 0 0 0 0)
