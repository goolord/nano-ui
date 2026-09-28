-- | Labels cut to their room, toggle buttons, and the colour helpers.
module Cases.Labels (tests) where

import Data.Text qualified as T
import Spec

tests :: [Spec]
tests =
  [ spec "button-label-truncates" runButtonTruncateTest
  , spec "select-label-truncates" runSelectTruncateTest
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
    warmup2 ctx inp (column (selectWith' (fixedW 120) [long, "Short"] 0))
  cut <- spansIn ctx (respRect resp)
  let
    Rect x _ w _ = respRect resp
  case cut of
    [(r, t)] -> do
      assert failed ("..." `T.isSuffixOf` t)
      -- Clear of the chevron at the right end.
      assert failed (rectX r + rectW r <= x + w - 16)
    _ -> assertEq failed (length cut) 1

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
  ref <- newIORef False
  let
    inp = withInputOff 300 200
    ui = column (held ref (toggleButton' Warning "M"))
  (resp, _) <- warmup2 ctx inp ui
  _ <- runClick ctx inp ui (centerOf resp)
  assertEq failed True =<< readIORef ref
  tint <- themeWarning <$> getTheme ctx
  (_, draw) <- warmupDraw ctx inp ui
  quads <- drawQuads draw
  assert failed (tint `elem` fillsIn (respRect resp) quads)
  _ <- runClick ctx inp ui (centerOf resp)
  assertEq failed False =<< readIORef ref

runColorHelpersTest :: Context -> IORef Int -> IO ()
runColorHelpersTest _ failed = do
  assertEq failed (colorRGB 1 2 3) (colorRGBA 1 2 3 255)
  assertEq failed (withAlpha (colorRGB 10 20 30) 0.5) (colorRGBA 10 20 30 128)
  assertEq failed (withAlpha colorWhite 2) colorWhite
  assertEq failed (withAlpha colorBlack (-1)) (colorRGBA 0 0 0 0)
  assertEq failed colorTransparent (colorRGBA 0 0 0 0)
