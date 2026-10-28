module Cases.Styling (tests) where

import Spec
import Control.Exception (ErrorCall, evaluate, try)
import NanoUI.Adornment qualified as A
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (getStyleIdx, lookupNodeByWidgetId, nodeFontBits)
import Data.Bits ((.&.))
import Data.Text (Text)
import Data.Text qualified as T

tests :: [Spec]
tests =
  [ spec "disabled-pointer" runDisabledPointerTest
  , spec "disabled-focus-order" runDisabledFocusOrderTest
  , spec "disabled-look" runDisabledLookTest
  , spec "styled-paint" runStyledPaintTest
  , spec "styled-nesting" runStyledNestingTest
  , spec "styled-damage" runStyledDamageTest
  , spec "styled-keeps-text-caches" runStyledKeepsTextCachesTest
  , spec "font-size-label-fit" runFontSizeLabelFitTest
  , spec "border-left" runBorderLeftTest
  , spec "background-moves-shades" runBackgroundMovesShadesTest
  , spec "label-font-flags" runLabelFontFlagsTest
  , spec "byte-enums" runByteEnumsTest
  ]

-- | The enums packed into a byte list just their own values.
runByteEnumsTest :: Context -> IORef Int -> IO ()
runByteEnumsTest _ failed = do
  assertEq failed "[LinesStart,LinesCenter,LinesEnd]" (show [minBound .. maxBound :: LineAlign])
  assertEq failed "[PointerAuto,PointerBlock,PointerPass]" (show [minBound :: PointerMode ..])
  assertEq failed "[BreakWords,BreakAnywhere]" (show [minBound :: TextBreak ..])
  assertEq failed "[LinesStart,LinesEnd]" (show [LinesStart, LinesEnd ..])
  assertEq failed [0, 1, 2] (map fromEnum [PointerAuto ..])
  -- 'succ' and 'pred' stop at the bounds, as a derived enum's do.
  assertEq failed (LinesCenter, PointerBlock) (succ LinesStart, pred PointerPass)
  past <-
    forM [fromEnum (succ LinesEnd), fromEnum (pred PointerAuto), fromEnum (succ BreakAnywhere)] $ \n ->
      either (const True) (const False) <$> try @ErrorCall (evaluate n)
  assertEq failed [True, True, True] past

-- | A pointer press, drag and typing on a disabled widget change nothing and
-- give it no focus.
runDisabledPointerTest :: Context -> IORef Int -> IO ()
runDisabledPointerTest _ failed = do
  let inp = withInputOff 400 200
      check :: (Eq a, Show a) => String -> a -> (Var a -> NanoUI Response) -> IO ()
      check name initial widget = do
        ctx <- newContext
        ref <- newVar initial
        let ui = column (disabledWhen True (held ref widget))
        (resp, _) <- warmup2 ctx inp ui
        let V2 cx cy = centerOf resp
            press = pressAt inp (V2 cx cy)
            dragged = holdAt press (V2 (cx + 60) cy)
        mapM_ (\i -> runFrame ctx i ui) [press, dragged, releaseAt dragged]
        _ <- runFrame ctx inp {inputMousePos = V2 cx cy, inputChars = "x"} ui
        (after, _) <- evalUi ctx inp ui
        value <- readVarIO ref
        focus <- readIORef (ctxFocusId ctx)
        when (value /= initial || focus == respId resp) $ putStrLn ("  disabled " <> name <> " changed or took focus")
        assertEq failed value initial
        assert failed (not (respClicked after) && not (respHovered after))
        assert failed (focus /= respId resp)
  check "button" False (\_ -> button' "Go")
  check "checkbox" False (checkbox' "Check")
  check "toggle" False (toggleSwitchWith' id)
  check "slider" (50 :: Float) (slider' 0 100)
  check "knob" (50 :: Float) (knobWith' id 36 0 100)
  check "radio" (0 :: Int) (radio' ["One", "Two"])
  check "select" (0 :: Int) (select' ["One", "Two"])
  check "text input" ("initial" :: Text) textInput'
  check "numeric input" (5 :: Double) numericInput'

-- | Tab skips a disabled widget.
runDisabledFocusOrderTest :: Context -> IORef Int -> IO ()
runDisabledFocusOrderTest ctx failed = do
  let inp = withInputOff 300 200
      ui = column $ do
        a <- button' "A"
        _ <- disabledWhen True (button' "B")
        c <- button' "C"
        pure (a, c)
  (a, c) <- warmup2 ctx inp ui
  writeIORef (ctxFocusId ctx) (respId a)
  _ <- runFrame ctx (tabInp inp) ui
  assertEq failed (respId c) =<< readIORef (ctxFocusId ctx)

-- | A disabled button paints its fill faded toward the page background.
runDisabledLookTest :: Context -> IORef Int -> IO ()
runDisabledLookTest ctx failed = do
  theme <- getTheme ctx
  let inp = withInputOff 300 200
      enabledBg = styleBg (themeButton theme)
      fadedBg = styleBg (themeButton (disabledTheme theme))
  (_, draw) <- warmupDraw ctx inp (column (disabledWhen True (button "Off")))
  quads <- drawQuads draw
  assert failed (any ((== fadedBg) . snd) quads)
  assert failed (not (any ((== enabledBg) . snd) quads))
  assert failed (fadedBg /= enabledBg)

-- | A styled scope paints only the widgets inside it with its theme.
runStyledPaintTest :: Context -> IORef Int -> IO ()
runStyledPaintTest ctx failed = do
  let inp = withInputOff 300 200
      red = colorRGBA 200 30 40 255
      ui = column $ do
        inside <- styled (buttonStyle (background red)) (button' "Red")
        outside <- button' "Plain"
        pure (inside, outside)
  ((inside, outside), draw) <- warmupDraw ctx inp ui
  quads <- drawQuads draw
  assert failed (red `elem` fillsIn (respRect inside) quads)
  assert failed (red `notElem` fillsIn (respRect outside) quads)

-- | Nested scopes modify the theme around them, and 'uiTheme' reads it.
runStyledNestingTest :: Context -> IORef Int -> IO ()
runStyledNestingTest ctx failed = do
  base <- getTheme ctx
  let inp = withInputOff 300 200
      teal = colorRGBA 20 160 150 255
  (outerAccent, innerAccent, innerRadius, afterAccent) <-
    warmup2 ctx inp $ column $ do
      (o, (i, r)) <- styled (accentColor teal) $ do
        o <- themeAccent <$> uiTheme
        ir <- styled (buttonStyle (cornerRadius 9)) $ do
          t <- uiTheme
          pure (themeAccent t, styleCornerRadius (themeButton t))
        pure (o, ir)
      a <- themeAccent <$> uiTheme
      pure (o, i, r, a)
  assertEq failed outerAccent teal
  assertEq failed innerAccent teal
  assertEq failed innerRadius 9
  assertEq failed afterAccent (themeAccent base)
  -- A primary button inside a disabled scope is still faded.
  offTheme <- warmup2 ctx inp (column (disabledWhen True (styled primary uiTheme)))
  assertEq failed (styleBg (themeButton offTheme)) (styleBg (themeButton (disabledTheme (primary base))))

-- | Changing only a scope's theme repaints.
runStyledDamageTest :: Context -> IORef Int -> IO ()
runStyledDamageTest ctx failed = do
  let inp = withInputOff 300 200
      ui c = column (styled (buttonStyle (background c)) (button "B"))
      blue = colorRGBA 30 60 200 255
      green = colorRGBA 30 200 60 255
  _ <- warmup2 ctx inp (ui blue)
  forM_ [green, blue] $ \c -> do
    (_, draw, _) <- runFrame ctx inp (ui c)
    assertEq failed DamageFull =<< takeDamage ctx
    quads <- drawQuads draw
    assert failed (any ((== c) . snd) quads)

-- | A scope's theme changes only how nodes look, so a frame that changes it
-- measures no text: the layout and the text caches stand. It once advanced
-- the metric generation, which dropped them all at every change, and every
-- frame for an animated colour.
runStyledKeepsTextCachesTest :: Context -> IORef Int -> IO ()
runStyledKeepsTextCachesTest base failed = do
  measured <- newIORef (0 :: Int)
  let ctx = withMeasureText base (\t -> modifyIORef' measured (+ 1) >> pure (8 * fromIntegral (T.length t), 16))
      inp = withInputOff 300 200
      ui c = column $ do
        label "Some text"
        styled (buttonStyle (background c)) (button "B")
  _ <- warmup2 ctx inp (ui (colorRGBA 30 60 200 255))
  writeIORef measured 0
  _ <- runFrame ctx inp (ui (colorRGBA 30 200 60 255))
  assertEq failed DamageFull =<< takeDamage ctx
  assertEq failed 0 =<< readIORef measured

-- | A widget with its own font size is laid out for its label in that font,
-- which paint draws it in. With a 12 px base font and 24 px cells for the
-- sized text, "Big" draws 72 px wide, and its button is those 72 px and two
-- 24 px spaces of padding, where the base font would have made it 60 px and
-- the label would spill out. A plain button keeps its 12 px size, and an icon
-- beside the sized label adds just the icon and the gap.
runFontSizeLabelFitTest :: Context -> IORef Int -> IO ()
runFontSizeLabelFitTest base failed = do
  doc <- either fail pure (parseSvg "<svg viewBox='0 0 24 24'><rect width='24' height='24'/></svg>")
  let
    ctx = withMonospaceFonts 12 24 base
    inp = withInputOff 600 300
    sized = fontSize 24 defaultLayout
    ui = column $ do
      big <- buttonWith' (fontSize 24) "Big"
      plain <- button' "Big"
      adorned <- buttonConfigured' defaultButtonConfig {bcLayout = sized, bcAdornments = A.leading (A.iconSized 16 doc)} "Big"
      pure (big, plain, adorned)
  (big, plain, adorned) <- warmup2 ctx inp ui
  spans <- collectTextSpans ctx
  case [r | (r, "Big", _, _, _) <- spans, covers (respRect big) r] of
    [labelR] -> do
      assertEq failed 72 (rectW labelR)
      assertEq failed (72 + 2 * 24) (rectW (respRect big))
    labels -> do
      putStrLn ("font-size-label-fit: labels inside the sized button: " <> show labels)
      assert failed False
  assertEq failed (3 * 12 + 2 * 12) (rectW (respRect plain))
  assertEq failed (rectW (respRect big) + 16 + 8) (rectW (respRect adorned))

-- | 'borderLeft' draws one bar down the left edge and no other side.
runBorderLeftTest :: Context -> IORef Int -> IO ()
runBorderLeftTest ctx failed = do
  let inp = withInputOff 300 200
      col = colorRGBA 220 40 90 255
      ui =
        styled (panelStyle (borderLeft 3 col . cornerRadius 0)) $
          panelWith (fixedWH 120 40 . tight) (snd <$> mouseArea (fillW . fillH) (pure ()))
  (inner, draw) <- warmupDraw ctx inp ui
  quads <- drawQuads draw
  let Rect x y _ h = respRect inner
  assertEq failed [Rect x y 3 h] [q | (q, c) <- quads, c == col]

-- | 'background' moves the hover and press shades with it: a focused field
-- fills with the new colour's press shade, not the theme's. It once set the
-- resting fill alone, so a field styled teal went back to the theme's dark
-- well when clicked. Shades equal to the old fill (a panel's hover) stay
-- equal to the new one.
runBackgroundMovesShadesTest :: Context -> IORef Int -> IO ()
runBackgroundMovesShadesTest ctx failed = do
  base <- getTheme ctx
  ref <- newVar ("text" :: Text)
  let teal = colorRGBA 13 148 136 255
      inp = withInputOff 300 200
      field = background teal (themeInput base)
      ui = styled (inputStyle (background teal)) $ column $ do
        r <- textInput' ref
        requestFocus (respId r)
        pure r
  _ <- warmupDraw ctx inp ui
  (r, draw, _) <- runFrame ctx inp ui
  quads <- drawQuads draw
  assert failed (styleActiveBg field /= styleActiveBg (themeInput base))
  assert failed (styleActiveBg field `elem` fillsIn (respRect r) quads)
  let surface = themePanel base
      tinted' = background teal surface
  assertEq failed (styleHoverBg surface == styleBg surface) (styleHoverBg tinted' == teal)

-- | A button's, select's and checkbox's label takes the font flags of its
-- layout: weight, slant and tone. They once shared the style index with the
-- widget's own data and were dropped. The flags leave that data alone: a
-- bold checkbox keeps a plain one's low bits, which say it is a checkbox.
runLabelFontFlagsTest :: Context -> IORef Int -> IO ()
runLabelFontFlagsTest ctx failed = do
  base <- getTheme ctx
  sel <- newVar (0 :: Int)
  on <- newVar False
  let inp = withInputOff 500 300
      flags = fontBold . fontItalic . fontDanger
      ui = column $ do
        b <- buttonWith' flags "Delete"
        s <- currentId
        selectWith flags ["Option"] sel
        c <- currentId
        checkboxWith flags "Bold box" on
        p <- currentId
        checkbox "Plain box" on
        pure (respId b, s, c, p)
  ((b, s, c, p), _) <- warmupDraw ctx inp ui
  spans <- collectTextSpans ctx
  let red = themeDanger base
      fgOf t = [fg | (_, t', fg, _, _) <- spans, t' == t]
  assertEq failed [red] (fgOf "Delete")
  assertEq failed [red] (fgOf "Option")
  assertEq failed [red] (fgOf "Bold box")
  let styleOf wid = lookupNodeByWidgetId (ctxNodeArena ctx) wid >>= maybe (pure 0) (getStyleIdx (ctxNodeArena ctx))
  [bs, ss, cs, ps] <- mapM styleOf [b, s, c, p]
  assertEq failed [WeightBold, WeightBold, WeightBold] [textNodeFontWeight (nodeFontBits NodeButton bs), textNodeFontWeight (nodeFontBits NodeSelect ss), textNodeFontWeight (nodeFontBits NodeButton cs)]
  assertEq failed FontStyleItalic (textNodeFontStyle (nodeFontBits NodeButton bs))
  assertEq failed (ps .&. 0xFFFFFFFF) (cs .&. 0xFFFFFFFF)
