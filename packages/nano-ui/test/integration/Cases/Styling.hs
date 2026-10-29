{-# OPTIONS_GHC -O0 #-}
module Cases.Styling (tests) where

import Spec
import Data.Bits ((.&.))
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Adornment qualified as A
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (getStyleIdx, lookupNodeByWidgetId, nodeFontBits)
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ spec "disabled-pointer" runDisabledPointerTest
  , simSpec "disabled-focus-order" 300 200 focusOrderUi disabledFocusOrder
  , spec "styled-paint" runStyledPaintTest
  , spec "styled-nesting" runStyledNestingTest
  , spec "font-size-label-fit" runFontSizeLabelFitTest
  , spec "border-left" runBorderLeftTest
  , spec "background-moves-shades" runBackgroundMovesShadesTest
  , spec "label-font-flags" runLabelFontFlagsTest
  ]

inp :: Input
inp = withInputOff 300 200

-- | A press, drag or typing on a disabled widget changes nothing and gives no focus;
-- its fill fades toward the page.
runDisabledPointerTest :: Context -> IORef Int -> IO ()
runDisabledPointerTest ctx failed = do
  let untouched :: (Eq a, Show a) => a -> (Var a -> NanoUI Response) -> IO ()
      untouched initial widget = do
        c <- newContext
        ref <- newVar initial
        runSim c failed (withInputOff 400 200) (column (disabledWhen True (widget ref))) $ do
          r <- result
          let p@(V2 x y) = centerOf r
          _ <- drag (At p) (At (V2 (x + 60) y)) 1
          _ <- typeText "x"
          after <- moveTo (At p)
          checkEq initial =<< readV ref
          check (not (respClicked after) && not (respHovered after))
          check . (/= respId r) =<< focusedId
  untouched False (\_ -> button' "Go")
  untouched False (checkbox' "Check")
  untouched False (toggleSwitchWith' id)
  untouched (50 :: Float) (slider' 0 100)
  untouched (50 :: Float) (knobWith' id 36 0 100)
  untouched (0 :: Int) (radio' ["One", "Two"])
  untouched (0 :: Int) (select' ["One", "Two"])
  untouched ("initial" :: Text) textInput'
  untouched (5 :: Double) numericInput'
  theme <- getTheme ctx
  let enabledBg = styleBg (themeButton theme)
      fadedBg = styleBg (themeButton (disabledTheme theme))
  quads <- map snd <$> (drawQuads . snd =<< warmupDraw ctx inp (column (disabledWhen True (button "Off"))))
  assert failed (fadedBg /= enabledBg && fadedBg `elem` quads && enabledBg `notElem` quads)

focusOrderUi :: NanoUI (Response, Response)
focusOrderUi = column ((,) <$> button' "A" <* disabledWhen True (button' "B") <*> button' "C")

-- | Tab skips a disabled widget.
disabledFocusOrder :: Sim (Response, Response) ()
disabledFocusOrder = do
  (a, c) <- result
  focusOn (respId a)
  pressTab 1
  checkEq (respId c) =<< focusedId

-- | A styled scope paints only its own widgets. Changing only its theme repaints but
-- measures no text (it once dropped the text caches on every change).
runStyledPaintTest :: Context -> IORef Int -> IO ()
runStyledPaintTest base failed = do
  measured <- newIORef (0 :: Int)
  let ctx = withMeasureText base (\t -> modifyIORef' measured (+ 1) >> pure (8 * fromIntegral (T.length t), 16))
      ui c = column $ do
        label "Some text"
        (,) <$> styled (buttonStyle (background c)) (button' "Red") <*> button' "Plain"
      red = colorRGBA 200 30 40 255
  ((inside, outside), draw) <- warmupDraw ctx inp (ui red)
  quads <- drawQuads draw
  assert failed (red `elem` fillsIn (respRect inside) quads && red `notElem` fillsIn (respRect outside) quads)
  writeIORef measured 0
  forM_ [colorRGBA 30 200 60 255, red] $ \c -> do
    (_, recoloured, _) <- runFrame ctx inp (ui c)
    assertEq failed DamageFull =<< takeDamage ctx
    assert failed . elem c . map snd =<< drawQuads recoloured
  assertEq failed 0 =<< readIORef measured

-- | Nested scopes modify the theme around them.
runStyledNestingTest :: Context -> IORef Int -> IO ()
runStyledNestingTest ctx failed = do
  base <- getTheme ctx
  let teal = colorRGBA 20 160 150 255
  seen <- warmup2 ctx inp $ column $ do
    (o, (i, r)) <- styled (accentColor teal) $ do
      o <- themeAccent <$> uiTheme
      let accentAndRadius t = (themeAccent t, styleCornerRadius (themeButton t))
      ir <- styled (buttonStyle (cornerRadius 9)) (accentAndRadius <$> uiTheme)
      pure (o, ir)
    (,,,) o i r . themeAccent <$> uiTheme
  assertEq failed (teal, teal, 9, themeAccent base) seen

-- | A widget with its own font size is laid out in that font: with 24 px cells "Big" is
-- 72 px plus two 24 px spaces of padding, not the base font's 60 px.
runFontSizeLabelFitTest :: Context -> IORef Int -> IO ()
runFontSizeLabelFitTest base failed = do
  doc <- either fail pure (parseSvg "<svg viewBox='0 0 24 24'><rect width='24' height='24'/></svg>")
  let ctx = withMonospaceFonts 12 24 base
      sized = fontSize 24 defaultLayout
      adornedCfg = defaultButtonConfig {bcLayout = sized, bcAdornments = A.leading (A.iconSized 16 doc)}
      ui = column $ (,,) <$> buttonWith' (fontSize 24) "Big" <*> button' "Big" <*> buttonConfigured' adornedCfg "Big"
  (big, plain, adorned) <- warmup2 ctx (withInputOff 600 300) ui
  labels <- (\ss -> [rectW r | (r, "Big", _, _, _) <- ss, covers (respRect big) r]) <$> collectTextSpans ctx
  assertEq failed ([72], 72 + 2 * 24) (labels, rectW (respRect big))
  assertEq failed (3 * 12 + 2 * 12) (rectW (respRect plain))
  assertEq failed (rectW (respRect big) + 16 + 8) (rectW (respRect adorned))

-- | 'borderLeft' draws one bar down the left edge and no other side.
runBorderLeftTest :: Context -> IORef Int -> IO ()
runBorderLeftTest ctx failed = do
  let col = colorRGBA 220 40 90 255
      ui =
        styled (panelStyle (borderLeft 3 col . cornerRadius 0)) $
          panelWith (fixedWH 120 40 . tight) (snd <$> mouseArea (fillW . fillH) (pure ()))
  (inner, draw) <- warmupDraw ctx inp ui
  quads <- drawQuads draw
  let Rect x y _ h = respRect inner
  assertEq failed [Rect x y 3 h] [q | (q, c) <- quads, c == col]

-- | 'background' moves the hover and press shades with it (a teal field once went back
-- to the theme's well when focused); a shade equal to the old fill follows it.
runBackgroundMovesShadesTest :: Context -> IORef Int -> IO ()
runBackgroundMovesShadesTest ctx failed = do
  base <- getTheme ctx
  ref <- newVar ("text" :: Text)
  let teal = colorRGBA 13 148 136 255
      field = background teal (themeInput base)
      ui = styled (inputStyle (background teal)) $ column $ do
        r <- textInput' ref
        r <$ requestFocus (respId r)
  _ <- warmupDraw ctx inp ui
  (r, draw, _) <- runFrame ctx inp ui
  quads <- drawQuads draw
  assert failed (styleActiveBg field /= styleActiveBg (themeInput base))
  assert failed (styleActiveBg field `elem` fillsIn (respRect r) quads)
  let surface = themePanel base
  assertEq failed (styleHoverBg surface == styleBg surface) (styleHoverBg (background teal surface) == teal)

-- | Button, select and checkbox labels take their layout's font flags (once dropped),
-- leaving the widget's own style bits alone.
runLabelFontFlagsTest :: Context -> IORef Int -> IO ()
runLabelFontFlagsTest ctx failed = do
  base <- getTheme ctx
  sel <- newVar (0 :: Int)
  on <- newVar False
  let flags = fontBold . fontItalic . fontDanger
      ui = column $ do
        b <- buttonWith' flags "Delete"
        s <- currentId
        selectWith flags ["Option"] sel
        c <- currentId
        checkboxWith flags "Bold box" on
        p <- currentId
        checkbox "Plain box" on
        pure (respId b, s, c, p)
  ((b, s, c, p), _) <- warmupDraw ctx (withInputOff 500 300) ui
  painted <- collectTextSpans ctx
  assertEq failed [[themeDanger base] | _ <- [1 .. 3 :: Int]] $
    map (\t -> [fg | (_, t', fg, _, _) <- painted, t' == t]) ["Delete", "Option", "Bold box"]
  let styleOf wid = lookupNodeByWidgetId (ctxNodeArena ctx) wid >>= maybe (pure 0) (getStyleIdx (ctxNodeArena ctx))
  [bs, ss, cs, ps] <- mapM styleOf [b, s, c, p]
  assertEq failed (replicate 3 WeightBold) $
    map textNodeFontWeight [nodeFontBits NodeButton bs, nodeFontBits NodeSelect ss, nodeFontBits NodeButton cs]
  assertEq failed FontStyleItalic (textNodeFontStyle (nodeFontBits NodeButton bs))
  assertEq failed (ps .&. 0xFFFFFFFF) (cs .&. 0xFFFFFFFF)
