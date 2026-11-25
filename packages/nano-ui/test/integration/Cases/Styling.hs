{-# OPTIONS_GHC -O0 #-}
module Cases.Styling (tests) where

import Spec
import Data.Bits ((.&.))
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Adornment qualified as A
import NanoUI.Internal.Context (Context (..), RecordedOp (..), drawRecordedOps, setDrawRecordOps)
import NanoUI.Internal.Layout.Arena (arenaCount, getNodeRect, getNodeType, getStyleIdx, lookupNodeByWidgetId, nodeFontBits)
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
  , spec "checkbox-box-center" runCheckboxBoxCenterTest
  , pixelSpec "font-size-scale-relative" runFontSizeScaleRelativeTest
  , pixelSpec "uniform-control-heights" runUniformControlHeightsTest
  , spec "button-group" runButtonGroupTest
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

-- | A checkbox taller than its box centres its square across the full height,
-- aligned with its single-line label, and sizes its box from its own font size.
runCheckboxBoxCenterTest :: Context -> IORef Int -> IO ()
runCheckboxBoxCenterTest base failed = do
  let ctx = withMonospaceFonts 12 24 base
  setDrawRecordOps ctx True
  on <- newVar True
  let ui = columnWith (tight . gap 4) $ do
        r1 <- checkboxWith' (fixedH 48) "Tall" on
        r2 <- checkboxWith' (fontSize 24 . fixedH 48) "Big" on
        pure (r1, r2)
  ((r1, r2), _) <- warmupDraw ctx (withInputOff 300 200) ui
  theme <- getTheme ctx
  ops <- drawRecordedOps ctx
  -- Checked checkbox fills its rounded box in themeAccent.
  let accentBox r = [q | RecordedOp _ _ _ (FillRoundedRect q _ c) <- ops, c == themeAccent theme, covers (respRect r) q]
      centerY (Rect _ y _ h) = y + h / 2
  case (accentBox r1, accentBox r2) of
    (b1 : _, b2 : _) -> do
      assert failed (abs (centerY b1 - centerY (respRect r1)) <= 1.5)
      assert failed (abs (centerY b2 - centerY (respRect r2)) <= 1.5)
      -- 12px font clamps box to 18px; 24px font clamps box to 22px.
      assertGt failed (rectW b2) (rectW b1 + 2)
    other -> assertEq failed (1 :: Int, 1 :: Int) (length (fst other), length (snd other))

-- | 'fontSizeScale' scales relative to the active base font size (not a
-- hardcoded 16), composes multiplicatively, respects an explicit 'fontSize',
-- and scales unsized 'A.icon' adornments on buttons.
runFontSizeScaleRelativeTest :: Context -> IORef Int -> IO ()
runFontSizeScaleRelativeTest base failed = do
  doc <- either fail pure (parseSvg "<svg viewBox='0 0 24 24'><rect width='24' height='24'/></svg>")
  let ctx = withFontMetrics (withFontSize base 12) (monospaceMetrics 12)
      btnPlain = defaultButtonConfig {bcAdornments = A.leading (A.icon doc)}
      btnScaled = defaultButtonConfig {bcLayout = fontSizeScale 1.5 defaultLayout, bcAdornments = A.leading (A.icon doc)}
      ui = columnWith (tight . gap 4) $ do
        labelWith (fontSizeScale 1.5) "Scaled"
        labelWith (fontSizeScale 0.5 . fontSizeScale 1.5) "Composed"
        labelWith (fontSizeScale 1.5 . fontSize 20) "Explicit"
        void (buttonConfigured' btnPlain "PlainBtn")
        void (buttonConfigured' btnScaled "ScaledBtn")
  _ <- warmup2 ctx (withInputOff 500 300) ui
  ss <- collectTextSpans ctx
  assertJust failed (spanRect "Scaled" ss) $ \r -> assertEq failed 18 (rectH r)
  assertJust failed (spanRect "Composed" ss) $ \r -> assertEq failed 9 (rectH r)
  assertJust failed (spanRect "Explicit" ss) $ \r -> assertEq failed 30 (rectH r)
  -- The two NodeImage nodes are the leading icons of PlainBtn (12 * 0.9 = 11)
  -- and ScaledBtn (18 * 0.9 = 16).
  let na = ctxNodeArena ctx
  n <- arenaCount na
  iconWs <-
    concat <$> forM [0 .. n - 1] (\i -> getNodeType na i >>= \nt -> if nt == NodeImage then (: []) . rectW <$> getNodeRect na i else pure [])
  assertEq failed [11, 16] iconWs

-- | Single-line controls (button, select, textInput, searchInput, numericInput,
-- and comboBox) share one intrinsic height across proportional and monospace fonts.
runUniformControlHeightsTest :: Context -> IORef Int -> IO ()
runUniformControlHeightsTest ctx failed = do
  sel <- newVar (0 :: Int)
  txt <- newVar ("abc" :: Text)
  qry <- newVar ("" :: Text)
  num <- newVar (42 :: Double)
  cmb <- newVar ("One" :: Text)
  let rowProp = rowWith (tight . gap 8) $ do
        rBtn <- button' "Save"
        rSel <- select' ["One", "Two"] sel
        rTxt <- textInput' txt
        rSrch <- searchInput' "Find" qry
        rNum <- numericInputConfigured' defaultNumericInputConfig {nicLayout = fixedW 90 defaultLayout} num
        rCmb <- comboBox' "Pick" ["One", "Two"] cmb
        pure (map (rectH . respRect) [rBtn, rSel, rTxt, rSrch, rNum, rCmb])
      rowMono = rowWith (tight . gap 8) $ do
        rBtn <- buttonWith' fontMono "Save"
        rSel <- selectWith' fontMono ["One", "Two"] sel
        rTxt <- textInputConfigured' defaultTextInputConfig {ticLayout = fontMono (ticLayout defaultTextInputConfig)} txt
        rSrch <- searchInputConfigured' defaultSearchInputConfig {sicLayout = fontMono (sicLayout defaultSearchInputConfig)} qry
        rNum <- numericInputConfigured' defaultNumericInputConfig {nicLayout = fontMono (fixedW 90 defaultLayout)} num
        pure (map (rectH . respRect) [rBtn, rSel, rTxt, rSrch, rNum])
  (hsProp, hsMono) <- warmup2 ctx (withInputOff 900 300) $ column ((,) <$> rowProp <*> rowMono)
  case hsProp of
    h0 : rest -> assert failed (h0 > 0 && all (\h -> abs (h - h0) < 0.5) rest)
    [] -> assert failed False
  case hsMono of
    h0 : rest -> assert failed (h0 > 0 && all (\h -> abs (h - h0) < 0.5) rest)
    [] -> assert failed False

-- | 'buttonGroup' joins adjacent buttons with zero gap, outer-only rounded
-- corners, shared 1-pixel internal seams, and automatic neighbour damage when
-- a button is added or removed.
runButtonGroupTest :: Context -> IORef Int -> IO ()
runButtonGroupTest ctx failed = do
  setDrawRecordOps ctx True
  let ui showThird = columnWith (tight . gap 0) $
        buttonGroup $ do
          a <- buttonWith' (fixedW 60) "Left"
          b <- buttonWith' (fixedW 60) "Mid"
          c <- if showThird then Just <$> buttonWith' (fixedW 60) "Right" else pure Nothing
          pure (a, b, c)
  ((a, b, mc), _) <- warmupDraw ctx (withInputOff 400 200) (ui True)
  let ra = respRect a
      rb = respRect b
  assertJust failed mc $ \c -> do
    let rc = respRect c
    -- Adjacent with zero gap.
    assertEq failed (rectX ra + rectW ra) (rectX rb)
    assertEq failed (rectX rb + rectW rb) (rectX rc)
    theme <- getTheme ctx
    ops <- drawRecordedOps ctx
    let borderCol = styleBorder (themeButton theme)
        -- Shared internal 1px dividers sit at the left edge of Mid and Right, not Left.
        dividers = [rectX r | RecordedOp _ _ _ (FillRect r col) <- ops, col == borderCol, rectW r <= 1.5, rectH r >= rectH ra - 1]
    assert failed (any (\x -> abs (x - rectX rb) <= 0.5) dividers)
    assert failed (any (\x -> abs (x - rectX rc) <= 0.5) dividers)
    assert failed (not (any (\x -> abs (x - rectX ra) <= 0.5) dividers))
    -- Middle button's clipped rounded rect extends past both left and right edges.
    let midFills = [r | RecordedOp _ clip _ (FillRoundedRect r _ _) <- ops, clip == rb]
    assert failed (any (\r -> rectX r < rectX rb && rectX r + rectW r > rectX rb + rectW rb) midFills)
  -- Removing "Right" repaints "Mid" on a partial-damage frame because its right
  -- corners change from square to rounded.
  writeIORef (ctxPaintFull ctx) False
  _ <- runFrame ctx (withInputOff 400 200) (ui False)
  dmg <- takeDamage ctx
  assert failed (damageCovers dmg rb)
