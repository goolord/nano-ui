module Cases.Styling (tests) where

import Spec
import NanoUI.Internal.Context (Context (..))
import Data.Text (Text)

tests :: [Spec]
tests =
  [ spec "disabled-pointer" runDisabledPointerTest
  , spec "disabled-focus-order" runDisabledFocusOrderTest
  , spec "disabled-look" runDisabledLookTest
  , spec "styled-paint" runStyledPaintTest
  , spec "styled-nesting" runStyledNestingTest
  , spec "styled-damage" runStyledDamageTest
  ]

-- | A pointer press, drag and typing on a disabled widget change nothing and
-- give it no focus.
runDisabledPointerTest :: Context -> IORef Int -> IO ()
runDisabledPointerTest _ failed = do
  let inp = withInputOff 400 200
      check :: (Eq a, Show a) => String -> a -> (a -> NanoUI (Response, a)) -> IO ()
      check name initial widget = do
        ctx <- newContext
        ref <- newIORef initial
        let ui = column (disabledWhen True (held ref widget))
        (resp, _) <- warmup2 ctx inp ui
        let V2 cx cy = centerOf resp
            (press, release) = clickPair inp (V2 cx cy)
            dragged = press {inputMousePressed = False, inputMousePos = V2 (cx + 60) cy}
        mapM_ (\i -> runFrame ctx i ui) [press, dragged, release {inputMousePos = V2 (cx + 60) cy}]
        _ <- runFrame ctx inp {inputMousePos = V2 cx cy, inputChars = "x"} ui
        (after, _, _, _) <- runFrame ctx inp ui
        value <- readIORef ref
        focus <- readIORef (ctxFocusId ctx)
        assertEqNamed name value initial
        assert failed (not (respClicked (fst after)) && not (respHovered (fst after)))
        if focus /= respId resp then pure () else putStrLn ("  disabled " <> name <> ": took focus")
        assert failed (focus /= respId resp)
      assertEqNamed :: (Eq b, Show b) => String -> b -> b -> IO ()
      assertEqNamed name a b =
        if a == b then pure () else do
          putStrLn ("  disabled " <> name <> ": " <> show a <> " /= " <> show b)
          assertEq failed a b
  check "button" False (\_ -> (\r -> (r, respClicked r)) <$> button' "Go")
  check "checkbox" False (checkbox' "Check")
  check "toggle" False toggleSwitch'
  check "slider" (50 :: Float) (slider' 0 100)
  check "knob" (50 :: Float) (knob' 0 100)
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
  focus <- readIORef (ctxFocusId ctx)
  assertEq failed focus (respId c)

-- | A disabled button paints its fill faded toward the window colour.
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
  let fills r = [c | (q, c) <- quads, rectIntersect q (respRect r) /= Nothing]
  assert failed (red `elem` fills inside)
  assert failed (red `notElem` fills outside)

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
  _ <- takeDamage ctx
  forM_ [green, blue] $ \c -> do
    (_, _, draw, _) <- runFrame ctx inp (ui c)
    dmg <- takeDamage ctx
    assertEq failed dmg DamageFull
    quads <- drawQuads draw
    assert failed (any ((== c) . snd) quads)
