module Cases.NumericInput (tests) where

import Spec
import Data.Text qualified as T

tests :: [Spec]
tests =
  [ spec "numeric-input" runNumericInputTest
  , spec "numeric-input-hex" runNumericInputHexTest
  ]

-- A numeric field steps with the arrow keys (Shift steps ten times as far),
-- drops typing that is not a number, clamps to its range while typing, and
-- Enter rewrites the text as the clamped value. The stepper's lower half steps
-- down.
runNumericInputTest :: Context -> IORef Int -> IO ()
runNumericInputTest ctx failed = do
  valueRef <- newIORef 12
  let inp = withInput 320 120
      cfg = defaultNumericInputConfig {nicMin = 0, nicMax = 100}
      ui = column (held valueRef (numericInputConfigured' cfg))
      step event = (\((_, v), _, _, _) -> v) <$> runFrame ctx event ui
      key k = keyInp k inp
  (resp, _) <- warmup2 ctx inp ui
  _ <- step (key KeyTab)
  assertEq failed 13 =<< step (key KeyUp)
  assertEq failed 23 =<< step ((key KeyUp) {inputModifiers = Modifiers True False False False})
  assertEq failed 23 =<< step (inp {inputChars = "4x"})
  assertEq failed 100 =<< step (inp {inputChars = "4"})
  assert failed =<< spanShown ctx "234"
  _ <- step (key KeyEnter)
  assert failed =<< spanShown ctx "100"
  let Rect x y w h = respRect resp
      press = pressAt inp (V2 (x + w - 4) (y + h * 0.75))
  assertEq failed 99 =<< step press
  assertEq failed 99 =<< step (releaseAt press)

-- Hexadecimal mode shows upper-case digits, takes hexadecimal typing but no
-- decimal point, and steps like decimal mode.
runNumericInputHexTest :: Context -> IORef Int -> IO ()
runNumericInputHexTest ctx failed = do
  valueRef <- newIORef 255
  let inp = withInput 320 120
      cfg = defaultNumericInputConfig {nicMin = 0, nicHex = True, nicDecimals = 2}
      ui = column (held valueRef (numericInputConfigured' cfg))
      step event = (\((_, v), _, _, _) -> v) <$> runFrame ctx event ui
      key k = keyInp k inp
  _ <- warmup2 ctx inp ui
  assert failed =<< spanShown ctx "FF"
  _ <- step (key KeyTab)
  assertEq failed 0xFFA =<< step (inp {inputChars = "a"})
  assertEq failed 0xFFA =<< step (inp {inputChars = "."})
  assertEq failed 0xFFB =<< step (key KeyUp)
  assert failed =<< spanShown ctx "FFB"

spanShown :: Context -> T.Text -> IO Bool
spanShown ctx txt = any (\(_, t, _, _, _) -> t == txt) <$> collectTextSpans ctx
