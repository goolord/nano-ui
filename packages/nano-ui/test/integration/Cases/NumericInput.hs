module Cases.NumericInput (tests) where

import Data.IORef (IORef, newIORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Backend
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, withInput)
import NanoUI.Testing.Harness (held, pressAt, releaseAt, warmup2)
import Spec (Spec, spec)

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
      key k = inp {inputKeys = inputKeysFromList [k]}
  (resp, _) <- warmup2 ctx inp ui
  _ <- step (key KeyTab)
  stepped <- step (key KeyUp)
  assertEq failed stepped 13
  steppedTen <- step ((key KeyUp) {inputModifiers = Modifiers True False False})
  assertEq failed steppedTen 23
  rejected <- step (inp {inputChars = "4x"})
  assertEq failed rejected 23
  clamped <- step (inp {inputChars = "4"})
  assertEq failed clamped 100
  assert failed =<< spanShown ctx "234"
  _ <- step (key KeyEnter)
  assert failed =<< spanShown ctx "100"
  let Rect x y w h = respRect resp
      press = pressAt inp (V2 (x + w - 4) (y + h * 0.75))
  pressed <- step press
  assertEq failed pressed 99
  released <- step (releaseAt press)
  assertEq failed released 99

-- Hexadecimal mode shows upper-case digits, takes hexadecimal typing but no
-- decimal point, and steps like decimal mode.
runNumericInputHexTest :: Context -> IORef Int -> IO ()
runNumericInputHexTest ctx failed = do
  valueRef <- newIORef 255
  let inp = withInput 320 120
      cfg = defaultNumericInputConfig {nicMin = 0, nicHex = True, nicDecimals = 2}
      ui = column (held valueRef (numericInputConfigured' cfg))
      step event = (\((_, v), _, _, _) -> v) <$> runFrame ctx event ui
      key k = inp {inputKeys = inputKeysFromList [k]}
  _ <- warmup2 ctx inp ui
  assert failed =<< spanShown ctx "FF"
  _ <- step (key KeyTab)
  typed <- step (inp {inputChars = "a"})
  assertEq failed typed 0xFFA
  point <- step (inp {inputChars = "."})
  assertEq failed point 0xFFA
  stepped <- step (key KeyUp)
  assertEq failed stepped 0xFFB
  assert failed =<< spanShown ctx "FFB"

spanShown :: Context -> T.Text -> IO Bool
spanShown ctx txt = any (\(_, t, _, _, _) -> t == txt) <$> collectTextSpans ctx
