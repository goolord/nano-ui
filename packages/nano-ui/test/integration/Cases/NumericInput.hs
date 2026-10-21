module Cases.NumericInput (tests) where

import Spec
import Data.IntMap.Strict qualified as IM
import Data.Text qualified as T
import NanoUI.Internal.Context (intKey)
import NanoUI.Internal.Store (Slot (..), WidgetStore (..), slotKey)

tests :: [Spec]
tests =
  [ spec "numeric-input" runNumericInputTest
  , spec "numeric-input-hex" runNumericInputHexTest
  , spec "numeric-input-focus-precision" runNumericInputFocusPrecisionTest
  , spec "numeric-input-command" runNumericInputCommandTest
  ]

-- Focusing a field keeps a value its decimals do not show (2.5 shown with
-- none) instead of reading the shown text back.
runNumericInputFocusPrecisionTest :: Context -> IORef Int -> IO ()
runNumericInputFocusPrecisionTest ctx failed = do
  valueRef <- newVar 2.5
  let inp = withInput 320 120
      cfg = defaultNumericInputConfig {nicMin = 0, nicMax = 100}
      ui = column (held valueRef (numericInputConfigured' cfg))
      step event = (\((_, v), _, _) -> v) <$> runFrame ctx event ui
  _ <- warmup2 ctx inp ui
  assertEq failed 2.5 =<< step (keyInp KeyTab inp)
  assertEq failed 2.5 =<< step inp

-- A numeric field steps with the arrow keys (Shift steps ten times as far),
-- drops typing that is not a number, clamps to its range while typing, and
-- Enter rewrites the text as the clamped value. The stepper's lower half steps
-- down.
runNumericInputTest :: Context -> IORef Int -> IO ()
runNumericInputTest ctx failed = do
  valueRef <- newVar 12
  let inp = withInput 320 120
      cfg = defaultNumericInputConfig {nicMin = 0, nicMax = 100}
      ui = column (held valueRef (numericInputConfigured' cfg))
      step event = (\((_, v), _, _) -> v) <$> runFrame ctx event ui
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
  valueRef <- newVar 255
  let inp = withInput 320 120
      cfg = defaultNumericInputConfig {nicMin = 0, nicHex = True, nicDecimals = 2}
      ui = column (held valueRef (numericInputConfigured' cfg))
      step event = (\((_, v), _, _) -> v) <$> runFrame ctx event ui
      key k = keyInp k inp
  _ <- warmup2 ctx inp ui
  assert failed =<< spanShown ctx "FF"
  _ <- step (key KeyTab)
  assertEq failed 0xFFA =<< step (inp {inputChars = "a"})
  assertEq failed 0xFFA =<< step (inp {inputChars = "."})
  assertEq failed 0xFFB =<< step (key KeyUp)
  assert failed =<< spanShown ctx "FFB"

-- A command run on a focused numeric field from above it in the view, before
-- this frame declares it, still edits it, and leaves no change pulse behind.
runNumericInputCommandTest :: Context -> IORef Int -> IO ()
runNumericInputCommandTest ctx failed = do
  valueRef <- newVar 12
  pending <- newIORef []
  field <- newIORef Nothing
  let inp = withInput 320 120
      ui = column $ do
        cmds <- liftIO (readIORef pending <* writeIORef pending [])
        target <- liftIO (readIORef field)
        forM_ target $ \wid -> mapM_ (runTextCommand wid) cmds
        (resp, _) <- held valueRef numericInput'
        liftIO (writeIORef field (Just (respId resp)))
  _ <- warmup2 ctx inp ui
  _ <- runFrame ctx (keyInp KeyTab inp) ui
  writeIORef pending [ReplaceAll "42"]
  _ <- runFrame ctx inp ui
  _ <- runFrame ctx inp ui
  assertEq failed 42 =<< readVarIO valueRef
  key <- maybe 0 intKey <$> readIORef field
  assert failed . not . IM.member (slotKey SlotTextAreaChanged key) . storeInt =<< getStore ctx

spanShown :: Context -> T.Text -> IO Bool
spanShown ctx txt = any (\(_, t, _, _, _) -> t == txt) <$> collectTextSpans ctx
