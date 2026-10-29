{-# OPTIONS_GHC -O0 #-}
module Cases.NumericInput (tests) where

import Spec
import Data.Text qualified as T
import NanoUI.Shortcut qualified as Shortcut
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ spec "numeric-input" runNumericInputTest
  , spec "numeric-input-command" runNumericInputCommandTest
  ]

-- | Each script drives a fresh field with its config and value, focused by
-- Tab, and checks the values its frames return.
runNumericInputTest :: Context -> IORef Int -> IO ()
runNumericInputTest _ failed =
  forM_ cases $ \(cfg, initial, script) -> do
    ctx <- newContext
    ref <- newVar initial
    runSim ctx failed (withInput 320 120) (column (held ref (numericInputConfigured' cfg))) $
      pressTab 1 >> script
  where
    range = defaultNumericInputConfig {nicMin = 0, nicMax = 100}
    values = fmap (map snd) . sequence
    shown txt = check =<< labelShown txt
    cases =
      [ -- The arrows step (Shift ten times as far), typing that is not a
        -- number is dropped, typing clamps to the range, and Enter rewrites
        -- the text as the clamped value. The stepper's lower half steps down.
        ( range
        , 12
        , do
            (resp, _) <- result
            let shiftUp = chord (Shortcut.shift <> Shortcut.key KeyUp)
            checkEq [13, 23, 23, 100] =<< values [pressKey KeyUp, shiftUp, typeText "4x", typeText "4"]
            shown "234"
            _ <- pressKey KeyEnter
            shown "100"
            let Rect x y w h = respRect resp
            checkEq [99, 99] =<< values [press (At (V2 (x + w - 4) (y + h * 0.75))), release]
        )
      , -- Hexadecimal shows upper-case digits, takes hexadecimal typing but no
        -- decimal point, and steps like decimal.
        ( defaultNumericInputConfig {nicMin = 0, nicHex = True, nicDecimals = 2}
        , 255
        , do
            shown "FF"
            checkEq [0xFFA, 0xFFA, 0xFFB] =<< values [typeText "a", typeText ".", pressKey KeyUp]
            shown "FFB"
        )
      , -- Focus keeps a value its decimals do not show rather than reading
        -- the shown text back.
        (range, 2.5, checkEq [2.5, 2.5] =<< values [result, frame])
      , -- A number typed without its leading zero is read, so Enter keeps it.
        ( defaultNumericInputConfig {nicDecimals = 2}
        , 0
        , forM_ [(".5", 0.5, "0.50"), ("-.25", -0.25, "-0.25")] $ \(typed, value, txt :: T.Text) -> do
            _ <- chord (Shortcut.cmdOrCtrl <> Shortcut.key 'a') >> typeText typed
            checkEq value . snd =<< pressKey KeyEnter
            shown txt
        )
      ]

-- | A command run on a focused numeric field from above it in the view, before
-- this frame declares it, still edits it, and pulses 'respChanged' once,
-- leaving no change behind for later frames.
runNumericInputCommandTest :: Context -> IORef Int -> IO ()
runNumericInputCommandTest ctx failed = do
  valueRef <- newVar 12
  pending <- newIORef []
  field <- newIORef Nothing
  -- The commands run before the field.
  let ui = column $ do
        cmds <- liftIO (atomicModifyIORef' pending ([],))
        liftIO (readIORef field) >>= mapM_ (\wid -> mapM_ (runTextCommand wid) cmds)
        (resp, v) <- held valueRef numericInput'
        (respChanged resp, v) <$ liftIO (writeIORef field (Just (respId resp)))
  runSim ctx failed (withInput 320 120) ui $ do
    pressTab 1
    liftIO (writeIORef pending [ReplaceAll "42"])
    checkEq [(True, 42), (False, 42), (False, 42)] =<< replicateM 3 frame
