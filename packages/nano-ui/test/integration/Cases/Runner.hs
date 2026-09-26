module Cases.Runner (tests) where

import Spec
import Control.Concurrent (threadDelay)
import Data.Foldable (toList)
import Control.Exception
  ( IOException
  , MaskingState (Unmasked)
  , getMaskingState
  , throwIO
  , try
  )
import NanoUI.Internal.Debug (DebugSamplerRef, newDebugSampler)
import NanoUI.Runner

tests :: [Spec]
tests =
  [ spec "session-loop" runSessionLoopTest
  , spec "session-loop-wake" runSessionLoopWakeTest
  , spec "session-loop-hard-quit" runSessionLoopHardQuitTest
  , spec "session-loop-key-order" runSessionLoopKeyOrderTest
  , spec "session-loop-close-request" runSessionLoopCloseTest
  , spec "session-loop-close-asks-the-view" runSessionLoopCloseAskTest
  , spec "session-loop-clicks" runSessionLoopClicksTest
  , spec "drawing-lock" runDrawingLockTest
  ]

-- | A driver with no events, nothing to draw, and event 3 as the window's
-- close. Each test overrides the fields it watches.
quietDriver :: DebugSamplerRef -> SessionDriver Int
quietDriver debug =
  SessionDriver
    { sdPollEvents = pure []
    , sdWaitEvents = \_ -> pure [3]
    , sdApplyEvent = \inp _ -> inp
    , sdIsButtonEdge = const False
    , sdIsSessionQuit = (== 3)
    , sdSyncDisplay = \c inp -> pure (c, inp)
    , sdDebug = debug
    , sdContinuous = False
    , sdPacingMs = 16
    , sdPresentPaces = pure False
    , sdAlignSec = 0
    , sdShouldDraw = \_ _ _ _ _ -> pure False
    , sdDraw = \_ _ _ -> pure False
    , sdOnCursor = \_ _ -> pure ()
    , sdShouldQuit = const False
    }

-- Exercise queued edges, a dirty follow-up frame, skipped input, and blocking
-- waits without requiring a native window or depending on wall-clock timing.
runSessionLoopTest :: Context -> IORef Int -> IO ()
runSessionLoopTest ctx failed = do
  logRef <- newIORef []
  waits <- newIORef [(-1, [1, 2]), (0, []), (-1, []), (-1, [3 :: Int])]
  draws <- newIORef (0 :: Int)
  debug <- newDebugSampler
  let
    note message = modifyIORef' logRef (<> [message])
    driver =
      (quietDriver debug)
        { sdPollEvents = do
            note "poll"
            pure []
        , sdWaitEvents = \timeout -> do
            note ("wait " <> show timeout)
            (expected, events) <- atomicModifyIORef' waits $ \batches -> case batches of
              batch : rest -> (rest, batch)
              [] -> ([], (timeout, [3]))
            assertEq failed expected timeout
            pure events
        , sdApplyEvent = \inp event -> inp {inputMousePos = V2 (fromIntegral event) 0}
        , sdIsButtonEdge = const True
        , sdShouldDraw = \_ previous current _ _ -> do
            note ("decide " <> show (inputMousePos previous, inputMousePos current))
            pure (inputMousePos current == V2 1 0)
        , sdDraw = \_ _ _ -> do
            n <- atomicModifyIORef' draws (\n -> (n + 1, n + 1))
            note "draw"
            pure (n <= 2)
        , sdOnCursor = \_ _ -> note "cursor"
        }
  -- A new context starts dirty, which would make the first wait immediate.
  clearDirty ctx
  runSessionLoop driver ctx emptyInput
  actual <- readIORef logRef
  assertEq
    failed
    [ "wait -1"
    , "decide " <> show (V2 0 0, V2 1 0)
    , "draw"
    , "cursor"
    , "draw"
    , "cursor"
    , "poll"
    , "wait 0"
    , "draw"
    , "cursor"
    , "wait -1"
    , "decide " <> show (V2 2 0, V2 2 0)
    , "cursor"
    , "wait -1"
    ]
    actual
  assertEq failed 3 =<< readIORef draws

-- Quick presses of one button count up to a triple click; a press of
-- another button restarts the count.
runSessionLoopClicksTest :: Context -> IORef Int -> IO ()
runSessionLoopClicksTest ctx failed = do
  clicks <- newIORef []
  debug <- newDebugSampler
  batches <- newIORef [[1, 2, 1, 2, 1, 2, 4 :: Int]]
  let buttonOf = \case
        1 -> Just (MouseLeft, True)
        2 -> Just (MouseLeft, False)
        4 -> Just (MouseRight, True)
        _ -> Nothing
      driver =
        (quietDriver debug)
          { sdWaitEvents = \_ -> atomicModifyIORef' batches (\case b : rest -> (rest, b); [] -> ([], [3]))
          , sdApplyEvent = \inp e -> maybe inp (\(b, down) -> applyMouseButton b down inp) (buttonOf e)
          , sdIsButtonEdge = \e -> buttonOf e /= Nothing
          , sdShouldDraw = \_ _ _ _ _ -> pure True
          , sdDraw = \_ inp _ -> False <$ when (anyButtonPressed inp) (modifyIORef' clicks (<> [inputMouseClicks inp]))
          }
  clearDirty ctx
  runSessionLoop driver ctx emptyInput
  assertEq failed [1, 2, 3, 1] =<< readIORef clicks

-- A requested wake bounds the idle wait and draws when it comes due, and the
-- loop blocks again once nothing asks for another. No pass runs in between:
-- the loop sleeps to the deadline instead of polling toward it.
runSessionLoopWakeTest :: Context -> IORef Int -> IO ()
runSessionLoopWakeTest ctx failed = do
  logRef <- newIORef []
  waits <- newIORef (0 :: Int)
  debug <- newDebugSampler
  let
    note message = modifyIORef' logRef (<> [message])
    driver =
      (quietDriver debug)
        { sdWaitEvents = \timeout -> do
            n <- atomicModifyIORef' waits (\n -> (n + 1, n))
            if n == 0
              then do
                note (if timeout > 0 && timeout <= 40 then "timed wait" else "wait " <> show timeout)
                threadDelay (max 0 timeout * 1000)
                pure []
              else do
                note ("wait " <> show timeout)
                pure [3]
        , sdShouldDraw = \_ _ _ _ due -> do
            note ("due " <> show due)
            pure due
        , sdDraw = \_ _ _ -> do
            note "draw"
            pure False
        }
  clearDirty ctx
  requestWakeAfter ctx 0.03
  runSessionLoop driver ctx emptyInput
  assertEq failed ["timed wait", "due True", "draw", "wait -1"] =<< readIORef logRef
  assertEq failed 0 =<< getWakeAt ctx

-- Ctrl+C quits without a frame, even when a later event in the same batch
-- releases Ctrl; typing c without Ctrl does not. Event 1 presses C holding
-- Ctrl, 2 releases Ctrl, and 4 types c alone.
runSessionLoopHardQuitTest :: Context -> IORef Int -> IO ()
runSessionLoopHardQuitTest ctx failed = do
  debug <- newDebugSampler
  let ctrl on inp = inp {inputModifiers = (inputModifiers inp) {modCtrl = on}}
      typeC inp = inp {inputChars = inputChars inp <> "c"}
      pressC = applyKey (KeyChar 'c') True
      draws batches = do
        queue <- newIORef batches
        drawn <- newIORef (0 :: Int)
        clearDirty ctx
        runSessionLoop
          (quietDriver debug)
            { sdWaitEvents = \_ -> atomicModifyIORef' queue $ \bs -> case bs of
                b : rest -> (rest, b)
                [] -> ([], [3])
            , sdApplyEvent = \inp ev -> case ev of
                1 -> ctrl True (pressC inp)
                2 -> ctrl False inp
                4 -> typeC inp
                _ -> inp
            , sdShouldDraw = \_ _ _ _ _ -> pure True
            , sdDraw = \_ _ _ -> False <$ modifyIORef' drawn (+ 1)
            }
          ctx
          emptyInput
        readIORef drawn
  assertEq failed 0 =<< draws [[1, 2]]
  assertEq failed 0 =<< draws [[1]]
  assertEq failed 1 =<< draws [[4, 2]]

-- | A batch keeps its text, keys and modifiers in order. A frame ends after
-- a command key followed by text, another key or a modifier change, so its
-- text precedes its one command key and that key sees its own modifiers.
-- Repeats and typing share a frame. Events: 1 types "l" with its key, 2
-- presses Enter, 3 types "x", 4 holds Ctrl, 5 presses S, 6 releases S, 7
-- releases Ctrl.
runSessionLoopKeyOrderTest :: Context -> IORef Int -> IO ()
runSessionLoopKeyOrderTest ctx failed = do
  debug <- newDebugSampler
  frames <- newIORef []
  let ctrlOn on inp = inp {inputModifiers = noModifiers {modCtrl = on}}
      apply inp = \case
        1 -> (applyKey (KeyChar 'l') True inp) {inputChars = inputChars inp <> "l"}
        2 -> applyKey KeyEnter True inp
        3 -> inp {inputChars = inputChars inp <> "x"}
        4 -> ctrlOn True inp
        5 -> applyKey (KeyChar 's') True inp
        6 -> applyKey (KeyChar 's') False inp
        7 -> ctrlOn False inp
        _ -> inp
      run batches = do
        writeIORef frames []
        waits <- batchedWaits (batches ++ [[0]])
        clearDirty ctx
        runSessionLoop
          (quietDriver debug)
            { sdWaitEvents = waits
            , sdApplyEvent = apply
            , sdIsSessionQuit = (== 0)
            , sdShouldDraw = \_ _ _ _ _ -> pure True
            , sdDraw = \_ inp _ -> False <$ modifyIORef' frames (<> [(inputChars inp, toList (inputKeys inp), modCtrl (inputModifiers inp))])
            }
          ctx
          emptyInput
        readIORef frames
  assertEq failed [("ll", [KeyChar 'l', KeyChar 'l', KeyEnter, KeyEnter], False), ("x", [KeyChar 's'], True), ("", [], False)]
    =<< run [[1, 1, 2, 2, 3, 4, 5, 6, 7]]
  -- One event per batch, as in steady typing, adds no frames.
  assertEq failed 7 . length =<< run (map pure [1, 2, 3, 4, 5, 6, 7])

-- | One batch of events per driver wait. A wait past the last batch throws,
-- so a runaway loop fails the test instead of hanging.
batchedWaits :: [[Int]] -> IO (Int -> IO [Int])
batchedWaits batches = do
  queue <- newIORef batches
  pure $ \_ -> atomicModifyIORef' queue (\case b : rest -> (rest, Just b); [] -> ([], Nothing))
    >>= maybe (throwIO (userError "the loop went on past its last events")) pure

-- | With 'wsExitOnCloseRequest' on, a close request ends the session
-- without another frame.
runSessionLoopCloseTest :: Context -> IORef Int -> IO ()
runSessionLoopCloseTest ctx failed = do
  debug <- newDebugSampler
  installWindowHost ctx defaultWindowSettings defaultWindowHost
  waits <- batchedWaits [[], [3]]
  drawn <- newIORef (0 :: Int)
  clearDirty ctx
  runSessionLoop
    (quietDriver debug)
      { sdWaitEvents = waits
      , sdShouldDraw = \_ _ _ _ _ -> pure True
      , sdDraw = \_ _ _ -> False <$ modifyIORef' drawn (+ 1)
      }
    ctx
    emptyInput
  assertEq failed 1 =<< readIORef drawn

-- | With 'wsExitOnCloseRequest' off, each close request is visible to the
-- next frame only, and the session ends after the frame that calls 'quitUi'.
runSessionLoopCloseAskTest :: Context -> IORef Int -> IO ()
runSessionLoopCloseAskTest ctx failed = do
  debug <- newDebugSampler
  installWindowHost ctx defaultWindowSettings {wsExitOnCloseRequest = False} defaultWindowHost
  waits <- batchedWaits [[3], [], [3]]
  seen <- newIORef []
  let view = do
        closing <- winCloseRequested <$> askWindow
        n <- uiIO (atomicModifyIORef' seen (\s -> (s <> [closing], length (filter id s))))
        when (closing && n == 1) quitUi
  clearDirty ctx
  runSessionLoop
    (quietDriver debug)
      { sdWaitEvents = waits
      , sdShouldDraw = \_ _ _ _ _ -> pure True
      , sdDraw = \c inp _ -> False <$ evalUi c inp view
      }
    ctx
    emptyInput
  assertEq failed [True, False, True] =<< readIORef seen

runDrawingLockTest :: Context -> IORef Int -> IO ()
runDrawingLockTest _ failed = do
  lock <- newDrawingLock
  result <- tryWithDrawingLock lock $ do
    assertEq failed Unmasked =<< getMaskingState
    assertEq failed Nothing =<< tryWithDrawingLock lock (pure ())
  assertEq failed (Just ()) result
  failure <-
    try (tryWithDrawingLock lock (throwIO (userError "draw failed"))) ::
      IO (Either IOException (Maybe ()))
  assertEq failed True (either (const True) (const False) failure)
  assertEq failed (Just ()) =<< tryWithDrawingLock lock (pure ())
