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
import NanoUI.Internal.Context (wakeFromThread)
import NanoUI.Runner
import NanoUI.Shortcut (ctrl, key)

tests :: [Spec]
tests =
  [ spec "session-loop" runSessionLoopTest
  , spec "session-loop-wakes" runSessionLoopWakesTest
  , spec "session-loop-hard-quit" runSessionLoopHardQuitTest
  , spec "session-loop-keys" runSessionLoopKeysTest
  , spec "session-loop-close" runSessionLoopCloseTest
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

-- | One batch of events per driver wait. A wait past the last batch throws,
-- so a runaway loop fails the test instead of hanging.
batchedWaits :: [[Int]] -> IO (Int -> IO [Int])
batchedWaits batches = do
  queue <- newIORef batches
  pure $ \_ -> atomicModifyIORef' queue (\case b : rest -> (rest, Just b); [] -> ([], Nothing))
    >>= maybe (throwIO (userError "the loop went on past its last events")) pure

-- | Run the session loop over @batches@ ('batchedWaits'), drawing on every
-- pass unless @edit@, which adjusts the driver, says otherwise.
runLoop :: Context -> (SessionDriver Int -> SessionDriver Int) -> [[Int]] -> IO ()
runLoop ctx edit batches = do
  debug <- newDebugSampler
  waits <- batchedWaits batches
  clearDirty ctx
  let driver = edit (quietDriver debug) {sdWaitEvents = waits, sdShouldDraw = \_ _ _ _ _ -> pure True}
  runSessionLoop driver ctx emptyInput

-- | A draw that runs one frame of @view@.
viewing :: NanoUI a -> Context -> Input -> Bool -> IO Bool
viewing view c inp _ = False <$ evalUi c inp view

-- | 'runLoop', counting the frames drawn.
drawCount :: Context -> (SessionDriver Int -> SessionDriver Int) -> [[Int]] -> IO Int
drawCount ctx edit batches = do
  drawn <- newIORef (0 :: Int)
  let counted d = let d' = edit d in d' {sdDraw = \c inp dbg -> modifyIORef' drawn (+ 1) >> sdDraw d' c inp dbg}
  runLoop ctx counted batches
  readIORef drawn

-- | Notes each frame's close request, quitting on one once @quitAfter@ holds
-- of those noted before.
closeWatcher :: IORef [Bool] -> ([Bool] -> Bool) -> NanoUI ()
closeWatcher seen quitAfter = do
  closing <- winCloseRequested <$> askWindow
  before <- liftIO (atomicModifyIORef' seen (\s -> (s <> [closing], s)))
  when (closing && quitAfter before) quitUi

-- | Queued edges, a dirty follow-up frame, skipped input and blocking waits,
-- without a native window or wall-clock timing.
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
        { sdPollEvents = [] <$ note "poll"
        , sdWaitEvents = \timeout -> do
            note ("wait " <> show timeout)
            (expected, events) <- atomicModifyIORef' waits $ \case
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
    =<< readIORef logRef
  assertEq failed 3 =<< readIORef draws

-- | Quick presses count up to a triple click; another button restarts.
runSessionLoopClicksTest :: Context -> IORef Int -> IO ()
runSessionLoopClicksTest ctx failed = do
  clicks <- newIORef []
  let buttonOf = \case
        1 -> Just (MouseLeft, True)
        2 -> Just (MouseLeft, False)
        4 -> Just (MouseRight, True)
        _ -> Nothing
  runLoop
    ctx
    ( \d ->
        d
          { sdApplyEvent = \inp e -> maybe inp (\(b, down) -> applyMouseButton b down inp) (buttonOf e)
          , sdIsButtonEdge = \e -> buttonOf e /= Nothing
          , sdDraw = \_ inp _ ->
              False <$ when (anyButtonPressed inp) (modifyIORef' clicks (<> [inputMouseClicks inp]))
          }
    )
    [[1, 2, 1, 2, 1, 2, 4], [3]]
  assertEq failed [1, 2, 3, 1] =<< readIORef clicks

-- | A requested wake bounds the idle wait (sleeping to it, not polling) and
-- draws when due; a thread's wake draws through 'shouldRedrawFrame' alone; a
-- wake a month away fits the 32-bit millisecond timeout backends take.
runSessionLoopWakesTest :: Context -> IORef Int -> IO ()
runSessionLoopWakesTest ctx failed = do
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
                [] <$ threadDelay (max 0 timeout * 1000)
              else [3] <$ note ("wait " <> show timeout)
        , sdShouldDraw = \_ _ _ _ due -> due <$ note ("due " <> show due)
        , sdDraw = \_ _ _ -> False <$ note "draw"
        }
  clearDirty ctx
  requestWakeAfter ctx 0.03
  runSessionLoop driver ctx emptyInput
  assertEq failed ["timed wait", "due True", "draw", "wait -1"] =<< readIORef logRef
  assertEq failed 0 =<< getWakeAt ctx
  let threadWake d =
        d
          { sdWaitEvents = \t -> do
              events <- sdWaitEvents d t
              -- A job finishing ends the wait with no event of its own.
              when (null events) (wakeFromThread ctx)
              pure events
          , sdShouldDraw = \c prev cur wasAnim due -> shouldRedrawFrame c prev cur wasAnim False due
          }
  assertEq failed 1 =<< drawCount ctx threadWake [[], [3]]
  timeouts <- newIORef []
  clearDirty ctx
  requestWakeAfter ctx (30 * 86400)
  let farWake = (quietDriver debug) {sdWaitEvents = \timeout -> [3] <$ modifyIORef' timeouts (timeout :)}
  runSessionLoop farWake ctx emptyInput
  ts <- readIORef timeouts
  assert failed (not (null ts) && all (\t -> t > 0 && t <= 2147483647) ts)

-- | Ctrl+C quits after its frame (Ctrl released later in the batch too, a
-- bare c not), unless a shortcut or a focused text field takes it; with
-- 'wsExitOnCloseRequest' off it asks to close instead. Events: 1 Ctrl+C,
-- 2 releases Ctrl, 4 types c, 5 Tab, others nothing.
runSessionLoopHardQuitTest :: Context -> IORef Int -> IO ()
runSessionLoopHardQuitTest ctx failed = do
  copies <- newIORef (0 :: Int)
  let typed inp = \case
        1 -> chordInp (ctrl <> key 'c') inp
        2 -> inp {inputModifiers = (inputModifiers inp) {modCtrl = False}}
        4 -> inp {inputChars = inputChars inp <> "c"}
        5 -> chordInp (key KeyTab) inp
        _ -> inp
      quits = drawCount ctx (\d -> d {sdApplyEvent = typed})
      frames view = drawCount ctx (\d -> d {sdApplyEvent = typed, sdDraw = viewing view})
      copy = whenM (shortcut (ctrl <> key 'c')) (liftIO (modifyIORef' copies (+ 1)))
  assertEq failed 1 =<< quits [[1, 2], [6], [3]]
  assertEq failed 1 =<< quits [[1], [6], [3]]
  assertEq failed 2 =<< quits [[4, 2], [6], [3]]
  assertEq failed 2 =<< frames copy [[1], [6], [3]]
  assertEq failed 1 =<< readIORef copies
  -- Two Tabs focus the field; one more wraps round to the button.
  let fields = button "b" >> void (withValue "" textInput')
  assertEq failed 4 =<< frames fields [[5], [5], [1], [6], [3]]
  assertEq failed 2 =<< frames fields [[5], [1]]
  installWindowHost ctx defaultWindowSettings {wsExitOnCloseRequest = False} defaultWindowHost
  seen <- newIORef []
  assertEq failed 2 =<< frames (closeWatcher seen (const True)) [[1], [6]]
  assertEq failed [False, True] =<< readIORef seen

-- | A batch splits into frames so text precedes each frame's one command key
-- and every key sees its own modifiers; repeats and typing share a frame.
-- Each auto-repeat or retyped character draws, though the input is equal.
runSessionLoopKeysTest :: Context -> IORef Int -> IO ()
runSessionLoopKeysTest ctx failed = do
  frames <- newIORef []
  -- 1 types "l" with its key, 2 presses Enter, 3 types "x", 4 holds Ctrl,
  -- 5 presses S, 6 releases S, 7 releases Ctrl.
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
      record _ inp _ =
        False <$ modifyIORef' frames (<> [(inputChars inp, toList (inputKeys inp), modCtrl (inputModifiers inp))])
      keyed d = d {sdApplyEvent = apply, sdIsSessionQuit = (== 0), sdDraw = record}
      run batches = writeIORef frames [] >> runLoop ctx keyed (batches ++ [[0]]) >> readIORef frames
  let ctrlS = ("", [KeyChar 's'], True)
  assertEq failed [("ll", [KeyChar 'l', KeyChar 'l', KeyEnter, KeyEnter], False), ("x", [], False), ctrlS, ("", [], False)]
    =<< run [[1, 1, 2, 2, 3, 4, 5, 6, 7]]
  -- One event per batch, as in steady typing, adds no frames.
  assertEq failed 7 . length =<< run (map pure [1, 2, 3, 4, 5, 6, 7])
  -- 1 presses Down (a repeat while held), 2 releases it, and 4 types "a".
  let typeA inp =
        applyKey (KeyChar 'a') False (applyKey (KeyChar 'a') True inp) {inputChars = inputChars inp <> "a"}
      repeats d =
        d
          { sdApplyEvent = \inp -> \case
              1 -> applyKey KeyDown True inp
              2 -> applyKey KeyDown False inp
              4 -> typeA inp
              _ -> inp
          , sdShouldDraw = \c prev cur anim due -> shouldRedrawFrame c prev cur anim False due
          }
  assertEq failed 7 =<< drawCount ctx repeats [[1], [1], [1], [1], [2], [4], [4], [3]]

-- | With 'wsExitOnCloseRequest' on, a close request, or 'quitUi' in a frame
-- drawn before the loop, ends the session. Off, each request reaches the
-- next frame only (past a pass that draws none) until one calls 'quitUi'.
runSessionLoopCloseTest :: Context -> IORef Int -> IO ()
runSessionLoopCloseTest _ failed = do
  let fresh exitOnClose = do
        ctx <- newContext
        let settings = defaultWindowSettings {wsExitOnCloseRequest = exitOnClose}
        ctx <$ installWindowHost ctx settings defaultWindowHost
  exits <- fresh True
  assertEq failed 1 =<< drawCount exits id [[], [3]]
  quitFirst <- fresh True
  evalUi quitFirst emptyInput quitUi
  assertEq failed 0 =<< drawCount quitFirst id []
  asks <- fresh False
  seen <- newIORef []
  runLoop asks (\d -> d {sdDraw = viewing (closeWatcher seen ((== 1) . length . filter id))}) [[3], [], [3]]
  assertEq failed [True, False, True] =<< readIORef seen
  skips <- fresh False
  decisions <- newIORef (0 :: Int)
  seenAfterSkip <- newIORef []
  runLoop
    skips
    ( \d ->
        d
          { -- The first pass, the close request's, draws nothing.
            sdShouldDraw = \_ _ _ _ _ -> atomicModifyIORef' decisions (\n -> (n + 1, n > 0))
          , sdDraw = viewing (closeWatcher seenAfterSkip (const True))
          }
    )
    [[3], [], [3]]
  assertEq failed [True] =<< readIORef seenAfterSkip

-- | The drawing lock runs its action unmasked, is not re-entrant, and is
-- free again after an action that throws.
runDrawingLockTest :: Context -> IORef Int -> IO ()
runDrawingLockTest _ failed = do
  lock <- newDrawingLock
  result <- tryWithDrawingLock lock $ do
    assertEq failed Unmasked =<< getMaskingState
    assertEq failed Nothing =<< tryWithDrawingLock lock (pure ())
  assertEq failed (Just ()) result
  failure <- try @IOException (tryWithDrawingLock lock (throwIO (userError "draw failed")))
  assertEq failed True (either (const True) (const False) failure)
  assertEq failed (Just ()) =<< tryWithDrawingLock lock (pure ())
