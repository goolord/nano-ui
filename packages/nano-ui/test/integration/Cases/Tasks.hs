module Cases.Tasks (tests) where

import Spec
import Control.Concurrent (newEmptyMVar, putMVar, readMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Exception (AsyncException (StackOverflow), ErrorCall, displayException, finally, onException, throwIO, try)
import Data.List (isInfixOf)
import Data.Function (fix)
import Data.Maybe (isJust)
import GHC.Conc (getUncaughtExceptionHandler, setUncaughtExceptionHandler)
import NanoUI.Internal.Context (Context (ctxWakeLoop), wakeFromThread)
import System.Timeout (timeout)

tests :: [Spec]
tests =
  [ spec "task-result-after-wake" runTaskResultTest
  , spec "task-key-change" runTaskKeyChangeTest
  , spec "task-starts" runTaskStartsTest
  , spec "task-lease" runTaskLeaseTest
  , spec "task-failure" runTaskFailureTest
  , spec "task-shutdown" runTaskShutdownTest
  , spec "burst-costs-one-frame" runBurstTest
  , spec "wake-delivery" runWakeDeliveryTest
  , spec "stream-lifecycle" runStreamLifecycleTest
  ]

inp :: Input
inp = withDelta 200 100 0.016

-- | Run a frame after each wake until the result passes @ok@. 'Nothing' if
-- two seconds pass without a wake.
frameUntil :: (Int -> IO Bool) -> Context -> NanoUI a -> (a -> Bool) -> IO (Maybe a)
frameUntil wait ctx ui ok = go (50 :: Int)
  where
    go n = do
      a <- evalUi ctx inp ui
      if ok a
        then pure (Just a)
        else do
          woke <- wait 2000000
          if woke && n > 0 then go (n - 1) else pure Nothing

-- | Whether the counter reaches @n@ within two seconds.
reaches :: IORef Int -> Int -> IO Bool
reaches ref n = isJust <$> timeout 2000000 (fix (\go -> readIORef ref >>= \v -> unless (v == n) (threadDelay 10000 >> go)))

tick :: IORef Int -> IO ()
tick ref = atomicModifyIORef' ref (\n -> (n + 1, ()))

-- | A job that sleeps until killed, an action that waits for it to start,
-- and a wait of up to @us@ for its kill. A job killed before its thread first
-- runs never runs its handler, so a test that kills a job at once must wait
-- for it to start.
sleeper :: IO (IO (), IO (), Int -> IO Bool)
sleeper = do
  started <- newEmptyMVar
  killed <- newEmptyMVar
  pure
    ( (void (tryPutMVar started ()) >> threadDelay 10000000) `onException` putMVar killed ()
    , takeMVar started
    , \us -> isJust <$> timeout us (takeMVar killed)
    )

-- | A box that shows @on@ only through its colour, which rect and text diffs
-- miss.
lamp :: Bool -> NanoUI ()
lamp on = box (fixedWH 20 20) (if on then colorRGBA 40 200 80 255 else colorRGBA 90 90 90 255)

-- | The next frame shows @v@ and requests no further frame.
settled :: (Eq a, Show a) => Context -> IORef Int -> NanoUI a -> a -> IO ()
settled ctx failed ui v = do
  (a, _, dirty) <- runFrame ctx inp ui
  assertEq failed (v, False) (a, dirty)

-- | The frame that picks up a job's result shows @v@ and repaints fully,
-- since nothing says which widgets depend on it. The next frame is idle.
takesNews :: (Eq a, Show a) => Context -> IORef Int -> NanoUI a -> a -> IO ()
takesNews ctx failed ui v = do
  assertEq failed v =<< evalUi ctx inp ui
  assertEq failed DamageFull =<< takeDamage ctx
  settled ctx failed ui v
  assert failed . damageIsEmpty =<< takeDamage ctx

-- | The hook returns 'Nothing' while the job runs. A running job neither
-- requests frames nor wakes the loop until it ends.
runTaskResultTest :: Context -> IORef Int -> IO ()
runTaskResultTest ctx failed = do
  job <- newTask
  wait <- newWakeSignal ctx
  gate <- newEmptyMVar
  let ui = useTask job ("answer" :: String) (takeMVar gate >> pure (42 :: Int)) >>= \a -> a <$ lamp (isJust a)
  _ <- warmup2 ctx inp ui
  settled ctx failed ui Nothing
  assertEq failed 0 =<< getWakeAt ctx
  _ <- wait 0
  assert failed . not =<< wait 50000
  putMVar gate ()
  assert failed =<< wait 2000000
  takesNews ctx failed ui (Just 42)

-- | A new key kills the old job and starts a new one; until it finishes,
-- 'useTask' returns the previous key's result, and 'useTaskStatus' reports
-- running, then done or failed, carrying it.
runTaskKeyChangeTest :: Context -> IORef Int -> IO ()
runTaskKeyChangeTest ctx failed = do
  job <- newTask
  wait <- newWakeSignal ctx
  (sleep, _, killedIn) <- sleeper
  gate <- newEmptyMVar
  keyRef <- newIORef (1 :: Int)
  let ui = liftIO (readIORef keyRef) >>= \k -> useTask job k (if k == 2 then sleep >> pure k else takeMVar gate >> pure (k * 10))
  assertEq failed Nothing =<< evalUi ctx inp ui
  putMVar gate ()
  assertEq failed (Just (Just 10)) =<< frameUntil wait ctx ui isJust
  forM_ [2, 3] $ \k -> writeIORef keyRef k >> (assertEq failed (Just 10) =<< evalUi ctx inp ui)
  assert failed =<< killedIn 2000000
  _ <- wait 0
  putMVar gate ()
  assertEq failed (Just (Just 30)) =<< frameUntil wait ctx ui (== Just 30)
  statusJob <- newTask
  writeIORef keyRef 1
  let status = do
        k <- liftIO (readIORef keyRef)
        s <- useTaskStatus statusJob k (takeMVar gate >> if k == 1 then pure ("one" :: String) else ioError (userError "two"))
        pure $ case s of
          TaskRunning prev -> ("running" :: String, prev)
          TaskDone a -> ("done", Just a)
          TaskFailed _ prev -> ("failed", prev)
  assertEq failed ("running", Nothing) =<< evalUi ctx inp status
  putMVar gate ()
  assertEq failed (Just ("done", Just "one")) =<< frameUntil wait ctx status ((== "done") . fst)
  writeIORef keyRef 2
  assertEq failed ("running", Just "one") =<< evalUi ctx inp status
  putMVar gate ()
  assertEq failed (Just ("failed", Just "one")) =<< frameUntil wait ctx status ((== "failed") . fst)
  settled ctx failed status ("failed", Just "one")
  cancelTasks ctx

-- | Switching typed owners at the same view position carries no result over
-- (a handle's result type itself cannot change). The same input runs again
-- under a new attempt count. A frame that runs the view twice after a hook
-- write starts its job once.
runTaskStartsTest :: Context -> IORef Int -> IO ()
runTaskStartsTest ctx failed = do
  intJob <- newTask
  stringJob <- newTask
  wait <- newWakeSignal ctx
  gate <- newEmptyMVar
  let asInt = useTask intJob ("same" :: String) (pure (1 :: Int))
      asString = useTask stringJob ("same" :: String) (readMVar gate >> pure ("one" :: String))
  assertEq failed (Just (Just 1)) =<< frameUntil wait ctx asInt isJust
  assertEq failed Nothing =<< evalUi ctx inp asString
  putMVar gate ()
  assertEq failed (Just (Just "one")) =<< frameUntil wait ctx asString isJust
  retried <- newTask
  starts <- newIORef 0
  attempt <- newIORef (0 :: Int)
  let ui = liftIO (readIORef attempt) >>= \n -> useTask retried ("same input" :: String, n) (tick starts >> readIORef starts)
  assertEq failed (Just (Just 1)) =<< frameUntil wait ctx ui isJust
  assertEq failed (Just 1) =<< evalUi ctx inp ui
  writeIORef attempt 1
  assertEq failed (Just (Just 2)) =<< frameUntil wait ctx ui (== Just 2)
  assertEq failed 2 =<< readIORef starts
  once <- newTask
  onceStarts <- newIORef 0
  let twoPasses = do
        nVar <- useVar (0 :: Int)
        n <- readVar nVar
        _ <- useTask once ("once" :: String) (tick onceStarts >> threadDelay 10000000)
        n <$ when (n == 0) (writeVar nVar 1)
  assertEq failed 1 =<< evalUi ctx inp twoPasses
  assert failed =<< reaches onceStarts 1
  threadDelay 20000
  assertEq failed 1 =<< readIORef onceStarts
  cancelTasks ctx

-- | A job lives while the view calls its hook. The first frame without the
-- call kills it without requesting a frame, and only that hook's job: the
-- other keeps its job until a frame skips it too. A later call starts anew.
runTaskLeaseTest :: Context -> IORef Int -> IO ()
runTaskLeaseTest ctx failed = do
  jobA <- newTask
  jobB <- newTask
  starts <- newIORef 0
  (sleepA, _, killedA) <- sleeper
  (sleepB, startedB, killedB) <- sleeper
  let ui showA showB = do
        scope (when showA (void (useTask jobA ("a" :: String) (tick starts >> sleepA))))
        scope (when showB (void (useTask jobB ("b" :: String) sleepB)))
        label "lease"
  replicateM_ 3 (runFrame ctx inp (ui True True))
  assert failed =<< reaches starts 1
  startedB
  assert failed . not =<< killedA 20000
  settled ctx failed (ui False True) ()
  replicateM_ 2 (runFrame ctx inp (ui False True))
  assert failed =<< killedA 2000000
  assert failed . not =<< killedB 20000
  _ <- runFrame ctx inp (ui True False)
  assert failed =<< reaches starts 2
  assert failed =<< killedB 2000000
  cancelTasks ctx

-- | A job that throws, returns a value that throws when forced, or meets an
-- asynchronous exception other than the hook's kill (a stack overflow)
-- fails with it and wakes the loop. Nothing reaches the uncaught handler,
-- and 'useTask' keeps its last result.
runTaskFailureTest :: Context -> IORef Int -> IO ()
runTaskFailureTest ctx failed = do
  [thrownJob, lazyJob, overflowJob, plainJob] <- replicateM 4 newTask
  wait <- newWakeSignal ctx
  gate <- newEmptyMVar
  uncaught <- newIORef 0
  handler <- getUncaughtExceptionHandler
  setUncaughtExceptionHandler (\_ -> tick uncaught)
  flip finally (setUncaughtExceptionHandler handler) $ do
    let ui =
          (,)
            <$> mapM
              (\(job, k, act) -> useTaskStatus job (k :: String) (readMVar gate >> act))
              [ (thrownJob, "thrown", ioError (userError "boom") :: IO Int)
              , (lazyJob, "lazy", pure (error "lazy"))
              , (overflowJob, "overflow", throwIO StackOverflow)
              ]
            <*> useTask plainJob ("thrown too" :: String) (readMVar gate >> ioError (userError "boom"))
        failure = \case
          TaskFailed e Nothing -> Just (displayException e)
          _ -> Nothing
    _ <- runFrame ctx inp ui
    _ <- wait 0
    putMVar gate ()
    Just (statuses, plain) <- frameUntil wait ctx ui (all (isJust . failure) . fst)
    assert failed (and (zipWith (\s msg -> maybe False (msg `isInfixOf`) (failure s)) statuses ["boom", "lazy"]))
    assertEq failed Nothing plain
    threadDelay 20000
    assertEq failed 0 =<< readIORef uncaught
  cancelTasks ctx

-- | 'cancelTasks' (run at session end) kills jobs still running and returns
-- once their cleanup has run. After it nothing wakes the loop.
runTaskShutdownTest :: Context -> IORef Int -> IO ()
runTaskShutdownTest ctx failed = do
  job <- newTask
  (sleep, started, killedIn) <- sleeper
  cleaned <- newIORef False
  wakes <- newIORef 0
  setWakeLoop ctx (tick wakes)
  _ <- runFrame ctx inp (useTask job ("shutdown" :: String) (sleep `finally` writeIORef cleaned True))
  started
  cancelTasks ctx
  assert failed =<< readIORef cleaned
  assert failed =<< killedIn 2000000
  before <- readIORef wakes
  wakeFromThread ctx
  assertEq failed before =<< readIORef wakes

-- | A producer that runs its step for 1 to 1000 once let go, then idles, and
-- the action that lets it go and waits for the burst to end.
burster :: IO ((Int -> IO ()) -> IO (), IO ())
burster = do
  go <- newEmptyMVar
  done <- newEmptyMVar
  pure (\step -> takeMVar go >> mapM_ step [1 .. 1000] >> putMVar done () >> takeMVar go, putMVar go () >> takeMVar done)

-- | A burst faster than the frame rate costs one frame showing the last
-- value: from an app thread that writes state and wakes the loop with
-- 'askWake', and from stream updates folded into the hook's state.
runBurstTest :: Context -> IORef Int -> IO ()
runBurstTest _ failed = do
  job <- newTask
  stream <- newStream
  latest <- newIORef 0
  (produceA, letGoA) <- burster
  (produceB, letGoB) <- burster
  let woken = do
        wake <- askWake
        _ <- useTask job ("stream" :: String) (produceA (\i -> writeIORef latest i >> wake))
        n <- liftIO (readIORef latest)
        n <$ lamp (n > 0)
      streamed = useStream stream ("stream" :: String) 0 (\update -> produceB (update . (+))) >>= \n -> n <$ lamp (n > 0)
  forM_ [(woken, letGoA, 1000), (streamed, letGoB, 500500)] $ \(ui, letGo, final) -> do
    ctx <- newContext
    wait <- newWakeSignal ctx
    _ <- warmup2 ctx inp ui
    settled ctx failed ui 0
    _ <- wait 0
    letGo
    assertEq failed [True, False] =<< replicateM 2 (wait 0)
    takesNews ctx failed ui final
    cancelTasks ctx

-- | A wake the action could not deliver (a full event queue), or made with
-- no action installed, does not leave the next wake waiting on it; once one
-- gets through, the rest wait for the loop to take it. A wake drained with
-- @wait 0@ and no frame after it does not swallow the job's wake after it.
runWakeDeliveryTest :: Context -> IORef Int -> IO ()
runWakeDeliveryTest ctx failed = do
  writeIORef (ctxWakeLoop ctx) Nothing
  wakeFromThread ctx
  runs <- newIORef (0 :: Int)
  delivers <- newIORef False
  setWakeLoopChecked ctx (tick runs >> readIORef delivers)
  replicateM_ 2 (wakeFromThread ctx)
  assertEq failed 2 =<< readIORef runs
  writeIORef delivers True
  replicateM_ 2 (wakeFromThread ctx)
  assertEq failed 3 =<< readIORef runs
  drained <- newContext
  wait <- newWakeSignal drained
  markDirty drained
  assert failed =<< wait 0
  wakeFromThread drained
  assert failed =<< wait 0

-- | A new key kills the old producer and restarts from the initial state,
-- dropping the old one's late updates. An update whose function throws
-- leaves the state as it was: the producer gets the exception and goes on.
-- An update to a stream whose hook stopped running wakes nothing: a thread
-- that kept its @update@ once woke the loop, repainting whole, for good.
runStreamLifecycleTest :: Context -> IORef Int -> IO ()
runStreamLifecycleTest ctx failed = do
  stream <- newStream
  wait <- newWakeSignal ctx
  (sleep, _, killedIn) <- sleeper
  keyRef <- newIORef (1 :: Int)
  let ui = liftIO (readIORef keyRef) >>= \k -> useStream stream k [] (\update -> update (k :) >> sleep)
  assertEq failed (Just [1]) =<< frameUntil wait ctx ui (not . null)
  writeIORef keyRef 2
  -- The new producer may already have run before the view reads it.
  assert failed . (`elem` [[], [2]]) =<< evalUi ctx inp ui
  assert failed =<< killedIn 2000000
  assertEq failed (Just [2]) =<< frameUntil wait ctx ui (not . null)
  throwing <- newStream
  (sleepT, _, _) <- sleeper
  let produce update = do
        update (const 1)
        _ <- try @ErrorCall (update (\_ -> error "boom"))
        _ <- try @ErrorCall (update (+ 10))
        sleepT
  assertEq failed (Just 11) =<< frameUntil wait ctx (useStream throwing () (0 :: Int) produce) (>= 10)
  cancelTasks ctx
  fresh <- newContext
  waitFresh <- newWakeSignal fresh
  released <- newStream
  (sleepR, started, killedR) <- sleeper
  kept <- newEmptyMVar
  let shownIf shown = when shown (void (useStream released () (0 :: Int) (\update -> putMVar kept update >> sleepR)))
  _ <- runFrame fresh inp (shownIf True)
  update <- takeMVar kept
  started
  _ <- runFrame fresh inp (shownIf False)
  assert failed =<< killedR 2000000
  _ <- waitFresh 0
  update (+ 1)
  assert failed . not =<< waitFresh 0
