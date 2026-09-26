module Cases.Tasks (tests) where

import Spec
import Control.Concurrent (newEmptyMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Exception (displayException, finally, onException)
import Data.List (isInfixOf)
import Data.Function (fix)
import Data.Maybe (isJust)
import GHC.Conc (getUncaughtExceptionHandler, setUncaughtExceptionHandler)
import System.Timeout (timeout)

tests :: [Spec]
tests =
  [ spec "task-result-after-wake" runTaskResultTest
  , spec "task-key-change" runTaskKeyChangeTest
  , spec "task-lease" runTaskLeaseTest
  , spec "task-two-passes" runTaskTwoPassTest
  , spec "task-failure" runTaskFailureTest
  , spec "task-status" runTaskStatusTest
  , spec "task-retry" runTaskRetryTest
  , spec "task-shutdown" runTaskShutdownTest
  , spec "wake-from-thread" runWakeFromThreadTest
  , spec "stream" runStreamTest
  , spec "stream-key-change" runStreamKeyChangeTest
  ]

inp :: Input
inp = withDelta 200 100 0.016

-- | Run frames, each after a wake, until the result passes @ok@: 'Nothing' once
-- two seconds pass without a wake, as for a job that never woke the loop.
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

-- | Whether the count comes to @n@ within two seconds, as threads run.
reaches :: IORef Int -> Int -> IO Bool
reaches ref n = isJust <$> timeout 2000000 (fix (\go -> readIORef ref >>= \v -> unless (v == n) (threadDelay 10000 >> go)))

tick :: IORef Int -> IO ()
tick ref = atomicModifyIORef' ref (\n -> (n + 1, ()))

-- | A job that sleeps until killed, and a wait of up to @us@ for its kill.
sleeper :: IO (IO (), Int -> IO Bool)
sleeper = do
  killed <- newEmptyMVar
  pure (threadDelay 10000000 `onException` putMVar killed (), \us -> isJust <$> timeout us (takeMVar killed))

-- | A box whose colour alone shows @on@, so no rect or text diff sees it change.
lamp :: Bool -> NanoUI ()
lamp on = box (fixedWH 20 20) (if on then colorRGBA 40 200 80 255 else colorRGBA 90 90 90 255)

-- | A frame shows @v@ and asks for no other.
settled :: (Eq a, Show a) => Context -> IORef Int -> NanoUI a -> a -> IO ()
settled ctx failed ui v = do
  (a, _, _, dirty) <- runFrame ctx inp ui
  assertEq failed (v, False) (a, dirty)

-- | The frame that takes a job's news shows @v@ and repaints whole, since
-- nothing says which widgets show it; the frame after is idle again.
takesNews :: (Eq a, Show a) => Context -> IORef Int -> NanoUI a -> a -> IO ()
takesNews ctx failed ui v = do
  assertEq failed v =<< evalUi ctx inp ui
  assertEq failed DamageFull =<< takeDamage ctx
  settled ctx failed ui v
  assert failed . damageIsEmpty =<< takeDamage ctx

-- | The hook reads 'Nothing' while its job runs, which neither keeps frames
-- coming nor wakes the loop until it ends.
runTaskResultTest :: Context -> IORef Int -> IO ()
runTaskResultTest ctx failed = do
  wait <- newWakeSignal ctx
  gate <- newEmptyMVar
  let ui = useTask ("answer" :: String) (takeMVar gate >> pure (42 :: Int)) >>= \a -> a <$ lamp (isJust a)
  _ <- warmup2 ctx inp ui
  settled ctx failed ui Nothing
  assertEq failed 0 =<< getWakeAt ctx
  _ <- wait 0
  assert failed . not =<< wait 50000
  putMVar gate ()
  assert failed =<< wait 2000000
  takesNews ctx failed ui (Just 42)

-- | A new key kills the old key's job and starts its own, whose result the
-- hook returns; until then it returns the last key's result.
runTaskKeyChangeTest :: Context -> IORef Int -> IO ()
runTaskKeyChangeTest ctx failed = do
  wait <- newWakeSignal ctx
  (sleep, killedIn) <- sleeper
  gate <- newEmptyMVar
  keyRef <- newIORef (1 :: Int)
  let ui = uiIO (readIORef keyRef) >>= \k -> useTask k (if k == 2 then sleep >> pure k else takeMVar gate >> pure (k * 10))
  _ <- wait 0
  assertEq failed Nothing =<< evalUi ctx inp ui
  putMVar gate ()
  assertEq failed (Just (Just 10)) =<< frameUntil wait ctx ui isJust
  writeIORef keyRef 2
  assertEq failed (Just 10) =<< evalUi ctx inp ui
  writeIORef keyRef 3
  assertEq failed (Just 10) =<< evalUi ctx inp ui
  assert failed =<< killedIn 2000000
  _ <- wait 0
  putMVar gate ()
  assertEq failed (Just (Just 30)) =<< frameUntil wait ctx ui (== Just 30)

-- | A job lives while the view calls its hook: the first frame without the call
-- kills it (asking for no frame), and a later call starts a new job.
runTaskLeaseTest :: Context -> IORef Int -> IO ()
runTaskLeaseTest ctx failed = do
  starts <- newIORef 0
  (sleep, killedIn) <- sleeper
  let ui shown = scope (when shown (void (useTask ("lease" :: String) (tick starts >> sleep)))) >> label "lease"
  replicateM_ 3 (runFrame ctx inp (ui True))
  assert failed =<< reaches starts 1
  assert failed . not =<< killedIn 20000
  settled ctx failed (ui False) ()
  assert failed =<< killedIn 2000000
  _ <- runFrame ctx inp (ui True)
  assert failed =<< reaches starts 2
  cancelTasks ctx

-- | A frame that runs the view twice after a hook write starts the job once.
runTaskTwoPassTest :: Context -> IORef Int -> IO ()
runTaskTwoPassTest ctx failed = do
  starts <- newIORef 0
  let ui = do
        (n, setN) <- useInt 0
        _ <- useTask ("once" :: String) (tick starts >> threadDelay 10000000)
        n <$ when (n == 0) (setN 1)
  assertEq failed 1 =<< evalUi ctx inp ui
  assert failed =<< reaches starts 1
  threadDelay 20000
  assertEq failed 1 =<< readIORef starts
  cancelTasks ctx

-- | A job that throws, or returns a value that throws, fails with its
-- exception and wakes the loop; none reaches the uncaught handler, and the
-- hook's result stays the last one.
runTaskFailureTest :: Context -> IORef Int -> IO ()
runTaskFailureTest ctx failed = do
  wait <- newWakeSignal ctx
  gate <- newEmptyMVar
  uncaught <- newIORef 0
  handler <- getUncaughtExceptionHandler
  setUncaughtExceptionHandler (\_ -> tick uncaught)
  flip finally (setUncaughtExceptionHandler handler) $ do
    let ui =
          (,,)
            <$> useTaskStatus ("thrown" :: String) (readMVar gate >> ioError (userError "boom") :: IO Int)
            <*> useTaskStatus ("lazy" :: String) (readMVar gate >> pure (error "lazy" :: Int))
            <*> useTask ("thrown too" :: String) (readMVar gate >> ioError (userError "boom") :: IO Int)
        failure = \case
          TaskFailed e Nothing -> Just (displayException e)
          _ -> Nothing
        shown (thrown, lazy, _) = all (isJust . failure) [thrown, lazy]
    _ <- runFrame ctx inp ui
    _ <- wait 0
    putMVar gate ()
    Just (thrown, lazy, plain) <- frameUntil wait ctx ui shown
    assert failed (maybe False ("boom" `isInfixOf`) (failure thrown))
    assert failed (maybe False ("lazy" `isInfixOf`) (failure lazy))
    assertEq failed Nothing plain
    threadDelay 20000
    assertEq failed 0 =<< readIORef uncaught

-- | A job runs, then is done; one for a new key runs with the last key's
-- result, and fails with it.
runTaskStatusTest :: Context -> IORef Int -> IO ()
runTaskStatusTest ctx failed = do
  wait <- newWakeSignal ctx
  gate <- newEmptyMVar
  keyRef <- newIORef (1 :: Int)
  let ui = do
        k <- uiIO (readIORef keyRef)
        status <- useTaskStatus k (takeMVar gate >> if k == 1 then pure ("one" :: String) else ioError (userError "two"))
        pure $ case status of
          TaskRunning prev -> ("running" :: String, prev)
          TaskDone a -> ("done", Just a)
          TaskFailed _ prev -> ("failed", prev)
  _ <- wait 0
  assertEq failed ("running", Nothing) =<< evalUi ctx inp ui
  putMVar gate ()
  assertEq failed (Just ("done", Just "one")) =<< frameUntil wait ctx ui ((== "done") . fst)
  writeIORef keyRef 2
  assertEq failed ("running", Just "one") =<< evalUi ctx inp ui
  putMVar gate ()
  assertEq failed (Just ("failed", Just "one")) =<< frameUntil wait ctx ui ((== "failed") . fst)
  settled ctx failed ui ("failed", Just "one")

-- | The same input runs again under a new attempt count.
runTaskRetryTest :: Context -> IORef Int -> IO ()
runTaskRetryTest ctx failed = do
  wait <- newWakeSignal ctx
  starts <- newIORef 0
  attempt <- newIORef (0 :: Int)
  let ui = do
        n <- uiIO (readIORef attempt)
        useTask ("same input" :: String, n) (tick starts >> readIORef starts)
  _ <- wait 0
  assertEq failed (Just (Just 1)) =<< frameUntil wait ctx ui isJust
  assertEq failed (Just 1) =<< evalUi ctx inp ui
  writeIORef attempt 1
  assertEq failed (Just (Just 2)) =<< frameUntil wait ctx ui (== Just 2)
  assertEq failed 2 =<< readIORef starts

-- | 'cancelTasks', which ends a session, kills the jobs still running.
runTaskShutdownTest :: Context -> IORef Int -> IO ()
runTaskShutdownTest ctx failed = do
  (sleep, killedIn) <- sleeper
  _ <- runFrame ctx inp (useTask ("shutdown" :: String) sleep)
  cancelTasks ctx
  assert failed =<< killedIn 2000000

-- | A thread of the app's own streaming values and waking the loop with
-- 'askWake' costs, for a burst faster than frames, the one frame that shows
-- the last value.
runWakeFromThreadTest :: Context -> IORef Int -> IO ()
runWakeFromThreadTest ctx failed = do
  wait <- newWakeSignal ctx
  latest <- newIORef (0 :: Int)
  go <- newEmptyMVar
  burst <- newEmptyMVar
  let produce wake = do
        takeMVar go
        forM_ [1 .. 1000] $ \i -> writeIORef latest i >> wake
        putMVar burst ()
        takeMVar go
      ui = do
        wake <- askWake
        _ <- useTask ("stream" :: String) (produce wake)
        n <- uiIO (readIORef latest)
        n <$ lamp (n > 0)
  _ <- warmup2 ctx inp ui
  settled ctx failed ui 0
  _ <- wait 0
  putMVar go ()
  takeMVar burst
  assertEq failed [True, False] =<< replicateM 2 (wait 0)
  takesNews ctx failed ui 1000
  cancelTasks ctx

-- | A stream's updates fold into the hook's state, and a burst faster than
-- frames costs the one frame that shows where it ended.
runStreamTest :: Context -> IORef Int -> IO ()
runStreamTest ctx failed = do
  wait <- newWakeSignal ctx
  go <- newEmptyMVar
  burst <- newEmptyMVar
  let produce update = do
        takeMVar go
        forM_ [1 .. 1000 :: Int] $ \i -> update (+ i)
        putMVar burst ()
        takeMVar go
      ui = do
        n <- useStream ("stream" :: String) 0 produce
        n <$ lamp (n > 0)
  _ <- warmup2 ctx inp ui
  settled ctx failed ui 0
  _ <- wait 0
  putMVar go ()
  takeMVar burst
  assertEq failed [True, False] =<< replicateM 2 (wait 0)
  takesNews ctx failed ui 500500
  cancelTasks ctx

-- | A new key kills the old producer and starts again from the initial
-- state; the old one's late updates do not reach the view.
runStreamKeyChangeTest :: Context -> IORef Int -> IO ()
runStreamKeyChangeTest ctx failed = do
  wait <- newWakeSignal ctx
  (sleep, killedIn) <- sleeper
  keyRef <- newIORef (1 :: Int)
  let ui = do
        k <- uiIO (readIORef keyRef)
        useStream k [] (\update -> update (k :) >> sleep)
  _ <- wait 0
  assertEq failed (Just [1]) =<< frameUntil wait ctx ui (not . null)
  writeIORef keyRef 2
  assertEq failed [] =<< evalUi ctx inp ui
  assert failed =<< killedIn 2000000
  assertEq failed (Just [2]) =<< frameUntil wait ctx ui (not . null)
  cancelTasks ctx
