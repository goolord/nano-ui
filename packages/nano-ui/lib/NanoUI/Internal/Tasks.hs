-- | Background work. 'useTaskStatus' and 'useTask' run an action on a worker
-- thread and return its outcome. 'useStream' runs a producer that updates
-- state the view reads. 'askWake' lets any other thread rerun the view.
--
-- Worker threads never touch the context's stores. A job writes into its own
-- 'IORef' and wakes the loop ('wakeFromThread'); the view reads the ref on its
-- next call to the hook. A job belongs to its typed handle and lives while
-- the view keeps calling the hook ('useHeld'). 'sweepHeld' ends the rest.
module NanoUI.Internal.Tasks
  ( TaskStatus (..)
  , Task
  , newTask
  , Stream
  , newStream
  , useTaskStatus
  , useTask
  , useStream
  , askWake
  , useHeld
  , holdAction
  , sweepHeld
  , cancelTasks
  , cancelTasksOf
  )
where

import Control.Concurrent (forkIO, forkIOWithUnmask, killThread, newEmptyMVar, putMVar, readMVar)
import Control.Exception (AsyncException (ThreadKilled), SomeException, evaluate, finally, fromException, mask_, throwIO, try)
import Control.Monad (filterM, forM_, unless, void, when)
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Primitive.PrimVar (modifyPrimVar, newPrimVar, readPrimVar, writePrimVar)
import Data.Unique (hashUnique, newUnique)
import GHC.Conc (labelThread)
import NanoUI.Internal.Context (Context (ctxWakeLoop, ctxHeld), intKey, wakeFromThread)
import NanoUI.Internal.Resource (Resource (..), newResource, Held (..), Holding (..))
import NanoUI.Internal.Monad (NanoUI, askContext, freshWidget, liftIO)
import System.Timeout (timeout)

-- | Hold a resource for key @k@ in its typed owner. While the key is
-- unchanged the cached value is returned. Otherwise the old resource is
-- released and @acquire@ builds a new one, receiving the old value.
-- A second view pass in the same frame reuses the first pass's
-- value. Resources are released when a frame skips the hook ('sweepHeld') and
-- when the session ends ('cancelTasks'). Holds job threads and
-- 'NanoUI.useImageRgba' images.
useHeld :: Eq k => Resource k a -> k -> (Context -> Maybe a -> IO (a, IO ())) -> NanoUI a
useHeld owner k acquire = useHeldBy owner k Just (\ctx old -> (\(v, release) -> (v, v, pure () <$ release)) <$> acquire ctx old)

-- | 'useHeld' keeping an @s@ and returning what @view@ finds in it. An entry
-- @view@ finds nothing in is replaced.
{-# INLINE useHeldBy #-}
useHeldBy :: Eq k => Resource k s -> k -> (s -> Maybe b) -> (Context -> Maybe b -> IO (s, b, IO (IO ()))) -> NanoUI b
useHeldBy (Resource key cell) k view acquire = do
  ctx <- askContext
  liftIO $ do
    let registry = ctxHeld ctx
        ref = heldEntries registry
        stampedVar = heldStamped registry
        countVar = heldCount registry
    entry <- readIORef cell
    frame <- readPrimVar (heldFrame registry)
    let valueOf (_, v, _) = view v
        stamp = modifyPrimVar stampedVar (+ 1)
    case entry of
      Just h@(k0, _, holding) | k0 == k, Just v <- valueOf h -> do
        touch registry frame holding
        pure v
      _ -> do
        -- The replaced entry leaves the map, and the counts, before its
        -- release, so an @acquire@ that throws leaves nothing released
        -- behind for a later frame to release again.
        forM_ entry $ \(_, _, old) -> do
          modifyIORef' ref (IM.delete key)
          modifyPrimVar countVar (subtract 1)
          s <- readPrimVar (holdingSeen old)
          when (s == frame) $ modifyPrimVar stampedVar (subtract 1)
          void (holdingRelease old)
        (stored, v, release) <- acquire ctx (valueOf =<< entry)
        seen <- newPrimVar frame
        let holding = Holding (writeIORef cell Nothing >> release) seen
        writeIORef cell (Just (k, stored, holding))
        modifyIORef' ref (IM.insert key holding)
        stamp
        modifyPrimVar countVar (+ 1)
        pure v

touch :: Held -> Int -> Holding -> IO ()
touch registry frame holding = do
  seen <- readPrimVar (holdingSeen holding)
  unless (seen == frame) $ do
    writePrimVar (holdingSeen holding) frame
    modifyPrimVar (heldStamped registry) (+ 1)

-- | Lease a callback-only built-in resource at a widget id. There is no
-- payload to recover: sweeping only runs its cleanup action.
holdAction :: IO () -> NanoUI ()
holdAction release = do
  (wid, ctx) <- freshWidget
  liftIO $ do
    let registry = ctxHeld ctx
        widget = intKey wid
    frame <- readPrimVar (heldFrame registry)
    widgets <- readIORef (heldWidgets registry)
    entries <- readIORef (heldEntries registry)
    case IM.lookup widget widgets >>= (`IM.lookup` entries) of
      Just holding -> touch registry frame holding
      Nothing -> do
        key <- hashUnique <$> newUnique
        seen <- newPrimVar frame
        let cleanup = do
              modifyIORef' (heldWidgets registry) (IM.delete widget)
              release
              pure (pure ())
        modifyIORef' (heldWidgets registry) (IM.insert widget key)
        modifyIORef' (heldEntries registry) (IM.insert key (Holding cleanup seen))
        modifyPrimVar (heldStamped registry) (+ 1)
        modifyPrimVar (heldCount registry) (+ 1)

-- | State of a 'useTaskStatus' job. The 'Maybe' is the latest result from an
-- earlier key, so the view can keep showing it instead of flickering.
data TaskStatus a
  = -- | The job is running.
    TaskRunning (Maybe a)
  | -- | The job returned this.
    TaskDone a
  | -- | The job threw this.
    TaskFailed SomeException (Maybe a)
  deriving (Show, Functor)

-- | A job's status and latest result, stored together so neither read
-- allocates.
data Outcome a = Outcome !(TaskStatus a) !(Maybe a)

-- | Allocate one typed task owner per component/session. Key and result types
-- are fixed by the handle. Skipping its hook releases its running job.
newtype Task k a = Task (Resource k (IORef (Outcome a)))

newTask :: IO (Task k a)
newTask = Task <$> newResource

-- | A typed stream owner, allocated before the per-frame view.
newtype Stream k s = Stream (Resource k (IORef s))

newStream :: IO (Stream k s)
newStream = Stream <$> newResource

-- | 'useHeldBy' for a job. @start@ builds what the hook keeps, the result
-- box (given the old box when @view@ finds one) and the action to fork.
{-# INLINE useJob #-}
useJob :: Eq k => Resource k s -> k -> (s -> Maybe b) -> (Context -> Maybe b -> IO (s, b, IO ())) -> NanoUI b
useJob owner k view start = useHeldBy owner k view $ \ctx old -> do
  (stored, box, run) <- start ctx old
  done <- newEmptyMVar
  -- Masked until the handler is in place, so a job killed before it first
  -- runs still reports that it has ended.
  tid <- mask_ (forkIOWithUnmask (\unmask -> unmask run `finally` putMVar done ()))
  labelThread tid "nano-ui task"
  -- Kill from a separate thread: 'killThread' blocks until the job receives
  -- the exception, which masking or a foreign call can delay. Only the
  -- session's end waits for the job to end ('cancelTasks').
  pure (stored, box, readMVar done <$ forkIO (killThread tid))

-- | Run an action on a worker thread and report its status: running, done,
-- or failed with the exception it threw.
--
-- The job runs once per key. Calling with a new key kills the running job
-- and starts another. To rerun with the same input, pair it with a counter
-- that a Retry button bumps:
--
-- > (attempt, setAttempt) <- useInt 0
-- > status <- useTaskStatus task (path, attempt) (T.readFile path)
-- > case status of
-- >   TaskRunning _ -> label "Loading..."
-- >   TaskDone contents -> label contents
-- >   TaskFailed e _ -> do
-- >     danger (T.pack (displayException e))
-- >     whenM (button "Retry") (setAttempt (attempt + 1))
--
-- The loop sleeps while the job runs and wakes once when it ends; that frame
-- repaints the whole window. Allocate @task <- newTask@ once during setup.
-- This consumes no widget id. A frame that skips the handle kills its job;
-- call it above a tab or branch that should not end it:
--
-- > when shown (void (useTask task path (T.readFile path)))
--
-- Kills are asynchronous and sent from another thread, so the frame does not
-- wait. An old job may run briefly alongside its replacement, until it next
-- allocates or returns from a foreign call. Use 'Control.Exception.bracket'
-- in jobs that write files or hold resources. Jobs still running when the
-- session ends are killed, and the session waits up to a second for them to
-- finish unwinding.
--
-- The result is forced to weak head normal form on the worker thread; force
-- deeper structure inside the action, or the view will pay for it.
-- Exceptions from the action or from forcing the result become 'TaskFailed',
-- asynchronous ones such as a stack overflow included; only the hook's own
-- kill ends the job without one. Build with @-threaded@ so jobs run while the
-- loop sleeps.
useTaskStatus :: Eq k => Task k a -> k -> IO a -> NanoUI (TaskStatus a)
useTaskStatus owner k run = (\(Outcome status _) -> status) <$> useOutcome owner k run

-- | Like 'useTaskStatus', but returns only the latest result: the current
-- job's once it returns, otherwise the previous key's, or 'Nothing'. A failed
-- job keeps the previous result.
--
-- > (path, setPath) <- useText "notes.txt"
-- > contents <- useTask task path (T.readFile (T.unpack path))
-- > label (fromMaybe "Loading..." contents)
useTask :: Eq k => Task k a -> k -> IO a -> NanoUI (Maybe a)
useTask owner k run = (\(Outcome _ latest) -> latest) <$> useOutcome owner k run

-- | The job's outcome as of this frame. A new key's job starts as running,
-- carrying the replaced job's latest result.
useOutcome :: Eq k => Task k a -> k -> IO a -> NanoUI (Outcome a)
useOutcome (Task owner) k run = do
  box <- useJob owner k Just $ \ctx old -> do
    prev <- maybe (pure Nothing) (fmap (\(Outcome _ latest) -> latest) . readIORef) old
    box <- newIORef (Outcome (TaskRunning prev) prev)
    let finish = (>> wakeFromThread ctx) . atomicWriteIORef box
    pure
      ( box
      , box
      , try (run >>= evaluate) >>= \case
          Right a -> finish (Outcome (TaskDone a) (Just a))
          Left e
            | Just ThreadKilled <- fromException e -> throwIO e
            | otherwise -> finish (Outcome (TaskFailed e prev) prev)
      )
  liftIO (readIORef box)

-- | Run a producer on a worker thread that updates state the view reads, such
-- as sensor readings, download progress, or a streamed chat reply. The
-- producer gets @update@, which applies a function to the state atomically,
-- forces the result to weak head normal form on the producer's thread, and
-- wakes the loop. Each call returns the current state, starting from
-- @initial@:
--
-- Allocate @stream <- newStream@ during setup, then read it in the view:
--
-- > sensorView stream = do
-- >   reading <- useStream stream () Nothing $ \update -> forever $ do
-- >     r <- readSensor
-- >     update (const (Just r))
-- >   label (maybe "--" (T.pack . show) reading)
--
-- Many updates between frames cost one frame. To keep every value, fold it
-- in: @update (x :)@. Keys and lifetime work as in 'useTaskStatus': a new key
-- kills the producer and restarts from @initial@, and a frame that skips the
-- hook kills it. A producer that returns leaves the state as it last set it.
-- An uncaught exception ends the producer like any 'forkIO' thread; catch it
-- inside to show it in the state.
useStream :: Eq k => Stream k s -> k -> s -> (((s -> s) -> IO ()) -> IO ()) -> NanoUI s
useStream (Stream owner) k initial produce = do
  box <- useJob owner k Just $ \ctx _ -> do
    box <- newIORef initial
    pure (box, box, produce (\f -> atomicModifyIORef' box (\s -> (f s, ())) >> wakeFromThread ctx))
  liftIO (readIORef box)

-- | An action any thread can call to rerun the view after changing something
-- it reads. The woken frame repaints the whole window, since nothing says
-- which widgets changed. Wakes before that frame runs merge into it, so a
-- thread faster than the display costs one frame per display frame.
--
-- 'useStream' is built on this. Use it directly for a thread the view does
-- not own: publish each value where the view reads it, then wake.
askWake :: NanoUI (IO ())
askWake = wakeFromThread <$> askContext

-- | End-of-frame sweep: release resources whose hook did not run this frame.
-- Two view passes in one frame count as one. When every hook ran, it touches
-- no entry.
sweepHeld :: Context -> IO ()
sweepHeld ctx = sweep (ctxHeld ctx)
  where
    sweep (Held ref _ frameVar stampedVar countVar) = do
      frame <- readPrimVar frameVar
      stamped <- readPrimVar stampedVar
      count <- readPrimVar countVar
      writePrimVar frameVar (frame + 1)
      writePrimVar stampedVar 0
      unless (stamped == count) $ do
        held <- readIORef ref
        gone <- filterM (\(_, Holding _ seen) -> (/= frame) <$> readPrimVar seen) (IM.toAscList held)
        writeIORef ref $! IM.withoutKeys held (IS.fromDistinctAscList (map fst gone))
        writePrimVar countVar (count - length gone)
        mapM_ (holdingRelease . snd) gone

-- | Kill every job on the context and release images held by
-- 'NanoUI.useImageRgba', at session end. 'NanoUI.Runner.runSessionLoop' calls
-- this when its loop returns; hosts that run frames themselves should too.
-- It removes the wake action ('NanoUI.Backend.setWakeLoop'), so nothing wakes
-- a loop that is gone, and waits up to a second for the killed jobs to finish
-- unwinding, so their 'Control.Exception.bracket' and
-- 'Control.Exception.finally' cleanups run before the session's resources go.
cancelTasks :: Context -> IO ()
cancelTasks ctx = cancelTasksOf (ctxWakeLoop ctx) (ctxHeld ctx)

-- | 'cancelTasks' on the context's 'ctxWakeLoop' and 'ctxHeld', for a caller
-- that keeps those rather than the context.
cancelTasksOf :: IORef (Maybe (IO Bool)) -> Held -> IO ()
cancelTasksOf wake (Held ref _ _ stampedVar countVar) = do
  writeIORef wake Nothing
  held <- readIORef ref
  writeIORef ref IM.empty
  writePrimVar stampedVar 0
  writePrimVar countVar 0
  -- Every kill is sent before the wait, so the jobs unwind together.
  waits <- mapM holdingRelease held
  void (timeout 1000000 (sequence_ waits))
