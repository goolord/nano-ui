-- | Background work. 'useTaskStatus' and 'useTask' run an action on a worker
-- thread and return its outcome. 'useStream' runs a producer that updates
-- state the view reads. 'askWake' lets any other thread rerun the view.
--
-- Worker threads never touch the context's stores. A job writes into its own
-- 'IORef' and wakes the loop ('wakeFromThread'); the view reads the ref on its
-- next call to the hook. A job is tied to its hook's widget id and lives while
-- the view keeps calling the hook ('useHeld'). 'sweepHeld' ends the rest.
module NanoUI.Internal.Tasks
  ( TaskStatus (..)
  , useTaskStatus
  , useTask
  , useStream
  , askWake
  , useHeld
  , sweepHeld
  , cancelTasks
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (SomeAsyncException, SomeException, evaluate, fromException, throwIO, try)
import Control.Monad (unless, void)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Maybe (isJust)
import Data.Typeable (Typeable, cast)
import Effectful (Eff, type (:>))
import GHC.Conc (labelThread)
import NanoUI.Internal.Context (Context, askHostIO, hostOrInit, intKey, wakeFromThread)
import NanoUI.Internal.Monad (Ui, askContext, freshWidget, uiIO)

-- | Per-context hook resources, stored as a host value ('hostOrInit').
newtype Held = Held (IORef HeldTable)

-- | Resources by widget store key, plus the keys whose hook ran this frame
-- (in any view pass).
data HeldTable = HeldTable !(IntMap Holding) !IntSet

-- | A hook's key (compared by value), its resource, and the release action.
data Holding = forall k. (Eq k, Typeable k) => Holding !k !Dynamic !(IO ())

-- | Hold a resource for key @k@ at this hook's widget id. While the key is
-- unchanged the cached value is returned. Otherwise the old resource is
-- released and @acquire@ builds a new one; it gets the old value when the
-- types match. A second view pass in the same frame reuses the first pass's
-- value. Resources are released when a frame skips the hook ('sweepHeld') and
-- when the session ends ('cancelTasks'). Holds job threads and
-- 'NanoUI.useImageRgba' images.
useHeld :: (Eq k, Typeable k, Typeable a, Ui :> es) => k -> (Context -> Maybe a -> IO (a, IO ())) -> Eff es a
useHeld k acquire = do
  (wid, ctx) <- freshWidget
  uiIO $ do
    Held ref <- hostOrInit ctx (Held <$> newIORef (HeldTable IM.empty IS.empty))
    HeldTable held called <- readIORef ref
    let key = intKey wid
        entry = IM.lookup key held
        valueOf (Holding _ v _) = fromDynamic v
    case entry of
      Just h@(Holding k0 _ _) | cast k0 == Just k, Just v <- valueOf h -> do
        unless (IS.member key called) $ writeIORef ref $! HeldTable held (IS.insert key called)
        pure v
      _ -> do
        mapM_ letGo entry
        (v, release) <- acquire ctx (valueOf =<< entry)
        writeIORef ref $! HeldTable (IM.insert key (Holding k (toDyn v) release) held) (IS.insert key called)
        pure v

letGo :: Holding -> IO ()
letGo (Holding _ _ release) = release

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

-- | 'useHeld' for a job. @start@ builds the result box (given the old box
-- when the types match) and the action to fork.
useJob :: (Eq k, Typeable k, Typeable b, Ui :> es) => k -> (Context -> Maybe b -> IO (b, IO ())) -> Eff es b
useJob k start = useHeld k $ \ctx old -> do
  (box, run) <- start ctx old
  tid <- forkIO run
  labelThread tid "nano-ui task"
  -- Kill from a separate thread: 'killThread' blocks until the job receives
  -- the exception, which masking or a foreign call can delay.
  pure (box, void (forkIO (killThread tid)))

-- | Run an action on a worker thread and report its status: running, done,
-- or failed with the exception it threw.
--
-- The job runs once per key. Calling with a new key kills the running job
-- and starts another. To rerun with the same input, pair it with a counter
-- that a Retry button bumps:
--
-- > (attempt, setAttempt) <- useInt 0
-- > status <- useTaskStatus (path, attempt) (T.readFile path)
-- > case status of
-- >   TaskRunning _ -> label "Loading..."
-- >   TaskDone contents -> label contents
-- >   TaskFailed e _ -> do
-- >     danger (T.pack (displayException e))
-- >     whenM (button "Retry") (setAttempt (attempt + 1))
--
-- The loop sleeps while the job runs and wakes once when it ends; that frame
-- repaints the whole window. Like any hook it takes the next widget id, and a
-- frame that skips the call kills the job. Call it where the result is shown,
-- or above any tab or branch that should not end the job. Wrap a conditional
-- call in 'NanoUI.scope' so later hooks keep their ids:
--
-- > scope (when shown (void (useTask path (T.readFile path))))
--
-- Kills are asynchronous and sent from another thread, so the frame does not
-- wait. An old job may run briefly alongside its replacement, until it next
-- allocates or returns from a foreign call. Use 'Control.Exception.bracket'
-- in jobs that write files or hold resources. Jobs still running when the
-- session ends are killed.
--
-- The result is forced to weak head normal form on the worker thread; force
-- deeper structure inside the action, or the view will pay for it.
-- Synchronous exceptions from the action or from forcing the result become
-- 'TaskFailed'; asynchronous ones end the job. Build with @-threaded@ so jobs
-- run while the loop sleeps.
useTaskStatus :: (Eq k, Typeable k, Typeable a, Ui :> es) => k -> IO a -> Eff es (TaskStatus a)
useTaskStatus k run = (\(Outcome status _) -> status) <$> useOutcome k run

-- | Like 'useTaskStatus', but returns only the latest result: the current
-- job's once it returns, otherwise the previous key's, or 'Nothing'. A failed
-- job keeps the previous result.
--
-- > (path, setPath) <- useText "notes.txt"
-- > contents <- useTask path (T.readFile (T.unpack path))
-- > label (fromMaybe "Loading..." contents)
useTask :: (Eq k, Typeable k, Typeable a, Ui :> es) => k -> IO a -> Eff es (Maybe a)
useTask k run = (\(Outcome _ latest) -> latest) <$> useOutcome k run

-- | The job's outcome as of this frame. A new key's job starts as running,
-- carrying the replaced job's latest result.
useOutcome :: (Eq k, Typeable k, Typeable a, Ui :> es) => k -> IO a -> Eff es (Outcome a)
useOutcome k run = do
  box <- useJob k $ \ctx old -> do
    prev <- maybe (pure Nothing) (fmap (\(Outcome _ latest) -> latest) . readIORef) old
    box <- newIORef (Outcome (TaskRunning prev) prev)
    let finish = (>> wakeFromThread ctx) . atomicWriteIORef box
    pure
      ( box
      , try (run >>= evaluate) >>= \case
          Right a -> finish (Outcome (TaskDone a) (Just a))
          Left e
            | isJust (fromException e :: Maybe SomeAsyncException) -> throwIO e
            | otherwise -> finish (Outcome (TaskFailed e prev) prev)
      )
  uiIO (readIORef box)

-- | Run a producer on a worker thread that updates state the view reads, such
-- as sensor readings, download progress, or a streamed chat reply. The
-- producer gets @update@, which applies a function to the state atomically,
-- forces the result to weak head normal form on the producer's thread, and
-- wakes the loop. Each call returns the current state, starting from
-- @initial@:
--
-- > sensorView :: NanoUI ()
-- > sensorView = do
-- >   reading <- useStream () Nothing $ \update -> forever $ do
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
useStream :: (Eq k, Typeable k, Typeable s, Ui :> es) => k -> s -> (((s -> s) -> IO ()) -> IO ()) -> Eff es s
useStream k initial produce = do
  box <- useJob k $ \ctx _ -> do
    box <- newIORef initial
    pure (box, produce (\f -> atomicModifyIORef' box (\s -> (f s, ())) >> wakeFromThread ctx))
  uiIO (readIORef box)

-- | An action any thread can call to rerun the view after changing something
-- it reads. The woken frame repaints the whole window, since nothing says
-- which widgets changed. Wakes before that frame runs merge into it, so a
-- thread faster than the display costs one frame per display frame.
--
-- 'useStream' is built on this. Use it directly for a thread the view does
-- not own: publish each value where the view reads it, then wake.
askWake :: Ui :> es => Eff es (IO ())
askWake = wakeFromThread <$> askContext

-- | End-of-frame sweep: release resources whose hook did not run this frame.
-- Two view passes in one frame count as one.
sweepHeld :: Context -> IO ()
sweepHeld ctx = askHostIO ctx >>= mapM_ (\(Held ref) -> sweep ref)
  where
    sweep ref = do
      HeldTable held called <- readIORef ref
      unless (IM.null held && IS.null called) $ do
        let (kept, gone) = IM.partitionWithKey (\k _ -> IS.member k called) held
        writeIORef ref $! HeldTable kept IS.empty
        mapM_ letGo gone

-- | Kill every job on the context and release images held by
-- 'NanoUI.useImageRgba', at session end. 'NanoUI.Runner.runSessionLoop' calls
-- this when its loop returns; hosts that run frames themselves should too.
cancelTasks :: Context -> IO ()
cancelTasks ctx = askHostIO ctx >>= mapM_ cancelAll
  where
    cancelAll (Held ref) = do
      HeldTable held _ <- readIORef ref
      writeIORef ref $! HeldTable IM.empty IS.empty
      mapM_ letGo held
