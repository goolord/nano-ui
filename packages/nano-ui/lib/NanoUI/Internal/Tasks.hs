-- | Background work. 'useTaskStatus' and 'useTask' run an action on a thread
-- of its own and hand the view its outcome, 'useStream' runs a producer that
-- updates a state the view reads, and 'askWake' gives any other thread a way
-- to run the view again.
--
-- A job's thread never touches the context's stores. It writes into a box
-- of its own and wakes the loop ('wakeFromThread'), and the view reads the
-- box the next time it calls the hook. A job belongs to its hook's widget id
-- and lives as long as the view calls the hook: 'sweepTasks' ends the ones a
-- frame left out.
module NanoUI.Internal.Tasks
  ( TaskStatus (..)
  , useTaskStatus
  , useTask
  , useStream
  , askWake
  , sweepTasks
  , cancelTasks
  )
where

import Control.Concurrent (ThreadId, forkIO, killThread)
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

-- | The context's jobs, kept on it as a host value ('hostOrInit').
newtype Tasks = Tasks (IORef TaskTable)

-- | The jobs by the store key of their hook's widget id, and the keys whose
-- hook ran this frame, in any of its view passes.
data TaskTable = TaskTable !(IntMap Job) !IntSet

-- | A job: its hook's key, compared by value, the box its thread writes
-- into, and the thread.
data Job = forall k. (Eq k, Typeable k) => Job !k !Dynamic !ThreadId

-- | Where a 'useTaskStatus' job is. The 'Maybe' is the result of the job
-- for an earlier key, the latest that finished, so a view can go on showing
-- it while the new key's job runs rather than flicker to nothing.
data TaskStatus a
  = -- | The job is running.
    TaskRunning (Maybe a)
  | -- | The job returned this.
    TaskDone a
  | -- | The job threw this.
    TaskFailed SomeException (Maybe a)
  deriving (Show, Functor)

-- | What a task job writes into its box: its status, and the latest result
-- that status has, so neither read allocates.
data Outcome a = Outcome !(TaskStatus a) !(Maybe a)

-- | The box of this hook's job for its key: the running job's, or, for a new
-- key or a hook new at this id, the box @start@ makes, given the box of the
-- job it replaces when that has the same type, along with the job to run.
-- A job for another key, or one another hook left at this id, is killed. A
-- second view pass in the frame finds the job the first started.
useJob :: (Eq k, Typeable k, Typeable b, Ui :> es) => k -> (Context -> Maybe b -> IO (b, IO ())) -> Eff es b
useJob k start = do
  (wid, ctx) <- freshWidget
  uiIO $ do
    Tasks ref <- hostOrInit ctx (Tasks <$> newIORef (TaskTable IM.empty IS.empty))
    TaskTable jobs called <- readIORef ref
    let key = intKey wid
        entry = IM.lookup key jobs
        boxOf (Job _ box _) = fromDynamic box
        current = case entry of
          Just job@(Job k0 _ _) | cast k0 == Just k -> boxOf job
          _ -> Nothing
    (box, jobs') <- case current of
      Just box -> pure (box, jobs)
      Nothing -> do
        mapM_ stopJob entry
        (box, run) <- start ctx (boxOf =<< entry)
        tid <- forkIO run
        labelThread tid "nano-ui task"
        pure (box, IM.insert key (Job k (toDyn box) tid) jobs)
    writeIORef ref $! TaskTable jobs' (IS.insert key called)
    pure box

-- | Run an action on a thread of its own and say where it is: running,
-- done with its result, or failed with the exception it threw.
--
-- The job starts the first frame this is called with a key, and runs once
-- per key: a frame that calls it with another key kills the running job and
-- starts one for the new key. Keep the key stable while the action should
-- keep running, and change it when the action should run again, even for
-- the same input: pair the input with a count that a Retry button bumps.
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
-- The job's end wakes the loop, once, so the loop sleeps while it runs; the
-- frame it wakes repaints the whole window, as 'askWake' says. Like any hook
-- it takes the next widget id, and the job lives as long as the view calls
-- the hook: a frame that does not call it kills the job. Call it where the
-- view shows the result, or above a tab or a branch that should not end the
-- job; a hook called on some frames and not others goes inside
-- 'NanoUI.scope', so the hooks after it keep their ids:
--
-- > scope (when shown (void (useTask path (T.readFile path))))
--
-- A job is killed by an asynchronous exception ('killThread') from a thread
-- of its own, so the frame does not wait for it: a job for an old key can
-- run on for a moment beside the new key's job, until it next allocates or
-- leaves a foreign call. Give a job that writes files or holds a resource a
-- 'Control.Exception.bracket', and do not count on the old one having
-- stopped. A session's end kills the jobs still running.
--
-- The result is evaluated to weak head normal form on the job's thread;
-- force a lazy structure there too, inside the action, or the view will. A
-- synchronous exception from the action, or from evaluating its result, is
-- caught and shown as 'TaskFailed'; an asynchronous one, such as the kill of
-- a job whose key changed, ends the job. The view must run on the threaded
-- runtime (@-threaded@) for a job to run while the loop sleeps.
useTaskStatus :: (Eq k, Typeable k, Typeable a, Ui :> es) => k -> IO a -> Eff es (TaskStatus a)
useTaskStatus k run = do
  box <- useTaskBox k run
  uiIO ((\(Outcome status _) -> status) <$> readIORef box)

-- | 'useTaskStatus' as the latest result there is: the job's once it has
-- returned, before that the result of the job for an earlier key, and
-- 'Nothing' when there is none. A job that failed leaves the result before
-- it.
--
-- > (path, setPath) <- useText "notes.txt"
-- > contents <- useTask path (T.readFile (T.unpack path))
-- > label (fromMaybe "Loading..." contents)
useTask :: (Eq k, Typeable k, Typeable a, Ui :> es) => k -> IO a -> Eff es (Maybe a)
useTask k run = do
  box <- useTaskBox k run
  uiIO ((\(Outcome _ latest) -> latest) <$> readIORef box)

-- | The box of a 'useTaskStatus' job. A new key's job starts out running,
-- with the latest result of the job it replaces.
useTaskBox :: (Eq k, Typeable k, Typeable a, Ui :> es) => k -> IO a -> Eff es (IORef (Outcome a))
useTaskBox k run = useJob k $ \ctx old -> do
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

-- | Run a producer on a thread of its own, which updates a state the view
-- reads: a stream of readings, a download's progress, a chat reply arriving
-- a token at a time. The producer gets @update@, which applies a function to
-- the state (atomically, forcing the new state to weak head normal form on
-- the producer's thread) and wakes the loop. The view reads the state as it
-- stood when its frame began, starting from @initial@:
--
-- > sensorView :: NanoUI ()
-- > sensorView = do
-- >   reading <- useStream () Nothing $ \update -> forever $ do
-- >     r <- readSensor
-- >     update (const (Just r))
-- >   label (maybe "--" (T.pack . show) reading)
--
-- Updates that come faster than frames cost one frame for all of them, and
-- a state that keeps every value folds each one in: @update (x :)@. Keys and
-- the job's life are as for 'useTaskStatus': a new key kills the producer
-- and starts another from @initial@, and the first frame that does not call
-- the hook kills it. A producer that returns leaves the state as it last
-- set it. An exception from it ends it, and goes where an uncaught one on a
-- 'forkIO' thread goes: catch it inside to show it in the state.
useStream :: (Eq k, Typeable k, Typeable s, Ui :> es) => k -> s -> (((s -> s) -> IO ()) -> IO ()) -> Eff es s
useStream k initial produce = do
  box <- useJob k $ \ctx _ -> do
    box <- newIORef initial
    pure (box, produce (\f -> atomicModifyIORef' box (\s -> (f s, ())) >> wakeFromThread ctx))
  uiIO (readIORef box)

-- | An action any thread may call to have the loop run the view again, for
-- something the thread changed that the view reads. The frame it wakes
-- repaints the whole window, since nothing says which widgets show the
-- change. Wakes that come before that frame runs cost that one frame, so a
-- thread faster than the display costs a frame a frame.
--
-- 'useStream' is built on it; this is for a thread the view does not own,
-- which publishes each value where the view reads it and then wakes the
-- loop.
askWake :: Ui :> es => Eff es (IO ())
askWake = wakeFromThread <$> askContext

-- | Kill a job's thread, from a thread of its own: 'killThread' returns once
-- the job takes the exception, which a job that masks it or sits in a
-- foreign call puts off, and the frame must not wait for that.
stopJob :: Job -> IO ()
stopJob (Job _ _ tid) = void (forkIO (killThread tid))

-- | End a frame for the jobs: those whose hook ran stay, and the rest are
-- killed. Two view passes of one frame count as one frame.
sweepTasks :: Context -> IO ()
sweepTasks ctx = askHostIO ctx >>= mapM_ (\(Tasks ref) -> sweep ref)
  where
    sweep ref = do
      TaskTable jobs called <- readIORef ref
      unless (IM.null jobs && IS.null called) $ do
        let (kept, gone) = IM.partitionWithKey (\k _ -> IS.member k called) jobs
        writeIORef ref $! TaskTable kept IS.empty
        mapM_ stopJob gone

-- | Kill every job on the context, as a session ends.
-- 'NanoUI.Runner.runSessionLoop' does this when its loop returns; a host
-- that runs frames itself should too.
cancelTasks :: Context -> IO ()
cancelTasks ctx = askHostIO ctx >>= mapM_ cancelAll
  where
    cancelAll (Tasks ref) = do
      TaskTable jobs _ <- readIORef ref
      writeIORef ref $! TaskTable IM.empty IS.empty
      mapM_ stopJob jobs
