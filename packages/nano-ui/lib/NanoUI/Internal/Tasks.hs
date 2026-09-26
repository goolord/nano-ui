-- | Background work. 'useTask' runs an action on a thread of its own and
-- hands the view its result; 'askWake' gives any other thread a way to run
-- the view again, which is all a stream or a poller of the app's own needs.
--
-- A job's thread never touches the context's stores. It writes its result
-- into a reference of its own and wakes the loop ('wakeFromThread'), and the
-- view reads the reference the next time it calls the hook. A job belongs to
-- its hook's widget id and lives as long as the view calls the hook:
-- 'sweepTasks' ends the ones a frame left out.
module NanoUI.Internal.Tasks
  ( useTask
  , askWake
  , sweepTasks
  , cancelTasks
  )
where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Exception (evaluate)
import Control.Monad (unless, void)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
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

-- | A job: its hook's key, compared by value, the @IORef (Maybe a)@ its
-- thread writes the result into, and the thread.
data Job = forall k. (Eq k, Typeable k) => Job !k !Dynamic !ThreadId

-- | Run an action on a thread of its own and return its result once it has
-- one: 'Nothing' until the job finishes, then 'Just' what it returned.
--
-- The job starts the first frame this is called with a key, and runs once
-- per key: a frame that calls it with another key kills the running job and
-- starts one for the new key. Keep the key stable while the action should
-- keep running, and change it when the action should run again:
--
-- > (path, setPath) <- useText "notes.txt"
-- > contents <- useTask path (T.readFile (T.unpack path))
-- > label (fromMaybe "Loading..." contents)
--
-- The job's end wakes the loop, once, so the loop sleeps while it runs; the
-- frame it wakes repaints the whole window, as 'askWake' says. Like any hook
-- it takes the next widget id, and the job lives as long as the view calls
-- the hook: a frame that does not call it kills the job. Call it where the
-- view shows the result, or above a tab or a branch that should not end the
-- job. A session's end kills the jobs still running.
--
-- The result is evaluated to weak head normal form on the job's thread;
-- force a lazy structure there too, inside the action, or the view will. An
-- action that throws leaves the hook at 'Nothing', and the exception goes
-- where an uncaught one on a 'forkIO' thread goes; catch it in the action
-- ('Control.Exception.try') to show it. The view must run on the threaded
-- runtime (@-threaded@) for a job to run while the loop sleeps.
useTask :: (Eq k, Typeable k, Typeable a, Ui :> es) => k -> IO a -> Eff es (Maybe a)
useTask k run = do
  (wid, ctx) <- freshWidget
  uiIO $ do
    Tasks ref <- hostOrInit ctx (Tasks <$> newIORef (TaskTable IM.empty IS.empty))
    TaskTable jobs called <- readIORef ref
    let key = intKey wid
        entry = IM.lookup key jobs
        current = case entry of
          Just (Job k0 box _) | cast k0 == Just k -> fromDynamic box
          _ -> Nothing
    -- A job for another key, or one another hook left at this id, is killed
    -- and replaced. A second view pass in the frame finds the job this one
    -- started.
    (box, jobs') <- case current of
      Just box -> pure (box, jobs)
      Nothing -> do
        mapM_ stopJob entry
        box <- newIORef Nothing
        tid <- forkIO $ do
          a <- run >>= evaluate
          atomicWriteIORef box (Just a)
          wakeFromThread ctx
        labelThread tid "nano-ui task"
        pure (box, IM.insert key (Job k (toDyn box) tid) jobs)
    writeIORef ref $! TaskTable jobs' (IS.insert key called)
    readIORef box

-- | An action any thread may call to have the loop run the view again, for
-- something the thread changed that the view reads. The frame it wakes
-- repaints the whole window, since nothing says which widgets show the
-- change. Wakes that come before that frame runs cost that one frame, so a
-- thread faster than the display costs a frame a frame.
--
-- It is how an app builds a stream or a poller of its own: a thread that
-- publishes each value where the view reads it, then wakes the loop. Keep
-- the thread in a 'useTask' job that does not return, and it is killed when
-- the view stops calling the hook:
--
-- > sensorLoop :: IORef (Maybe Double) -> IO () -> IO ()
-- > sensorLoop latest wake = forever $ do
-- >   reading <- readSensor
-- >   writeIORef latest (Just reading)
-- >   wake
-- >
-- > sensorView :: IORef (Maybe Double) -> NanoUI ()
-- > sensorView latest = do
-- >   wake <- askWake
-- >   _ <- useTask () (sensorLoop latest wake)
-- >   reading <- uiIO (readIORef latest)
-- >   label (maybe "--" (T.pack . show) reading)
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
