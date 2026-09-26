-- | Background work. 'useTaskStatus' and 'useTask' run an action on a thread
-- of its own and hand the view its outcome, 'useStream' runs a producer that
-- updates a state the view reads, and 'askWake' gives any other thread a way
-- to run the view again.
--
-- A job's thread never touches the context's stores. It writes into a box
-- of its own and wakes the loop ('wakeFromThread'), and the view reads the
-- box the next time it calls the hook. A job belongs to its hook's widget id
-- and lives as long as the view calls the hook ('useHeld'): 'sweepHeld' ends
-- the ones a frame left out.
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

-- | What the context's hooks hold, kept on it as a host value ('hostOrInit').
newtype Held = Held (IORef HeldTable)

-- | What each hook holds, by the store key of its widget id, and the keys
-- whose hook ran this frame, in any of its view passes.
data HeldTable = HeldTable !(IntMap Holding) !IntSet

-- | A hook's key, compared by value, what it holds for the key, and how to
-- let that go.
data Holding = forall k. (Eq k, Typeable k) => Holding !k !Dynamic !(IO ())

-- | What this hook holds for its key: what it held already, or, for a new
-- key or a hook new at this id, what @acquire@ makes, given what it replaces
-- when that has the same type, along with how to let it go. What another key,
-- or another hook at this id, held is let go first. A second view pass in
-- the frame finds what the first made. What a hook holds is let go once a
-- frame does not call it ('sweepHeld'), and as the session ends
-- ('cancelTasks'): a job's thread, or a 'NanoUI.useImageRgba' image.
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

-- | The box of this hook's job for its key ('useHeld'): @start@ makes the
-- box, given the box of the job it replaces when that has the same type,
-- along with the job to run on a thread of its own.
useJob :: (Eq k, Typeable k, Typeable b, Ui :> es) => k -> (Context -> Maybe b -> IO (b, IO ())) -> Eff es b
useJob k start = useHeld k $ \ctx old -> do
  (box, run) <- start ctx old
  tid <- forkIO run
  labelThread tid "nano-ui task"
  -- Killed from a thread of its own: 'killThread' returns once the job
  -- takes the exception, which a job that masks it or sits in a foreign
  -- call puts off, and the frame must not wait for that.
  pure (box, void (forkIO (killThread tid)))

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
useTaskStatus k run = (\(Outcome status _) -> status) <$> useOutcome k run

-- | 'useTaskStatus' as the latest result there is: the job's once it has
-- returned, before that the result of the job for an earlier key, and
-- 'Nothing' when there is none. A job that failed leaves the result before
-- it.
--
-- > (path, setPath) <- useText "notes.txt"
-- > contents <- useTask path (T.readFile (T.unpack path))
-- > label (fromMaybe "Loading..." contents)
useTask :: (Eq k, Typeable k, Typeable a, Ui :> es) => k -> IO a -> Eff es (Maybe a)
useTask k run = (\(Outcome _ latest) -> latest) <$> useOutcome k run

-- | Where a 'useTaskStatus' job is as this frame reads it. A new key's job
-- starts out running, with the latest result of the job it replaces.
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

-- | End a frame for what the hooks hold: what a hook that ran holds stays,
-- and the rest is let go. Two view passes of one frame count as one frame.
sweepHeld :: Context -> IO ()
sweepHeld ctx = askHostIO ctx >>= mapM_ (\(Held ref) -> sweep ref)
  where
    sweep ref = do
      HeldTable held called <- readIORef ref
      unless (IM.null held && IS.null called) $ do
        let (kept, gone) = IM.partitionWithKey (\k _ -> IS.member k called) held
        writeIORef ref $! HeldTable kept IS.empty
        mapM_ letGo gone

-- | Kill every job on the context, and let go of the images its
-- 'NanoUI.useImageRgba' hooks hold, as a session ends.
-- 'NanoUI.Runner.runSessionLoop' does this when its loop returns; a host
-- that runs frames itself should too.
cancelTasks :: Context -> IO ()
cancelTasks ctx = askHostIO ctx >>= mapM_ cancelAll
  where
    cancelAll (Held ref) = do
      HeldTable held _ <- readIORef ref
      writeIORef ref $! HeldTable IM.empty IS.empty
      mapM_ letGo held
