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
import Control.Monad (filterM, unless, void, when)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Maybe (isJust, isNothing)
import Data.Primitive.PrimVar (PrimVar, modifyPrimVar, newPrimVar, readPrimVar, writePrimVar)
import Data.Type.Equality ((:~:) (Refl))
import Data.Typeable (Typeable, cast, eqT)
import GHC.Conc (labelThread)
import GHC.Exts (RealWorld)
import NanoUI.Internal.Context (Context, askHostIO, hostOrInit, intKey, wakeFromThread)
import NanoUI.Internal.Monad (NanoUI, askContext, freshWidget, liftIO)

-- | Per-context hook resources, stored as a host value ('hostOrInit'): the
-- resources by widget store key; the frame the view is running, which each
-- hook stamps its entry with; how many entries are stamped this frame (in
-- any view pass); and how many there are. A frame whose hooks all ran ends
-- without looking at any entry.
data Held = Held !(IORef (IntMap Holding)) !(PrimVar RealWorld Int) !(PrimVar RealWorld Int) !(PrimVar RealWorld Int)

-- | A hook's key (compared by value), its resource, the release action, and
-- the last frame its hook ran.
data Holding = forall k. (Eq k, Typeable k) => Holding !k !Dynamic !(IO ()) !(PrimVar RealWorld Int)

-- | Hold a resource for key @k@ at this hook's widget id. While the key is
-- unchanged the cached value is returned. Otherwise the old resource is
-- released and @acquire@ builds a new one; it gets the old value when the
-- types match. A second view pass in the same frame reuses the first pass's
-- value. Resources are released when a frame skips the hook ('sweepHeld') and
-- when the session ends ('cancelTasks'). Holds job threads and
-- 'NanoUI.useImageRgba' images.
useHeld :: (Eq k, Typeable k, Typeable a) => k -> (Context -> Maybe a -> IO (a, IO ())) -> NanoUI a
useHeld k acquire = useHeldBy k Just (\ctx old -> (\(v, release) -> (v, v, release)) <$> acquire ctx old)

-- | 'useHeld' keeping an @s@ and returning what @view@ finds in it. An entry
-- @view@ finds nothing in is replaced, as one of another type is.
{-# INLINE useHeldBy #-}
useHeldBy :: (Eq k, Typeable k, Typeable s) => k -> (s -> Maybe b) -> (Context -> Maybe b -> IO (s, b, IO ())) -> NanoUI b
useHeldBy k view acquire = do
  (wid, ctx) <- freshWidget
  liftIO $ do
    Held ref frameVar stampedVar countVar <- hostOrInit ctx newHeld
    held <- readIORef ref
    frame <- readPrimVar frameVar
    let key = intKey wid
        entry = IM.lookup key held
        valueOf (Holding _ v _ _) = view =<< fromDynamic v
        stamp = modifyPrimVar stampedVar (+ 1)
    case entry of
      Just h@(Holding k0 _ _ seen) | cast k0 == Just k, Just v <- valueOf h -> do
        s <- readPrimVar seen
        unless (s == frame) $ writePrimVar seen frame >> stamp
        pure v
      _ -> do
        -- A replaced entry stamped earlier this frame stays counted.
        counted <- maybe (pure False) (\old -> letGo old >> (== frame) <$> seenIn old) entry
        (stored, v, release) <- acquire ctx (valueOf =<< entry)
        seen <- newPrimVar frame
        writeIORef ref $! IM.insert key (Holding k (toDyn stored) release seen) held
        unless counted stamp
        when (isNothing entry) $ modifyPrimVar countVar (+ 1)
        pure v
  where
    newHeld = Held <$> newIORef IM.empty <*> newPrimVar 0 <*> newPrimVar 0 <*> newPrimVar 0
    seenIn (Holding _ _ _ seen) = readPrimVar seen

letGo :: Holding -> IO ()
letGo (Holding _ _ release _) = release

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

-- | A job's result box as its hook keeps it. The type of the box itself is
-- known here, so finding it costs a fingerprint compare; a 'Dynamic' of the
-- box would build that type from the caller's result type on every call.
data TaskBox = forall a. Typeable a => TaskBox !(IORef (Outcome a))

-- | The box, if its job returns an @a@.
taskRef :: forall a. Typeable a => TaskBox -> Maybe (IORef (Outcome a))
taskRef (TaskBox (ref :: IORef (Outcome b))) = (\Refl -> ref) <$> eqT @a @b

-- | 'useHeldBy' for a job. @start@ builds what the hook keeps, the result
-- box (given the old box when @view@ finds one) and the action to fork.
{-# INLINE useJob #-}
useJob :: (Eq k, Typeable k, Typeable s) => k -> (s -> Maybe b) -> (Context -> Maybe b -> IO (s, b, IO ())) -> NanoUI b
useJob k view start = useHeldBy k view $ \ctx old -> do
  (stored, box, run) <- start ctx old
  tid <- forkIO run
  labelThread tid "nano-ui task"
  -- Kill from a separate thread: 'killThread' blocks until the job receives
  -- the exception, which masking or a foreign call can delay.
  pure (stored, box, void (forkIO (killThread tid)))

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
useTaskStatus :: (Eq k, Typeable k, Typeable a) => k -> IO a -> NanoUI (TaskStatus a)
useTaskStatus k run = (\(Outcome status _) -> status) <$> useOutcome k run

-- | Like 'useTaskStatus', but returns only the latest result: the current
-- job's once it returns, otherwise the previous key's, or 'Nothing'. A failed
-- job keeps the previous result.
--
-- > (path, setPath) <- useText "notes.txt"
-- > contents <- useTask path (T.readFile (T.unpack path))
-- > label (fromMaybe "Loading..." contents)
useTask :: (Eq k, Typeable k, Typeable a) => k -> IO a -> NanoUI (Maybe a)
useTask k run = (\(Outcome _ latest) -> latest) <$> useOutcome k run

-- | The job's outcome as of this frame. A new key's job starts as running,
-- carrying the replaced job's latest result.
useOutcome :: (Eq k, Typeable k, Typeable a) => k -> IO a -> NanoUI (Outcome a)
useOutcome k run = do
  box <- useJob k taskRef $ \ctx old -> do
    prev <- maybe (pure Nothing) (fmap (\(Outcome _ latest) -> latest) . readIORef) old
    box <- newIORef (Outcome (TaskRunning prev) prev)
    let finish = (>> wakeFromThread ctx) . atomicWriteIORef box
    pure
      ( TaskBox box
      , box
      , try (run >>= evaluate) >>= \case
          Right a -> finish (Outcome (TaskDone a) (Just a))
          Left e
            | isJust (fromException e :: Maybe SomeAsyncException) -> throwIO e
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
useStream :: (Eq k, Typeable k, Typeable s) => k -> s -> (((s -> s) -> IO ()) -> IO ()) -> NanoUI s
useStream k initial produce = do
  box <- useJob k Just $ \ctx _ -> do
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
sweepHeld ctx = askHostIO ctx >>= mapM_ sweep
  where
    sweep (Held ref frameVar stampedVar countVar) = do
      frame <- readPrimVar frameVar
      stamped <- readPrimVar stampedVar
      count <- readPrimVar countVar
      writePrimVar frameVar (frame + 1)
      writePrimVar stampedVar 0
      unless (stamped == count) $ do
        held <- readIORef ref
        gone <- filterM (\(_, Holding _ _ _ seen) -> (/= frame) <$> readPrimVar seen) (IM.toAscList held)
        writeIORef ref $! IM.withoutKeys held (IS.fromDistinctAscList (map fst gone))
        writePrimVar countVar (count - length gone)
        mapM_ (letGo . snd) gone

-- | Kill every job on the context and release images held by
-- 'NanoUI.useImageRgba', at session end. 'NanoUI.Runner.runSessionLoop' calls
-- this when its loop returns; hosts that run frames themselves should too.
cancelTasks :: Context -> IO ()
cancelTasks ctx = askHostIO ctx >>= mapM_ cancelAll
  where
    cancelAll (Held ref _ stampedVar countVar) = do
      held <- readIORef ref
      writeIORef ref IM.empty
      writePrimVar stampedVar 0
      writePrimVar countVar 0
      mapM_ letGo held
