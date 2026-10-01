-- | Tasks: run slow work on a background thread without freezing the UI.
--
-- A view runs every frame, so it must never block. Slow work (a search, a
-- download, a file read) goes in a job that 'useTask', 'useTaskStatus' or
-- 'useStream' runs on a worker thread. The hook returns at once with what the
-- job has produced so far, and the view draws that.
--
-- The rules, all shown below:
--
-- * A job starts the first frame its hook sees a key, and restarts (killing
--   the old one) when the key changes. To rerun the same input, put an
--   attempt counter in the key.
-- * A job lives while the view keeps calling its hook. A frame that skips
--   the hook kills the job, so turning a feature off is just not calling it.
-- * The loop sleeps while a job runs and wakes when it ends (or, for a
--   stream, when it pushes a value). A running job costs no frames; only
--   what you choose to animate meanwhile, such as a spinner, does.
--
-- Each 'Task' or 'Stream' handle is allocated once in 'newApp', like any
-- other state that a positional hook cannot hold. Build with @-threaded@
-- (the examples are) so jobs run while the loop sleeps.
--
-- Run it with @cabal run nano-ui-example-tasks@.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (displayException, throwIO)
import Control.Monad (forM_, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

-- | One handle per job. The types fix each job's key and result.
data App = App
  { appSearch :: !(Task Text [Text])
  , appFlaky :: !(Task Int Text)
  , appProgress :: !(Stream () Int)
  }

newApp :: IO App
newApp = App <$> newTask <*> newTask <*> newStream

main :: IO ()
main = do
  app <- newApp
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings = defaultWindowSettings {wsTitle = "Tasks", wsSize = Size 720 640}
      }
    (view app)

view :: App -> NanoUI ()
view app = columnWith (padAll 16 . gap 12 . grow) $ do
  card (searchSection app)
  card (flakySection app)
  card (progressSection app)

-- | (a) A search whose results come from a slow job keyed by the query.
searchSection :: App -> NanoUI ()
searchSection app = do
  heading "Slow search"
  (query, setQuery) <- useText ""
  (committed, setCommitted) <- useText ""
  (resp, query') <- searchInput' "Find a word" query
  setQuery query'
  -- searchInput' reports a change once typing pauses, so a fast typist
  -- starts one job, not one per keystroke. Keying by query' would also work;
  -- each new key would just kill the job before it.
  when (respChanged resp) (setCommitted query')
  status <- useTaskStatus (appSearch app) committed (slowSearch committed)
  -- While a new query runs, TaskRunning carries the previous results, so
  -- the list stays put instead of flashing empty.
  let (hits, running) = case status of
        TaskRunning previous -> (fromMaybe [] previous, True)
        TaskDone found -> (found, False)
        TaskFailed _ previous -> (fromMaybe [] previous, False)
  rowWith (tight . gap 8 . alignMid) $ do
    -- The spinner comes and goes, so it sits in a scope: the label after it
    -- keeps its widget id either way.
    scope (when running spinner)
    muted (T.pack (show (length hits)) <> " matches" <> if running then " (searching...)" else "")
  rowWith (wrap . gap 6 . fillW) $
    forM_ (take 24 hits) $ \w -> withKey w (label w)

-- | Pretend to be slow. The result is forced only to weak head normal form
-- on the worker, so force the list's spine here rather than in the view.
slowSearch :: Text -> IO [Text]
slowSearch q = do
  threadDelay 500000
  let found = filter (T.isInfixOf (T.toLower q)) wordList
  length found `seq` pure found

wordList :: [Text]
wordList =
  T.words
    "apple apricot avocado banana blackberry blueberry cantaloupe cherry \
    \clementine coconut cranberry currant date dragonfruit durian elderberry \
    \fig gooseberry grape grapefruit guava honeydew jackfruit kiwi kumquat \
    \lemon lime lychee mango mandarin melon mulberry nectarine olive orange \
    \papaya passionfruit peach pear persimmon pineapple plantain plum \
    \pomegranate quince raspberry rhubarb starfruit strawberry tangerine \
    \tomato watermelon"

-- | (b) A job that fails half the time, with a Retry button.
flakySection :: App -> NanoUI ()
flakySection app = do
  heading "Flaky request"
  (attempt, setAttempt) <- useInt 0
  -- Same input every time, so the attempt number is the key: bumping it is
  -- what reruns the job.
  status <- useTaskStatus (appFlaky app) attempt (flakyRequest attempt)
  -- Each branch declares different widgets, so the whole case is one scope.
  scope $ case status of
    TaskRunning _ -> rowWith (tight . gap 8 . alignMid) $ do
      spinner
      label "Contacting the server..."
    TaskDone reply -> rowWith (tight . gap 8 . alignMid) $ do
      label reply
      whenM (button "Run again") (setAttempt (attempt + 1))
    TaskFailed err _ -> rowWith (tight . gap 8 . alignMid) $ do
      danger (T.pack (displayException err))
      whenM (button "Retry") (setAttempt (attempt + 1))

-- | Waits, then succeeds or throws on a coin flip. An exception thrown by
-- the job becomes 'TaskFailed'.
flakyRequest :: Int -> IO Text
flakyRequest attempt = do
  threadDelay 800000
  coin <- getMonotonicTimeNSec
  if even (coin `div` 1000)
    then pure ("Attempt " <> T.pack (show (attempt + 1)) <> " succeeded.")
    else throwIO (userError ("attempt " <> show (attempt + 1) <> " timed out"))

-- | (c) A producer pushing progress ticks, alive only while "Run" is ticked.
progressSection :: App -> NanoUI ()
progressSection app = do
  heading "Streamed progress"
  (running, setRunning) <- useFlag False
  setRunning =<< checkbox "Run" running
  -- When "Run" is unticked this hook is not called, and that alone kills
  -- the producer: there is no stop button to wire up. Ticking it again
  -- starts a fresh producer from the initial value. The scope keeps
  -- anything declared after it on stable ids.
  scope $ when running $ do
    pct <- useStream (appProgress app) () (0 :: Int) $ \update ->
      forM_ [1 .. 100] $ \i -> do
        threadDelay 50000
        -- Each update wakes the loop for one frame; updates that arrive
        -- between frames share it.
        update (const i)
    progressBar (fromIntegral pct / 100)
    muted (if pct >= 100 then "Done. Untick and tick Run to start over." else T.pack (show pct) <> "%")
