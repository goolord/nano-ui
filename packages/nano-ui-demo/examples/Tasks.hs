-- | Tasks: run slow work on a background thread without freezing the UI.
--
-- A view runs every frame, so it must never block. Slow work (a search, a
-- download, a file read) goes in a job that 'useTask', 'useTaskStatus' or
-- 'useStream' runs on a worker thread. The hook returns at once with what the
-- job has produced so far, and the view draws that.
--
-- The rules, all shown below:
--
-- * A task starts on the first frame its hook is called with a key, and
--   restarts (cancelling the previous thread) when the key changes. To re-run
--   with the same input, include an attempt counter in the key.
-- * A task runs only while the view continues calling its hook. Omitting the
--   call on a frame cancels the task.
-- * The frame loop sleeps while a task runs and wakes when it completes (or
--   when a stream pushes a value).
--
-- Each 'Task' or 'Stream' handle is allocated once in 'newApp', like any
-- other state that 'useVar' cannot hold. Build with @-threaded@
-- (the examples are) so jobs run while the loop sleeps.
--
-- Run it with @cabal run nano-ui-example-tasks@.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (displayException, throwIO)
import Control.Monad (forM_, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTimeNSec)
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

-- ANCHOR: handles

-- | One handle per job. The types fix each job's key and result.
data App = App
  { appSearch :: !(Task Text [Text])
  , appFlaky :: !(Task Int Text)
  , appProgress :: !(Stream () Int)
  }

newApp :: IO App
newApp = App <$> newTask <*> newTask <*> newStream

-- ANCHOR_END: handles

main :: IO ()
main = do
  app <- newApp
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings =
          defaultWindowSettings {wsTitle = "Tasks", wsSize = Size 720 640}
      }
    (view app)

view :: App -> NanoUI ()
view app = columnWith (padAll 16 . gap 12 . grow) $ do
  card (heading "Slow search" >> searchCard app)
  card (heading "Flaky request" >> flakyCard app)
  card (heading "Streamed progress" >> progressCard app)

-- | (a) A search whose results come from a slow job keyed by the query.
-- ANCHOR: search_card
searchCard :: App -> NanoUI ()
searchCard app = do
  query <- useVar ""
  committed <- useVar ""
  resp <- searchInput' "Find a word" query
  -- searchInput' reports a change once typing pauses, so a fast typist
  -- starts one job, not one per keystroke. Keying by the query itself would
  -- also work; each new key would just kill the job before it.
  when (respChanged resp) (writeVar committed =<< readVar query)
  key <- readVar committed
  status <- useTaskStatus (appSearch app) key (slowSearch key)
  -- While a new query runs, TaskRunning carries the previous results, so
  -- the list stays put instead of flashing empty.
  let
    (hits, running) = case status of
      TaskRunning previous -> (fromMaybe [] previous, True)
      TaskDone found -> (found, False)
      TaskFailed _ previous -> (fromMaybe [] previous, False)
  rowWith (tight . gap 8 . alignMid) $ do
    -- The spinner comes and goes, so it sits in a scope: the label after it
    -- keeps its widget id either way.
    scope (when running spinner)
    muted
      ( T.pack (show (length hits))
          <> " matches"
          <> if running then " (searching...)" else ""
      )
  rowWith (wrap . gap 6 . fillW) $
    forM_ (take 24 hits) $
      \w -> withKey w (label w)

-- ANCHOR_END: search_card

-- | Pretend to be slow. The result is forced only to weak head normal form
-- on the worker, so force the list's spine here rather than in the view.
-- ANCHOR: slow_search
slowSearch :: Text -> IO [Text]
slowSearch q = do
  threadDelay 500000
  let
    found = filter (T.isInfixOf (T.toLower q)) wordList
  length found `seq` pure found

-- ANCHOR_END: slow_search

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
-- ANCHOR: flaky_card
flakyCard :: App -> NanoUI ()
flakyCard app = do
  attemptVar <- useVar (0 :: Int)
  attempt <- readVar attemptVar
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
      whenM (button "Run again") (modifyVar attemptVar (+ 1))
    TaskFailed err _ -> rowWith (tight . gap 8 . alignMid) $ do
      danger (T.pack (displayException err))
      whenM (button "Retry") (modifyVar attemptVar (+ 1))

-- ANCHOR_END: flaky_card

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
-- ANCHOR: progress_card
progressCard :: App -> NanoUI ()
progressCard app = do
  running <- useVar False
  checkbox "Run" running
  isRunning <- readVar running
  -- When "Run" is unticked 'useStream' is not called, and that alone kills
  -- the producer: there is no stop button to wire up. Ticking it again
  -- starts a fresh producer from the initial value. The scope keeps
  -- anything declared after it on stable ids.
  scope $ when isRunning $ do
    pct <- useStream (appProgress app) () (0 :: Int) $ \update ->
      forM_ [1 .. 100] $ \i -> do
        threadDelay 50000
        -- Each update wakes the loop for one frame; updates that arrive
        -- between frames share it.
        update (const i)
    progressBar (fromIntegral pct / 100)
    muted
      ( if pct >= 100
          then "Done. Untick and tick Run to start over."
          else T.pack (show pct) <> "%"
      )

-- ANCHOR_END: progress_card
