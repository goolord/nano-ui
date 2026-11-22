-- | Shared runner for the @exitcode-stdio@ integration suites: runs named
-- specs against fresh contexts, selecting them by command-line name.
--
-- The program arguments choose what runs:
--
-- * no arguments runs every spec;
-- * an argument that is a spec's full name selects that spec;
-- * any other argument selects every spec whose name starts with it, and
--   one that starts no name stops the run with an error;
-- * @--list@ prints the names the other arguments select, one per line,
--   and runs nothing.
module NanoUI.Testing.Runner
  ( runTests
  , runChecks
  , check
  ) where

import Control.Exception (SomeAsyncException, SomeException, catch, displayException, fromException, throwIO)
import Control.Monad (forM, unless, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf, partition)
import Data.Maybe (isJust)
import NanoUI.Testing (Context)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

-- | Run the given specs. Each entry is a test name, a context maker, and the
-- test body (which receives the context and a shared failure counter). The
-- program arguments select which tests run, as the module header describes.
-- A test counts as failed when it incremented the counter or threw.
runTests :: [(String, IO Context, Context -> IORef Int -> IO ())] -> IO ()
runTests specs = runChecks [(name, \failed -> mkCtx >>= \ctx -> run ctx failed) | (name, mkCtx, run) <- specs]

-- | 'runTests' for specs that need no 'Context': each entry is a test name
-- and a body that receives the shared failure counter.
runChecks :: [(String, IORef Int -> IO ())] -> IO ()
runChecks specs = do
  (flags, args) <- partition (== "--list") <$> getArgs
  let names = map fst specs
      selects arg name = arg == name || (arg `notElem` names && arg `isPrefixOf` name)
      unknown = filter (\arg -> not (any (selects arg) names)) args
      chosen = [s | s@(name, _) <- specs, null args || any (`selects` name) args]
  unless (null unknown) $
    fail ("Unknown test names: " ++ unwords unknown)
  if not (null flags)
    then mapM_ (putStrLn . fst) chosen
    else do
      failed <- newIORef (0 :: Int)
      results <-
        forM chosen $ \(name, run) -> do
          putStrLn ("RUN: " ++ name)
          hFlush stdout
          before <- readIORef failed
          -- A test that throws fails on its own, and the specs after it still run.
          run failed `catch` \(e :: SomeException) -> do
            when (isJust (fromException @SomeAsyncException e)) (throwIO e)
            putStrLn ("EXCEPTION: " ++ displayException e)
            modifyIORef' failed (+ 1)
          after <- readIORef failed
          when (after > before) $ putStrLn ("FAIL: " ++ name)
          pure (after > before)
      let n = length (filter id results)
      if n == 0
        then putStrLn "All tests passed."
        else do
          putStrLn $ show n ++ " test(s) failed."
          fail "tests failed"

-- | Print a named check as @[PASS] name@ or @[FAIL] name@. A failure
-- increments the counter and returns, so the checks after it still run.
check :: IORef Int -> String -> Bool -> IO ()
check failed name ok
  | ok = putStrLn ("[PASS] " ++ name)
  | otherwise = putStrLn ("[FAIL] " ++ name) >> modifyIORef' failed (+ 1)
