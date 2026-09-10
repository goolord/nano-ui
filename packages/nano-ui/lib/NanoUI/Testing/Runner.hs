-- | Shared runner for the @exitcode-stdio@ integration suites: runs named
-- specs against fresh contexts, selecting them by command-line name.
module NanoUI.Testing.Runner
  ( runTests
  ) where

import Control.Monad (forM_, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import NanoUI.Testing (Context)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

-- | Run the given specs. Each entry is a test name, a context maker, and the
-- test body (which receives the context and a shared failure counter). Names
-- passed as program arguments select which tests run; with no arguments
-- everything runs. A test counts as failed when it incremented the counter.
runTests :: [(String, IO Context, Context -> IORef Int -> IO ())] -> IO ()
runTests specs = do
  args <- getArgs
  let
    wantAll = null args
    want name = wantAll || name `elem` args
  failed <- newIORef (0 :: Int)
  failedTests <- newIORef (0 :: Int)
  forM_ specs $ \(name, mkCtx, run) ->
    when (want name) $ do
      putStrLn ("RUN: " ++ name)
      hFlush stdout
      before <- readIORef failed
      ctx <- mkCtx
      run ctx failed
      after <- readIORef failed
      when (after > before) $ do
        modifyIORef' failedTests (+ 1)
        putStrLn ("FAIL: " ++ name)
  n <- readIORef failedTests
  if n == 0
    then putStrLn "All tests passed."
    else do
      putStrLn $ show n ++ " test(s) failed."
      fail "tests failed"
