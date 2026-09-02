module Main (main) where

import Cases
import Control.Monad (forM_, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import NanoUI.Testing (Context, newContext)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

data TestSpec
  = TestSpec
      { specName :: String
      , specRun :: Context -> IORef Int -> IO ()
      }

main :: IO ()
main = do
  args <- getArgs
  let
    wantAll = null args
    want name = wantAll || name `elem` args
  failed <- newIORef (0 :: Int)
  failedTests <- newIORef (0 :: Int)
  forM_ testSpecs $ \TestSpec{specName = name, specRun} ->
    when (want name) $ do
      putStrLn ("RUN: " ++ name)
      hFlush stdout
      before <- readIORef failed
      ctx <- newContext
      specRun ctx failed
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

testSpecs :: [TestSpec]
testSpecs =
  [ TestSpec "vt-decode" runVtTest
  , TestSpec "cells-and-diff" runCellsTest
  , TestSpec "terminal-default-gap" runTerminalDefaultGapTest
  , TestSpec "terminal-slider-track" runTerminalSliderTrackTest
  , TestSpec "terminal-text-input" runTerminalTextInputDisplayTest
  , TestSpec "terminal-modal-overlay" runTerminalModalOverlayTest
  , TestSpec "terminal-modal-scroll" runTerminalModalScrollTest
  , TestSpec "terminal-modal-tight" runTerminalModalTightTest
  , TestSpec "terminal-modal-open-redraw" runTerminalModalOpenRedrawTest
  , TestSpec "terminal-window-overlay" runTerminalWindowOverlayTest
  , TestSpec "terminal-window-drag" runTerminalWindowDragTest
  , TestSpec "terminal-window-drag-icons" runTerminalWindowDragIconTest
  , TestSpec "terminal-close-button" runTerminalCloseButtonTest
  , TestSpec "terminal-icon-chrome" runTerminalIconChromeTest
  , TestSpec "terminal-icon-close" runTerminalIconCloseTest
  , TestSpec "terminal-button-brackets" runTerminalButtonBracketTest
  , TestSpec "terminal-wide-clear-bracket" runTerminalWideClearBracketTest
  , TestSpec "terminal-wide-cursor-cup" runTerminalWideCursorCupTest
  , TestSpec "terminal-wide-transitions" runTerminalWideTransitionTest
  , TestSpec "terminal-wide-pairs" runTerminalWidePairTest
  , TestSpec "terminal-theme-contrast" runTerminalThemeContrastTest
  , TestSpec "terminal-separator-span" runTerminalSeparatorSpanTest
  ]
