module Main (main) where

import Cases
import Data.IORef (IORef)
import NanoUI.Testing (Context, newContext)
import NanoUI.Testing.Runner (runTests)

main :: IO ()
main = runTests [(specName, newContext, specRun) | TestSpec{specName, specRun} <- testSpecs]

data TestSpec
  = TestSpec
      { specName :: String
      , specRun :: Context -> IORef Int -> IO ()
      }

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
