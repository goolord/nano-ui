-- | Headless UI tests for the SDL demo apps. Each test draws its app on a
-- hidden window, drives real mouse and keyboard gestures, and fails on the
-- first check that does not hold.
module Main (main) where

import DemoSelftest qualified as Demo
import LogsSelftest qualified as Logs
import NotepadSelftest qualified as Notepad

main :: IO ()
main = do
  Demo.selftest False
  Demo.selftest True
  Notepad.selftest
  Logs.selftest
