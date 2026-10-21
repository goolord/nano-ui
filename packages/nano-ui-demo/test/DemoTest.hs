-- | Headless UI tests for the SDL demo apps. Each test draws its app on a
-- hidden window, drives real mouse and keyboard gestures, and fails on the
-- first check that does not hold. With no display to open a window on, as
-- on Linux CI or over ssh, they draw on SDL's dummy driver.
module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Maybe (isNothing)
import DemoSelftest qualified as Demo
import ImeSelftest qualified as Ime
import LogsSelftest qualified as Logs
import NotepadSelftest qualified as Notepad
import System.Environment (lookupEnv, setEnv)
import System.Info (os)

main :: IO ()
main = do
  display <- (<|>) <$> lookupEnv "DISPLAY" <*> lookupEnv "WAYLAND_DISPLAY"
  when (os `notElem` ["mingw32", "darwin"] && isNothing display) $ do
    setEnv "SDL_VIDEODRIVER" "dummy"
    setEnv "SDL_RENDER_DRIVER" "software"
  Demo.selftest False
  Demo.selftest True
  Notepad.selftest
  Logs.selftest
  Ime.selftest
