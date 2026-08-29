-- | SDL helpers for benchmarks and integration tests.
module NanoUI.Testing.Sdl
  ( SdlEnv (..)
  , newSdlContext
  , sdlDrawFrame
  , withSdlBench
  , syncDisplay
  ) where

import NanoUI.Sdl.Runner (newSdlContext, sdlDrawFrame)
import NanoUI.Sdl.Window (SdlEnv (..), syncDisplay, withSdlBench)
