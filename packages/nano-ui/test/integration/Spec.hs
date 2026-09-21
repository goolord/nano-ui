-- | What every integration test module imports: the library, the test
-- harness and assertions, and the shape of a test with its two helpers.
module Spec
  ( Spec
  , spec
  , pixelSpec
  , module Control.Monad
  , module Data.IORef
  , module NanoUI
  , module NanoUI.Backend
  , module NanoUI.Testing
  , module NanoUI.Testing.Assert
  , module NanoUI.Testing.Harness
  ) where

import Control.Monad
import Data.IORef
import NanoUI
import NanoUI.Backend
import NanoUI.Testing
import NanoUI.Testing.Assert
import NanoUI.Testing.Harness

-- | A test's name, the context it runs on, and the test, which bumps the
-- failure counter for each failed check.
type Spec = (String, IO Context, Context -> IORef Int -> IO ())

-- | A test on a headless context, and one on a pixel-snapped context.
spec, pixelSpec :: String -> (Context -> IORef Int -> IO ()) -> Spec
spec name run = (name, newContext, run)
pixelSpec name run = (name, newPixelContext, run)
