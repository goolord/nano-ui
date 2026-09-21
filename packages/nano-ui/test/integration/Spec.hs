-- | The shape of an integration test: a name, the context it runs on, and the
-- test itself, which bumps the failure counter for each failed check.
module Spec (Spec, spec, pixelSpec) where

import Data.IORef (IORef)
import NanoUI.Testing (Context, newContext, newPixelContext)

type Spec = (String, IO Context, Context -> IORef Int -> IO ())

-- | A test on a headless context, and one on a pixel-snapped context.
spec, pixelSpec :: String -> (Context -> IORef Int -> IO ()) -> Spec
spec name run = (name, newContext, run)
pixelSpec name run = (name, newPixelContext, run)
