{-# LANGUAGE DataKinds #-}

-- | SDL pixel-host context defaults shared with headless tests.
module NanoUI.Sdl.Context
  ( newSdlContext
  ) where

import NanoUI.Context.Internal (Context)
import NanoUI.Context.New (newPixelHostContext)

newSdlContext :: IO Context
newSdlContext = newPixelHostContext
