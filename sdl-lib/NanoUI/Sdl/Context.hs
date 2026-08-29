{-# LANGUAGE DataKinds #-}

-- | SDL pixel-host context defaults shared with headless tests.
module NanoUI.Sdl.Context
  ( newSdlContext
  ) where

import NanoUI.Testing (Context, newPixelContext)

newSdlContext :: IO Context
newSdlContext = newPixelContext
