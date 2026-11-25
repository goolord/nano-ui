-- |
-- Module      : NanoUI.Backend.Rgfw
-- Description : RGFW and OpenGL 3.2 backend for nano-ui
-- Copyright   : (c) 2026 Zachary Churchill
-- License     : MIT
-- Maintainer  : zacharyachurchill@gmail.com
--
-- Run nano-ui views in an RGFW window with OpenGL 3.2 and the bundled
-- Cozette bitmap font. 'runRgfwApp' runs a view; 'runRgfwAppReduce' runs a
-- view against a model and an update function for "NanoUI.Emit".
module NanoUI.Backend.Rgfw
  ( -- * Running
    runRgfwApp
  , runRgfwAppWith
  , runRgfwAppReduce
  , runRgfwAppReduceCustom
  , runRgfwAppReduceCustomWith

    -- * Options and window settings
  , RgfwOptions (..)
  , defaultRgfwOptions
  , WindowSettings (..)
  , defaultWindowSettings
  , WindowPosition (..)
  , WindowMode (..)

    -- * Low-level context
  , newRgfwContext
  , applyRgfwTheme

    -- * Debugging
  , RgfwDebugSampler
  , newRgfwDebugSampler
  , RgfwDebugSnapshot (..)
  , CoreDebugSnapshot (..)
  , RgfwFrameStats (..)
  , askRgfwDebug
  , debugWindowBody
  , emptyRgfwDebug
  ) where

import NanoUI (WindowMode (..), WindowPosition (..), WindowSettings (..), defaultWindowSettings)
import NanoUI.Internal.Debug (CoreDebugSnapshot (..))
import NanoUI.Rgfw.Internal.Context (applyRgfwTheme, newRgfwContext)
import NanoUI.Rgfw.Internal.Debug
  ( RgfwDebugSnapshot (..)
  , RgfwFrameStats (..)
  , RgfwDebugSampler
  , newRgfwDebugSampler
  , askRgfwDebug
  , debugWindowBody
  , emptyRgfwDebug
  )
import NanoUI.Rgfw.Internal.Session
  ( RgfwOptions (..)
  , defaultRgfwOptions
  , runRgfwApp
  , runRgfwAppWith
  , runRgfwAppReduce
  , runRgfwAppReduceCustom
  , runRgfwAppReduceCustomWith
  )
