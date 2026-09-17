-- | Run nano-ui views in an RGFW window with OpenGL 3.2 and the bundled
-- Cozette bitmap font. 'runRgfwApp' runs a view; 'runRgfwAppReduce' runs a
-- view against a model and an update function for "NanoUI.Emit".
module NanoUI.Backend.Rgfw
  ( runRgfwApp
  , runRgfwAppReduce
  , runRgfwAppReduceCustom
  , RgfwOptions (..)
  , defaultRgfwOptions
  , newRgfwContext
  , applyRgfwTheme
  , RgfwDebugSnapshot (..)
  , askRgfwDebug
  , debugWindowBody
  , emptyRgfwDebug
  ) where

import NanoUI.Rgfw.Context (applyRgfwTheme, newRgfwContext)
import NanoUI.Rgfw.Debug
  ( RgfwDebugSnapshot (..)
  , askRgfwDebug
  , debugWindowBody
  , emptyRgfwDebug
  )
import NanoUI.Rgfw.Session
  ( RgfwOptions (..)
  , defaultRgfwOptions
  , runRgfwApp
  , runRgfwAppReduce
  , runRgfwAppReduceCustom
  )
