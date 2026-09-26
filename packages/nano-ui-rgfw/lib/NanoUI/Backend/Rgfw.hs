-- | Run nano-ui views in an RGFW window with OpenGL 3.2 and the bundled
-- Cozette bitmap font. 'runRgfwApp' runs a view; 'runRgfwAppReduce' runs a
-- view against a model and an update function for "NanoUI.Emit".
module NanoUI.Backend.Rgfw
  ( runRgfwApp
  , runRgfwAppReduce
  , runRgfwAppReduceCustom
  , RgfwOptions (..)
  , defaultRgfwOptions
  , WindowPosition (..)
  , RgbaImage (..)
  , newRgfwContext
  , applyRgfwTheme
  , RgfwDebugSnapshot (..)
  , askRgfwDebug
  , debugWindowBody
  , emptyRgfwDebug
  ) where

import NanoUI (RgbaImage (..), WindowPosition (..))
import NanoUI.Rgfw.Internal.Context (applyRgfwTheme, newRgfwContext)
import NanoUI.Rgfw.Internal.Debug
  ( RgfwDebugSnapshot (..)
  , askRgfwDebug
  , debugWindowBody
  , emptyRgfwDebug
  )
import NanoUI.Rgfw.Internal.Session
  ( RgfwOptions (..)
  , defaultRgfwOptions
  , runRgfwApp
  , runRgfwAppReduce
  , runRgfwAppReduceCustom
  )
