-- | Layout options, text styling, and theme palettes. Re-export facade over
-- "NanoUI.Internal.Style.Layout", "NanoUI.Internal.Style.Theme", and
-- "NanoUI.Internal.Style.Presets".
module NanoUI.Internal.Style
  ( module NanoUI.Internal.Style.Layout
  , module NanoUI.Internal.Style.Theme
  , module NanoUI.Internal.Style.Presets
  , fadeAlpha
  ) where

import NanoUI.Internal.Style.Layout
import NanoUI.Internal.Style.Presets
import NanoUI.Internal.Style.Theme
import NanoUI.Internal.Types (fadeAlpha)
