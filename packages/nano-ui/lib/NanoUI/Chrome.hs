-- | Client-side window chrome for frameless windows: caption buttons,
-- window borders, and title-bar drag regions.
module NanoUI.Chrome
  ( -- * Caption buttons
    CaptionGlyph (..)
  , CaptionAction (..)
  , CaptionConfig (..)
  , defaultCaptionConfig
  , captionButton
  , captionButtons
  , captionButtonsConfigured

    -- * Window border
  , WindowFrame (..)
  , defaultWindowFrame
  , windowFrame

    -- * Geometry
  , captionBarHeight
  , dragSpans
  ) where

import NanoUI.Internal.Widgets.Caption
