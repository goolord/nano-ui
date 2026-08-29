{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Back-compat re-exports for frame internals split by ownership.
module NanoUI.Frame.Internal
  ( module NanoUI.Frame.Hit
  , module NanoUI.Frame.Focus
  , module NanoUI.Frame.Clip
  , module NanoUI.Frame.Chrome
  , UiCursorKind (..)
  , grabHoverKind
  , grabDragKind
  ) where

import NanoUI.Frame.Clip
import NanoUI.Frame.Chrome
import NanoUI.Frame.CursorKind (UiCursorKind (..), grabDragKind, grabHoverKind)
import NanoUI.Frame.Focus
import NanoUI.Frame.Hit
