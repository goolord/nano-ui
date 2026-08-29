{-# LANGUAGE OverloadedStrings #-}

-- | Back-compat re-exports for widget internals split by ownership.
module NanoUI.Widgets.Internal
  ( module NanoUI.Widgets.Node
  , module NanoUI.Widgets.Chrome
  ) where

import NanoUI.Widgets.Chrome
import NanoUI.Widgets.Node
