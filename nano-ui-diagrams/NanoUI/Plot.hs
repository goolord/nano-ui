{-# LANGUAGE OverloadedStrings #-}

-- | Charting and plotting API for nano-ui.
module NanoUI.Plot
  ( module NanoUI.Plot.Types
  , module NanoUI.Plot.Series
  , module NanoUI.Plot.Scale
  , module NanoUI.Plot.Decimate
  , module NanoUI.Plot.Chrome
  , module NanoUI.Plot.Widget
  , module NanoUI.Plot.Builder
  , module NanoUI.Plot.Hit
  ) where

import NanoUI.Plot.Builder
import NanoUI.Plot.Hit
import NanoUI.Plot.Chrome
import NanoUI.Plot.Decimate
import NanoUI.Plot.Scale
import NanoUI.Plot.Series
import NanoUI.Plot.Types
import NanoUI.Plot.Widget
