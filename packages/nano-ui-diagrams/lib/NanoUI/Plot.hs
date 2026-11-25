-- |
-- Module      : NanoUI.Plot
-- Description : Interactive charts and plotting widgets for nano-ui
-- Copyright   : (c) 2026 Zachary Churchill
-- License     : MIT
-- Maintainer  : zacharyachurchill@gmail.com
--
-- Charting and plotting API for nano-ui: line, scatter, bar, area, and step
-- series, automatic tick scaling, LTTB point decimation, hover inspection,
-- and cached chart widgets.
module NanoUI.Plot
  ( -- * Chart builders
    module NanoUI.Plot.Builder

    -- * Chart widgets
  , module NanoUI.Plot.Widget

    -- * Series constructors
  , module NanoUI.Plot.Series

    -- * Core types
  , module NanoUI.Plot.Types

    -- * Domains and scales
  , module NanoUI.Plot.Scale

    -- * Decimation
  , module NanoUI.Plot.Decimate

    -- * Axes and chrome
  , module NanoUI.Plot.Chrome
  ) where

import NanoUI.Plot.Builder
import NanoUI.Plot.Chrome
import NanoUI.Plot.Decimate
import NanoUI.Plot.Scale
import NanoUI.Plot.Series
import NanoUI.Plot.Types
import NanoUI.Plot.Widget
