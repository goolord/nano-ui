-- |
-- Module      : NanoUI.Diagrams
-- Description : Charts and diagrams-lib rendering for nano-ui
-- Copyright   : (c) 2026 Zachary Churchill
-- License     : MIT
-- Maintainer  : zacharyachurchill@gmail.com
--
-- Full @nano-ui-diagrams@ API: the @diagrams-lib@ backend, diagram widgets,
-- and "NanoUI.Plot" charting widgets.
module NanoUI.Diagrams
  ( -- * Diagrams backend
    module NanoUI.Diagrams.Backend

    -- * Diagram widgets
  , module NanoUI.Diagrams.Widget

    -- * Charts
  , module NanoUI.Plot
  ) where

import NanoUI.Diagrams.Backend
import NanoUI.Diagrams.Widget
import NanoUI.Plot
