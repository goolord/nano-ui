-- | Terminal test helpers: adaptive contexts, palette probes, and raster checks.
module NanoUI.Testing.Term
  ( newAdaptiveTerminalContext
  , newTerminalContext
  , queryTerminalColors
  , terminalDefaultFg
  , terminalDefaultBg
  , terminalThemeFromColors
  , frameBytes
  , cellChar
  , cellRows
  , cellsH
  , narrowChar
  , rasterize
  , rasterizeLayered
  , MouseAction (..)
  , MouseBtn (..)
  , TermEvent (..)
  , noMods
  , decode
  , flushPending
  ) where

import NanoUI.Term.Ansi (frameBytes)
import NanoUI.Term.Cells (cellChar, cellRows, cellsH, narrowChar, rasterize, rasterizeLayered)
import NanoUI.Term.Event (MouseAction (..), MouseBtn (..), TermEvent (..), noMods)
import NanoUI.Term.Palette
  ( newAdaptiveTerminalContext
  , newTerminalContext
  , queryTerminalColors
  , terminalDefaultBg
  , terminalDefaultFg
  , terminalThemeFromColors
  )
import NanoUI.Term.Vt (decode, flushPending)
