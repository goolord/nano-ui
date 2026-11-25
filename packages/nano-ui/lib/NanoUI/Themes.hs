-- | Extra built-in theme presets and Base16 palette construction.
--
-- "NanoUI" exports 'NanoUI.defaultTheme', 'NanoUI.defaultLightTheme', and
-- semantic styling modifiers. Import this module for the Tomorrow presets
-- and for generating themes from 16-color Base16 palettes.
module NanoUI.Themes
  ( -- * Presets
    tomorrowNightMinDarkTheme
  , tomorrowMinLightTheme
  , tomorrowMidnightMinDarkTheme

    -- * Base16 palettes
  , Base16 (..)
  , themeFromBase16
  , themeFromBase16Dark
  , themeFromBase16Light
  , base16TomorrowNight
  , base16TomorrowLight
  ) where

import NanoUI.Internal.Style
  ( Base16 (..)
  , base16TomorrowLight
  , base16TomorrowNight
  , themeFromBase16
  , themeFromBase16Dark
  , themeFromBase16Light
  , tomorrowMidnightMinDarkTheme
  , tomorrowMinLightTheme
  , tomorrowNightMinDarkTheme
  )
