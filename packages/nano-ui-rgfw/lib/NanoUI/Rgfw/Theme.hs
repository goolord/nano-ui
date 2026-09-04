module NanoUI.Rgfw.Theme
  ( RgfwTheme (..)
  , defaultDarkTheme
  , defaultLightTheme
  , tomorrowMinLightTheme
  , tomorrowNightMinDarkTheme
  , tomorrowMidnightMinDarkTheme
  ) where

import NanoUI (Color, colorRGBA)

-- | Pure color theme.
-- All visual geometry is strictly 1:1 rectangles matching bounding/collision boxes.
data RgfwTheme = RgfwTheme
  { thBackground    :: !Color
  , thPanelBg       :: !Color
  , thBorder        :: !Color
  , thBorderFocused :: !Color
  , thText          :: !Color
  , thTextMuted     :: !Color
  , thPrimary       :: !Color
  , thPrimaryHover  :: !Color
  , thPrimaryActive :: !Color
  , thWidgetBg      :: !Color
  , thWidgetHover   :: !Color
  , thWidgetActive  :: !Color
  , thThumb         :: !Color
  , thThumbHover    :: !Color
  , thSelection     :: !Color
  , thWindowHeader  :: !Color
  , thScrollTrack   :: !Color
  , thScrollThumb   :: !Color
  }
  deriving (Eq, Show)

-- | Ported from "Tomorrow Min" in https://github.com/biaqat/tomorrow-min-theme-zed
tomorrowMinLightTheme :: RgfwTheme
tomorrowMinLightTheme =
  RgfwTheme
    { thBackground    = colorRGBA 255 255 255 255
    , thPanelBg       = colorRGBA 242 242 242 255
    , thBorder        = colorRGBA 222 222 222 255
    , thBorderFocused = colorRGBA 82 134 188 255
    , thText          = colorRGBA 96 96 95 255
    , thTextMuted     = colorRGBA 140 140 140 255
    , thPrimary       = colorRGBA 82 134 188 255
    , thPrimaryHover  = colorRGBA 62 108 155 255
    , thPrimaryActive = colorRGBA 46 82 119 255
    , thWidgetBg      = colorRGBA 234 234 234 255
    , thWidgetHover   = colorRGBA 223 223 223 255
    , thWidgetActive  = colorRGBA 208 208 208 255
    , thThumb         = colorRGBA 204 204 204 255
    , thThumbHover    = colorRGBA 184 184 184 255
    , thSelection     = colorRGBA 82 134 188 80
    , thWindowHeader  = colorRGBA 230 230 230 255
    , thScrollTrack   = colorRGBA 242 242 242 255
    , thScrollThumb   = colorRGBA 204 204 204 255
    }

-- | Ported from "Tomorrow Night Min" in https://github.com/biaqat/tomorrow-min-theme-zed
tomorrowNightMinDarkTheme :: RgfwTheme
tomorrowNightMinDarkTheme =
  RgfwTheme
    { thBackground    = colorRGBA 37 150 190 255 -- #2596BE
    , thPanelBg       = colorRGBA 28 118 150 255 -- #1C7696
    , thBorder        = colorRGBA 50 180 225 255 -- #32B4E1
    , thBorderFocused = colorRGBA 255 255 255 255 -- #FFFFFF
    , thText          = colorRGBA 255 255 255 255 -- #FFFFFF
    , thTextMuted     = colorRGBA 195 235 248 255 -- #C3EBF8
    , thPrimary       = colorRGBA 240 198 116 255 -- Tomorrow Gold (#F0C674)
    , thPrimaryHover  = colorRGBA 248 214 148 255 -- #F8D694
    , thPrimaryActive = colorRGBA 222 178 92 255  -- #DEB25C
    , thWidgetBg      = colorRGBA 22 96 122 255   -- #16607A
    , thWidgetHover   = colorRGBA 33 138 174 255  -- #218AAE
    , thWidgetActive  = colorRGBA 44 165 208 255  -- #2CA5D0
    , thThumb         = colorRGBA 50 180 225 255  -- #32B4E1
    , thThumbHover    = colorRGBA 80 205 245 255  -- #50CDF5
    , thSelection     = colorRGBA 240 198 116 90
    , thWindowHeader  = colorRGBA 28 118 150 255
    , thScrollTrack   = colorRGBA 24 105 133 255
    , thScrollThumb   = colorRGBA 50 180 225 255
    }

-- | Ported from "Tomorrow at Midnight Min" in https://github.com/biaqat/tomorrow-min-theme-zed
tomorrowMidnightMinDarkTheme :: RgfwTheme
tomorrowMidnightMinDarkTheme =
  RgfwTheme
    { thBackground    = colorRGBA 0 0 0 255
    , thPanelBg       = colorRGBA 16 17 20 255
    , thBorder        = colorRGBA 48 52 70 255
    , thBorderFocused = colorRGBA 140 182 226 255
    , thText          = colorRGBA 238 238 238 255
    , thTextMuted     = colorRGBA 128 132 150 255
    , thPrimary       = colorRGBA 140 182 226 255
    , thPrimaryHover  = colorRGBA 164 200 238 255
    , thPrimaryActive = colorRGBA 116 158 203 255
    , thWidgetBg      = colorRGBA 26 27 34 255
    , thWidgetHover   = colorRGBA 40 42 54 255
    , thWidgetActive  = colorRGBA 56 60 81 255
    , thThumb         = colorRGBA 48 52 70 255
    , thThumbHover    = colorRGBA 68 73 96 255
    , thSelection     = colorRGBA 48 52 70 143
    , thWindowHeader  = colorRGBA 24 25 32 255
    , thScrollTrack   = colorRGBA 13 14 18 255
    , thScrollThumb   = colorRGBA 48 52 70 255
    }

defaultDarkTheme :: RgfwTheme
defaultDarkTheme = tomorrowNightMinDarkTheme

defaultLightTheme :: RgfwTheme
defaultLightTheme = tomorrowMinLightTheme
