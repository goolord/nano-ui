{-# LANGUAGE StrictData #-}

-- | Built-in theme presets and Base16 palette conversion.
module NanoUI.Internal.Style.Presets
  ( defaultTheme
  , defaultLightTheme
  , defaultThemeFor
  , tomorrowNightMinDarkTheme
  , tomorrowMinLightTheme
  , tomorrowMidnightMinDarkTheme
  , Base16 (..)
  , themeFromBase16
  , themeFromBase16Dark
  , themeFromBase16Light
  , base16TomorrowNight
  , base16TomorrowLight
  ) where

import NanoUI.Internal.Style.Theme
  ( Appearance
  , Style
  , Theme (..)
  , accentColor
  , buildThemePreset
  , flatStyle
  , lightDark
  , readableTone
  , withReadableTones
  )
import NanoUI.Internal.Types (Color, colorLuminance, colorRGBA, fadeAlpha, lerpColor)

-- | 'defaultLightTheme' for a light appearance, 'defaultTheme' otherwise.
defaultThemeFor :: Maybe Appearance -> Theme
defaultThemeFor = lightDark defaultLightTheme defaultTheme

-- | Neutral charcoal surfaces, warm text, and a blue selection accent.
defaultTheme :: Theme
defaultTheme =
  withReadableTones $
  let panelSurface =
        flatStyle
          (colorRGBA 34 34 38 255)
          (colorRGBA 236 234 230 255)
          (colorRGBA 54 54 62 255)
          (colorRGBA 34 34 38 255)
          (colorRGBA 30 30 34 255)
      greenCol = colorRGBA 104 168 124 255
      redCol = colorRGBA 252 165 165 255
   in Theme
        { themeWindow = colorRGBA 24 24 27 255
        , themePage = Nothing
        , themePanel = panelSurface
        , themeFloatingWindow = panelSurface
        , themePopup = panelSurface
        , themeButton =
            flatStyle
              (colorRGBA 52 52 58 255)
              (colorRGBA 248 247 245 255)
              (colorRGBA 74 76 84 255)
              (colorRGBA 68 70 78 255)
              (colorRGBA 42 42 48 255)
        , themeInput =
            flatStyle
              (colorRGBA 18 18 21 255)
              (colorRGBA 236 234 230 255)
              (colorRGBA 70 72 80 255)
              (colorRGBA 24 24 28 255)
              (colorRGBA 14 14 17 255)
        , themeSeparator = colorRGBA 62 64 72 255
        , themeAccent = colorRGBA 88 156 246 255
        , themeMuted = colorRGBA 176 172 164 255
        , themeRed = redCol
        , themeOrange = colorRGBA 216 140 72 255
        , themeYellow = colorRGBA 212 176 88 255
        , themeGreen = greenCol
        , themePurple = colorRGBA 176 140 220 255
        , themeSuccess = greenCol
        , themeWarning = colorRGBA 242 180 76 255
        , themeDanger = redCol
        , themeOverlayDim = colorRGBA 8 8 10 176
        , themeOnAccent = colorRGBA 255 255 255 255
        , themeSelection = fadeAlpha (colorRGBA 88 156 246 255) 115
        , themeFocusRing = colorRGBA 88 156 246 255
        , themeLink = colorRGBA 124 178 250 255
        , themeShadow = colorRGBA 0 0 0 72
        , themeDisabledFade = 0.55
        }

-- | Light counterpart of 'defaultTheme', for use with @followSystemTheme@.
defaultLightTheme :: Theme
defaultLightTheme =
  buildThemePreset
    defaultTheme
    ( flatStyle
        (colorRGBA 252 252 251 255)
        (colorRGBA 36 36 40 255)
        (colorRGBA 220 220 216 255)
        (colorRGBA 252 252 251 255)
        (colorRGBA 240 240 238 255)
    )
    $ \t ->
      t
        { themeWindow = colorRGBA 244 244 242 255
        , themeButton =
            flatStyle
              (colorRGBA 234 234 231 255)
              (colorRGBA 24 24 27 255)
              (colorRGBA 196 196 192 255)
              (colorRGBA 222 222 218 255)
              (colorRGBA 210 210 206 255)
        , themeInput =
            flatStyle
              (colorRGBA 255 255 255 255)
              (colorRGBA 36 36 40 255)
              (colorRGBA 190 190 186 255)
              (colorRGBA 250 250 249 255)
              (colorRGBA 255 255 255 255)
        , themeSeparator = colorRGBA 214 214 210 255
        , themeAccent = colorRGBA 37 99 235 255
        , themeMuted = colorRGBA 108 105 100 255
        , themeRed = colorRGBA 190 40 40 255
        , themeOrange = colorRGBA 184 82 14 255
        , themeYellow = colorRGBA 150 104 0 255
        , themeGreen = colorRGBA 30 128 70 255
        , themePurple = colorRGBA 128 70 190 255
        , themeWarning = colorRGBA 150 90 0 255
        , themeOverlayDim = colorRGBA 20 20 24 90
        , themeSelection = fadeAlpha (colorRGBA 37 99 235 255) 80
        , themeFocusRing = colorRGBA 37 99 235 255
        , themeLink = colorRGBA 29 78 216 255
        , themeShadow = colorRGBA 0 0 0 40
        }

-- | Ported from "Tomorrow Night Min" in https://github.com/biaqat/tomorrow-min-theme-zed
tomorrowNightMinDarkTheme :: Theme
tomorrowNightMinDarkTheme =
  buildThemePreset
    (accentColor accentCol defaultTheme)
    ( flatStyle
        (colorRGBA 30 31 33 255)
        (colorRGBA 234 234 234 255)
        edgeCol
        (colorRGBA 52 54 62 255)
        (colorRGBA 26 27 29 255)
    )
    $ \t ->
      t
        { themeWindow = windowCol
        , themeButton =
            flatStyle
              (colorRGBA 44 46 51 255)
              (colorRGBA 245 245 245 255)
              edgeCol
              (colorRGBA 69 74 83 255)
              (colorRGBA 28 29 32 255)
        , themeInput =
            flatStyle
              (colorRGBA 23 24 26 255)
              (colorRGBA 234 234 234 255)
              edgeCol
              (colorRGBA 29 30 33 255)
              (colorRGBA 19 20 22 255)
        , themeSeparator = colorRGBA 55 59 65 255
        , themeMuted = colorRGBA 150 152 150 255
        , themeRed = colorRGBA 204 102 102 255
        , themeOrange = colorRGBA 222 147 95 255
        , themeYellow = colorRGBA 240 198 116 255
        , themeGreen = colorRGBA 181 189 104 255
        , themePurple = colorRGBA 178 148 187 255
        , themeWarning = colorRGBA 240 198 116 255
        , themeOverlayDim = colorRGBA 0 0 0 160
        , themeLink = accentCol
        }
  where
    windowCol = colorRGBA 23 24 26 255
    edgeCol = colorRGBA 77 80 87 255
    accentCol = colorRGBA 103 150 230 255

-- | Ported from "Tomorrow Min" in https://github.com/biaqat/tomorrow-min-theme-zed
tomorrowMinLightTheme :: Theme
tomorrowMinLightTheme =
  buildThemePreset
    (accentColor (colorRGBA 82 134 188 255) defaultLightTheme)
    ( flatStyle
        (colorRGBA 242 242 242 255)
        (colorRGBA 55 59 65 255)
        (colorRGBA 222 222 222 255)
        (colorRGBA 231 231 231 255)
        (colorRGBA 219 219 219 255)
    )
    $ \t ->
      t
        { themeWindow = colorRGBA 255 255 255 255
        , themeButton =
            flatStyle
              (colorRGBA 232 232 232 255)
              (colorRGBA 55 59 65 255)
              (colorRGBA 214 214 214 255)
              (colorRGBA 214 214 214 255)
              (colorRGBA 196 196 196 255)
        , themeInput =
            flatStyle
              (colorRGBA 255 255 255 255)
              (colorRGBA 55 59 65 255)
              (colorRGBA 210 210 210 255)
              (colorRGBA 243 243 243 255)
              (colorRGBA 255 255 255 255)
        , themeSeparator = colorRGBA 222 222 222 255
        , themeMuted = colorRGBA 140 140 140 255
        , themeRed = colorRGBA 197 78 82 255
        , themeOrange = colorRGBA 231 140 69 255
        , themeYellow = colorRGBA 231 197 71 255
        , themeGreen = colorRGBA 113 140 0 255
        , themePurple = colorRGBA 137 91 144 255
        , themeWarning = colorRGBA 150 94 0 255
        , themeOverlayDim = colorRGBA 0 0 0 100
        , themeSelection = fadeAlpha (colorRGBA 82 134 188 255) 80
        , themeLink = colorRGBA 66 113 174 255
        , themeShadow = colorRGBA 0 0 0 36
        }

-- | Ported from "Tomorrow at Midnight Min" in https://github.com/biaqat/tomorrow-min-theme-zed
tomorrowMidnightMinDarkTheme :: Theme
tomorrowMidnightMinDarkTheme =
  buildThemePreset
    (accentColor accentCol defaultTheme)
    ( flatStyle
        (colorRGBA 16 17 20 255)
        (colorRGBA 238 238 238 255)
        edgeCol
        (colorRGBA 46 48 56 255)
        (colorRGBA 12 13 15 255)
    )
    $ \t ->
      t
        { themeWindow = colorRGBA 0 0 0 255
        , themeButton =
            flatStyle
              (colorRGBA 26 27 34 255)
              (colorRGBA 238 238 238 255)
              edgeCol
              (colorRGBA 54 58 72 255)
              (colorRGBA 56 60 81 255)
        , themeInput =
            flatStyle
              (colorRGBA 13 14 18 255)
              (colorRGBA 238 238 238 255)
              edgeCol
              (colorRGBA 21 22 28 255)
              (colorRGBA 8 9 11 255)
        , themeSeparator = edgeCol
        , themeMuted = colorRGBA 128 132 150 255
        , themeRed = colorRGBA 213 78 83 255
        , themeOrange = colorRGBA 231 140 69 255
        , themeYellow = colorRGBA 231 197 71 255
        , themeGreen = colorRGBA 185 202 74 255
        , themePurple = colorRGBA 195 151 216 255
        , themeWarning = colorRGBA 231 197 71 255
        , themeOverlayDim = colorRGBA 0 0 0 160
        , themeLink = accentCol
        , themeShadow = colorRGBA 0 0 0 96
        }
  where
    edgeCol = colorRGBA 48 52 70 255
    accentCol = colorRGBA 140 182 226 255

-- | Standard Base16 palette containing 16 styling tones and syntax colours.
data Base16 = Base16
  { base00 :: {-# UNPACK #-} !Color -- ^ Default Background
  , base01 :: {-# UNPACK #-} !Color -- ^ Lighter Background
  , base02 :: {-# UNPACK #-} !Color -- ^ Selection Background
  , base03 :: {-# UNPACK #-} !Color -- ^ Comments, Invisibles, Borders
  , base04 :: {-# UNPACK #-} !Color -- ^ Dark Foreground
  , base05 :: {-# UNPACK #-} !Color -- ^ Default Foreground
  , base06 :: {-# UNPACK #-} !Color -- ^ Light Foreground
  , base07 :: {-# UNPACK #-} !Color -- ^ Light Background / Highest contrast foreground
  , base08 :: {-# UNPACK #-} !Color -- ^ Red
  , base09 :: {-# UNPACK #-} !Color -- ^ Orange
  , base0A :: {-# UNPACK #-} !Color -- ^ Yellow
  , base0B :: {-# UNPACK #-} !Color -- ^ Green
  , base0C :: {-# UNPACK #-} !Color -- ^ Cyan
  , base0D :: {-# UNPACK #-} !Color -- ^ Blue / Primary Accent
  , base0E :: {-# UNPACK #-} !Color -- ^ Purple / Magenta
  , base0F :: {-# UNPACK #-} !Color -- ^ Brown / Deprecated
  }
  deriving (Eq, Show)

-- | Calculate a 'Theme' from a 'Base16' colorscheme, choosing dark or light
-- styling from background vs foreground luminance.
themeFromBase16 :: Base16 -> Theme
themeFromBase16 b
  | isDark = themeFromBase16Dark b
  | otherwise = themeFromBase16Light b
  where
    isDark = colorLuminance (base00 b) < colorLuminance (base05 b)

-- | Calculate a dark 'Theme' from a 'Base16' colorscheme.
themeFromBase16Dark :: Base16 -> Theme
themeFromBase16Dark = themeFromBase16Mode True

-- | Calculate a light 'Theme' from a 'Base16' colorscheme.
themeFromBase16Light :: Base16 -> Theme
themeFromBase16Light = themeFromBase16Mode False

themeFromBase16Mode :: Bool -> Base16 -> Theme
themeFromBase16Mode dark b =
  let pick :: a -> a -> a
      pick night day = if dark then night else day
      edgeCol = pick (lerpColor (base02 b) (base03 b) 0.35) (base02 b)
      panelBg =
        pick (lerpColor (base01 b) (base02 b) 0.3) (lerpColor (base00 b) (base01 b) 0.5)
      panelSurface :: Style
      panelSurface =
        flatStyle
          panelBg
          (base05 b)
          edgeCol
          (pick (lerpColor panelBg (base02 b) 0.5) (lerpColor panelBg (base00 b) 0.4))
          (lerpColor panelBg (pick (base00 b) (base02 b)) 0.4)
      readable = readableTone (base00 b)
   in buildThemePreset (accentColor (base0D b) (pick defaultTheme defaultLightTheme)) panelSurface $ \t ->
        t
          { themeWindow = base00 b
          , themeButton =
              flatStyle
                (pick (base02 b) (base01 b))
                (pick (base07 b) (base05 b))
                edgeCol
                (pick (lerpColor (base02 b) (base03 b) 0.4) (base02 b))
                (pick (base01 b) (lerpColor (base02 b) (base03 b) 0.35))
          , themeInput =
              flatStyle
                (base00 b)
                (base05 b)
                edgeCol
                (pick (base01 b) (lerpColor (base00 b) (base01 b) 0.3))
                (pick (base00 b) (lerpColor (base00 b) (base01 b) 0.6))
          , themeSeparator = edgeCol
          , themeMuted = base03 b
          , themeRed = base08 b
          , themeOrange = base09 b
          , themeYellow = base0A b
          , themeGreen = base0B b
          , themePurple = base0E b
          , themeWarning = readable (pick (base0A b) (base09 b))
          , themeOverlayDim = colorRGBA 0 0 0 (pick 160 100)
          , themeOnAccent =
              if colorLuminance (base0D b) > 0.6
                then pick (base00 b) (base07 b)
                else colorRGBA 255 255 255 255
          , themeSelection = fadeAlpha (base0D b) (pick 115 80)
          , themeLink = base0D b
          , themeShadow = colorRGBA 0 0 0 (pick 72 36)
          }

-- | Tomorrow Night Base16 reference palette.
base16TomorrowNight :: Base16
base16TomorrowNight =
  Base16
    { base00 = colorRGBA 29 31 33 255
    , base01 = colorRGBA 40 42 46 255
    , base02 = colorRGBA 55 59 65 255
    , base03 = colorRGBA 150 152 150 255
    , base04 = colorRGBA 180 183 180 255
    , base05 = colorRGBA 197 200 198 255
    , base06 = colorRGBA 224 224 224 255
    , base07 = colorRGBA 255 255 255 255
    , base08 = colorRGBA 213 78 83 255
    , base09 = colorRGBA 231 140 69 255
    , base0A = colorRGBA 231 197 71 255
    , base0B = colorRGBA 185 202 74 255
    , base0C = colorRGBA 112 192 186 255
    , base0D = colorRGBA 103 150 230 255
    , base0E = colorRGBA 195 151 216 255
    , base0F = colorRGBA 163 104 90 255
    }

-- | Tomorrow Light Base16 reference palette.
base16TomorrowLight :: Base16
base16TomorrowLight =
  Base16
    { base00 = colorRGBA 255 255 255 255
    , base01 = colorRGBA 242 242 242 255
    , base02 = colorRGBA 222 222 222 255
    , base03 = colorRGBA 140 140 140 255
    , base04 = colorRGBA 150 152 150 255
    , base05 = colorRGBA 55 59 65 255
    , base06 = colorRGBA 40 42 46 255
    , base07 = colorRGBA 29 31 33 255
    , base08 = colorRGBA 197 78 82 255
    , base09 = colorRGBA 231 140 69 255
    , base0A = colorRGBA 231 197 71 255
    , base0B = colorRGBA 113 140 0 255
    , base0C = colorRGBA 62 153 159 255
    , base0D = colorRGBA 82 134 188 255
    , base0E = colorRGBA 137 91 144 255
    , base0F = colorRGBA 163 104 90 255
    }
