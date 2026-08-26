module NanoUI.Style
  ( Sizing (..)
  , Direction (..)
  , AlignX (..)
  , AlignY (..)
  , Padding (..)
  , Layout (..)
  , defaultLayout
  , Style (..)
  , defaultStyle
  , Theme (..)
  , defaultTheme
  , terminalTheme
  , sdlTheme
  ) where

import NanoUI.Types (Color, colorRGBA)

data Sizing
  = Fixed Float
  | Fit
  | Grow Float
  | Shrink Float
  | Percent Float
  deriving (Eq, Show)

data Direction = Row | Column
  deriving (Eq, Show, Enum, Bounded)

data AlignX = AlignStart | AlignCenter | AlignEnd
  deriving (Eq, Show, Enum, Bounded)

data AlignY = AlignTop | AlignMiddle | AlignBottom
  deriving (Eq, Show, Enum, Bounded)

data Padding = Padding
  { padL :: Float
  , padR :: Float
  , padT :: Float
  , padB :: Float
  }
  deriving (Eq, Show)

data Layout = Layout
  { layoutDirection :: Direction
  , layoutWidth :: Sizing
  , layoutHeight :: Sizing
  , layoutPadding :: Padding
  , layoutGap :: Float
  , layoutWrap :: Bool
  , layoutAlignX :: AlignX
  , layoutAlignY :: AlignY
  , layoutMinW :: Float
  , layoutMinH :: Float
  , layoutMaxW :: Float
  , layoutMaxH :: Float
  }
  deriving (Eq, Show)

defaultLayout :: Layout
defaultLayout =
  Layout
    { layoutDirection = Column
    , layoutWidth = Fit
    , layoutHeight = Fit
    , layoutPadding = Padding 4 4 4 4
    , layoutGap = 4
    , layoutWrap = False
    , layoutAlignX = AlignStart
    , layoutAlignY = AlignTop
    , layoutMinW = 0
    , layoutMinH = 0
    , layoutMaxW = 1e9
    , layoutMaxH = 1e9
    }

data Style = Style
  { styleBg :: Color
  , styleFg :: Color
  , styleBorder :: Color
  , styleBorderWidth :: Float
  , styleCornerRadius :: Float
  , styleHoverBg :: Color
  , styleActiveBg :: Color
  }
  deriving (Eq, Show)

defaultStyle :: Style
defaultStyle =
  Style
    { styleBg = colorRGBA 60 60 60 255
    , styleFg = colorRGBA 240 240 240 255
    , styleBorder = colorRGBA 100 100 100 255
    , styleBorderWidth = 1
    , styleCornerRadius = 4
    , styleHoverBg = colorRGBA 80 80 80 255
    , styleActiveBg = colorRGBA 40 40 40 255
    }

data Theme = Theme
  { themeWindow :: Color
  , themePanel :: Style
  , themeButton :: Style
  , themeInput :: Style
  , themeSeparator :: Color
  , themeAccent :: Color
  , themeOverlayDim :: Color
  }
  deriving (Eq, Show)

defaultTheme :: Theme
defaultTheme =
  Theme
    { themeWindow = colorRGBA 33 33 36 255
    , themePanel =
        Style
          { styleBg = colorRGBA 45 45 48 255
          , styleFg = colorRGBA 240 240 240 255
          , styleBorder = colorRGBA 70 70 70 255
          , styleBorderWidth = 1
          , styleCornerRadius = 6
          , styleHoverBg = colorRGBA 55 55 58 255
          , styleActiveBg = colorRGBA 35 35 38 255
        }
    , themeButton =
        Style
          { styleBg = colorRGBA 70 70 75 255
          , styleFg = colorRGBA 240 240 240 255
          , styleBorder = colorRGBA 90 90 95 255
          , styleBorderWidth = 1
          , styleCornerRadius = 4
          , styleHoverBg = colorRGBA 90 90 95 255
          , styleActiveBg = colorRGBA 50 50 55 255
        }
    , themeInput =
        Style
          { styleBg = colorRGBA 22 22 26 255
          , styleFg = colorRGBA 240 240 240 255
          , styleBorder = colorRGBA 100 105 115 255
          , styleBorderWidth = 1
          , styleCornerRadius = 4
          , styleHoverBg = colorRGBA 28 28 32 255
          , styleActiveBg = colorRGBA 18 18 22 255
          }
    , themeSeparator = colorRGBA 80 80 85 255
    , themeAccent = colorRGBA 100 149 237 255
    , themeOverlayDim = colorRGBA 0 0 0 120
    }

-- Dracula-inspired palette similar to dvui's default dark themes.
sdlTheme :: Theme
sdlTheme =
  Theme
    { themeWindow = colorRGBA 33 34 44 255
    , themePanel =
        Style
          { styleBg = colorRGBA 40 42 54 255
          , styleFg = colorRGBA 248 248 242 255
          , styleBorder = colorRGBA 98 114 164 255
          , styleBorderWidth = 0
          , styleCornerRadius = 8
          , styleHoverBg = colorRGBA 40 42 54 255
          , styleActiveBg = colorRGBA 40 42 54 255
          }
    , themeButton =
        Style
          { styleBg = colorRGBA 68 71 90 255
          , styleFg = colorRGBA 248 248 242 255
          , styleBorder = colorRGBA 98 114 164 255
          , styleBorderWidth = 0
          , styleCornerRadius = 6
          , styleHoverBg = colorRGBA 98 114 164 255
          , styleActiveBg = colorRGBA 255 121 198 255
          }
    , themeInput =
        Style
          { styleBg = colorRGBA 25 26 36 255
          , styleFg = colorRGBA 248 248 242 255
          , styleBorder = colorRGBA 98 114 164 255
          , styleBorderWidth = 1
          , styleCornerRadius = 4
          , styleHoverBg = colorRGBA 32 34 46 255
          , styleActiveBg = colorRGBA 20 21 30 255
          }
    , themeSeparator = colorRGBA 98 114 164 96
    , themeAccent = colorRGBA 255 121 198 255
    , themeOverlayDim = colorRGBA 0 0 0 120
    }

terminalTheme :: Theme
terminalTheme =
  Theme
    { themeWindow = colorRGBA 18 18 22 255
    , themePanel =
        Style
          { styleBg = colorRGBA 28 28 32 255
          , styleFg = colorRGBA 230 230 235 255
          , styleBorder = colorRGBA 140 145 160 255
          , styleBorderWidth = 1
          , styleCornerRadius = 0
          , styleHoverBg = colorRGBA 38 38 44 255
          , styleActiveBg = colorRGBA 22 22 26 255
        }
    , themeButton =
        Style
          { styleBg = colorRGBA 55 60 72 255
          , styleFg = colorRGBA 240 240 245 255
          , styleBorder = colorRGBA 160 165 180 255
          , styleBorderWidth = 1
          , styleCornerRadius = 0
          , styleHoverBg = colorRGBA 75 80 95 255
          , styleActiveBg = colorRGBA 40 45 55 255
        }
    , themeInput =
        Style
          { styleBg = colorRGBA 14 14 18 255
          , styleFg = colorRGBA 240 240 245 255
          , styleBorder = colorRGBA 120 125 140 255
          , styleBorderWidth = 1
          , styleCornerRadius = 0
          , styleHoverBg = colorRGBA 20 20 24 255
          , styleActiveBg = colorRGBA 10 10 14 255
          }
    , themeSeparator = colorRGBA 100 105 120 255
    , themeAccent = colorRGBA 90 160 255 255
    , themeOverlayDim = colorRGBA 0 0 0 80
    }
