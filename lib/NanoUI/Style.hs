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
  ) where

import NanoUI.Types (Color, colorRGBA)

data Sizing
  = Fixed Float
  | Fit
  | Grow Float
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
  { themePanel :: Style
  , themeButton :: Style
  , themeInput :: Style
  , themeSeparator :: Color
  , themeAccent :: Color
  }
  deriving (Eq, Show)

defaultTheme :: Theme
defaultTheme =
  Theme
    { themePanel =
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
          { styleBg = colorRGBA 30 30 32 255
          , styleFg = colorRGBA 240 240 240 255
          , styleBorder = colorRGBA 80 80 85 255
          , styleBorderWidth = 1
          , styleCornerRadius = 3
          , styleHoverBg = colorRGBA 35 35 38 255
          , styleActiveBg = colorRGBA 25 25 28 255
        }
    , themeSeparator = colorRGBA 80 80 85 255
    , themeAccent = colorRGBA 100 149 237 255
    }
