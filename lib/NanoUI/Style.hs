{-# LANGUAGE StrictData #-}

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
  , panelPaintPad
  , padAll
  , padXY
  , gap
  , fillW
  , fillH
  , grow
  , minW
  , fixedH
  , fixedWH
  , alignMid
  , wrap
  , tight
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
  { padL :: {-# UNPACK #-} !Float
  , padR :: {-# UNPACK #-} !Float
  , padT :: {-# UNPACK #-} !Float
  , padB :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

-- Containers fill with themePanel when any pad side is at least this.
panelPaintPad :: Float
panelPaintPad = 12

data Layout = Layout
  { layoutDirection :: !Direction
  , layoutWidth :: !Sizing
  , layoutHeight :: !Sizing
  , layoutPadding :: !Padding
  , layoutGap :: {-# UNPACK #-} !Float
  , layoutWrap :: !Bool
  , layoutAlignX :: !AlignX
  , layoutAlignY :: !AlignY
  , layoutMinW :: {-# UNPACK #-} !Float
  , layoutMinH :: {-# UNPACK #-} !Float
  , layoutMaxW :: {-# UNPACK #-} !Float
  , layoutMaxH :: {-# UNPACK #-} !Float
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

{-# INLINE padAll #-}
padAll :: Float -> Layout -> Layout
padAll n l = l {layoutPadding = Padding n n n n}

{-# INLINE padXY #-}
padXY :: Float -> Float -> Layout -> Layout
padXY x y l = l {layoutPadding = Padding x x y y}

{-# INLINE gap #-}
gap :: Float -> Layout -> Layout
gap n l = l {layoutGap = n}

{-# INLINE fillW #-}
fillW :: Layout -> Layout
fillW l = l {layoutWidth = Grow 1}

{-# INLINE fillH #-}
fillH :: Layout -> Layout
fillH l = l {layoutHeight = Grow 1}

{-# INLINE grow #-}
grow :: Layout -> Layout
grow = fillW . fillH

{-# INLINE minW #-}
minW :: Float -> Layout -> Layout
minW n l = l {layoutMinW = n}

{-# INLINE fixedH #-}
fixedH :: Float -> Layout -> Layout
fixedH n l = l {layoutHeight = Fixed n}

{-# INLINE fixedWH #-}
fixedWH :: Float -> Float -> Layout -> Layout
fixedWH w h l = l {layoutWidth = Fixed w, layoutHeight = Fixed h}

{-# INLINE alignMid #-}
alignMid :: Layout -> Layout
alignMid l = l {layoutAlignY = AlignMiddle}

{-# INLINE wrap #-}
wrap :: Layout -> Layout
wrap l = l {layoutWrap = True}

{-# INLINE tight #-}
tight :: Layout -> Layout
tight l = l {layoutPadding = Padding 0 0 0 0}

data Style = Style
  { styleBg :: !Color
  , styleFg :: !Color
  , styleBorder :: !Color
  , styleBorderWidth :: {-# UNPACK #-} !Float
  , styleCornerRadius :: {-# UNPACK #-} !Float
  , styleHoverBg :: !Color
  , styleActiveBg :: !Color
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

-- Charcoal tiling surfaces (Untitled.png) with Adwaita-dark widget chrome.
sdlTheme :: Theme
sdlTheme =
  Theme
    { themeWindow = colorRGBA 29 29 29 255
    , themePanel =
        Style
          { styleBg = colorRGBA 37 37 38 255
          , styleFg = colorRGBA 232 230 227 255
          , styleBorder = colorRGBA 68 70 78 255
          , styleBorderWidth = 1
          , styleCornerRadius = 10
          , styleHoverBg = colorRGBA 37 37 38 255
          , styleActiveBg = colorRGBA 32 32 33 255
          }
    , themeButton =
        Style
          { styleBg = colorRGBA 58 58 61 255
          , styleFg = colorRGBA 246 245 244 255
          , styleBorder = colorRGBA 78 78 82 255
          , styleBorderWidth = 1
          , styleCornerRadius = 9
          , styleHoverBg = colorRGBA 74 74 78 255
          , styleActiveBg = colorRGBA 46 46 49 255
          }
    , themeInput =
        Style
          { styleBg = colorRGBA 20 20 21 255
          , styleFg = colorRGBA 232 230 227 255
          , styleBorder = colorRGBA 74 74 78 255
          , styleBorderWidth = 1
          , styleCornerRadius = 9
          , styleHoverBg = colorRGBA 26 26 27 255
          , styleActiveBg = colorRGBA 16 16 17 255
          }
    , themeSeparator = colorRGBA 88 90 96 255
    , themeAccent = colorRGBA 53 132 228 255
    , themeOverlayDim = colorRGBA 0 0 0 168
    }

defaultTheme :: Theme
defaultTheme = sdlTheme

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
