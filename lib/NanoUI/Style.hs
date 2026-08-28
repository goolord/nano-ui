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
  , windowPad
  , windowMargin
  , padAll
  , padXY
  , gap
  , fillW
  , fillH
  , grow
  , minW
  , fixedW
  , fixedH
  , fixedWH
  , alignMid
  , alignEnd
  , wrap
  , tight
  , percent
  , percentH
  , aspect
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
panelPaintPad = 8

-- Floating window chrome.
windowPad :: Padding
windowPad = Padding 10 10 7 9

-- Screen inset for floating window/modal max size and default placement.
windowMargin :: Float
windowMargin = 16

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
  , layoutAspect :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

defaultLayout :: Layout
defaultLayout =
  Layout
    { layoutDirection = Column
    , layoutWidth = Fit
    , layoutHeight = Fit
    , layoutPadding = Padding 3 3 3 3
    , layoutGap = 4
    , layoutWrap = False
    , layoutAlignX = AlignStart
    , layoutAlignY = AlignTop
    , layoutMinW = 0
    , layoutMinH = 0
    , layoutMaxW = 1e9
    , layoutMaxH = 1e9
    , layoutAspect = 0
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

{-# INLINE fixedW #-}
fixedW :: Float -> Layout -> Layout
fixedW n l = l {layoutWidth = Fixed n, layoutMinW = n, layoutMaxW = n}

{-# INLINE fixedH #-}
fixedH :: Float -> Layout -> Layout
fixedH n l = l {layoutHeight = Fixed n}

{-# INLINE fixedWH #-}
fixedWH :: Float -> Float -> Layout -> Layout
fixedWH w h l = l {layoutWidth = Fixed w, layoutHeight = Fixed h}

{-# INLINE alignMid #-}
alignMid :: Layout -> Layout
alignMid l = l {layoutAlignY = AlignMiddle}

{-# INLINE alignEnd #-}
alignEnd :: Layout -> Layout
alignEnd l = l {layoutAlignX = AlignEnd}

{-# INLINE wrap #-}
wrap :: Layout -> Layout
wrap l = l {layoutWrap = True}

{-# INLINE tight #-}
tight :: Layout -> Layout
tight l = l {layoutPadding = Padding 0 0 0 0}

{-# INLINE percent #-}
percent :: Float -> Layout -> Layout
percent p l = l {layoutWidth = Percent p}

{-# INLINE percentH #-}
percentH :: Float -> Layout -> Layout
percentH p l = l {layoutHeight = Percent p}

-- Width over height. After width is known, height becomes width / ratio.
{-# INLINE aspect #-}
aspect :: Float -> Layout -> Layout
aspect r l = l {layoutAspect = r}

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
    , styleCornerRadius = 2
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
  , themeMuted :: Color
  , themeOverlayDim :: Color
  }
  deriving (Eq, Show)

-- Charcoal tiling surfaces (Untitled.png) with Adwaita-dark widget chrome.
sdlTheme :: Theme
sdlTheme =
  Theme
    { themeWindow = colorRGBA 24 24 27 255
    , themePanel =
        Style
          { styleBg = colorRGBA 34 34 38 255
          , styleFg = colorRGBA 236 234 230 255
          , styleBorder = colorRGBA 62 64 72 255
          , styleBorderWidth = 1
          , styleCornerRadius = 2
          , styleHoverBg = colorRGBA 34 34 38 255
          , styleActiveBg = colorRGBA 30 30 34 255
          }
    , themeButton =
        Style
          { styleBg = colorRGBA 52 52 58 255
          , styleFg = colorRGBA 248 247 245 255
          , styleBorder = colorRGBA 74 76 84 255
          , styleBorderWidth = 1
          , styleCornerRadius = 2
          , styleHoverBg = colorRGBA 68 70 78 255
          , styleActiveBg = colorRGBA 42 42 48 255
          }
    , themeInput =
        Style
          { styleBg = colorRGBA 18 18 21 255
          , styleFg = colorRGBA 236 234 230 255
          , styleBorder = colorRGBA 70 72 80 255
          , styleBorderWidth = 1
          , styleCornerRadius = 2
          , styleHoverBg = colorRGBA 24 24 28 255
          , styleActiveBg = colorRGBA 14 14 17 255
          }
    , themeSeparator = colorRGBA 78 80 88 255
    , themeAccent = colorRGBA 88 156 246 255
    , themeMuted = colorRGBA 176 172 164 255
    , themeOverlayDim = colorRGBA 8 8 10 176
    }

defaultTheme :: Theme
defaultTheme = sdlTheme

-- Dusk ink, bone text, copper headings. Tuned for cell-grid contrast, not SDL chrome.
terminalTheme :: Theme
terminalTheme =
  Theme
    { themeWindow = colorRGBA 16 18 28 255
    , themePanel =
        Style
          { styleBg = colorRGBA 26 30 44 255
          , styleFg = colorRGBA 236 228 210 255
          , styleBorder = colorRGBA 92 100 122 255
          , styleBorderWidth = 1
          , styleCornerRadius = 0
          , styleHoverBg = colorRGBA 36 42 60 255
          , styleActiveBg = colorRGBA 20 24 36 255
          }
    , themeButton =
        Style
          { styleBg = colorRGBA 42 48 68 255
          , styleFg = colorRGBA 236 228 210 255
          , styleBorder = colorRGBA 110 118 140 255
          , styleBorderWidth = 1
          , styleCornerRadius = 0
          , styleHoverBg = colorRGBA 58 66 90 255
          , styleActiveBg = colorRGBA 32 38 54 255
          }
    , themeInput =
        Style
          { styleBg = colorRGBA 12 14 22 255
          , styleFg = colorRGBA 236 228 210 255
          , styleBorder = colorRGBA 92 100 122 255
          , styleBorderWidth = 1
          , styleCornerRadius = 0
          , styleHoverBg = colorRGBA 20 24 36 255
          , styleActiveBg = colorRGBA 10 12 18 255
          }
    , themeSeparator = colorRGBA 92 100 122 255
    , themeAccent = colorRGBA 214 154 96 255
    , themeMuted = colorRGBA 184 176 160 255
    , themeOverlayDim = colorRGBA 8 10 16 186
    }
