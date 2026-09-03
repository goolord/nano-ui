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
  , Theme (..)
  , defaultTheme
  , themeSeries
  , scrollBarTrackColor
  , scrollBarThumbColor
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
  , aspect
  , FontVariant (..)
  , LayoutModifier
  , fontRegular
  , fontHeading
  , fontMuted
  , fontMono
  , alignStart
  , alignCenter
  , alignTop
  , alignBottom
  ) where

import Data.Bits ((.&.), (.|.))
import Data.Word (Word8)
import NanoUI.Types (Color (..), colorRGBA, lerpColor)

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
windowPad = Padding 10 10 0 10

-- Screen inset for floating window/modal max size and default placement.
windowMargin :: Float
windowMargin = 16

data FontVariant
  = FontRegular
  | FontHeading
  | FontMuted
  | FontMono
  deriving (Eq, Show, Enum, Bounded, Ord)

type LayoutModifier = Layout -> Layout

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
  , layoutFontVariant :: !FontVariant
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
    , layoutFontVariant = FontRegular
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

-- Width over height. After width is known, height becomes width / ratio.
{-# INLINE aspect #-}
aspect :: Float -> Layout -> Layout
aspect r l = l {layoutAspect = r}

{-# INLINE fontRegular #-}
fontRegular :: Layout -> Layout
fontRegular l = l {layoutFontVariant = FontRegular}

{-# INLINE fontHeading #-}
fontHeading :: Layout -> Layout
fontHeading l = l {layoutFontVariant = FontHeading}

{-# INLINE fontMuted #-}
fontMuted :: Layout -> Layout
fontMuted l = l {layoutFontVariant = FontMuted}

{-# INLINE fontMono #-}
fontMono :: Layout -> Layout
fontMono l = l {layoutFontVariant = FontMono}

{-# INLINE alignStart #-}
alignStart :: Layout -> Layout
alignStart l = l {layoutAlignX = AlignStart}

{-# INLINE alignCenter #-}
alignCenter :: Layout -> Layout
alignCenter l = l {layoutAlignX = AlignCenter}

{-# INLINE alignTop #-}
alignTop :: Layout -> Layout
alignTop l = l {layoutAlignY = AlignTop}

{-# INLINE alignBottom #-}
alignBottom :: Layout -> Layout
alignBottom l = l {layoutAlignY = AlignBottom}

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

data Theme = Theme
  { themeWindow :: Color
  , themePanel :: Style
  , themeFloatingWindow :: Style
  , themeButton :: Style
  , themeInput :: Style
  , themeSeparator :: Color
  , themeAccent :: Color
  , themeMuted :: Color
  , themeRed :: Color
  , themeOrange :: Color
  , themeYellow :: Color
  , themeGreen :: Color
  , themePurple :: Color
  , themeOverlayDim :: Color
  }
  deriving (Eq, Show)

-- Charcoal tiling surfaces (Untitled.png) with Adwaita-dark widget chrome.
defaultTheme :: Theme
defaultTheme =
  let panelStyle =
        Style
          { styleBg = colorRGBA 34 34 38 255
          , styleFg = colorRGBA 236 234 230 255
          , styleBorder = colorRGBA 62 64 72 255
          , styleBorderWidth = 1
          , styleCornerRadius = 2
          , styleHoverBg = colorRGBA 34 34 38 255
          , styleActiveBg = colorRGBA 30 30 34 255
          }
   in Theme
        { themeWindow = colorRGBA 24 24 27 255
        , themePanel = panelStyle
        , themeFloatingWindow = panelStyle
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
        , themeRed = colorRGBA 204 102 102 255
        , themeOrange = colorRGBA 216 140 72 255
        , themeYellow = colorRGBA 212 176 88 255
        , themeGreen = colorRGBA 104 168 124 255
        , themePurple = colorRGBA 176 140 220 255
        , themeOverlayDim = colorRGBA 8 8 10 176
        }

-- Status and series colours in hue order, then accent.
themeSeries :: Theme -> [Color]
themeSeries t =
  [ themeRed t
  , themeOrange t
  , themeYellow t
  , themeGreen t
  , themeAccent t
  , themePurple t
  ]

-- Scroll track/thumb tints. Cell hosts use opaque theme mixes so light palettes
-- stay visible on floating windows; SDL keeps the old translucent overlay.
scrollBarTrackColor :: Style -> Theme -> Bool -> Color
scrollBarTrackColor base theme terminal =
  let solid = lerpColor (styleBg base) (themeSeparator theme) 0.28
   in if terminal then solid else fadeAlpha solid 20

scrollBarThumbColor :: Style -> Theme -> Bool -> Color
scrollBarThumbColor base theme terminal =
  let solid = lerpColor (themeSeparator theme) (styleFg base) 0.58
   in if terminal then solid else fadeAlpha solid 130

fadeAlpha :: Color -> Word8 -> Color
fadeAlpha (Color w) a = Color ((w .&. 0xFFFFFF00) .|. fromIntegral a)
