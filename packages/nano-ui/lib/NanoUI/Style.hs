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
  , tomorrowNightMinTheme
  , tomorrowNightMinDarkTheme
  , tomorrowLightTheme
  , tomorrowMinLightTheme
  , tomorrowMidnightMinTheme
  , tomorrowMidnightMinDarkTheme
  , Base16 (..)
  , Base16ColorScheme
  , base0a
  , base0b
  , base0c
  , base0d
  , base0e
  , base0f
  , themeFromBase16
  , themeFromBase16Dark
  , themeFromBase16Light
  , base16Theme
  , base16ToTheme
  , base16TomorrowNight
  , base16TomorrowLight
  , packPanelStyle
  , unpackPanelStyle
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
  , maxW
  , fixedW
  , minH
  , maxH
  , fixedH
  , fixedWH
  , alignMid
  , alignEnd
  , tight
  , percent
  , gridMinColW
  , fixedAspectW
  , fixedAspectH
  , gridCols
  , cols
  , FontVariant (..)
  , FontWeight (..)
  , FontStyle (..)
  , TextDecoration (..)
  , LayoutModifier
  , fontRegular
  , fontHeading
  , fontMuted
  , fontMono
  , fontDanger
  , fontSize
  , fontSizeScale
  , fontColor
  , textColor
  , fontWeight
  , fontBold
  , fontLight
  , fontMedium
  , fontSemiBold
  , fontExtraBold
  , fontBlack
  , fontStyle
  , fontItalic
  , fontOblique
  , textDecoration
  , fontUnderline
  , fontStrike
  , fontStrikethrough
  , alignStart
  , alignCenter
  , alignTop
  , alignBottom
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Word (Word8, Word32, Word64)
import NanoUI.Types (Color (..), colorLuminance, colorRGBA, lerpColor)

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
windowPad = Padding 10 10 0 2

-- Screen inset for floating window/modal max size and default placement.
windowMargin :: Float
windowMargin = 16

data FontVariant
  = FontRegular
  | FontHeading
  | FontMuted
  | FontMono
  | FontDanger
  deriving (Eq, Show, Enum, Bounded, Ord)

data FontWeight
  = WeightNormal
  | WeightBold
  | WeightLight
  | WeightMedium
  | WeightSemiBold
  | WeightExtraBold
  | WeightBlack
  deriving (Eq, Show, Enum, Bounded, Ord)

data FontStyle
  = FontStyleNormal
  | FontStyleItalic
  | FontStyleOblique
  deriving (Eq, Show, Enum, Bounded, Ord)

data TextDecoration
  = DecorationNone
  | DecorationUnderline
  | DecorationStrikethrough
  | DecorationUnderlineStrike
  deriving (Eq, Show, Enum, Bounded, Ord)

type LayoutModifier = Layout -> Layout

data Layout = Layout
  { layoutDirection :: !Direction
  , layoutWidth :: !Sizing
  , layoutHeight :: !Sizing
  , layoutPadding :: !Padding
  , layoutGap :: {-# UNPACK #-} !Float
  , layoutAlignX :: !AlignX
  , layoutAlignY :: !AlignY
  , layoutMinW :: {-# UNPACK #-} !Float
  , layoutMinH :: {-# UNPACK #-} !Float
  , layoutMaxW :: {-# UNPACK #-} !Float
  , layoutMaxH :: {-# UNPACK #-} !Float
  , layoutFontVariant :: !FontVariant
  , layoutGridCols :: {-# UNPACK #-} !Int
  , layoutGridMinColW :: {-# UNPACK #-} !Float
  , layoutFontSize :: {-# UNPACK #-} !Float
  , layoutFontColor :: !(Maybe Color)
  , layoutFontWeight :: !FontWeight
  , layoutFontStyle :: !FontStyle
  , layoutTextDecoration :: !TextDecoration
  }
  deriving (Eq, Show)

defaultLayout :: Layout
defaultLayout =
  Layout
    { layoutDirection = Column
    , layoutWidth = Fit
    , layoutHeight = Fit
    , layoutPadding = Padding 3 3 3 3
    , layoutGap = 8
    , layoutAlignX = AlignStart
    , layoutAlignY = AlignTop
    , layoutMinW = 0
    , layoutMinH = 0
    , layoutMaxW = 1e9
    , layoutMaxH = 1e9
    , layoutFontVariant = FontRegular
    , layoutGridCols = 0
    , layoutGridMinColW = 0
    , layoutFontSize = 0
    , layoutFontColor = Nothing
    , layoutFontWeight = WeightNormal
    , layoutFontStyle = FontStyleNormal
    , layoutTextDecoration = DecorationNone
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

{-# INLINE maxW #-}
maxW :: Float -> Layout -> Layout
maxW n l = l {layoutMaxW = n}

{-# INLINE fixedW #-}
fixedW :: Float -> Layout -> Layout
fixedW n l = l {layoutWidth = Fixed n, layoutMinW = n, layoutMaxW = n}

{-# INLINE minH #-}
minH :: Float -> Layout -> Layout
minH n l = l {layoutMinH = n}

{-# INLINE maxH #-}
maxH :: Float -> Layout -> Layout
maxH n l = l {layoutMaxH = n}

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

{-# INLINE tight #-}
tight :: Layout -> Layout
tight l = l {layoutPadding = Padding 0 0 0 0}

{-# INLINE percent #-}
percent :: Float -> Layout -> Layout
percent p l = l {layoutWidth = Percent p}

{-# INLINE gridMinColW #-}
gridMinColW :: Float -> Layout -> Layout
gridMinColW w l = l {layoutGridMinColW = max 0 w}

{-# INLINE fixedAspectW #-}
fixedAspectW :: Float -> Float -> Layout -> Layout
fixedAspectW w ratio = fixedWH w (w / ratio)

{-# INLINE fixedAspectH #-}
fixedAspectH :: Float -> Float -> Layout -> Layout
fixedAspectH h ratio = fixedWH (h * ratio) h

{-# INLINE gridCols #-}
gridCols :: Int -> Layout -> Layout
gridCols n l = l {layoutGridCols = max 0 n}

{-# INLINE cols #-}
cols :: Int -> Layout -> Layout
cols = gridCols

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

{-# INLINE fontDanger #-}
fontDanger :: Layout -> Layout
fontDanger l = l {layoutFontVariant = FontDanger}

{-# INLINE fontSize #-}
fontSize :: Float -> Layout -> Layout
fontSize sz l = l {layoutFontSize = max 0 sz}

{-# INLINE fontSizeScale #-}
fontSizeScale :: Float -> Layout -> Layout
fontSizeScale s l =
  let cur = layoutFontSize l
      sz = if cur > 0 then cur * s else 16 * s
   in l {layoutFontSize = max 0 sz}

{-# INLINE fontColor #-}
fontColor :: Color -> Layout -> Layout
fontColor col l = l {layoutFontColor = Just col}

{-# INLINE textColor #-}
textColor :: Color -> Layout -> Layout
textColor = fontColor

{-# INLINE fontWeight #-}
fontWeight :: FontWeight -> Layout -> Layout
fontWeight w l = l {layoutFontWeight = w}

{-# INLINE fontBold #-}
fontBold :: Layout -> Layout
fontBold = fontWeight WeightBold

{-# INLINE fontLight #-}
fontLight :: Layout -> Layout
fontLight = fontWeight WeightLight

{-# INLINE fontMedium #-}
fontMedium :: Layout -> Layout
fontMedium = fontWeight WeightMedium

{-# INLINE fontSemiBold #-}
fontSemiBold :: Layout -> Layout
fontSemiBold = fontWeight WeightSemiBold

{-# INLINE fontExtraBold #-}
fontExtraBold :: Layout -> Layout
fontExtraBold = fontWeight WeightExtraBold

{-# INLINE fontBlack #-}
fontBlack :: Layout -> Layout
fontBlack = fontWeight WeightBlack

{-# INLINE fontStyle #-}
fontStyle :: FontStyle -> Layout -> Layout
fontStyle s l = l {layoutFontStyle = s}

{-# INLINE fontItalic #-}
fontItalic :: Layout -> Layout
fontItalic = fontStyle FontStyleItalic

{-# INLINE fontOblique #-}
fontOblique :: Layout -> Layout
fontOblique = fontStyle FontStyleOblique

{-# INLINE textDecoration #-}
textDecoration :: TextDecoration -> Layout -> Layout
textDecoration d l = l {layoutTextDecoration = d}

{-# INLINE fontUnderline #-}
fontUnderline :: Layout -> Layout
fontUnderline l =
  let newDeco = case layoutTextDecoration l of
        DecorationStrikethrough -> DecorationUnderlineStrike
        DecorationUnderlineStrike -> DecorationUnderlineStrike
        _ -> DecorationUnderline
   in l {layoutTextDecoration = newDeco}

{-# INLINE fontStrike #-}
fontStrike :: Layout -> Layout
fontStrike l =
  let newDeco = case layoutTextDecoration l of
        DecorationUnderline -> DecorationUnderlineStrike
        DecorationUnderlineStrike -> DecorationUnderlineStrike
        _ -> DecorationStrikethrough
   in l {layoutTextDecoration = newDeco}

{-# INLINE fontStrikethrough #-}
fontStrikethrough :: Layout -> Layout
fontStrikethrough = fontStrike

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

{-# INLINE packPanelStyle #-}
packPanelStyle :: Color -> Color -> Int
packPanelStyle (Color bg) (Color border) =
  let bg64 = fromIntegral bg :: Word64
      br64 = fromIntegral border :: Word64
   in fromIntegral ((br64 `shiftL` 32) .|. (bg64 .&. 0xFFFFFFFF))

{-# INLINE unpackPanelStyle #-}
unpackPanelStyle :: Style -> Int -> Style
unpackPanelStyle baseStyle si =
  let raw = fromIntegral si :: Word64
      bg = fromIntegral (raw .&. 0xFFFFFFFF) :: Word32
      border = fromIntegral ((raw `shiftR` 32) .&. 0xFFFFFFFF) :: Word32
      s1 = if bg /= 0 then baseStyle { styleBg = Color bg } else baseStyle
      s2 = if border /= 0 then s1 { styleBorder = Color border } else s1
   in s2

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
        , themeRed = colorRGBA 252 165 165 255
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

-- | Ported from "Tomorrow Night Min" in https://github.com/biaqat/tomorrow-min-theme-zed
tomorrowNightMinTheme :: Theme
tomorrowNightMinTheme =
  let panelStyle =
        Style
          { styleBg = colorRGBA 30 31 33 255          -- base.bg #1E1F21 (elevated panel canvas)
          , styleFg = colorRGBA 234 234 234 255       -- bright.fg #EAEAEA
          , styleBorder = borderColor
          , styleBorderWidth = 1
          , styleCornerRadius = 2
          , styleHoverBg = colorRGBA 52 54 62 255     -- #34363E
          , styleActiveBg = colorRGBA 26 27 29 255    -- #1A1B1D
          }
   in Theme
        { themeWindow = colorRGBA 23 24 26 255         -- #17181A (dark root window backdrop)
        , themePanel = panelStyle
        , themeFloatingWindow = panelStyle
        , themeButton =
            Style
              { styleBg = colorRGBA 44 46 51 255       -- elevated button surface
              , styleFg = colorRGBA 245 245 245 255    -- bright.fg / white
              , styleBorder = borderColor
              , styleBorderWidth = 1
              , styleCornerRadius = 2
              , styleHoverBg = colorRGBA 69 74 83 255  -- #454A53
              , styleActiveBg = colorRGBA 28 29 32 255 -- depressed on click
              }
        , themeInput =
            Style
              { styleBg = colorRGBA 23 24 26 255       -- #17181A (recessed into #1E1F21 panel)
              , styleFg = colorRGBA 234 234 234 255    -- bright.fg #EAEAEA
              , styleBorder = borderColor
              , styleBorderWidth = 1
              , styleCornerRadius = 2
              , styleHoverBg = colorRGBA 29 30 33 255
              , styleActiveBg = colorRGBA 19 20 22 255
              }
        , themeSeparator = separatorColor
        , themeAccent = activeColor
        , themeMuted = colorRGBA 150 152 150 255       -- comment #969896
        , themeRed = colorRGBA 204 102 102 255         -- base.red #CC6666
        , themeOrange = colorRGBA 222 147 95 255       -- base.orange #DE935F
        , themeYellow = colorRGBA 240 198 116 255      -- base.yellow #F0C674
        , themeGreen = colorRGBA 181 189 104 255       -- base.green #B5BD68
        , themePurple = colorRGBA 178 148 187 255      -- base.purple #B294BB
        , themeOverlayDim = colorRGBA 0 0 0 160
        }
  where
  borderColor    = colorRGBA 77 80 87 255              -- window #4D5057 (touch brighter crisp border)
  separatorColor = colorRGBA 55 59 65 255              -- base.selection #373B41 (subtle divider)
  activeColor    = colorRGBA 103 150 230 255           -- vscode.cornflower_blue #6796E6


tomorrowNightMinDarkTheme :: Theme
tomorrowNightMinDarkTheme = tomorrowNightMinTheme

-- | Ported from "Tomorrow Min" in https://github.com/biaqat/tomorrow-min-theme-zed
tomorrowLightTheme :: Theme
tomorrowLightTheme =
  let panelStyle =
        Style
          { styleBg = colorRGBA 242 242 242 255       -- #F2F2F2
          , styleFg = colorRGBA 55 59 65 255          -- #373B41
          , styleBorder = colorRGBA 222 222 222 255   -- #DEDEDE
          , styleBorderWidth = 1
          , styleCornerRadius = 2
          , styleHoverBg = colorRGBA 231 231 231 255 -- #E7E7E7 (darker than #F2F2F2 so hover reads)
          , styleActiveBg = colorRGBA 219 219 219 255 -- #DBDBDB
          }
   in Theme
        { themeWindow = colorRGBA 255 255 255 255     -- #FFFFFF
        , themePanel = panelStyle
        , themeFloatingWindow = panelStyle
        , themeButton =
            Style
              { styleBg = colorRGBA 232 232 232 255   -- #E8E8E8 (step down from panel for zebra rows)
              , styleFg = colorRGBA 55 59 65 255      -- #373B41
              , styleBorder = colorRGBA 214 214 214 255 -- #D6D6D6
              , styleBorderWidth = 1
              , styleCornerRadius = 2
              , styleHoverBg = colorRGBA 214 214 214 255 -- #D6D6D6
              , styleActiveBg = colorRGBA 196 196 196 255 -- #C4C4C4
              }
        , themeInput =
            Style
              { styleBg = colorRGBA 255 255 255 255   -- #FFFFFF
              , styleFg = colorRGBA 55 59 65 255
              , styleBorder = colorRGBA 210 210 210 255
              , styleBorderWidth = 1
              , styleCornerRadius = 2
              , styleHoverBg = colorRGBA 243 243 243 255 -- #F3F3F3 (darker than white so hover reads)
              , styleActiveBg = colorRGBA 255 255 255 255 -- focus keeps the normal white bg; accent border signals focus
              }
        , themeSeparator = colorRGBA 222 222 222 255  -- #DEDEDE
        , themeAccent = colorRGBA 82 134 188 255      -- #5286BC (Tomorrow Blue)
        , themeMuted = colorRGBA 140 140 140 255      -- #8C8C8C
        , themeRed = colorRGBA 197 78 82 255          -- Tomorrow Red #C54E52
        , themeOrange = colorRGBA 231 140 69 255      -- Tomorrow Orange #E78C45
        , themeYellow = colorRGBA 231 197 71 255      -- Tomorrow Yellow #E7C547
        , themeGreen = colorRGBA 113 140 0 255        -- Tomorrow Green #718C00
        , themePurple = colorRGBA 137 91 144 255      -- Tomorrow Purple #895B90
        , themeOverlayDim = colorRGBA 0 0 0 100
        }

tomorrowMinLightTheme :: Theme
tomorrowMinLightTheme = tomorrowLightTheme

-- | Ported from "Tomorrow at Midnight Min" in https://github.com/biaqat/tomorrow-min-theme-zed
tomorrowMidnightMinTheme :: Theme
tomorrowMidnightMinTheme =
  let panelStyle =
        Style
          { styleBg = colorRGBA 16 17 20 255          -- #101114 (elevated panel canvas)
          , styleFg = colorRGBA 238 238 238 255       -- #EEEEEE
          , styleBorder = borderColor
          , styleBorderWidth = 1
          , styleCornerRadius = 2
          , styleHoverBg = colorRGBA 46 48 56 255     -- #2E3038
          , styleActiveBg = colorRGBA 12 13 15 255    -- #0C0D0F
          }
   in Theme
        { themeWindow = colorRGBA 0 0 0 255           -- #000000 (pitch black root window backdrop)
        , themePanel = panelStyle
        , themeFloatingWindow = panelStyle
        , themeButton =
            Style
              { styleBg = colorRGBA 26 27 34 255       -- #1A1B22
              , styleFg = colorRGBA 238 238 238 255    -- #EEEEEE
              , styleBorder = borderColor
              , styleBorderWidth = 1
              , styleCornerRadius = 2
              , styleHoverBg = colorRGBA 54 58 72 255     -- #363A48
              , styleActiveBg = colorRGBA 56 60 81 255 -- #383C51
              }
        , themeInput =
            Style
              { styleBg = colorRGBA 13 14 18 255       -- #0D0E12 (recessed into panel)
              , styleFg = colorRGBA 238 238 238 255
              , styleBorder = borderColor
              , styleBorderWidth = 1
              , styleCornerRadius = 2
              , styleHoverBg = colorRGBA 21 22 28 255
              , styleActiveBg = colorRGBA 8 9 11 255
              }
        , themeSeparator = separatorColor
        , themeAccent = activeColor
        , themeMuted = colorRGBA 128 132 150 255       -- #808496
        , themeRed = colorRGBA 213 78 83 255           -- bright.red #D54E53
        , themeOrange = colorRGBA 231 140 69 255       -- bright.orange #E78C45
        , themeYellow = colorRGBA 231 197 71 255       -- bright.yellow #E7C547
        , themeGreen = colorRGBA 185 202 74 255        -- bright.green #B9CA4A
        , themePurple = colorRGBA 195 151 216 255      -- bright.purple #C397D8
        , themeOverlayDim = colorRGBA 0 0 0 160
        }
  where
  borderColor    = colorRGBA 48 52 70 255              -- #303446
  separatorColor = colorRGBA 48 52 70 255              -- #303446
  activeColor    = colorRGBA 140 182 226 255           -- #8CB6E2

tomorrowMidnightMinDarkTheme :: Theme
tomorrowMidnightMinDarkTheme = tomorrowMidnightMinTheme

-- -----------------------------------------------------------------------------
-- Base16 Colorschemes
-- -----------------------------------------------------------------------------

-- | Standard Base16 palette containing 16 styling tones and syntax colours
-- following Chris Kempson's Base16 specification.
data Base16 = Base16
  { base00 :: Color -- ^ Default Background
  , base01 :: Color -- ^ Lighter Background (status bars, line numbers, panel backgrounds)
  , base02 :: Color -- ^ Selection Background (active elements, subtle highlights)
  , base03 :: Color -- ^ Comments, Invisibles, Line Highlighting (muted text, borders)
  , base04 :: Color -- ^ Dark Foreground (status bar foreground, secondary text)
  , base05 :: Color -- ^ Default Foreground, Caret, Delimiters, Operators
  , base06 :: Color -- ^ Light Foreground
  , base07 :: Color -- ^ Light Background / Highest contrast foreground
  , base08 :: Color -- ^ Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted (Red)
  , base09 :: Color -- ^ Integers, Boolean, Constants, XML Attributes, Markup Link Url (Orange)
  , base0A :: Color -- ^ Classes, Markup Bold, Search Text Background (Yellow)
  , base0B :: Color -- ^ Strings, Inherited Class, Markup Code, Diff Inserted (Green)
  , base0C :: Color -- ^ Support, Regular Expressions, Escape Characters, Markup Quotes (Cyan)
  , base0D :: Color -- ^ Functions, Methods, Attribute IDs, Headings (Blue / Primary Accent)
  , base0E :: Color -- ^ Keywords, Storage, Selector, Markup Italic, Diff Changed (Purple / Magenta)
  , base0F :: Color -- ^ Deprecated, Opening/Closing Embedded Language Tags (Brown)
  }
  deriving (Eq, Show)

type Base16ColorScheme = Base16

-- | Lowercase field aliases for the hex letter tones in Base16.
base0a, base0b, base0c, base0d, base0e, base0f :: Base16 -> Color
base0a = base0A
base0b = base0B
base0c = base0C
base0d = base0D
base0e = base0E
base0f = base0F

-- | Calculate a 'Theme' from a 'Base16' colorscheme, automatically selecting
-- dark or light styling based on background vs foreground luminance.
themeFromBase16 :: Base16 -> Theme
themeFromBase16 b
  | isDark = themeFromBase16Dark b
  | otherwise = themeFromBase16Light b
  where
    isDark = colorLuminance (base00 b) < colorLuminance (base05 b)

-- | Alias for 'themeFromBase16'.
base16Theme :: Base16 -> Theme
base16Theme = themeFromBase16

-- | Alias for 'themeFromBase16'.
base16ToTheme :: Base16 -> Theme
base16ToTheme = themeFromBase16

-- | Calculate a dark 'Theme' from a 'Base16' colorscheme.
themeFromBase16Dark :: Base16 -> Theme
themeFromBase16Dark b =
  let borderColor = lerpColor (base02 b) (base03 b) 0.35
      panelBg = lerpColor (base01 b) (base02 b) 0.3
      panelStyle =
        Style
          { styleBg = panelBg
          , styleFg = base05 b
          , styleBorder = borderColor
          , styleBorderWidth = 1
          , styleCornerRadius = 2
          , styleHoverBg = lerpColor panelBg (base02 b) 0.5
          , styleActiveBg = lerpColor panelBg (base00 b) 0.4
          }
   in Theme
        { themeWindow = base00 b
        , themePanel = panelStyle
        , themeFloatingWindow = panelStyle
        , themeButton =
            Style
              { styleBg = base02 b
              , styleFg = base07 b
              , styleBorder = borderColor
              , styleBorderWidth = 1
              , styleCornerRadius = 2
              , styleHoverBg = lerpColor (base02 b) (base03 b) 0.4
              , styleActiveBg = base01 b
              }
        , themeInput =
            Style
              { styleBg = base00 b
              , styleFg = base05 b
              , styleBorder = borderColor
              , styleBorderWidth = 1
              , styleCornerRadius = 2
              , styleHoverBg = base01 b
              , styleActiveBg = base00 b
              }
        , themeSeparator = borderColor
        , themeAccent = base0D b
        , themeMuted = base03 b
        , themeRed = base08 b
        , themeOrange = base09 b
        , themeYellow = base0A b
        , themeGreen = base0B b
        , themePurple = base0E b
        , themeOverlayDim = colorRGBA 0 0 0 160
        }

-- | Calculate a light 'Theme' from a 'Base16' colorscheme.
themeFromBase16Light :: Base16 -> Theme
themeFromBase16Light b =
  let borderColor = base02 b
      panelBg = lerpColor (base00 b) (base01 b) 0.5
      panelStyle =
        Style
          { styleBg = panelBg
          , styleFg = base05 b
          , styleBorder = borderColor
          , styleBorderWidth = 1
          , styleCornerRadius = 2
          , styleHoverBg = lerpColor panelBg (base00 b) 0.4
          , styleActiveBg = lerpColor panelBg (base02 b) 0.4
          }
   in Theme
        { themeWindow = base00 b
        , themePanel = panelStyle
        , themeFloatingWindow = panelStyle
        , themeButton =
            Style
              { styleBg = base01 b
              , styleFg = base05 b
              , styleBorder = borderColor
              , styleBorderWidth = 1
              , styleCornerRadius = 2
              , styleHoverBg = base02 b
              , styleActiveBg = lerpColor (base02 b) (base03 b) 0.35
              }
        , themeInput =
            Style
              { styleBg = base00 b
              , styleFg = base05 b
              , styleBorder = borderColor
              , styleBorderWidth = 1
              , styleCornerRadius = 2
              , styleHoverBg = lerpColor (base00 b) (base01 b) 0.3
              , styleActiveBg = lerpColor (base00 b) (base01 b) 0.6
              }
        , themeSeparator = borderColor
        , themeAccent = base0D b
        , themeMuted = base03 b
        , themeRed = base08 b
        , themeOrange = base09 b
        , themeYellow = base0A b
        , themeGreen = base0B b
        , themePurple = base0E b
        , themeOverlayDim = colorRGBA 0 0 0 100
        }

-- | Tomorrow Night Base16 reference palette.
base16TomorrowNight :: Base16
base16TomorrowNight =
  Base16
    { base00 = colorRGBA 29 31 33 255     -- #1D1F21
    , base01 = colorRGBA 40 42 46 255     -- #282A2E
    , base02 = colorRGBA 55 59 65 255     -- #373B41
    , base03 = colorRGBA 150 152 150 255 -- #969896
    , base04 = colorRGBA 180 183 180 255 -- #B4B7B4
    , base05 = colorRGBA 197 200 198 255 -- #C5C8C6
    , base06 = colorRGBA 224 224 224 255 -- #E0E0E0
    , base07 = colorRGBA 255 255 255 255 -- #FFFFFF
    , base08 = colorRGBA 213 78 83 255   -- #D54E53
    , base09 = colorRGBA 231 140 69 255  -- #E78C45
    , base0A = colorRGBA 231 197 71 255  -- #E7C547
    , base0B = colorRGBA 185 202 74 255  -- #B9CA4A
    , base0C = colorRGBA 112 192 186 255 -- #70C0BA
    , base0D = colorRGBA 103 150 230 255 -- #6796E6
    , base0E = colorRGBA 195 151 216 255 -- #C397D8
    , base0F = colorRGBA 163 104 90 255  -- #A3685A
    }

-- | Tomorrow Light Base16 reference palette.
base16TomorrowLight :: Base16
base16TomorrowLight =
  Base16
    { base00 = colorRGBA 255 255 255 255 -- #FFFFFF
    , base01 = colorRGBA 242 242 242 255 -- #F2F2F2
    , base02 = colorRGBA 222 222 222 255 -- #DEDEDE
    , base03 = colorRGBA 140 140 140 255 -- #8C8C8C
    , base04 = colorRGBA 150 152 150 255 -- #969896
    , base05 = colorRGBA 55 59 65 255    -- #373B41
    , base06 = colorRGBA 40 42 46 255    -- #282A2E
    , base07 = colorRGBA 29 31 33 255    -- #1D1F21
    , base08 = colorRGBA 197 78 82 255   -- #C54E52
    , base09 = colorRGBA 231 140 69 255  -- #E78C45
    , base0A = colorRGBA 231 197 71 255  -- #E7C547
    , base0B = colorRGBA 113 140 0 255   -- #718C00
    , base0C = colorRGBA 62 153 159 255  -- #3E999F
    , base0D = colorRGBA 82 134 188 255  -- #5286BC
    , base0E = colorRGBA 137 91 144 255  -- #895B90
    , base0F = colorRGBA 163 104 90 255  -- #A3685A
    }
