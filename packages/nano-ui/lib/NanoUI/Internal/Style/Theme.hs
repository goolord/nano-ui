{-# LANGUAGE StrictData #-}

-- | Surface styles, theme definitions, readability helpers, and style/theme
-- modifiers.
module NanoUI.Internal.Style.Theme
  ( Style (..)
  , Side (..)
  , styleHasSide
  , fieldIconColor
  , Theme (..)
  , Appearance (..)
  , lightDark
  , themeAppearance
  , darkColor
  , readableTone
  , toneColor
  , textToneColor
  , variantFace
  , variantTone
  , flatStyle
  , withReadableTones
  , buildThemePreset
  , background
  , foreground
  , borderColor
  , borderWidth
  , borderSides
  , borderLeft
  , borderRight
  , borderTop
  , borderBottom
  , cornerRadius
  , hoverBackground
  , pressBackground
  , fillColor
  , buttonStyle
  , inputStyle
  , panelStyle
  , windowStyle
  , popupStyle
  , everyStyle
  , accentColor
  , textColor
  , mutedColor
  , linkColor
  , selectionColor
  , windowColor
  , pageBackground
  , pageColor
  , panelPage
  , rounded
  , tinted
  , tone
  , primary
  , destructive
  , success
  , warning
  , subtle
  , readableOn
  , disabledTheme
  , themeSeries
  , scrollBarTrackColor
  , scrollBarThumbColor
  , scrollBarThumbHoverColor
  ) where

import Control.Applicative ((<|>))
import Data.Bits (setBit, testBit)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import NanoUI.Internal.Style.Layout (FontVariant (..), Tone (..))
import NanoUI.Internal.Types (Color (..), clamp, colorA, colorB, colorG, colorLuminance, colorR, colorRGBA, colorTransparent, contrastRatio, fadeAlpha, lerpColor)

-- | Surface colours and border geometry. Border width and corner radius use
-- logical pixels and affect painting, not layout size.
data Style = Style
  { styleBg :: {-# UNPACK #-} !Color
  , styleFg :: {-# UNPACK #-} !Color
  , styleBorder :: {-# UNPACK #-} !Color
  , styleBorderWidth :: {-# UNPACK #-} !Float
  , styleCornerRadius :: {-# UNPACK #-} !Float
  , styleHoverBg :: {-# UNPACK #-} !Color
  , styleActiveBg :: {-# UNPACK #-} !Color
  , styleBorderSides :: {-# UNPACK #-} !Word8
  -- ^ The sides the border is drawn on, one bit per 'Side' in its order.
  }
  deriving (Eq, Show)

-- | A side of a rectangle, for 'borderSides'.
data Side = SideLeft | SideRight | SideTop | SideBottom
  deriving (Eq, Show, Enum, Bounded, Ord)

-- | Whether the style's border is drawn on this side.
{-# INLINE styleHasSide #-}
styleHasSide :: Side -> Style -> Bool
styleHasSide side s = testBit (styleBorderSides s) (fromEnum side)

-- | Colours and surface styles used to paint a view.
data Theme = Theme
  { themeWindow :: {-# UNPACK #-} !Color
  , themePage :: !(Maybe Color)
  , themePanel :: !Style
  , themeFloatingWindow :: !Style
  , themePopup :: !Style
  , themeButton :: !Style
  , themeInput :: !Style
  , themeSeparator :: {-# UNPACK #-} !Color
  , themeAccent :: {-# UNPACK #-} !Color
  , themeMuted :: {-# UNPACK #-} !Color
  , themeRed :: {-# UNPACK #-} !Color
  , themeOrange :: {-# UNPACK #-} !Color
  , themeYellow :: {-# UNPACK #-} !Color
  , themeGreen :: {-# UNPACK #-} !Color
  , themePurple :: {-# UNPACK #-} !Color
  , themeSuccess :: {-# UNPACK #-} !Color
  , themeWarning :: {-# UNPACK #-} !Color
  , themeDanger :: {-# UNPACK #-} !Color
  , themeOverlayDim :: {-# UNPACK #-} !Color
  , themeOnAccent :: {-# UNPACK #-} !Color
  , themeSelection :: {-# UNPACK #-} !Color
  , themeFocusRing :: {-# UNPACK #-} !Color
  , themeLink :: {-# UNPACK #-} !Color
  , themeShadow :: {-# UNPACK #-} !Color
  , themeDisabledFade :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

-- | The system's light or dark preference.
data Appearance
  = AppearanceLight
  | AppearanceDark
  deriving (Eq, Show, Enum, Bounded, Ord)

-- | Pick the light theme for 'AppearanceLight', otherwise the dark one.
lightDark :: Theme -> Theme -> Maybe Appearance -> Theme
lightDark light _ (Just AppearanceLight) = light
lightDark _ dark _ = dark

-- | Classify a theme by its page background: dark if white text reads better on
-- it than black.
themeAppearance :: Theme -> Appearance
themeAppearance t = if darkColor (pageBackground t) then AppearanceDark else AppearanceLight

-- | Whether white reads better on a colour than black.
darkColor :: Color -> Bool
darkColor c = colorLuminance c < 0.179

-- | Blend a colour toward white (dark window) or black (light window) in 5%
-- steps until it reaches 4.5:1 contrast with the window.
readableTone :: Color -> Color -> Color
readableTone window c0 =
  fromMaybe (lerpColor c0 toward 0.95) $
    find (\c -> contrastRatio c window >= 4.5) [lerpColor c0 toward (fromIntegral i * 0.05) | i <- [0 .. 18 :: Int]]
  where
    toward = if darkColor window then colorRGBA 255 255 255 255 else colorRGBA 0 0 0 255

-- | A tone's colour in a theme.
toneColor :: Theme -> Tone -> Color
toneColor t = \case
  Accent -> themeAccent t
  Muted -> themeMuted t
  Success -> themeSuccess t
  Warning -> themeWarning t
  Danger -> themeDanger t

-- | Text colour when no explicit colour is set.
textToneColor :: Theme -> FontVariant -> Maybe Tone -> Color
textToneColor theme variant t = case t <|> variantTone variant of
  Just t' -> toneColor theme t'
  Nothing -> styleFg (themePanel theme)

-- | The face a variant draws in. Colour-only variants use the regular face.
variantFace :: FontVariant -> FontVariant
variantFace = \case
  FontHeading -> FontRegular
  FontMuted -> FontRegular
  FontDanger -> FontRegular
  v -> v

-- | The tone implied by a colour-only variant.
variantTone :: FontVariant -> Maybe Tone
variantTone = \case
  FontHeading -> Just Accent
  FontMuted -> Just Muted
  FontDanger -> Just Danger
  _ -> Nothing

-- | Set the background, shifting hover/press shades proportionally.
background :: Color -> Style -> Style
background c s =
  s {styleBg = c, styleHoverBg = follow styleHoverBg, styleActiveBg = follow styleActiveBg}
  where
    old = styleBg s
    follow shade
      | shade s == old = c
      | colorA old == 0 = shade (fillColor c s)
      | otherwise = shiftColor old (shade s) c

shiftColor :: Color -> Color -> Color -> Color
shiftColor from to c =
  let ch f = fromIntegral (clamp 0 255 (fromIntegral (f c) + fromIntegral (f to) - fromIntegral (f from) :: Int))
   in colorRGBA (ch colorR) (ch colorG) (ch colorB) (colorA c)

-- | Set a surface's text and foreground colour.
foreground :: Color -> Style -> Style
foreground c s = s {styleFg = c}

-- | Set the border colour without changing its width.
borderColor :: Color -> Style -> Style
borderColor c s = s {styleBorder = c}

-- | Set border width in logical pixels, clamped to zero or greater.
borderWidth :: Float -> Style -> Style
borderWidth w s = s {styleBorderWidth = max 0 w}

-- | Draw the border on these sides only.
borderSides :: [Side] -> Style -> Style
borderSides sides s = s {styleBorderSides = foldl' (\m side -> setBit m (fromEnum side)) 0 sides}

-- | A border of width @w@ and colour @c@ on one side only.
borderLeft, borderRight, borderTop, borderBottom :: Float -> Color -> Style -> Style
borderLeft w c s = s {styleBorder = c, styleBorderWidth = max 0 w, styleBorderSides = setBit 0 (fromEnum SideLeft)}
borderRight w c s = s {styleBorder = c, styleBorderWidth = max 0 w, styleBorderSides = setBit 0 (fromEnum SideRight)}
borderTop w c s = s {styleBorder = c, styleBorderWidth = max 0 w, styleBorderSides = setBit 0 (fromEnum SideTop)}
borderBottom w c s = s {styleBorder = c, styleBorderWidth = max 0 w, styleBorderSides = setBit 0 (fromEnum SideBottom)}

-- | Set corner radius in logical pixels, clamped to zero or greater.
cornerRadius :: Float -> Style -> Style
cornerRadius r s = s {styleCornerRadius = max 0 r}

-- | Set the background used while the pointer hovers over a control.
hoverBackground :: Color -> Style -> Style
hoverBackground c s = s {styleHoverBg = c}

-- | Set the background used while a control is active.
pressBackground :: Color -> Style -> Style
pressBackground c s = s {styleActiveBg = c}

-- | A background with hover and press shades derived from it.
fillColor :: Color -> Style -> Style
fillColor c s =
  s
    { styleBg = c
    , styleHoverBg = lerpColor c (styleFg s) 0.12
    , styleActiveBg = lerpColor c (colorRGBA 0 0 0 (colorA c)) 0.18
    }

-- | The colour of icons and adornments inside a field of this style.
fieldIconColor :: Style -> Color
fieldIconColor s = lerpColor (styleFg s) (styleBg s) 0.45

-- | Modify the theme's button surface.
buttonStyle :: (Style -> Style) -> Theme -> Theme
buttonStyle f t = t {themeButton = f (themeButton t)}

-- | Modify the theme's input surface.
inputStyle :: (Style -> Style) -> Theme -> Theme
inputStyle f t = t {themeInput = f (themeInput t)}

-- | Modify the theme's panel surface.
panelStyle :: (Style -> Style) -> Theme -> Theme
panelStyle f t = t {themePanel = f (themePanel t)}

-- | Modify the theme's floating-window surface.
windowStyle :: (Style -> Style) -> Theme -> Theme
windowStyle f t = t {themeFloatingWindow = f (themeFloatingWindow t)}

-- | Modify the theme's popup surface.
popupStyle :: (Style -> Style) -> Theme -> Theme
popupStyle f t = t {themePopup = f (themePopup t)}

-- | Modify button, input, panel, floating-window and popup surfaces together.
everyStyle :: (Style -> Style) -> Theme -> Theme
everyStyle f t =
  t
    { themeButton = f (themeButton t)
    , themeInput = f (themeInput t)
    , themePanel = f (themePanel t)
    , themeFloatingWindow = f (themeFloatingWindow t)
    , themePopup = f (themePopup t)
    }

-- | Set the accent and focus-ring colours, recolouring selection while keeping its alpha.
accentColor :: Color -> Theme -> Theme
accentColor c t = t {themeAccent = c, themeFocusRing = c, themeSelection = fadeAlpha c (colorA (themeSelection t))}

-- | The foreground of every surface.
textColor :: Color -> Theme -> Theme
textColor c = everyStyle (foreground c)

-- | Set the colour for secondary text.
mutedColor :: Color -> Theme -> Theme
mutedColor c t = t {themeMuted = c}

-- | Set the colour for rich-text hyperlinks.
linkColor :: Color -> Theme -> Theme
linkColor c t = t {themeLink = c}

-- | Set the selected-text highlight, including its alpha.
selectionColor :: Color -> Theme -> Theme
selectionColor c t = t {themeSelection = c}

-- | The window background, and the page's unless 'pageColor' sets another.
windowColor :: Color -> Theme -> Theme
windowColor c t = t {themeWindow = c}

-- | The colour a page is painted on.
pageBackground :: Theme -> Color
pageBackground t = fromMaybe (themeWindow t) (themePage t)

-- | Set the page surface colour painted under the view.
pageColor :: Color -> Theme -> Theme
pageColor c t = t {themePage = Just c}

-- | A page painted on the panel surface.
panelPage :: Theme -> Theme
panelPage t = pageColor (styleBg (themePanel t)) t

-- | The corner radius of every surface.
rounded :: Float -> Theme -> Theme
rounded r = everyStyle (cornerRadius r)

-- | Buttons filled with a colour picked from the theme, with a readable label.
tinted :: (Theme -> Color) -> Theme -> Theme
tinted pick t =
  let c = pick t
      label = readableOn t c
   in buttonStyle
        ( \s ->
            s
              { styleBg = c
              , styleFg = label
              , styleBorder = c
              , styleHoverBg = lerpColor c label 0.14
              , styleActiveBg = lerpColor c (themeWindow t) 0.22
              }
        )
        t

-- | Fill buttons with a tone's colour and pick a readable label colour.
tone :: Tone -> Theme -> Theme
tone t = tinted (`toneColor` t)

-- | Buttons in the accent colour: 'tone' 'Accent'.
primary :: Theme -> Theme
primary = tone Accent

-- | Buttons in the danger colour: 'tone' 'Danger'.
destructive :: Theme -> Theme
destructive = tone Danger

-- | Buttons in the success colour: 'tone' 'Success'.
success :: Theme -> Theme
success = tone Success

-- | Buttons in the warning colour: 'tone' 'Warning'.
warning :: Theme -> Theme
warning = tone Warning

-- | Buttons without a fill or border until hovered.
subtle :: Theme -> Theme
subtle =
  buttonStyle $ \s ->
    s
      { styleBg = colorTransparent
      , styleBorder = colorTransparent
      , styleBorderWidth = 0
      , styleHoverBg = fadeAlpha (styleFg s) 30
      , styleActiveBg = fadeAlpha (styleFg s) 48
      }

-- | Whichever of the theme's text colours reads best on @c@.
readableOn :: Theme -> Color -> Color
readableOn t c =
  let !lc = colorLuminance c
      ratio a =
        let !la = colorLuminance a
         in (max la lc + 0.05) / (min la lc + 0.05)
      !c0 = themeOnAccent t
      !c1 = styleFg (themePanel t)
      !c2 = themeWindow t
      !r0 = ratio c0
      !r1 = ratio c1
      !r2 = ratio c2
   in if r0 >= r1 && r0 >= r2
        then c0
        else if r1 >= r2 then c1 else c2

-- | The theme disabled widgets are drawn with.
disabledTheme :: Theme -> Theme
disabledTheme t =
  let f = themeDisabledFade t
      page = pageBackground t
      fade c
        | colorA c == 0 = c
        | otherwise = fadeAlpha (lerpColor c page f) (colorA c)
      fadeStyle s =
        let bg = fade (styleBg s)
         in s {styleBg = bg, styleFg = fade (styleFg s), styleBorder = fade (styleBorder s), styleHoverBg = bg, styleActiveBg = bg}
   in (everyStyle fadeStyle t)
        { themeSeparator = fade (themeSeparator t)
        , themeAccent = fade (themeAccent t)
        , themeMuted = fade (themeMuted t)
        , themeRed = fade (themeRed t)
        , themeOrange = fade (themeOrange t)
        , themeYellow = fade (themeYellow t)
        , themeGreen = fade (themeGreen t)
        , themePurple = fade (themePurple t)
        , themeSuccess = fade (themeSuccess t)
        , themeWarning = fade (themeWarning t)
        , themeDanger = fade (themeDanger t)
        , themeOnAccent = fade (themeOnAccent t)
        , themeFocusRing = fade (themeFocusRing t)
        , themeLink = fade (themeLink t)
        }

-- | Flat widget style: bg/fg/border plus hover and active fills with border width 1 and radius 2.
flatStyle :: Color -> Color -> Color -> Color -> Color -> Style
flatStyle bg fg border hoverBg activeBg =
  Style
    { styleBg = bg
    , styleFg = fg
    , styleBorder = border
    , styleBorderWidth = 1
    , styleCornerRadius = 2
    , styleHoverBg = hoverBg
    , styleActiveBg = activeBg
    , styleBorderSides = 15
    }

-- | Set 'themeSuccess' and 'themeDanger' from the theme's green and red,
-- toned to read on its page ('readableTone').
withReadableTones :: Theme -> Theme
withReadableTones t =
  t
    { themeSuccess = readableTone (pageBackground t) (themeGreen t)
    , themeDanger = readableTone (pageBackground t) (themeRed t)
    }

-- | Shared preset builder: installs @panelSurface@ on panel, floating window,
-- and popup slots, derives readable success/danger tones, and applies overrides.
buildThemePreset :: Theme -> Style -> (Theme -> Theme) -> Theme
buildThemePreset base panelSurface refine =
  withReadableTones $
    refine
      base
        { themePanel = panelSurface
        , themeFloatingWindow = panelSurface
        , themePopup = panelSurface
        }

-- | Series palette in order: red, orange, yellow, green, accent, purple.
themeSeries :: Theme -> [Color]
themeSeries t =
  [ themeRed t
  , themeOrange t
  , themeYellow t
  , themeGreen t
  , themeAccent t
  , themePurple t
  ]

-- | Scrollbar track colour mixed from background and separator.
scrollBarTrackColor :: Style -> Theme -> Color
scrollBarTrackColor base theme =
  lerpColor (styleBg base) (themeSeparator theme) 0.28

-- | Scrollbar thumb colour mixed from separator and foreground, with alpha 130.
scrollBarThumbColor :: Style -> Theme -> Color
scrollBarThumbColor base theme =
  let solid = lerpColor (themeSeparator theme) (styleFg base) 0.58
   in fadeAlpha solid 130

-- | Scrollbar thumb colour under hover/drag, with alpha 180.
scrollBarThumbHoverColor :: Style -> Theme -> Color
scrollBarThumbHoverColor base theme =
  let solid = lerpColor (themeSeparator theme) (styleFg base) 0.72
   in fadeAlpha solid 180
