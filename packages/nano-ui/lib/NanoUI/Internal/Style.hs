{-# LANGUAGE StrictData #-}

-- | Layout options, text styling, and theme palettes. Modifiers compose with
-- @(.)@, with the leftmost modifier winning when both set the same field.
module NanoUI.Internal.Style
  ( Sizing (..)
  , Direction (..)
  , AlignX (..)
  , AlignY (..)
  , Padding (..)
  , Layout (..)
  , defaultLayout
  , Style (..)
  , fieldIconColor
  , Theme (..)
  , defaultTheme
  , defaultLightTheme
  , Appearance (..)
  , tomorrowNightMinDarkTheme
  , tomorrowMinLightTheme
  , tomorrowMidnightMinDarkTheme
  , Base16 (..)
  , themeFromBase16
  , themeFromBase16Dark
  , themeFromBase16Light
  , base16TomorrowNight
  , base16TomorrowLight
  -- * Style modifiers
  , background
  , foreground
  , borderColor
  , borderWidth
  , cornerRadius
  , hoverBackground
  , pressBackground
  , fillColor
  -- * Theme modifiers
  , buttonStyle
  , inputStyle
  , panelStyle
  , windowStyle
  , everyStyle
  , accentColor
  , textColor
  , mutedColor
  , linkColor
  , selectionColor
  , windowColor
  , rounded
  , tinted
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
  , fadeAlpha
  , windowPad
  , windowMargin
  , padAll
  , padXY
  , padTop
  , padBottom
  , padLeft
  , padRight
  , padLRTB
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
  , fontWarning
  , fontSize
  , fontSizeScale
  , fontColor
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
  , addUnderline
  , fontStrike
  , alignStart
  , alignCenter
  , alignTop
  , alignBottom
  , alignBaseline
  , wrap
  , lineGap
  , pinAt
  ) where

import Data.Bits ((.&.), (.|.))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import NanoUI.Internal.Types (Color (..), V2 (..), colorA, colorLuminance, colorRGBA, contrastRatio, lerpColor)

-- | Size along one axis. Fixed sizes use logical pixels; grow/shrink values
-- are relative weights, and percentages use 100 for the full available size.
data Sizing
  = Fixed Float
  | Fit
  | Grow Float
  | Shrink Float
  | Percent Float
  deriving (Eq, Show)

-- | Main axis for laying out a container's children. A 'Stack' has none:
-- each of its children takes the whole content box, placed in it by its own
-- alignment, and is drawn over the children declared before it. A scroll
-- container lays out a 'Stack' as a 'Column', and a grid ignores it.
data Direction = Row | Column | Stack
  deriving (Eq, Show, Enum, Bounded)

-- | Horizontal alignment: left, centre, or right.
data AlignX = AlignStart | AlignCenter | AlignEnd
  deriving (Eq, Show, Enum, Bounded)

-- | 'AlignBaseline' lines a row's text children up on their first baseline, and
-- sits any other child on it by its bottom edge. Outside a row it is
-- 'AlignTop'.
data AlignY = AlignTop | AlignMiddle | AlignBottom | AlignBaseline
  deriving (Eq, Show, Enum, Bounded)

-- | Insets in logical pixels, ordered left, right, top, bottom.
data Padding = Padding
  { padL :: {-# UNPACK #-} !Float
  , padR :: {-# UNPACK #-} !Float
  , padT :: {-# UNPACK #-} !Float
  , padB :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

-- | Floating window padding. The body sits one side-pad below the chrome and one
-- side-pad above the window's bottom edge (the window's own column gap fills
-- the top; see 'NanoUI.Internal.Widgets.Overlay').
windowPad :: Padding
windowPad = Padding 10 10 0 10

-- | Screen inset in logical pixels for floating window/modal size and placement.
windowMargin :: Float
windowMargin = 14

-- | Semantic font choice. The backend selects a face and the theme supplies
-- colours for heading, muted, danger, and warning text.
data FontVariant
  = FontRegular
  | FontHeading
  | FontMuted
  | FontMono
  | FontDanger
  | FontWarning
  deriving (Eq, Show, Enum, Bounded, Ord)

-- | Requested font weight. Available faces and synthetic weights depend on the backend.
data FontWeight
  = WeightNormal
  | WeightBold
  | WeightLight
  | WeightMedium
  | WeightSemiBold
  | WeightExtraBold
  | WeightBlack
  deriving (Eq, Show, Enum, Bounded, Ord)

-- | Upright, italic, or oblique text. Backend support determines the rendered face.
data FontStyle
  = FontStyleNormal
  | FontStyleItalic
  | FontStyleOblique
  deriving (Eq, Show, Enum, Bounded, Ord)

-- | Underline and strikethrough flags for text painting.
data TextDecoration
  = DecorationNone
  | DecorationUnderline
  | DecorationStrikethrough
  | DecorationUnderlineStrike
  deriving (Eq, Show, Enum, Bounded, Ord)

-- | A layout update. Compose with @(.)@; the leftmost update wins when two
-- modifiers set the same field.
type LayoutModifier = Layout -> Layout

-- | Layout and text options for a node. Lengths use logical pixels. Font size
-- 0 selects the backend default; 'Nothing' for font colour uses the theme.
-- Grid column count 0 leaves the count to grid sizing. A row or column with
-- 'layoutWrap' starts a new line where the next child would overflow it,
-- 'layoutLineGap' apart ('Nothing' takes the gap). A node with 'layoutPin'
-- sits at that offset from its parent's content box instead of in its flow.
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
  , layoutWrap :: !Bool
  , layoutLineGap :: !(Maybe Float)
  , layoutPin :: !(Maybe V2)
  }
  deriving (Eq, Show)

-- | Fit-sized column with 3-pixel padding, an 8-pixel gap, top-left alignment,
-- and the regular font. Widgets may override these defaults.
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
    , layoutWrap = False
    , layoutLineGap = Nothing
    , layoutPin = Nothing
    }

-- | Set all four padding edges in logical pixels.
padAll :: Float -> Layout -> Layout
padAll n l = l {layoutPadding = Padding n n n n}

-- | Set horizontal and vertical padding, in that order, in logical pixels.
padXY :: Float -> Float -> Layout -> Layout
padXY x y l = l {layoutPadding = Padding x x y y}

-- | Set the top padding in logical pixels, keeping the other three edges.
padTop :: Float -> Layout -> Layout
padTop n l = l {layoutPadding = (layoutPadding l) {padT = n}}

-- | Set the bottom padding in logical pixels, keeping the other three edges.
padBottom :: Float -> Layout -> Layout
padBottom n l = l {layoutPadding = (layoutPadding l) {padB = n}}

padLeft :: Float -> Layout -> Layout
padLeft n l = l {layoutPadding = (layoutPadding l) {padL = n}}

padRight :: Float -> Layout -> Layout
padRight n l = l {layoutPadding = (layoutPadding l) {padR = n}}

-- | Set left, right, top and bottom padding, in that order, in logical pixels.
padLRTB :: Float -> Float -> Float -> Float -> Layout -> Layout
padLRTB left right top bottom l = l {layoutPadding = Padding left right top bottom}

-- | Set the space between children in logical pixels.
gap :: Float -> Layout -> Layout
gap n l = l {layoutGap = n}

-- | Share available horizontal space with grow weight 1.
fillW :: Layout -> Layout
fillW l = l {layoutWidth = Grow 1}

-- | Share available vertical space with grow weight 1.
fillH :: Layout -> Layout
fillH l = l {layoutHeight = Grow 1}

-- | Apply 'fillW' and 'fillH'.
grow :: Layout -> Layout
grow = fillW . fillH

-- | Set the minimum width in logical pixels without changing the sizing mode.
minW :: Float -> Layout -> Layout
minW n l = l {layoutMinW = n}

-- | Set the maximum width in logical pixels without changing the sizing mode.
maxW :: Float -> Layout -> Layout
maxW n l = l {layoutMaxW = n}

-- | Fix width and both width limits to the given logical-pixel value.
fixedW :: Float -> Layout -> Layout
fixedW n l = l {layoutWidth = Fixed n, layoutMinW = n, layoutMaxW = n}

-- | Set the minimum height in logical pixels without changing the sizing mode.
minH :: Float -> Layout -> Layout
minH n l = l {layoutMinH = n}

-- | Set the maximum height in logical pixels without changing the sizing mode.
maxH :: Float -> Layout -> Layout
maxH n l = l {layoutMaxH = n}

-- | Request a fixed height in logical pixels, retaining existing height limits.
fixedH :: Float -> Layout -> Layout
fixedH n l = l {layoutHeight = Fixed n}

-- | Request fixed width and height in logical pixels, retaining size limits.
fixedWH :: Float -> Float -> Layout -> Layout
fixedWH w h l = l {layoutWidth = Fixed w, layoutHeight = Fixed h}

-- | Centre vertically in the available space.
alignMid :: Layout -> Layout
alignMid l = l {layoutAlignY = AlignMiddle}

-- | Align to the right edge of the available space.
alignEnd :: Layout -> Layout
alignEnd l = l {layoutAlignX = AlignEnd}

-- | Remove all padding, retaining the gap between children.
tight :: Layout -> Layout
tight l = l {layoutPadding = Padding 0 0 0 0}

-- | Set width as a percentage of the parent's available width: 100 means all.
percent :: Float -> Layout -> Layout
percent p l = l {layoutWidth = Percent p}

-- | Set the minimum column width for an adaptive grid, in logical pixels.
-- Non-positive values disable this minimum.
gridMinColW :: Float -> Layout -> Layout
gridMinColW w l = l {layoutGridMinColW = max 0 w}

-- | Fixed width and width/height ratio. The ratio must be positive.
fixedAspectW :: Float -> Float -> Layout -> Layout
fixedAspectW w ratio = fixedWH w (w / ratio)

-- | Fixed height and width/height ratio. The ratio must be positive.
fixedAspectH :: Float -> Float -> Layout -> Layout
fixedAspectH h ratio = fixedWH (h * ratio) h

-- | Set the grid column count, clamping negative counts to zero.
gridCols :: Int -> Layout -> Layout
gridCols n l = l {layoutGridCols = max 0 n}

-- | Select the regular font variant.
fontRegular :: Layout -> Layout
fontRegular l = l {layoutFontVariant = FontRegular}

-- | Select the heading font variant.
fontHeading :: Layout -> Layout
fontHeading l = l {layoutFontVariant = FontHeading}

-- | Select regular text in the theme's muted colour.
fontMuted :: Layout -> Layout
fontMuted l = l {layoutFontVariant = FontMuted}

-- | Select the backend's monospace font variant.
fontMono :: Layout -> Layout
fontMono l = l {layoutFontVariant = FontMono}

-- | Select text in the theme's danger colour.
fontDanger :: Layout -> Layout
fontDanger l = l {layoutFontVariant = FontDanger}

-- | Select text in the theme's warning colour ('themeWarning'), as in
-- @labelWith fontWarning "Unsaved changes"@.
fontWarning :: Layout -> Layout
fontWarning l = l {layoutFontVariant = FontWarning}

-- | Set logical font size. Non-positive values select the backend default.
fontSize :: Float -> Layout -> Layout
fontSize sz l = l {layoutFontSize = max 0 sz}

-- | Multiply an explicit font size, or 16 when none is set, by a scale factor.
-- This uses 16 rather than querying the backend's default size; set that
-- ('NanoUI.Internal.Monad.uiFontSize') with 'fontSize' first to scale it.
fontSizeScale :: Float -> Layout -> Layout
fontSizeScale s l = fontSize ((if layoutFontSize l > 0 then layoutFontSize l else 16) * s) l

-- | Override the theme's text colour for this node.
fontColor :: Color -> Layout -> Layout
fontColor col l = l {layoutFontColor = Just col}

-- | Set the requested font weight independently of its semantic variant.
fontWeight :: FontWeight -> Layout -> Layout
fontWeight w l = l {layoutFontWeight = w}

-- | Request 'WeightBold'.
fontBold :: Layout -> Layout
fontBold = fontWeight WeightBold

-- | Request 'WeightLight'.
fontLight :: Layout -> Layout
fontLight = fontWeight WeightLight

-- | Request 'WeightMedium'.
fontMedium :: Layout -> Layout
fontMedium = fontWeight WeightMedium

-- | Request 'WeightSemiBold'.
fontSemiBold :: Layout -> Layout
fontSemiBold = fontWeight WeightSemiBold

-- | Request 'WeightExtraBold'.
fontExtraBold :: Layout -> Layout
fontExtraBold = fontWeight WeightExtraBold

-- | Request 'WeightBlack', the heaviest weight.
fontBlack :: Layout -> Layout
fontBlack = fontWeight WeightBlack

-- | Set upright, italic, or oblique text without changing its weight.
fontStyle :: FontStyle -> Layout -> Layout
fontStyle s l = l {layoutFontStyle = s}

-- | Request 'FontStyleItalic'.
fontItalic :: Layout -> Layout
fontItalic = fontStyle FontStyleItalic

-- | Request 'FontStyleOblique'.
fontOblique :: Layout -> Layout
fontOblique = fontStyle FontStyleOblique

-- | Replace the node's underline and strikethrough settings.
textDecoration :: TextDecoration -> Layout -> Layout
textDecoration d l = l {layoutTextDecoration = d}

-- | Add an underline while preserving any strikethrough.
fontUnderline :: Layout -> Layout
fontUnderline l = l {layoutTextDecoration = addUnderline (layoutTextDecoration l)}

-- | The decoration with an underline added, keeping any strikethrough. The
-- constructor order of 'TextDecoration' is the bit pattern it stands for:
-- underline is bit 0, strikethrough bit 1.
addUnderline :: TextDecoration -> TextDecoration
addUnderline d = toEnum (fromEnum d .|. 1)

-- | Add a strikethrough while preserving any underline.
fontStrike :: Layout -> Layout
fontStrike l =
  l {layoutTextDecoration = toEnum (fromEnum (layoutTextDecoration l) .|. 2)}

-- | Align to the left edge of the available space.
alignStart :: Layout -> Layout
alignStart l = l {layoutAlignX = AlignStart}

-- | Centre horizontally in the available space.
alignCenter :: Layout -> Layout
alignCenter l = l {layoutAlignX = AlignCenter}

-- | Align to the top edge of the available space.
alignTop :: Layout -> Layout
alignTop l = l {layoutAlignY = AlignTop}

-- | Align to the bottom edge of the available space.
alignBottom :: Layout -> Layout
alignBottom l = l {layoutAlignY = AlignBottom}

-- | Sit on the row's shared text baseline, so labels of different sizes read
-- as one line of type. 'alignBottom' lines up their boxes instead, and a larger
-- font's deeper descent lifts its baseline above the smaller one's.
alignBaseline :: Layout -> Layout
alignBaseline l = l {layoutAlignY = AlignBaseline}

-- | Flow a row's children onto a new line below, or a column's into a new
-- column to the right, where the next child would overflow the main axis, as
-- a list of tags or chips does. A child longer than a whole line takes one to
-- itself. Children keep their gap within a line, and lines are 'lineGap'
-- apart. Grow children share the space left on their own line, and a child's
-- cross-axis alignment places it within its line. A row wraps at the width it
-- is given; a column needs a bounded height ('fixedH', 'maxH') to wrap. Grids
-- and scroll containers ignore it.
wrap :: Layout -> Layout
wrap l = l {layoutWrap = True}

-- | Set the space between a wrapping container's lines in logical pixels. By
-- default lines are the 'gap' apart.
lineGap :: Float -> Layout -> Layout
lineGap n l = l {layoutLineGap = Just (max 0 n)}

-- | Take the node out of its parent's flow and place it @x@ right and @y@ down
-- from the parent's content box (inside its padding), over its siblings,
-- taking the pointer from them where it covers them. The siblings lay out as
-- if it were absent, and it does not count towards the parent's size, but it
-- is clipped and scrolled with them. It keeps its own
-- size: a content or fixed size as usual, even past the parent's edge, a
-- grow size fills the content box past the offset, and a percentage is of
-- the content box. Windows, modals and popups place themselves and ignore it.
pinAt :: Float -> Float -> Layout -> Layout
pinAt x y l = l {layoutPin = Just (V2 x y)}

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
  }
  deriving (Eq, Show)

-- | Colours and surface styles used to paint a view. Use theme modifiers with
-- @styled@ for a subtree or @setUiTheme@ to change the session's base theme.
data Theme = Theme
  { themeWindow :: {-# UNPACK #-} !Color
  , themePanel :: !Style
  , themeFloatingWindow :: !Style
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
  , themeWarning :: {-# UNPACK #-} !Color
  -- ^ Warning text ('fontWarning') and 'warning' buttons: an amber that
  -- reads on the window colour.
  , themeOverlayDim :: {-# UNPACK #-} !Color
  , themeOnAccent :: {-# UNPACK #-} !Color
  -- ^ Text and marks drawn on an accent fill: a checked box, an active tab,
  -- a primary button.
  , themeSelection :: {-# UNPACK #-} !Color
  -- ^ Selected text's highlight, drawn under the text. Usually translucent.
  , themeFocusRing :: {-# UNPACK #-} !Color
  , themeLink :: {-# UNPACK #-} !Color
  , themeShadow :: {-# UNPACK #-} !Color
  -- ^ Offset shadow under menus, dropdowns and floating windows. A zero alpha
  -- draws none.
  , themeDisabledFade :: {-# UNPACK #-} !Float
  -- ^ How far a disabled widget's colours fade toward the window colour, from
  -- 0 (not at all) to 1 (invisible).
  }
  deriving (Eq, Show)

-- | Whether the system asks apps for light or dark colours. See
-- @systemAppearance@ and @followSystemTheme@ in "NanoUI".
data Appearance
  = AppearanceLight
  | AppearanceDark
  deriving (Eq, Show, Enum, Bounded, Ord)

-- -----------------------------------------------------------------------------
-- Style and theme modifiers
-- -----------------------------------------------------------------------------

-- | Set the resting background, retaining hover and pressed colours.
background :: Color -> Style -> Style
background c s = s {styleBg = c}

-- | Set a surface's text and foreground colour.
foreground :: Color -> Style -> Style
foreground c s = s {styleFg = c}

-- | Set the border colour without changing its width.
borderColor :: Color -> Style -> Style
borderColor c s = s {styleBorder = c}

-- | Set border width in logical pixels, clamped to zero or greater.
borderWidth :: Float -> Style -> Style
borderWidth w s = s {styleBorderWidth = max 0 w}

-- | Set corner radius in logical pixels, clamped to zero or greater.
cornerRadius :: Float -> Style -> Style
cornerRadius r s = s {styleCornerRadius = max 0 r}

-- | Set the background used while the pointer hovers over a control.
hoverBackground :: Color -> Style -> Style
hoverBackground c s = s {styleHoverBg = c}

-- | Set the background used while a control is active.
pressBackground :: Color -> Style -> Style
pressBackground c s = s {styleActiveBg = c}

-- | A background with hover and press shades derived from it: hovering mixes
-- in some of the foreground, pressing darkens.
fillColor :: Color -> Style -> Style
fillColor c s =
  s
    { styleBg = c
    , styleHoverBg = lerpColor c (styleFg s) 0.12
    , styleActiveBg = lerpColor c (colorRGBA 0 0 0 (colorA c)) 0.18
    }

-- | The colour of the icons and adornments inside a field of this style: its
-- text colour faded toward its background, so they read as part of the box.
fieldIconColor :: Style -> Color
fieldIconColor s = lerpColor (styleFg s) (styleBg s) 0.45

-- | Modify the theme's button surface.
buttonStyle :: (Style -> Style) -> Theme -> Theme
buttonStyle f t = t {themeButton = f (themeButton t)}

-- | Text fields, text areas, sliders' wells and scroller wells.
inputStyle :: (Style -> Style) -> Theme -> Theme
inputStyle f t = t {themeInput = f (themeInput t)}

-- | Panels, cards, menus, and label text.
panelStyle :: (Style -> Style) -> Theme -> Theme
panelStyle f t = t {themePanel = f (themePanel t)}

-- | Floating windows.
windowStyle :: (Style -> Style) -> Theme -> Theme
windowStyle f t = t {themeFloatingWindow = f (themeFloatingWindow t)}

-- | Modify button, input, panel, and floating-window surfaces together.
everyStyle :: (Style -> Style) -> Theme -> Theme
everyStyle f = buttonStyle f . inputStyle f . panelStyle f . windowStyle f

-- | Set the accent and focus-ring colours, and recolour the selection
-- highlight while preserving its alpha.
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

-- | The backdrop behind everything, which disabled widgets also fade toward.
windowColor :: Color -> Theme -> Theme
windowColor c t = t {themeWindow = c}

-- | The corner radius of every surface.
rounded :: Float -> Theme -> Theme
rounded r = everyStyle (cornerRadius r)

-- | Buttons filled with a colour picked from the theme, with a readable label.
--
-- > styled (tinted themePurple) (button "Tag")
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

-- | Buttons in the accent colour, for the action a view is for.
primary :: Theme -> Theme
primary = tinted themeAccent

-- | Buttons in the theme's red, for destructive actions.
destructive :: Theme -> Theme
destructive = tinted themeRed

-- | Fill buttons with the theme's green and choose a readable label colour.
success :: Theme -> Theme
success = tinted themeGreen

-- | Buttons in the theme's warning amber, for an action that needs care but
-- is not destructive.
warning :: Theme -> Theme
warning = tinted themeWarning

-- | Buttons without a fill or border until hovered, for toolbars and
-- secondary actions.
subtle :: Theme -> Theme
subtle =
  buttonStyle $ \s ->
    s
      { styleBg = clear
      , styleBorder = clear
      , styleBorderWidth = 0
      , styleHoverBg = fadeAlpha (styleFg s) 30
      , styleActiveBg = fadeAlpha (styleFg s) 48
      }
  where
    clear = colorRGBA 0 0 0 0

-- | Whichever of the theme's text colours reads best on @c@.
readableOn :: Theme -> Color -> Color
readableOn t c =
  let candidates = [themeOnAccent t, styleFg (themePanel t), themeWindow t]
      best a b = if contrastRatio a c >= contrastRatio b c then a else b
   in foldr1 best candidates

-- | The theme disabled widgets are drawn with: every colour faded toward the
-- window colour by 'themeDisabledFade', and no hover or press feedback.
disabledTheme :: Theme -> Theme
disabledTheme t =
  let f = themeDisabledFade t
      fade c
        | colorA c == 0 = c
        | otherwise = fadeAlpha (lerpColor c (themeWindow t) f) (colorA c)
      fadeStyle s =
        let bg = fade (styleBg s)
         in s {styleBg = bg, styleFg = fade (styleFg s), styleBorder = fade (styleBorder s), styleHoverBg = bg, styleActiveBg = bg}
   in t
        { themePanel = fadeStyle (themePanel t)
        , themeFloatingWindow = fadeStyle (themeFloatingWindow t)
        , themeButton = fadeStyle (themeButton t)
        , themeInput = fadeStyle (themeInput t)
        , themeSeparator = fade (themeSeparator t)
        , themeAccent = fade (themeAccent t)
        , themeMuted = fade (themeMuted t)
        , themeRed = fade (themeRed t)
        , themeOrange = fade (themeOrange t)
        , themeYellow = fade (themeYellow t)
        , themeGreen = fade (themeGreen t)
        , themePurple = fade (themePurple t)
        , themeWarning = fade (themeWarning t)
        , themeOnAccent = fade (themeOnAccent t)
        , themeFocusRing = fade (themeFocusRing t)
        , themeLink = fade (themeLink t)
        }

-- | Flat widget style: bg/fg/border plus hover and active fills.
-- Border width 1 and corner radius 2, as the built-in themes use.
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
    }

-- | Neutral charcoal surfaces, warm text, and a blue selection accent.
-- Keep structural edges quiet; interactive borders and focus carry contrast.
defaultTheme :: Theme
defaultTheme =
  let panelSurface =
        flatStyle
          (colorRGBA 34 34 38 255)
          (colorRGBA 236 234 230 255)
          (colorRGBA 54 54 62 255)
          (colorRGBA 34 34 38 255)
          (colorRGBA 30 30 34 255)
   in Theme
        { themeWindow = colorRGBA 24 24 27 255
        , themePanel = panelSurface
        , themeFloatingWindow = panelSurface
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
        , themeRed = colorRGBA 252 165 165 255
        , themeOrange = colorRGBA 216 140 72 255
        , themeYellow = colorRGBA 212 176 88 255
        , themeGreen = colorRGBA 104 168 124 255
        , themePurple = colorRGBA 176 140 220 255
        , themeWarning = colorRGBA 242 180 76 255
        , themeOverlayDim = colorRGBA 8 8 10 176
        , themeOnAccent = colorRGBA 255 255 255 255
        , themeSelection = fadeAlpha (colorRGBA 88 156 246 255) 115
        , themeFocusRing = colorRGBA 88 156 246 255
        , themeLink = colorRGBA 124 178 250 255
        , themeShadow = colorRGBA 0 0 0 72
        , themeDisabledFade = 0.55
        }

-- | 'defaultTheme' in light: off-white surfaces, near-black text, and a
-- deeper blue accent that reads on white. The two make a pair for
-- @followSystemTheme@.
defaultLightTheme :: Theme
defaultLightTheme =
  let panelSurface =
        flatStyle
          (colorRGBA 252 252 251 255)
          (colorRGBA 36 36 40 255)
          (colorRGBA 220 220 216 255)
          (colorRGBA 252 252 251 255)
          (colorRGBA 240 240 238 255)
   in Theme
        { themeWindow = colorRGBA 244 244 242 255
        , themePanel = panelSurface
        , themeFloatingWindow = panelSurface
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
        , themeOnAccent = colorRGBA 255 255 255 255
        , themeSelection = fadeAlpha (colorRGBA 37 99 235 255) 80
        , themeFocusRing = colorRGBA 37 99 235 255
        , themeLink = colorRGBA 29 78 216 255
        , themeShadow = colorRGBA 0 0 0 40
        , themeDisabledFade = 0.55
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

-- | Scrollbar track colour: an opaque tint of its background surface toward
-- the separator colour, also used for divider strips and similar hairline
-- chrome.
scrollBarTrackColor :: Style -> Theme -> Color
scrollBarTrackColor base theme =
  lerpColor (styleBg base) (themeSeparator theme) 0.28

-- | Scrollbar thumb colour mixed from separator and foreground, with alpha 130.
scrollBarThumbColor :: Style -> Theme -> Color
scrollBarThumbColor base theme =
  let solid = lerpColor (themeSeparator theme) (styleFg base) 0.58
   in fadeAlpha solid 130

-- | The thumb of a scrollbar under the pointer or being dragged: the
-- 'scrollBarThumbColor' mix taken further toward the foreground, with alpha
-- 180.
scrollBarThumbHoverColor :: Style -> Theme -> Color
scrollBarThumbHoverColor base theme =
  let solid = lerpColor (themeSeparator theme) (styleFg base) 0.72
   in fadeAlpha solid 180

-- | Replaces the alpha channel of a color.
fadeAlpha :: Color -> Word8 -> Color
fadeAlpha (Color w) a = Color ((w .&. 0xFFFFFF00) .|. fromIntegral a)

-- | Ported from "Tomorrow Night Min" in https://github.com/biaqat/tomorrow-min-theme-zed
tomorrowNightMinDarkTheme :: Theme
tomorrowNightMinDarkTheme =
  let panelSurface =
        flatStyle
          (colorRGBA 30 31 33 255)  -- base.bg #1E1F21 (elevated panel canvas)
          (colorRGBA 234 234 234 255)  -- bright.fg #EAEAEA
          edgeCol
          (colorRGBA 52 54 62 255)  -- #34363E
          (colorRGBA 26 27 29 255)  -- #1A1B1D
   in (accentColor accentCol defaultTheme)
        { themeWindow = colorRGBA 23 24 26 255         -- #17181A (dark root window backdrop)
        , themePanel = panelSurface
        , themeFloatingWindow = panelSurface
        , themeButton =
            flatStyle
              (colorRGBA 44 46 51 255)  -- elevated button surface
              (colorRGBA 245 245 245 255)  -- bright.fg / white
              edgeCol
              (colorRGBA 69 74 83 255)  -- #454A53
              (colorRGBA 28 29 32 255)  -- depressed on click
        , themeInput =
            flatStyle
              (colorRGBA 23 24 26 255)  -- #17181A (recessed into #1E1F21 panel)
              (colorRGBA 234 234 234 255)  -- bright.fg #EAEAEA
              edgeCol
              (colorRGBA 29 30 33 255)
              (colorRGBA 19 20 22 255)
        , themeSeparator = colorRGBA 55 59 65 255      -- base.selection #373B41 (subtle divider)
        , themeMuted = colorRGBA 150 152 150 255       -- comment #969896
        , themeRed = colorRGBA 204 102 102 255         -- base.red #CC6666
        , themeOrange = colorRGBA 222 147 95 255       -- base.orange #DE935F
        , themeYellow = colorRGBA 240 198 116 255      -- base.yellow #F0C674
        , themeGreen = colorRGBA 181 189 104 255       -- base.green #B5BD68
        , themePurple = colorRGBA 178 148 187 255      -- base.purple #B294BB
        , themeWarning = colorRGBA 240 198 116 255     -- base.yellow #F0C674
        , themeOverlayDim = colorRGBA 0 0 0 160
        , themeLink = accentCol
        }
  where
  edgeCol    = colorRGBA 77 80 87 255              -- window #4D5057 (touch brighter crisp border)
  accentCol    = colorRGBA 103 150 230 255           -- vscode.cornflower_blue #6796E6

-- | Ported from "Tomorrow Min" in https://github.com/biaqat/tomorrow-min-theme-zed
tomorrowMinLightTheme :: Theme
tomorrowMinLightTheme =
  let panelSurface =
        flatStyle
          (colorRGBA 242 242 242 255)  -- #F2F2F2
          (colorRGBA 55 59 65 255)  -- #373B41
          (colorRGBA 222 222 222 255)  -- #DEDEDE
          (colorRGBA 231 231 231 255)  -- #E7E7E7 (darker than #F2F2F2 so hover reads)
          (colorRGBA 219 219 219 255)  -- #DBDBDB
   in (accentColor (colorRGBA 82 134 188 255) defaultTheme) -- #5286BC (Tomorrow Blue)
        { themeWindow = colorRGBA 255 255 255 255     -- #FFFFFF
        , themePanel = panelSurface
        , themeFloatingWindow = panelSurface
        , themeButton =
            flatStyle
              (colorRGBA 232 232 232 255)  -- #E8E8E8 (step down from panel for zebra rows)
              (colorRGBA 55 59 65 255)  -- #373B41
              (colorRGBA 214 214 214 255)  -- #D6D6D6
              (colorRGBA 214 214 214 255)  -- #D6D6D6
              (colorRGBA 196 196 196 255)  -- #C4C4C4
        , themeInput =
            flatStyle
              (colorRGBA 255 255 255 255)  -- #FFFFFF
              (colorRGBA 55 59 65 255)
              (colorRGBA 210 210 210 255)
              (colorRGBA 243 243 243 255)  -- #F3F3F3 (darker than white so hover reads)
              (colorRGBA 255 255 255 255)  -- focus keeps the normal white bg; accent border signals focus
        , themeSeparator = colorRGBA 222 222 222 255  -- #DEDEDE
        , themeMuted = colorRGBA 140 140 140 255      -- #8C8C8C
        , themeRed = colorRGBA 197 78 82 255          -- Tomorrow Red #C54E52
        , themeOrange = colorRGBA 231 140 69 255      -- Tomorrow Orange #E78C45
        , themeYellow = colorRGBA 231 197 71 255      -- Tomorrow Yellow #E7C547
        , themeGreen = colorRGBA 113 140 0 255        -- Tomorrow Green #718C00
        , themePurple = colorRGBA 137 91 144 255      -- Tomorrow Purple #895B90
        , themeWarning = colorRGBA 150 94 0 255       -- #965E00 (an amber dark enough to read on white)
        , themeOverlayDim = colorRGBA 0 0 0 100
        , themeSelection = fadeAlpha (colorRGBA 82 134 188 255) 80
        , themeLink = colorRGBA 66 113 174 255
        , themeShadow = colorRGBA 0 0 0 36
        }

-- | Ported from "Tomorrow at Midnight Min" in https://github.com/biaqat/tomorrow-min-theme-zed
tomorrowMidnightMinDarkTheme :: Theme
tomorrowMidnightMinDarkTheme =
  let panelSurface =
        flatStyle
          (colorRGBA 16 17 20 255)  -- #101114 (elevated panel canvas)
          (colorRGBA 238 238 238 255)  -- #EEEEEE
          edgeCol
          (colorRGBA 46 48 56 255)  -- #2E3038
          (colorRGBA 12 13 15 255)  -- #0C0D0F
   in (accentColor accentCol defaultTheme)
        { themeWindow = colorRGBA 0 0 0 255           -- #000000 (pitch black root window backdrop)
        , themePanel = panelSurface
        , themeFloatingWindow = panelSurface
        , themeButton =
            flatStyle
              (colorRGBA 26 27 34 255)  -- #1A1B22
              (colorRGBA 238 238 238 255)  -- #EEEEEE
              edgeCol
              (colorRGBA 54 58 72 255)  -- #363A48
              (colorRGBA 56 60 81 255)  -- #383C51
        , themeInput =
            flatStyle
              (colorRGBA 13 14 18 255)  -- #0D0E12 (recessed into panel)
              (colorRGBA 238 238 238 255)
              edgeCol
              (colorRGBA 21 22 28 255)
              (colorRGBA 8 9 11 255)
        , themeSeparator = edgeCol
        , themeMuted = colorRGBA 128 132 150 255       -- #808496
        , themeRed = colorRGBA 213 78 83 255           -- bright.red #D54E53
        , themeOrange = colorRGBA 231 140 69 255       -- bright.orange #E78C45
        , themeYellow = colorRGBA 231 197 71 255       -- bright.yellow #E7C547
        , themeGreen = colorRGBA 185 202 74 255        -- bright.green #B9CA4A
        , themePurple = colorRGBA 195 151 216 255      -- bright.purple #C397D8
        , themeWarning = colorRGBA 231 197 71 255      -- bright.yellow #E7C547
        , themeOverlayDim = colorRGBA 0 0 0 160
        , themeLink = accentCol
        , themeShadow = colorRGBA 0 0 0 96
        }
  where
  edgeCol    = colorRGBA 48 52 70 255              -- #303446
  accentCol    = colorRGBA 140 182 226 255           -- #8CB6E2

-- -----------------------------------------------------------------------------
-- Base16 Colorschemes
-- -----------------------------------------------------------------------------

-- | Standard Base16 palette containing 16 styling tones and syntax colours
-- following Chris Kempson's Base16 specification.
data Base16 = Base16
  { base00 :: {-# UNPACK #-} !Color -- ^ Default Background
  , base01 :: {-# UNPACK #-} !Color -- ^ Lighter Background (status bars, line numbers, panel backgrounds)
  , base02 :: {-# UNPACK #-} !Color -- ^ Selection Background (active elements, subtle highlights)
  , base03 :: {-# UNPACK #-} !Color -- ^ Comments, Invisibles, Line Highlighting (muted text, borders)
  , base04 :: {-# UNPACK #-} !Color -- ^ Dark Foreground (status bar foreground, secondary text)
  , base05 :: {-# UNPACK #-} !Color -- ^ Default Foreground, Caret, Delimiters, Operators
  , base06 :: {-# UNPACK #-} !Color -- ^ Light Foreground
  , base07 :: {-# UNPACK #-} !Color -- ^ Light Background / Highest contrast foreground
  , base08 :: {-# UNPACK #-} !Color -- ^ Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted (Red)
  , base09 :: {-# UNPACK #-} !Color -- ^ Integers, Boolean, Constants, XML Attributes, Markup Link Url (Orange)
  , base0A :: {-# UNPACK #-} !Color -- ^ Classes, Markup Bold, Search Text Background (Yellow)
  , base0B :: {-# UNPACK #-} !Color -- ^ Strings, Inherited Class, Markup Code, Diff Inserted (Green)
  , base0C :: {-# UNPACK #-} !Color -- ^ Support, Regular Expressions, Escape Characters, Markup Quotes (Cyan)
  , base0D :: {-# UNPACK #-} !Color -- ^ Functions, Methods, Attribute IDs, Headings (Blue / Primary Accent)
  , base0E :: {-# UNPACK #-} !Color -- ^ Keywords, Storage, Selector, Markup Italic, Diff Changed (Purple / Magenta)
  , base0F :: {-# UNPACK #-} !Color -- ^ Deprecated, Opening/Closing Embedded Language Tags (Brown)
  }
  deriving (Eq, Show)

-- | Calculate a 'Theme' from a 'Base16' colorscheme, automatically selecting
-- dark or light styling based on background vs foreground luminance.
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
  let
    pick :: a -> a -> a
    pick night day = if dark then night else day
    edgeCol = pick (lerpColor (base02 b) (base03 b) 0.35) (base02 b)
    panelBg =
      pick (lerpColor (base01 b) (base02 b) 0.3) (lerpColor (base00 b) (base01 b) 0.5)
    panelSurface =
      flatStyle
        panelBg
        (base05 b)
        edgeCol
        (pick (lerpColor panelBg (base02 b) 0.5) (lerpColor panelBg (base00 b) 0.4))
        (lerpColor panelBg (pick (base00 b) (base02 b)) 0.4)
    -- Warning text is the scheme's yellow on a dark background and its
    -- orange on a light one, where yellow rarely reads, taken toward white
    -- or black until it reads on the window at 4.5:1.
    warn0 = pick (base0A b) (base09 b)
    toward = pick (colorRGBA 255 255 255 255) (colorRGBA 0 0 0 255)
    warnCol =
      fromMaybe (lerpColor warn0 toward 0.95) $
        find
          (\c -> contrastRatio c (base00 b) >= 4.5)
          [lerpColor warn0 toward (fromIntegral i * 0.05) | i <- [0 .. 18 :: Int]]
   in
    (accentColor (base0D b) defaultTheme)
      { themeWindow = base00 b
      , themePanel = panelSurface
      , themeFloatingWindow = panelSurface
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
      , themeWarning = warnCol
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
