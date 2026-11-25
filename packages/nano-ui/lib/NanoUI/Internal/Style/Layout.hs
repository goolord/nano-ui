{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}

-- | Layout options, flow modes, alignment, typography attributes, and layout
-- modifiers. Modifiers compose with @(.)@, with the leftmost modifier winning
-- when both set the same field.
module NanoUI.Internal.Style.Layout
  ( Sizing (..)
  , Direction (..)
  , Flow (..)
  , AlignX (..)
  , AlignY (..)
  , ItemsAlign (ItemsStart, ItemsCenter, ItemsEnd, ItemsBaseline)
  , itemsAlign
  , itemsStart
  , itemsCenter
  , itemsEnd
  , itemsBaseline
  , alignXFraction
  , alignYFraction
  , Padding (..)
  , Layout (..)
  , defaultLayout
  , Tone (..)
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
  , hasExplicitMin
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
  , aspect
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
  , fontTone
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
  , fontTabular
  , alignStart
  , alignCenter
  , alignTop
  , alignBottom
  , alignBaseline
  , wrap
  , layered
  , lineGap
  , lineAlign
  , LineAlign (LinesStart, LinesCenter, LinesEnd)
  , lineAlignFraction
  , pinAt
  , PointerMode (PointerAuto, PointerBlock, PointerPass)
  , pointer
  , TextBreak (BreakWords, BreakAnywhere)
  , textBreak
  , breakAnywhere
  , TextEllipsis (EllipsisEnd, EllipsisMiddle, EllipsisStart)
  , textEllipsis
  , ellipsisEnd
  , ellipsisMiddle
  , ellipsisStart
  ) where

import Data.Bits ((.|.))
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen, predError, succError, toEnumError)
import GHC.TypeLits (KnownNat, Nat, natVal)
import NanoUI.Internal.Types (Color (..), V2 (..), noLimit)

-- | Size along one axis. Fixed sizes use logical pixels; grow/shrink values
-- are relative weights, and percentages use 100 for the full available size.
data Sizing
  = Fixed Float
  | Fit
  | Grow Float
  | Shrink Float
  | Percent Float
  deriving (Eq, Show)

-- | Main axis for laying out a container's children: left to right, or top
-- to bottom. 'Flow' decides whether they stay in one line, wrap, or layer.
data Direction = Row | Column
  deriving (Eq, Show, Enum, Bounded, Ord)

-- | How a container places its children ('layoutFlow'). Scroll containers
-- and grids ignore it; put a wrapping or layered column inside them instead.
data Flow
  = -- | A single row or column along the 'Direction'. The default.
    Line
  | -- | Starts a new line where the next child would overflow the main axis
    -- ('wrap').
    Wrap
  | -- | Every child covers the content box, placed by its own alignment and
    -- drawn over earlier siblings ('layered'). 'Direction' is ignored.
    Layered
  deriving (Eq, Show, Enum, Bounded, Ord)

-- | Horizontal alignment: left, centre, or right.
data AlignX = AlignStart | AlignCenter | AlignEnd
  deriving (Eq, Show, Enum, Bounded, Ord)

-- | 'AlignBaseline' lines a row's text children up on their first baseline, and
-- sits any other child on it by its bottom edge. Outside a row it is
-- 'AlignTop'.
data AlignY = AlignTop | AlignMiddle | AlignBottom | AlignBaseline
  deriving (Eq, Show, Enum, Bounded, Ord)

-- | Where 'AlignX' puts content across the room left over: 0 at the left, 0.5
-- centred, 1 at the right.
{-# INLINE alignXFraction #-}
alignXFraction :: AlignX -> Float
alignXFraction AlignStart = 0
alignXFraction AlignCenter = 0.5
alignXFraction AlignEnd = 1

-- | Where 'AlignY' puts content down the room left over: 0 at the top, 0.5
-- centred, 1 at the bottom. 'AlignBaseline' counts as 'AlignTop': only a row
-- has a baseline to share, and it places those children itself.
{-# INLINE alignYFraction #-}
alignYFraction :: AlignY -> Float
alignYFraction AlignMiddle = 0.5
alignYFraction AlignBottom = 1
alignYFraction _ = 0

-- | Insets in logical pixels, ordered left, right, top, bottom.
data Padding = Padding
  { padL :: {-# UNPACK #-} !Float
  , padR :: {-# UNPACK #-} !Float
  , padT :: {-# UNPACK #-} !Float
  , padB :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

-- | Floating window padding.
windowPad :: Padding
windowPad = Padding 10 10 0 10

-- | Screen inset in logical pixels for floating window/modal size and placement.
windowMargin :: Float
windowMargin = 14

-- | Semantic font choice. The backend selects a face.
data FontVariant
  = FontRegular
  | FontHeading
  | FontMuted
  | FontMono
  | FontDanger
  deriving (Eq, Show, Enum, Bounded, Ord)

-- | A semantic theme colour ('NanoUI.Internal.Style.Theme.toneColor') for text
-- ('fontTone') and buttons.
data Tone
  = Accent
  | Muted
  | Success
  | Warning
  | Danger
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
data Layout = Layout
  { layoutDirection :: !Direction
  , layoutWidth :: !Sizing
  , layoutHeight :: !Sizing
  , layoutPadding :: {-# UNPACK #-} !Padding
  , layoutGap :: {-# UNPACK #-} !Float
  , layoutPointer :: {-# UNPACK #-} !PointerMode
  -- ^ Placed next to 'layoutGap' so both share one word.
  , layoutLineAlign :: {-# UNPACK #-} !LineAlign
  -- ^ Also in the 'layoutGap' word.
  , layoutTextBreak :: {-# UNPACK #-} !TextBreak
  -- ^ Where rich text may wrap ('textBreak'). Also in the 'layoutGap' word.
  , layoutEllipsis :: {-# UNPACK #-} !TextEllipsis
  -- ^ Where truncated single-line text places its ellipsis. Completes the 'layoutGap' word.
  , layoutAlignX :: !AlignX
  , layoutAlignY :: !AlignY
  , layoutItemsAlign :: {-# UNPACK #-} !ItemsAlign
  -- ^ Cross-axis alignment applied to children without an explicit alignment ('itemsAlign').
  , layoutAlignExplicit :: {-# UNPACK #-} !Word8
  -- ^ Bit 0: 'layoutAlignX' set explicitly; bit 1: 'layoutAlignY' set explicitly.
  , layoutMinW :: {-# UNPACK #-} !Float
  , layoutMinH :: {-# UNPACK #-} !Float
  , layoutMaxW :: {-# UNPACK #-} !Float
  , layoutMaxH :: {-# UNPACK #-} !Float
  , layoutFontVariant :: !FontVariant
  , layoutGridCols :: {-# UNPACK #-} !Int
  , layoutGridMinColW :: {-# UNPACK #-} !Float
  , layoutFontSize :: {-# UNPACK #-} !Float
  , layoutLineGap :: {-# UNPACK #-} !Float
  , layoutAspect :: {-# UNPACK #-} !Float
  , layoutFontColor :: !(Maybe Color)
  , layoutFontTone :: !(Maybe Tone)
  -- ^ Text colour tone, overridden by 'layoutFontColor' ('fontTone').
  , layoutFontWeight :: !FontWeight
  , layoutFontStyle :: !FontStyle
  , layoutTextDecoration :: !TextDecoration
  , layoutFontTabular :: !Bool
  -- ^ Uniform-width ASCII digits ('fontTabular').
  , layoutFlow :: !Flow
  , layoutPin :: !(Maybe V2)
  }
  deriving (Eq, Show)

-- | Fit-sized column with 3-pixel padding, an 8-pixel gap, top-left alignment,
-- and the regular font.
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
    , layoutItemsAlign = ItemsStart
    , layoutAlignExplicit = 0
    , layoutMinW = 0
    , layoutMinH = 0
    , layoutMaxW = noLimit
    , layoutMaxH = noLimit
    , layoutFontVariant = FontRegular
    , layoutGridCols = 0
    , layoutGridMinColW = 0
    , layoutFontSize = 0
    , layoutFontColor = Nothing
    , layoutFontTone = Nothing
    , layoutFontWeight = WeightNormal
    , layoutFontStyle = FontStyleNormal
    , layoutTextDecoration = DecorationNone
    , layoutFontTabular = False
    , layoutFlow = Line
    , layoutLineGap = -1
    , layoutLineAlign = LinesStart
    , layoutTextBreak = BreakWords
    , layoutEllipsis = EllipsisEnd
    , layoutAspect = 0
    , layoutPin = Nothing
    , layoutPointer = PointerAuto
    }

-- | Set all four padding edges in logical pixels.
{-# INLINE padAll #-}
padAll :: Float -> Layout -> Layout
padAll n l = l {layoutPadding = Padding n n n n}

-- | Set horizontal and vertical padding, in that order, in logical pixels.
{-# INLINE padXY #-}
padXY :: Float -> Float -> Layout -> Layout
padXY x y l = l {layoutPadding = Padding x x y y}

-- | Set the top padding in logical pixels, keeping the other three edges.
{-# INLINE padTop #-}
padTop :: Float -> Layout -> Layout
padTop n l = l {layoutPadding = (layoutPadding l) {padT = n}}

-- | Set the bottom padding in logical pixels, keeping the other three edges.
{-# INLINE padBottom #-}
padBottom :: Float -> Layout -> Layout
padBottom n l = l {layoutPadding = (layoutPadding l) {padB = n}}

-- | Set the left padding in logical pixels, keeping the other three edges.
{-# INLINE padLeft #-}
padLeft :: Float -> Layout -> Layout
padLeft n l = l {layoutPadding = (layoutPadding l) {padL = n}}

-- | Set the right padding in logical pixels, keeping the other three edges.
{-# INLINE padRight #-}
padRight :: Float -> Layout -> Layout
padRight n l = l {layoutPadding = (layoutPadding l) {padR = n}}

-- | Set left, right, top and bottom padding, in that order, in logical pixels.
{-# INLINE padLRTB #-}
padLRTB :: Float -> Float -> Float -> Float -> Layout -> Layout
padLRTB left right top bottom l = l {layoutPadding = Padding left right top bottom}

-- | Set the space between children in logical pixels.
{-# INLINE gap #-}
gap :: Float -> Layout -> Layout
gap n l = l {layoutGap = n}

-- | Share available horizontal space with grow weight 1.
{-# INLINE fillW #-}
fillW :: Layout -> Layout
fillW l = l {layoutWidth = Grow 1}

-- | Share available vertical space with grow weight 1.
{-# INLINE fillH #-}
fillH :: Layout -> Layout
fillH l = l {layoutHeight = Grow 1}

-- | Apply 'fillW' and 'fillH'.
{-# INLINE grow #-}
grow :: Layout -> Layout
grow = fillW . fillH

-- | Set the minimum width in logical pixels without changing the sizing mode.
{-# INLINE minW #-}
minW :: Float -> Layout -> Layout
minW n l = l {layoutMinW = explicitMin n}

-- | Set the maximum width in logical pixels without changing the sizing mode.
{-# INLINE maxW #-}
maxW :: Float -> Layout -> Layout
maxW n l = l {layoutMaxW = n}

-- | Fix width and both width limits to the given logical-pixel value.
{-# INLINE fixedW #-}
fixedW :: Float -> Layout -> Layout
fixedW n l = l {layoutWidth = Fixed n, layoutMinW = n, layoutMaxW = n}

-- | Set the minimum height in logical pixels without changing the sizing mode.
{-# INLINE minH #-}
minH :: Float -> Layout -> Layout
minH n l = l {layoutMinH = explicitMin n}

explicitMin :: Float -> Float
explicitMin n = if n == 0 then -0.0 else n

-- | Whether a minimum was set on purpose ('explicitMin').
{-# INLINE hasExplicitMin #-}
hasExplicitMin :: Float -> Bool
hasExplicitMin m = m > 0 || isNegativeZero m

-- | Set the maximum height in logical pixels without changing the sizing mode.
{-# INLINE maxH #-}
maxH :: Float -> Layout -> Layout
maxH n l = l {layoutMaxH = n}

-- | Request a fixed height in logical pixels, retaining existing height limits.
{-# INLINE fixedH #-}
fixedH :: Float -> Layout -> Layout
fixedH n l = l {layoutHeight = Fixed n}

-- | Fix width and height in logical pixels: @fixedW w . fixedH h@.
{-# INLINE fixedWH #-}
fixedWH :: Float -> Float -> Layout -> Layout
fixedWH w h = fixedW w . fixedH h

-- | Centre vertically in the available space.
{-# INLINE alignMid #-}
alignMid :: Layout -> Layout
alignMid l = l {layoutAlignY = AlignMiddle, layoutAlignExplicit = layoutAlignExplicit l .|. 2}

-- | Align to the right edge of the available space.
{-# INLINE alignEnd #-}
alignEnd :: Layout -> Layout
alignEnd l = l {layoutAlignX = AlignEnd, layoutAlignExplicit = layoutAlignExplicit l .|. 1}

-- | Remove all padding, retaining the gap between children.
{-# INLINE tight #-}
tight :: Layout -> Layout
tight l = l {layoutPadding = Padding 0 0 0 0}

-- | Set width as a percentage of the parent's available width: 100 means all.
{-# INLINE percent #-}
percent :: Float -> Layout -> Layout
percent p l = l {layoutWidth = Percent p}

-- | Set the minimum column width for an adaptive grid, in logical pixels.
{-# INLINE gridMinColW #-}
gridMinColW :: Float -> Layout -> Layout
gridMinColW w l = l {layoutGridMinColW = max 0 w}

-- | Fixed width and width/height ratio; the height is derived. A ratio that
-- is not positive and finite gives a square, as 'aspect' ignores one.
fixedAspectW :: Float -> Float -> Layout -> Layout
fixedAspectW w ratio = fixedWH w (w / validRatio ratio)

-- | Fixed height and width/height ratio; the width is derived. A ratio that
-- is not positive and finite gives a square.
fixedAspectH :: Float -> Float -> Layout -> Layout
fixedAspectH h ratio = fixedWH (h * validRatio ratio) h

-- | The ratio when it is positive and finite, otherwise 1.
validRatio :: Float -> Float
validRatio r = if r > 0 && not (isInfinite r) then r else 1

-- | Keep width / height at @ratio@.
aspect :: Float -> Layout -> Layout
aspect ratio l = l {layoutAspect = if ratio > 0 && not (isInfinite ratio) then ratio else 0}

-- | Set the grid column count, clamping negative counts to zero.
{-# INLINE gridCols #-}
gridCols :: Int -> Layout -> Layout
gridCols n l = l {layoutGridCols = max 0 n}

-- | Select the regular font variant.
{-# INLINE fontRegular #-}
fontRegular :: Layout -> Layout
fontRegular l = l {layoutFontVariant = FontRegular}

-- | Text set with medium weight for headings.
{-# INLINE fontHeading #-}
fontHeading :: Layout -> Layout
fontHeading = fontMedium

-- | Text in the theme's muted colour: 'fontTone' 'Muted'.
{-# INLINE fontMuted #-}
fontMuted :: Layout -> Layout
fontMuted = fontTone Muted

-- | Select the backend's monospace font variant.
{-# INLINE fontMono #-}
fontMono :: Layout -> Layout
fontMono l = l {layoutFontVariant = FontMono}

-- | Text in the theme's danger colour: 'fontTone' 'Danger'.
{-# INLINE fontDanger #-}
fontDanger :: Layout -> Layout
fontDanger = fontTone Danger

-- | Colour text with a tone. 'fontColor' takes precedence.
{-# INLINE fontTone #-}
fontTone :: Tone -> Layout -> Layout
fontTone t l = l {layoutFontTone = Just t}

-- | Set logical font size. Non-positive values select the backend default.
{-# INLINE fontSize #-}
fontSize :: Float -> Layout -> Layout
fontSize sz l = l {layoutFontSize = max 0 sz}

-- | Multiply an explicit font size, or 16 when none is set, by a scale factor.
fontSizeScale :: Float -> Layout -> Layout
fontSizeScale s l = fontSize ((if layoutFontSize l > 0 then layoutFontSize l else 16) * s) l

-- | Override the theme's text colour for this node.
{-# INLINE fontColor #-}
fontColor :: Color -> Layout -> Layout
fontColor col l = l {layoutFontColor = Just col}

-- | Set the requested font weight independently of its semantic variant.
{-# INLINE fontWeight #-}
fontWeight :: FontWeight -> Layout -> Layout
fontWeight w l = l {layoutFontWeight = w}

-- | Request 'WeightBold'.
{-# INLINE fontBold #-}
fontBold :: Layout -> Layout
fontBold = fontWeight WeightBold

-- | Request 'WeightLight'.
{-# INLINE fontLight #-}
fontLight :: Layout -> Layout
fontLight = fontWeight WeightLight

-- | Request 'WeightMedium'.
{-# INLINE fontMedium #-}
fontMedium :: Layout -> Layout
fontMedium = fontWeight WeightMedium

-- | Request 'WeightSemiBold'.
{-# INLINE fontSemiBold #-}
fontSemiBold :: Layout -> Layout
fontSemiBold = fontWeight WeightSemiBold

-- | Request 'WeightExtraBold'.
{-# INLINE fontExtraBold #-}
fontExtraBold :: Layout -> Layout
fontExtraBold = fontWeight WeightExtraBold

-- | Request 'WeightBlack', the heaviest weight.
{-# INLINE fontBlack #-}
fontBlack :: Layout -> Layout
fontBlack = fontWeight WeightBlack

-- | Set upright, italic, or oblique text without changing its weight.
{-# INLINE fontStyle #-}
fontStyle :: FontStyle -> Layout -> Layout
fontStyle s l = l {layoutFontStyle = s}

-- | Request 'FontStyleItalic'.
{-# INLINE fontItalic #-}
fontItalic :: Layout -> Layout
fontItalic = fontStyle FontStyleItalic

-- | Request 'FontStyleOblique'.
{-# INLINE fontOblique #-}
fontOblique :: Layout -> Layout
fontOblique = fontStyle FontStyleOblique

-- | Replace the node's underline and strikethrough settings.
{-# INLINE textDecoration #-}
textDecoration :: TextDecoration -> Layout -> Layout
textDecoration d l = l {layoutTextDecoration = d}

-- | Add an underline while preserving any strikethrough.
{-# INLINE fontUnderline #-}
fontUnderline :: Layout -> Layout
fontUnderline l = l {layoutTextDecoration = addUnderline (layoutTextDecoration l)}

-- | The decoration with an underline added, keeping any strikethrough.
{-# INLINE addUnderline #-}
addUnderline :: TextDecoration -> TextDecoration
addUnderline d = toEnum (fromEnum d .|. 1)

-- | Add a strikethrough while preserving any underline.
{-# INLINE fontStrike #-}
fontStrike :: Layout -> Layout
fontStrike l =
  l {layoutTextDecoration = toEnum (fromEnum (layoutTextDecoration l) .|. 2)}

-- | Align to the left edge of the available space.
{-# INLINE alignStart #-}
alignStart :: Layout -> Layout
alignStart l = l {layoutAlignX = AlignStart, layoutAlignExplicit = layoutAlignExplicit l .|. 1}

-- | Centre horizontally in the available space.
{-# INLINE alignCenter #-}
alignCenter :: Layout -> Layout
alignCenter l = l {layoutAlignX = AlignCenter, layoutAlignExplicit = layoutAlignExplicit l .|. 1}

-- | Align to the top edge of the available space.
{-# INLINE alignTop #-}
alignTop :: Layout -> Layout
alignTop l = l {layoutAlignY = AlignTop, layoutAlignExplicit = layoutAlignExplicit l .|. 2}

-- | Align to the bottom edge of the available space.
{-# INLINE alignBottom #-}
alignBottom :: Layout -> Layout
alignBottom l = l {layoutAlignY = AlignBottom, layoutAlignExplicit = layoutAlignExplicit l .|. 2}

-- | Sit on the row's shared text baseline.
{-# INLINE alignBaseline #-}
alignBaseline :: Layout -> Layout
alignBaseline l = l {layoutAlignY = AlignBaseline, layoutAlignExplicit = layoutAlignExplicit l .|. 2}

-- | Cross-axis alignment a container applies to children that do not set their
-- own alignment along that axis ('itemsAlign').
newtype ItemsAlign = ItemsAlign Word8
  deriving newtype (Eq, Ord)
  deriving (Bounded, Enum) via BoundedByte 3

-- | Top of a row or grid row, left of a column. The default.
pattern ItemsStart :: ItemsAlign
pattern ItemsStart = ItemsAlign 0

-- | Centred across the container: vertically in a row or grid row,
-- horizontally in a column.
pattern ItemsCenter :: ItemsAlign
pattern ItemsCenter = ItemsAlign 1

-- | Bottom of a row or grid row, right of a column.
pattern ItemsEnd :: ItemsAlign
pattern ItemsEnd = ItemsAlign 2

-- | Shared text baseline across a row. Outside a row, behaves like
-- 'ItemsStart'.
pattern ItemsBaseline :: ItemsAlign
pattern ItemsBaseline = ItemsAlign 3

{-# COMPLETE ItemsStart, ItemsCenter, ItemsEnd, ItemsBaseline #-}

instance Show ItemsAlign where
  show ItemsStart = "ItemsStart"
  show ItemsCenter = "ItemsCenter"
  show ItemsEnd = "ItemsEnd"
  show ItemsBaseline = "ItemsBaseline"

-- | Set cross-axis alignment for direct children that leave their own
-- cross-axis alignment unset.
{-# INLINE itemsAlign #-}
itemsAlign :: ItemsAlign -> Layout -> Layout
itemsAlign a l = l {layoutItemsAlign = a}

-- | Align children to the start of the cross axis: top of a row, left of a column.
{-# INLINE itemsStart #-}
itemsStart :: Layout -> Layout
itemsStart = itemsAlign ItemsStart

-- | Centre children across the container: vertically in a row or grid row,
-- horizontally in a column.
{-# INLINE itemsCenter #-}
itemsCenter :: Layout -> Layout
itemsCenter = itemsAlign ItemsCenter

-- | Align children to the end of the cross axis: bottom of a row or grid row,
-- right of a column.
{-# INLINE itemsEnd #-}
itemsEnd :: Layout -> Layout
itemsEnd = itemsAlign ItemsEnd

-- | Align row children on their shared text baseline. Outside a row, behaves
-- like 'itemsStart'.
{-# INLINE itemsBaseline #-}
itemsBaseline :: Layout -> Layout
itemsBaseline = itemsAlign ItemsBaseline

-- | Start a new line when the next child would overflow the main axis ('Wrap').
{-# INLINE wrap #-}
wrap :: Layout -> Layout
wrap l = l {layoutFlow = Wrap}

-- | Stack children in the content box instead of a line ('Layered').
{-# INLINE layered #-}
layered :: Layout -> Layout
layered l = l {layoutFlow = Layered}

-- | Space between a wrapping container's lines, in logical pixels.
{-# INLINE lineGap #-}
lineGap :: Float -> Layout -> Layout
lineGap n l = l {layoutLineGap = max 0 n}

-- | Position of a wrapping container's short lines along the main axis.
{-# INLINE lineAlign #-}
lineAlign :: LineAlign -> Layout -> Layout
lineAlign a l = l {layoutLineAlign = a}

-- | Shared 'Bounded' and 'Enum' implementation for 'Word8'-backed enum
-- newtypes with values @[0 .. n]@.
newtype BoundedByte (n :: Nat) = BoundedByte Word8
  deriving newtype (Eq, Ord)

instance KnownNat n => Bounded (BoundedByte n) where
  minBound = BoundedByte 0
  maxBound = BoundedByte (fromIntegral (natVal (Proxy @n)))

instance KnownNat n => Enum (BoundedByte n) where
  fromEnum (BoundedByte x) = fromIntegral x
  toEnum i
    | i >= 0 && i <= hi = BoundedByte (fromIntegral i)
    | otherwise = toEnumError "BoundedByte" i (0 :: Int, hi)
    where
      hi = fromIntegral (natVal (Proxy @n))
  succ a = if a >= maxBound then succError "BoundedByte" else toEnum (fromEnum a + 1)
  pred a = if a <= minBound then predError "BoundedByte" else toEnum (fromEnum a - 1)
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

-- | Line position for 'lineAlign': 'LinesStart', 'LinesCenter' or 'LinesEnd'.
newtype LineAlign = LineAlign Word8
  deriving newtype (Eq, Ord)
  deriving (Bounded, Enum) via BoundedByte 2

-- | Left of a row, top of a column.
pattern LinesStart :: LineAlign
pattern LinesStart = LineAlign 0

-- | Centred on the main axis.
pattern LinesCenter :: LineAlign
pattern LinesCenter = LineAlign 1

-- | Right of a row, bottom of a column.
pattern LinesEnd :: LineAlign
pattern LinesEnd = LineAlign 2

{-# COMPLETE LinesStart, LinesCenter, LinesEnd #-}

instance Show LineAlign where
  show LinesStart = "LinesStart"
  show LinesCenter = "LinesCenter"
  show LinesEnd = "LinesEnd"

-- | Fraction of a line's free space placed before it: 0, 0.5 or 1.
lineAlignFraction :: LineAlign -> Float
lineAlignFraction (LineAlign n) = min 1 (fromIntegral n / 2)

-- | Take the node out of its parent's flow and draw it over its siblings in
-- the parent's content box.
pinAt :: Float -> Float -> Layout -> Layout
pinAt x y l = l {layoutPin = Just (V2 x y)}

-- | Pointer handling where a node overlaps others, through layers or 'pinAt'.
newtype PointerMode = PointerMode Word8
  deriving newtype (Eq, Ord)
  deriving (Bounded, Enum) via BoundedByte 2

-- | Controls take the pointer from what they cover; passive containers pass it through.
pattern PointerAuto :: PointerMode
pattern PointerAuto = PointerMode 0

-- | The node's box blocks presses, wheel and hover from reaching anything beneath.
pattern PointerBlock :: PointerMode
pattern PointerBlock = PointerMode 1

-- | The node and its descendants ignore the pointer entirely.
pattern PointerPass :: PointerMode
pattern PointerPass = PointerMode 2

{-# COMPLETE PointerAuto, PointerBlock, PointerPass #-}

instance Show PointerMode where
  show PointerAuto = "PointerAuto"
  show PointerBlock = "PointerBlock"
  show PointerPass = "PointerPass"

-- | Set how the node handles the pointer where it overlaps others.
pointer :: PointerMode -> Layout -> Layout
pointer m l = l {layoutPointer = m}

-- | Where rich text may wrap a line: 'BreakWords' or 'BreakAnywhere'.
newtype TextBreak = TextBreak Word8
  deriving newtype (Eq, Ord)
  deriving (Bounded, Enum) via BoundedByte 1

-- | Wrap at spaces and newlines.
pattern BreakWords :: TextBreak
pattern BreakWords = TextBreak 0

-- | Wrap between any two characters.
pattern BreakAnywhere :: TextBreak
pattern BreakAnywhere = TextBreak 1

{-# COMPLETE BreakWords, BreakAnywhere #-}

instance Show TextBreak where
  show b = if b == BreakWords then "BreakWords" else "BreakAnywhere"

-- | Set where rich text may wrap its lines.
textBreak :: TextBreak -> Layout -> Layout
textBreak b l = l {layoutTextBreak = b}

-- | Wrap rich text between any two characters, as for code.
breakAnywhere :: Layout -> Layout
breakAnywhere = textBreak BreakAnywhere

-- | Render ASCII digits (@0@ to @9@) with uniform tabular advance widths so live
-- counters and aligned numeric columns never jitter horizontally, even in a
-- proportional UI font.
{-# INLINE fontTabular #-}
fontTabular :: Layout -> Layout
fontTabular l = l {layoutFontTabular = True}

-- | Where single-line text places its ellipsis (@...@) when truncated to fit.
newtype TextEllipsis = TextEllipsis Word8
  deriving newtype (Eq, Ord)
  deriving (Bounded, Enum) via BoundedByte 2

-- | Truncate at the trailing end (@"ghc-internal:GHC..."@). The default.
pattern EllipsisEnd :: TextEllipsis
pattern EllipsisEnd = TextEllipsis 0

-- | Truncate in the middle (@"ghc-internal:...Natural.add"@), preserving both
-- package/path prefix and identifier/file suffix.
pattern EllipsisMiddle :: TextEllipsis
pattern EllipsisMiddle = TextEllipsis 1

-- | Truncate at the leading start (@"...Bignum.Natural.add"@).
pattern EllipsisStart :: TextEllipsis
pattern EllipsisStart = TextEllipsis 2

{-# COMPLETE EllipsisEnd, EllipsisMiddle, EllipsisStart #-}

instance Show TextEllipsis where
  show EllipsisEnd = "EllipsisEnd"
  show EllipsisMiddle = "EllipsisMiddle"
  show EllipsisStart = "EllipsisStart"

-- | Set where truncated single-line text places its ellipsis.
{-# INLINE textEllipsis #-}
textEllipsis :: TextEllipsis -> Layout -> Layout
textEllipsis e l = l {layoutEllipsis = e}

-- | Truncate single-line text at the trailing end.
{-# INLINE ellipsisEnd #-}
ellipsisEnd :: Layout -> Layout
ellipsisEnd = textEllipsis EllipsisEnd

-- | Truncate single-line text in the middle, keeping prefix and suffix visible.
{-# INLINE ellipsisMiddle #-}
ellipsisMiddle :: Layout -> Layout
ellipsisMiddle = textEllipsis EllipsisMiddle

-- | Truncate single-line text at the leading start, keeping the suffix visible.
{-# INLINE ellipsisStart #-}
ellipsisStart :: Layout -> Layout
ellipsisStart = textEllipsis EllipsisStart
