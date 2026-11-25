-- | Widget labels, numeric/colour formatting, and packed node-style encodings
-- shared by construction, layout, and painting.
module NanoUI.Internal.WidgetText
  ( treeEncodeStyle
  , treeDecodeStyle
  , treeDecodeStripe
  , textInputFieldText
  , textInputMinWidth
  , textInputFieldHeight
  , textInputFlagSearch
  , textInputFlagSelectable
  , textInputFlagPassword
  , textInputFlagNumeric
  , numericStepperW
  , numericTextClip
  , numericStepperRects
  , comboTextClip
  , searchInputReserveW
  , searchInputTextClip
  , textClipBetween
  , searchInputIconRects
  , selectDisplayText
  , selectChevronReserve
  , selectChevronCenterX
  , colorPickerGap
  , colorPickerSvH
  , colorToHex
  , colorToHexA
  , colorFromHex
  , colorPickerParseHex
  , buttonFlagClose
  , buttonCloseTrailing
  , buttonCloseTab
  , buttonFlagTab
  , buttonFlagTable
  , buttonFlagMenu
  , buttonFlagMenuBar
  , buttonFlagContent
  , buttonFlagChoice
  , buttonFlagRow
  , ButtonKind (..)
  , buttonKindOf
  , containerFlagInert
  , containerFlagClip
  , containerFlagShaderEffect
  , containerFlagButtonGroup
  , hasFlag
  , tableSortLane
  , tableHeaderPadX
  , tableStripeColor
  , rowStripe
  , stripeBits
  , panelSurface
  , tableStripeInset
  , stripeColor
  , packTextNodeStyle
  , textNodeFontKey
  , textNodeFontVariant
  , textNodeFontTone
  , textNodeFontWeight
  , textNodeFontStyle
  , textNodeTextDecoration
  , textNodeFontTabular
  , textNodeEllipsis
  , tableSortMarkOf
  , buttonVisualStyle
  , labelFontStyle
  , labelColor
  , buttonToneStyle
  , buttonToneOf
  , tabEncodeStyle
  , tabDecodeStyle
  , tabLabelAtStart
  , tabHeaderPadX
  , TabChrome (..)
  , tabChromeEncode
  , tabChromeDecode
  ) where

import Control.Applicative ((<|>))
import Data.Bits ((.&.), (.|.), finiteBitSize, shiftL, shiftR, xor)
import Data.Char (digitToInt, isHexDigit)
import Data.Maybe (fromMaybe)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, smallArrayFromList)
import Data.Text (Text)
import Data.Word (Word8, Word64)
import GHC.Float (castFloatToWord32)
import Numeric (showHex)
import NanoUI.Internal.Font (FontMetrics (..), controlPadY, fmLineHeight, tableCellInset, widgetContentInset)
import NanoUI.Internal.Layout.Arena (labelFontShift)
import NanoUI.Internal.StyleBits (BitField (..))
import qualified NanoUI.Internal.StyleBits as SB
import NanoUI.Internal.Style (FontStyle (..), FontVariant (..), FontWeight (..), Layout (..), Style (..), TextDecoration (..), TextEllipsis (..), Theme (..), Tone (..), toneColor, variantFace, variantTone)
import NanoUI.Internal.Types (Color (..), Rect (..), clamp, colorA, colorB, colorG, colorR, colorRGBA, lerpColor)
import qualified Data.Text as T

-- | A tree row's button style: 'buttonFlagRow', its depth
-- ('SB.treeDepth'), and whether it has children ('SB.treeHasKids'), is
-- expanded ('SB.treeExpanded') and is an odd row ('SB.treeOdd').
treeEncodeStyle :: Int -> Bool -> Bool -> Bool -> Word64
treeEncodeStyle depth hasKids expanded isOdd =
  buttonFlagRow
    .|. (if isOdd then SB.fieldMask SB.treeOdd else 0)
    .|. (if expanded then SB.fieldMask SB.treeExpanded else 0)
    .|. (if hasKids then SB.fieldMask SB.treeHasKids else 0)
    .|. SB.packField SB.treeDepth depth

-- | A tree row's depth, and whether it has children and is expanded.
treeDecodeStyle :: Word64 -> (Int, Bool, Bool)
treeDecodeStyle s = (SB.getField SB.treeDepth s, SB.testField SB.treeHasKids s, SB.testField SB.treeExpanded s)

-- | A tree row's stripe code for 'stripeColor', from its odd-row bit.
treeDecodeStripe :: Word64 -> Int
treeDecodeStripe s = if SB.testField SB.treeOdd s then 2 else 1

textInputMinWidth :: Float
textInputMinWidth = 160

textInputFieldHeight :: FontMetrics -> Float
textInputFieldHeight fm = fmLineHeight fm + controlPadY fm

-- | Search-field icon geometry. Returns
-- (icon diameter, outer pad, left chrome lead, right chrome tail). The lead/tail
-- are the horizontal space the magnifier / clear buttons reserve either side of
-- the editable text.
searchInputChrome :: FontMetrics -> (Float, Float, Float, Float)
searchInputChrome fm =
  let (ix, _) = widgetContentInset fm
      s = clamp 12 15 (fmLineHeight fm * 0.8)
      pad = fmAdvance fm ' ' * 0.6
   in (s, ix, ix + s + pad, pad + s + ix)

-- | Total horizontal chrome a caption-less search box reserves for its icons.
searchInputReserveW :: FontMetrics -> Float
searchInputReserveW fm =
  let (_, _, lead, tailw) = searchInputChrome fm
   in lead + tailw

-- | Region a caption-less search field's editable text may occupy. Excludes the
-- magnifier on the left and the clear slot on the right.
searchInputTextClip :: FontMetrics -> Float -> Float -> Float -> Float -> Rect
searchInputTextClip fm x y w h =
  let (_, _, lead, tailw) = searchInputChrome fm
   in textClipBetween fm lead tailw x y w h

-- | A field's text region: inside the vertical content inset, between @lead@
-- from the left edge and @trail@ from the right.
textClipBetween :: FontMetrics -> Float -> Float -> Float -> Float -> Float -> Float -> Rect
textClipBetween fm lead trail x y w h =
  let (_, iy) = widgetContentInset fm
   in Rect (x + lead) (y + iy) (max 0 (w - lead - trail)) (max 0 (h - 2 * iy))

-- | Square slots (magnifier left, clear right) the search icons are drawn in.
searchInputIconRects :: FontMetrics -> Float -> Float -> Float -> Float -> (Rect, Rect)
searchInputIconRects fm x y w h =
  let (s, ix, _, _) = searchInputChrome fm
      cy = y + h / 2
      mag = Rect (x + ix) (cy - s / 2) s s
      clear = Rect (x + w - ix - s) (cy - s / 2) s s
   in (mag, clear)

textInputFieldText :: Text -> Text -> Bool -> Text
textInputFieldText ph value focused
  | T.null value && not focused = ph
  | otherwise = value

-- | Marks a @NodeTextInput@ as a caption-less search field ('SB.inputSearch').
{-# INLINE textInputFlagSearch #-}
textInputFlagSearch :: Word64
textInputFlagSearch = SB.fieldMask SB.inputSearch

-- | Marks a @NodeTextInput@ as a selectable text label: read-only, caption-less,
-- chrome-less, sized to its text content, with mouse drag-to-select and copy.
{-# INLINE textInputFlagSelectable #-}
textInputFlagSelectable :: Word64
textInputFlagSelectable = SB.fieldMask SB.inputSelectable

-- | Marks a @NodeTextInput@ as a password field: its value is displayed masked
-- and is never copied or cut to the clipboard.
{-# INLINE textInputFlagPassword #-}
textInputFlagPassword :: Word64
textInputFlagPassword = SB.fieldMask SB.inputPassword

-- | Marks a @NodeTextInput@ as a numeric field: a caption-less box whose text
-- stops short of an up / down stepper at its right edge.
{-# INLINE textInputFlagNumeric #-}
textInputFlagNumeric :: Word64
textInputFlagNumeric = SB.fieldMask SB.inputNumeric

-- | Width of a numeric field's stepper column.
numericStepperW :: Float
numericStepperW = 18

-- | Region a numeric field's text may occupy: inside the content inset, left
-- of the stepper.
numericTextClip :: FontMetrics -> Float -> Float -> Float -> Float -> Rect
numericTextClip fm x y w h =
  let (ix, _) = widgetContentInset fm
   in textClipBetween fm ix (ix + numericStepperW) x y w h

-- | The up and down halves of a numeric field's stepper.
numericStepperRects :: Float -> Float -> Float -> Float -> (Rect, Rect)
numericStepperRects x y w h =
  let sx = x + w - numericStepperW
      half = h / 2
   in (Rect sx y numericStepperW half, Rect sx (y + half) numericStepperW (h - half))

-- | Region a combo box's editable text may occupy: from the left content inset
-- to the select chevron reserve on the right.
comboTextClip :: FontMetrics -> Float -> Float -> Float -> Float -> Rect
comboTextClip fm x y w h =
  let (ix, _) = widgetContentInset fm
   in textClipBetween fm ix selectChevronReserve x y w h

selectDisplayText :: Text -> Text -> Text
selectDisplayText lbl opt
  | T.null lbl = opt
  | otherwise = lbl <> ": " <> opt

-- Space reserved on the right of a select for the chevron.
selectChevronReserve :: Float
selectChevronReserve = 16

selectChevronCenterX :: Float -> Float -> Float
selectChevronCenterX x w = x + w - selectChevronReserve / 2

colorPickerGap :: Float
colorPickerGap = 4

-- Height of a colour picker's field row; the field grows to a square this tall.
colorPickerSvH :: Float
colorPickerSvH = 250

-- | Lowercase @#rrggbb@ text. Discards alpha; use 'colorToHexA' to retain it.
colorToHex :: Color -> Text
colorToHex c =
  "#" <> hexByte (colorR c) <> hexByte (colorG c) <> hexByte (colorB c)

-- | Lowercase @#rrggbbaa@: 'colorToHex' with the alpha byte.
colorToHexA :: Color -> Text
colorToHexA c = colorToHex c <> hexByte (colorA c)

hexByte :: Word8 -> Text
hexByte n = indexSmallArray hexBytes (fromIntegral n)

-- Each byte's two-character representation is allocated once, shared by
-- color-picker labels instead of formatting fresh Strings every frame.
hexBytes :: SmallArray Text
hexBytes =
  smallArrayFromList
    [T.justifyRight 2 '0' (T.pack (showHex n "")) | n <- [0 .. 255 :: Int]]

-- | Parse a hex colour, accepting an optional leading @#@ and either 6 or 8
-- digits. The fourth component is 'Nothing' for the six-digit form.
colorPickerParseHex :: Text -> Maybe (Word8, Word8, Word8, Maybe Word8)
colorPickerParseHex txt
  | (n == 6 || n == 8) && T.all isHexDigit bare =
      Just (byte 0, byte 2, byte 4, if n == 8 then Just (byte 6) else Nothing)
  | otherwise = Nothing
  where
    bare = T.dropWhile (== '#') (T.strip txt)
    n = T.length bare
    byte i = fromIntegral (digitToInt (T.index bare i) * 16 + digitToInt (T.index bare (i + 1)))

-- | Parse six or eight hex digits as RGB or RGBA. Strips surrounding whitespace
-- and leading @#@ characters; invalid digits/length return 'Nothing'. RGB is opaque.
colorFromHex :: Text -> Maybe Color
colorFromHex txt = do
  (r, g, b, ma) <- colorPickerParseHex txt
  pure (colorRGBA r g b (fromMaybe 255 ma))

-- | A text node's style index: the layout's font face, weight, slant and
-- decoration, a row stripe code (see 'stripeColor'), and the tone (the
-- 'Tone' plus one, or 0 for none). "NanoUI.Internal.StyleBits" places each
-- field. A colour-only variant packs as the regular face plus its tone, so
-- it is measured in the base font.
{-# INLINE packTextNodeStyle #-}
packTextNodeStyle :: Layout -> Int -> Word64
packTextNodeStyle l stripe =
  stripeBits stripe
    .|. SB.packField SB.textFace (fromEnum (variantFace (layoutFontVariant l)))
    .|. SB.packField SB.textWeight (fromEnum (layoutFontWeight l))
    .|. SB.packField SB.textSlant (fromEnum (layoutFontStyle l))
    .|. SB.packField SB.textDecoration (fromEnum (layoutTextDecoration l))
    .|. (if layoutFontTabular l then SB.fieldMask SB.textTabular else 0)
    .|. SB.placeField SB.textTone (maybe 0 ((+ 1) . fromEnum) (layoutFontTone l <|> variantTone (layoutFontVariant l)))
    .|. SB.packField SB.textEllipsis (fromEnum (layoutEllipsis l))

-- | The enum packed in field @f@ of the style bits, or @fallback@ when the
-- field holds no constructor.
{-# INLINE decodeStyleEnum #-}
decodeStyleEnum :: forall a. (Bounded a, Enum a) => BitField -> a -> Word64 -> a
decodeStyleEnum f fallback si =
  let v = SB.getField f si
   in if v >= fromEnum (minBound :: a) && v <= fromEnum (maxBound :: a) then toEnum v else fallback

-- | The tone in a field that holds a 'Tone' plus one, 'Nothing' for 0.
{-# INLINE decodeToneField #-}
decodeToneField :: BitField -> Word64 -> Maybe Tone
decodeToneField f si = case SB.getField f si of
  0 -> Nothing
  v -> Just (decodeStyleEnum (BitField 0 (bfWidth f)) Danger (fromIntegral (v - 1)))

-- | The font a text node of font size @size@ and style index @si@ is measured
-- in, as one key: the size and the variant, weight and slant bits of the
-- style index.
textNodeFontKey :: Float -> Word64 -> Int
textNodeFontKey size si =
  let w = castFloatToWord32 size
      bits = fromIntegral (si .&. SB.textFontKeyMask)
   in if finiteBitSize (0 :: Int) > 32
        then fromIntegral w `shiftL` 18 .|. bits
        else fromIntegral (w `xor` (w `shiftR` 16)) `shiftL` 17 .|. bits

-- | The font variant packed in a text node's style index.
{-# INLINE textNodeFontVariant #-}
textNodeFontVariant :: Word64 -> FontVariant
textNodeFontVariant = decodeStyleEnum SB.textFace FontRegular

-- | The tone packed in a text node's style index, if any.
{-# INLINE textNodeFontTone #-}
textNodeFontTone :: Word64 -> Maybe Tone
textNodeFontTone = decodeToneField SB.textTone

-- | Decode weight bits, falling back to normal for an invalid enum value.
{-# INLINE textNodeFontWeight #-}
textNodeFontWeight :: Word64 -> FontWeight
textNodeFontWeight = decodeStyleEnum SB.textWeight WeightNormal

-- | Decode slant bits, falling back to upright for an invalid enum value.
{-# INLINE textNodeFontStyle #-}
textNodeFontStyle :: Word64 -> FontStyle
textNodeFontStyle = decodeStyleEnum SB.textSlant FontStyleNormal

-- | Decode the underline/strikethrough bits of a text node's style code.
{-# INLINE textNodeTextDecoration #-}
textNodeTextDecoration :: Word64 -> TextDecoration
textNodeTextDecoration = decodeStyleEnum SB.textDecoration DecorationNone

-- | Whether a text node's style requests uniform tabular digit advances.
{-# INLINE textNodeFontTabular #-}
textNodeFontTabular :: Word64 -> Bool
textNodeFontTabular = SB.testField SB.textTabular

-- | Decode where a truncated single-line text node places its ellipsis.
{-# INLINE textNodeEllipsis #-}
textNodeEllipsis :: Word64 -> TextEllipsis
textNodeEllipsis = decodeStyleEnum SB.textEllipsis EllipsisEnd

-- | Row fill for stripe code 1 (even rows) or 2 (odd rows); 0 is unstriped.
{-# INLINE stripeColor #-}
stripeColor :: Theme -> Int -> Maybe Color
stripeColor theme s = case s of
  1 -> Just (lerpColor (styleBg (themePanel theme)) (themeWindow theme) 0.26)
  2 -> Just (lerpColor (styleBg (themePanel theme)) (styleBg (themeButton theme)) 0.55)
  _ -> Nothing

-- | The stripe code of table row @ri@.
{-# INLINE rowStripe #-}
rowStripe :: Int -> Int
rowStripe ri = if even ri then 1 else 2

-- | A stripe code in its style-index bits, as 'tableStripeColor' decodes it.
{-# INLINE stripeBits #-}
stripeBits :: Int -> Word64
stripeBits = SB.placeField SB.stripe

-- | Row fill decoded from a text node's or a panel's style index.
tableStripeColor :: Theme -> Word64 -> Maybe Color
tableStripeColor theme si = stripeColor theme (SB.getField SB.stripe si)

-- | The surface of a panel with style index @si@: the theme's panel, or, for
-- a table's widget cell, its row fill ('tableStripeColor') with no border or
-- rounding.
{-# INLINE panelSurface #-}
panelSurface :: Theme -> Word64 -> Style
panelSurface theme si = case tableStripeColor theme si of
  Just bg -> (themePanel theme) {styleBg = bg, styleBorderWidth = 0, styleCornerRadius = 0}
  Nothing -> themePanel theme

-- | How far the lines of a text node with style index @si@ keep from each
-- side: a table cell's inset when it has a row fill ('tableStripeColor').
{-# INLINE tableStripeInset #-}
tableStripeInset :: Word64 -> Float
tableStripeInset si = case SB.getField SB.stripe si of
  1 -> tableCellInset
  2 -> tableCellInset
  _ -> 0

-- | The lane every table header keeps beside its label for the sort mark,
-- on the side the label does not align to: the right of a left-aligned
-- label, the left of a right-aligned one. Reserved whether or not the column
-- is sorted, so the mark never changes the column's width or covers the
-- label.
tableSortLane :: Float
tableSortLane = 14

-- | A table header's horizontal padding: the cell inset on both sides and
-- the sort mark's lane.
tableHeaderPadX :: Float
tableHeaderPadX = 2 * tableCellInset + tableSortLane

-- | The sort direction a table header's style packs ('SB.headerSortMark'):
-- 0 none, 1 ascending, 2 descending.
{-# INLINE tableSortMarkOf #-}
tableSortMarkOf :: Word64 -> Int
tableSortMarkOf = SB.getField SB.headerSortMark

-- Type flags live in bits 24-31 so visual style and tab index stay in the
-- low bits ("NanoUI.Internal.StyleBits").
{-# INLINE buttonFlagClose #-}
buttonFlagClose :: Word64
buttonFlagClose = SB.fieldMask SB.buttonClose

-- | Visual style of a title-bar close button: its cross sits against the
-- box's right edge, so it lines up with the panel padding the way the title
-- does on the left.
buttonCloseTrailing :: Word64
buttonCloseTrailing = 1

-- | Visual style of a tab header's close button: its cross in the header's
-- label colour ('NanoUI.Internal.Style.fontColor'), dimmed at rest, on a
-- round fill while hovered.
buttonCloseTab :: Word64
buttonCloseTab = 2

{-# INLINE buttonFlagTab #-}
buttonFlagTab :: Word64
buttonFlagTab = SB.fieldMask SB.buttonTab

{-# INLINE buttonFlagTable #-}
buttonFlagTable :: Word64
buttonFlagTable = SB.fieldMask SB.buttonTable

-- Flat menu row / menu-bar entry: transparent at rest, hover highlight, and an
-- accent marker on hover. Rendered by 'menuItemVisualStyle'.
{-# INLINE buttonFlagMenu #-}
buttonFlagMenu :: Word64
buttonFlagMenu = SB.fieldMask SB.buttonMenu

-- Flat menu-bar title: same flat/hover/open fill as a menu row, but centered
-- text and no hover accent marker (that marker belongs to drop-down rows).
{-# INLINE buttonFlagMenuBar #-}
buttonFlagMenuBar :: Word64
buttonFlagMenuBar = SB.fieldMask SB.buttonMenuBar

-- | A button whose content is a view of its own ('NanoUI.buttonContent'),
-- padded as a labelled button is.
{-# INLINE buttonFlagContent #-}
buttonFlagContent :: Word64
buttonFlagContent = SB.fieldMask SB.buttonContent

-- | A checkbox, or with visual style 1 a radio option: no fill, border or
-- padding, its box or ring before its label, checked while its value is over
-- a half.
{-# INLINE buttonFlagChoice #-}
buttonFlagChoice :: Word64
buttonFlagChoice = SB.fieldMask SB.buttonChoice

-- | A tree row: square and borderless, its stripe's fill, or the accent's
-- tint while its value is over a half. Its style packs the rest
-- ('treeEncodeStyle').
{-# INLINE buttonFlagRow #-}
buttonFlagRow :: Word64
buttonFlagRow = SB.fieldMask SB.buttonRow

-- | Mutually exclusive visual role of a 'NodeButton' decoded from its style flags.
data ButtonKind
  = ButtonPlain
  | ButtonClose
  | ButtonTab
  | ButtonTable
  | ButtonMenu
  | ButtonMenuBar
  | ButtonContent
  | ButtonChoice
  | ButtonRow
  deriving (Eq, Show, Enum, Bounded)

-- | Decode a button's visual role from its packed style flags. Inlined so
-- case analysis compiles to direct bit-test jumps with zero allocation.
{-# INLINE buttonKindOf #-}
buttonKindOf :: Word64 -> ButtonKind
buttonKindOf si
  | hasFlag buttonFlagClose si = ButtonClose
  | hasFlag buttonFlagTab si = ButtonTab
  | hasFlag buttonFlagTable si = ButtonTable
  | hasFlag buttonFlagMenu si = ButtonMenu
  | hasFlag buttonFlagMenuBar si = ButtonMenuBar
  | hasFlag buttonFlagContent si = ButtonContent
  | hasFlag buttonFlagChoice si = ButtonChoice
  | hasFlag buttonFlagRow si = ButtonRow
  | otherwise = ButtonPlain

-- | Marks a @NodeContainer@ whose widgets are for display: the pointer passes
-- through them to the widget they are drawn in
-- ('NanoUI.Internal.Frame.Hit.innermostHit').
{-# INLINE containerFlagInert #-}
containerFlagInert :: Word64
containerFlagInert = SB.fieldMask SB.containerInert

-- | Marks a @NodeContainer@ that clips its children to its rect, for paint,
-- text spans and the pointer, as a panel does.
{-# INLINE containerFlagClip #-}
containerFlagClip :: Word64
containerFlagClip = SB.fieldMask SB.containerClip

-- | Marks a @NodeContainer@ that wraps its children in a Tier 2 shader effect.
{-# INLINE containerFlagShaderEffect #-}
containerFlagShaderEffect :: Word64
containerFlagShaderEffect = SB.fieldMask SB.containerShaderEffect

-- | Marks a @NodeContainer@ that joins adjacent buttons ('NanoUI.buttonGroup').
{-# INLINE containerFlagButtonGroup #-}
containerFlagButtonGroup :: Word64
containerFlagButtonGroup = SB.fieldMask SB.containerButtonGroup

-- | Whether the packed style index @si@ carries @flag@.
{-# INLINE hasFlag #-}
hasFlag :: Word64 -> Word64 -> Bool
hasFlag flag si = si .&. flag /= 0

-- | A button's own style bits ('SB.buttonVisual'): those below the type
-- flags, leaving out the label font above them ('labelFontStyle').
{-# INLINE buttonVisualStyle #-}
buttonVisualStyle :: Word64 -> Word64
buttonVisualStyle si = si .&. SB.fieldMask SB.buttonVisual

-- | The colour of a widget label in font bits @font@ ('nodeFontBits') on a
-- surface of @style@: the layout's 'NanoUI.Internal.Style.fontColor', else
-- its tone's, else the surface's text colour.
{-# INLINE labelColor #-}
labelColor :: Theme -> Style -> Word64 -> Maybe Color -> Color
labelColor theme style font = fromMaybe (maybe (styleFg style) (toneColor theme) (textNodeFontTone font))

-- | A button's or select's label font from its layout: the text-node packing
-- ('packTextNodeStyle') moved up to 'labelFontShift', for 'nodeFontBits' to
-- read back.
{-# INLINE labelFontStyle #-}
labelFontStyle :: Layout -> Word64
labelFontStyle l = packTextNodeStyle l 0 `shiftL` labelFontShift

-- | A plain button's packed style carrying the 'Tone' it fills with while
-- its value is on ('SB.buttonTone', a toggle button's).
{-# INLINE buttonToneStyle #-}
buttonToneStyle :: Tone -> Word64
buttonToneStyle t = SB.placeField SB.buttonTone (fromEnum t + 1)

-- | The tone 'buttonToneStyle' packed, 'Nothing' for the accent.
{-# INLINE buttonToneOf #-}
buttonToneOf :: Word64 -> Maybe Tone
buttonToneOf = decodeToneField SB.buttonTone

-- | A tab header's packed button style: the strip's tab style (0-3,
-- 'SB.tabStyle'), its orientation (0-3, 'SB.tabOrient'), and
-- 'buttonFlagTab'.
{-# INLINE tabEncodeStyle #-}
tabEncodeStyle :: Int -> Int -> Word64
tabEncodeStyle style orient = SB.packField SB.tabStyle style .|. SB.packField SB.tabOrient orient .|. buttonFlagTab

-- | The tab style and orientation of a tab header's packed style.
{-# INLINE tabDecodeStyle #-}
tabDecodeStyle :: Word64 -> (Int, Int)
tabDecodeStyle si = (SB.getField SB.tabStyle si, SB.getField SB.tabOrient si)

-- | Space either side of a tab header's label and adornments.
tabHeaderPadX :: Float
tabHeaderPadX = 12

-- | Whether a button's label and adornments start at its left padding
-- ('tabHeaderPadX') rather than sit centred: a header of a vertical tab
-- strip.
{-# INLINE tabLabelAtStart #-}
tabLabelAtStart :: Word64 -> Bool
tabLabelAtStart si = hasFlag buttonFlagTab si && snd (tabDecodeStyle si) >= 2

-- | What a tab strip's container paints under its children.
data TabChrome
  = TabChromeNone
  | TabChromeRule
  -- ^ A one-pixel rule along the side facing the body.
  | TabChromeTrack
  -- ^ A rounded track behind segmented headers.
  | TabChromeBody
  -- ^ A contained body's fill, and its border on the three sides away from
  -- the headers.
  deriving (Eq, Show, Enum, Bounded)

-- | A @NodeContainer@'s style for 'TabChrome' @part@ of a strip with tab
-- style @style@ and orientation @orient@ ('SB.chromePart', 'SB.chromeStyle',
-- 'SB.chromeOrient'), clear of 'containerFlagInert'.
{-# INLINE tabChromeEncode #-}
tabChromeEncode :: TabChrome -> Int -> Int -> Word64
tabChromeEncode part style orient =
  SB.placeField SB.chromePart (fromEnum part) .|. SB.packField SB.chromeStyle style .|. SB.packField SB.chromeOrient orient

-- | The chrome part, tab style and orientation a container's style packs.
{-# INLINE tabChromeDecode #-}
tabChromeDecode :: Word64 -> (TabChrome, Int, Int)
tabChromeDecode si =
  (decodeStyleEnum SB.chromePart TabChromeNone si, SB.getField SB.chromeStyle si, SB.getField SB.chromeOrient si)
