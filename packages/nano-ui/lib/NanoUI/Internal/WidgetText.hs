-- | Widget labels, numeric/colour formatting, and packed node-style encodings
-- shared by construction, layout, and painting.
module NanoUI.Internal.WidgetText
  ( intValueText
  , treeEncodeStyle
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
  , buttonFlagTab
  , buttonFlagTable
  , buttonFlagMenu
  , buttonFlagMenuBar
  , buttonFlagContent
  , containerFlagInert
  , hasFlag
  , tableSortReserve
  , tableStripeColor
  , stripeColor
  , packTextNodeStyle
  , textNodeFontKey
  , textNodeFontVariant
  , textNodeFontWeight
  , textNodeFontStyle
  , textNodeTextDecoration
  , tableHeaderLabel
  , tableHeaderDisplayText
  , tableSortMarkOf
  , buttonVisualStyle
  , tabEncodeStyle
  ) where

import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Data.Char (digitToInt, isHexDigit)
import Data.Maybe (fromMaybe)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, smallArrayFromList)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import Data.Word (Word8)
import GHC.Float (castFloatToWord32)
import Numeric (showHex)
import NanoUI.Internal.Font (FontMetrics (..), fmLineHeight, widgetContentInset)
import NanoUI.Internal.Style (FontStyle (..), FontVariant (..), FontWeight (..), Layout (..), TextDecoration (..), Theme (..), styleBg)
import NanoUI.Internal.Types (Color (..), Rect (..), clamp, colorA, colorB, colorG, colorR, colorRGBA, lerpColor)
import qualified Data.Text as T

intValueText :: Int -> Text
intValueText = TL.toStrict . TB.toLazyText . TB.decimal

-- | styleIdx: nodeIdx in bits 11+, depth in 0-7, hasKids bit 8, expanded bit 9, stripeOdd bit 10.
treeEncodeStyle :: Int -> Int -> Bool -> Bool -> Bool -> Int
treeEncodeStyle nodeIdx depth hasKids expanded isOdd =
  (nodeIdx `shiftL` 11)
    .|. (if isOdd then 0x400 else 0)
    .|. (if expanded then 0x200 else 0)
    .|. (if hasKids then 0x100 else 0)
    .|. (depth .&. 0xff)

treeDecodeStyle :: Int -> (Int, Int, Bool, Bool)
treeDecodeStyle s =
  ( s `shiftR` 11
  , s .&. 0xff
  , s .&. 0x100 /= 0
  , s .&. 0x200 /= 0
  )

-- | A tree row's stripe code for 'stripeColor', from its odd-row bit.
treeDecodeStripe :: Int -> Int
treeDecodeStripe s = if s .&. 0x400 /= 0 then 2 else 1

textInputMinWidth :: Float
textInputMinWidth = 160

textInputFieldPadY :: FontMetrics -> Float
textInputFieldPadY fm = max 3 (fmAdvance fm ' ' * 1.25)

textInputFieldHeight :: FontMetrics -> Float
textInputFieldHeight fm = fmLineHeight fm + 2 * textInputFieldPadY fm

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

-- | Marks a @NodeTextInput@ as a caption-less search field. Lives in the high
-- style bits (like the button flags) so it survives the arena's int storage.
textInputFlagSearch :: Int
textInputFlagSearch = 0x04000000

-- | Marks a @NodeTextInput@ as a selectable text label: read-only, caption-less,
-- chrome-less, sized to its text content, with mouse drag-to-select and copy.
textInputFlagSelectable :: Int
textInputFlagSelectable = 0x10000000

-- | Marks a @NodeTextInput@ as a password field: its value is displayed masked
-- and is never copied or cut to the clipboard.
textInputFlagPassword :: Int
textInputFlagPassword = 0x20000000

-- | Marks a @NodeTextInput@ as a numeric field: a caption-less box whose text
-- stops short of an up / down stepper at its right edge.
textInputFlagNumeric :: Int
textInputFlagNumeric = 0x40000000

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

-- | Eight-digit form for the alpha-aware picker: @#RRGGBBAA@.
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

-- | A text node's style index: the layout's font variant, weight, slant and
-- decoration, and a row stripe code (see 'stripeColor') in bits 4-7.
{-# INLINE packTextNodeStyle #-}
packTextNodeStyle :: Layout -> Int -> Int
packTextNodeStyle l stripe =
  (stripe `shiftL` 4)
    .|. (fromEnum (layoutFontVariant l) .&. 0x0F)
    .|. ((fromEnum (layoutFontWeight l) .&. 0x0F) `shiftL` 8)
    .|. ((fromEnum (layoutFontStyle l) .&. 0x03) `shiftL` 12)
    .|. ((fromEnum (layoutTextDecoration l) .&. 0x03) `shiftL` 14)

-- | The enum packed in the style bits at @shift@ under @mask@, or @fallback@
-- when they hold no constructor.
{-# INLINE decodeStyleEnum #-}
decodeStyleEnum :: forall a. (Bounded a, Enum a) => Int -> Int -> a -> Int -> a
decodeStyleEnum shift mask fallback si =
  let v = (si `shiftR` shift) .&. mask
   in if v >= fromEnum (minBound :: a) && v <= fromEnum (maxBound :: a) then toEnum v else fallback

-- | The font a text node of font size @size@ and style index @si@ is measured
-- in, as one key: the size and the variant, weight and slant bits of the
-- style index.
textNodeFontKey :: Float -> Int -> Int
textNodeFontKey size si = fromIntegral (castFloatToWord32 size) `shiftL` 16 .|. (si .&. 0x3F0F)

{-# INLINE textNodeFontVariant #-}
textNodeFontVariant :: Int -> FontVariant
textNodeFontVariant = decodeStyleEnum 0 0x0F FontRegular

-- | Decode weight bits, falling back to normal for an invalid enum value.
{-# INLINE textNodeFontWeight #-}
textNodeFontWeight :: Int -> FontWeight
textNodeFontWeight = decodeStyleEnum 8 0x0F WeightNormal

-- | Decode slant bits, falling back to upright for an invalid enum value.
{-# INLINE textNodeFontStyle #-}
textNodeFontStyle :: Int -> FontStyle
textNodeFontStyle = decodeStyleEnum 12 0x03 FontStyleNormal

-- | Decode the underline/strikethrough bits of a text node's style code.
{-# INLINE textNodeTextDecoration #-}
textNodeTextDecoration :: Int -> TextDecoration
textNodeTextDecoration = decodeStyleEnum 14 0x03 DecorationNone

-- | Row fill for stripe code 1 (even rows) or 2 (odd rows); 0 is unstriped.
{-# INLINE stripeColor #-}
stripeColor :: Theme -> Int -> Maybe Color
stripeColor theme s = case s of
  1 -> Just (lerpColor (styleBg (themePanel theme)) (themeWindow theme) 0.26)
  2 -> Just (lerpColor (styleBg (themePanel theme)) (styleBg (themeButton theme)) 0.55)
  _ -> Nothing

-- | The row fill a text node's style index asks for.
tableStripeColor :: Theme -> Int -> Maybe Color
tableStripeColor theme si = stripeColor theme ((si `shiftR` 4) .&. 0x0F)

-- | Slot reserved in every header so the sort mark never changes column
-- width: trailing, or leading ('tableSortReserveLead') in a right-aligned
-- column, whose arrow sits on the left.
tableSortReserve :: Text
tableSortReserve = "  ▲"

tableSortReserveLead :: Text
tableSortReserveLead = "▲  "

-- | A header's label with its sort slot, leading when @alignEnd@.
tableHeaderLabel :: Bool -> Text -> Text
tableHeaderLabel alignEnd hdr
  | alignEnd = tableSortReserveLead <> hdr
  | otherwise = hdr <> tableSortReserve

-- | Sort direction encoded for a table-header style, in bits 16-17: clear of
-- the font fields in the low bits, which it would otherwise restyle.
tableSortMarkOf :: Int -> Int
tableSortMarkOf styleIdx = (styleIdx `shiftR` 16) .&. 0x03

-- | Blank reserve slot (spaces only). The sort mark is drawn as a triangle
-- over this slot, so the ▲/▼ codepoint never enters measured or laid-out text
-- (the pruned UI font does not carry it).
tableSortBlank :: Text
tableSortBlank = T.map (const ' ') tableSortReserve

tableHeaderDisplayText :: Text -> Text
tableHeaderDisplayText txt
  | Just hdr <- T.stripSuffix tableSortReserve txt = hdr <> tableSortBlank
  | Just hdr <- T.stripPrefix tableSortReserveLead txt = tableSortBlank <> hdr
  | otherwise = txt <> tableSortBlank

-- Type flags live in bits 28-31 so visual style and tab index stay in the low bits.
buttonFlagClose :: Int
buttonFlagClose = 0x20000000

-- | Visual style of a title-bar close button: its cross sits against the
-- box's right edge, so it lines up with the panel padding the way the title
-- does on the left.
buttonCloseTrailing :: Int
buttonCloseTrailing = 1

buttonFlagTab :: Int
buttonFlagTab = 0x40000000

buttonFlagTable :: Int
buttonFlagTable = 0x80000000

-- Flat menu row / menu-bar entry: transparent at rest, hover highlight, and an
-- accent marker on hover. Rendered by 'menuItemVisualStyle'.
buttonFlagMenu :: Int
buttonFlagMenu = 0x10000000

-- Flat menu-bar title: same flat/hover/open fill as a menu row, but centered
-- text and no hover accent marker (that marker belongs to drop-down rows).
buttonFlagMenuBar :: Int
buttonFlagMenuBar = 0x08000000

-- | A button whose content is a view of its own ('NanoUI.buttonContent'),
-- padded as a labelled button is.
buttonFlagContent :: Int
buttonFlagContent = 0x04000000

buttonFlagMask :: Int
buttonFlagMask = buttonFlagClose .|. buttonFlagTab .|. buttonFlagTable .|. buttonFlagMenu .|. buttonFlagMenuBar .|. buttonFlagContent

-- | Marks a @NodeContainer@ whose widgets are for display: the pointer passes
-- through them to the widget they are drawn in
-- ('NanoUI.Internal.Frame.Hit.innermostHit'). Plain containers carry no
-- other style.
containerFlagInert :: Int
containerFlagInert = 1

-- | Whether the packed style index @si@ carries @flag@.
{-# INLINE hasFlag #-}
hasFlag :: Int -> Int -> Bool
hasFlag flag si = si .&. flag /= 0

{-# INLINE buttonVisualStyle #-}
buttonVisualStyle :: Int -> Int
buttonVisualStyle si = si .&. complement buttonFlagMask

-- | A tab header's packed button style: the strip's tab style (0-3) and
-- 'buttonFlagTab'.
{-# INLINE tabEncodeStyle #-}
tabEncodeStyle :: Int -> Int
tabEncodeStyle style = style .|. buttonFlagTab
