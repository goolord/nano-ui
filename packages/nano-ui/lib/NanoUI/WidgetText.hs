module NanoUI.WidgetText
  ( intValueText
  , treeEncodeStyle
  , treeDecodeStyle
  , treeDecodeStripe
  , textInputFieldText
  , textInputMinWidth
  , textInputFieldPadY
  , textInputFieldHeight
  , textInputFlagSearch
  , textInputSearchMode
  , textInputFlagSelectable
  , textInputSelectableMode
  , textInputFlagPassword
  , textInputPasswordMode
  , textInputFlagNumeric
  , textInputNumericMode
  , numericStepperW
  , numericTextClip
  , numericStepperRects
  , comboTextClip
  , searchFieldReserveW
  , searchFieldTextClip
  , searchFieldIconRects
  , selectDisplayText
  , selectChevronReserve
  , selectChevronCenterX
  , colorPickerGap
  , colorPickerSvH
  , colorPickerCurrentLabel
  , colorPickerNewLabel
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
  , buttonFlagMask
  , tableStripeEven
  , tableStripeOdd
  , tableSortReserve
  , tableStripeColor
  , stripeColor
  , packTextNodeStyleFull
  , textNodeFontVariant
  , textNodeFontWeight
  , textNodeFontStyle
  , textNodeTextDecoration
  , textNodeStripe
  , tableHeaderLabel
  , tableHeaderDisplayText
  , tableSortMarkOf
  , tableSortBlank
  , isCloseButtonStyle
  , isTabButtonStyle
  , isTableHeaderStyle
  , isMenuItemStyle
  , isMenuBarStyle
  , buttonVisualStyle
  , buttonFlagsFromStyle
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
import Numeric (showHex)
import NanoUI.Font (FontMetrics (..), fmLineHeight, widgetContentInset)
import NanoUI.Style (FontStyle (..), FontVariant (..), FontWeight (..), TextDecoration (..), Theme (..), styleBg, themeButton, themePanel, themeWindow)
import NanoUI.Types (Color (..), Rect (..), colorA, colorB, colorG, colorR, colorRGBA, lerpColor)
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

treeDecodeStripe :: Int -> Int
treeDecodeStripe s = if s .&. 0x400 /= 0 then tableStripeOdd else tableStripeEven

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
searchFieldChrome :: FontMetrics -> (Float, Float, Float, Float)
searchFieldChrome fm =
  let (ix, _) = widgetContentInset fm
      s = max 12 (min 15 (fmLineHeight fm * 0.8))
      pad = fmAdvance fm ' ' * 0.6
   in (s, ix, ix + s + pad, pad + s + ix)

-- | Total horizontal chrome a caption-less search box reserves for its icons.
searchFieldReserveW :: FontMetrics -> Float
searchFieldReserveW fm =
  let (_, _, lead, tailw) = searchFieldChrome fm
   in lead + tailw

-- | Region a caption-less search field's editable text may occupy. Excludes the
-- magnifier on the left and the clear slot on the right.
searchFieldTextClip :: FontMetrics -> Float -> Float -> Float -> Float -> Rect
searchFieldTextClip fm x y w h =
  let (_, _, lead, tailw) = searchFieldChrome fm
      (_, iy) = widgetContentInset fm
   in Rect (x + lead) (y + iy) (max 0 (w - lead - tailw)) (max 0 (h - 2 * iy))

-- | Square slots (magnifier left, clear right) the search icons are drawn in.
searchFieldIconRects :: FontMetrics -> Float -> Float -> Float -> Float -> (Rect, Rect)
searchFieldIconRects fm x y w h =
  let (s, ix, _, _) = searchFieldChrome fm
      cy = y + h / 2
      mag = Rect (x + ix) (cy - s / 2) s s
      clear = Rect (x + w - ix - s) (cy - s / 2) s s
   in (mag, clear)

textInputFieldText :: Text -> Text -> Bool -> Text
textInputFieldText ph value focused =
  let body = value
   in if T.null body && not focused
        then ph
        else body

-- | Marks a @NodeTextInput@ as a caption-less search field. Lives in the high
-- style bits (like the button flags) so it survives the arena's int storage.
textInputFlagSearch :: Int
textInputFlagSearch = 0x04000000

{-# INLINE textInputSearchMode #-}
textInputSearchMode :: Int -> Bool
textInputSearchMode si = si .&. textInputFlagSearch /= 0

-- | Marks a @NodeTextInput@ as a selectable text label: read-only, caption-less,
-- chrome-less, sized to its text content, with mouse drag-to-select and copy.
textInputFlagSelectable :: Int
textInputFlagSelectable = 0x10000000

{-# INLINE textInputSelectableMode #-}
textInputSelectableMode :: Int -> Bool
textInputSelectableMode si = si .&. textInputFlagSelectable /= 0

-- | Marks a @NodeTextInput@ as a password field: its value is displayed masked
-- and is never copied or cut to the clipboard.
textInputFlagPassword :: Int
textInputFlagPassword = 0x20000000

{-# INLINE textInputPasswordMode #-}
textInputPasswordMode :: Int -> Bool
textInputPasswordMode si = si .&. textInputFlagPassword /= 0

-- | Marks a @NodeTextInput@ as a numeric field: a caption-less box whose text
-- stops short of an up / down stepper at its right edge.
textInputFlagNumeric :: Int
textInputFlagNumeric = 0x40000000

{-# INLINE textInputNumericMode #-}
textInputNumericMode :: Int -> Bool
textInputNumericMode si = si .&. textInputFlagNumeric /= 0

-- | Width of a numeric field's stepper column.
numericStepperW :: Float
numericStepperW = 18

-- | Region a numeric field's text may occupy: inside the content inset, left
-- of the stepper.
numericTextClip :: FontMetrics -> Float -> Float -> Float -> Float -> Rect
numericTextClip fm x y w h =
  let (ix, iy) = widgetContentInset fm
   in Rect (x + ix) (y + iy) (max 0 (w - 2 * ix - numericStepperW)) (max 0 (h - 2 * iy))

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
  let (ix, iy) = widgetContentInset fm
   in Rect (x + ix) (y + iy) (max 0 (w - ix - selectChevronReserve)) (max 0 (h - 2 * iy))

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

colorPickerCurrentLabel :: Text
colorPickerCurrentLabel = "Current"

colorPickerNewLabel :: Text
colorPickerNewLabel = "New"

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
colorPickerParseHex txt =
  let bare = T.dropWhile (== '#') (T.strip txt)
      pair i = parseHexPair (T.take 2 (T.drop i bare))
   in case T.length bare of
        6 -> do
          r <- pair 0
          g <- pair 2
          b <- pair 4
          pure (r, g, b, Nothing)
        8 -> do
          r <- pair 0
          g <- pair 2
          b <- pair 4
          a <- pair 6
          pure (r, g, b, Just a)
        _ -> Nothing

colorFromHex :: Text -> Maybe Color
colorFromHex txt = do
  (r, g, b, ma) <- colorPickerParseHex txt
  pure (colorRGBA r g b (fromMaybe 255 ma))

parseHexPair :: Text -> Maybe Word8
parseHexPair t = case T.unpack t of
  [hi, lo]
    | isHexDigit hi && isHexDigit lo -> Just (fromIntegral (digitToInt hi * 16 + digitToInt lo))
  _ -> Nothing

tableStripeEven :: Int
tableStripeEven = 1

tableStripeOdd :: Int
tableStripeOdd = 2

{-# INLINE packTextNodeStyleFull #-}
packTextNodeStyleFull :: FontVariant -> FontWeight -> FontStyle -> TextDecoration -> Int -> Int
packTextNodeStyleFull fvar weight fstyle deco stripe =
  (stripe `shiftL` 4)
    .|. (fromEnum fvar .&. 0x0F)
    .|. ((fromEnum weight .&. 0x0F) `shiftL` 8)
    .|. ((fromEnum fstyle .&. 0x03) `shiftL` 12)
    .|. ((fromEnum deco .&. 0x03) `shiftL` 14)

{-# INLINE textNodeFontVariant #-}
textNodeFontVariant :: Int -> FontVariant
textNodeFontVariant si =
  let v = si .&. 0x0F
   in if v >= fromEnum (minBound :: FontVariant) && v <= fromEnum (maxBound :: FontVariant)
        then toEnum v
        else FontRegular

{-# INLINE textNodeFontWeight #-}
textNodeFontWeight :: Int -> FontWeight
textNodeFontWeight si =
  let w = (si `shiftR` 8) .&. 0x0F
   in if w >= fromEnum (minBound :: FontWeight) && w <= fromEnum (maxBound :: FontWeight)
        then toEnum w
        else WeightNormal

{-# INLINE textNodeFontStyle #-}
textNodeFontStyle :: Int -> FontStyle
textNodeFontStyle si =
  let s = (si `shiftR` 12) .&. 0x03
   in if s >= fromEnum (minBound :: FontStyle) && s <= fromEnum (maxBound :: FontStyle)
        then toEnum s
        else FontStyleNormal

{-# INLINE textNodeTextDecoration #-}
textNodeTextDecoration :: Int -> TextDecoration
textNodeTextDecoration si =
  let d = (si `shiftR` 14) .&. 0x03
   in if d >= fromEnum (minBound :: TextDecoration) && d <= fromEnum (maxBound :: TextDecoration)
        then toEnum d
        else DecorationNone

{-# INLINE textNodeStripe #-}
textNodeStripe :: Int -> Int
textNodeStripe si = (si `shiftR` 4) .&. 0x0F

{-# INLINE stripeColor #-}
stripeColor :: Theme -> Int -> Maybe Color
stripeColor theme s
  | s == tableStripeEven = Just (lerpColor (styleBg (themePanel theme)) (themeWindow theme) 0.26)
  | s == tableStripeOdd = Just (lerpColor (styleBg (themePanel theme)) (styleBg (themeButton theme)) 0.55)
  | otherwise = Nothing

tableStripeColor :: Theme -> Int -> Maybe Color
tableStripeColor theme si = stripeColor theme (textNodeStripe si)

-- | Trailing slot reserved in every header so the sort mark never changes column width.
tableSortReserve :: Text
tableSortReserve = "  ▲"

tableHeaderLabel :: Text -> Text
tableHeaderLabel hdr = hdr <> tableSortReserve

-- | Sort direction encoded for a table-header style. Lives in bits 16-17: the
-- low nibbles are the font fields, and a mark value of 1 or 2 in bit 0-1 used
-- to flip the header's font variant (heading / muted), which blanked the
-- arrow glyph.
tableSortMarkOf :: Int -> Int
tableSortMarkOf styleIdx = (styleIdx `shiftR` 16) .&. 0x03

-- | Blank reserve slot (spaces only). The sort mark is drawn as a triangle
-- over this slot, so the ▲/▼ codepoint never enters measured or laid-out text
-- (the pruned UI font does not carry it).
tableSortBlank :: Text
tableSortBlank = T.map (const ' ') tableSortReserve

tableHeaderDisplayText :: Text -> Text
tableHeaderDisplayText txt =
  fromMaybe txt (T.stripSuffix tableSortReserve txt) <> tableSortBlank

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

buttonFlagMask :: Int
buttonFlagMask = buttonFlagClose .|. buttonFlagTab .|. buttonFlagTable .|. buttonFlagMenu .|. buttonFlagMenuBar

{-# INLINE buttonVisualStyle #-}
buttonVisualStyle :: Int -> Int
buttonVisualStyle si = si .&. complement buttonFlagMask

{-# INLINE buttonFlagsFromStyle #-}
buttonFlagsFromStyle :: Int -> (Bool, Bool, Bool)
buttonFlagsFromStyle si =
  ( si .&. buttonFlagClose /= 0
  , si .&. buttonFlagTab /= 0
  , si .&. buttonFlagTable /= 0
  )

{-# INLINE isCloseButtonStyle #-}
isCloseButtonStyle :: Int -> Bool
isCloseButtonStyle si = si .&. buttonFlagClose /= 0

{-# INLINE isTabButtonStyle #-}
isTabButtonStyle :: Int -> Bool
isTabButtonStyle si = si .&. buttonFlagTab /= 0

{-# INLINE isTableHeaderStyle #-}
isTableHeaderStyle :: Int -> Bool
isTableHeaderStyle si = si .&. buttonFlagTable /= 0

{-# INLINE isMenuItemStyle #-}
isMenuItemStyle :: Int -> Bool
isMenuItemStyle si = si .&. buttonFlagMenu /= 0

{-# INLINE isMenuBarStyle #-}
isMenuBarStyle :: Int -> Bool
isMenuBarStyle si = si .&. buttonFlagMenuBar /= 0
