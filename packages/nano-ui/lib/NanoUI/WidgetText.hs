module NanoUI.WidgetText
  ( sliderValueText
  , treeEncodeStyle
  , treeDecodeStyle
  , treeDecodeStripe
  , treeDisplayText
  , treeMeasureLabel
  , textInputTerminalText
  , textInputFieldText
  , textInputPlaceholder
  , textInputMinWidth
  , textInputLabelGap
  , textInputFieldPadY
  , textInputFieldHeight
  , textInputFlagSearch
  , textInputSearchMode
  , textInputSearchBody
  , textInputSearchTerminalText
  , searchFieldReserveW
  , searchFieldTextClip
  , searchFieldIconRects
  , selectDisplayText
  , selectChevronReserve
  , selectChevronCenterX
  , colorPickerLabelText
  , colorPickerCurrentLabel
  , colorPickerNewLabel
  , colorPickerDisplayText
  , colorPickerToHex
  , colorPickerFromHex
  , buttonFlagClose
  , buttonFlagTab
  , buttonFlagTable
  , buttonFlagMask
  , tableStripeEven
  , tableStripeOdd
  , tableScrollSlaveStyle
  , scrollNative2DStyle
  , tableSortReserve
  , tableStripeColor
  , stripeColor
  , packTextNodeStyle
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
  , buttonVisualStyle
  , buttonFlagsFromStyle
  ) where

import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Data.Char (chr)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word8)
import NanoUI.Font (FontMetrics (..), fmLineHeight, widgetContentInset)
import NanoUI.Icons (Icons, treeExpandMark)
import NanoUI.Style (FontStyle (..), FontVariant (..), FontWeight (..), TextDecoration (..), Theme (..), styleBg, themeButton, themePanel, themeWindow)
import NanoUI.Types (Color (..), HostProfile, Rect (..), colorB, colorG, colorR, colorRGBA, isCellHost, lerpColor)
import qualified Data.Text as T

sliderValueText :: Float -> Text
sliderValueText = T.pack . show . (round :: Float -> Int)

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

-- | Visible terminal row: indent, expand mark, label.
treeDisplayText :: Icons -> Int -> Bool -> Bool -> Text -> Text
treeDisplayText icons depth hasKids expanded label =
  T.replicate (max 0 depth) "  "
    <> treeExpandMark icons hasKids expanded
    <> label

-- | Cell-host measure stand-in. Mark width matches ASCII "v " / "> ".
treeMeasureLabel :: Int -> Text -> Text
treeMeasureLabel depth label =
  T.replicate (max 0 depth) "  "
    <> "  "
    <> if T.null label then " " else label

textInputMinWidth :: Float
textInputMinWidth = 160

textInputLabelGap :: FontMetrics -> Float
textInputLabelGap fm =
  if fmLineHeight fm <= 14 then 3 else 4

textInputFieldPadY :: FontMetrics -> Float
textInputFieldPadY fm = max 3 (fmAdvance fm ' ' * 1.25)

textInputFieldHeight :: FontMetrics -> Float
textInputFieldHeight fm = fmLineHeight fm + 2 * textInputFieldPadY fm

-- | Search-field icon geometry on GUI hosts (zero on cell hosts). Returns
-- (icon diameter, outer pad, left chrome lead, right chrome tail). The lead/tail
-- are the horizontal space the magnifier / clear buttons reserve either side of
-- the editable text.
searchFieldChrome :: HostProfile -> FontMetrics -> (Float, Float, Float, Float)
searchFieldChrome host fm
  | isCellHost host = (0, 0, 0, 0)
  | otherwise =
      let (ix, _) = widgetContentInset host fm
          s = max 12 (min 15 (fmLineHeight fm * 0.8))
          pad = fmAdvance fm ' ' * 0.6
       in (s, ix, ix + s + pad, pad + s + ix)

-- | Total horizontal chrome a caption-less search box reserves for its icons.
searchFieldReserveW :: HostProfile -> FontMetrics -> Float
searchFieldReserveW host fm =
  let (_, _, lead, tailw) = searchFieldChrome host fm
   in lead + tailw

-- | Region a caption-less search field's editable text may occupy. Excludes the
-- magnifier on the left and the clear slot on the right.
searchFieldTextClip :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> Rect
searchFieldTextClip host fm x y w h =
  let (_, _, lead, tailw) = searchFieldChrome host fm
      (_, iy) = widgetContentInset host fm
   in Rect (x + lead) (y + iy) (max 0 (w - lead - tailw)) (max 0 (h - 2 * iy))

-- | Square slots (magnifier left, clear right) the search icons are drawn in.
searchFieldIconRects :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> (Rect, Rect)
searchFieldIconRects host fm x y w h =
  let (s, ix, _, _) = searchFieldChrome host fm
      cy = y + h / 2
      mag = Rect (x + ix) (cy - s / 2) s s
      clear = Rect (x + w - ix - s) (cy - s / 2) s s
   in (mag, clear)

textInputPlaceholder :: Text -> Text
textInputPlaceholder lbl =
  if T.null lbl
    then "Enter text"
    else "Enter " <> T.toLower lbl

textInputFieldText :: Text -> Text -> Bool -> Text
textInputFieldText lbl value focused =
  let body = value
   in if T.null body && not focused
        then textInputPlaceholder lbl
        else body

textInputTerminalText :: Text -> Text -> Int -> Bool -> Text
textInputTerminalText lbl value cursor focused =
  let body = value
      shown =
        if focused
          then
            let c = max 0 (min (T.length body) cursor)
             in T.take c body <> "\x2502" <> T.drop c body
          else body
   in lbl <> ": " <> shown

-- | Marks a @NodeTextInput@ as a caption-less search field. Lives in the high
-- style bits (like the button flags) so it survives the arena's int storage.
textInputFlagSearch :: Int
textInputFlagSearch = 0x04000000

{-# INLINE textInputSearchMode #-}
textInputSearchMode :: Int -> Bool
textInputSearchMode si = si .&. textInputFlagSearch /= 0

-- | Body of a search field: the live value, or the placeholder while empty and
-- unfocused. @ph@ is the caller-supplied placeholder, not the derived one used
-- by captioned 'textInputFieldText'.
textInputSearchBody :: Text -> Text -> Bool -> Text
textInputSearchBody ph value focused =
  if T.null value && not focused
    then ph
    else value

-- | Terminal representation of a search field: value (or placeholder), with the
-- caret inserted when focused. No caption prefix.
textInputSearchTerminalText :: Text -> Text -> Int -> Bool -> Text
textInputSearchTerminalText ph value cursor focused
  | focused && T.null value = "\x2502" <> ph
  | focused =
      let v = value
          c = max 0 (min (T.length v) cursor)
       in T.take c v <> "\x2502" <> T.drop c v
  | T.null value = ph
  | otherwise = value

selectDisplayText :: Text -> Text -> Text
selectDisplayText lbl opt = lbl <> ": " <> opt

-- Space reserved on the right of a select for the chevron.
selectChevronReserve :: Float
selectChevronReserve = 16

selectChevronCenterX :: Float -> Float -> Float
selectChevronCenterX x w = x + w - selectChevronReserve / 2

colorPickerLabelText :: Text -> Text
colorPickerLabelText = T.strip

colorPickerCurrentLabel :: Text
colorPickerCurrentLabel = "Current Color"

colorPickerNewLabel :: Text
colorPickerNewLabel = "New Color"

colorPickerToHex :: Color -> Text
colorPickerToHex c =
  "#" <> hexByte (colorR c) <> hexByte (colorG c) <> hexByte (colorB c)

hexByte :: Word8 -> Text
hexByte n = T.pack (showHexWord8 n)

showHexWord8 :: Word8 -> String
showHexWord8 n =
  let hi = n `div` 16
      lo = n `mod` 16
      ch i = if i < 10 then chr (48 + fromIntegral i) else chr (87 + fromIntegral i)
   in [ch hi, ch lo]

colorPickerFromHex :: Text -> Maybe Color
colorPickerFromHex txt =
  let bare = T.dropWhile (== '#') (T.strip txt)
   in if T.length bare /= 6
        then Nothing
        else do
          r <- parseHexPair (T.take 2 bare)
          g <- parseHexPair (T.take 2 (T.drop 2 bare))
          b <- parseHexPair (T.take 2 (T.drop 4 bare))
          pure (colorRGBA r g b 255)

parseHexPair :: Text -> Maybe Word8
parseHexPair t =
  case (parseHexDigit (T.index t 0), parseHexDigit (T.index t 1)) of
    (Just a, Just b) -> Just (a * 16 + b)
    _ -> Nothing

parseHexDigit :: Char -> Maybe Word8
parseHexDigit c
  | c >= '0' && c <= '9' = Just (fromIntegral (fromEnum c - 48))
  | c >= 'a' && c <= 'f' = Just (fromIntegral (fromEnum c - 87))
  | c >= 'A' && c <= 'F' = Just (fromIntegral (fromEnum c - 55))
  | otherwise = Nothing

colorPickerDisplayText :: Text -> Color -> Text
colorPickerDisplayText lbl col = colorPickerLabelText lbl <> ": " <> colorPickerToHex col

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

{-# INLINE packTextNodeStyle #-}
packTextNodeStyle :: FontVariant -> Int -> Int
packTextNodeStyle fvar stripe = packTextNodeStyleFull fvar WeightNormal FontStyleNormal DecorationNone stripe

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

-- | Scroll container that shares an id with a master pane and must not paint chrome.
tableScrollSlaveStyle :: Int
tableScrollSlaveStyle = 1

-- | Native 2D scroll container (both axes active).
scrollNative2DStyle :: Int
scrollNative2DStyle = 2

-- | Trailing slot reserved in every header so the sort mark never changes column width.
tableSortReserve :: Bool -> Text
tableSortReserve True = " ^"
tableSortReserve False = "  ▲"

tableSortMark :: Bool -> Bool -> Text
tableSortMark True True = " v"
tableSortMark True False = " ^"
tableSortMark False True = "  ▼"
tableSortMark False False = "  ▲"

tableHeaderLabel :: Bool -> Text -> Text
tableHeaderLabel terminal hdr = hdr <> tableSortReserve terminal

-- | Sort direction encoded for a table-header style. Lives in bits 16-17: the
-- low nibbles are the font fields, and a mark value of 1 or 2 in bit 0-1 used
-- to flip the header's font variant (heading / muted), which blanked the
-- arrow glyph.
tableSortMarkOf :: Int -> Int
tableSortMarkOf styleIdx = (styleIdx `shiftR` 16) .&. 0x03

-- | Blank reserve slot (spaces only). Non-terminal hosts draw the sort mark
-- as a triangle over this slot, so the ▲/▼ codepoint never enters measured
-- or laid-out text (the pruned UI font does not carry it).
tableSortBlank :: Bool -> Text
tableSortBlank = T.map (const ' ') . tableSortReserve

tableHeaderDisplayText :: Bool -> Int -> Text -> Text
tableHeaderDisplayText terminal styleIdx txt =
  let full = txt
      reserve = tableSortReserve terminal
      title = fromMaybe full (T.stripSuffix reserve full)
   in case tableSortMarkOf styleIdx of
        1 | terminal -> title <> tableSortMark terminal False
        2 | terminal -> title <> tableSortMark terminal True
        _ -> title <> tableSortBlank terminal

-- Type flags live in bits 29-31 so visual style and tab index stay in the low bits.
buttonFlagClose :: Int
buttonFlagClose = 0x20000000

buttonFlagTab :: Int
buttonFlagTab = 0x40000000

buttonFlagTable :: Int
buttonFlagTable = 0x80000000

buttonFlagMask :: Int
buttonFlagMask = buttonFlagClose .|. buttonFlagTab .|. buttonFlagTable

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
