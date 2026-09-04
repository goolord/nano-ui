module NanoUI.WidgetText
  ( sliderDisplayText
  , sliderLabelText
  , sliderValueText
  , sliderText
  , checkboxLabelText
  , radioLabelText
  , treeEncodeStyle
  , treeDecodeStyle
  , treeDecodeStripe
  , treeLabelText
  , treeDisplayText
  , treeMeasureLabel
  , textInputDisplayText
  , textInputTerminalText
  , textInputFieldText
  , textInputPlaceholder
  , textInputMinWidth
  , textInputLabelGap
  , textInputFieldPadY
  , textInputFieldHeight
  , selectOptions
  , selectLabelText
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
  , textNodeFontVariant
  , textNodeStripe
  , tableHeaderLabel
  , tableHeaderDisplayText
  , isCloseButtonText
  , isTabButtonText
  , isTableHeaderText
  , isCloseButtonStyle
  , isTabButtonStyle
  , isTableHeaderStyle
  , packButtonStyle
  , buttonVisualStyle
  , buttonFlagsFromStyle
  , closeButtonDisplayText
  , tabButtonDisplayText
  , buttonDisplayText
  , buttonFlags
  , buttonDisplayTextFromFlags
  ) where

import Data.Bits (complement, (.&.), (.|.), shiftL, shiftR)
import Data.Char (chr)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word8)
import NanoUI.Font (FontMetrics (..), fmLineHeight)
import NanoUI.Icons (Icons, checkboxPrefixes, radioPrefixes, treeExpandMark, treeExpandPrefixes)
import NanoUI.Style (FontVariant (..), Theme (..), styleBg, themeButton, themePanel, themeWindow)
import NanoUI.Types (Color (..), colorB, colorG, colorR, colorRGBA, lerpColor, sliderBarCells)
import qualified Data.Text as T

sliderDisplayText :: Text -> Float -> Text
sliderDisplayText lbl value = lbl <> ": " <> T.pack (show (round value :: Int))

sliderLabelText :: Text -> Text
sliderLabelText txt =
  let (lbl, rest) = T.breakOn ": " txt
   in if T.null rest then T.stripEnd (T.takeWhile (/= '[') txt) else lbl

sliderValueText :: Float -> Text
sliderValueText = T.pack . show . (round :: Float -> Int)

sliderText :: Text -> Float -> Float -> Text
sliderText lbl frac value =
  let filled = max 0 (min sliderBarCells (round (frac * fromIntegral sliderBarCells)))
      bar = T.replicate filled "\x2588" <> T.replicate (sliderBarCells - filled) "\x2591"
   in lbl <> " [" <> bar <> "] " <> T.pack (show (round value :: Int))

checkboxLabelText :: Text -> Text
checkboxLabelText txt =
  case find (`T.isPrefixOf` txt) checkboxPrefixes of
    Nothing -> txt
    Just p -> T.drop (T.length p) txt

radioLabelText :: Text -> Text
radioLabelText txt =
  case find (`T.isPrefixOf` txt) radioPrefixes of
    Nothing -> txt
    Just p -> T.drop (T.length p) txt

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

treeLabelText :: Text -> Text
treeLabelText txt =
  case find (`T.isPrefixOf` txt) treeExpandPrefixes of
    Nothing -> txt
    Just p -> T.drop (T.length p) txt

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

textInputDisplayText :: Text -> Text -> Bool -> Text
textInputDisplayText lbl value focused =
  textInputFieldText lbl value focused

selectOptions :: Text -> (Text, [Text])
selectOptions txt =
  case T.splitOn "\n" txt of
    [] -> ("", [])
    (lbl : rest) -> (lbl, rest)

selectLabelText :: Text -> Text
selectLabelText txt = fst (selectOptions txt)

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

{-# INLINE packTextNodeStyle #-}
packTextNodeStyle :: FontVariant -> Int -> Int
packTextNodeStyle fvar stripe =
  (stripe `shiftL` 4) .|. (fromEnum fvar .&. 0x0F)

{-# INLINE textNodeFontVariant #-}
textNodeFontVariant :: Int -> FontVariant
textNodeFontVariant si =
  let v = si .&. 0x0F
   in if v >= fromEnum (minBound :: FontVariant) && v <= fromEnum (maxBound :: FontVariant)
        then toEnum v
        else FontRegular

{-# INLINE textNodeStripe #-}
textNodeStripe :: Int -> Int
textNodeStripe si = (si `shiftR` 4) .&. 0x0F

{-# INLINE stripeColor #-}
stripeColor :: Theme -> Int -> Maybe Color
stripeColor theme s
  | s == tableStripeEven = Just (lerpColor (styleBg (themePanel theme)) (themeWindow theme) 0.18)
  | s == tableStripeOdd = Just (lerpColor (styleBg (themePanel theme)) (styleBg (themeButton theme)) 0.42)
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

tableHeaderDisplayText :: Bool -> Int -> Text -> Text
tableHeaderDisplayText terminal styleIdx txt =
  let full = txt
      reserve = tableSortReserve terminal
      title = fromMaybe full (T.stripSuffix reserve full)
      blank = T.map (const ' ') reserve
   in case buttonVisualStyle styleIdx of
        1 -> title <> tableSortMark terminal False
        2 -> title <> tableSortMark terminal True
        _ -> title <> blank

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

{-# INLINE packButtonStyle #-}
packButtonStyle :: Int -> Text -> Int
packButtonStyle visual _ = visual

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

{-# INLINE buttonFlags #-}
buttonFlags :: Text -> (Bool, Bool, Bool)
buttonFlags _ = (False, False, False)

{-# INLINE isCloseButtonText #-}
isCloseButtonText :: Text -> Bool
isCloseButtonText _ = False

{-# INLINE isTabButtonText #-}
isTabButtonText :: Text -> Bool
isTabButtonText _ = False

{-# INLINE isTableHeaderText #-}
isTableHeaderText :: Text -> Bool
isTableHeaderText _ = False

{-# INLINE buttonDisplayTextFromFlags #-}
buttonDisplayTextFromFlags :: (Bool, Bool, Bool) -> Text -> Text
buttonDisplayTextFromFlags _ txt = txt

{-# INLINE closeButtonDisplayText #-}
closeButtonDisplayText :: Text -> Text
closeButtonDisplayText = id

{-# INLINE tabButtonDisplayText #-}
tabButtonDisplayText :: Text -> Text
tabButtonDisplayText = id

{-# INLINE buttonDisplayText #-}
buttonDisplayText :: Text -> Text
buttonDisplayText = id
