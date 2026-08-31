module NanoUI.WidgetText
  ( sliderDisplayText
  , sliderLabelText
  , sliderValueText
  , sliderText
  , checkboxLabelText
  , radioLabelText
  , treeEncodeStyle
  , treeDecodeStyle
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
  , colorPickerDisplayText
  , colorPickerToHex
  , colorPickerFromHex
  , closeButtonMarker
  , tabButtonMarker
  , tableHeaderMarker
  , tableStripeEven
  , tableStripeOdd
  , tableScrollSlaveStyle
  , tableSortReserve
  , tableHeaderLabel
  , tableHeaderDisplayText
  , stripButtonBrackets
  , isCloseButtonText
  , isTabButtonText
  , isTableHeaderText
  , closeButtonDisplayText
  , tabButtonDisplayText
  , buttonDisplayText
  , buttonFlags
  , buttonDisplayTextFromFlags
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (chr)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word8)
import NanoUI.Font (FontMetrics (..), fmLineHeight)
import NanoUI.Icons (Icons, checkboxPrefixes, radioPrefixes, treeExpandMark, treeExpandPrefixes)
import NanoUI.Types (Color (..), colorB, colorG, colorR, colorRGBA, sliderBarCells)
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

-- | styleIdx: nodeIdx in high bits, depth in 0-7, hasKids bit 8, expanded bit 9.
treeEncodeStyle :: Int -> Int -> Bool -> Bool -> Int
treeEncodeStyle nodeIdx depth hasKids expanded =
  (nodeIdx `shiftL` 10)
    .|. (depth .&. 0xff)
    .|. (if hasKids then 0x100 else 0)
    .|. (if expanded then 0x200 else 0)

treeDecodeStyle :: Int -> (Int, Int, Bool, Bool)
treeDecodeStyle s =
  ( s `shiftR` 10
  , s .&. 0xff
  , s .&. 0x100 /= 0
  , s .&. 0x200 /= 0
  )

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
textInputFieldPadY fm = max 3 (fmLineHeight fm * 0.22)

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

closeButtonMarker :: Text
closeButtonMarker = T.singleton '\x01'

tabButtonMarker :: Text
tabButtonMarker = T.singleton '\x02'

tableHeaderMarker :: Text
tableHeaderMarker = T.singleton '\x05'

tableStripeEven :: Int
tableStripeEven = 1

tableStripeOdd :: Int
tableStripeOdd = 2

-- | Scroll container that shares an id with a master pane and must not paint chrome.
tableScrollSlaveStyle :: Int
tableScrollSlaveStyle = 1

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
tableHeaderLabel terminal hdr = tableHeaderMarker <> hdr <> tableSortReserve terminal

tableHeaderDisplayText :: Bool -> Int -> Text -> Text
tableHeaderDisplayText terminal styleIdx txt =
  let full = T.drop 1 (stripButtonBrackets txt)
      title = fromMaybe full (T.stripSuffix (tableSortReserve terminal) full)
   in case styleIdx of
        1 -> title <> tableSortMark terminal False
        2 -> title <> tableSortMark terminal True
        _ -> title

{-# INLINE stripButtonBrackets #-}
stripButtonBrackets :: Text -> Text
stripButtonBrackets txt =
  let t = T.strip txt
   in if T.isPrefixOf "[ " t && T.isSuffixOf " ]" t
        then T.strip $ T.dropEnd 2 $ T.drop 2 t
        else txt

{-# INLINE buttonFlags #-}
buttonFlags :: Text -> (Bool, Bool, Bool)
buttonFlags txt =
  let lbl = stripButtonBrackets txt
   in ( closeButtonMarker `T.isPrefixOf` lbl
      , tabButtonMarker `T.isPrefixOf` lbl
      , tableHeaderMarker `T.isPrefixOf` lbl
      )

{-# INLINE isCloseButtonText #-}
isCloseButtonText :: Text -> Bool
isCloseButtonText txt =
  let (c, _, _) = buttonFlags txt
   in c

{-# INLINE isTabButtonText #-}
isTabButtonText :: Text -> Bool
isTabButtonText txt =
  let (_, t, _) = buttonFlags txt
   in t

{-# INLINE isTableHeaderText #-}
isTableHeaderText :: Text -> Bool
isTableHeaderText txt =
  let (_, _, h) = buttonFlags txt
   in h

{-# INLINE buttonDisplayTextFromFlags #-}
buttonDisplayTextFromFlags :: (Bool, Bool, Bool) -> Text -> Text
buttonDisplayTextFromFlags (isClose, isTab, isTable) txt
  | isClose || isTab || isTable = T.drop 1 (stripButtonBrackets txt)
  | otherwise = txt

{-# INLINE closeButtonDisplayText #-}
closeButtonDisplayText :: Text -> Text
closeButtonDisplayText txt = buttonDisplayTextFromFlags (True, False, False) txt

{-# INLINE tabButtonDisplayText #-}
tabButtonDisplayText :: Text -> Text
tabButtonDisplayText txt = buttonDisplayTextFromFlags (False, True, False) txt

{-# INLINE buttonDisplayText #-}
buttonDisplayText :: Text -> Text
buttonDisplayText txt = buttonDisplayTextFromFlags (buttonFlags txt) txt
