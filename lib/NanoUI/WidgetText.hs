module NanoUI.WidgetText
  ( sliderRangeSep
  , sliderDisplayText
  , sliderLabelText
  , sliderValueText
  , sliderPackRange
  , sliderPackTerminal
  , sliderParseRange
  , sliderText
  , checkboxLabelText
  , textInputDisplayText
  , textInputTerminalText
  , textInputFieldText
  , textInputPlaceholder
  , textInputMinWidth
  , textInputLabelGap
  , textInputFieldPadY
  , textInputFieldHeight
  , selectPackOptions
  , selectParseOptions
  , selectLabelText
  , selectDisplayText
  , selectChevronReserve
  , selectChevronCenterX
  ) where

import Data.Text (Text)
import NanoUI.Font (FontMetrics (..), fmLineHeight)
import NanoUI.Icons (checkboxPrefixes)
import NanoUI.Types (sliderBarCells)
import qualified Data.Text as T

sliderRangeSep :: Text
sliderRangeSep = T.singleton '\US'

sliderDisplayText :: Text -> Float -> Text
sliderDisplayText lbl value = lbl <> ": " <> T.pack (show (round value :: Int))

sliderLabelText :: Text -> Text
sliderLabelText txt =
  let bare = T.takeWhile (/= '\US') txt
      (lbl, rest) = T.breakOn ": " bare
   in if T.null rest then T.stripEnd (T.takeWhile (/= '[') bare) else lbl

sliderValueText :: Float -> Text
sliderValueText = T.pack . show . (round :: Float -> Int)

sliderText :: Text -> Float -> Float -> Text
sliderText lbl frac value =
  let filled = max 0 (min sliderBarCells (round (frac * fromIntegral sliderBarCells)))
      bar = T.replicate filled "\x2588" <> T.replicate (sliderBarCells - filled) "\x2591"
   in lbl <> " [" <> bar <> "] " <> T.pack (show (round value :: Int))

sliderPackRange :: Text -> Float -> Float -> Text
sliderPackRange lbl minV maxV =
  lbl <> sliderRangeSep <> sliderValueText minV <> "," <> sliderValueText maxV

-- | Terminal slider node text: visible bar plus hidden min/max suffix after US.
sliderPackTerminal :: Text -> Float -> Float -> Float -> Float -> Text
sliderPackTerminal lbl frac val minV maxV =
  sliderText lbl frac val
    <> sliderRangeSep
    <> sliderValueText minV
    <> ","
    <> sliderValueText maxV

sliderParseRange :: Text -> (Text, Float, Float)
sliderParseRange txt =
  let bare = T.takeWhile (/= '\US') txt
      (minV, maxV) =
        case T.breakOn sliderRangeSep txt of
          (_, suffix)
            | not (T.null suffix) ->
                case T.breakOn "," (T.drop 1 suffix) of
                  (a, b) ->
                    ( readFloat a 0
                    , readFloat (T.drop 1 b) 100
                    )
          _ -> (0, 100)
   in (sliderLabelText bare, minV, maxV)
  where
    readFloat t fallback =
      case reads (T.unpack t) of
        [(v, "")] -> v
        _ -> fallback

checkboxLabelText :: Text -> Text
checkboxLabelText txt = go checkboxPrefixes
  where
    go [] = txt
    go (p : ps) =
      if T.isPrefixOf p txt
        then T.drop (T.length p) txt
        else go ps

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

textInputFieldText :: Text -> String -> Bool -> Text
textInputFieldText lbl value focused =
  let body = T.pack value
   in if T.null body && not focused
        then textInputPlaceholder lbl
        else body

textInputTerminalText :: Text -> String -> Int -> Bool -> Text
textInputTerminalText lbl value cursor focused =
  let body = T.pack value
      shown =
        if focused
          then
            let c = max 0 (min (T.length body) cursor)
             in T.take c body <> "\x2502" <> T.drop c body
          else body
   in lbl <> ": " <> shown

textInputDisplayText :: Text -> String -> Bool -> Text
textInputDisplayText lbl value focused =
  textInputFieldText lbl value focused

selectPackOptions :: Text -> [Text] -> Text
selectPackOptions lbl opts =
  T.intercalate sliderRangeSep (lbl : opts)

selectParseOptions :: Text -> (Text, [Text])
selectParseOptions txt =
  case T.splitOn sliderRangeSep txt of
    [] -> ("", [])
    (lbl : rest) -> (lbl, rest)

selectLabelText :: Text -> Text
selectLabelText txt = fst (selectParseOptions txt)

selectDisplayText :: Text -> Text -> Text
selectDisplayText lbl opt = lbl <> ": " <> opt

-- Space reserved on the right of a select for the chevron.
selectChevronReserve :: Float
selectChevronReserve = 16

selectChevronCenterX :: Float -> Float -> Float
selectChevronCenterX x w = x + w - selectChevronReserve / 2
