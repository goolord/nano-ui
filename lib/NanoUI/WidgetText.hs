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
  , selectPackOptions
  , selectParseOptions
  , selectLabelText
  , selectDisplayText
  , selectChevronReserve
  , selectChevronCenterX
  ) where

import Data.Text (Text)
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

sliderBarCells :: Int
sliderBarCells = 12

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
checkboxLabelText txt =
  if T.isPrefixOf "[x] " txt
    then T.drop 4 txt
    else
      if T.isPrefixOf "[ ] " txt
        then T.drop 4 txt
        else txt

textInputDisplayText :: Text -> String -> Bool -> Text
textInputDisplayText lbl value _focused =
  lbl <> ": " <> T.pack value

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
