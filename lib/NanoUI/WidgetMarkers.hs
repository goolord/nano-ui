module NanoUI.WidgetMarkers
  ( closeButtonMarker
  , stripButtonBrackets
  , isCloseButtonText
  , closeButtonDisplayText
  , buttonDisplayText
  ) where

import qualified Data.Text as T

stripButtonBrackets :: T.Text -> T.Text
stripButtonBrackets txt =
  let t = T.strip txt
   in if T.isPrefixOf "[ " t && T.isSuffixOf " ]" t
        then T.strip $ T.dropEnd 2 $ T.drop 2 t
        else txt

closeButtonMarker :: T.Text
closeButtonMarker = T.singleton '\x01'

isCloseButtonText :: T.Text -> Bool
isCloseButtonText txt =
  closeButtonMarker `T.isPrefixOf` stripButtonBrackets txt

closeButtonDisplayText :: T.Text -> T.Text
closeButtonDisplayText txt = T.drop 1 (stripButtonBrackets txt)

buttonDisplayText :: T.Text -> T.Text
buttonDisplayText txt =
  let lbl = stripButtonBrackets txt
   in if isCloseButtonText txt then closeButtonDisplayText txt else lbl
