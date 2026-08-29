module NanoUI.WidgetMarkers
  ( closeButtonMarker
  , tabButtonMarker
  , stripButtonBrackets
  , isCloseButtonText
  , isTabButtonText
  , closeButtonDisplayText
  , tabButtonDisplayText
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

tabButtonMarker :: T.Text
tabButtonMarker = T.singleton '\x02'

isCloseButtonText :: T.Text -> Bool
isCloseButtonText txt =
  closeButtonMarker `T.isPrefixOf` stripButtonBrackets txt

closeButtonDisplayText :: T.Text -> T.Text
closeButtonDisplayText txt = T.drop 1 (stripButtonBrackets txt)

isTabButtonText :: T.Text -> Bool
isTabButtonText txt =
  tabButtonMarker `T.isPrefixOf` stripButtonBrackets txt

tabButtonDisplayText :: T.Text -> T.Text
tabButtonDisplayText txt = T.drop 1 (stripButtonBrackets txt)

buttonDisplayText :: T.Text -> T.Text
buttonDisplayText txt =
  let lbl = stripButtonBrackets txt
   in if closeButtonMarker `T.isPrefixOf` lbl
        then T.drop 1 lbl
        else if tabButtonMarker `T.isPrefixOf` lbl
               then T.drop 1 lbl
               else lbl
