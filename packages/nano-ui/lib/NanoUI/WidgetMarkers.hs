-- | Control-character sentinels for widget chrome.
module NanoUI.WidgetMarkers
  ( closeButtonMarker
  , tabButtonMarker
  , tableHeaderMarker
  , tabSentinelChar
  ) where

import Data.Text (Text)
import qualified Data.Text as T

closeButtonMarker :: Text
closeButtonMarker = T.singleton '\x01'

tabButtonMarker :: Text
tabButtonMarker = T.singleton '\x02'

tableHeaderMarker :: Text
tableHeaderMarker = T.singleton '\x05'

tabSentinelChar :: Char
tabSentinelChar = '\x2409'
