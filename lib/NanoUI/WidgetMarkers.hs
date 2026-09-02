-- | Control-character sentinels for widget chrome and font routing.
-- tabButtonMarker and monoFontMarker share codepoint \x02 in disjoint contexts.
module NanoUI.WidgetMarkers
  ( closeButtonMarker
  , tabButtonMarker
  , monoFontMarker
  , headingFontMarker
  , mutedFontMarker
  , tableHeaderMarker
  , tabSentinelChar
  ) where

import Data.Text (Text)
import qualified Data.Text as T

closeButtonMarker :: Text
closeButtonMarker = T.singleton '\x01'

tabButtonMarker :: Text
tabButtonMarker = T.singleton '\x02'

monoFontMarker :: Text
monoFontMarker = tabButtonMarker

headingFontMarker :: Text
headingFontMarker = T.singleton '\x03'

mutedFontMarker :: Text
mutedFontMarker = T.singleton '\x04'

tableHeaderMarker :: Text
tableHeaderMarker = T.singleton '\x05'

tabSentinelChar :: Char
tabSentinelChar = '\x2409'
