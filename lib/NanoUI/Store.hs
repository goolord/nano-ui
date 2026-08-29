module NanoUI.Store
  ( WidgetStore (..)
  , emptyWidgetStore
  ) where

import Data.IntMap.Strict (IntMap)
import Data.Text (Text)
import qualified Data.IntMap.Strict as IM

data WidgetStore = WidgetStore
  { storeCheckbox :: IntMap Bool
  , storeSlider :: IntMap Float
  , storeText :: IntMap Text
  , storeCursor :: IntMap Int
  , storeSelAnchor :: IntMap Int
  , storeScroll :: IntMap Float
  , storeSelect :: IntMap Int
  , storeSelectOpen :: IntMap Bool
  , storeDisabled :: IntMap Bool
  , storeFlag :: IntMap Bool
  , storeNote :: IntMap Text
  , storeWindow :: IntMap (Float, Float)
  , storeWindowSize :: IntMap (Float, Float)
  }
  deriving (Eq, Show)

emptyWidgetStore :: WidgetStore
emptyWidgetStore =
  WidgetStore
    { storeCheckbox = IM.empty
    , storeSlider = IM.empty
    , storeText = IM.empty
    , storeCursor = IM.empty
    , storeSelAnchor = IM.empty
    , storeScroll = IM.empty
    , storeSelect = IM.empty
    , storeSelectOpen = IM.empty
    , storeDisabled = IM.empty
    , storeFlag = IM.empty
    , storeNote = IM.empty
    , storeWindow = IM.empty
    , storeWindowSize = IM.empty
    }
