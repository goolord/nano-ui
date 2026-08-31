module NanoUI.Store
  ( WidgetStore (..)
  , emptyWidgetStore
  , mirrorStoresChanged
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Text (Text)
import Data.Word (Word32)
import qualified Data.IntMap.Strict as IM

data WidgetStore = WidgetStore
  { storeCheckbox :: IntMap Bool
  , storeRadio :: IntMap Int
  , storeSlider :: IntMap Float
  , storeText :: IntMap Text
  , storeCursor :: IntMap Int
  , storeSelAnchor :: IntMap Int
  , storeScroll :: IntMap Float
  , storeSelect :: IntMap Int
  , storeSelectOpen :: IntMap Bool
  , storeColor :: IntMap Word32
  , storeColorHue :: IntMap Float
  , storeColorSv :: IntMap (Float, Float)
  , storeColorDrag :: IntMap Int
  , storeTreeSelected :: IntMap Int
  , storeTreeExpanded :: IntMap IntSet
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
    , storeRadio = IM.empty
    , storeSlider = IM.empty
    , storeText = IM.empty
    , storeCursor = IM.empty
    , storeSelAnchor = IM.empty
    , storeScroll = IM.empty
    , storeSelect = IM.empty
    , storeSelectOpen = IM.empty
    , storeColor = IM.empty
    , storeColorHue = IM.empty
    , storeColorSv = IM.empty
    , storeColorDrag = IM.empty
    , storeTreeSelected = IM.empty
    , storeTreeExpanded = IM.empty
    , storeDisabled = IM.empty
    , storeFlag = IM.empty
    , storeNote = IM.empty
    , storeWindow = IM.empty
    , storeWindowSize = IM.empty
    }

-- useText/useFlag mirror into these maps. Frame re-runs UI when they change.
mirrorStoresChanged :: WidgetStore -> WidgetStore -> Bool
mirrorStoresChanged old new =
  storeNote old /= storeNote new || storeFlag old /= storeFlag new
