{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Plot.Types
  ( Domain (..)
  , Range (..)
  , ScaleType (..)
  , SeriesData (..)
  , MarkShape (..)
  , SeriesKind (..)
  , Series (..)
  , LegendPos (..)
  , GridMode (..)
  , Chart (..)
  , PlotHover (..)
  , PlotResponse (..)
  , emptyChart
  ) where

import Data.Text (Text)
import Data.Vector (Vector)
import NanoUI (Color, Response)

data Domain = Domain !Double !Double
  deriving (Eq, Show)

data Range = Range !Double !Double
  deriving (Eq, Show)

data ScaleType = Linear | Log | Category
  deriving (Eq, Show)

data SeriesData
  = PointsXY !(Vector (Double, Double))
  | CategoryY !(Vector (Text, Double))
  deriving (Eq, Show)

data MarkShape = MarkCircle | MarkSquare | MarkDiamond | MarkTriangle | MarkCross
  deriving (Eq, Show)

data SeriesKind
  = LineSeries !Float (Maybe MarkShape)
  | ScatterSeries !Float !MarkShape
  | BarSeries !Float
  | AreaSeries !Double
  | StepSeries !Float
  deriving (Eq, Show)

data Series = Series
  { seriesName :: !Text
  , seriesColor :: !(Maybe Color)
  , seriesKind :: !SeriesKind
  , seriesData :: !SeriesData
  }
  deriving (Eq, Show)

data LegendPos = LegendRight | LegendBottom | LegendTop | LegendInside | LegendNone
  deriving (Eq, Show)

data GridMode = GridBoth | GridHorizontal | GridVertical | GridNone
  deriving (Eq, Show)

data Chart = Chart
  { chartTitle :: !(Maybe Text)
  , chartXTitle :: !(Maybe Text)
  , chartYTitle :: !(Maybe Text)
  , chartSeries :: ![Series]
  , chartLegend :: !LegendPos
  , chartGrid :: !GridMode
  , chartDecimate :: !Bool
  }
  deriving (Eq, Show)

data PlotHover = PlotHover
  { hoverDataX :: !Double
  , hoverDataY :: !Double
  , hoverSeriesIdx :: !Int
  , hoverPointIdx :: !Int
  }
  deriving (Eq, Show)

data PlotResponse = PlotResponse
  { plotResponse :: !Response
  , plotHover :: !(Maybe PlotHover)
  }
  deriving (Eq, Show)

emptyChart :: Chart
emptyChart =
  Chart
    { chartTitle = Nothing
    , chartXTitle = Nothing
    , chartYTitle = Nothing
    , chartSeries = []
    , chartLegend = LegendRight
    , chartGrid = GridBoth
    , chartDecimate = True
    }
