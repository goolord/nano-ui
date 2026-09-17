{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Plot.Types
  ( Domain (..)
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

import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.SmallArray (SmallArray)
import Data.Text (Text)
import Data.Vector.Unboxed qualified as U
import NanoUI (Color, Response)

data Domain = Domain !Double !Double
  deriving (Eq, Show)

data SeriesData
  = PointsXY !(U.Vector (Double, Double))
  | -- | Bar labels and their values, in order.
    CategoryY !(SmallArray Text) !(PrimArray Double)
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
