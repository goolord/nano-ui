-- | Chart, series, domain and hover types.
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

-- | Lower and upper axis bounds. Use finite, increasing bounds for plotting.
data Domain = Domain !Double !Double
  deriving (Eq, Show)

-- | Numeric x/y pairs or category labels with y values. Category arrays must
-- have equal lengths; use the constructors in "NanoUI.Plot.Series" to build them.
data SeriesData
  = PointsXY !(U.Vector (Double, Double))
  | -- | Bar labels and their values, in order.
    CategoryY !(SmallArray Text) !(PrimArray Double)
  deriving (Eq, Show)

-- | Shape of a plotted point marker.
data MarkShape = MarkCircle | MarkSquare | MarkDiamond | MarkTriangle | MarkCross
  deriving (Eq, Show)

-- | Drawing method and its size parameter: line/step stroke width, scatter
-- marker size, bar fraction of a category slot, or area baseline in data units.
data SeriesKind
  = LineSeries !Float (Maybe MarkShape)
  | ScatterSeries !Float !MarkShape
  | BarSeries !Float
  | AreaSeries !Double
  | StepSeries !Float
  deriving (Eq, Show)

-- | Named data and rendering options. An absent colour selects the chart's
-- theme palette by series index.
data Series = Series
  { seriesName :: !Text
  , seriesColor :: !(Maybe Color)
  , seriesKind :: !SeriesKind
  , seriesData :: !SeriesData
  }
  deriving (Eq, Show)

-- | Legend position relative to the plot, or no legend.
data LegendPos = LegendRight | LegendBottom | LegendTop | LegendInside | LegendNone
  deriving (Eq, Show)

-- | Which axis grid lines to draw through the data box.
data GridMode = GridBoth | GridHorizontal | GridVertical | GridNone
  deriving (Eq, Show)

-- | Series and chart decoration. Decimation reduces numeric series before
-- drawing and hit testing; it does not alter the stored data.
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

-- | Nearest drawn point in data coordinates. Both indices are zero-based;
-- the point index addresses the decimated series when decimation is enabled.
data PlotHover = PlotHover
  { hoverDataX :: !Double
  , hoverDataY :: !Double
  , hoverSeriesIdx :: !Int
  , hoverPointIdx :: !Int
  }
  deriving (Eq, Show)

-- | Widget response plus the nearest point while the pointer is inside the
-- data box. Hover is 'Nothing' outside it or when there are no points.
data PlotResponse = PlotResponse
  { plotResponse :: !Response
  , plotHover :: !(Maybe PlotHover)
  }
  deriving (Eq, Show)

-- | No series or titles, a right-hand legend, both grid directions, and decimation.
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
