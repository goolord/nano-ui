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

import Data.Primitive.PrimArray (PrimArray, indexPrimArray, sizeofPrimArray)
import Data.Primitive.SmallArray (SmallArray)
import Data.Text (Text)
import Data.Vector.Unboxed qualified as U
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import NanoUI (Color, Response)

-- | Lower and upper axis bounds. Use finite, increasing bounds for plotting.
data Domain = Domain !Double !Double
  deriving (Eq, Show)

-- | Numeric x/y pairs or category labels with y values. Category arrays must
-- have equal lengths; use the constructors in "NanoUI.Plot.Series" to build them.
-- Points and values that are not finite (NaN, infinities) are not drawn.
data SeriesData
  = PointsXY !(U.Vector (Double, Double))
  | -- | Bar labels and their values, in order.
    CategoryY !(SmallArray Text) !(PrimArray Double)
  deriving (Show)

-- Points compare pair by pair, as the vector's own instance does, without
-- boxing each pair: charts are compared every frame to reuse their drawing.
-- A NaN equals the same NaN, or a chart holding one would be new every
-- frame, rebuilt and repainted forever.
instance Eq SeriesData where
  PointsXY a == PointsXY b = U.length a == U.length b && go 0
    where
      go !i
        | i >= U.length a = True
        | otherwise = case (U.unsafeIndex a i, U.unsafeIndex b i) of
            ((x, y), (x', y')) -> same x x' && same y y' && go (i + 1)
  CategoryY labels values == CategoryY labels' values' =
    labels == labels' && n == sizeofPrimArray values' && go 0
    where
      n = sizeofPrimArray values
      go !i = i >= n || (same (indexPrimArray values i) (indexPrimArray values' i) && go (i + 1))
  _ == _ = False

same :: Double -> Double -> Bool
same a b = a == b || castDoubleToWord64 a == castDoubleToWord64 b

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
  deriving (Show)

-- A NaN parameter equals the same NaN, as in 'SeriesData'.
instance Eq SeriesKind where
  LineSeries w m == LineSeries w' m' = sameF w w' && m == m'
  ScatterSeries w m == ScatterSeries w' m' = sameF w w' && m == m'
  BarSeries f == BarSeries f' = sameF f f'
  AreaSeries b == AreaSeries b' = same b b'
  StepSeries w == StepSeries w' = sameF w w'
  _ == _ = False

sameF :: Float -> Float -> Bool
sameF a b = a == b || castFloatToWord32 a == castFloatToWord32 b

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

-- | Series and chart decoration. Decimation reduces line, area and step
-- series before drawing and hit testing; it does not alter the stored data.
-- Scatter series and lines with markers are thinned only past 2000 points.
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
-- the point index addresses the drawn points, which leave out those that are
-- not finite and are decimated when decimation is enabled.
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
