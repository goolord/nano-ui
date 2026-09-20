-- | Series constructors (line, scatter, bar, area, step) and their style
-- modifiers.
module NanoUI.Plot.Series
  ( line
  , scatter
  , bar
  , area
  , step
  , withColor
  , withStrokeWidth
  , withMarker
  , withBaseline
  , lineVec
  , scatterVec
  , areaVec
  , stepVec
  , barVec
  ) where

import Data.Text (Text)
import Data.Foldable (toList)
import Data.Primitive.PrimArray (generatePrimArray)
import Data.Primitive.SmallArray (createSmallArray, writeSmallArray)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as G
import Data.Vector.Unboxed qualified as U
import NanoUI (Color)
import NanoUI.Plot.Types
  ( MarkShape (..)
  , Series (..)
  , SeriesData (..)
  , SeriesKind (..)
  )

-- | Connect numeric points in input order with a 1.5-width line. Points are
-- not sorted; supply them in the desired path order.
line :: Foldable f => Text -> f (Double, Double) -> Series
line name = lineVec name . U.fromList . toList

-- | Plot numeric points as circle markers of size 3.
scatter :: Foldable f => Text -> f (Double, Double) -> Series
scatter name = scatterVec name . U.fromList . toList

-- | Category bars in input order, occupying 72% of each category slot.
bar :: Foldable f => Text -> f (Text, Double) -> Series
bar name = barVec name . V.fromList . toList

-- | Fill the path through numeric points down to a baseline of zero.
area :: Foldable f => Text -> f (Double, Double) -> Series
area name = areaVec name . U.fromList . toList

-- | Join points in input order with horizontal and vertical steps.
step :: Foldable f => Text -> f (Double, Double) -> Series
step name = stepVec name . U.fromList . toList

-- | Override the theme palette colour for this series.
withColor :: Color -> Series -> Series
withColor c s = s {seriesColor = Just c}

-- | Set line/step width, scatter marker size, or bar slot fraction. Has no
-- effect on area series. The parameter is interpreted by the series kind.
withStrokeWidth :: Float -> Series -> Series
withStrokeWidth w s =
  case seriesKind s of
    LineSeries _ m -> s {seriesKind = LineSeries w m}
    ScatterSeries _ mk -> s {seriesKind = ScatterSeries w mk}
    BarSeries _ -> s {seriesKind = BarSeries w}
    StepSeries _ -> s {seriesKind = StepSeries w}
    _ -> s

-- | Set the marker option on line or scatter series; leave other kinds unchanged.
-- The renderer draws scatter markers; the line marker option is stored but
-- does not add markers to line geometry.
withMarker :: MarkShape -> Series -> Series
withMarker mk s =
  case seriesKind s of
    LineSeries w _ -> s {seriesKind = LineSeries w (Just mk)}
    ScatterSeries w _ -> s {seriesKind = ScatterSeries w mk}
    _ -> s

-- | Set an area series' baseline in data units; leave other kinds unchanged.
withBaseline :: Double -> Series -> Series
withBaseline b s =
  case seriesKind s of
    AreaSeries _ -> s {seriesKind = AreaSeries b}
    _ -> s

-- | Numeric series retain unboxed coordinates. Unboxed inputs are shared;
-- boxed/storable inputs are converted once at the construction boundary.
{-# INLINE lineVec #-}
lineVec :: G.Vector v (Double, Double) => Text -> v (Double, Double) -> Series
lineVec name pts = Series name Nothing (LineSeries 1.5 Nothing) (PointsXY (G.convert pts))

-- | Vector form of 'scatter'; converts coordinates to unboxed storage once.
{-# INLINE scatterVec #-}
scatterVec :: G.Vector v (Double, Double) => Text -> v (Double, Double) -> Series
scatterVec name pts = Series name Nothing (ScatterSeries 3 MarkCircle) (PointsXY (G.convert pts))

-- | Vector form of 'area', with a baseline of zero.
{-# INLINE areaVec #-}
areaVec :: G.Vector v (Double, Double) => Text -> v (Double, Double) -> Series
areaVec name pts = Series name Nothing (AreaSeries 0) (PointsXY (G.convert pts))

-- | Vector form of 'step', preserving point order.
{-# INLINE stepVec #-}
stepVec :: G.Vector v (Double, Double) => Text -> v (Double, Double) -> Series
stepVec name pts = Series name Nothing (StepSeries 1.5) (PointsXY (G.convert pts))

-- | Vector form of 'bar'. Copies labels and values into equally sized arrays.
barVec :: Text -> Vector (Text, Double) -> Series
barVec name rows =
  Series name Nothing (BarSeries 0.72) $
    CategoryY
      (createSmallArray n "" (\out -> V.imapM_ (\i (t, _) -> writeSmallArray out i t) rows))
      (generatePrimArray n (snd . V.unsafeIndex rows))
  where
    n = V.length rows
