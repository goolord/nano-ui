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

line :: Foldable f => Text -> f (Double, Double) -> Series
line name = lineVec name . U.fromList . toList

scatter :: Foldable f => Text -> f (Double, Double) -> Series
scatter name = scatterVec name . U.fromList . toList

bar :: Foldable f => Text -> f (Text, Double) -> Series
bar name = barVec name . V.fromList . toList

area :: Foldable f => Text -> f (Double, Double) -> Series
area name = areaVec name . U.fromList . toList

step :: Foldable f => Text -> f (Double, Double) -> Series
step name = stepVec name . U.fromList . toList

withColor :: Color -> Series -> Series
withColor c s = s {seriesColor = Just c}

withStrokeWidth :: Float -> Series -> Series
withStrokeWidth w s =
  case seriesKind s of
    LineSeries _ m -> s {seriesKind = LineSeries w m}
    ScatterSeries _ mk -> s {seriesKind = ScatterSeries w mk}
    BarSeries _ -> s {seriesKind = BarSeries w}
    StepSeries _ -> s {seriesKind = StepSeries w}
    _ -> s

withMarker :: MarkShape -> Series -> Series
withMarker mk s =
  case seriesKind s of
    LineSeries w _ -> s {seriesKind = LineSeries w (Just mk)}
    ScatterSeries w _ -> s {seriesKind = ScatterSeries w mk}
    _ -> s

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

{-# INLINE scatterVec #-}
scatterVec :: G.Vector v (Double, Double) => Text -> v (Double, Double) -> Series
scatterVec name pts = Series name Nothing (ScatterSeries 3 MarkCircle) (PointsXY (G.convert pts))

{-# INLINE areaVec #-}
areaVec :: G.Vector v (Double, Double) => Text -> v (Double, Double) -> Series
areaVec name pts = Series name Nothing (AreaSeries 0) (PointsXY (G.convert pts))

{-# INLINE stepVec #-}
stepVec :: G.Vector v (Double, Double) => Text -> v (Double, Double) -> Series
stepVec name pts = Series name Nothing (StepSeries 1.5) (PointsXY (G.convert pts))

barVec :: Text -> Vector (Text, Double) -> Series
barVec name rows =
  Series name Nothing (BarSeries 0.72) $
    CategoryY
      (createSmallArray n "" (\out -> V.imapM_ (\i (t, _) -> writeSmallArray out i t) rows))
      (generatePrimArray n (snd . V.unsafeIndex rows))
  where
    n = V.length rows
