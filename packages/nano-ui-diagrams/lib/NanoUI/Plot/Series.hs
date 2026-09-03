{-# LANGUAGE OverloadedStrings #-}

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
import Data.Vector (Vector)
import Data.Vector qualified as V
import NanoUI (Color)
import NanoUI.Plot.Types
  ( MarkShape (..)
  , Series (..)
  , SeriesData (..)
  , SeriesKind (..)
  )

line :: Text -> [(Double, Double)] -> Series
line name pts =
  Series name Nothing (LineSeries 1.5 Nothing) (PointsXY (V.fromList pts))

scatter :: Text -> [(Double, Double)] -> Series
scatter name pts =
  Series name Nothing (ScatterSeries 3 MarkCircle) (PointsXY (V.fromList pts))

bar :: Text -> [(Text, Double)] -> Series
bar name pts =
  Series name Nothing (BarSeries 0.72) (CategoryY (V.fromList pts))

area :: Text -> [(Double, Double)] -> Series
area name pts =
  Series name Nothing (AreaSeries 0) (PointsXY (V.fromList pts))

step :: Text -> [(Double, Double)] -> Series
step name pts =
  Series name Nothing (StepSeries 1.5) (PointsXY (V.fromList pts))

withColor :: Color -> Series -> Series
withColor c s = s {seriesColor = Just c}

withStrokeWidth :: Float -> Series -> Series
withStrokeWidth w s =
  case seriesKind s of
    LineSeries _ m -> s {seriesKind = LineSeries w m}
    ScatterSeries _ mk -> s {seriesKind = ScatterSeries w mk}
    BarSeries _ -> s {seriesKind = BarSeries w}
    StepSeries _ -> s {seriesKind = StepSeries w}
    k -> s {seriesKind = k}

withMarker :: MarkShape -> Series -> Series
withMarker mk s =
  case seriesKind s of
    LineSeries w _ -> s {seriesKind = LineSeries w (Just mk)}
    ScatterSeries w _ -> s {seriesKind = ScatterSeries w mk}
    k -> s {seriesKind = k}

withBaseline :: Double -> Series -> Series
withBaseline b s =
  case seriesKind s of
    AreaSeries _ -> s {seriesKind = AreaSeries b}
    k -> s {seriesKind = k}

lineVec :: Text -> Vector (Double, Double) -> Series
lineVec name pts = Series name Nothing (LineSeries 1.5 Nothing) (PointsXY pts)

scatterVec :: Text -> Vector (Double, Double) -> Series
scatterVec name pts = Series name Nothing (ScatterSeries 3 MarkCircle) (PointsXY pts)

areaVec :: Text -> Vector (Double, Double) -> Series
areaVec name pts = Series name Nothing (AreaSeries 0) (PointsXY pts)

stepVec :: Text -> Vector (Double, Double) -> Series
stepVec name pts = Series name Nothing (StepSeries 1.5) (PointsXY pts)

barVec :: Text -> Vector (Text, Double) -> Series
barVec name pts = Series name Nothing (BarSeries 0.72) (CategoryY pts)
