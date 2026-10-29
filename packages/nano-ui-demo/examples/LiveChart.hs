-- | LiveChart: a chart fed live data from a background producer.
--
-- A 'useStream' producer runs on a worker thread and appends a random-walk
-- sample every 100 ms. Each 'update' it makes wakes the loop for one frame,
-- and the view draws the latest window of ~200 points with 'plot' from
-- @nano-ui-diagrams@ (see Tasks.hs for the background-work rules).
--
-- 'plot' keeps the chart's drawing in a 'PlotCache' and rebuilds it only when
-- the chart differs from the one it drew last time: once per new sample while
-- the feed runs, never while it is paused. Building the 'Chart' value each
-- frame is fine; comparing 200 points is cheap.
--
-- Pause stops calling 'useStream', and that alone kills the producer.
-- With nothing running and nothing changing, the loop sleeps: a paused
-- chart costs no frames at all. Stopping a stream discards its state, so the
-- view saves the last samples on Pause and passes them as the stream's
-- initial value on Resume.
--
-- Run it with @cabal run nano-ui-example-live-chart@.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Data.Bits (shiftR)
import Data.Foldable (for_)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)
import NanoUI.Diagrams
  ( Chart
  , GridMode (..)
  , LegendPos (..)
  , PlotCache
  , PlotHover (..)
  , PlotResponse (..)
  , chart
  , line
  , newPlotCache
  , plot
  , withGrid
  , withLegend
  , withXAxis
  , withYAxis
  )
import Numeric (showFFloat)

-- | Everything the producer needs to carry on: the next sample's index, the
-- random generator's seed, the current level, and the visible window.
data Feed = Feed
  { feedIndex :: !Int
  , feedSeed :: !Word64
  , feedLevel :: !Double
  , feedPoints :: !(Seq (Double, Double))
  }
  deriving Eq

emptyFeed :: Feed
emptyFeed = Feed 0 42 0 Seq.empty

-- | How many samples the chart shows.
keepSamples :: Int
keepSamples = 200

-- | Add one sample: step a 64-bit linear congruential generator (Knuth's
-- MMIX constants) and move the level by up to half a unit either way.
advance :: Feed -> Feed
advance feed =
  let
    seed' = feedSeed feed * 6364136223846793005 + 1442695040888963407
    -- The top 53 bits as a Double in [0, 1).
    unit = fromIntegral (seed' `shiftR` 11) / 2 ^ (53 :: Int) :: Double
    level' = feedLevel feed + unit - 0.5
    !sample = (fromIntegral (feedIndex feed), level')
    kept = feedPoints feed |> sample
   in
    Feed
      (feedIndex feed + 1)
      seed'
      level'
      (Seq.drop (Seq.length kept - keepSamples) kept)

-- ANCHOR: chart_handles
data App = App
  { appFeed :: !(Stream () Feed)
  , appSaved :: !(Var Feed)
  , appPlots :: !PlotCache
  }

newApp :: IO App
newApp = App <$> newStream <*> newVar emptyFeed <*> newPlotCache
-- ANCHOR_END: chart_handles

main :: IO ()
main = do
  app <- newApp
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings =
          defaultWindowSettings {wsTitle = "Live chart", wsSize = Size 760 520}
      }
    (view app)

-- ANCHOR: chart_card
view :: App -> NanoUI ()
view app = columnWith (padAll 16 . gap 12 . grow) $ do
  running <- useVar True
  isRunning <- readVar running
  saved <- readVar (appSaved app)
  -- While running, the stream is the source of truth; 'saved' only seeds a
  -- restarted producer and is ignored after its first frame. While paused,
  -- 'useStream' is not called and the saved samples are shown instead.
  feed <-
    scope $
      if isRunning
        then useStream (appFeed app) () saved $ \update -> forever $ do
          threadDelay 100000
          update advance
        else pure saved

  rowWith (tight . gap 8 . alignMid . fillW) $ do
    whenM (button (if isRunning then "Pause" else "Resume")) $ do
      when isRunning (writeVar (appSaved app) feed)
      writeVar running (not isRunning)
    muted
      (if isRunning then "Live: a sample every 100 ms" else "Paused: the loop is idle")

  let
    points = feedPoints feed
  -- A crosshair for reading values off the plot; the response reports the
  -- sample nearest the pointer.
  resp <-
    withCursorShape
      UiCursorCrosshair
      (plot (appPlots app) (fillW . minH 300) (walkChart points))

  rowWith (tight . gap 16 . alignMid . fillW) $ do
    let
      ys = fmap snd points
    -- Both parts come and go, so each sits in a scope.
    scope $
      if Seq.null ys
        then muted "Waiting for the first sample..."
        else do
          kv "min" (fmt (minimum ys))
          kv "max" (fmt (maximum ys))
          kv "last" (fmt (feedLevel feed))
    scope . for_ (plotHover resp) $ \hover ->
      kv
        "cursor"
        ( "#"
            <> T.pack (show (round (hoverDataX hover) :: Int))
            <> " = "
            <> fmt (hoverDataY hover)
        )
-- ANCHOR_END: chart_card

-- ANCHOR: walk_chart
walkChart :: Seq (Double, Double) -> Chart
walkChart points =
  withGrid GridBoth
    . withLegend LegendNone
    . withXAxis "sample"
    . withYAxis "value" $
    chart [line "walk" points]
-- ANCHOR_END: walk_chart

fmt :: Double -> Text
fmt x = T.pack (showFFloat (Just 2) x "")
