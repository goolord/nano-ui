{-# LANGUAGE StrictData #-}

-- | Visibility values and typed sensor storage, independent of frame execution.
module NanoUI.Internal.Sensor.Types
  ( Visibility (..)
  , VisibilityEvent (..)
  , SensorConfig (..)
  , Sensors (..)
  , SensorState (..)
  , Cell (..)
  , CellState (..)
  , Watch (..)
  , Seen (..)
  , ClipMemo (..)
  , MemoEntry (..)
  )
where

import Data.IORef (IORef)
import Data.IntMap.Strict (IntMap)
import Data.Primitive.Array (MutableArray)
import GHC.Exts (RealWorld)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Style (Layout)
import NanoUI.Internal.Types (Rect)

-- | Visibility at the last completed layout. Movement alone emits no event.
data Visibility = Visibility
  { visVisible :: !Bool
  -- ^ Overlaps the window and ancestor clips, including the anticipation
  -- margin, for at least sensorDelay. Empty extents count as lines or points.
  , visEvent :: !(Maybe VisibilityEvent)
  -- ^ Consumed by the first view pass that reads it.
  , visRect :: !Rect
  -- ^ Visible portion without anticipation margin; empty off screen.
  , visBounds :: !Rect
  -- ^ Whole widget bounds; empty when no node exists.
  }
  deriving (Eq, Show)

data VisibilityEvent = BecameVisible | BecameHidden
  deriving (Eq, Show)

data SensorConfig = SensorConfig
  { sensorAnticipate :: Float
  -- ^ Grow the window and clips by this many logical pixels; negatives act as 0.
  , sensorDelay :: Double
  -- ^ Seconds continuously visible before entering; leaving is immediate.
  , sensorLayout :: Layout -> Layout
  -- ^ Container layout modifier, ignored by useVisibility.
  }

data Sensors = Sensors (IORef SensorState) (IORef ClipMemo)

-- | Pass, watched cells and count, and all retained cells with their count.
data SensorState = SensorState Int [Cell] Int (IntMap Cell) Int

data Cell = Cell Int (IORef CellState)

data CellState = CellState Int Watch Seen

data Watch = Watch WidgetId SensorConfig

data Seen = Seen Visibility Double

-- | Reused ancestor-clip array, invalidated by a per-layout stamp.
data ClipMemo = ClipMemo Int (MutableArray RealWorld MemoEntry)

data MemoEntry = NoClips | Clips Int Float (Maybe Rect, Maybe Rect)
