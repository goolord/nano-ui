{-# LANGUAGE StrictData #-}

-- | Visibility sensors: whether a widget is on screen, and when it scrolls
-- into or out of view.
--
-- The view registers a watch each build ('sensorConfigured', 'useVisibility'),
-- keyed by the sensor's widget id. After layout, 'updateSensors' measures each
-- target and stores the result for the next build, so like
-- 'NanoUI.Internal.Widgets.Node.respRect' the view sees last frame's layout.
-- A visibility change requests a follow-up frame. Sensors not built in a
-- pass are dropped.
module NanoUI.Internal.Widgets.Sensor
  ( Visibility (..)
  , VisibilityEvent (..)
  , becameVisible
  , becameHidden
  , SensorConfig (..)
  , defaultSensorConfig
  , sensor
  , sensorWith
  , sensorConfigured
  , useVisibility
  , beginSensors
  , updateSensors
  ) where

import Control.Monad (foldM, unless, void, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromMaybe, isJust)
import Data.Primitive.Array (MutableArray, newArray, readArray, sizeofMutableArray, writeArray)
import Effectful (Eff, type (:>))
import GHC.Clock (getMonotonicTime)
import GHC.Exts (RealWorld)
import NanoUI.Internal.Context
import NanoUI.Internal.Frame.Node (childPaintClip)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Monad (Ui, askContext, askDefaultLayout, currentId, freshWidget, uiIO)
import NanoUI.Internal.Style (Direction (..), Layout (..), tight)
import NanoUI.Internal.Types (Rect (..), Size (..), rectInflate, rectIntersect)
import NanoUI.Internal.Widgets.Node (container, tagContainer)

-- | Whether a widget was on screen when the last frame was laid out.
data Visibility = Visibility
  { visVisible :: !Bool
    -- ^ The widget overlapped the window and every enclosing scroller and
    -- panel, each grown by 'sensorAnticipate', for at least 'sensorDelay'.
    -- A zero-width or zero-height widget counts as a line or point.
  , visEvent :: !(Maybe VisibilityEvent)
    -- ^ How 'visVisible' changed at the last layout. Reported only to the
    -- first view pass that reads the sensor after the change.
  , visRect :: !Rect
    -- ^ The on-screen part of the widget, in logical window coordinates,
    -- ignoring the anticipate margin; empty when off screen. Movement alone
    -- does not request a frame.
  , visBounds :: !Rect
    -- ^ The whole widget in logical window coordinates, on screen or not;
    -- empty when it had no node.
  }
  deriving (Eq, Show)

-- | A widget entering or leaving view.
data VisibilityEvent
  = BecameVisible
  | BecameHidden
  deriving (Eq, Show)

-- | Whether the widget came into view at the last layout.
becameVisible :: Visibility -> Bool
becameVisible v = visEvent v == Just BecameVisible

-- | Whether the widget went out of view at the last layout.
becameHidden :: Visibility -> Bool
becameHidden v = visEvent v == Just BecameHidden

-- | When a sensor counts its widget as visible, and how 'sensorConfigured'
-- lays out its body.
data SensorConfig = SensorConfig
  { sensorAnticipate :: Float
    -- ^ Logical pixels to grow the window and every enclosing clip by. With
    -- 200, the widget counts as visible up to 200 pixels before it scrolls
    -- in, in time to start loading it. Negative values count as 0.
  , sensorDelay :: Double
    -- ^ Seconds the widget must stay in view before it counts as visible,
    -- so a fast fling does not load every row it passes. Leaving view is
    -- immediate and restarts the wait. Waiting schedules a wake-up rather
    -- than drawing frames.
  , sensorLayout :: Layout -> Layout
    -- ^ Modifies the 'sensorConfigured' container, a column without padding.
    -- Ignored by 'useVisibility'.
  }

-- | No margin, no delay, unpadded column.
defaultSensorConfig :: SensorConfig
defaultSensorConfig = SensorConfig {sensorAnticipate = 0, sensorDelay = 0, sensorLayout = id}

-- | Run @body@ in a container that reports its 'Visibility'. The container is
-- a column without padding.
--
-- > (vis, _) <- sensor (label "Row 42")
-- > when (becameVisible vis) (uiIO (putStrLn "row 42 scrolled into view"))
{-# INLINE sensor #-}
sensor :: Ui :> es => Eff es a -> Eff es (Visibility, a)
sensor = sensorConfigured defaultSensorConfig

-- | 'sensor' with modified layout.
{-# INLINE sensorWith #-}
sensorWith :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es (Visibility, a)
sensorWith f = sensorConfigured defaultSensorConfig {sensorLayout = f}

-- | 'sensor' with a full 'SensorConfig'. Like
-- 'NanoUI.Internal.Widgets.Layout.column', the container takes one widget id
-- and runs its body in its own id scope.
sensorConfigured :: Ui :> es => SensorConfig -> Eff es a -> Eff es (Visibility, a)
sensorConfigured cfg body = do
  -- The container's own id, unique to it; the sensor is keyed by it.
  wid <- currentId
  ctx <- askContext
  base <- askDefaultLayout
  let layout = sensorLayout cfg (tight base) {layoutDirection = Column}
  a <- container NodeContainer layout (tagContainer wid >> body)
  vis <- uiIO (watchSensor ctx wid (Watch wid cfg))
  pure (vis, a)

-- | The 'Visibility' of the widget @target@, such as @respId resp@ of an
-- earlier widget. This is a hook: it takes the next widget id, so call it
-- every frame. A target with no node is hidden.
--
-- > resp <- image' (fixedWH 96 96) thumb
-- > vis <- useVisibility defaultSensorConfig {sensorAnticipate = 200} (respId resp)
useVisibility :: Ui :> es => SensorConfig -> WidgetId -> Eff es Visibility
useVisibility cfg target = do
  (wid, ctx) <- freshWidget
  uiIO (watchSensor ctx wid (Watch target cfg))

-- | Per-context sensor state, stored as a host value ('hostOrInit') so a
-- context without sensors never allocates it.
data Sensors = Sensors (IORef SensorState) (IORef ClipMemo)

-- | The pass number (bumped by 'beginSensors'), the cells watched this pass
-- (newest first, each once) and their count, and every sensor's cell by key
-- with the map's size. A steady frame writes cells, not the map.
data SensorState = SensorState Int [Cell] Int (IntMap Cell) Int

-- | One sensor, by key.
data Cell = Cell Int (IORef CellState)

-- | The pass that last watched the sensor, its last watch and its last result.
data CellState = CellState Int Watch Seen

data Watch = Watch WidgetId SensorConfig

-- | Last result, plus the monotonic time the widget entered view while the
-- 'sensorDelay' runs (0 otherwise).
data Seen = Seen Visibility Double

-- | Initial state for a new sensor: hidden, no node.
unseen :: Seen
unseen = Seen (Visibility False Nothing (Rect 0 0 0 0) (Rect 0 0 0 0)) 0

-- | Register a watch for this pass and return the last result. Reading
-- consumes the event, so a second view pass in the frame sees none.
watchSensor :: Context -> WidgetId -> Watch -> IO Visibility
watchSensor ctx wid watch = do
  Sensors ref _ <- hostOrInit ctx (Sensors <$> newIORef (SensorState 1 [] 0 IM.empty 0) <*> (newIORef . ClipMemo 0 =<< newArray 0 NoClips))
  SensorState pass watched n cells size <- readIORef ref
  let k = intKey wid
  case IM.lookup k cells of
    Just cell@(Cell _ cref) -> do
      CellState stamp _ seen@(Seen vis since) <- readIORef cref
      -- The last watch of a key wins; the cell is listed once.
      writeIORef cref $! CellState pass watch (if isJust (visEvent vis) then Seen vis {visEvent = Nothing} since else seen)
      when (stamp /= pass) $ writeIORef ref $! SensorState pass (cell : watched) (n + 1) cells size
      pure vis
    Nothing -> do
      cell <- Cell k <$> newIORef (CellState pass watch unseen)
      writeIORef ref $! SensorState pass (cell : watched) (n + 1) (IM.insert k cell cells) (size + 1)
      let Seen vis _ = unseen
      pure vis

-- | Start a pass: clear the last pass's watches before the view reruns.
beginSensors :: Context -> IO ()
beginSensors ctx =
  askHostIO ctx >>= mapM_ (\(Sensors ref _) -> modifyIORef' ref (\(SensorState pass _ _ cells size) -> SensorState (pass + 1) [] 0 cells size))

-- | Measure every watch against the final layout. Sensors not built this
-- pass are dropped. A visibility change marks the context dirty: the view's
-- reaction may change paint state no diff describes, so the follow-up frame
-- repaints like a model change.
updateSensors :: Context -> Size -> IO ()
updateSensors ctx size =
  askHostIO ctx >>= mapM_ (\(Sensors ref memoRef) -> do
    SensorState pass watched n _ count <- readIORef ref
    unless (n == 0 && count == 0) $ do
      memo <- freshMemo ctx memoRef
      let step !event (Cell _ cref) = do
            CellState stamp watch old <- readIORef cref
            new@(Seen vis _) <- measureSensor ctx memo size old watch
            unless (sameSeen old new) $ writeIORef cref $! CellState stamp watch new
            pure (event || isJust (visEvent vis))
      event <- foldM step False watched
      -- Every cell is listed at most once, so a count short of the map's
      -- means some sensor was not built this pass.
      when (count /= n) $
        writeIORef ref $! SensorState pass watched n (IM.fromList [(k, c) | c@(Cell k _) <- watched]) n
      when event (markDirty ctx))

sameSeen :: Seen -> Seen -> Bool
sameSeen (Seen a since) (Seen b since') = since == since' && a == b

-- | Measure one watch. While 'sensorDelay' runs, request a wake-up for when
-- it ends, as 'NanoUI.Internal.Widgets.Popup.tooltipTimer' does.
measureSensor :: Context -> ClipMemo -> Size -> Seen -> Watch -> IO Seen
measureSensor ctx@Context {ctxNodeArena = na} memo size (Seen before since0) (Watch target cfg) = do
  mIdx <- lookupNodeByWidgetId na target
  (inView, bounds, onScreen) <- case mIdx of
    Nothing -> pure (False, Rect 0 0 0 0, Nothing)
    Just idx -> do
      rect <- getNodeRect na idx
      (exact, grown) <- paintClips ctx memo size (max 0 (sensorAnticipate cfg)) idx
      pure (maybe False (overlaps rect) grown, rect, exact >>= rectIntersect rect)
  let was = visVisible before
      delay = sensorDelay cfg
  -- @since@ is kept only while the widget is in view but not yet visible.
  (shown, since) <-
    if not inView || was || delay <= 0
      then pure (inView, 0)
      else do
        now <- getMonotonicTime
        let t = if since0 > 0 then since0 else now
            due = t + delay
        if now >= due
          then pure (True, 0)
          else (False, t) <$ requestWakeAt ctx due
  let event
        | shown == was = Nothing
        | shown = Just BecameVisible
        | otherwise = Just BecameHidden
  pure (Seen (Visibility shown event (fromMaybe (Rect 0 0 0 0) onScreen) bounds) since)

-- | The clip node @idx@ is painted within, exactly and with every clip grown
-- by @margin@; 'Nothing' when paint never reaches it. Mirrors paint: clip to
-- each enclosing scroller viewport, panel interior and widget rect, and skip
-- subtrees that are empty or outside their clip, except plain containers with
-- a pinned node below. Floating panels clip only to themselves.
paintClips :: Context -> ClipMemo -> Size -> Float -> NodeIdx -> IO (Maybe Rect, Maybe Rect)
paintClips ctx@Context {ctxNodeArena = na} memo size margin idx = do
  floating <- isFloatingNode <$> getNodeType na idx
  if floating then pure (windowClips size margin) else getParent na idx >>= clipsInside ctx memo size margin

-- | The clips before any node: the window, and the window grown by @margin@.
windowClips :: Size -> Float -> (Maybe Rect, Maybe Rect)
windowClips (Size ww wh) margin = let window = Rect 0 0 ww wh in (Just window, Just (rectInflate margin window))

-- | The clips inside ancestor @i@ ('windowClips' for none), entered from the
-- root or the first floating panel down. Sensors under one ancestor share
-- them through the memo.
clipsInside :: Context -> ClipMemo -> Size -> Float -> NodeIdx -> IO (Maybe Rect, Maybe Rect)
clipsInside ctx@Context {ctxNodeArena = na} memo@(ClipMemo stamp entries) size margin i
  | i < 0 = pure (windowClips size margin)
  | otherwise = do
      entry <- readArray entries i
      case entry of
        Clips s m clips | s == stamp && m == margin -> pure clips
        _ -> do
          floating <- isFloatingNode <$> getNodeType na i
          outer <- if floating then pure (windowClips size margin) else getParent na i >>= clipsInside ctx memo size margin
          clips <- enterClips ctx margin outer i
          clips <$ writeArray entries i (Clips stamp margin clips)

-- | The clips inside node @i@, given those outside it.
enterClips :: Context -> Float -> (Maybe Rect, Maybe Rect) -> NodeIdx -> IO (Maybe Rect, Maybe Rect)
enterClips _ _ clips@(_, Nothing) _ = pure clips
enterClips ctx@Context {ctxNodeArena = na} margin (exact, grown) i = do
  nt <- getNodeType na i
  rect <- getNodeRect na i
  cut <- childPaintClip ctx i nt rect
  -- A plain container clips nothing, so a pinned descendant can show
  -- even when the container is empty or off screen.
  walked <- if nt == NodeContainer then hasPinnedBelow na i else pure False
  let within grow clip = do
        c <- clip
        unless walked (void (rectIntersect c rect))
        maybe (Just c) (rectIntersect c . grow) cut
  pure (within id exact, within (rectInflate margin) grown)

-- | The clips inside each ancestor by node index, for one 'updateSensors'.
-- Entries of an earlier stamp count as absent, so the array is reused
-- rather than cleared.
data ClipMemo = ClipMemo Int (MutableArray RealWorld MemoEntry)

-- | The stamp and margin the clips were computed for.
data MemoEntry = NoClips | Clips Int Float (Maybe Rect, Maybe Rect)

-- | Bump the stamp and make room for every node of this layout.
freshMemo :: Context -> IORef ClipMemo -> IO ClipMemo
freshMemo Context {ctxNodeArena = na} ref = do
  ClipMemo stamp memo <- readIORef ref
  nodes <- arenaCount na
  memo' <- if sizeofMutableArray memo >= nodes then pure memo else newArray (max nodes (2 * sizeofMutableArray memo)) NoClips
  let fresh = ClipMemo (stamp + 1) memo'
  fresh <$ writeIORef ref fresh

-- | Whether @r@ overlaps @clip@. An empty extent counts as a line or point.
overlaps :: Rect -> Rect -> Bool
overlaps (Rect x y w h) (Rect cx cy cw ch) = along x w cx cw && along y h cy ch
  where
    along a len c clen = a < c + clen && (if len > 0 then c < a + len else c <= a)
