{-# LANGUAGE StrictData #-}

-- | Visibility sensors: whether a widget is on screen, and the frame it
-- scrolls into view or out of it.
--
-- A sensor is a watch the view registers each build ('sensorConfigured',
-- 'useVisibility'), keyed by the sensor's own widget id and naming the widget
-- it watches. The frame measures every watched widget once its layout is
-- final ('updateSensors') and keeps the result for the next build to read, so
-- a view sees the visibility of the last layout, as 'NanoUI.Internal.Widgets.Node.respRect'
-- sees its rect. A change of visibility asks for a follow-up frame, where the
-- view reads it; a sensor that is not built is forgotten.
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
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
import NanoUI.Internal.Frame.Node (readScrollNode)
import NanoUI.Internal.Frame.Scroll.Geometry (borderContentClip, scrollNodeViewport)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Monad (Ui, askContext, askDefaultLayout, currentId, freshWidget, uiIO)
import NanoUI.Internal.Style (Direction (..), Layout (..), themePanel, tight)
import NanoUI.Internal.Types (Rect (..), Size (..), rectInflate, rectIntersect)
import NanoUI.Internal.Widgets.Node (container, tagContainer)

-- | Whether a widget was on screen when the last frame was laid out.
data Visibility = Visibility
  { visVisible :: !Bool
    -- ^ The widget overlapped the window and the inside of every scroller,
    -- panel and floating panel around it, each grown by the sensor's
    -- anticipate margin: it was painted, or would have been with that much
    -- more room. A widget of zero width or height counts as the line or point
    -- it sits at.
  , visEvent :: !(Maybe VisibilityEvent)
    -- ^ How 'visVisible' changed at the last layout. Reported once: to the
    -- first view pass that reads the sensor after the change.
  , visRect :: !Rect
    -- ^ The part of the widget on screen at the last layout, in logical
    -- window coordinates, without the anticipate margin; empty when no part
    -- is. Like 'NanoUI.Internal.Widgets.Node.respRect' it is last frame's,
    -- and moving alone asks for no frame.
  }
  deriving (Eq, Show)

-- | A widget coming into view or going out of it.
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

-- | How a 'sensorConfigured' sensor lays out its body and when it counts as
-- visible.
data SensorConfig = SensorConfig
  { sensorAnticipate :: Float
    -- ^ Logical pixels the window, and every scroller, panel and floating
    -- panel around the widget, grow by for it: a sensor with a margin of 200
    -- reports its widget visible while it is still up to 200 pixels outside
    -- the viewport, in time to start loading it. Negative margins count as 0.
  , sensorLayout :: Layout -> Layout
    -- ^ Modifies the sensor's container: a column without padding.
  }

-- | No margin, and the body in a column without padding.
defaultSensorConfig :: SensorConfig
defaultSensorConfig = SensorConfig {sensorAnticipate = 0, sensorLayout = id}

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

-- | 'sensor' with an anticipate margin as well as layout. The container takes
-- one widget id, like a 'NanoUI.Internal.Widgets.Layout.column', and its body
-- runs in an id scope of its own.
sensorConfigured :: Ui :> es => SensorConfig -> Eff es a -> Eff es (Visibility, a)
sensorConfigured cfg body = do
  -- The id the container takes as it opens its scope, which no other widget
  -- has: the sensor is keyed by it, and the container carries it.
  wid <- currentId
  ctx <- askContext
  base <- askDefaultLayout
  let layout = sensorLayout cfg (tight base) {layoutDirection = Column}
  a <- container NodeContainer layout (tagContainer wid >> body)
  vis <- uiIO (watchSensor ctx wid wid (sensorAnticipate cfg))
  pure (vis, a)

-- | The 'Visibility' of the widget with id @target@, such as @respId resp@ of
-- a widget built before it, with an anticipate margin of @margin@ logical
-- pixels (see 'sensorAnticipate'). A hook: it takes the next widget id, so
-- call it on every frame, like the other hooks. A target with no node this
-- frame is hidden.
--
-- > resp <- image' (fixedWH 96 96) thumb
-- > vis <- useVisibility 200 (respId resp)
useVisibility :: Ui :> es => Float -> WidgetId -> Eff es Visibility
useVisibility margin target = do
  (wid, ctx) <- freshWidget
  uiIO (watchSensor ctx wid target margin)

-- | Sensors on a context: those built this pass, and what each one measured
-- at the last layout, both by the sensor's own key. Kept as a host value
-- ('hostOrInit'), which a context without sensors never allocates.
newtype Sensors = Sensors (IORef SensorState)

-- | The sensors built this pass, and what each one saw at the last layout.
data SensorState = SensorState (IntMap Watch) (IntMap Visibility)

-- | The widget a sensor watches, and its anticipate margin.
data Watch = Watch WidgetId Float

-- | Register sensor @wid@ watching @target@ for this pass, and return what it
-- saw at the last layout. Reading an event consumes it, so a second view pass
-- in the same frame sees the state without the event.
watchSensor :: Context -> WidgetId -> WidgetId -> Float -> IO Visibility
watchSensor ctx wid target margin = do
  Sensors ref <- hostOrInit ctx (Sensors <$> newIORef (SensorState IM.empty IM.empty))
  SensorState watched seen <- readIORef ref
  let k = intKey wid
      vis = fromMaybe notSeen (IM.lookup k seen)
      seen' = if isJust (visEvent vis) then IM.insert k vis {visEvent = Nothing} seen else seen
  writeIORef ref $! SensorState (IM.insert k (Watch target margin) watched) seen'
  pure vis

notSeen :: Visibility
notSeen = Visibility False Nothing (Rect 0 0 0 0)

-- | Forget the sensors the last view pass built: the view is about to run
-- again and build its own.
beginSensors :: Context -> IO ()
beginSensors ctx =
  askHostIO ctx >>= mapM_ (\(Sensors ref) -> modifyIORef' ref (\(SensorState _ seen) -> SensorState IM.empty seen))

-- | Measure every sensor the view built against the final layout of a
-- window of @size@. A sensor not built is dropped with what it saw. A change
-- of visibility marks the context dirty, so a follow-up frame shows it to the
-- view; whatever the view does about it can change paint state no diff
-- describes, so the follow-up repaints as a model change does.
updateSensors :: Context -> Size -> IO ()
updateSensors ctx size =
  askHostIO ctx >>= mapM_ (\(Sensors ref) -> do
    SensorState watched seen <- readIORef ref
    unless (IM.null watched && IM.null seen) $ do
      seen' <- IM.traverseWithKey (\k w -> measureSensor ctx size (IM.lookup k seen) w) watched
      writeIORef ref $! SensorState watched seen'
      when (any (isJust . visEvent) seen') (markDirty ctx))

measureSensor :: Context -> Size -> Maybe Visibility -> Watch -> IO Visibility
measureSensor ctx@Context {ctxNodeArena = na} size before (Watch target margin) = do
  mIdx <- lookupNodeByWidgetId na target
  (shown, onScreen) <- case mIdx of
    Nothing -> pure (False, Nothing)
    Just idx -> do
      rect <- getNodeRect na idx
      (exact, grown) <- paintClips ctx size (max 0 margin) idx
      pure (maybe False (overlaps rect) grown, exact >>= rectIntersect rect)
  let was = maybe False visVisible before
      event
        | shown == was = Nothing
        | shown = Just BecameVisible
        | otherwise = Just BecameHidden
  pure (Visibility shown event (fromMaybe (Rect 0 0 0 0) onScreen))

-- | The part of a window of @size@ node @idx@ is painted within, and the part
-- it would be with every clip grown by @margin@: 'Nothing' when paint does not
-- reach it. Paint clips a node to the viewport of every scroller around it,
-- the inside of every panel and the rect of every widget, and skips a node
-- outside its clip, or empty, with everything inside it, except that it
-- still walks a plain container with a pinned node below it. A floating panel
-- paints over the page clipped to itself alone, so what it is declared in
-- does not matter.
paintClips :: Context -> Size -> Float -> NodeIdx -> IO (Maybe Rect, Maybe Rect)
paintClips ctx@Context {ctxNodeArena = na} (Size ww wh) margin idx = do
  floating <- isFloatingNode <$> getNodeType na idx
  chain <- if floating then pure [] else getParent na idx >>= outward []
  let window = Rect 0 0 ww wh
  foldM enter (Just window, Just (rectInflate margin window)) chain
  where
    -- The node's ancestors up to the root or the first floating panel,
    -- outermost first.
    outward acc i
      | i < 0 = pure acc
      | otherwise = do
          floating <- isFloatingNode <$> getNodeType na i
          if floating then pure (i : acc) else getParent na i >>= outward (i : acc)
    enter clips@(_, Nothing) _ = pure clips
    enter (exact, grown) i = do
      nt <- getNodeType na i
      rect@(Rect x y w h) <- getNodeRect na i
      cut <- case nt of
        NodeContainer -> pure Nothing
        NodeScrollContainer -> (\sn -> Just (scrollNodeViewport sn x y w h)) <$> readScrollNode na i
        NodePanel -> (\theme -> Just (borderContentClip (themePanel theme) rect)) <$> nodeTheme ctx i
        _ -> pure (Just rect)
      -- Paint still walks a plain container it would skip when a pinned node
      -- is below it: the container clips nothing, so the pinned node can show
      -- outside it, even when it has no size or is off screen.
      walked <- if nt == NodeContainer then hasPinnedBelow na i else pure False
      let within grow clip = do
            c <- clip
            unless walked (void (rectIntersect c rect))
            maybe (Just c) (rectIntersect c . grow) cut
      pure (within id exact, within (rectInflate margin) grown)

-- | Whether @r@ overlaps @clip@, with an empty extent of @r@ counting as the
-- line or point it sits at.
overlaps :: Rect -> Rect -> Bool
overlaps (Rect x y w h) (Rect cx cy cw ch) = along x w cx cw && along y h cy ch
  where
    along a len c clen = a < c + clen && (if len > 0 then c < a + len else c <= a)
