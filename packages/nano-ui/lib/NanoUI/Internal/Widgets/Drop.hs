{-# LANGUAGE StrictData #-}

-- | Local pointer drags and operating-system drag and drop.
--
-- 'useDrop' turns the frame's 'NanoUI.Internal.Input.DropEvent's into a 'DropTarget'
-- for one rectangle. 'dropZone' wraps a panel and does the same for its rect.
--
-- > (_, _, target) <- dropZone fillW (label "Drop files here")
-- > when (dropReceived target) (mapM_ openFile (dropFiles target))
module NanoUI.Internal.Widgets.Drop
  ( DropTarget (..)
  , useDrop
  , dropZone
  , Drag (..)
  , DragPhase (..)
  , DragHandle
  , newDrag
  , useDrag
  , insertionIndex
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.List (find)
import Data.Text (Text)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import NanoUI.Internal.Context
import NanoUI.Internal.Input
import NanoUI.Internal.Monad (NanoUI, askContext, askDefaultLayout, askInput, freshWidget, liftIO)
import NanoUI.Internal.Store (deleteSlot, fieldPoint, flagSlot, insertSlot, lookupSlot, setFlagSlot)
import NanoUI.Internal.Style (Layout)
import NanoUI.Internal.Types (Rect (..), V2 (..), rectContains, rectHit)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Widgets.Node (Response, containerResponse, respRect, respPressed)
import NanoUI.Internal.Widgets.Behavior (DragAxis (..), dragThresholdPx)

-- | A thresholded pointer gesture. Terminal phases last one frame.
data DragPhase = DragStarted | Dragging | DragReleased | DragCancelled
  deriving (Eq, Show)

-- | Application-owned payload and window coordinates. Map the payload to
-- adapt one gesture to another target without restarting its lifecycle.
data Drag a = Drag
  { dragPayload :: !a
  , dragAt :: !V2
  -- ^ Where the pointer is.
  , dragFrom :: !V2
  -- ^ Where the press was.
  , dragPhase :: !DragPhase
  }
  deriving (Eq, Show, Functor)

data DragState a = DragState !a !V2 !Bool
  deriving (Eq)

-- | Component-owned gesture state, allocated once before rendering. Use on
-- the UI thread in one session; each independent gesture needs its own handle.
newtype DragHandle a = DragHandle (IORef (Maybe (DragState a)))

newDrag :: IO (DragHandle a)
newDrag = DragHandle <$> newIORef Nothing

-- | Track a drag across uniquely keyed responses. Call with the same handle
-- every frame, including for an empty collection. Only owned presses start;
-- payload identity survives reordering. Below the threshold, returns Nothing.
-- Escape, lost hold or source removal cancels; only DragReleased may commit.
useDrag :: Eq a => DragHandle a -> [(a, Response)] -> NanoUI (Maybe (Drag a))
useDrag (DragHandle ref) sources = do
  ctx <- askContext
  inp <- askInput
  let pos@(V2 x y) = inputMousePos inp
  old <- liftIO (readIORef ref)
  let armed = if pressedIn MouseLeft inp
        then (\(a, _) -> DragState a pos False) <$> find (respPressed . snd) sources
        else old
      step (DragState a origin@(V2 sx sy) hot) =
        let cancelled = pressedIn KeyEscape inp || all ((/= a) . fst) sources
              || (not (heldIn MouseLeft inp) && not (releasedIn MouseLeft inp))
            dx = x - sx
            dy = y - sy
            moved = hot || dx * dx + dy * dy > dragThresholdPx * dragThresholdPx
            released = releasedIn MouseLeft inp
            phase | cancelled = DragCancelled
                  | released = DragReleased
                  | not hot = DragStarted
                  | otherwise = Dragging
            emitted = if hot || (moved && not cancelled) then Just (Drag a pos origin phase) else Nothing
            retained = if cancelled || released then Nothing else Just (DragState a origin moved)
         in (retained, emitted)
      (next, result) = maybe (Nothing, Nothing) step armed
  when (next /= old) $ liftIO $ do
    writeIORef ref next
    damageFull ctx
    markDirtyCovered ctx
  pure result

-- | Insertion slot in an ordered row or column, bounded by its visible
-- viewport. Pass remaining items (omit the source for a same-list move).
-- Empty targets accept slot zero. Rectangles may extend beyond the viewport
-- when scrolled; their centers still determine the original list index.
insertionIndex :: DragAxis -> Rect -> [Rect] -> V2 -> Maybe Int
insertionIndex axis bounds items pos@(V2 x y)
  | not (rectHit bounds pos) = Nothing
  | otherwise = Just (length (takeWhile before items))
  where
    before (Rect rx ry rw rh) = case axis of
      DragAxisX -> x > rx + rw / 2
      DragAxisY -> y > ry + rh / 2

-- | Per-frame drop state for a single rectangular drop target.
data DropTarget = DropTarget
  { dropHovered :: !Bool
    -- ^ A drag is currently positioned over the target rect.
  , dropReceived :: !Bool
    -- ^ One or more payloads landed on the target this frame.
  , dropFiles :: ![Text]
    -- ^ File paths dropped on the target this frame.
  , dropTexts :: ![Text]
    -- ^ Text snippets dropped on the target this frame.
  , dropPosition :: !(Maybe V2)
    -- ^ Last known drop position in window coordinates, if any.
  }
  deriving (Eq, Show)

-- | The drop state for a rectangle from this frame's drop events. Hover
-- persists while the OS drag is still; payloads are reported on the frame
-- they arrive. A payload lands at the last 'DropPosition', not its own
-- coordinates, which SDL reports as (0,0) when it has none.
useDrop :: Rect -> NanoUI DropTarget
useDrop bounds = do
  (wid, ctx) <- freshWidget
  inp <- askInput
  let key = intKey wid
      activeK = slotKey SlotDrop key
      posK = slotKey SlotDropPos key
  store <- liftIO (getStore ctx)
  let active0 = flagSlot activeK store
      lastPos0 = uncurry V2 <$> lookupSlot fieldPoint posK store
      -- A drag is active from 'DropBegin' until 'DropComplete'. A payload
      -- uses the position current at its point in the sequence.
      step (active, pos, fs, ts) ev = case dropEventType ev of
        DropBegin -> (True, pos, fs, ts)
        DropPosition -> (active, dropEventPos ev <|> pos, fs, ts)
        DropComplete -> (False, Nothing, fs, ts)
        DropFile | posInside bounds pos -> (active, pos, dropEventData ev : fs, ts)
        DropText | posInside bounds pos -> (active, pos, fs, dropEventData ev : ts)
        _ -> (active, pos, fs, ts)
      (active1, lastPos1, filesRev, textsRev) =
        foldl' step (active0, lastPos0, [], []) (inputDrops inp)
      files = reverse filesRev
      texts = reverse textsRev
      hovered = active1 && posInside bounds lastPos1
  when (active1 /= active0 || lastPos1 /= lastPos0) $
    liftIO . modifyStore ctx $
      setFlagSlot activeK active1
        . maybe (deleteSlot fieldPoint posK) (\(V2 x y) -> insertSlot fieldPoint posK (x, y)) lastPos1
  pure
    DropTarget
      { dropHovered = hovered
      , dropReceived = not (null files) || not (null texts)
      , dropFiles = files
      , dropTexts = texts
      , dropPosition = lastPos1
      }

posInside :: Rect -> Maybe V2 -> Bool
posInside bounds = maybe False (rectContains bounds)

-- | A panel that is also a drop target. Returns the body's result, the
-- panel's 'Response', and the 'DropTarget' for its rect.
dropZone :: (Layout -> Layout) -> NanoUI a -> NanoUI (a, Response, DropTarget)
dropZone f child = do
  base <- askDefaultLayout
  (a, resp) <- containerResponse NodePanel (f base) child
  target <- useDrop (respRect resp)
  pure (a, resp, target)
