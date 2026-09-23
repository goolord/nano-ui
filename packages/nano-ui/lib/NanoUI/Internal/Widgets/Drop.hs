{-# LANGUAGE StrictData #-}

-- | Operating-system drag and drop.
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
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
import NanoUI.Internal.Input
import NanoUI.Internal.Monad (Ui, askDefaultLayout, askInput, freshWidget, uiIO)
import NanoUI.Internal.Store (deleteSlot, fieldPoint, flagSlot, insertSlot, lookupSlot, setFlagSlot)
import NanoUI.Internal.Style (Layout)
import NanoUI.Internal.Types (Rect, V2 (..), rectContains)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Widgets.Node (Response, containerResponse, respRect)

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

-- | Compute the drop state for a rectangle from the current frame's drop events.
--
-- Active/hover state persists across frames in the widget store, so a target
-- keeps highlighting while the OS drag is stationary. Payload events
-- ('DropFile'/'DropText') are one-shot: they are reported exactly on the frame
-- they arrive.
--
-- Attribution uses the tracked drag position (the coordinates of the most
-- recent 'DropPosition') rather than a payload's own coordinates. SDL
-- synthesizes file/text events at the last drag position and reports (0,0)
-- when it never observed one, so the position stream is the only reliable
-- signal for "which target is this drop over".
useDrop :: Ui :> es => Rect -> Eff es DropTarget
useDrop bounds = do
  (wid, ctx) <- freshWidget
  inp <- askInput
  let key = intKey wid
      activeK = slotKey SlotDrop key
      posK = slotKey SlotDropPos key
  store <- uiIO (getStore ctx)
  let active0 = flagSlot activeK store
      lastPos0 = uncurry V2 <$> lookupSlot fieldPoint posK store
      -- A drag is active from 'DropBegin' until 'DropComplete'. Only
      -- 'DropPosition' moves the tracked position and 'DropComplete' clears
      -- it. Payload events leave it unchanged, so a payload is attributed to
      -- the position at that point in the sequence, not to the frame's final
      -- position.
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
    uiIO . modifyStore ctx $
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
dropZone :: Ui :> es => (Layout -> Layout) -> Eff es a -> Eff es (a, Response, DropTarget)
dropZone f child = do
  base <- askDefaultLayout
  (a, resp) <- containerResponse NodePanel (f base) child
  target <- useDrop (respRect resp)
  pure (a, resp, target)
