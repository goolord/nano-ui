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

-- | The drop state for a rectangle from this frame's drop events. Hover
-- persists while the OS drag is still; payloads are reported on the frame
-- they arrive. A payload lands at the last 'DropPosition', not its own
-- coordinates, which SDL reports as (0,0) when it has none.
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
