{-# LANGUAGE StrictData #-}

-- | Operating-system drag and drop.
--
-- 'useDrop' turns the frame's 'NanoUI.Input.DropEvent's into a 'DropTarget'
-- for one rectangle. 'dropZone' wraps a panel and does the same for its rect.
--
-- @
-- (_, _, target) <- dropZone fillW (label "Drop files here")
-- when (dropReceived target) (mapM_ openFile (dropFiles target))
-- @
module NanoUI.Widgets.Drop
  ( DropTarget (..)
  , useDrop
  , dropZone
  ) where

import Control.Monad (when)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Foldable (toList)
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( getStore
  , intKey
  , setStore
  )
import NanoUI.Input
  ( DropEvent (..)
  , DropType (..)
  , inputDrops
  )
import NanoUI.Monad (Ui, askContext, askDefaultLayout, askInput, nextId, uiIO)
import NanoUI.Store
  ( WidgetStore (..)
  , slotDrop
  , slotDropPos
  , slotKey
  )
import NanoUI.Style (Layout)
import NanoUI.Types (Rect, V2 (..), rectContains)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Widgets.Node (Response, containerResponse, respRect)

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
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  let key = intKey wid
      activeK = slotKey slotDrop key
      posK = slotKey slotDropPos key
  store <- uiIO (getStore ctx)
  let active0 = IM.findWithDefault 0 activeK (storeInt store) /= 0
      lastPos0 = fmap (\(x, y) -> V2 x y) (IM.lookup posK (storePoint store))
      events = toList (inputDrops inp)
      -- A drag is active from 'DropBegin' until 'DropComplete'.
      active1 =
        foldl'
          ( \active ev -> case dropEventType ev of
              DropBegin -> True
              DropComplete -> False
              _ -> active
          )
          active0
          events
      -- Tracked position after each event: only 'DropPosition' moves it and
      -- 'DropComplete' clears it. Payload events leave it unchanged, so a
      -- payload is attributed to the position at that point in the sequence,
      -- not to the frame's final position.
      positions =
        drop 1 $ scanl
          ( \pos ev -> case dropEventType ev of
              DropPosition -> maybe pos Just (dropEventPos ev)
              DropComplete -> Nothing
              _ -> pos
          )
          lastPos0
          events
      lastPos1 = last (lastPos0 : positions)
      payloads ty =
        [dropEventData ev | (ev, pos) <- zip events positions, dropEventType ev == ty, posInside bounds pos]
      files = payloads DropFile
      texts = payloads DropText
      hovered = active1 && posInside bounds lastPos1
  when (active1 /= active0 || lastPos1 /= lastPos0) $
    uiIO $
      getStore ctx >>= \st -> setStore ctx $
        st
          { storeInt =
              if active1
                then IM.insert activeK 1 (storeInt st)
                else IM.delete activeK (storeInt st)
          , storePoint =
              maybe
                (IM.delete posK (storePoint st))
                (\(V2 x y) -> IM.insert posK (x, y) (storePoint st))
                lastPos1
          }
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
