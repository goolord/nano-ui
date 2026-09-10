{-# LANGUAGE StrictData #-}

-- | Composable OS drag-and-drop support for widgets.
--
-- The low-level event stream is 'NanoUI.Input.DropEvent' (see 'inputDrops').
-- This module provides a small, reusable hook that turns those events into a
-- per-widget 'DropTarget', plus action combinators in the style of 'onClick':
--
-- @
-- (body, resp) <- panelResponse' layout $ do
--   ...
-- target <- useDrop (respRect resp)
-- onDropHover target (damageWidgetNow (respId resp) DamageSelf)
-- onDrop target (mapM_ handleFile (dropFiles target))
-- @
module NanoUI.Widgets.Drop
  ( -- * Widget-facing result
    DropTarget (..)
    -- * Hook
  , useDrop
    -- * Action combinators
  , onDrop
  , onDropHover
    -- * Convenience
  , dropZone
  ) where

import Control.Monad (when)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Vector qualified as V
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
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO)
import NanoUI.Store
  ( WidgetStore (..)
  , slotDrop
  , slotDropPos
  , slotKey
  )
import NanoUI.Style (Layout)
import NanoUI.Types (Rect, V2 (..), rectContains)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Widgets.Node (Responding (respRect), Response, containerResponse)

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
      dropList = V.toList (inputDrops inp)
  store <- uiIO (getStore ctx)
  let active0 = IM.findWithDefault 0 activeK (storeInt store) /= 0
      lastPos0 = fmap (\(x, y) -> V2 x y) (IM.lookup posK (storePoint store))
      DropScan active1 lastPos1 filesRev textsRev =
        foldl' (step bounds) (DropScan active0 lastPos0 [] []) dropList
      files = reverse filesRev
      texts = reverse textsRev
      hovered = active1 && posInside bounds lastPos1
      lastPosStore1 = fmap (\(V2 x y) -> (x, y)) lastPos1
  when (active1 /= active0 || lastPos1 /= lastPos0) $
    uiIO $ do
      let sInt =
            if active1
              then IM.insert activeK 1 (storeInt store)
              else IM.delete activeK (storeInt store)
          sPoint =
            maybe
              (IM.delete posK (storePoint store))
              (\p -> IM.insert posK p (storePoint store))
              lastPosStore1
      setStore ctx (store {storeInt = sInt, storePoint = sPoint})
  pure
    DropTarget
      { dropHovered = hovered
      , dropReceived = not (null files) || not (null texts)
      , dropFiles = files
      , dropTexts = texts
      , dropPosition = lastPos1
      }

-- | Position tracking plus payloads collected while scanning a frame's events.
-- A target only remembers the drag position reported by 'DropPosition' events;
-- payload coordinates are never folded back into the tracked position.
data DropScan = DropScan !Bool !(Maybe V2) ![Text] ![Text]

posInside :: Rect -> Maybe V2 -> Bool
posInside bounds = maybe False (rectContains bounds)

step :: Rect -> DropScan -> DropEvent -> DropScan
step bounds (DropScan active lastPos filesRev textsRev) ev =
  case dropEventType ev of
    DropBegin -> DropScan True lastPos filesRev textsRev
    DropComplete -> DropScan False Nothing filesRev textsRev
    DropPosition ->
      DropScan active (maybe lastPos Just (dropEventPos ev)) filesRev textsRev
    DropFile ->
      let hit = posInside bounds lastPos
       in DropScan active lastPos (if hit then dropEventData ev : filesRev else filesRev) textsRev
    DropText ->
      let hit = posInside bounds lastPos
       in DropScan active lastPos filesRev (if hit then dropEventData ev : textsRev else textsRev)

-- | Run an action when a payload was dropped on the target this frame.
onDrop :: DropTarget -> Eff es () -> Eff es ()
onDrop target act = when (dropReceived target) act

-- | Run an action while a drag is hovering over the target.
onDropHover :: DropTarget -> Eff es () -> Eff es ()
onDropHover target act = when (dropHovered target) act

-- | Wrap a panel as a drop zone, returning the child result, its click
-- 'Response', and the drop 'DropTarget'.
--
-- Combine with 'onDrop'/'onDropHover' (or inspect 'dropFiles'/'dropTexts')
-- to react to the drop without hand-rolling the rect plumbing.
dropZone :: Ui :> es => Layout -> Eff es a -> Eff es (a, Response, DropTarget)
dropZone layout child = do
  (a, resp) <- containerResponse NodePanel layout child
  target <- useDrop (respRect resp)
  pure (a, resp, target)
