-- | Interaction hooks shared by widgets: 1D drags, drag reordering, arrow-key
-- navigation, click-outside and Escape dismissal, and the keyboard focus
-- check. Their state lives in the widget store.
module NanoUI.Internal.Widgets.Behavior
  ( DragAxis (..)
  , keyedDragHeld
  , useDrag1D
  , holdActiveWhile
  , useReorder
  , useKeyNav
  , keyboardFocused
  , keyActivated
  , KeyNav (..)
  , navStep
  , useDismissable
  , dragThresholdPx
  )
where

import Control.Monad (when)
import Data.Hashable (Hashable, hash)
import Data.IORef (readIORef, writeIORef)
import Data.List (find)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
  ( Context (..)
  , getStore
  , intKey
  , isDisabled
  , markEscapeConsumed
  , pointerBlockedByModal
  , Slot (..)
  , slotKey
  , modifyStore
  )
import NanoUI.Internal.Id (WidgetId (..), enterKeyed, hashWidgetId, idContextWidgetId)
import NanoUI.Internal.Input
  ( Input (..)
  , Key (..)
  , inputChars
  , inputKeys
  , inputKeysElem
  , inputKeysNull
  , inputMouseDown
  , inputMousePos
  , inputMousePressed
  , inputMouseReleased
  , inputMouseRightPressed
  )
import NanoUI.Internal.Monad (Ui, askContext, askFrameInput, askInput, focusedWidget, nextId, uiIO, withContext)
import NanoUI.Internal.Store (fieldFloat, fieldInt, findSlot, insertSlot, quietFlag, setQuietFlag)
import NanoUI.Internal.Types (Rect (..), clamp01, rectHit, v2X, v2Y)
import qualified Data.Text as T

-- | Pointer slop in pixels before a held press counts as a drag.
dragThresholdPx :: Float
dragThresholdPx = 8

data DragAxis = DragAxisX | DragAxisY
  deriving (Eq, Show)

-- | True when a prior keyed useDrag1D on this path is still held.
-- Peeks the keyed first-id without enterKeyed bumping parent siblingId.
keyedDragHeld :: (Hashable k, Ui :> es) => k -> Eff es Bool
keyedDragHeld k = do
  ctx <- askContext
  uiIO $ do
    old <- readIORef (ctxIdContext ctx)
    let wid = idContextWidgetId (snd (enterKeyed (fromIntegral (hash k)) old))
        dragK = slotKey SlotDrag (intKey wid)
    quietFlag dragK <$> getStore ctx

-- | Clamped 1D drag. Maps pointer position on @track@ into [lo, hi]. The drag
-- starts with a press on the track and lasts until the button comes up; a
-- button held from elsewhere and moved onto the track drags nothing.
useDrag1D ::
  (Ui :> es) =>
  DragAxis ->
  Float ->
  Float ->
  Float ->
  Rect ->
  Eff es (Float, Bool)
useDrag1D axis lo hi current track = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  let key = intKey wid
      dragK = slotKey SlotDrag key
      trackLen = case axis of
        DragAxisX -> rectW track
        DragAxisY -> rectH track
      origin = case axis of
        DragAxisX -> rectX track
        DragAxisY -> rectY track
      mouse = case axis of
        DragAxisX -> v2X (inputMousePos inp)
        DragAxisY -> v2Y (inputMousePos inp)
  store <- uiIO (getStore ctx)
  let active0 = quietFlag dragK store
      started = inputMousePressed inp && rectHit track (inputMousePos inp)
      active = inputMouseDown inp && (active0 || started)
      frac =
        if trackLen <= 0
          then 0
          else clamp01 ((mouse - origin) / trackLen)
      next =
        if active
          then lo + frac * (hi - lo)
          else current
  when (active /= active0) $ uiIO (modifyStore ctx (setQuietFlag dragK active))
  pure (next, active)

-- | Hold the active id for @wid@ while its drag lasts and let it go after, so
-- the widget paints and takes the cursor as pressed wherever the pointer goes.
holdActiveWhile :: (Ui :> es) => WidgetId -> Bool -> Eff es ()
holdActiveWhile wid dragging = do
  ctx <- askContext
  uiIO $ do
    active <- readIORef (ctxActiveId ctx)
    when (dragging /= (active == wid)) $
      writeIORef (ctxActiveId ctx) (if dragging then wid else WidgetId 0)

-- | Drag-and-drop reorder of a visible index list.
useReorder ::
  (Ui :> es) =>
  [Int] ->
  [(Int, Rect)] ->
  Eff es ([Int], Maybe Int)
useReorder order items = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  let key = intKey wid
      dragK = slotKey SlotDrag key
      dragWK = slotKey SlotDragW key
      mouse = inputMousePos inp
      down = inputMouseDown inp
      press = inputMousePressed inp
      release = inputMouseReleased inp
      hit =
        find
          (\(_, r) -> rectHit r mouse)
          items
  store <- uiIO (getStore ctx)
  let from0 = findSlot fieldInt (-1) dragK store
      startX = findSlot fieldFloat 0 dragWK store
      dragging = if press then maybe (-1) fst hit else from0
      nextDrag =
        if release || not down
          then -1
          else dragging
      -- Resolve the drop using the held source before clearing it on release.
      moved =
        not press && dragging >= 0 && abs (v2X mouse - startX) > dragThresholdPx
      dropTo = if moved then fmap fst hit else Nothing
      nextOrder =
        case dropTo of
          Just toCol | release -> moveItem order dragging toCol
          _ -> order
  when (nextDrag /= from0 || (press && nextDrag >= 0)) $
    uiIO . modifyStore ctx $
      insertSlot fieldInt dragK nextDrag
        . insertSlot fieldFloat dragWK (if press then v2X mouse else startX)
  pure (nextOrder, if nextDrag >= 0 then Just nextDrag else Nothing)

moveItem :: [Int] -> Int -> Int -> [Int]
moveItem xs from to
  | from == to = xs
  | otherwise =
      let without = filter (/= from) xs
          (pre, post) = break (== to) without
       in pre ++ from : post

data KeyNav = KeyNav
  { knUp :: !Bool
  , knDown :: !Bool
  , knLeft :: !Bool
  , knRight :: !Bool
  , knEnter :: !Bool
  , knSpace :: !Bool
  }
  deriving (Eq, Show)

-- | Focus alone does not grant keyboard input. A retained focus ID must still
-- respect disabled state and the modal currently being declared. Unfocused
-- controls avoid the store and modal checks entirely.
{-# INLINE keyboardFocused #-}
keyboardFocused :: Ui :> es => WidgetId -> Eff es Bool
keyboardFocused wid
  | hashWidgetId wid == 0 = pure False
  | otherwise = do
      ctx <- askContext
      focus <- focusedWidget
      if focus /= wid
        then pure False
        else uiIO $ do
          disabled <- isDisabled ctx wid
          if disabled then pure False else not <$> pointerBlockedByModal ctx

-- | Arrow / Enter / Space while @wid@ is focused and eligible for input.
useKeyNav :: (Ui :> es) => WidgetId -> Eff es KeyNav
useKeyNav wid = do
  inp <- askInput
  let keys = inputKeys inp
      none = KeyNav False False False False False False
  if hashWidgetId wid == 0 || (inputKeysNull keys && T.null (inputChars inp))
    then pure none
    else do
      eligible <- keyboardFocused wid
      if not eligible
        then pure none
        else pure KeyNav
          { knUp = inputKeysElem KeyUp keys
          , knDown = inputKeysElem KeyDown keys
          , knLeft = inputKeysElem KeyLeft keys
          , knRight = inputKeysElem KeyRight keys
          , knEnter = inputKeysElem KeyEnter keys
          , knSpace = T.any (== ' ') (inputChars inp)
          }

-- | The step the arrow keys ask for along a control that grows rightwards and
-- upwards: @1@ for Right or Up, @-1@ for Left or Down.
{-# INLINE navStep #-}
navStep :: KeyNav -> Int
navStep nav = fromEnum (knRight nav || knUp nav) - fromEnum (knLeft nav || knDown nav)

-- | True when Enter or Space was pressed while @wid@ holds focus. Buttons,
-- checkboxes, and toggle switches treat this as a click.
{-# INLINE keyActivated #-}
keyActivated :: (Ui :> es) => WidgetId -> Eff es Bool
keyActivated wid = do
  nav <- useKeyNav wid
  pure (knEnter nav || knSpace nav)

-- | Escape and click-outside-rect dismiss. Consumes Escape when it fires. A
-- press anywhere else dismisses, whoever it belongs to, so this watches the
-- frame's input rather than the pointer routed here.
useDismissable :: (Ui :> es) => Rect -> Eff es Bool
useDismissable panel = do
  inp <- askFrameInput
  let mouse = inputMousePos inp
      inside = rectHit panel mouse
      esc = inputKeysElem KeyEscape (inputKeys inp)
      backdrop = (inputMousePressed inp || inputMouseRightPressed inp) && not inside
      dismissed = esc || backdrop
  when esc $ withContext markEscapeConsumed
  pure dismissed
