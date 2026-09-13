-- | Concrete interaction hooks. No generic view trait; store-backed state only.
module NanoUI.Widgets.Behavior
  ( DragAxis (..)
  , keyedDragHeld
  , useDrag1D
  , useReorder
  , useSelection
  , useKeyNav
  , keyboardFocused
  , keyActivated
  , KeyNav (..)
  , useDismissable
  , dragThresholdPx
  , ensureInt
  , ensureIntSet
  , putInt
  , putIntSet
  )
where

import Control.Monad (when)
import Data.Hashable (Hashable, hash)
import Data.IORef (readIORef)
import Data.List (find)
import Effectful (Eff, type (:>))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import NanoUI.Context
  ( Context (..)
  , getFocusId
  , getStore
  , intKey
  , isDisabled
  , markEscapeConsumed
  , menuPointerGestureActive
  , pointerBlockedByModal
  , setStore
  , slotDrag
  , slotDragW
  , slotKey
  )
import NanoUI.Hooks (useInt)
import NanoUI.Id (IdContext (..), WidgetId (..), enterKeyed, hashWidgetId, mix64)
import NanoUI.Input
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
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO)
import NanoUI.Store (WidgetStore (..))
import NanoUI.Types (Rect (..), rectHit, v2X, v2Y)
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
    let (_, child) = enterKeyed (fromIntegral (hash k)) old
        IdContext cid sid = child
        raw = mix64 cid sid
        wid = if raw == 0 then WidgetId 1 else WidgetId raw
        dragK = slotKey slotDrag (intKey wid)
    store <- getStore ctx
    pure (IM.findWithDefault 0 dragK (storeInt store) /= 0)

-- | Clamped 1D drag. Maps pointer position on 'track' into [lo, hi].
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
      dragK = slotKey slotDrag key
      trackLen = case axis of
        DragAxisX -> rectW track
        DragAxisY -> rectH track
      origin = case axis of
        DragAxisX -> rectX track
        DragAxisY -> rectY track
      mouse = case axis of
        DragAxisX -> v2X (inputMousePos inp)
        DragAxisY -> v2Y (inputMousePos inp)
      down = inputMouseDown inp
  store <- uiIO (getStore ctx)
  gesture <- uiIO (menuPointerGestureActive ctx)
  let active0 = IM.findWithDefault 0 dragK (storeInt store) /= 0
      hit = rectHit track (inputMousePos inp) && not gesture
      active = down && not gesture && (active0 || hit)
      frac =
        if trackLen <= 0
          then 0
          else max 0 (min 1 ((mouse - origin) / trackLen))
      next =
        if active
          then lo + frac * (hi - lo)
          else current
  when (active /= active0) $
    uiIO $ do
      st <- getStore ctx
      setStore
        ctx
        ( st
            { storeInt =
                if active
                  then IM.insert dragK 1 (storeInt st)
                  else IM.delete dragK (storeInt st)
            }
        )
  pure (next, active)

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
      dragK = slotKey slotDrag key
      mouse = inputMousePos inp
      down = inputMouseDown inp
      press = inputMousePressed inp
      release = inputMouseReleased inp
      hit =
        find
          (\(_, r) -> rectHit r mouse)
          items
  store <- uiIO (getStore ctx)
  let from0 = IM.findWithDefault (-1) dragK (storeInt store)
      startX = IM.findWithDefault 0 (slotKey slotDragW key) (storeFloat store)
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
    uiIO $ do
      st <- getStore ctx
      setStore
        ctx
        ( st
            { storeInt = IM.insert dragK nextDrag (storeInt st)
            , storeFloat =
                IM.insert
                  (slotKey slotDragW key)
                  (if press then v2X mouse else startX)
                  (storeFloat st)
            }
        )
  pure (nextOrder, if nextDrag >= 0 then Just nextDrag else Nothing)

moveItem :: [Int] -> Int -> Int -> [Int]
moveItem xs from to
  | from == to = xs
  | otherwise =
      let without = filter (/= from) xs
          (pre, post) = break (== to) without
       in pre ++ from : post

-- | Discrete Int selection. Frame re-runs UI when the value changes.
useSelection :: (Ui :> es) => Int -> Eff es (Int, Int -> Eff es ())
useSelection = useInt

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
      focus <- uiIO (getFocusId ctx)
      if focus /= wid
        then pure False
        else uiIO $ do
          disabled <- isDisabled ctx wid
          if disabled then pure False else not <$> pointerBlockedByModal ctx

-- | Arrow / Enter / Space while 'wid' is focused and eligible for input.
{-# INLINE useKeyNav #-}
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

-- | True when Enter or Space was pressed while @wid@ holds focus. Buttons,
-- checkboxes, and toggle switches treat this as a click.
{-# INLINE keyActivated #-}
keyActivated :: (Ui :> es) => WidgetId -> Eff es Bool
keyActivated wid = do
  nav <- useKeyNav wid
  pure (knEnter nav || knSpace nav)

-- | Escape and click-outside-rect dismiss. Consumes Escape when it fires.
useDismissable :: (Ui :> es) => Rect -> Eff es Bool
useDismissable panel = do
  ctx <- askContext
  inp <- askInput
  let mouse = inputMousePos inp
      inside = rectHit panel mouse
      esc = inputKeysElem KeyEscape (inputKeys inp)
      backdrop = (inputMousePressed inp || inputMouseRightPressed inp) && not inside
      dismissed = esc || backdrop
  when esc $ uiIO (markEscapeConsumed ctx)
  pure dismissed

-- | Read 'storeInt', inserting 'initial' on first use.
ensureInt :: (Ui :> es) => Int -> Int -> Eff es Int
ensureInt key initial = do
  ctx <- askContext
  st <- uiIO (getStore ctx)
  case IM.lookup key (storeInt st) of
    Just v -> pure v
    Nothing -> do
      uiIO $ setStore ctx (st {storeInt = IM.insert key initial (storeInt st)})
      pure initial

-- | Read 'storeIntSet', inserting 'initial' on first use.
ensureIntSet :: (Ui :> es) => Int -> IS.IntSet -> Eff es IS.IntSet
ensureIntSet key initial = do
  ctx <- askContext
  st <- uiIO (getStore ctx)
  case IM.lookup key (storeIntSet st) of
    Just v -> pure v
    Nothing -> do
      uiIO $ setStore ctx (st {storeIntSet = IM.insert key initial (storeIntSet st)})
      pure initial

putInt :: (Ui :> es) => Int -> Int -> Eff es ()
putInt key v = do
  ctx <- askContext
  st <- uiIO (getStore ctx)
  case IM.lookup key (storeInt st) of
    Just old | old == v -> pure ()
    _ -> uiIO $ setStore ctx (st {storeInt = IM.insert key v (storeInt st)})

putIntSet :: (Ui :> es) => Int -> IS.IntSet -> Eff es ()
putIntSet key v = do
  ctx <- askContext
  st <- uiIO (getStore ctx)
  case IM.lookup key (storeIntSet st) of
    Just old | old == v -> pure ()
    _ -> uiIO $ setStore ctx (st {storeIntSet = IM.insert key v (storeIntSet st)})
