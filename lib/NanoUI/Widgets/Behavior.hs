-- | Concrete interaction hooks. No generic view trait; store-backed state only.
module NanoUI.Widgets.Behavior
  ( DragAxis (..)
  , useDrag1D
  , useReorder
  , useSelection
  , useKeyNav
  , KeyNav (..)
  , useDismissable
  , ensureInt
  , ensureIntSet
  , putInt
  , putIntSet
  )
where

import Control.Monad (when)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Effectful (Eff, type (:>))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import NanoUI.Context
  ( bumpMirror
  , getFocusId
  , getStore
  , intKey
  , markEscapeConsumed
  , setStore
  , slotDrag
  , slotDragW
  , slotKey
  )
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input
  ( Input (..)
  , Key (..)
  , inputChars
  , inputKeys
  , inputKeysElem
  , inputMouseDown
  , inputMousePos
  , inputMousePressed
  , inputMouseReleased
  , inputMouseRightPressed
  )
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO)
import NanoUI.Store (WidgetStore (..))
import NanoUI.Types (Rect (..), rectContains, v2X, v2Y)
import qualified Data.Text as T

data DragAxis = DragAxisX | DragAxisY
  deriving (Eq, Show)

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
      hit = rectW track > 0 && rectH track > 0 && rectContains track (inputMousePos inp)
  store <- uiIO (getStore ctx)
  let active0 = IM.findWithDefault 0 dragK (storeInt store) /= 0
      active = down && (active0 || hit)
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
          (\(_, r) -> rectW r > 0 && rectH r > 0 && rectContains r mouse)
          items
  store <- uiIO (getStore ctx)
  let from0 = IM.findWithDefault (-1) dragK (storeInt store)
      startX = IM.findWithDefault 0 (slotKey slotDragW key) (storeFloat store)
      fromPress = if press then maybe (-1) fst hit else from0
      dragging =
        if release || not down
          then -1
          else fromPress
      moved = dragging >= 0 && abs (v2X mouse - startX) > 8
      dropTo = if moved then fmap fst hit else Nothing
      nextOrder =
        case (release, moved, dropTo) of
          (True, True, Just toCol) | dragging >= 0 -> moveItem order dragging toCol
          _ -> order
  when (dragging /= from0 || (press && dragging >= 0)) $
    uiIO $ do
      st <- getStore ctx
      setStore
        ctx
        ( st
            { storeInt = IM.insert dragK dragging (storeInt st)
            , storeFloat =
                IM.insert
                  (slotKey slotDragW key)
                  (if press then v2X mouse else startX)
                  (storeFloat st)
            }
        )
  pure (nextOrder, if dragging >= 0 then Just dragging else Nothing)

moveItem :: [Int] -> Int -> Int -> [Int]
moveItem xs from to
  | from == to = xs
  | otherwise =
      let without = filter (/= from) xs
          (pre, post) = splitAt (fromMaybe (length without) (indexOf to without)) without
       in pre ++ from : post

indexOf :: Eq a => a -> [a] -> Maybe Int
indexOf x = fmap fst . find ((== x) . snd) . zip [0 ..]

-- | Discrete Int selection. Frame re-runs UI when the value changes.
useSelection :: (Ui :> es) => Int -> Eff es (Int, Int -> Eff es ())
useSelection initial = do
  wid <- nextId
  ctx <- askContext
  let key = intKey wid
      get = uiIO $ do
        st <- getStore ctx
        pure (IM.findWithDefault initial key (storeInt st))
      set v = uiIO $ do
        st <- getStore ctx
        let prev = IM.findWithDefault initial key (storeInt st)
        when (prev /= v) $
          setStore ctx (bumpMirror (st {storeInt = IM.insert key v (storeInt st)}))
  cur <- get
  pure (cur, \v -> when (v /= cur) (set v))

data KeyNav = KeyNav
  { knUp :: !Bool
  , knDown :: !Bool
  , knLeft :: !Bool
  , knRight :: !Bool
  , knEnter :: !Bool
  , knSpace :: !Bool
  }
  deriving (Eq, Show)

-- | Arrow / Enter / Space while 'wid' is focused.
useKeyNav :: (Ui :> es) => WidgetId -> Eff es KeyNav
useKeyNav wid = do
  ctx <- askContext
  inp <- askInput
  focus <- uiIO (getFocusId ctx)
  let on = hashWidgetId wid /= 0 && focus == wid
      keys = inputKeys inp
      none = KeyNav False False False False False False
  if not on
    then pure none
    else
      pure
        KeyNav
          { knUp = inputKeysElem KeyUp keys
          , knDown = inputKeysElem KeyDown keys
          , knLeft = inputKeysElem KeyLeft keys
          , knRight = inputKeysElem KeyRight keys
          , knEnter = inputKeysElem KeyEnter keys
          , knSpace = T.any (== ' ') (inputChars inp)
          }

-- | Escape and click-outside-rect dismiss. Consumes Escape when it fires.
useDismissable :: (Ui :> es) => Rect -> Eff es Bool
useDismissable panel = do
  ctx <- askContext
  inp <- askInput
  let mouse = inputMousePos inp
      inside =
        rectW panel > 0 && rectH panel > 0 && rectContains panel mouse
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
