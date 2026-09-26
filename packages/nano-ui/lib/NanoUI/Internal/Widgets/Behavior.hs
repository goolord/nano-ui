-- | Interaction hooks shared by widgets: 1D drags, drag reordering, arrow-key
-- navigation, click-outside and Escape dismissal, and the keyboard focus
-- check. Their state lives in the widget store.
module NanoUI.Internal.Widgets.Behavior
  ( DragAxis (..)
  , useDrag1D
  , holdActiveWhile
  , useReorder
  , useKeyNav
  , keyboardFocused
  , useInputMethod
  , keyActivated
  , KeyNav (..)
  , navStep
  , useDismissable
  , dragThresholdPx
  )
where

import Control.Monad (when)
import Data.IORef (readIORef, writeIORef)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Input
import NanoUI.Internal.Monad (Ui, (<&&>), askContext, askFrameInput, askInput, focusedWidget, freshWidget, uiIO, withContext)
import NanoUI.Internal.Store (fieldFloat, fieldInt, findSlot, insertSlot, quietFlag, setQuietFlag)
import NanoUI.Internal.Types (Rect (..), clamp01, rectHit, v2X, v2Y)

-- | Pointer slop in pixels before a held press counts as a drag.
dragThresholdPx :: Float
dragThresholdPx = 8

data DragAxis = DragAxisX | DragAxisY
  deriving (Eq, Show)

-- | Clamped 1D drag on a track of widget @owner@. Maps pointer position on
-- @track@ into [lo, hi]. The drag starts with a press on the track and lasts
-- until the button comes up; a button held from elsewhere and moved onto the
-- track drags nothing, nor does a press where layers or a pinned node draw
-- another widget over the owner ('pointerCovered'). Returns the value,
-- whether the drag is held, and whether it was held before this frame.
useDrag1D ::
  (Ui :> es) =>
  DragAxis ->
  WidgetId ->
  Float ->
  Float ->
  Float ->
  Rect ->
  Eff es (Float, Bool, Bool)
useDrag1D axis owner lo hi current track = do
  (wid, ctx) <- freshWidget
  inp <- askInput
  let dragK = slotKey SlotDrag (intKey wid)
      (origin, trackLen, mouse) = case axis of
        DragAxisX -> (rectX track, rectW track, v2X (inputMousePos inp))
        DragAxisY -> (rectY track, rectH track, v2Y (inputMousePos inp))
  active0 <- quietFlag dragK <$> uiIO (getStore ctx)
  started <- pure (pressedIn MouseLeft inp && rectHit track (inputMousePos inp)) <&&> (not <$> uiIO (pointerCovered ctx owner))
  let active = heldIn MouseLeft inp && (active0 || started)
      frac = if trackLen <= 0 then 0 else clamp01 ((mouse - origin) / trackLen)
  when (active /= active0) $ uiIO (modifyStore ctx (setQuietFlag dragK active))
  pure (if active then lo + frac * (hi - lo) else current, active, active0)

-- | Hold the active id for @wid@ while its drag lasts and let it go after, so
-- the widget paints and takes the cursor as pressed wherever the pointer goes.
holdActiveWhile :: (Ui :> es) => WidgetId -> Bool -> Eff es ()
holdActiveWhile wid dragging = withContext $ \ctx -> do
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
  (wid, ctx) <- freshWidget
  inp <- askInput
  let key = intKey wid
      dragK = slotKey SlotDrag key
      dragWK = slotKey SlotDragW key
      mouse = inputMousePos inp
      press = pressedIn MouseLeft inp
      release = releasedIn MouseLeft inp
      hit = fst <$> find (\(_, r) -> rectHit r mouse) items
  store <- uiIO (getStore ctx)
  let from0 = findSlot fieldInt (-1) dragK store
      startX = findSlot fieldFloat 0 dragWK store
      dragging = if press then fromMaybe (-1) hit else from0
      nextDrag = if release || not (heldIn MouseLeft inp) then -1 else dragging
      -- Resolve the drop using the held source before clearing it on release.
      moved = not press && dragging >= 0 && abs (v2X mouse - startX) > dragThresholdPx
      nextOrder = case hit of
        Just toCol | release, moved -> moveItem order dragging toCol
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
      pure (focus == wid)
        <&&> uiIO (not <$> isDisabled ctx wid)
        <&&> uiIO (not <$> pointerBlockedByModal ctx)

-- | Take text from the input method (IME) for the widget with this id, as
-- iced's @request_input_method@ does: the widget that has the keyboard asks
-- every frame it takes text, with its caret in window coordinates and what
-- it takes, as the text fields ask for themselves. While the input method
-- composes for it, this answers the 'Composition', which the widget draws at
-- its caret, and the frame drops the keys, which are the input method's;
-- the text it commits arrives as typed text ('inputChars'). The backend
-- puts the input method's candidate window by the caret, and takes text
-- input at all only while a widget asks: a widget of its own that reads
-- 'inputChars', such as a terminal, asks here. A widget without the
-- keyboard, or disabled, or behind a modal, asks nothing and gets
-- 'Nothing'.
--
-- > wid <- nextId
-- > Rect x y _ _ <- fromMaybe (Rect 0 0 0 0) <$> lastRect wid
-- > preedit <- useInputMethod wid InputNormal (Rect (x + caretX) y 1 lineH)
-- > customWidgetWithId wid spec {widgetFocusable = True, widgetKeys = KeysAll}
useInputMethod :: Ui :> es => WidgetId -> InputPurpose -> Rect -> Eff es (Maybe Composition)
useInputMethod wid purpose caret = do
  focused <- keyboardFocused wid
  if not focused
    then pure Nothing
    else withContext $ \ctx -> do
      requestInputMethod ctx wid (Just caret) purpose
      fieldComposition ctx wid

-- | Arrow / Enter / Space while @wid@ is focused and eligible for input, each
-- alone or with Shift ('shiftAtMost'): with Ctrl, Alt or Super it is a
-- chord, for a shortcut. An arrow held down steps again on each
-- auto-repeat; Enter and Space, which activate, count only when they go
-- down.
useKeyNav :: (Ui :> es) => WidgetId -> Eff es KeyNav
useKeyNav wid = do
  inp <- askInput
  let none = KeyNav False False False False False False
  if hashWidgetId wid == 0 || inputKeysNull (inputKeys inp) || not (shiftAtMost (inputModifiers inp))
    then pure none
    else do
      eligible <- keyboardFocused wid
      if not eligible
        then pure none
        else pure KeyNav
          { knUp = pressedIn KeyUp inp
          , knDown = pressedIn KeyDown inp
          , knLeft = pressedIn KeyLeft inp
          , knRight = pressedIn KeyRight inp
          , knEnter = pressedOnceIn KeyEnter inp
          , knSpace = pressedOnceIn KeySpace inp
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
-- frame's input rather than the pointer routed here. A press on an open
-- dropdown or text-edit menu, which may lie outside the panel it belongs to,
-- does not; nor does an Escape that closes one, or that something declared
-- earlier (a popup inside this one) already took.
useDismissable :: (Ui :> es) => Rect -> Eff es Bool
useDismissable panel = do
  inp <- askFrameInput
  withContext $ \ctx -> do
    route <- getsInteraction ctx isPointerRoute
    menu <- getsInteraction ctx isTextInputMenu
    dropdown <- anySelectOpen <$> getStore ctx
    taken <- overlayConsumesQuit ctx inp
    let onMenu = case route of
          RouteLayer _ -> False
          _ -> True
        esc = pressedOnceIn KeyEscape inp && not taken && null menu && not dropdown
        pressed = anyButtonPressed inp && not onMenu
    when esc (markEscapeConsumed ctx)
    pure (esc || (pressed && not (rectHit panel (inputMousePos inp))))
