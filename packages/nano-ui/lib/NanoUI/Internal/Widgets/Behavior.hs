-- | Interaction hooks shared by widgets: 1D drags, drag reordering, arrow-key
-- navigation, click-outside and Escape dismissal, and the keyboard focus
-- check. Their state lives in the widget store.
module NanoUI.Internal.Widgets.Behavior
  ( DragAxis (..)
  , useDrag1D
  , holdActiveWhile
  , Reorder (..)
  , useReorder
  , useKeyNav
  , keyboardFocused
  , useInputMethod
  , keyActivated
  , focusedKeyPressed
  , focusedKeyPressedOnce
  , KeyNav (..)
  , navStep
  , useDismissable
  , dragThresholdPx
  )
where

import Control.Monad (when, (>=>))
import Data.IORef (readIORef, writeIORef)
import Data.IntSet qualified as IS
import Data.List (find, minimumBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import NanoUI.Internal.Context
import NanoUI.Internal.Frame.Hit (findNodeByWidgetId)
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Input
import NanoUI.Internal.Layout.Arena (getParent, getWidgetId)
import NanoUI.Internal.Monad (NanoUI, (<&&>), askContext, askFrameInput, askInput, focusedWidget, freshWidget, liftIO, withContext)
import NanoUI.Internal.Store (deleteSlot, fieldInt, fieldPoint, findSlot, flagSlot, insertSlot, quietFlag, setFlagSlot, setQuietFlag)
import NanoUI.Internal.Types (Rect (..), V2 (..), clamp01, rectHit, v2X, v2Y)

-- | Pointer slop in pixels before a held press counts as a drag.
dragThresholdPx :: Float
dragThresholdPx = 8

data DragAxis = DragAxisX | DragAxisY
  deriving (Eq, Show)

-- | Clamped 1D drag on a track of widget @owner@. Maps pointer position on
-- @track@ into [lo, hi]. The drag starts with a press on the track and lasts
-- until the button comes up. A button held from elsewhere and moved onto the
-- track drags nothing, nor does a press where another widget covers the
-- owner ('trackCovered'). Returns the value, whether the drag is held, and
-- whether it was held before this frame.
useDrag1D ::
  DragAxis ->
  WidgetId ->
  Float ->
  Float ->
  Float ->
  Rect ->
  NanoUI (Float, Bool, Bool)
useDrag1D axis owner lo hi current track = do
  (wid, ctx) <- freshWidget
  inp <- askInput
  let dragK = slotKey SlotDrag (intKey wid)
      (origin, trackLen, mouse) = case axis of
        DragAxisX -> (rectX track, rectW track, v2X (inputMousePos inp))
        DragAxisY -> (rectY track, rectH track, v2Y (inputMousePos inp))
  active0 <- quietFlag dragK <$> liftIO (getStore ctx)
  started <- pure (pressedIn MouseLeft inp && rectHit track (inputMousePos inp)) <&&> (not <$> liftIO (trackCovered ctx owner (inputMousePos inp)))
  let active = heldIn MouseLeft inp && (active0 || started)
      frac = if trackLen <= 0 then 0 else clamp01 ((mouse - origin) / trackLen)
  when (active /= active0) $ liftIO (modifyStore ctx (setQuietFlag dragK active))
  pure (if active then lo + frac * (hi - lo) else current, active, active0)

-- | Whether a press at @mouse@ on a track of widget @owner@ is covered
-- ('pointerCovered'). A track wider than its owner (a colour picker's bar)
-- takes presses beside the owner too, where the pointer reaches what holds
-- the owner rather than the owner itself: there only a node that is none of
-- the owner's ancestors covers it.
trackCovered :: Context -> WidgetId -> V2 -> IO Bool
trackCovered ctx owner mouse = do
  covered <- pointerCovered ctx owner
  inOwner <- maybe False (`rectHit` mouse) <$> getPrevRect ctx owner
  if not covered || inOwner
    then pure covered
    else do
      reach <- readIORef (ctxPointerReach ctx)
      ancestors <- maybe (pure IS.empty) (getParent na >=> idsUp IS.empty) =<< findNodeByWidgetId ctx owner
      pure (maybe False (not . (`IS.isSubsetOf` ancestors)) reach)
  where
    na = ctxNodeArena ctx
    idsUp !acc i
      | i < 0 = pure acc
      | otherwise = do
          wid <- getWidgetId na i
          getParent na i >>= idsUp (if hashWidgetId wid == 0 then acc else IS.insert (intKey wid) acc)

-- | Hold the active id for @wid@ while its drag lasts and let it go after, so
-- the widget paints and takes the cursor as pressed wherever the pointer goes.
holdActiveWhile :: WidgetId -> Bool -> NanoUI ()
holdActiveWhile wid dragging = withContext $ \ctx -> do
  active <- readIORef (ctxActiveId ctx)
  when (dragging /= (active == wid)) $
    writeIORef (ctxActiveId ctx) (if dragging then wid else WidgetId 0)

-- | Where a drag-and-drop reorder stands after this frame ('useReorder').
data Reorder = Reorder
  { reorderOrder :: ![Int]
    -- ^ The order after this frame: the one passed in, with the dragged
    -- item moved on the frame it is dropped.
  , reorderPreview :: ![Int]
    -- ^ The order a drop would leave now, the one passed in while nothing
    -- is being dragged. Draw from it to show the items making room.
  , reorderDragging :: !(Maybe Int)
    -- ^ The item held, from the press on it until the button comes up.
  , reorderMoved :: !Bool
    -- ^ The pointer has gone past the drag threshold since the press, so
    -- the press is a drag and not a click. Still set on the release frame,
    -- so a click on the item there can be ignored.
  }
  deriving (Eq, Show)

-- | Drag-and-drop reorder of a list of item ids. Pass the order and each
-- item's rect as drawn last frame ('respRect'), in the order drawn: the
-- order passed, or 'reorderPreview' for a live preview. A press on an item
-- starts a drag; once the pointer passes the drag threshold the item takes
-- the place of the item nearest the pointer, so the list can wrap over
-- several rows. Store 'reorderOrder'; releasing outside every item drops
-- at the nearest one too.
--
-- Allocate @orderCell <- newState [0 .. 4]@ during component setup.
--
-- > (order, setOrder) <- useState orderCell
-- > rects <- liftIO (readIORef lastRects)
-- > r <- useReorder order rects
-- > drawn <- forM (reorderPreview r) $ \i -> (i,) . respRect <$> chip i
-- > liftIO (writeIORef lastRects drawn)
-- > setOrder (reorderOrder r)
useReorder :: [Int] -> [(Int, Rect)] -> NanoUI Reorder
useReorder order items = do
  (wid, ctx) <- freshWidget
  inp <- askInput
  let key = intKey wid
      dragK = slotKey SlotDrag key
      startK = slotKey SlotDragW key
      movedK = slotKey SlotDrop key
      mouse@(V2 mx my) = inputMousePos inp
      press = pressedIn MouseLeft inp
      hit = fst <$> find (\(_, r) -> rectHit r mouse) items
  store <- liftIO (getStore ctx)
  let dragging = if press then fromMaybe (-1) hit else findSlot fieldInt (-1) dragK store
      (sx, sy) = if press then (mx, my) else findSlot fieldPoint (mx, my) startK store
      moved =
        dragging >= 0
          && not press
          && (flagSlot movedK store || (mx - sx) * (mx - sx) + (my - sy) * (my - sy) > dragThresholdPx * dragThresholdPx)
      -- The slot nearest the pointer, by its distance from each rect.
      distance (Rect x y w h) =
        let dx = max 0 (max (x - mx) (mx - x - w))
            dy = max 0 (max (y - my) (my - y - h))
         in dx * dx + dy * dy
      target = if null items then Nothing else Just (fst (minimumBy (comparing (distance . snd . snd)) (zip [0 :: Int ..] items)))
      preview = case target of
        Just t | moved, dragging `elem` order ->
          let (before, after) = splitAt t (filter (/= dragging) order)
           in before ++ dragging : after
        _ -> order
      released = releasedIn MouseLeft inp
      ended = released || not (heldIn MouseLeft inp)
      next = if ended then -1 else dragging
  when (next /= findSlot fieldInt (-1) dragK store || (press && dragging >= 0) || moved /= flagSlot movedK store) $
    liftIO . modifyStore ctx $
      if next < 0
        then deleteSlot fieldInt dragK . deleteSlot fieldPoint startK . setFlagSlot movedK False
        else insertSlot fieldInt dragK next . insertSlot fieldPoint startK (sx, sy) . setFlagSlot movedK moved
  pure
    Reorder
      { reorderOrder = if released && moved then preview else order
      , reorderPreview = if ended then order else preview
      , reorderDragging = if dragging >= 0 && not ended then Just dragging else Nothing
      , reorderMoved = moved
      }

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
keyboardFocused :: WidgetId -> NanoUI Bool
keyboardFocused wid
  | hashWidgetId wid == 0 = pure False
  | otherwise = do
      ctx <- askContext
      focus <- focusedWidget
      pure (focus == wid)
        <&&> liftIO (not <$> isDisabled ctx wid)
        <&&> liftIO (not <$> pointerBlockedByModal ctx)

-- | Request input method (IME) text for widget @wid@, like iced's
-- @request_input_method@. Call it every frame the widget accepts text,
-- passing its caret in window coordinates and the input purpose; text fields
-- do this themselves.
--
-- While the IME is composing, this returns the 'Composition' for the widget
-- to draw at its caret, and the frame drops the IME's keys. Committed text
-- arrives as 'inputChars'. The backend places the candidate window at the
-- caret and enables text input only while some widget requests it, so a
-- custom widget reading 'inputChars' (a terminal, say) must call this.
-- Returns 'Nothing' and requests nothing when the widget is unfocused,
-- disabled or behind a modal.
--
-- > wid <- nextId
-- > Rect x y _ _ <- fromMaybe (Rect 0 0 0 0) <$> lastRect wid
-- > preedit <- useInputMethod wid InputNormal (Rect (x + caretX) y 1 lineH)
-- > customWidgetWithId wid spec {widgetFocusable = True, widgetKeys = KeysAll}
useInputMethod :: WidgetId -> InputPurpose -> Rect -> NanoUI (Maybe Composition)
useInputMethod wid purpose caret = do
  focused <- keyboardFocused wid
  if not focused
    then pure Nothing
    else withContext $ \ctx -> do
      requestInputMethod ctx wid (Just caret) purpose
      fieldComposition ctx wid

-- | Arrow / Enter / Space while @wid@ is focused and eligible for input,
-- bare or with Shift only ('shiftAtMost'); other modifiers leave them to
-- shortcuts. Arrows repeat with key auto-repeat; Enter and Space count only
-- on the initial press.
useKeyNav :: WidgetId -> NanoUI KeyNav
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

-- | Whether @key@ was pressed this frame, auto-repeats included, while
-- @wid@ holds the keyboard: focused, enabled and not behind a modal. For a
-- custom widget reading the keys it claims ('NanoUI.Widgets.Custom.widgetKeys'),
-- which 'NanoUI.keyPressed' leaves out:
--
-- > (resp, ()) <- customWidget spec {widgetFocusable = True}
-- > whenM (focusedKeyPressed (respId resp) KeyDelete) clearCell
focusedKeyPressed :: WidgetId -> Key -> NanoUI Bool
focusedKeyPressed wid k = keyboardFocused wid <&&> (pressedIn k <$> askInput)

-- | 'focusedKeyPressed' without auto-repeats, so holding the key acts once.
focusedKeyPressedOnce :: WidgetId -> Key -> NanoUI Bool
focusedKeyPressedOnce wid k = keyboardFocused wid <&&> (pressedOnceIn k <$> askInput)

-- | True when Enter or Space was pressed while @wid@ holds focus. Buttons,
-- checkboxes, and toggle switches treat this as a click.
{-# INLINE keyActivated #-}
keyActivated :: WidgetId -> NanoUI Bool
keyActivated wid = do
  nav <- useKeyNav wid
  pure (knEnter nav || knSpace nav)

-- | Escape and click-outside-rect dismiss. Consumes Escape when it fires. A
-- press anywhere else dismisses, whoever it belongs to, so this watches the
-- frame's input rather than the pointer routed here. A press on an open
-- dropdown or text-edit menu, which may lie outside the panel it belongs to,
-- does not; nor does an Escape that closes one, or that something declared
-- earlier (a popup inside this one) already took.
useDismissable :: Rect -> NanoUI Bool
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
