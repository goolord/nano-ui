-- | Interaction hooks shared by widgets: 1D drags, drag reordering, arrow-key
-- navigation, click-outside and Escape dismissal, and the keyboard focus
-- check. Their state lives in the widget store.
module NanoUI.Internal.Widgets.Behavior
  ( DragAxis (..)
  , useDrag1D
  , leftPress
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
  , ListNav (..)
  , useListNav
  , useListNavWith
  , useDismissable
  , pastDragThreshold
  )
where

import Control.Monad (forM_, when, (>=>))
import Data.Bits (shiftL, (.|.))
import Data.IORef (readIORef, writeIORef)
import Data.IntSet qualified as IS
import Data.List (find, minimumBy)
import Data.Map.Strict qualified as M
import Data.Ord (comparing)
import Data.Maybe (isJust)
import GHC.Float (castFloatToWord32)
import NanoUI.Internal.Context
import NanoUI.Internal.Frame.Hit (findNodeByWidgetId, nodeInSubtree, nodeInteractionHit)
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Input
import NanoUI.Internal.Layout.Arena (getParent, getWidgetId, lookupNodeByKey)
import NanoUI.Internal.Monad (NanoUI, (<&&>), askContext, askFrameInput, askInput, focusedWidget, freshWidget, liftIO, withContext)
import NanoUI.Internal.Store (deleteSpec, findSpec, insertSpec, lookupSpec)
import NanoUI.Internal.Store.Slots (setFlagSpec, slotDragItem, slotDragPress, slotDragStartPos, slotDrop)
import NanoUI.Internal.Types (Rect (..), V2 (..), clamp01, rectContains, v2X, v2Y)
import NanoUI.Internal.Widgets.Node (Response, respPressed, respRect)

-- | Pointer slop in pixels before a held press counts as a drag.
dragThresholdPx :: Float
dragThresholdPx = 8

-- | Whether the pointer has moved past 'dragThresholdPx' from the press origin.
{-# INLINE pastDragThreshold #-}
pastDragThreshold :: V2 -> V2 -> Bool
pastDragThreshold (V2 sx sy) (V2 mx my) =
  let dx = mx - sx
      dy = my - sy
   in dx * dx + dy * dy > dragThresholdPx * dragThresholdPx

-- | The axis a drag or an insertion index measures along.
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
  let key = intKey wid
      (origin, trackLen, mouse) = case axis of
        DragAxisX -> (rectX track, rectW track, v2X (inputMousePos inp))
        DragAxisY -> (rectY track, rectH track, v2Y (inputMousePos inp))
  -- The drag keeps the point of the press that started it in a quiet slot
  -- (0 for none), and lasts only under that press: one released while this
  -- hook did not run does not come back with a later press.
  held <- findSpec slotDragPress 0 key <$> liftIO (getStore ctx)
  active0 <- if held == 0 then pure False else (== held) <$> liftIO (leftPress ctx)
  -- Hit-test against the clipped node so a track scrolled out of its
  -- viewport does not respond at its stale rect.
  let onTrack = findNodeByWidgetId ctx owner >>= maybe (pure (rectContains track (inputMousePos inp))) (\i -> nodeInteractionHit ctx i track (inputMousePos inp))
  started <- pure (pressedIn MouseLeft inp) <&&> liftIO onTrack <&&> (not <$> liftIO (trackCovered ctx owner (inputMousePos inp)))
  let active = heldIn MouseLeft inp && (active0 || started)
      frac = if trackLen <= 0 then 0 else clamp01 ((mouse - origin) / trackLen)
  when (if active then not active0 else held /= 0) . liftIO $
    if active
      then leftPress ctx >>= modifyStore ctx . insertSpec slotDragPress key
      else modifyStore ctx (deleteSpec slotDragPress key)
  pure (if active then lo + frac * (hi - lo) else current, active, active0)

-- | The left button's press point as one nonzero Int, exact in both
-- coordinates; 0 while it is up. A drag that keeps the one it started under
-- lasts only under that press.
leftPress :: Context -> IO Int
leftPress ctx = maybe 0 (\PressPoint {ppPos = V2 x y} -> (fromIntegral (castFloatToWord32 x) `shiftL` 32 .|. fromIntegral (castFloatToWord32 y)) + 1) . M.lookup MouseLeft <$> readIORef (ctxPressPos ctx)

-- | Whether a press at @mouse@ on a track of widget @owner@ is covered
-- ('pointerCovered'). A track wider than its owner (a colour picker's bar)
-- takes presses beside the owner too, where the pointer reaches what holds
-- the owner rather than the owner itself: there only a node that is none of
-- the owner's ancestors covers it.
trackCovered :: Context -> WidgetId -> V2 -> IO Bool
trackCovered ctx owner mouse = do
  covered <- pointerCovered ctx owner
  inOwner <- maybe False (`rectContains` mouse) <$> getPrevRect ctx owner
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
-- On the release frame the id stays until the frame lets it go after the
-- view, so a container around the widget does not take the release as its
-- click.
holdActiveWhile :: WidgetId -> Bool -> NanoUI ()
holdActiveWhile wid dragging = do
  inp <- askInput
  withContext $ \ctx -> do
    active <- readIORef (ctxActiveId ctx)
    when (dragging /= (active == wid) && (dragging || not (releasedIn MouseLeft inp))) $
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
-- item's response from this view pass, in the order drawn: the
-- order passed, or 'reorderPreview' for a live preview. A press on an item
-- starts a drag; once the pointer passes the drag threshold the item takes
-- the place of the item nearest the pointer, so the list can wrap over
-- several rows. Store 'reorderOrder'; releasing outside every item drops
-- at the nearest one too.
--
-- Make @orderVar <- newVar [0 .. 4]@ during component setup.
--
-- > order <- readVar orderVar
-- > drawn <- forM order $ \i -> (i,) <$> chip i
-- > r <- useReorder order drawn
-- > writeVar orderVar (reorderOrder r)
--
-- Only an owned press starts the drag; clipping, covering controls and
-- disabled scopes are respected. Geometry comes from the response's last layout.
useReorder :: [Int] -> [(Int, Response)] -> NanoUI Reorder
useReorder order responses = do
  (wid, ctx) <- freshWidget
  inp <- askInput
  let key = intKey wid
      V2 mx my = inputMousePos inp
      press = pressedIn MouseLeft inp
      hit = fst <$> find (respPressed . snd) responses
      items = [(i, respRect r) | (i, r) <- responses]
  store <- liftIO (getStore ctx)
  -- The drag keeps the press that started it ('leftPress') and lasts only
  -- under it: one released while this hook did not run does not come back.
  let stored = lookupSpec slotDragItem key store
  token <- if press || isJust stored then liftIO (leftPress ctx) else pure 0
  let dragging
        | press = hit
        | token == findSpec slotDragPress 0 key store = stored
        | otherwise = Nothing
      startPos = if press then V2 mx my else findSpec slotDragStartPos (V2 mx my) key store
      moved =
        isJust dragging
          && not press
          && (findSpec slotDrop False key store || pastDragThreshold startPos (V2 mx my))
      distance (Rect x y w h) =
        let dx = max 0 (max (x - mx) (mx - x - w))
            dy = max 0 (max (y - my) (my - y - h))
         in dx * dx + dy * dy
      target = if null items then Nothing else Just (fst (minimumBy (comparing (distance . snd . snd)) (zip [0 :: Int ..] items)))
      preview = case (target, dragging) of
        (Just t, Just d) | moved, d `elem` order ->
          let (before, after) = splitAt t (filter (/= d) order)
           in before ++ d : after
        _ -> order
      released = releasedIn MouseLeft inp
      ended = released || not (heldIn MouseLeft inp)
      next = if ended then Nothing else dragging
  when (next /= stored || (press && isJust dragging) || moved /= findSpec slotDrop False key store) $
    liftIO . modifyStore ctx $
      case next of
        Nothing -> deleteSpec slotDragItem key . deleteSpec slotDragPress key . deleteSpec slotDragStartPos key . setFlagSpec slotDrop key False
        Just d -> insertSpec slotDragItem key d . insertSpec slotDragPress key token . insertSpec slotDragStartPos key startPos . setFlagSpec slotDrop key moved
  pure
    Reorder
      { reorderOrder = if released && moved then preview else order
      , reorderPreview = if ended then order else preview
      , reorderDragging = next
      , reorderMoved = moved
      }

-- | The navigation keys pressed this frame ('useKeyNav'). The arrows count
-- key auto-repeats; Enter and Space only the initial press.
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
        <&&> liftIO (not <$> isInert ctx wid)
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

-- | Directional step from arrow keys: @1@ for Right or Up, @-1@ for Left or Down.
{-# INLINE navStep #-}
navStep :: KeyNav -> Int
navStep nav = fromEnum (knRight nav || knUp nav) - fromEnum (knLeft nav || knDown nav)

-- | Result of one frame of keyboard list navigation ('useListNav').
data ListNav = ListNav
  { lnIndex :: !Int
  -- ^ Selection index after applying this frame's keys, or @-1@ when empty or
  -- unselected.
  , lnChanged :: !Bool
  -- ^ 'True' on the frame the index moved via keyboard navigation.
  , lnActivated :: !Bool
  -- ^ 'True' when Enter or Space was pressed while the list held focus and
  -- had a valid selection (@lnIndex >= 0@).
  }
  deriving (Eq, Show)

-- | Keyboard list navigation for a scrolled list of @count@ uniform-height
-- rows (@rowStep@ logical pixels per row, including gap). While @scrollId@
-- holds keyboard focus (see 'NanoUI.scrollFocusable'), handles Up, Down,
-- PageUp, PageDown, Home, End, Enter, and Space, and scrolls the newly
-- selected row into view via 'NanoUI.scrollRectIntoViewUi' when @rowStep > 0@.
-- Pass @0@ for @rowStep@ to skip automatic scrolling and scroll manually
-- (for example with 'NanoUI.scrollIntoViewUi').
--
-- Call before 'NanoUI.scrollConfigured' using 'NanoUI.currentId' so the new
-- selection highlights on the same frame:
--
-- > sid <- currentId
-- > sel <- readVar selVar
-- > nav <- useListNav sid (length items) sel 28
-- > when (lnChanged nav) (writeVar selVar (lnIndex nav))
-- > scrollConfigured defaultScrollConfig {scrollPolicyX = ScrollNone, scrollFocusable = True} (fixedWH 240 180) $ ...
useListNav :: WidgetId -> Int -> Int -> Float -> NanoUI ListNav
useListNav scrollId count current rowStep =
  useListNavCore scrollId count current (if rowStep > 0 then Just (\i -> Rect 0 (fromIntegral i * rowStep) 1 rowStep) else Nothing) rowStep

-- | Like 'useListNav', computing each row's content-space rectangle with
-- @rowRect@ (for variable-height rows or lists with a header above row 0).
useListNavWith :: (Int -> Rect) -> WidgetId -> Int -> Int -> NanoUI ListNav
useListNavWith rowRect scrollId count current =
  useListNavCore scrollId count current (Just rowRect) 0

useListNavCore :: WidgetId -> Int -> Int -> Maybe (Int -> Rect) -> Float -> NanoUI ListNav
useListNavCore scrollId count current mRowRect rowStep
  | count <= 0 = pure (ListNav (-1) False False)
  | otherwise = do
      let cur = if current < 0 then -1 else min (count - 1) current
          none = ListNav cur False False
      inp <- askInput
      if hashWidgetId scrollId == 0 || inputKeysNull (inputKeys inp) || not (shiftAtMost (inputModifiers inp))
        then pure none
        else do
          eligible <- keyboardFocused scrollId
          if not eligible
            then pure none
            else do
              let home = pressedOnceIn KeyHome inp
                  end = pressedOnceIn KeyEnd inp
                  pgUp = pressedIn KeyPageUp inp
                  pgDown = pressedIn KeyPageDown inp
                  up = pressedIn KeyUp inp
                  down = pressedIn KeyDown inp
                  act = pressedOnceIn KeyEnter inp || pressedOnceIn KeySpace inp
              pageStep <-
                if pgUp || pgDown
                  then withContext $ \ctx -> do
                    mm <- readScrollMetrics ctx scrollId
                    let viewH = maybe 0 (rectH . scrollViewport) mm
                        stepH
                          | rowStep > 0 = rowStep
                          | Just rectOf <- mRowRect = max 1 (rectH (rectOf (max 0 cur)))
                          | otherwise = 0
                    pure (if viewH > 0 && stepH > 0 then max 1 (floor (viewH / stepH)) else 10)
                  else pure 10
              let next
                    | home = 0
                    | end = count - 1
                    | pgUp = if cur < 0 then count - 1 else max 0 (cur - pageStep)
                    | pgDown = if cur < 0 then 0 else min (count - 1) (cur + pageStep)
                    | up = if cur < 0 then count - 1 else max 0 (cur - 1)
                    | down = if cur < 0 then 0 else min (count - 1) (cur + 1)
                    | otherwise = cur
                  changed = next /= cur
              when changed $
                forM_ mRowRect $ \rectOf ->
                  withContext (\ctx -> scrollRectIntoView ctx scrollId (rectOf next) ScrollNearest ScrollInstant)
              pure (ListNav next changed (act && next >= 0))

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
-- frame's input rather than the pointer routed here. A press on @opener@
-- does not: the widget that opened the panel toggles it on its own click,
-- which would otherwise reopen what the press just closed. Nor does a press
-- on an open dropdown or text-edit menu, which may lie outside the panel it
-- belongs to, or an Escape that closes one or a focused combo box's list, or
-- that something declared earlier (a popup inside this one) already took
-- ('escapeReserved'). Nor does a press on a
-- floating panel inside @owner@'s (a context menu or popup declared in its
-- body), which may lie past the panel's edge.
useDismissable :: WidgetId -> Rect -> Rect -> NanoUI Bool
useDismissable owner panel opener = do
  inp <- askFrameInput
  withContext $ \ctx -> do
    route <- getsInteraction ctx isPointerRoute
    reserved <- escapeReserved ctx inp
    let onMenu = case route of
          RouteLayer _ -> False
          _ -> True
        esc = pressedOnceIn KeyEscape inp && not reserved
        pressed = anyButtonPressed inp && not onMenu
        at = inputMousePos inp
        outside = pressed && not (rectContains panel at || rectContains opener at)
    when esc (markEscapeConsumed ctx)
    inChild <- case route of
      RouteLayer k | outside && k /= 0 && k /= intKey owner -> do
        ownerNode <- findNodeByWidgetId ctx owner
        layerNode <- lookupNodeByKey (ctxNodeArena ctx) k
        case (layerNode, ownerNode) of
          (Just l, Just o) -> nodeInSubtree ctx l o
          _ -> pure False
      _ -> pure False
    pure (esc || (outside && not inChild))
