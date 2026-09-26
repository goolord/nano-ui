-- | The pointer and keyboard steps that 'NanoUI.Internal.Frame.runFrame' runs around
-- the view: recording where a press landed, choosing the active and hot
-- widgets, turning a release into a click, moving keyboard focus, and copying
-- selection state from the store into the nodes that paint it. All but
-- 'armPointerPress' run after layout, so their hit tests use this frame's
-- solved rects, where the view had only the previous frame's. Also the checks
-- the backend runs between frames: whether input needs a frame, and the
-- gestures and panels in progress.
module NanoUI.Internal.Frame.Input
  ( finalizeTabFocus
  , refreshHover
  , armPointerPress
  , disarmPointerPress
  , pressTargets
  , finalizePointerPress
  , finalizePointerRelease
  , finalizeTextInputFocus
  , finalizeSelectFocus
  , finalizeFocusRequest
  , PressTargets (..)
  , targetsAt
  , constrainFocusToModal
  , recordFocusKind
  , needsRedraw
  , pointerDragActive
  , textFieldActive
  , floatingPanelActive
  , debugPanelOpen
  , probeHotId
  , recordCoveredWidgets
  ) where

import Control.Applicative ((<|>))
import Control.Monad (filterM, guard, mfilter, unless, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as M
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.IntSet qualified as IS
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, maybeToList)
import NanoUI.Internal.Context
import NanoUI.Internal.Frame.Hit
import NanoUI.Internal.Frame.Scroll (probeScrollBarHover)
import NanoUI.Internal.Frame.Select (focusedComboNode, overlayMenuOwnerAt)
import NanoUI.Internal.Frame.TextArea (collapseTextFieldSelection)
import NanoUI.Internal.Frame.TextInput (nodeTextFieldGeom)
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Input
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Monad (ifM, unlessM, whenM, (<&&>))
import NanoUI.Internal.Types (DamageBounds (..), Rect (..), V2 (..), defaultDamageSlop, rectContains)
import NanoUI.Internal.WidgetText (hasFlag, buttonFlagClose, buttonFlagMenuBar, buttonFlagMenu, buttonFlagRow)

-- | On Tab (Shift+Tab goes backwards), move keyboard focus to 'tabTarget'
-- and show the focus ring. A Tab consumed by the focused widget
-- ('NanoUI.Internal.Context.markTabConsumed') moves nothing.
finalizeTabFocus :: Context -> Input -> IO ()
finalizeTabFocus ctx inp =
  whenM (pure (inputKeysElem KeyTab (inputKeys inp)) <&&> (not <$> tabConsumed ctx)) $ do
    cur <- readIORef (ctxFocusId ctx)
    next <- tabTarget ctx cur (modShift (inputModifiers inp))
    when (hashWidgetId next /= 0) $ do
      -- Tab can reveal the focus ring without changing the focused rectangle.
      -- Damage that case explicitly; geometry comparison cannot detect it.
      wasVisible <- readIORef (ctxFocusVisible ctx)
      when (next == cur && not wasVisible) $
        damageWidget ctx next (DamageInflated defaultDamageSlop)
      writeIORef (ctxFocusId ctx) next
      writeIORef (ctxFocusVisible ctx) True
      markDirty ctx

-- | Where Tab moves focus from @cur@ (backwards when @back@), stepping
-- through 'tabStops' and wrapping at both ends. @WidgetId 0@ if there are
-- none.
tabTarget :: Context -> WidgetId -> Bool -> IO WidgetId
tabTarget ctx cur back = (\stops -> tabNext cur stops back) <$> tabStops ctx

-- | Tab stops in declaration order: the widgets that called
-- 'NanoUI.Internal.Context.registerFocusable' during the view. While a
-- modal is open, only those inside the top modal.
tabStops :: Context -> IO [WidgetId]
tabStops ctx = do
  -- The modal's root is looked up once for the whole list. Each widget then
  -- costs one walk up its ancestors.
  top <- topModalNode (ctxNodeArena ctx)
  let inModal w = maybe (pure True) (\modal -> widgetIdInSubtree ctx modal w) top
  filterM inModal . filter (/= WidgetId 0) =<< getFocusables ctx

-- | Whether @wid@ is a menu row or a menu-bar title. Their hover highlight
-- switches on and off at once, so 'refreshHover' runs no animation for them.
isMenuButtonWidget :: Context -> WidgetId -> IO Bool
isMenuButtonWidget ctx wid =
  withWidgetNode ctx wid False $ \idx ->
    ((== NodeButton) <$> getNodeType (ctxNodeArena ctx) idx)
      <&&> ((\si -> hasFlag buttonFlagMenu si || hasFlag buttonFlagMenuBar si) <$> getStyleIdx (ctxNodeArena ctx) idx)

-- | Find the hot widget in this frame's layout and store it in 'ctxHotId' and
-- 'ctxLastHotId'. Runs after layout and before painting. When the hot widget
-- changes, the hover animation of the widget that lost the pointer runs from
-- 1 to 0, and that of the widget that gained it from 0 to 1, each over 0.12
-- seconds. The painter blends a widget's hover colour by that value.
refreshHover :: Context -> Input -> IO ()
refreshHover ctx inp = do
  prevHot <- readIORef (ctxLastHotId ctx)
  newHot <- probeHotId ctx (inputMousePos inp)
  writeIORef (ctxHotId ctx) newHot
  writeIORef (ctxLastHotId ctx) newHot
  when (prevHot /= newHot) $ do
    prevMenu <- isMenuButtonWidget ctx prevHot
    newMenu <- isMenuButtonWidget ctx newHot
    when (hashWidgetId prevHot /= 0 && not prevMenu) $ startAnimation ctx prevHot 1 0 0.12
    when (hashWidgetId newHot /= 0 && not newMenu) $ startAnimation ctx newHot 0 1 0.12

-- | Record in 'ctxPressPos' where each button pressed this frame went down.
-- Runs before the view. A widget treats a release as its click, or a held
-- button as held on it, only if the press point is also on it, so a press
-- that drifts onto a neighbour clicks nothing. 'disarmPointerPress' clears
-- the point on release.
armPointerPress :: Context -> Input -> IO ()
armPointerPress ctx inp =
  when (anyButtonPressed inp) $ do
    let here = inputMousePos inp
    modifyIORef' (ctxPressPos ctx) $ \m -> foldr (`M.insert` here) m (buttonsToList (inputButtonsPressed inp))
    -- A pointer press hides the keyboard focus ring.
    when (pressedIn MouseLeft inp) $ writeIORef (ctxFocusVisible ctx) False

-- | Forget the press point of each button that came up this frame. Runs after
-- the view, which compares the release with the press point.
disarmPointerPress :: Context -> Input -> IO ()
disarmPointerPress ctx inp =
  when (anyButtonReleased inp) $
    modifyIORef' (ctxPressPos ctx) $ \m -> foldr M.delete m (buttonsToList (inputButtonsReleased inp))

-- | What a left press landed on, for the steps that act on it: the
-- interactive widget, the text field or text area, and the select under the
-- pointer, all 'Nothing' on a frame without a press; and whether the
-- interactive widget is a control drawn inside the text field.
data PressTargets = PressTargets
  { ptInteractive :: !(Maybe WidgetId)
  , ptTextField :: !(Maybe WidgetId)
  , ptSelect :: !(Maybe WidgetId)
  , ptFieldControl :: !Bool
  }

-- | The 'PressTargets' of this frame's left press ('targetsAt'). Runs after
-- layout, like the steps.
pressTargets :: Context -> Input -> IO PressTargets
pressTargets ctx inp
  | not (pressedIn MouseLeft inp) = pure none
  | otherwise = targetsAt ctx (inputMousePos inp)

-- | The widgets a press at @mouse@ would land on, found in one pass over the
-- arena instead of one per step. A node's hit test does not depend on which
-- step asks, so each node is tested at most once, and the pass stops once
-- every target is found. Each is the first match in arena order, which is
-- declaration order. The painter draws siblings from the last declared to the
-- first, so where two overlap the earlier one is on top, unless a stack or
-- pinned child draws a later match over it ('topmostHit'). A widget drawn
-- inside the interactive one, such as a control among its adornments, is on
-- top of it and takes the press ('innermostHit'). When a stack or pinned
-- node could draw a 'PointerBlock' node on top, a first pass checks whether
-- it takes the press; if so there are no targets.
targetsAt :: Context -> V2 -> IO PressTargets
targetsAt ctx@Context {ctxNodeArena = na} mouse = do
  top <- overlayHitRoot ctx mouse
  let under idx = getNodeType na idx >>= \nt -> widgetUnderMouse ctx top mouse nt idx
  -- A 'PointerBlock' node on top with no widget of its own under the press
  -- swallows it: nothing beneath is pressed or focused. Only stacks and
  -- pinned nodes draw one node over another.
  layered <- layeredNodeCount na
  let takerUnder idx = takesPointer na idx <&&> under idx
      blocking idx = not . isWidgetNode <$> getNodeType na idx
  blocked <-
    if layered == 0
      then pure False
      else maybe (pure False) blocking =<< reachedHit ctx takerUnder
  if blocked then pure none else pressTargetsAt ctx under

-- | 'targetsAt' once no blocking node took the press. @under@ is the press
-- hit test.
pressTargetsAt :: Context -> (NodeIdx -> IO Bool) -> IO PressTargets
pressTargetsAt ctx@Context {ctxNodeArena = na} under = do
  found <- newIORef none
  _ <- findClassNodeM na PointerNodes $ \idx -> do
    nt <- getNodeType na idx
    PressTargets i t s onControl <- readIORef found
    let wantI = isNothing i && isWidgetNode nt
        wantT = isNothing t && (nt == NodeTextInput || nt == NodeTextArea)
        wantS = isNothing s && nt == NodeSelect
    pure (wantI || wantT || wantS) <&&> under idx <&&> do
      wid <- getWidgetId na idx
      let topmost kind = topmostHit ctx (\d -> (kind <$> getNodeType na d) <&&> under d) idx
          topmostId want kind = if want then getWidgetId na =<< topmost kind else pure wid
      inner <- if wantI then getWidgetId na =<< innermostHit ctx under =<< topmost isWidgetNode else pure wid
      fieldWid <- topmostId wantT (\k -> k == NodeTextInput || k == NodeTextArea)
      selectWid <- topmostId wantS (== NodeSelect)
      let pick want hit cur = if want then Just hit else cur
          !r =
            PressTargets
              (if wantI then Just inner else i)
              (pick wantT fieldWid t)
              (pick wantS selectWid s)
              (onControl || (wantI && wantT && inner /= fieldWid))
      writeIORef found r
      pure (isJust (ptInteractive r) && isJust (ptTextField r) && isJust (ptSelect r))
  readIORef found

none :: PressTargets
none = PressTargets Nothing Nothing Nothing False

-- | On a left press, make the interactive widget under the pointer the active
-- widget, unless it is disabled. Runs after layout. 'pressTargets' searches
-- the arena in the same order as hover ('refreshHover'), so where widgets
-- overlap both pick the one on top.
finalizePointerPress :: Context -> PressTargets -> IO ()
finalizePointerPress ctx targets =
  enabledTarget ctx (ptInteractive targets) >>= mapM_ (writeIORef (ctxActiveId ctx))

-- | Whether a press at @mouse@ lands on node @idx@ of type @nt@: the point is
-- in its hit rect ('widgetHitRect') and clip, floating panels and modals
-- leave it reachable ('overlayHitAllowed', with @top@ from
-- 'overlayHitRoot'), and the node does not pass the pointer through
-- ('passesPointer').
{-# INLINE widgetUnderMouse #-}
widgetUnderMouse :: Context -> Maybe NodeIdx -> V2 -> NodeType -> NodeIdx -> IO Bool
widgetUnderMouse ctx top mouse nt idx = do
  Rect x y w h <- getNodeRect (ctxNodeArena ctx) idx
  rect <- widgetHitRect ctx nt idx x y w h
  nodeClippedHit ctx idx rect mouse
    <&&> overlayHitAllowed ctx top idx
    <&&> (not <$> passesPointer (ctxNodeArena ctx) idx)

-- | The rect a press on node @idx@ must land in: a text field's box
-- ('nodeTextFieldGeom'), a close button's padded target, or the node rect.
widgetHitRect :: Context -> NodeType -> NodeIdx -> Float -> Float -> Float -> Float -> IO Rect
widgetHitRect ctx nt idx x y w h = case nt of
  NodeTextInput -> fst <$> nodeTextFieldGeom ctx idx x y w h
  NodeButton -> do
    si <- getStyleIdx (ctxNodeArena ctx) idx
    -- Close buttons get a padded target that stays inside the title bar, so
    -- the inner east resize still works below the control.
    pure $
      if hasFlag buttonFlagClose si
        then Rect (x - 8) (y - 4) (w + 10) (h + 4)
        else Rect x y w h
  _ -> pure (Rect x y w h)

-- | Resolve a left-button release against this frame's solved rects, and let
-- go of the active widget. Runs after layout.
--
-- During the view a widget tests the pointer against its rect from the
-- previous frame, so it misses a release when it has moved since, or when
-- this is its first frame. This step repeats the test with the solved rect.
-- When the release is on the active widget and the view did not see it there,
-- the widget's id goes into 'ctxClickedId', and the widget reports the click
-- on the next frame, which this asks for. Only the node types in
-- 'postsLayoutClick' get one. A radio option, tree row or tab header reports
-- it like any other click, and its group picks it on that frame.
--
-- A release on the widget also sets its hover animation to 1, so it paints as
-- fully hovered at once.
finalizePointerRelease :: Context -> Input -> IO ()
finalizePointerRelease ctx@Context {ctxNodeArena = na} inp =
  when (releasedIn MouseLeft inp) $ do
    let mouse = inputMousePos inp
    active <- readIORef (ctxActiveId ctx)
    when (hashWidgetId active /= 0) $ do
      releasedClicked <- readIORef (ctxReleaseClickedId ctx)
      -- Several nodes can carry the active id. Every one of them that the
      -- release is on handles it. Whether the pointer came up over the widget
      -- is decided by the first node with the id, in arena order.
      let release over idx = do
            wid <- getWidgetId na idx
            if wid /= active
              then pure over
              else do
                nt <- getNodeType na idx
                rect <- getNodeRect na idx
                visible <- nodeClippedHit ctx idx rect mouse
                when (visible && postsLayoutClick nt && releasedClicked /= active) $
                  unlessM (inUiClickHit ctx active mouse) $ do
                    writeIORef (ctxClickedId ctx) active
                    -- Covered: whatever the click changes is damaged on the
                    -- frame that reports it.
                    markDirtyCovered ctx
                pure (over <|> Just visible)
      releasedOver <- foldNodesM na release Nothing
      writeIORef (ctxActiveId ctx) (WidgetId 0)
      when (releasedOver == Just True) $
        setAnimationValue ctx active 1

-- | The node types for which 'finalizePointerRelease' turns a release the view
-- missed into a click on the next frame.
postsLayoutClick :: NodeType -> Bool
postsLayoutClick nt = nt == NodeButton || nt == NodeSelect

-- | Whether the view's own hit test saw a release at @mouse@ on widget @wid@.
-- It repeats the test of 'NanoUI.Internal.Widgets.Node.resolveInteraction': the widget
-- is enabled, the frame routed the pointer to its layer, and the point is
-- inside its rect from the previous frame and inside the scroll viewports
-- above it. A covered widget ('ctxPointerCovered') also passes: the view
-- already saw that release and rejected it.
inUiClickHit :: Context -> WidgetId -> V2 -> IO Bool
inUiClickHit ctx wid mouse = do
  disabled <- isDisabled ctx wid
  mrect <- getPrevRect ctx wid
  case mrect of
    Just r | not disabled ->
      -- The view saw the release only if the frame routed the pointer to
      -- this node's layer.
      withWidgetNode ctx wid (rectContains r mouse) $ \idx ->
        nodeOwnsPointer ctx idx <&&> nodeInteractionHit ctx idx r mouse
    _ -> pure False

-- | Focus the enabled text field under a left press using solved geometry.
-- A press elsewhere clears focus, collapses the prior selection, and closes
-- its edit menu. Menu/dropdown presses are removed from the supplied layer
-- input, preserving the owning field's focus until the pick is processed.
-- A press on a control drawn inside a text field keeps the focus as it was.
finalizeTextInputFocus :: Context -> Input -> PressTargets -> IO ()
finalizeTextInputFocus ctx inp targets =
  when (pressedIn MouseLeft inp) $ do
    prevFocus <- readIORef (ctxFocusId ctx)
    mFocused <-
      if ptFieldControl targets
        then pure (mfilter (== prevFocus) (ptTextField targets))
        else enabledTarget ctx (ptTextField targets)
    case mFocused of
      Nothing -> do
        when (prevFocus /= WidgetId 0) $ markDirty ctx
        collapseTextFieldSelection ctx prevFocus
        writeIORef (ctxFocusId ctx) (WidgetId 0)
        modifyInteraction ctx (\s -> s {isTextInputMenu = Nothing})
      Just wid -> focusWidget ctx wid

-- | On a left press on an enabled select's field, give the select keyboard
-- focus, whether the press opens or closes it. Runs after
-- 'finalizeTextInputFocus', which has cleared focus for a press outside every
-- text field.
finalizeSelectFocus :: Context -> PressTargets -> IO ()
finalizeSelectFocus ctx targets =
  enabledTarget ctx (ptSelect targets) >>= mapM_ (focusWidget ctx)

-- | Apply the view's last focus request this frame
-- ('NanoUI.Internal.Monad.requestFocus', 'NanoUI.Internal.Monad.focusNext',
-- 'NanoUI.Internal.Monad.focusPrevious',
-- 'NanoUI.Internal.Monad.clearFocus'). Runs after the pointer steps and
-- before 'constrainFocusToModal' and 'finalizeTabFocus', so a Tab in the
-- same frame continues from the newly focused widget.
--
-- 'FocusNowhere' clears focus, and 'FocusNext' and 'FocusPrevious' go where
-- Tab would ('tabTarget'). 'FocusOn' succeeds only for one of the
-- 'tabStops', never a disabled widget; otherwise the request is dropped.
-- Moving focus behaves like a press elsewhere: the old field collapses its
-- selection, and the text-field menu and any open dropdown close. The new
-- widget shows the focus ring. Requesting the focused widget changes
-- nothing, including the ring.
finalizeFocusRequest :: Context -> IO ()
finalizeFocusRequest ctx =
  readIORef (ctxFocusRequest ctx) >>= mapM_ (\req -> do
    writeIORef (ctxFocusRequest ctx) Nothing
    prev <- readIORef (ctxFocusId ctx)
    stops <- tabStops ctx
    let step back = mfilter (/= WidgetId 0) (Just (tabNext prev stops back))
        target = case req of
          FocusOn w -> w <$ guard (w `elem` stops)
          FocusNowhere -> Just (WidgetId 0)
          FocusNext -> step False
          FocusPrevious -> step True
    for_ (mfilter (/= prev) target) $ \wid -> do
      collapseTextFieldSelection ctx prev
      modifyInteraction ctx (\s -> s {isTextInputMenu = Nothing})
      store <- getStore ctx
      when (anySelectOpen store) $ setStore ctx (closeSelects store)
      -- The damage pass repaints both widgets, since the focused id moved.
      writeIORef (ctxFocusId ctx) wid
      when (hashWidgetId wid /= 0) $ writeIORef (ctxFocusVisible ctx) True
      markDirty ctx)

-- | Give @wid@ keyboard focus, repainting when focus moved.
focusWidget :: Context -> WidgetId -> IO ()
focusWidget ctx wid = do
  prev <- readIORef (ctxFocusId ctx)
  writeIORef (ctxFocusId ctx) wid
  when (prev /= wid) $ markDirty ctx

-- | The pressed widget, unless it is disabled. A disabled field counts as
-- nothing here, so a press on it takes focus away from the field that had it
-- and gives it to no other.
enabledTarget :: Context -> Maybe WidgetId -> IO (Maybe WidgetId)
enabledTarget ctx = fmap listToMaybe . filterM (fmap not . isDisabled ctx) . maybeToList

-- | Next focus id, or previous with Shift, wrapping at both ends. An unknown
-- current id selects the first entry; an empty list returns @WidgetId 0@.
tabNext :: WidgetId -> [WidgetId] -> Bool -> WidgetId
tabNext cur ids shift =
  fromMaybe (WidgetId 0) . listToMaybe $ case break (== cur) ids of
    (_, []) -> ids
    (before, _ : after)
      | shift -> reverse (if null before then ids else before)
      | otherwise -> after ++ ids

-- | While a modal is open, take keyboard focus away from a widget outside the
-- top modal. The frame runs this after the pointer steps, which can move
-- focus, and before 'finalizeTabFocus'.
constrainFocusToModal :: Context -> IO ()
constrainFocusToModal ctx = do
  focus <- readIORef (ctxFocusId ctx)
  when (hashWidgetId focus /= 0) $
    unlessM (widgetOverlayAllowed ctx focus) $ writeIORef (ctxFocusId ctx) (WidgetId 0)

-- | Record in 'isFocusKind' what kind of widget has the keyboard and which
-- keys it claims ('KeyClaim'), from last frame's nodes. Runs before the
-- view rebuilds them, so a shortcut declared before the focused widget
-- still sees it. @ime@: an input method owns the focused field's keys this
-- frame ('NanoUI.Internal.Frame.TextInput.claimComposition').
recordFocusKind :: Context -> Bool -> IO ()
recordFocusKind ctx ime = do
  focus <- readIORef (ctxFocusId ctx)
  kind <- case () of
    _
      | hashWidgetId focus == 0 -> pure FocusNone
      | ime -> pure FocusComposing
      | otherwise -> withWidgetNode ctx focus FocusNone $ \idx -> do
          let si = getStyleIdx (ctxNodeArena ctx) idx
          getNodeType (ctxNodeArena ctx) idx >>= \case
            NodeTextInput -> pure FocusTextLine
            NodeTextArea -> pure (FocusControl KeysType)
            -- A tree row moves with the arrows; any other button activates.
            NodeButton -> si <&> \s -> FocusControl (if hasFlag buttonFlagRow s then KeysNavigate else KeysActivate)
            NodeDrawing -> FocusControl . drawingKeyClaim <$> si
            _ -> pure (FocusControl KeysNavigate)
  was <- getsInteraction ctx isFocusKind
  when (kind /= was) $ modifyInteraction ctx (\s -> s {isFocusKind = kind})

-- | Whether state or input changes require a frame. Arguments are previous
-- and current input. Tests hover only after pointer motion; timed wake
-- deadlines are handled separately by the session runner.
needsRedraw :: Context -> Input -> Input -> IO Bool
needsRedraw ctx prev inp = do
  dirty <- isDirty ctx
  anim <- anyAnimating ctx
  drag <- getsInteraction ctx (\s -> isJust (isScrollDrag s) || isJust (isWindowDrag s))
  overlay <- overlayMenuOpen ctx
  -- The layout overlay highlights whatever node the pointer is over.
  explain <- getExplainLayout ctx
  let moved = inputMousePos prev /= inputMousePos inp
  if dirty
    || anim
    || inputInteracted prev inp
    || inputWindowRedraw inp
    || inputPointerHeld inp
    || drag
    || (overlay && moved)
    || (explain && moved)
    then pure True
    else
      -- Idle: hover can only change when the pointer moved since the frame
      -- whose hover state we still hold. Skip the O(n) hot probe otherwise.
      pure moved <&&> do
        -- A widget that tracks the pointer wants every move over it;
        -- one that does not wants only the move that leaves it.
        lastHot <- readIORef (ctxLastHotId ctx)
        tracked <-
          if hashWidgetId lastHot == 0
            then pure False
            else maybe False cdrTracked <$> lookupCustomDrawing ctx lastHot
        hotMoved <- if tracked then pure True else (/= lastHot) <$> probeHotId ctx (inputMousePos inp)
        -- A scrollbar is not a widget but brightens on hover. A tooltip
        -- target need not be a widget either, and a tooltip that follows
        -- the pointer moves with it.
        if hotMoved
          then pure True
          else ifM scrollBarHoverMoved (pure True) (hoverZoneCrossed ctx (inputMousePos prev) (inputMousePos inp))
  where
    bar = fmap (\(wid, dir, _) -> (wid, dir))
    scrollBarHoverMoved =
      (/=) <$> (bar <$> getsInteraction ctx isScrollHover) <*> (bar <$> probeScrollBarHover ctx inp)

-- | Whether a window, scrollbar, resize, slider, or colour-picker gesture
-- is active. Text-selection drags are tracked separately.
pointerDragActive :: Context -> IO Bool
pointerDragActive ctx = do
  gesture <- getsInteraction ctx $ \s ->
    isJust (isWindowDrag s) || isJust (isScrollDrag s) || isJust (isWindowResize s)
  sliderOrPicker <- focusedNodeIs ctx ctxActiveId (\nt -> nt == NodeSlider || nt == NodeColorPicker)
  pure (gesture || sliderOrPicker)

-- | Whether the node of the widget id held in @ref@ satisfies @p@.
focusedNodeIs :: Context -> (Context -> IORef WidgetId) -> (NodeType -> Bool) -> IO Bool
focusedNodeIs ctx ref p = do
  wid <- readIORef (ref ctx)
  withWidgetNode ctx wid False $ \idx -> p <$> getNodeType (ctxNodeArena ctx) idx

-- Select dropdown or text-input menu is open. Overlay hover is not a widget id,
-- so while one is up every pointer move needs a frame. A focused combo (a
-- search-style field carrying options) also owns an open dropdown.
overlayMenuOpen :: Context -> IO Bool
overlayMenuOpen ctx = do
  store <- getStore ctx
  menu <- getsInteraction ctx isTextInputMenu
  if anySelectOpen store || isJust menu
    then pure True
    else isJust <$> focusedComboNode ctx

-- | Focused text field or its context menu. Typing reaches it as input events,
-- which wake the loop by themselves, so focus alone keeps nothing running.
textFieldActive :: Context -> IO Bool
textFieldActive ctx =
  ifM (isJust <$> getsInteraction ctx isTextInputMenu) (pure True) $
    focusedNodeIs ctx ctxFocusId (\nt -> nt == NodeTextInput || nt == NodeTextArea)

-- | Whether modal state or the current arena contains a floating panel,
-- including windows and popups.
floatingPanelActive :: Context -> IO Bool
floatingPanelActive ctx =
  (||) <$> modalActive ctx <*> ((> 0) <$> floatingNodeCount (ctxNodeArena ctx))

-- | Whether the arena contains any floating window. The name does not imply
-- that its contents are a debug readout.
debugPanelOpen :: Context -> IO Bool
debugPanelOpen ctx = isJust <$> topmostFloating ctx (== NodeWindow) (const True)

probeHotId :: Context -> V2 -> IO WidgetId
probeHotId ctx@Context {ctxNodeArena = na} mouse = do
  -- A button that went down on a menu or dropdown keeps everything cold.
  offLayers <- pointerHeldOffLayers ctx
  if offLayers
    then pure (WidgetId 0)
    else do
      mOverlay <- overlayMenuOwnerAt ctx mouse
      case mOverlay of
        Just wid -> pure wid
        Nothing -> maybe (pure (WidgetId 0)) (getWidgetId na) =<< reachedWidgetAt ctx mouse

-- | Record in 'ctxPointerReach' which nodes the pointer reaches in the frame
-- the user saw: where stacks or pinned nodes overlap, the topmost node under
-- the pointer that takes it ('reachedAt', as hover finds it) and its
-- ancestors. Every other node under the pointer, widget or not, is covered.
--
-- Runs before the view, against last frame's layout. During the view each
-- widget hit-tests its rect in that layout
-- ('NanoUI.Internal.Widgets.Node.resolveInteraction'), and a covered one
-- gets neither hover nor presses. Nothing is covered when the pointer is on
-- a menu or dropdown (which routes it away from every layer), when nothing
-- under it takes the pointer, or when the layout has no stack or pinned node
-- ('layeredNodeCount'), since then nodes only overlap their ancestors.
recordCoveredWidgets :: Context -> PointerRoute -> Input -> IO ()
recordCoveredWidgets ctx@Context {ctxNodeArena = na} route inp = do
  layered <- layeredNodeCount na
  reach <- case route of
    RouteLayer _ | layered > 0 -> traverse (idsUpFrom IS.empty) =<< reachedAt ctx mouse
    _ -> pure Nothing
  -- Most frames have nothing covered before or after, and write nothing.
  old <- readIORef (ctxPointerReach ctx)
  unless (isNothing old && isNothing reach) $
    writeIORef (ctxPointerReach ctx) $! reach
 where
  mouse = inputMousePos inp
  -- Ids of node @i@ and its ancestors. A node sharing one of these ids is
  -- not covered either.
  idsUpFrom !acc i
    | i < 0 = pure acc
    | otherwise = do
        wid <- getWidgetId na i
        let acc' = if hashWidgetId wid == 0 then acc else IS.insert (intKey wid) acc
        getParent na i >>= idsUpFrom acc'
