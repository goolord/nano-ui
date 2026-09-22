-- | The pointer and keyboard steps that 'NanoUI.Internal.Frame.runFrame' runs around
-- the view: recording where a press landed, choosing the active and hot
-- widgets, turning a release into a click, and moving keyboard focus. All but
-- 'armPointerPress' run after layout, so their hit tests use this frame's
-- solved rects, where the view had only the previous frame's.
module NanoUI.Internal.Frame.Input
  ( finalizeTabFocus
  , refreshHover
  , armPointerPress
  , disarmPointerPress
  , PressTargets
  , pressTargets
  , finalizePointerPress
  , finalizePointerRelease
  , finalizeTextInputFocus
  , finalizeSelectFocus
  , findTopWidgetUnderMouse
  , isInteractiveNode
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isJust, isNothing)
import NanoUI.Internal.Context
  ( Context (..)
  , damageWidget
  , getFocusables
  , isDisabled
  , markDirty
  , markDirtyCovered
  , setAnimationValue
  , modifyInteraction
  , startAnimation
  , tabConsumed
  , InteractionState (..)
  )
import NanoUI.Internal.Frame.Focus (filterModalFocusables, tabNext, tabNextFocusables)
import NanoUI.Internal.Frame.Hit
  ( modalTreeOpen
  , nodeClippedHit
  , nodeInteractionHit
  , nodeOwnsPointer
  , overlayHitAllowed
  , overlayHitRoot
  , scrollHitRect
  , withWidgetNode
  )
import NanoUI.Internal.Frame.Redraw (probeHotId)
import NanoUI.Internal.Frame.TextEdit (collapseTextFieldSelection)
import NanoUI.Internal.Frame.TextInput (nodeTextFieldGeom)
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Input
  ( Input (..)
  , Key (..)
  , inputKeysElem
  , inputModifiers
  , inputMousePos
  , inputMousePressed
  , inputMouseReleased
  , inputMouseRightPressed
  , inputMouseRightReleased
  , modShift
  )
import NanoUI.Internal.Layout.Arena
  ( isWidgetNode
  , NodeClass (PointerNodes)
  , NodeIdx
  , NodeType (..)
  , findClassNodeM
  , foldNodesM
  , getNodeRect
  , getNodeType
  , getRect
  , getStyleIdx
  , getWidgetId
  )
import NanoUI.Internal.Monad (ifM, unlessM, whenM, (<&&>))
import NanoUI.Internal.Types (DamageBounds (..), Rect (..), V2 (..), defaultDamageSlop, rectContains)
import NanoUI.Internal.WidgetText (hasFlag, buttonFlagClose, buttonFlagMenuBar, buttonFlagMenu)

-- | Move keyboard focus when Tab was pressed, backwards with Shift held. Focus
-- steps through the widgets that called 'NanoUI.Internal.Context.registerFocusable'
-- during the view, in declaration order, and wraps at both ends. While a modal
-- is open, only the widgets inside the top modal take part. Focus moved this
-- way shows the focus ring. A Tab taken by the widget holding the keyboard
-- ('NanoUI.Internal.Context.markTabConsumed') moves nothing.
finalizeTabFocus :: Context -> Input -> IO ()
finalizeTabFocus ctx inp =
  whenM (pure (inputKeysElem KeyTab (inputKeys inp)) <&&> (not <$> tabConsumed ctx)) $ do
    open <- modalTreeOpen ctx
    let shift = modShift (inputModifiers inp)
    cur <- readIORef (ctxFocusId ctx)
    next <-
      if not open
        then tabNextFocusables ctx cur shift
        else do
          focusables <- getFocusables ctx
          ids <- filterModalFocusables ctx (filter (/= WidgetId 0) focusables)
          pure (tabNext cur ids shift)
    when (hashWidgetId next /= 0) $ do
      -- Tab can reveal the focus ring without changing the focused rectangle.
      -- Damage that case explicitly; geometry comparison cannot detect it.
      wasVisible <- readIORef (ctxFocusVisible ctx)
      when (next == cur && not wasVisible) $
        damageWidget ctx next (DamageInflated defaultDamageSlop)
      writeIORef (ctxFocusId ctx) next
      writeIORef (ctxFocusVisible ctx) True
      markDirty ctx

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

-- | Record where each button went down, in 'ctxPressPos' and
-- 'ctxRightPressPos'. Runs before the view. A widget counts a release as its
-- click only when the press point is on it as well, so a press that drifts
-- onto a neighbouring widget before the button comes up clicks nothing.
-- 'disarmPointerPress' forgets the point once the button is up.
armPointerPress :: Context -> Input -> IO ()
armPointerPress ctx inp = do
  let here = Just (inputMousePos inp)
  when (inputMousePressed inp) $ do
    writeIORef (ctxPressPos ctx) here
    -- A pointer press hides the keyboard focus ring.
    writeIORef (ctxFocusVisible ctx) False
  when (inputMouseRightPressed inp) $ writeIORef (ctxRightPressPos ctx) here

-- | Forget the press point of each button that came up this frame. Runs after
-- the view, which compares the release with the press point.
disarmPointerPress :: Context -> Input -> IO ()
disarmPointerPress ctx inp = do
  when (inputMouseReleased inp) $ writeIORef (ctxPressPos ctx) Nothing
  when (inputMouseRightReleased inp) $ writeIORef (ctxRightPressPos ctx) Nothing

-- | What a left press landed on, for the steps that act on it: the
-- interactive widget, the text field or text area, and the select under the
-- pointer, each as 'findTopWidgetUnderMouse' would find it. All 'Nothing'
-- on a frame without a press.
data PressTargets = PressTargets
  { ptInteractive :: !(Maybe WidgetId)
  , ptTextField :: !(Maybe WidgetId)
  , ptSelect :: !(Maybe WidgetId)
  }

-- | The 'PressTargets' of this frame's left press, found in one pass over the
-- arena instead of one per step. A node's hit test does not depend on which
-- step asks, so each node is tested at most once, and the pass stops once
-- every target is found. Runs after layout, like the steps.
pressTargets :: Context -> Input -> IO PressTargets
pressTargets ctx inp
  | not (inputMousePressed inp) = pure none
  | otherwise = do
      let na = ctxNodeArena ctx
          mouse = inputMousePos inp
      top <- overlayHitRoot ctx mouse
      found <- newIORef none
      _ <- findClassNodeM na PointerNodes $ \idx -> do
        nt <- getNodeType na idx
        PressTargets i t s <- readIORef found
        let wantI = isNothing i && isInteractiveNode nt
            wantT = isNothing t && isTextFieldNode nt
            wantS = isNothing s && nt == NodeSelect
        pure (wantI || wantT || wantS) <&&> widgetUnderMouse ctx top mouse nt idx <&&> do
          wid <- getWidgetId na idx
          let pick want cur = if want then Just wid else cur
              !r = PressTargets (pick wantI i) (pick wantT t) (pick wantS s)
          writeIORef found r
          pure (isJust (ptInteractive r) && isJust (ptTextField r) && isJust (ptSelect r))
      readIORef found
 where
  none = PressTargets Nothing Nothing Nothing

-- | Text fields and text areas, which a press focuses.
isTextFieldNode :: NodeType -> Bool
isTextFieldNode nt = nt == NodeTextInput || nt == NodeTextArea

-- | On a left press, make the interactive widget under the pointer the active
-- widget, unless it is disabled. Runs after layout. 'pressTargets' searches
-- the arena in the same order as hover ('refreshHover'), so where widgets
-- overlap both pick the one on top.
finalizePointerPress :: Context -> PressTargets -> IO ()
finalizePointerPress ctx targets =
  enabledTarget ctx (ptInteractive targets) >>= mapM_ (writeIORef (ctxActiveId ctx))

-- | The widget under @mouse@ whose node type satisfies @wanted@, or 'Nothing'.
-- It is the first match in arena order, which is declaration order. The
-- painter draws siblings from the last declared to the first, so where two
-- overlap the earlier one is on top. Only pointer nodes ('PointerNodes') are
-- searched, so @wanted@ must reject every other type.
findTopWidgetUnderMouse :: Context -> V2 -> (NodeType -> Bool) -> IO (Maybe WidgetId)
findTopWidgetUnderMouse ctx mouse wanted = do
  let na = ctxNodeArena ctx
  top <- overlayHitRoot ctx mouse
  mIdx <-
    findClassNodeM na PointerNodes $ \idx -> do
      nt <- getNodeType na idx
      pure (wanted nt) <&&> widgetUnderMouse ctx top mouse nt idx
  traverse (getWidgetId na) mIdx

-- | Whether a press at @mouse@ lands on node @idx@ of type @nt@: the point is
-- in its hit rect ('widgetHitRect') and in its clip, and the floating panels
-- and modals leave it reachable there ('overlayHitAllowed', with @top@ from
-- 'overlayHitRoot').
{-# INLINE widgetUnderMouse #-}
widgetUnderMouse :: Context -> Maybe NodeIdx -> V2 -> NodeType -> NodeIdx -> IO Bool
widgetUnderMouse ctx top mouse nt idx = do
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  rect <- widgetHitRect ctx nt idx x y w h
  nodeClippedHit ctx idx rect mouse <&&> overlayHitAllowed ctx top idx

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

-- | The node types a press can make active: the controls of 'isWidgetNode'
-- except the bare 'NodeWidget', which paints and takes nothing.
isInteractiveNode :: NodeType -> Bool
isInteractiveNode nt = nt /= NodeWidget && isWidgetNode nt

-- | Resolve a left-button release against this frame's solved rects, and let
-- go of the active widget. Runs after layout.
--
-- During the view a widget tests the pointer against its rect from the
-- previous frame, so it misses a release when it has moved since, or when
-- this is its first frame. This step repeats the test with the solved rect.
-- When the release is on the active widget and the view did not see it there,
-- the widget's id goes into 'ctxClickedId', and the widget reports the click
-- on the next frame, which this asks for. Only the node types in
-- 'postsLayoutClick' get one. A radio option or tab header reports it like any
-- other click, and its group picks it on that frame.
--
-- A release on the widget also sets its hover animation to 1, so it paints as
-- fully hovered at once.
finalizePointerRelease :: Context -> Input -> IO ()
finalizePointerRelease ctx inp =
  when (inputMouseReleased inp) $ do
    let mouse = inputMousePos inp
        na = ctxNodeArena ctx
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
postsLayoutClick nt =
  nt == NodeButton || nt == NodeTree || nt == NodeSelect || nt == NodeCheckbox || nt == NodeRadio

-- | Whether the view's own hit test saw a release at @mouse@ on widget @wid@.
-- It repeats the test of 'NanoUI.Internal.Widgets.Node.resolveInteraction': the widget
-- is enabled, the frame routed the pointer to its layer, and the point is
-- inside its rect from the previous frame and inside the scroll viewports
-- above it.
inUiClickHit :: Context -> WidgetId -> V2 -> IO Bool
inUiClickHit ctx wid mouse = do
  disabled <- isDisabled ctx wid
  mrect <- scrollHitRect ctx wid
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
finalizeTextInputFocus :: Context -> Input -> PressTargets -> IO ()
finalizeTextInputFocus ctx inp targets =
  when (inputMousePressed inp) $ do
    prevFocus <- readIORef (ctxFocusId ctx)
    mFocused <- enabledTarget ctx (ptTextField targets)
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
enabledTarget ctx mWid = case mWid of
  Just wid -> ifM (isDisabled ctx wid) (pure Nothing) (pure mWid)
  Nothing -> pure Nothing
