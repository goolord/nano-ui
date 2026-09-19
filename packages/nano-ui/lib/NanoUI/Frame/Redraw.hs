{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.Redraw
  ( needsRedraw
  , pointerDragActive
  , textFieldActive
  , floatingPanelActive
  , debugPanelOpen
  , overlayMenuOpen
  , probeHotId
  ) where

import Data.IORef (IORef, readIORef)
import Data.Maybe (isJust)
import NanoUI.Context
  ( Context (..)
  , anyAnimating
  , anySelectOpen
  , getMenuPointerGesture
  , getScrollDrag
  , getStore
  , getTextInputMenu
  , getWindowDrag
  , getWindowResize
  , isDirty
  , modalActive
  )
import NanoUI.Frame.Hit (findNodeByWidgetId, nodePointVisible, overlayHitAllowed)
import NanoUI.Frame.Select (overlayMenuOwnerAt)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), inputInteracted, inputMousePos, inputPointerHeld)
import NanoUI.Layout.Arena
  ( NodeType (..)
  , findNodeM
  , getNodeType
  , getOptions
  , getWidgetId
  , isFloatingNode
  , isWidgetNode
  )
import NanoUI.Types (V2 (..))

needsRedraw :: Context -> Input -> Input -> IO Bool
needsRedraw ctx prev inp = do
  dirty <- isDirty ctx
  anim <- anyAnimating ctx
  mDrag <- getScrollDrag ctx
  mWinDrag <- getWindowDrag ctx
  overlay <- overlayMenuOpen ctx
  let moved = inputMousePos prev /= inputMousePos inp
  if dirty
    || anim
    || inputInteracted prev inp
    || inputWindowRedraw inp
    || inputPointerHeld inp
    || isJust mDrag
    || isJust mWinDrag
    || (overlay && moved)
    then pure True
    else
      -- Idle: hover can only change when the pointer moved since the frame
      -- whose hover state we still hold. Skip the O(n) hot probe otherwise.
      if not moved
        then pure False
        else do
          lastHot <- readIORef (ctxLastHotId ctx)
          (/= lastHot) <$> probeHotId ctx (inputMousePos inp)

-- Window/scroll/resize drag marks dirty every frame, so input must still be
-- polled on those frames.
-- Color picker and slider hold ctxActiveId without extra window/scroll refs.
pointerDragActive :: Context -> IO Bool
pointerDragActive ctx = do
  winDrag <- isJust <$> getWindowDrag ctx
  scrollDrag <- isJust <$> getScrollDrag ctx
  winResize <- isJust <$> getWindowResize ctx
  sliderOrPicker <- focusedNodeIs ctx ctxActiveId (\nt -> nt == NodeSlider || nt == NodeColorPicker)
  pure (winDrag || scrollDrag || winResize || sliderOrPicker)

-- | Whether the node of the widget id held in @ref@ satisfies @p@.
focusedNodeIs :: Context -> (Context -> IORef WidgetId) -> (NodeType -> Bool) -> IO Bool
focusedNodeIs ctx ref p = do
  wid <- readIORef (ref ctx)
  if hashWidgetId wid == 0
    then pure False
    else do
      mIdx <- findNodeByWidgetId ctx wid
      case mIdx of
        Nothing -> pure False
        Just idx -> p <$> getNodeType (ctxNodeArena ctx) idx

-- Select dropdown or text-input menu is open. Overlay hover is not a widget id.
-- A focused combo (a search-style field carrying options) also owns an open
-- dropdown: report it so every frame while it is up redraws with full damage:
-- the floating list is painted by an overlay, so clip-damage frames would
-- leave stale rows in the retained texture.
overlayMenuOpen :: Context -> IO Bool
overlayMenuOpen ctx = do
  store <- getStore ctx
  menu <- getTextInputMenu ctx
  if anySelectOpen store || isJust menu
    then pure True
    else do
      focus <- readIORef (ctxFocusId ctx)
      if hashWidgetId focus == 0
        then pure False
        else do
          mIdx <- findNodeByWidgetId ctx focus
          case mIdx of
            Nothing -> pure False
            Just idx -> do
              nt <- getNodeType (ctxNodeArena ctx) idx
              if nt /= NodeTextInput
                then pure False
                else not . null <$> getOptions (ctxNodeArena ctx) idx

-- Focused text field or its context menu. Typing reaches it as input events,
-- which wake the loop by themselves, so focus alone keeps nothing running.
textFieldActive :: Context -> IO Bool
textFieldActive ctx = do
  menu <- getTextInputMenu ctx
  if isJust menu
    then pure True
    else focusedNodeIs ctx ctxFocusId (\nt -> nt == NodeTextInput || nt == NodeTextArea)

-- Last frame still has a floating node (modal or window). Used by backends to
-- decide whether overlay content might need periodic refresh (debug HUD).
floatingPanelActive :: Context -> IO Bool
floatingPanelActive ctx = do
  modal <- modalActive ctx
  if modal
    then pure True
    else isJust <$> findNodeM (ctxNodeArena ctx) (fmap isFloatingNode . getNodeType (ctxNodeArena ctx))

-- Floating window overlay (debug HUD). Prev floating rects persist across idle frames.
debugPanelOpen :: Context -> IO Bool
debugPanelOpen ctx =
  isJust <$> findNodeM (ctxNodeArena ctx) (fmap (== NodeWindow) . getNodeType (ctxNodeArena ctx))

probeHotId :: Context -> V2 -> IO WidgetId
probeHotId ctx mouse = do
  gesture <- getMenuPointerGesture ctx
  if gesture
    then pure (WidgetId 0)
    else do
      mOverlay <- overlayMenuOwnerAt ctx mouse
      case mOverlay of
        Just wid -> pure wid
        -- Earlier siblings paint over later ones, so the first hit wins.
        Nothing -> maybe (pure (WidgetId 0)) (getWidgetId na) =<< findNodeM na hits
  where
    na = ctxNodeArena ctx
    hits idx = do
      nt <- getNodeType na idx
      if not (isWidgetNode nt)
        then pure False
        else do
          visible <- nodePointVisible ctx idx mouse
          if visible then overlayHitAllowed ctx idx mouse else pure False
