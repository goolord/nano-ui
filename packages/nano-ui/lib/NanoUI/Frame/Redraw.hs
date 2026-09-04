{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.Redraw
  ( needsRedraw
  , needsRedrawIdle
  , pointerDragActive
  , textFieldActive
  , floatingPanelActive
  , debugPanelOpen
  , overlayMenuOpen
  , hoverWouldChange
  , probeHotId
  ) where


import Data.IORef (readIORef)
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
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), inputInteracted, inputMousePos, inputPointerHeld)
import NanoUI.Layout.Arena
  ( NodeType (..)
  , arenaCount
  , foldNodeRevM
  , getNodeType
  , getWidgetId
  , isFloatingNode
  , isWidgetNode
  )
import NanoUI.Types (V2 (..))
import NanoUI.Frame.Hit (findNodeByWidgetId, overlayHitAllowed, nodePointVisible)
import NanoUI.Frame.Select (overlayMenuOwnerAt)

needsRedraw :: Context -> Input -> Input -> IO Bool
needsRedraw = needsRedrawBody

-- Terminal keeps the last blit while idle. SDL debug HUD refresh is
-- rate-limited in the session (`takeDebugLive`), not by this predicate.
needsRedrawIdle :: Context -> Input -> Input -> IO Bool
needsRedrawIdle = needsRedrawBody

-- Window/scroll/resize drag marks dirty every frame. TUI must still poll input then.
-- Color picker and slider hold ctxActiveId without extra window/scroll refs.
pointerDragActive :: Context -> IO Bool
pointerDragActive ctx = do
  winDrag <- isJust <$> getWindowDrag ctx
  scrollDrag <- isJust <$> getScrollDrag ctx
  winResize <- isJust <$> getWindowResize ctx
  sliderOrPicker <- widgetDragActive ctx
  pure (winDrag || scrollDrag || winResize || sliderOrPicker)

widgetDragActive :: Context -> IO Bool
widgetDragActive ctx = do
  active <- readIORef (ctxActiveId ctx)
  if hashWidgetId active == 0
    then pure False
    else do
      mIdx <- findNodeByWidgetId ctx active
      case mIdx of
        Nothing -> pure False
        Just idx -> do
          nt <- getNodeType (ctxNodeArena ctx) idx
          pure (nt == NodeSlider || nt == NodeColorPicker)

needsRedrawBody :: Context -> Input -> Input -> IO Bool
needsRedrawBody ctx prev inp = do
  dirty <- isDirty ctx
  anim <- anyAnimating ctx
  hover <- hoverWouldChange ctx inp
  mDrag <- getScrollDrag ctx
  mWinDrag <- getWindowDrag ctx
  overlay <- overlayMenuOpen ctx
  edit <- textFieldActive ctx
  let overlayMove = overlay && inputMousePos prev /= inputMousePos inp
  pure
    ( dirty
        || anim
        || inputInteracted prev inp
        || inputPointerHeld inp
        || hover
        || isJust mDrag
        || isJust mWinDrag
        || overlayMove
        || edit
    )

-- Select dropdown or text-input menu is open. Overlay hover is not a widget id.
overlayMenuOpen :: Context -> IO Bool
overlayMenuOpen ctx = do
  store <- getStore ctx
  menu <- getTextInputMenu ctx
  pure (anySelectOpen store || isJust menu)

-- Focused text field or its context menu. Keep the loop live so typed bytes
-- are not stuck behind SDL_WaitEvent.
textFieldActive :: Context -> IO Bool
textFieldActive ctx = do
  menu <- getTextInputMenu ctx
  if isJust menu
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
              pure (nt == NodeTextInput || nt == NodeTextArea)

-- Last frame still has a floating node (modal or window). Used by backends to
-- decide whether overlay content might need periodic refresh (debug HUD).
floatingPanelActive :: Context -> IO Bool
floatingPanelActive ctx = do
  modal <- modalActive ctx
  if modal
    then pure True
    else do
      count <- arenaCount (ctxNodeArena ctx)
      let go idx
            | idx >= count = pure False
            | otherwise = do
                nt <- getNodeType (ctxNodeArena ctx) idx
                if isFloatingNode nt
                  then pure True
                  else go (idx + 1)
      go 0

-- Floating window overlay (debug HUD). Prev floating rects persist across idle frames.
debugPanelOpen :: Context -> IO Bool
debugPanelOpen ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  let go idx
        | idx >= count = pure False
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            if nt == NodeWindow
              then pure True
              else go (idx + 1)
  go 0

hoverWouldChange :: Context -> Input -> IO Bool
hoverWouldChange ctx inp = do
  lastHot <- readIORef (ctxLastHotId ctx)
  nextHot <- probeHotId ctx (inputMousePos inp)
  pure (nextHot /= lastHot)

probeHotId :: Context -> V2 -> IO WidgetId
probeHotId ctx mouse = do
  gesture <- getMenuPointerGesture ctx
  if gesture
    then pure (WidgetId 0)
    else do
      mOverlay <- overlayMenuOwnerAt ctx mouse
      case mOverlay of
        Just wid -> pure wid
        Nothing ->
          foldNodeRevM (ctxNodeArena ctx) updateHot (WidgetId 0)
  where
    updateHot acc idx = do
      nt <- getNodeType (ctxNodeArena ctx) idx
      if not (isWidgetNode nt)
        then pure acc
        else do
          wid <- getWidgetId (ctxNodeArena ctx) idx
          visible <- nodePointVisible ctx idx mouse
          if visible
            then do
              allow <- overlayHitAllowed ctx idx mouse
              pure (if allow then wid else acc)
            else pure acc

