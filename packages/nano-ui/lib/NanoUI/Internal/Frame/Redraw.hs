-- | Frame-needed checks and hover probing used while the backend waits for input.
module NanoUI.Internal.Frame.Redraw
  ( needsRedraw
  , pointerDragActive
  , textFieldActive
  , floatingPanelActive
  , debugPanelOpen
  , probeHotId
  ) where

import Data.IORef (IORef, readIORef)
import Data.Maybe (isJust)
import NanoUI.Internal.Context
  ( Context (..)
  , anyAnimating
  , anySelectOpen
  , pointerHeldOffLayers
  , getsInteraction
  , getStore
  , InteractionState (..)
  , isDirty
  , isPointerTracked
  , modalActive
  )
import NanoUI.Internal.Frame.Hit (nodePointVisible, overlayHitAllowed, overlayHitRoot, withWidgetNode)
import NanoUI.Internal.Frame.Select (focusedComboNode, overlayMenuOwnerAt)
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Input (Input (..), inputInteracted, inputMousePos, inputPointerHeld)
import NanoUI.Internal.Layout.Arena
  ( NodeType (..)
  , findFloatingNodeRevM
  , NodeClass (PointerNodes)
  , findClassNodeM
  , floatingNodeCount
  , getNodeType
  , getWidgetId
  , isWidgetNode
  )
import NanoUI.Internal.Monad ((<&&>))
import NanoUI.Internal.Types (V2 (..))

-- | Whether state or input changes require a frame. Arguments are previous
-- and current input. Tests hover only after pointer motion; timed wake
-- deadlines are handled separately by the session runner.
needsRedraw :: Context -> Input -> Input -> IO Bool
needsRedraw ctx prev inp = do
  dirty <- isDirty ctx
  anim <- anyAnimating ctx
  mDrag <- getsInteraction ctx isScrollDrag
  mWinDrag <- getsInteraction ctx isWindowDrag
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
      pure moved <&&> do
        -- A widget that tracks the pointer wants every move over it;
        -- one that does not wants only the move that leaves it.
        lastHot <- readIORef (ctxLastHotId ctx)
        tracked <- if hashWidgetId lastHot == 0 then pure False else isPointerTracked ctx lastHot
        if tracked
          then pure True
          else (/= lastHot) <$> probeHotId ctx (inputMousePos inp)

-- | Whether a window, scrollbar, resize, slider, or colour-picker gesture
-- is active. Text-selection drags are tracked separately.
pointerDragActive :: Context -> IO Bool
pointerDragActive ctx = do
  winDrag <- isJust <$> getsInteraction ctx isWindowDrag
  scrollDrag <- isJust <$> getsInteraction ctx isScrollDrag
  winResize <- isJust <$> getsInteraction ctx isWindowResize
  sliderOrPicker <- focusedNodeIs ctx ctxActiveId (\nt -> nt == NodeSlider || nt == NodeColorPicker)
  pure (winDrag || scrollDrag || winResize || sliderOrPicker)

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
textFieldActive ctx = do
  menu <- getsInteraction ctx isTextInputMenu
  if isJust menu
    then pure True
    else focusedNodeIs ctx ctxFocusId (\nt -> nt == NodeTextInput || nt == NodeTextArea)

-- | Whether modal state or the current arena contains a floating panel,
-- including windows and popups.
floatingPanelActive :: Context -> IO Bool
floatingPanelActive ctx = do
  modal <- modalActive ctx
  floating <- floatingNodeCount (ctxNodeArena ctx)
  pure (modal || floating > 0)

-- | Whether the arena contains any floating window. The name does not imply
-- that its contents are a debug readout.
debugPanelOpen :: Context -> IO Bool
debugPanelOpen ctx =
  isJust <$> findFloatingNodeRevM (ctxNodeArena ctx) (fmap (== NodeWindow) . getNodeType (ctxNodeArena ctx))

probeHotId :: Context -> V2 -> IO WidgetId
probeHotId ctx mouse = do
  -- A button that went down on a menu or dropdown keeps everything cold.
  offLayers <- pointerHeldOffLayers ctx
  if offLayers
    then pure (WidgetId 0)
    else do
      mOverlay <- overlayMenuOwnerAt ctx mouse
      case mOverlay of
        Just wid -> pure wid
        -- Earlier siblings paint over later ones, so the first hit wins.
        Nothing -> do
          top <- overlayHitRoot ctx mouse
          let hits idx =
                (isWidgetNode <$> getNodeType na idx) <&&> nodePointVisible ctx idx mouse <&&> overlayHitAllowed ctx top idx
          maybe (pure (WidgetId 0)) (getWidgetId na) =<< findClassNodeM na PointerNodes hits
  where
    na = ctxNodeArena ctx
