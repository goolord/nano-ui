{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
  , overlayMenuOwnerAt
  , openSelectOwnerAt
  ) where


import Data.IORef (readIORef)
import Data.Maybe (isJust)
import NanoUI.Context
  ( Context (..)
  , TextInputMenu (..)
  , anyAnimating
  , anySelectOpen
  , getStore
  , intKey
  , isDirty
  , isSelectOpen
  )
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), inputInteracted, inputMousePos, inputPointerHeld)
import NanoUI.Layout.Arena
  ( NodeType (..)
  , arenaCount
  , foldNodeRevM
  , getNodeType
  , getRect
  , getText
  , getWidgetId
  , isFloatingNode
  , isWidgetNode
  )
import NanoUI.Types (V2 (..), rectContains)
import NanoUI.WidgetText (selectOptions)
import NanoUI.Frame.Hit (findNodeByWidgetId, overlayHitAllowed, nodePointVisible)
import NanoUI.Frame.Select (selectDropRect)

needsRedraw :: Context -> Input -> Input -> IO Bool
needsRedraw = needsRedraw' True

-- Terminal keeps the last blit while idle; SDL windows tick live for damage.
needsRedrawIdle :: Context -> Input -> Input -> IO Bool
needsRedrawIdle = needsRedraw' False

-- Window/scroll/resize drag marks dirty every frame. TUI must still poll input then.
-- Color picker and slider hold ctxActiveId without extra window/scroll refs.
pointerDragActive :: Context -> IO Bool
pointerDragActive ctx = do
  winDrag <- isJust <$> readIORef (ctxWindowDrag ctx)
  scrollDrag <- isJust <$> readIORef (ctxScrollDrag ctx)
  winResize <- isJust <$> readIORef (ctxWindowResize ctx)
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

needsRedraw' :: Bool -> Context -> Input -> Input -> IO Bool
needsRedraw' includeLive ctx prev inp = do
  dirty <- isDirty ctx
  anim <- anyAnimating ctx
  hover <- hoverWouldChange ctx inp
  mDrag <- readIORef (ctxScrollDrag ctx)
  mWinDrag <- readIORef (ctxWindowDrag ctx)
  overlay <- overlayMenuOpen ctx
  edit <- textFieldActive ctx
  winLive <- debugPanelOpen ctx
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
        || (includeLive && winLive)
    )

-- Select dropdown or text-input menu is open. Overlay hover is not a widget id.
overlayMenuOpen :: Context -> IO Bool
overlayMenuOpen ctx = do
  store <- getStore ctx
  menu <- readIORef (ctxTextInputMenu ctx)
  pure (anySelectOpen store || isJust menu)

overlayMenuOwnerAt :: Context -> V2 -> IO (Maybe WidgetId)
overlayMenuOwnerAt ctx mouse = do
  menu <- readIORef (ctxTextInputMenu ctx)
  case menu of
    Just m | rectContains (textInputMenuRect m) mouse ->
      pure (Just (textInputMenuWidget m))
    _ -> openSelectOwnerAt ctx mouse

openSelectOwnerAt :: Context -> V2 -> IO (Maybe WidgetId)
openSelectOwnerAt ctx mouse = do
  store <- getStore ctx
  count <- arenaCount (ctxNodeArena ctx)
  let go idx
        | idx >= count = pure Nothing
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            if nt /= NodeSelect
              then go (idx + 1)
              else do
                wid <- getWidgetId (ctxNodeArena ctx) idx
                let key = intKey wid
                if not (isSelectOpen store key)
                  then go (idx + 1)
                  else do
                    txt <- getText (ctxNodeArena ctx) idx
                    (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                    let (_, opts) = selectOptions txt
                        dropRect = selectDropRect (ctxHostProfile ctx) (ctxFontMetrics ctx) x y w h (length opts)
                    if rectContains dropRect mouse
                      then pure (Just wid)
                      else go (idx + 1)
  go 0

-- Focused text field or its context menu. Keep the loop live so typed bytes
-- are not stuck behind SDL_WaitEvent.
textFieldActive :: Context -> IO Bool
textFieldActive ctx = do
  menu <- readIORef (ctxTextInputMenu ctx)
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
              pure (nt == NodeTextInput)

-- Last frame still has a floating node (modal or window). Used by backends to
-- decide whether overlay content might need periodic refresh (debug HUD).
floatingPanelActive :: Context -> IO Bool
floatingPanelActive ctx = do
  modal <- readIORef (ctxModalActive ctx)
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

