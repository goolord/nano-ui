{-# LANGUAGE DataKinds #-}

module NanoUI.Frame
  ( runFrame
  , runFrameEff
  , runFrameReduce
  , runFrameReduceEff
  , needsRedraw
  , pointerDragActive
  , textFieldActive
  , floatingPanelActive
  , debugPanelOpen
  , collectTextSpans
  , collectOverlayTextSpans
  , collectRasterSpans
  , widgetNodeCount
  , pointerCursorWanted
  , cursorKindIs
  , uiCursorKind
  , UiCursorKind (..)
  )
where

import Control.Monad (unless, when)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Typeable (Typeable)
import Effectful (Eff, IOE, runEff, type (:>))
import NanoUI.Context
  ( Context (..)
  , PointerRoute (..)
  , beginThemeScopes
  , damageFull
  , themeScopesChanged
  , FrameMsg (..)
  , clearDirty
  , clearWakeAt
  , decodeMessages
  , drainMessages
  , getLiveAnimations
  , getPrevRect
  , getStore
  , isDirty
  , lookupPopupConfig
  , markDirty
  , pruneDrawOpCache
  , resetDrawingScopeCache
  , stepScrollGlides
  , takeDamage
  , tickAnimations
  , hasCustomLayoutInputs
  , ensureMetricCaches
  , getsOverlay
  , OverlayState (..)
  , getsDamage
  , DamageState (..)
  )
import NanoUI.Context (beginFrameModal)
import NanoUI.Damage (FrameSnapshot (..), updatePrevRects, writeDamage)
import NanoUI.Draw
  ( DrawData
  , Layer (..)
  , beginLayer
  , finishDraw
  , pushRect
  , resetDrawArena
  , setClip
  )
import NanoUI.Frame.Cursor
  ( UiCursorKind (..)
  , cursorKindIs
  , pointerCursorWanted
  , uiCursorKind
  )
import NanoUI.Frame.Input
  ( armPointerPress
  , disarmPointerPress
  , finalizePointerPress
  , finalizePointerRelease
  , finalizeSelectFocus
  , finalizeTabFocus
  , finalizeTextInputFocus
  , refreshHover
  )
import NanoUI.Frame.Focus (constrainFocusToModal, syncWidgetLabels)
import NanoUI.Frame.Paint (lowerShapes)
import NanoUI.Frame.Redraw
  ( debugPanelOpen
  , floatingPanelActive
  , needsRedraw
  , overlayMenuOpen
  , pointerDragActive
  , textFieldActive
  )
import NanoUI.Frame.Scroll
  ( applyScrollOffsets
  , updateScrollDrag
  , updateScrollWheel
  )
import NanoUI.Frame.Select
  ( closeSelectOnOutsideClick
  , drawSelectOverlays
  , finalizeSelectKeyboard
  , finalizeSelectPick
  , routePointer
  )
import NanoUI.Frame.Spans
  ( collectOverlayTextSpans
  , collectRasterSpans
  , collectTextSpans
  , widgetNodeCount
  )
import NanoUI.Frame.Overlay (drawModalOverlays, drawPopupOverlays, drawWindowOverlays)
import NanoUI.Frame.TextEdit (finalizeTextFieldMouse)
import NanoUI.Frame.TextEdit.Menu
  ( closeTextEditMenuOnEscape
  , closeTextEditMenuOnOutsideClick
  , drawTextEditMenuOverlays
  , finalizeTextEditMenuPick
  , openTextEditMenu
  )
import NanoUI.Frame.Window
  ( contextMeasurers
  , lookupWindowPos
  , lookupWindowSize
  , persistWindowPositions
  , updateWindowDrag
  , updateWindowResize
  )
import NanoUI.Id (WidgetId (..), initialIdContext)
import NanoUI.Input (Input (..), inputMousePressed, stripInteractionInput, withoutPointer)
import NanoUI.Layout.Arena
  ( captureLayoutCache
  , layoutCacheEligible
  , layoutInputsMatch
  , newLayoutCache
  , resetNodeArena
  , restoreLayoutCache
  )
import NanoUI.Layout.Solve (placeModals, placePopups, placeWindows, solveLayout)
import NanoUI.Monad (NanoUI, Ui, runUi, whenM)
import NanoUI.Store (mirrorStoresChanged)
import NanoUI.Style (Theme (..))
import NanoUI.Types (Damage (..), Size (..), rectInflate, rectNonEmpty)

runFrame :: Context -> Input -> NanoUI a -> IO (a, [FrameMsg], DrawData, Bool)
runFrame = runFrameEff runEff

-- View this model, then apply decoded messages at frame end.
-- DrawData is from the pre-reduce model (one-frame lag). The idle
-- loop redraws when the reduced model differs.
runFrameReduce ::
  (Typeable msg, Eq model) =>
  (msg -> model -> model)
  -> Context
  -> Input
  -> model
  -> (model -> NanoUI a)
  -> IO (a, model, [msg], DrawData, Bool)
runFrameReduce = runFrameReduceEff runEff

runFrameReduceEff ::
  (IOE :> es, Typeable msg, Eq model) =>
  (forall x. Eff es x -> IO x)
  -> (msg -> model -> model)
  -> Context
  -> Input
  -> model
  -> (model -> Eff (Ui : es) a)
  -> IO (a, model, [msg], DrawData, Bool)
runFrameReduceEff unlift update ctx inp model view = do
  (a, msgs, draw, dirty) <- runFrameEff unlift ctx inp (view model)
  let
    typed = decodeMessages msgs
    model' = foldl' (flip update) model typed
  when (model' /= model) (markDirty ctx)
  dirty' <- isDirty ctx
  pure (a, model', typed, draw, dirty || dirty')

runFrameEff ::
  IOE :> es =>
  (forall x. Eff es x -> IO x)
  -> Context
  -> Input
  -> Eff (Ui : es) a
  -> IO (a, [FrameMsg], DrawData, Bool)
runFrameEff unlift ctx frameInp ui = do
  ensureMetricCaches ctx
  oldHot <- readIORef (ctxLastHotId ctx)
  oldActive <- readIORef (ctxActiveId ctx)
  oldFocus <- readIORef (ctxFocusId ctx)
  oldHotRect <- getPrevRect ctx oldHot
  oldActiveRect <- getPrevRect ctx oldActive
  oldFocusRect <- getPrevRect ctx oldFocus
  oldFloatingRects <- getsOverlay ctx osPrevFloatingRects
  oldRects <- getsDamage ctx dsPrevRects
  oldTexts <- getsDamage ctx dsPrevNodeTexts
  oldSize <- getsDamage ctx dsLastWindowSize
  oldStore <- getStore ctx
  wasDirty <- isDirty ctx
  clearDirty ctx
  -- Timed wakes are re-requested by whatever is still built this frame.
  clearWakeAt ctx
  animKeys <- IM.keysSet <$> getLiveAnimations ctx
  -- Decide what the pointer belongs to before anything reads it, against the
  -- frame the user saw. The view gets its input routed layer by layer, and
  -- each step below gets the input of what it serves, with no pointer in it
  -- unless that is where the pointer went, so none of them has to ask:
  -- @layerInp@ for scrolling, windows, presses, focus and text fields,
  -- @menuInp@ for the text-edit menu, and @dropInp@ for the dropdowns, which
  -- only that menu is drawn over. @frameInp@, the pointer whoever it belongs
  -- to, is for what watches the whole window: a press anywhere else closing
  -- a menu, hover, and what the overlays paint.
  route <- routePointer ctx frameInp
  let routedIf mine = if mine then frameInp else withoutPointer frameInp
      layerInp = routedIf (case route of RouteLayer _ -> True; _ -> False)
      menuInp = routedIf (route == RouteTextMenu)
      dropInp = routedIf (route /= RouteTextMenu)
  -- Wheel and thumb-drag input targets the previous frame's layout, so apply
  -- it while that arena is still intact, before it is reset for the new
  -- build. Settling offsets before the UI pass keeps build-time
  -- virtualization (table body rows) materialized for the range that will
  -- actually be visible, without a second build pass.
  updateScrollWheel ctx layerInp
  -- A glide advances with the wheel, before the build, for the same reason:
  -- the offset this frame renders at is the one virtualization must see.
  stepScrollGlides ctx (inputDeltaTime frameInp)
  updateScrollDrag ctx layerInp
  beginThemeScopes ctx True
  resetNodeArena (ctxNodeArena ctx)
  resetDrawArena (ctxDrawArena ctx)
  resetUiBuildScopes ctx
  beginFrameModal ctx
  writeIORef (ctxReleaseClickedId ctx) (WidgetId 0)
  armPointerPress ctx frameInp
  result0 <- unlift (runUi ctx frameInp ui)
  -- Pending click is one-shot. Clear before a mirror rebuild so toggles do not fire twice.
  writeIORef (ctxClickedId ctx) (WidgetId 0)
  storeMid <- getStore ctx
  result <-
    if mirrorStoresChanged oldStore storeMid
      then do
        resetUiBuild ctx
        unlift (runUi ctx (stripInteractionInput frameInp) ui)
      else pure result0
  -- Scopes only change how nodes look, which the rect and text diffs below
  -- cannot see, and custom widgets' cached ops hold the old theme's colours.
  whenM (themeScopesChanged ctx) $ do
    damageFull ctx
    modifyIORef' (ctxMetricGen ctx) (+ 1)
  -- Sync widget node values (checkbox/radio/tree) from the store before measure
  -- so labels and layout reflect the current state.
  syncWidgetLabels ctx
  let
    Size w h = inputWindowSize frameInp
  reused <- tryReuseLayout ctx (Size w h)
  unless reused $ do
    solvePlaceWindows ctx w h
    captureLayout ctx (Size w h)
  movedResize <- updateWindowResize ctx layerInp w h
  movedWindow <- updateWindowDrag ctx layerInp
  when (movedResize || movedWindow) $
    placeWindows
      (ctxNodeArena ctx)
      (contextMeasurers ctx)
      w
      h
      (lookupWindowPos ctx)
      (lookupWindowSize ctx)
  persistWindowPositions ctx
  applyScrollOffsets ctx
  -- A press on a menu or dropdown leaves nothing active, whatever a release
  -- that never arrived left behind.
  when (inputMousePressed frameInp && not (inputMousePressed layerInp)) $
    writeIORef (ctxActiveId ctx) (WidgetId 0)
  finalizePointerPress ctx layerInp
  finalizePointerRelease ctx layerInp
  disarmPointerPress ctx frameInp
  finalizeTextInputFocus ctx layerInp
  finalizeSelectFocus ctx layerInp
  finalizeTextFieldMouse ctx layerInp
  closeTextEditMenuOnOutsideClick ctx frameInp
  openTextEditMenu ctx layerInp
  finalizeTextEditMenuPick ctx menuInp
  closeTextEditMenuOnEscape ctx frameInp
  constrainFocusToModal ctx
  finalizeTabFocus ctx frameInp
  finalizeSelectKeyboard ctx frameInp
  finalizeSelectPick ctx dropInp
  closeSelectOnOutsideClick ctx frameInp
  storeAfter <- getStore ctx
  let storeChanged = mirrorStoresChanged storeMid storeAfter
  when storeChanged $ syncWidgetLabels ctx
  let layoutDirty = storeChanged || movedResize || movedWindow
  when layoutDirty $ do
    solvePlaceWindows ctx w h
    captureLayout ctx (Size w h)
    applyScrollOffsets ctx
  updatePrevRects ctx
  refreshHover ctx frameInp
  tickAnimations ctx (inputDeltaTime frameInp)
  pruneDrawOpCache ctx
  overlayOpen <- overlayMenuOpen ctx
  writeDamage ctx frameInp overlayOpen
    FrameSnapshot
      { fsWasDirty = wasDirty
      , fsSize = oldSize
      , fsStore = oldStore
      , fsHot = oldHot
      , fsActive = oldActive
      , fsFocus = oldFocus
      , fsHotRect = oldHotRect
      , fsActiveRect = oldActiveRect
      , fsFocusRect = oldFocusRect
      , fsFloatingRects = oldFloatingRects
      , fsRects = oldRects
      , fsTexts = oldTexts
      , fsAnimKeys = animKeys
      }
  -- Clip frames only repaint the damaged region: the retain texture already
  -- holds every other pixel, and the runner scissors the present to the same
  -- damage. The region repaints from the window backdrop, inflated by one
  -- logical pixel to cover the runner's outward pixel snap. Full-present
  -- frames (fresh retain, forced full, continuous) paint everything.
  paintFull <- readIORef (ctxPaintFull ctx)
  beginLayer (ctxDrawArena ctx) LayerBackground
  unless paintFull $
    paintDamageClip ctx =<< takeDamage ctx
  lowerShapes ctx
  beginLayer (ctxDrawArena ctx) LayerOverlay
  drawWindowOverlays ctx
  drawModalOverlays ctx (inputWindowSize frameInp)
  drawPopupOverlays ctx
  drawSelectOverlays ctx frameInp
  drawTextEditMenuOverlays ctx frameInp
  drawData <- finishDraw (ctxDrawArena ctx)
  msgs <- drainMessages ctx
  dirtyAfterUi <- isDirty ctx
  pure (result, msgs, drawData, dirtyAfterUi)

-- Second UI pass after mirror store write. Keeps ctxStore, animations, and
-- prev rects; only rebuilds node arena and id scopes.
resetUiBuild :: Context -> IO ()
resetUiBuild ctx = do
  beginThemeScopes ctx False
  resetNodeArena (ctxNodeArena ctx)
  resetUiBuildScopes ctx

-- | Start a clip frame from the window backdrop, as a full frame starts from a
-- window-coloured clear. Widgets with a transparent fill, such as an idle
-- menu-bar title, draw nothing over the pixels they covered, so without the
-- backdrop a hover that just ended would stay in the retain texture.
paintDamageClip :: Context -> Damage -> IO ()
paintDamageClip _ DamageFull = pure ()
paintDamageClip ctx (DamageClip r) = do
  let da = ctxDrawArena ctx
      clip = rectInflate 1 r
  setClip da clip
  when (rectNonEmpty r) $ do
    theme <- readIORef (ctxTheme ctx)
    pushRect da clip (themeWindow theme)

resetUiBuildScopes :: Context -> IO ()
resetUiBuildScopes ctx = do
  writeIORef (ctxContainerStack ctx) []
  writeIORef (ctxIdContext ctx) initialIdContext
  writeIORef (ctxFocusablesCount ctx) 0
  writeIORef (ctxHotId ctx) (WidgetId 0)
  resetDrawingScopeCache ctx

solvePlaceWindows :: Context -> Float -> Float -> IO ()
solvePlaceWindows ctx w h = do
  let ms = contextMeasurers ctx
  solveLayout (ctxNodeArena ctx) ms w h
  placeModals (ctxNodeArena ctx) ms w h
  placeWindows
    (ctxNodeArena ctx)
    ms
    w
    h
    (lookupWindowPos ctx)
    (lookupWindowSize ctx)
  placePopups
    (ctxNodeArena ctx)
    ms
    w
    h
    (lookupPopupConfig ctx)

-- | Reuse solved geometry for unchanged layout inputs. Floating placement and
-- custom measurement have dependencies outside the arena and must be solved.
tryReuseLayout :: Context -> Size -> IO Bool
tryReuseLayout ctx size = do
  custom <- hasCustomLayoutInputs ctx
  if custom
    then pure False
    else do
      gen <- readIORef (ctxMetricGen ctx)
      mc <- readIORef (ctxLayoutCache ctx)
      case mc of
        Just (c, cachedSize, cachedGen)
          | cachedSize == size && cachedGen == gen -> do
              ok <- layoutInputsMatch (ctxNodeArena ctx) c
              if ok
                then restoreLayoutCache (ctxNodeArena ctx) c >> pure True
                else pure False
        _ -> pure False

-- | Snapshot the solved layout so the next frame can reuse it.
captureLayout :: Context -> Size -> IO ()
captureLayout ctx size = do
  custom <- hasCustomLayoutInputs ctx
  eligible <- if custom then pure False else layoutCacheEligible (ctxNodeArena ctx)
  if not eligible
    then writeIORef (ctxLayoutCache ctx) Nothing
    else do
      gen <- readIORef (ctxMetricGen ctx)
      mc <- readIORef (ctxLayoutCache ctx)
      c0 <- case mc of
        Just (c, _, _) -> pure c
        Nothing -> newLayoutCache 64
      c <- captureLayoutCache (ctxNodeArena ctx) c0
      writeIORef (ctxLayoutCache ctx) (Just (c, size, gen))
