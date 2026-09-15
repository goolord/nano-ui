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
import Data.IORef (readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Typeable (Typeable)
import Effectful (Eff, IOE, runEff, type (:>))
import NanoUI.Context
  ( Context (..)
  , armMenuPointerCapture
  , FrameMsg (..)
  , clearDirty
  , decodeMessages
  , drainMessages
  , getLastWindowSize
  , getLiveAnimations
  , getPrevFloatingRects
  , getPrevNodeTexts
  , getPrevRect
  , getPrevRects
  , getStore
  , isDirty
  , lookupPopupConfig
  , markDirty
  , pruneDrawOpCache
  , resetDrawingScopeCache
  , setMenuPointerGesture
  , setSelectDropPress
  , takeDamage
  , tickAnimations
  , lookupCustomMeasure
  , hasCustomLayoutInputs
  , ensureMetricCaches
  )
import NanoUI.Context (beginFrameModal)
import NanoUI.Damage (FrameSnapshot (..), updatePrevRects, writeDamage)
import NanoUI.Draw
  ( DrawArena
  , DrawData
  , Layer (..)
  , beginLayer
  , finishDraw
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
  ( finalizePointerPress
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
  ( cacheOpenSelectDrop
  , closeSelectOnOutsideClick
  , drawSelectOverlays
  , finalizeSelectKeyboard
  , finalizeSelectPick
  , markSelectDropPress
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
  ( lookupWindowPos
  , lookupWindowSize
  , persistWindowPositions
  , updateWindowDrag
  , updateWindowResize
  )
import NanoUI.Id (WidgetId (..), initialIdContext)
import NanoUI.Input (Input (..), inputMouseDown, stripInteractionInput)
import NanoUI.Layout.Arena
  ( captureLayoutCache
  , layoutCacheEligible
  , layoutInputsMatch
  , newLayoutCache
  , resetNodeArena
  , restoreLayoutCache
  )
import NanoUI.Layout.Solve (placeModals, placePopups, placeWindows, solveLayout)
import NanoUI.Monad (NanoUI, Ui, runUi)
import NanoUI.Store (mirrorStoresChanged)
import NanoUI.Types (Damage (..), Size (..), rectInflate)

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
runFrameEff unlift ctx inp ui = do
  ensureMetricCaches ctx
  oldHot <- readIORef (ctxLastHotId ctx)
  oldActive <- readIORef (ctxActiveId ctx)
  oldFocus <- readIORef (ctxFocusId ctx)
  oldHotRect <- getPrevRect ctx oldHot
  oldActiveRect <- getPrevRect ctx oldActive
  oldFocusRect <- getPrevRect ctx oldFocus
  oldFloatingRects <- getPrevFloatingRects ctx
  oldRects <- getPrevRects ctx
  oldTexts <- getPrevNodeTexts ctx
  oldSize <- getLastWindowSize ctx
  oldStore <- getStore ctx
  wasDirty <- isDirty ctx
  clearDirty ctx
  animKeys <- IM.keysSet <$> getLiveAnimations ctx
  -- Wheel and thumb-drag input targets the previous frame's layout, so apply
  -- it while that arena is still intact, before it is reset for the new
  -- build. Settling offsets before the UI pass keeps build-time
  -- virtualization (table body rows) materialized for the range that will
  -- actually be visible, without a second build pass.
  updateScrollWheel ctx inp
  updateScrollDrag ctx inp
  resetNodeArena (ctxNodeArena ctx)
  resetDrawArena (ctxDrawArena ctx)
  resetUiBuildScopes ctx
  unless (inputMouseDown inp) $
    setSelectDropPress ctx False
  when (not (inputMouseDown inp) && not (inputMouseReleased inp)) $
    setMenuPointerGesture ctx False
  beginFrameModal ctx
  writeIORef (ctxReleaseClickedId ctx) (WidgetId 0)
  armMenuPointerCapture ctx inp
  result0 <- unlift (runUi ctx inp ui)
  -- Pending click is one-shot. Clear before a mirror rebuild so toggles do not fire twice.
  writeIORef (ctxClickedId ctx) (WidgetId 0)
  storeMid <- getStore ctx
  result <-
    if mirrorStoresChanged oldStore storeMid
      then do
        resetUiBuild ctx
        unlift (runUi ctx (stripInteractionInput inp) ui)
      else pure result0
  -- Sync widget node values (checkbox/radio/tree) from the store before measure
  -- so labels and layout reflect the current state.
  syncWidgetLabels ctx
  let
    Size w h = inputWindowSize inp
  reused <- tryReuseLayout ctx (Size w h)
  unless reused $ do
    solvePlaceWindows ctx w h
    captureLayout ctx (Size w h)
  movedResize <- updateWindowResize ctx inp w h
  movedWindow <- updateWindowDrag ctx inp
  when (movedResize || movedWindow) $
    placeWindows
      (ctxNodeArena ctx)
      (ctxFontMetrics ctx)
      w
      h
      (lookupWindowPos ctx)
      (lookupWindowSize ctx)
  persistWindowPositions ctx
  applyScrollOffsets ctx
  finalizePointerPress ctx inp
  finalizePointerRelease ctx inp
  finalizeTextInputFocus ctx inp
  finalizeSelectFocus ctx inp
  finalizeTextFieldMouse ctx inp
  closeTextEditMenuOnOutsideClick ctx inp
  openTextEditMenu ctx inp
  finalizeTextEditMenuPick ctx inp
  closeTextEditMenuOnEscape ctx inp
  constrainFocusToModal ctx
  finalizeTabFocus ctx inp
  finalizeSelectKeyboard ctx inp
  markSelectDropPress ctx inp
  finalizeSelectPick ctx inp
  closeSelectOnOutsideClick ctx inp
  storeAfter <- getStore ctx
  let storeChanged = mirrorStoresChanged storeMid storeAfter
  when storeChanged $ syncWidgetLabels ctx
  let layoutDirty = storeChanged || movedResize || movedWindow
  when layoutDirty $ do
    solvePlaceWindows ctx w h
    captureLayout ctx (Size w h)
    applyScrollOffsets ctx
  cacheOpenSelectDrop ctx
  updatePrevRects ctx
  refreshHover ctx inp
  tickAnimations ctx (inputDeltaTime inp)
  pruneDrawOpCache ctx
  overlayOpen <- overlayMenuOpen ctx
  writeDamage ctx inp overlayOpen
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
  -- damage. Inflate by one logical pixel to cover the runner's outward pixel
  -- snap. Full-present frames (fresh retain, forced full, continuous) paint
  -- everything.
  paintFull <- readIORef (ctxPaintFull ctx)
  unless paintFull $
    caseDamage (ctxDrawArena ctx) =<< takeDamage ctx
  beginLayer (ctxDrawArena ctx) LayerBackground
  lowerShapes ctx
  beginLayer (ctxDrawArena ctx) LayerOverlay
  drawWindowOverlays ctx
  drawModalOverlays ctx (inputWindowSize inp)
  drawPopupOverlays ctx
  drawSelectOverlays ctx inp
  drawTextEditMenuOverlays ctx inp
  drawData <- finishDraw (ctxDrawArena ctx)
  msgs <- drainMessages ctx
  dirtyAfterUi <- isDirty ctx
  pure (result, msgs, drawData, dirtyAfterUi)

-- Second UI pass after mirror store write. Keeps ctxStore, animations, and
-- prev rects; only rebuilds node arena and id scopes.
resetUiBuild :: Context -> IO ()
resetUiBuild ctx = do
  resetNodeArena (ctxNodeArena ctx)
  resetUiBuildScopes ctx

caseDamage :: DrawArena -> Damage -> IO ()
caseDamage _ DamageFull = pure ()
caseDamage da (DamageClip r) = setClip da (rectInflate 1 r)

resetUiBuildScopes :: Context -> IO ()
resetUiBuildScopes ctx = do
  writeIORef (ctxContainerStack ctx) []
  writeIORef (ctxIdContext ctx) initialIdContext
  writeIORef (ctxFocusablesCount ctx) 0
  writeIORef (ctxHotId ctx) (WidgetId 0)
  resetDrawingScopeCache ctx

solvePlaceWindows :: Context -> Float -> Float -> IO ()
solvePlaceWindows ctx w h = do
  let fontResolver sz weight style var = do
        (fm, _) <- ctxResolveFont ctx sz weight style var
        pure (fm, ctxResolveMeasure ctx sz weight style var)
  solveLayout
    (ctxNodeArena ctx)
    (ctxFontMetrics ctx)
    (ctxMonoFontMetrics ctx)
    (ctxMeasureText ctx)
    fontResolver
    (lookupCustomMeasure ctx)
    w
    h
  placeModals (ctxNodeArena ctx) (ctxFontMetrics ctx) w h
  placeWindows
    (ctxNodeArena ctx)
    (ctxFontMetrics ctx)
    w
    h
    (lookupWindowPos ctx)
    (lookupWindowSize ctx)
  placePopups
    (ctxNodeArena ctx)
    (ctxFontMetrics ctx)
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
