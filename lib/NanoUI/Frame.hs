{-# LANGUAGE DataKinds #-}

module NanoUI.Frame
  ( runFrame
  , runFrameEff
  , runFrameReduce
  , runFrameReduceEff
  , needsRedraw
  , needsRedrawIdle
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
  , animInProgress
  , clearPopupConfigs
  , decodeMessages
  , drainMessages
  , getPrevRect
  , getStore
  , isDirty
  , lookupPopupConfig
  , markDirty
  , tickAnimations
  )
import NanoUI.Context (beginFrameModal)
import NanoUI.Damage (updatePrevRects, writeDamage)
import NanoUI.Draw
  ( DrawData
  , Layer (..)
  , beginLayer
  , finishDraw
  , resetDrawArena
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
  , finalizeTextInputMouse
  , refreshHover
  )
import NanoUI.Frame.Focus (constrainFocusToModal, syncWidgetLabels)
import NanoUI.Frame.Paint (lowerShapes)
import NanoUI.Frame.Redraw
  ( debugPanelOpen
  , floatingPanelActive
  , needsRedraw
  , needsRedrawIdle
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
import NanoUI.Frame.TextInput
  ( closeTextInputMenuOnEscape
  , closeTextInputMenuOnOutsideClick
  , drawTextInputMenuOverlays
  , finalizeTextInputMenuPick
  , openTextInputMenu
  )
import NanoUI.Frame.Window
  ( drawModalOverlays
  , drawPopupOverlays
  , drawWindowOverlays
  , lookupWindowPos
  , lookupWindowSize
  , persistWindowPositions
  , updateWindowDrag
  , updateWindowResize
  )
import NanoUI.Id (WidgetId (..), initialIdContext)
import NanoUI.Input (Input (..), inputMouseDown, stripInteractionInput)
import NanoUI.Layout.Arena (resetNodeArena)
import NanoUI.Layout.Solve (placeModals, placePopups, placeWindows, solveLayout)
import NanoUI.Monad (NanoUI, Ui, runUi)
import NanoUI.Store (mirrorStoresChanged)
import NanoUI.Types (Size (..))

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
  oldHot <- readIORef (ctxLastHotId ctx)
  oldActive <- readIORef (ctxActiveId ctx)
  oldFocus <- readIORef (ctxFocusId ctx)
  oldHotRect <- getPrevRect ctx oldHot
  oldActiveRect <- getPrevRect ctx oldActive
  oldFocusRect <- getPrevRect ctx oldFocus
  oldFloatingRects <- readIORef (ctxPrevFloatingRects ctx)
  oldRects <- readIORef (ctxPrevRects ctx)
  oldSize <- readIORef (ctxLastWindowSize ctx)
  oldStore <- getStore ctx
  wasDirty <- isDirty ctx
  writeIORef (ctxDirty ctx) False
  animKeys <- IM.keys . IM.filter animInProgress <$> readIORef (ctxAnimations ctx)
  resetNodeArena (ctxNodeArena ctx)
  resetDrawArena (ctxDrawArena ctx)
  resetUiBuildScopes ctx
  unless (inputMouseDown inp) $
    writeIORef (ctxSelectDropPress ctx) False
  when (not (inputMouseDown inp) && not (inputMouseReleased inp)) $
    writeIORef (ctxMenuPointerGesture ctx) False
  beginFrameModal ctx
  writeIORef (ctxEscapeConsumed ctx) False
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
  -- Terminal sliders embed the bar in node text; sync before measure so width is correct.
  syncWidgetLabels ctx
  let
    Size w h = inputWindowSize inp
  solvePlaceWindows ctx w h
  movedResize <- updateWindowResize ctx inp w h
  movedWindow <- updateWindowDrag ctx inp
  when (movedResize || movedWindow) $
    placeWindows
      (ctxNodeArena ctx)
      (ctxHostProfile ctx)
      (ctxFontMetrics ctx)
      w
      h
      (lookupWindowPos ctx)
      (lookupWindowSize ctx)
  persistWindowPositions ctx
  updateScrollWheel ctx inp
  updateScrollDrag ctx inp
  applyScrollOffsets ctx
  finalizePointerPress ctx inp
  finalizePointerRelease ctx inp
  finalizeTextInputFocus ctx inp
  finalizeSelectFocus ctx inp
  finalizeTextInputMouse ctx inp
  closeTextInputMenuOnOutsideClick ctx inp
  openTextInputMenu ctx inp
  finalizeTextInputMenuPick ctx inp
  closeTextInputMenuOnEscape ctx inp
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
    applyScrollOffsets ctx
  cacheOpenSelectDrop ctx
  updatePrevRects ctx
  refreshHover ctx inp
  tickAnimations ctx (inputDeltaTime inp)
  beginLayer (ctxDrawArena ctx) LayerBackground
  lowerShapes ctx
  beginLayer (ctxDrawArena ctx) LayerOverlay
  drawWindowOverlays ctx
  drawModalOverlays ctx (inputWindowSize inp)
  drawPopupOverlays ctx
  drawSelectOverlays ctx inp
  drawTextInputMenuOverlays ctx inp
  drawData <- finishDraw (ctxDrawArena ctx)
  overlayOpen <- overlayMenuOpen ctx
  writeDamage
    ctx
    inp
    wasDirty
    overlayOpen
    oldSize
    oldStore
    oldHot
    oldActive
    oldFocus
    oldHotRect
    oldActiveRect
    oldFocusRect
    oldFloatingRects
    oldRects
    animKeys
  msgs <- drainMessages ctx
  dirtyAfterUi <- isDirty ctx
  writeIORef (ctxDirty ctx) dirtyAfterUi
  pure (result, msgs, drawData, dirtyAfterUi)

-- Second UI pass after mirror store write. Keeps ctxStore, animations, and
-- prev rects; only rebuilds node arena and id scopes.
resetUiBuild :: Context -> IO ()
resetUiBuild ctx = do
  resetNodeArena (ctxNodeArena ctx)
  resetUiBuildScopes ctx

resetUiBuildScopes :: Context -> IO ()
resetUiBuildScopes ctx = do
  writeIORef (ctxContainerStack ctx) []
  writeIORef (ctxIdContext ctx) initialIdContext
  writeIORef (ctxFocusablesCount ctx) 0
  writeIORef (ctxHotId ctx) (WidgetId 0)
  writeIORef (ctxWidgetNodeTypes ctx) Nothing
  writeIORef (ctxFloatingAncestor ctx) Nothing
  clearPopupConfigs ctx

solvePlaceWindows :: Context -> Float -> Float -> IO ()
solvePlaceWindows ctx w h = do
  solveLayout
    (ctxNodeArena ctx)
    (ctxHostProfile ctx)
    (ctxFontMetrics ctx)
    (ctxMeasureText ctx)
    w
    h
  placeModals (ctxNodeArena ctx) (ctxHostProfile ctx) (ctxFontMetrics ctx) w h
  placeWindows
    (ctxNodeArena ctx)
    (ctxHostProfile ctx)
    (ctxFontMetrics ctx)
    w
    h
    (lookupWindowPos ctx)
    (lookupWindowSize ctx)
  placePopups
    (ctxNodeArena ctx)
    (ctxHostProfile ctx)
    (ctxFontMetrics ctx)
    w
    h
    (lookupPopupConfig ctx)
