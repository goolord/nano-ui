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
  ) where

import Control.Monad (unless, when)
import Data.IORef (readIORef, writeIORef)
import Data.Typeable (Typeable)
import qualified Data.IntMap.Strict as IM
import Effectful (Eff, IOE, runEff, type (:>))
import NanoUI.Context
  ( Context (..)
  , FrameMsg (..)
  , decodeMessages
  , drainMessages
  , getStore
  , getPrevRect
  , isDirty
  , markDirty
  , clearMeasureCache
  , tickAnimations
  , animInProgress
  , clearTooltips
  )
import NanoUI.Context.Modal (beginFrameModal)
import NanoUI.Draw (DrawData, Layer (..), beginLayer, finishDraw, resetDrawArena)
import NanoUI.Id (WidgetId (..))
import NanoUI.Input (Input (..), inputMouseDown)
import NanoUI.Layout.Arena (resetNodeArena)
import NanoUI.Layout.Solve (placeModals, placeWindows, solveLayout)
import NanoUI.Monad (NanoUI, Ui, runUi)
import NanoUI.Damage (updatePrevRects, updatePrevNodeTexts, writeDamage)
import NanoUI.Types (Size (..))

import NanoUI.Frame.Cursor (UiCursorKind (..), cursorKindIs, pointerCursorWanted, uiCursorKind)
import NanoUI.Frame.Internal
  ( constrainFocusToModal
  , syncWidgetLabels
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
import NanoUI.Frame.Paint (drawTooltipOverlays, lowerShapes)
import NanoUI.Frame.Redraw
  ( debugPanelOpen
  , floatingPanelActive
  , needsRedraw
  , needsRedrawIdle
  , overlayMenuOpen
  , pointerDragActive
  , textFieldActive
  )
import NanoUI.Frame.Select
  ( closeSelectOnOutsideClick
  , finalizeSelectKeyboard
  , finalizeSelectPick
  , markSelectDropPress
  , drawSelectOverlays
  )
import NanoUI.Frame.Spans
  ( collectOverlayTextSpans
  , collectRasterSpans
  , collectTextSpans
  , widgetNodeCount
  )
import NanoUI.Frame.Scroll (applyScrollOffsets, updateScrollDrag, updateScrollWheel)
import NanoUI.Frame.TextInput
  ( closeTextInputMenuOnEscape
  , closeTextInputMenuOnOutsideClick
  , drawTextInputMenuOverlays
  , finalizeTextInputMenuPick
  , openTextInputMenu
  )
import NanoUI.Frame.Window
  ( lookupWindowPos
  , lookupWindowSize
  , persistWindowPositions
  , updateWindowDrag
  , updateWindowResize
  , drawModalOverlays
  , drawWindowOverlays
  )

runFrame :: Context -> Input -> NanoUI a -> IO (a, [FrameMsg], DrawData, Bool)
runFrame = runFrameEff runEff

-- View this model, then apply decoded messages at frame end.
-- DrawData is from the pre-reduce model (one-frame lag). The idle
-- loop redraws when the reduced model differs.
runFrameReduce ::
  (Typeable msg, Eq model) =>
  (msg -> model -> model) ->
  Context ->
  Input ->
  model ->
  (model -> NanoUI a) ->
  IO (a, model, [msg], DrawData, Bool)
runFrameReduce = runFrameReduceEff runEff

runFrameReduceEff ::
  (IOE :> es, Typeable msg, Eq model) =>
  (forall x. Eff es x -> IO x) ->
  (msg -> model -> model) ->
  Context ->
  Input ->
  model ->
  (model -> Eff (Ui : es) a) ->
  IO (a, model, [msg], DrawData, Bool)
runFrameReduceEff unlift update ctx inp model view = do
  (a, msgs, draw, dirty) <- runFrameEff unlift ctx inp (view model)
  let typed = decodeMessages msgs
      model' = foldl' (flip update) model typed
  when (model' /= model) (markDirty ctx)
  dirty' <- isDirty ctx
  pure (a, model', typed, draw, dirty || dirty')

runFrameEff ::
  IOE :> es =>
  (forall x. Eff es x -> IO x) ->
  Context ->
  Input ->
  Eff (Ui : es) a ->
  IO (a, [FrameMsg], DrawData, Bool)
runFrameEff unlift ctx inp ui = do
  oldHot <- readIORef (ctxLastHotId ctx)
  oldActive <- readIORef (ctxActiveId ctx)
  oldFocus <- readIORef (ctxFocusId ctx)
  oldHotRect <- getPrevRect ctx oldHot
  oldActiveRect <- getPrevRect ctx oldActive
  oldFocusRect <- getPrevRect ctx oldFocus
  oldFloatingRects <- readIORef (ctxPrevFloatingRects ctx)
  oldRects <- readIORef (ctxPrevRects ctx)
  oldTexts <- readIORef (ctxPrevNodeTexts ctx)
  oldSize <- readIORef (ctxLastWindowSize ctx)
  oldStore <- getStore ctx
  wasDirty <- isDirty ctx
  writeIORef (ctxDirty ctx) False
  animKeys <- IM.keys . IM.filter animInProgress <$> readIORef (ctxAnimations ctx)
  resetNodeArena (ctxNodeArena ctx)
  resetDrawArena (ctxDrawArena ctx)
  clearMeasureCache ctx
  writeIORef (ctxContainerStack ctx) []
  writeIORef (ctxFocusablesCount ctx) 0
  writeIORef (ctxHotId ctx) (WidgetId 0)
  writeIORef (ctxWidgetNodeTypes ctx) Nothing
  unless (inputMouseDown inp) $
    writeIORef (ctxSelectDropPress ctx) False
  beginFrameModal ctx
  writeIORef (ctxEscapeConsumed ctx) False
  clearTooltips ctx
  result <- unlift (runUi ctx inp ui)
  -- Terminal sliders embed the bar in node text; sync before measure so width is correct.
  syncWidgetLabels ctx
  let Size w h = inputWindowSize inp
  solveLayout (ctxNodeArena ctx) (ctxHostProfile ctx) (ctxFontMetrics ctx) (ctxMeasureText ctx) w h
  placeModals (ctxNodeArena ctx) (ctxHostProfile ctx) (ctxFontMetrics ctx) w h
  placeWindows (ctxNodeArena ctx) (ctxHostProfile ctx) (ctxFontMetrics ctx) w h (lookupWindowPos ctx) (lookupWindowSize ctx)
  movedResize <- updateWindowResize ctx inp w h
  movedWindow <- updateWindowDrag ctx inp
  when (movedResize || movedWindow) $
    placeWindows (ctxNodeArena ctx) (ctxHostProfile ctx) (ctxFontMetrics ctx) w h (lookupWindowPos ctx) (lookupWindowSize ctx)
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
  syncWidgetLabels ctx
  refreshHover ctx inp
  tickAnimations ctx (inputDeltaTime inp)
  beginLayer (ctxDrawArena ctx) LayerBackground
  lowerShapes ctx
  beginLayer (ctxDrawArena ctx) LayerOverlay
  drawWindowOverlays ctx
  drawModalOverlays ctx (inputWindowSize inp)
  drawSelectOverlays ctx inp
  drawTextInputMenuOverlays ctx inp
  drawTooltipOverlays ctx
  drawData <- finishDraw (ctxDrawArena ctx)
  updatePrevRects ctx
  updatePrevNodeTexts ctx
  newTexts <- readIORef (ctxPrevNodeTexts ctx)
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
    oldTexts
    newTexts
    animKeys
  msgs <- drainMessages ctx
  dirtyAfterUi <- isDirty ctx
  -- Keep mid-frame markDirty for the next loop. Open and close both flip
  -- the modal flag one frame after the click, so that follow-up must run.
  writeIORef (ctxDirty ctx) dirtyAfterUi
  pure (result, msgs, drawData, dirtyAfterUi)
