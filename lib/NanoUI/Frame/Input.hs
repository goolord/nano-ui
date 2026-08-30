{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NanoUI.Frame.Input
  ( finalizeTabFocus
  , refreshHover
  , finalizePointerPress
  , finalizePointerRelease
  , finalizeTextInputFocus
  , finalizeSelectFocus
  , finalizeTextInputMouse
  , findTopWidgetUnderMouse
  , isInteractiveNode
  , findTextInputUnderMouse
  ) where


import Control.Monad (filterM, foldM, forM, forM_, unless, void, when)
import Data.Char (isAlphaNum, isSpace)
import Data.IORef (readIORef, writeIORef)
import Data.Typeable (Typeable)
import Data.List (findIndex)
import Data.Maybe (isJust)
import Data.Word (Word32)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import NanoUI.Damage (floatingPanelRects, updatePrevRects, writeDamage)
import NanoUI.Context
  ( Context (..)
  , FrameMsg (..)
  , WidgetStore (..)
  , TextInputMenu (..)
  , TextInputDrag (..)
  , WindowResizeDrag (..)
  , WindowResizeEdge (..)
  , anyAnimating
  , decodeMessages
  , drainMessages
  , getFocusables
  , getScrollOffset
  , getStore
  , intKey
  , isDirty
  , isDisabled
  , markDirty
  , getHotId
  , getPrevRect
  , setScrollOffset
  , setStore
  , startAnimation
  , setAnimationValue
  , tickAnimations
  , getAnimationValue
  , animInProgress
  , clearTooltips
  , readTooltips
  , PendingTooltip (..)
  , ctxClipboardGet
  , clearMeasureCache
  , markEscapeConsumed
  , lookupImageUv
  , atlasTextureId
  )
import NanoUI.Draw
  ( DrawArena
  , DrawData
  , Layer (..)
  , beginLayer
  , currentLayer
  , finishDraw
  , pushLine
  , pushFilledTriangle
  , pushRect
  , pushBackdropDim
  , pushImage
  , pushRoundedRect
  , pushRoundedStroke
  , pushText
  , resetDrawArena
  , withClip
  )
import NanoUI.Font
  ( FontMetrics (..)
  , checkboxBoxSize
  , checkboxLeading
  , fmLineHeight
  , layoutLineHeight
  , hasHeadingMarker
  , hasMonoFontMarker
  , hasMutedMarker
  , labelContentInset
  , resolveLayoutPadding
  , stripWidgetMarkers
  , lineWidth
  , textDisplayWidth
  , ScrollBarSlot (..)
  , scrollBarGeomFor
  , scrollBarOuterGap
  , scrollLayoutGutter
  , sliderTrackBounds
  , widgetContentInset
  , centeredTextY
  , alignedTextBox
  , wrapTextLines
  , wrapTextLinesIO
  )
import NanoUI.Host (HostProfile, isCellHost)
import NanoUI.WidgetMarkers
  ( buttonDisplayText
  , closeButtonDisplayText
  , isCloseButtonText
  , stripButtonBrackets
  )
import NanoUI.Icons (Icons (..), checkboxMark, terminalPaintColumns)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), Key (..), Modifiers (..), inputInteracted, inputKeys, inputKeysElem, inputPointerHeld, inputMouseDown, inputMousePos, inputMousePressed, inputMouseReleased, inputMouseRightPressed, inputScroll, inputDeltaTime, inputWindowSize, modShift)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , getAlignX
  , getDirection
  , getFirstChild
  , getHeightSizing
  , getMinMax
  , getNextSibling
  , getParent
  , getNodeType
  , getNodeValue
  , getPadding
  , getRect
  , getStyleIdx
  , getText
  , getWidthSizing
  , getWidgetId
  , isWidgetNode
  , isContainerNode
  , isFloatingNode
  , isScrollNode
  , NodeType (NodeButton, NodeCheckbox, NodeRadio, NodeSelect, NodeSlider, NodeTextInput, NodeModal, NodeImage, NodePanel, NodeWindow, NodeContainer, NodeScrollContainer, NodeText, NodeSeparator, NodeSpacer, NodeBox)
  , resetNodeArena
  , setNodeText
  , setNodeValue
  , setRect
  )
import NanoUI.Layout.Solve (placeModals, placeWindows, positionWindowNode, scrollBarSlotOf, solveLayout)
import Effectful (Eff, IOE, runEff, type (:>))
import NanoUI.Monad (NanoUI, Ui, runUi)
import NanoUI.Widgets (applyTextInputMenuAction)
import NanoUI.WidgetText
  ( checkboxLabelText
  , sliderLabelText
  , sliderPackRange
  , sliderParseRange
  , sliderPackTerminal
  , sliderValueText
  , textInputFieldHeight
  , textInputFieldText
  , textInputLabelGap
  , textInputTerminalText
  , selectParseOptions
  , selectDisplayText
  , selectChevronReserve
  , selectChevronCenterX
  , radioParseOption
  )
import NanoUI.Style (Padding (..), Style (..), Theme (..), scrollBarThumbColor, scrollBarTrackColor, themeAccent, themeButton, themeFloatingWindow, themeInput, themeMuted, themeOverlayDim, themePanel, themeSeparator, themeWindow)
import NanoUI.Types (Color (..), ImageId (..), Rect (..), Size (..), V2 (..), colorRGBA, lerpColor, rectContains, rectH, rectIntersect, rectOverlapArea, rectUnion, rectW, rectX, rectY, v2X, v2Y)
import NanoUI.Frame.Chrome (displayText)
import NanoUI.Frame.Focus (filterModalFocusables, tabNext, unlessHit)
import NanoUI.Frame.Hit (overlayHitAllowed)
import NanoUI.Frame.Redraw (probeHotId)
import NanoUI.Frame.Select (findSelectUnderMouse, selectDropRect)
import NanoUI.Frame.Spans (widgetHitRect)
import NanoUI.Frame.TextInput (collapseTextInputSelection, textInputGeomForWidget, applyTextInputClick, applyTextInputDrag, textInputCharAtX)

finalizeTabFocus :: Context -> Input -> IO ()
finalizeTabFocus ctx inp =
  when (inputKeysElem KeyTab (inputKeys inp)) $ do
    focusables <- getFocusables ctx
    let raw = filter (/= WidgetId 0) focusables
    ids <- filterModalFocusables ctx raw
    if null ids
      then pure ()
      else do
        cur <- readIORef (ctxFocusId ctx)
        let shift = modShift (inputModifiers inp)
            next = tabNext cur ids shift
        writeIORef (ctxFocusId ctx) next
        markDirty ctx

refreshHover :: Context -> Input -> IO ()
refreshHover ctx inp = do
  prevHot <- readIORef (ctxLastHotId ctx)
  newHot <- probeHotId ctx (inputMousePos inp)
  writeIORef (ctxHotId ctx) newHot
  writeIORef (ctxLastHotId ctx) newHot
  let terminal = isCellHost (ctxHostProfile ctx)
  when (prevHot /= newHot) $ do
    unless terminal $ do
      when (hashWidgetId prevHot /= 0) $ startAnimation ctx prevHot 1 0 0.12
      when (hashWidgetId newHot /= 0) $ startAnimation ctx newHot 0 1 0.12

-- Same walk as refreshHover: later nodes paint first, earlier widget hits win.
finalizePointerPress :: Context -> Input -> IO ()
finalizePointerPress ctx inp =
  when (inputMousePressed inp) $ do
    let mouse = inputMousePos inp
    count <- arenaCount (ctxNodeArena ctx)
    mWid <- findTopWidgetUnderMouse ctx count mouse isInteractiveNode
    case mWid of
      Nothing -> pure ()
      Just wid -> do
        disabled <- isDisabled ctx wid
        unless disabled $ writeIORef (ctxActiveId ctx) wid

findTopWidgetUnderMouse ::
  Context -> Int -> V2 -> (NodeType -> Bool) -> IO (Maybe WidgetId)
findTopWidgetUnderMouse ctx count mouse wanted = go (count - 1)
  where
    go idx
      | idx < 0 = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if not (wanted nt)
            then go (idx - 1)
            else do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              rect <- widgetHitRect ctx nt idx x y w h
              if rectW rect > 0 && rectH rect > 0 && rectContains rect mouse
                then do
                  allow <- overlayHitAllowed ctx idx mouse
                  if allow then pure (Just wid) else go (idx - 1)
                else go (idx - 1)

isInteractiveNode :: NodeType -> Bool
isInteractiveNode nt =
  nt == NodeButton
    || nt == NodeCheckbox
    || nt == NodeRadio
    || nt == NodeSlider
    || nt == NodeSelect
    || nt == NodeTextInput

-- Clicks are finalized against solved layout rects; widgets only track press state.
finalizePointerRelease :: Context -> Input -> IO ()
finalizePointerRelease ctx inp =
  if not (inputMouseReleased inp)
    then pure ()
    else do
      active <- readIORef (ctxActiveId ctx)
      when (hashWidgetId active /= 0) $ do
        let mouse = inputMousePos inp
        count <- arenaCount (ctxNodeArena ctx)
        releasedOver <-
          if count <= 0
            then pure False
            else checkReleasedOver ctx count active mouse
        forM_ [0 .. count - 1] $ \idx -> do
          wid <- getWidgetId (ctxNodeArena ctx) idx
          when (wid == active) $ do
            nt <- getNodeType (ctxNodeArena ctx) idx
            (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
            let rect = Rect x y w h
            when (w > 0 && h > 0 && rectContains rect mouse) $
              case nt of
                NodeCheckbox -> do
                  store <- getStore ctx
                  let key = intKey wid
                      current =
                        IM.findWithDefault False key (storeCheckbox store)
                      newVal = not current
                  setStore
                    ctx
                    ( store
                        { storeCheckbox = IM.insert key newVal (storeCheckbox store)
                        }
                    )
                NodeRadio -> do
                  store <- getStore ctx
                  txt <- getText (ctxNodeArena ctx) idx
                  let (groupKey, optIdx, _) = radioParseOption txt
                  setStore
                    ctx
                    ( store
                        { storeRadio = IM.insert groupKey optIdx (storeRadio store)
                        }
                    )
                _ -> pure ()
        writeIORef (ctxActiveId ctx) (WidgetId 0)
        when releasedOver $
          unless (isCellHost (ctxHostProfile ctx)) $
            setAnimationValue ctx active 1

checkReleasedOver :: Context -> Int -> WidgetId -> V2 -> IO Bool
checkReleasedOver ctx count active mouse = go 0
  where
    go idx
      | idx >= count = pure False
      | otherwise = do
          wid <- getWidgetId (ctxNodeArena ctx) idx
          if wid /= active
            then go (idx + 1)
            else do
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              let rect = Rect x y w h
              pure (w > 0 && h > 0 && rectContains rect mouse)

-- Focus text inputs using solved layout rects so the caret appears on first press.
finalizeTextInputFocus :: Context -> Input -> IO ()
finalizeTextInputFocus ctx inp =
  when (inputMousePressed inp) $ do
    mMenu <- readIORef (ctxTextInputMenu ctx)
    let mouse = inputMousePos inp
    when (case mMenu of
            Just menu -> not (rectContains (textInputMenuRect menu) mouse)
            Nothing -> True) $ do
      prevFocus <- readIORef (ctxFocusId ctx)
      count <- arenaCount (ctxNodeArena ctx)
      mFocused <- findTextInputUnderMouse ctx count mouse
      case mFocused of
        Nothing -> do
          when (prevFocus /= WidgetId 0) $ markDirty ctx
          collapseTextInputSelection ctx prevFocus
          writeIORef (ctxFocusId ctx) (WidgetId 0)
          writeIORef (ctxTextInputMenu ctx) Nothing
        Just wid -> do
          writeIORef (ctxFocusId ctx) wid
          when (prevFocus /= wid) $ markDirty ctx

finalizeSelectFocus :: Context -> Input -> IO ()
finalizeSelectFocus ctx inp =
  when (inputMousePressed inp) $ do
    count <- arenaCount (ctxNodeArena ctx)
    mWid <- findSelectUnderMouse ctx count (inputMousePos inp)
    case mWid of
      Nothing -> pure ()
      Just wid -> do
        disabled <- isDisabled ctx wid
        unless disabled $ do
          prev <- readIORef (ctxFocusId ctx)
          writeIORef (ctxFocusId ctx) wid
          when (prev /= wid) $ markDirty ctx

finalizeTextInputMouse :: Context -> Input -> IO ()
finalizeTextInputMouse ctx inp = do
  focus <- readIORef (ctxFocusId ctx)
  when (hashWidgetId focus /= 0) $ do
    mGeom <- textInputGeomForWidget ctx focus
    case mGeom of
      Nothing -> pure ()
      Just (fieldRect, contentX, value) -> do
        let mouse = inputMousePos inp
            inField = rectContains fieldRect mouse
        if inputMousePressed inp && inField
          then do
            idx <- textInputCharAtX ctx value contentX (v2X mouse)
            let clicks = max 1 (inputMouseClicks inp)
            applyTextInputClick ctx focus value idx clicks
            writeIORef (ctxTextInputDrag ctx) (Just (TextInputDrag focus idx clicks))
          else do
            mDrag <- readIORef (ctxTextInputDrag ctx)
            case mDrag of
              Just drag
                | textInputDragWidget drag == focus && (inputMouseDown inp || inputMouseReleased inp) -> do
                    idx <- textInputCharAtX ctx value contentX (v2X mouse)
                    applyTextInputDrag ctx focus value (textInputDragAnchor drag) idx (textInputDragClicks drag)
              _ -> pure ()
  when (inputMouseReleased inp) $
    writeIORef (ctxTextInputDrag ctx) Nothing

findTextInputUnderMouse :: Context -> Int -> V2 -> IO (Maybe WidgetId)
findTextInputUnderMouse ctx count mouse = go 0
  where
    go idx
      | idx >= count = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt == NodeTextInput
            then do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              rect <- widgetHitRect ctx nt idx x y w h
              if rectW rect > 0 && rectH rect > 0 && rectContains rect mouse
                then do
                  allow <- overlayHitAllowed ctx idx mouse
                  if allow then pure (Just wid) else go (idx + 1)
                else go (idx + 1)
            else go (idx + 1)

