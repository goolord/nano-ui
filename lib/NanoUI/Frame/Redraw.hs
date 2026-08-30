{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
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
import NanoUI.Input (Input (..), Key (..), Modifiers (..), inputInteracted, inputKeys, inputPointerHeld, inputMouseDown, inputMousePos, inputMousePressed, inputMouseReleased, inputMouseRightPressed, inputScroll, inputDeltaTime, inputWindowSize, modShift)
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
  )
import NanoUI.Style (Padding (..), Style (..), Theme (..), scrollBarThumbColor, scrollBarTrackColor, themeAccent, themeButton, themeFloatingWindow, themeInput, themeMuted, themeOverlayDim, themePanel, themeSeparator, themeWindow)
import NanoUI.Types (Color (..), ImageId (..), Rect (..), Size (..), V2 (..), colorRGBA, lerpColor, rectContains, rectH, rectIntersect, rectOverlapArea, rectUnion, rectW, rectX, rectY, v2X, v2Y)
import NanoUI.Frame.Hit (findNodeByWidgetId, overlayHitAllowed)
import NanoUI.Frame.Select (selectDropRect)

needsRedraw :: Context -> Input -> Input -> IO Bool
needsRedraw = needsRedraw' True

-- Terminal keeps the last blit while idle; SDL windows tick live for damage.
needsRedrawIdle :: Context -> Input -> Input -> IO Bool
needsRedrawIdle = needsRedraw' False

-- Window/scroll/resize drag marks dirty every frame. TUI must still poll input then.
pointerDragActive :: Context -> IO Bool
pointerDragActive ctx = do
  winDrag <- isJust <$> readIORef (ctxWindowDrag ctx)
  scrollDrag <- isJust <$> readIORef (ctxScrollDrag ctx)
  winResize <- isJust <$> readIORef (ctxWindowResize ctx)
  pure (winDrag || scrollDrag || winResize)

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
  pure (any id (IM.elems (storeSelectOpen store)) || isJust menu)

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
                if not (IM.findWithDefault False key (storeSelectOpen store))
                  then go (idx + 1)
                  else do
                    txt <- getText (ctxNodeArena ctx) idx
                    (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                    let (_, opts) = selectParseOptions txt
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

-- Floating window overlay (debug HUD). Uses persisted window store because
-- the node arena is empty between skipped idle frames.
debugPanelOpen :: Context -> IO Bool
debugPanelOpen ctx = do
  store <- getStore ctx
  if not (IM.null (storeWindow store))
    then pure True
    else do
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
    Nothing -> do
      count <- arenaCount (ctxNodeArena ctx)
      if count <= 0
        then pure (WidgetId 0)
        else go (WidgetId 0) (count - 1)
  where
    go acc idx
      | idx < 0 = pure acc
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          acc' <-
            if not (isWidgetNode nt)
              then pure acc
              else do
                wid <- getWidgetId (ctxNodeArena ctx) idx
                (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                if w > 0 && h > 0 && rectContains (Rect x y w h) mouse
                  then do
                    allow <- overlayHitAllowed ctx idx mouse
                    pure (if allow then wid else acc)
                  else pure acc
          go acc' (idx - 1)

