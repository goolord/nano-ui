{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NanoUI.Frame.Window
  ( lookupWindowPos
  , lookupWindowSize
  , persistWindowPositions
  , updateWindowDrag
  , updateWindowResize
  , drawWindowOverlays
  , drawModalOverlays
  , windowResizeCursorKind
  , topmostWindowAtResizeHalo
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
  , NodeType (NodeButton, NodeCheckbox, NodeRadio, NodeSelect, NodeColorPicker, NodeSlider, NodeTextInput, NodeModal, NodeImage, NodePanel, NodeWindow, NodeContainer, NodeScrollContainer, NodeText, NodeSeparator, NodeSpacer, NodeBox)
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
import NanoUI.Frame.CursorKind (UiCursorKind (..))
import NanoUI.Frame.Chrome
  ( fillStyledRect
  , overlayModalStyle
  , overlayWindowStyle
  , pushMenuShadow
  , strokeStyledRect
  )
import NanoUI.Frame.Clip (scrollChromeLane, scrollContentClip, terminalModalOuterClip)
import NanoUI.Frame.Hit
  ( findNodeByWidgetId
  , modalTreeOpen
  , nodeInSubtree
  , nodeInTopmostModal
  , topmostModalIdx
  , topmostWindowAtMouse
  , widgetOverlayAllowed
  )
import NanoUI.Frame.Input (findTopWidgetUnderMouse, isInteractiveNode)
import NanoUI.Frame.Paint (walkChildren)
import NanoUI.Frame.Redraw (probeHotId)
import NanoUI.Frame.Scroll (paintScrollChrome)

topmostWindowAtResizeHalo :: Context -> V2 -> IO (Maybe NodeIdx)
topmostWindowAtResizeHalo ctx mouse = do
  count <- arenaCount (ctxNodeArena ctx)
  go (count - 1)
  where
    go idx
      | idx < 0 = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt /= NodeWindow
            then go (idx - 1)
            else do
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              if w <= 0 || h <= 0
                then go (idx - 1)
                else do
                  let rect = Rect x y w h
                  if rectContains (windowResizeHalo rect) mouse
                    then pure (Just idx)
                    else do
                      inInner <- windowInnerEastResizeHit ctx idx rect mouse
                      if inInner
                        then pure (Just idx)
                        else go (idx - 1)

windowInnerEastResizeHit :: Context -> NodeIdx -> Rect -> V2 -> IO Bool
windowInnerEastResizeHit ctx winIdx winRect mouse@(V2 mx _) = do
  let Rect x _ w _ = winRect
  pad <- getPadding (ctxNodeArena ctx) winIdx
  let pr = padR pad
  if mx < x + w - pr || mx > x + w
    then pure False
    else do
      mLane <- windowBodyScrollLane ctx winIdx
      pure (not (maybe False (`rectContains` mouse) mLane))

lookupWindowPos :: Context -> WidgetId -> IO (Maybe (Float, Float))
lookupWindowPos ctx wid = do
  store <- getStore ctx
  pure (IM.lookup (intKey wid) (storeWindow store))

lookupWindowSize :: Context -> WidgetId -> IO (Maybe (Float, Float))
lookupWindowSize ctx wid = do
  store <- getStore ctx
  pure (IM.lookup (intKey wid) (storeWindowSize store))

persistWindowPositions :: Context -> IO ()
persistWindowPositions ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  store0 <- getStore ctx
  pos1 <- foldlPos 0 count (storeWindow store0)
  size1 <- foldlSize 0 count (storeWindowSize store0)
  let store1 = store0 {storeWindow = pos1, storeWindowSize = size1}
  when (store1 /= store0) $ setStore ctx store1
  where
    foldlPos idx count acc
      | idx >= count = pure acc
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          acc' <-
            if nt /= NodeWindow
              then pure acc
              else do
                wid <- getWidgetId (ctxNodeArena ctx) idx
                (x, y, _, _) <- getRect (ctxNodeArena ctx) idx
                pure (IM.insert (intKey wid) (x, y) acc)
          foldlPos (idx + 1) count acc'
    foldlSize idx count acc
      | idx >= count = pure acc
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          acc' <-
            if nt /= NodeWindow
              then pure acc
              else do
                wid <- getWidgetId (ctxNodeArena ctx) idx
                (_, _, w, h) <- getRect (ctxNodeArena ctx) idx
                pure (IM.insert (intKey wid) (w, h) acc)
          foldlSize (idx + 1) count acc'

updateWindowDrag :: Context -> Input -> IO Bool
updateWindowDrag ctx inp = do
  resizing <- isJust <$> readIORef (ctxWindowResize ctx)
  if resizing
    then pure False
    else do
      drag <- readIORef (ctxWindowDrag ctx)
      case drag of
        Just (wid, gx, gy)
          | inputMouseDown inp -> do
              let V2 mx my = inputMousePos inp
                  pos = (mx - gx, my - gy)
              store <- getStore ctx
              setStore ctx (store {storeWindow = IM.insert (intKey wid) pos (storeWindow store)})
              markDirty ctx
              pure True
          | otherwise -> do
              writeIORef (ctxWindowDrag ctx) Nothing
              pure False
        Nothing
          | inputMousePressed inp -> do
              started <- tryStartWindowDrag ctx (inputMousePos inp)
              pure started
          | otherwise -> pure False

windowResizeHandle :: Float
windowResizeHandle = 12

windowResizeHalo :: Rect -> Rect
windowResizeHalo (Rect x y w h) =
  Rect (x - windowResizeHandle) (y - windowResizeHandle) (w + 2 * windowResizeHandle) (h + 2 * windowResizeHandle)

-- Handles sit outside the window. The right pad strip also resizes beside the bar.
windowResizeEdgeAt :: Rect -> V2 -> Maybe WindowResizeEdge
windowResizeEdgeAt (Rect x y w h) (V2 mx my) =
  let s = windowResizeHandle
      onL = mx >= x - s && mx < x
      onR = mx > x + w && mx <= x + w + s
      onT = my >= y - s && my < y
      onB = my > y + h && my <= y + h + s
   in if not (onL || onR || onT || onB)
        then Nothing
        else
          Just $
            case (onT, onB, onL, onR) of
              (True, _, True, _) -> ResizeNW
              (True, _, _, True) -> ResizeNE
              (_, True, True, _) -> ResizeSW
              (_, True, _, True) -> ResizeSE
              (True, _, _, _) -> ResizeN
              (_, True, _, _) -> ResizeS
              (_, _, True, _) -> ResizeW
              _ -> ResizeE

innerEastCornerEdge :: Padding -> Rect -> Float -> Maybe WindowResizeEdge
innerEastCornerEdge pad (Rect _ y _ h) my =
  let topBand = max 6 (min windowResizeHandle (padT pad))
      botBand = max 6 (min windowResizeHandle (padB pad))
   in case (my >= y && my < y + topBand, my > y + h - botBand && my <= y + h) of
        (True, _) -> Just ResizeNE
        (_, True) -> Just ResizeSE
        _ -> Just ResizeE

windowBodyScrollLane :: Context -> NodeIdx -> IO (Maybe Rect)
windowBodyScrollLane ctx winIdx = do
  fc <- getFirstChild (ctxNodeArena ctx) winIdx
  go fc
  where
    go ci
      | ci < 0 = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) ci
          ns <- getNextSibling (ctxNodeArena ctx) ci
          case nt of
            NodeScrollContainer -> do
              slot <- scrollBarSlotOf (ctxNodeArena ctx) ci
              if slot /= ScrollBarWindow
                then go ns
                else do
                  (x, y, w, h) <- getRect (ctxNodeArena ctx) ci
                  pad <- getPadding (ctxNodeArena ctx) ci
                  dir <- getDirection (ctxNodeArena ctx) ci
                  contentSize <- getNodeValue (ctxNodeArena ctx) ci
                  let fm = ctxFontMetrics ctx
                      innerH = h - padT pad - padB pad
                  if contentSize <= innerH
                    then go ns
                    else
                      pure
                        ( Just
                            (scrollChromeLane (ctxHostProfile ctx) fm slot dir x y w h pad)
                        )
            _ -> go ns

windowInnerResizeEdgeAt :: Context -> NodeIdx -> Rect -> V2 -> IO (Maybe WindowResizeEdge)
windowInnerResizeEdgeAt ctx winIdx winRect mouse = do
  hit <- windowInnerEastResizeHit ctx winIdx winRect mouse
  if hit
    then do
      pad <- getPadding (ctxNodeArena ctx) winIdx
      pure (innerEastCornerEdge pad winRect (v2Y mouse))
    else pure Nothing

windowResizeEdgeFor :: Context -> NodeIdx -> Rect -> V2 -> IO (Maybe WindowResizeEdge)
windowResizeEdgeFor ctx winIdx winRect mouse = do
  case windowResizeEdgeAt winRect mouse of
    Just edge -> pure (Just edge)
    Nothing -> windowInnerResizeEdgeAt ctx winIdx winRect mouse

cursorForResizeEdge :: WindowResizeEdge -> UiCursorKind
cursorForResizeEdge edge =
  case edge of
    ResizeN -> UiCursorNsResize
    ResizeS -> UiCursorNsResize
    ResizeE -> UiCursorEwResize
    ResizeW -> UiCursorEwResize
    ResizeNW -> UiCursorNwseResize
    ResizeSE -> UiCursorNwseResize
    ResizeNE -> UiCursorNeswResize
    ResizeSW -> UiCursorNeswResize

resizeFromEdge :: WindowResizeDrag -> V2 -> Float -> Float -> (Float, Float, Float, Float)
resizeFromEdge wrd (V2 mx my) winW winH =
  let dx = mx - wrdGrabX wrd
      dy = my - wrdGrabY wrd
      minW = wrdMinW wrd
      minH = wrdMinH wrd
      maxW = min (wrdMaxW wrd) winW
      maxH = min (wrdMaxH wrd) winH
      right0 = wrdStartX wrd + wrdStartW wrd
      bottom0 = wrdStartY wrd + wrdStartH wrd
      fromE = case wrdEdge wrd of
        ResizeE -> True
        ResizeNE -> True
        ResizeSE -> True
        _ -> False
      fromW = case wrdEdge wrd of
        ResizeW -> True
        ResizeNW -> True
        ResizeSW -> True
        _ -> False
      fromS = case wrdEdge wrd of
        ResizeS -> True
        ResizeSE -> True
        ResizeSW -> True
        _ -> False
      fromN = case wrdEdge wrd of
        ResizeN -> True
        ResizeNE -> True
        ResizeNW -> True
        _ -> False
      w0
        | fromE = wrdStartW wrd + dx
        | fromW = wrdStartW wrd - dx
        | otherwise = wrdStartW wrd
      h0
        | fromS = wrdStartH wrd + dy
        | fromN = wrdStartH wrd - dy
        | otherwise = wrdStartH wrd
      w = max minW (min maxW w0)
      h = max minH (min maxH h0)
      x0 = if fromW then right0 - w else wrdStartX wrd
      y0 = if fromN then bottom0 - h else wrdStartY wrd
      x = max 0 (min x0 (max 0 (winW - w)))
      y = max 0 (min y0 (max 0 (winH - h)))
   in (w, h, x, y)

updateWindowResize :: Context -> Input -> Float -> Float -> IO Bool
updateWindowResize ctx inp winW winH = do
  drag <- readIORef (ctxWindowResize ctx)
  case drag of
    Just wrd
      | inputMouseDown inp -> do
          let (nw, nh, nx, ny) = resizeFromEdge wrd (inputMousePos inp) winW winH
          store <- getStore ctx
          setStore
            ctx
            ( store
                { storeWindowSize = IM.insert (intKey (wrdWidget wrd)) (nw, nh) (storeWindowSize store)
                , storeWindow = IM.insert (intKey (wrdWidget wrd)) (nx, ny) (storeWindow store)
                }
            )
          relayoutWindow ctx winW winH (wrdWidget wrd) nw nh
          markDirty ctx
          pure True
      | otherwise -> do
          writeIORef (ctxWindowResize ctx) Nothing
          pure False
    Nothing
      | inputMousePressed inp -> tryStartWindowResize ctx (inputMousePos inp)
      | otherwise -> pure False

relayoutWindow :: Context -> Float -> Float -> WidgetId -> Float -> Float -> IO ()
relayoutWindow ctx winW winH wid nw nh = do
  mIdx <- findNodeByWidgetId ctx wid
  case mIdx of
    Nothing -> pure ()
    Just idx -> do
      (minW, minH, maxW, maxH) <- getMinMax (ctxNodeArena ctx) idx
      let w = max minW (min (min maxW winW) nw)
          h = max minH (min (min maxH winH) nh)
      mpos <- lookupWindowPos ctx wid
      (x, y, _, _) <- getRect (ctxNodeArena ctx) idx
      let (x0, y0) = maybe (x, y) id mpos
          x' = max 0 (min x0 (max 0 (winW - w)))
          y' = max 0 (min y0 (max 0 (winH - h)))
      positionWindowNode (ctxNodeArena ctx) (ctxHostProfile ctx) (ctxFontMetrics ctx) idx x' y' w h

tryStartWindowResize :: Context -> V2 -> IO Bool
tryStartWindowResize ctx mouse = do
  mWin <- topmostWindowAtResizeHalo ctx mouse
  case mWin of
    Nothing -> pure False
    Just idx -> do
      blocked <- resizeHaloBlocked ctx mouse idx
      overClose <- windowTitleHasInteractive ctx idx mouse
      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
      if blocked || overClose
        then pure False
        else do
          mEdge <- windowResizeEdgeFor ctx idx (Rect x y w h) mouse
          case mEdge of
            Nothing -> pure False
            Just edge -> do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              (minW, minH, maxW, maxH) <- getMinMax (ctxNodeArena ctx) idx
              let V2 mx my = mouse
              writeIORef (ctxWindowResize ctx) $
                Just
                  WindowResizeDrag
                    { wrdWidget = wid
                    , wrdEdge = edge
                    , wrdGrabX = mx
                    , wrdGrabY = my
                    , wrdStartX = x
                    , wrdStartY = y
                    , wrdStartW = w
                    , wrdStartH = h
                    , wrdMinW = minW
                    , wrdMinH = minH
                    , wrdMaxW = maxW
                    , wrdMaxH = maxH
                    }
              markDirty ctx
              pure True

windowResizeCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
windowResizeCursorKind ctx inp = do
  mDrag <- readIORef (ctxWindowResize ctx)
  case mDrag of
    Just wrd | inputMouseDown inp -> pure (Just (cursorForResizeEdge (wrdEdge wrd)))
    Just _ -> pure Nothing
    Nothing -> do
      let mouse = inputMousePos inp
      mWin <- topmostWindowAtResizeHalo ctx mouse
      case mWin of
        Nothing -> pure Nothing
        Just idx -> do
          blocked <- resizeHaloBlocked ctx mouse idx
          overClose <- windowTitleHasInteractive ctx idx mouse
          (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
          if blocked || overClose
            then pure Nothing
            else fmap cursorForResizeEdge <$> windowResizeEdgeFor ctx idx (Rect x y w h) mouse

-- Halo must not steal hits from page widgets or another window's interior.
resizeHaloBlocked :: Context -> V2 -> NodeIdx -> IO Bool
resizeHaloBlocked ctx mouse winIdx = do
  mInside <- topmostWindowAtMouse ctx mouse
  case mInside of
    Just other | other /= winIdx -> pure True
    _ -> do
      hot <- probeHotId ctx mouse
      if hashWidgetId hot == 0
        then pure False
        else do
          mHot <- findNodeByWidgetId ctx hot
          case mHot of
            Nothing -> pure False
            Just hotIdx -> not <$> nodeInSubtree ctx hotIdx winIdx

tryStartWindowDrag :: Context -> V2 -> IO Bool
tryStartWindowDrag ctx mouse = do
  mWin <- topmostWindowAtMouse ctx mouse
  case mWin of
    Nothing -> pure False
    Just idx -> do
      mTitle <- windowTitleRect ctx idx
      case mTitle of
        Nothing -> pure False
        Just title -> do
          let overTitle = rectContains title mouse
          overClose <- windowTitleHasInteractive ctx idx mouse
          if overTitle && not overClose
            then do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              (wx, wy, _, _) <- getRect (ctxNodeArena ctx) idx
              let V2 mx my = mouse
              writeIORef (ctxWindowDrag ctx) (Just (wid, mx - wx, my - wy))
              markDirty ctx
              pure True
            else pure False

windowTitleRect :: Context -> NodeIdx -> IO (Maybe Rect)
windowTitleRect ctx idx = do
  fc <- getFirstChild (ctxNodeArena ctx) idx
  go fc Nothing
  where
    go ci best
      | ci < 0 = pure best
      | otherwise = do
          (x, y, w, h) <- getRect (ctxNodeArena ctx) ci
          ns <- getNextSibling (ctxNodeArena ctx) ci
          let here = Rect x y w h
              best' =
                case best of
                  Nothing -> Just here
                  Just b -> if y < rectY b then Just here else Just b
          go ns best'

windowTitleHasInteractive :: Context -> NodeIdx -> V2 -> IO Bool
windowTitleHasInteractive ctx idx mouse = do
  count <- arenaCount (ctxNodeArena ctx)
  mWid <- findTopWidgetUnderMouse ctx count mouse isInteractiveNode
  case mWid of
    Nothing -> pure False
    Just wid -> do
      mNode <- findNodeByWidgetId ctx wid
      case mNode of
        Nothing -> pure False
        Just wi -> nodeInSubtree ctx wi idx

drawWindowOverlays :: Context -> IO ()
drawWindowOverlays ctx = do
  let theme = ctxTheme ctx
      style = overlayWindowStyle theme
  forFloatingNode ctx NodeWindow $ \idx rect ->
    drawFloatingPanel ctx idx style rect rect

drawModalOverlays :: Context -> Size -> IO ()
drawModalOverlays ctx (Size ww wh) = do
  let da = ctxDrawArena ctx
      theme = ctxTheme ctx
      fm = ctxFontMetrics ctx
      terminal = isCellHost (ctxHostProfile ctx)
  found <- modalTreeOpen ctx
  when found $ do
    when terminal $
      pushBackdropDim da (Rect 0 0 ww wh) (themeOverlayDim theme)
    when (not terminal) $
      pushRect da (Rect 0 0 ww wh) (themeOverlayDim theme)
    forFloatingNode ctx NodeModal $ \idx rect@(Rect x y w h) -> do
      pad <- getPadding (ctxNodeArena ctx) idx
      wid <- getWidgetId (ctxNodeArena ctx) idx
      dir <- getDirection (ctxNodeArena ctx) idx
      contentSize <- getNodeValue (ctxNodeArena ctx) idx
      slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
      let style = if terminal then overlayWindowStyle theme else overlayModalStyle theme
          clip =
            if terminal
              then terminalModalOuterClip (ctxHostProfile ctx) fm x y w h pad
              else scrollContentClip (ctxHostProfile ctx) fm slot dir x y w h pad contentSize
      drawFloatingPanel ctx idx style rect clip
      when (not terminal) $
        paintScrollChrome ctx da idx wid x y w h pad theme terminal

forFloatingNode :: Context -> NodeType -> (NodeIdx -> Rect -> IO ()) -> IO ()
forFloatingNode ctx nodeType draw = do
  count <- arenaCount (ctxNodeArena ctx)
  forM_ [0 .. count - 1] $ \idx -> do
    actualType <- getNodeType (ctxNodeArena ctx) idx
    when (actualType == nodeType) $ do
      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
      draw idx (Rect x y w h)

drawFloatingPanel :: Context -> NodeIdx -> Style -> Rect -> Rect -> IO ()
drawFloatingPanel ctx idx style rect@(Rect x y w h) clip = do
  let da = ctxDrawArena ctx
      terminal = isCellHost (ctxHostProfile ctx)
  when (not terminal) $ pushMenuShadow da rect (styleCornerRadius style)
  fillStyledRect da terminal style rect
  strokeStyledRect da terminal style x y w h
  withClip da clip $ walkChildren ctx idx

