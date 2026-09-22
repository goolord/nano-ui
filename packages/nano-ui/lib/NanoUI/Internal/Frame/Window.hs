-- | Floating window input: dragging by the title bar, resizing by the edges
-- and margins, the resize cursor, and persisting window placement.
module NanoUI.Internal.Frame.Window
  ( contextMeasurers
  , lookupWindowPos
  , lookupWindowSize
  , persistWindowPositions
  , updateWindowDrag
  , updateWindowResize
  , WindowResizeEdge (..)
  , windowResizeCursorKind
  ) where

import Control.Monad (guard, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust)
import NanoUI.Internal.Context
  ( Context (..)
  , WindowResizeDrag (..)
  , WindowResizeEdge (..)
  , damageWidget
  , getStore
  , getWindowDrag
  , getWindowResize
  , intKey
  , markDirty
  , modifyStore
  , setStore
  , slotKey
  , Slot (..)
  , InteractionState (..)
  , modifyInteraction
  , lookupCustomMeasure
  )
import NanoUI.Internal.Font (ScrollBarSlot (..))
import NanoUI.Internal.Frame.Hit (nodeInSubtree, topmostOverlayAtMouse, widgetIdInSubtree, withWidgetNode)
import NanoUI.Internal.Frame.Input (findTopWidgetUnderMouse, isInteractiveNode)
import NanoUI.Internal.Frame.Redraw (probeHotId)
import NanoUI.Internal.Frame.Scroll.Geometry (scrollChromeLane)
import NanoUI.Internal.Id (WidgetId (..))
import NanoUI.Internal.Input (Input (..), UiCursorKind (..), inputMouseDown, inputMousePos, inputMousePressed)
import NanoUI.Internal.Layout.Arena
  ( NodeIdx
  , NodeType (..)
  , findFloatingNodeRevM
  , floatingNodeCount
  , foldFloatingNodesM
  , getDirection
  , getFirstChild
  , getMinMax
  , getNextSibling
  , getNodeRect
  , getNodeType
  , getNodeValue
  , getPadding
  , getRect
  , getWidgetId
  )
import NanoUI.Internal.Layout.Solve (Measurers (..), placeWindowNode, windowBodyScroller)
import NanoUI.Internal.Monad ((<&&>))
import NanoUI.Internal.Store (fieldPoint, insertSlot, lookupSlot)
import NanoUI.Internal.Style (Padding (..))
import NanoUI.Internal.Types (DamageBounds (..), Rect (..), V2 (..), clamp, haloDamageSlop, rectContains, rectInflate, rectNonEmpty)

topmostWindowAtResizeHalo :: Context -> V2 -> IO (Maybe NodeIdx)
topmostWindowAtResizeHalo ctx mouse =
  findFloatingNodeRevM na $ \idx ->
    ((== NodeWindow) <$> getNodeType na idx) <&&> do
      rect <- getNodeRect na idx
      pure (rectNonEmpty rect && rectContains (rectInflate windowResizeHandleFor rect) mouse)
 where
  na = ctxNodeArena ctx

-- | Saved floating-window x/y in logical pixels, or 'Nothing' before placement.
lookupWindowPos :: Context -> WidgetId -> IO (Maybe (Float, Float))
lookupWindowPos ctx wid = lookupSlot fieldPoint (intKey wid) <$> getStore ctx

-- | Saved floating-window width/height in logical pixels, or 'Nothing'.
lookupWindowSize :: Context -> WidgetId -> IO (Maybe (Float, Float))
lookupWindowSize ctx wid = lookupSlot fieldPoint (slotKey SlotWinSize (intKey wid)) <$> getStore ctx

-- | Save solved floating-window positions and sizes to the store for later frames.
persistWindowPositions :: Context -> IO ()
persistWindowPositions ctx = floatingNodeCount na >>= \floating -> when (floating > 0) $ do
  store0 <- getStore ctx
  let record acc idx = do
        nt <- getNodeType na idx
        if nt /= NodeWindow
          then pure acc
          else do
            wid <- getWidgetId na idx
            (x, y, w, h) <- getRect na idx
            let k = intKey wid
                sizeKey = slotKey SlotWinSize k
            -- Keep an unchanged map as is, so the store comparison below
            -- short-circuits on pointer equality.
            pure $
              if lookupSlot fieldPoint k acc == Just (x, y) && lookupSlot fieldPoint sizeKey acc == Just (w, h)
                then acc
                else insertSlot fieldPoint k (x, y) (insertSlot fieldPoint sizeKey (w, h) acc)
  store1 <- foldFloatingNodesM na record store0
  when (store1 /= store0) $ setStore ctx store1
 where
  na = ctxNodeArena ctx

-- | Start or continue a title-bar drag and save its position. Returns 'True'
-- while a drag starts or is held; releases clear it. Resize gestures take priority.
updateWindowDrag :: Context -> Input -> IO Bool
updateWindowDrag ctx inp = do
  resizing <- isJust <$> getWindowResize ctx
  if resizing
    then pure False
    else do
      drag <- getWindowDrag ctx
      case drag of
        Just (wid, gx, gy)
          | inputMouseDown inp -> do
              let V2 mx my = inputMousePos inp
              modifyStore ctx (insertSlot fieldPoint (intKey wid) (mx - gx, my - gy))
              damageWidget ctx wid (DamageInflated haloDamageSlop)
              markDirty ctx
              pure True
          | otherwise -> do
              modifyInteraction ctx (\s -> s {isWindowDrag = Nothing})
              pure False
        Nothing
          | inputMousePressed inp -> tryStartWindowDrag ctx (inputMousePos inp)
          | otherwise -> pure False

-- | How far the resize handles reach out past the window's edges.
windowResizeHandleFor :: Float
windowResizeHandleFor = 12

-- | The least a handle reaches in from an edge, for sides with no padding.
windowResizeInnerMin :: Float
windowResizeInnerMin = 6

-- | How far along an edge from a corner its handle resizes both ways.
windowResizeCornerReach :: Float
windowResizeCornerReach = 16

-- | Edge handle under @mouse@ for a window at @rect@ with padding @pad@. Each
-- side's handle runs from 'windowResizeHandleFor' outside the edge to the
-- side's padding inside it, and near a corner the handle takes both sides.
windowResizeEdgeAt :: Padding -> Rect -> V2 -> Maybe WindowResizeEdge
windowResizeEdgeAt pad (Rect x y w h) (V2 mx my) =
  let s = windowResizeHandleFor
      inner p extent = min (extent / 3) (max windowResizeInnerMin p)
      reachW = min windowResizeCornerReach (w / 3)
      reachH = min windowResizeCornerReach (h / 3)
      inX = mx >= x - s && mx <= x + w + s
      inY = my >= y - s && my <= y + h + s
      onL = inY && mx >= x - s && mx < x + inner (padL pad) w
      onR = inY && mx > x + w - inner (padR pad) w && mx <= x + w + s
      onT = inX && my >= y - s && my < y + inner (padT pad) h
      onB = inX && my > y + h - inner (padB pad) h && my <= y + h + s
      onSide = onL || onR
      onEnd = onT || onB
      north = onT || (onSide && my < y + reachH)
      south = onB || (onSide && my > y + h - reachH)
      west = onL || (onEnd && mx < x + reachW)
      east = onR || (onEnd && mx > x + w - reachW)
   in if not (onSide || onEnd)
        then Nothing
        else
          Just $
            case (north, south, west, east) of
              (True, _, True, _) -> ResizeNW
              (True, _, _, True) -> ResizeNE
              (_, True, True, _) -> ResizeSW
              (_, True, _, True) -> ResizeSE
              (True, _, _, _) -> ResizeN
              (_, True, _, _) -> ResizeS
              (_, _, True, _) -> ResizeW
              _ -> ResizeE

-- | Lane of the window body's scrollbar while its content overflows.
windowBodyScrollLane :: Context -> NodeIdx -> IO (Maybe Rect)
windowBodyScrollLane ctx winIdx = do
  let na = ctxNodeArena ctx
  mBody <- windowBodyScroller na winIdx
  case mBody of
    Nothing -> pure Nothing
    Just ci -> do
      (x, y, w, h) <- getRect na ci
      pad <- getPadding na ci
      contentSize <- getNodeValue na ci
      if contentSize > h - padT pad - padB pad
        then do
          dir <- getDirection na ci
          pure (Just (scrollChromeLane ScrollBarWindow dir x y w h pad))
        else pure Nothing

-- | Resize edge under @mouse@, leaving the body's scrollbar to scroll.
windowResizeEdgeFor :: Context -> NodeIdx -> Rect -> V2 -> IO (Maybe WindowResizeEdge)
windowResizeEdgeFor ctx winIdx winRect mouse = do
  pad <- getPadding (ctxNodeArena ctx) winIdx
  case windowResizeEdgeAt pad winRect mouse of
    Nothing -> pure Nothing
    Just edge
      | rectContains winRect mouse -> do
          mLane <- windowBodyScrollLane ctx winIdx
          pure (if maybe False (`rectContains` mouse) mLane then Nothing else Just edge)
      | otherwise -> pure (Just edge)

cursorForResizeEdge :: WindowResizeEdge -> UiCursorKind
cursorForResizeEdge = \case
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
  let !dx = mx - wrdGrabX wrd
      !dy = my - wrdGrabY wrd
      !minW = max (wrdMinW wrd) 1.0
      !minH = max (wrdMinH wrd) 1.0
      !maxW = min (wrdMaxW wrd) winW
      !maxH = min (wrdMaxH wrd) winH
      !right0 = wrdStartX wrd + wrdStartW wrd
      !bottom0 = wrdStartY wrd + wrdStartH wrd
      edge = wrdEdge wrd
      !fromE = edge `elem` [ResizeE, ResizeNE, ResizeSE]
      !fromW = edge `elem` [ResizeW, ResizeNW, ResizeSW]
      !fromS = edge `elem` [ResizeS, ResizeSE, ResizeSW]
      !fromN = edge `elem` [ResizeN, ResizeNE, ResizeNW]
      !w0
        | fromE = wrdStartW wrd + dx
        | fromW = wrdStartW wrd - dx
        | otherwise = wrdStartW wrd
      !h0
        | fromS = wrdStartH wrd + dy
        | fromN = wrdStartH wrd - dy
        | otherwise = wrdStartH wrd
      !w = clamp minW maxW w0
      !h = clamp minH maxH h0
      !x0 = if fromW then right0 - w else wrdStartX wrd
      !y0 = if fromN then bottom0 - h else wrdStartY wrd
      !x = clamp 0 (max 0 (winW - w)) x0
      !y = clamp 0 (max 0 (winH - h)) y0
   in (w, h, x, y)

-- | Start or continue a resize within logical window width/height. Updates
-- stored bounds and relayouts the window. Returns 'True' while starting or held.
updateWindowResize :: Context -> Input -> Float -> Float -> IO Bool
updateWindowResize ctx inp winW winH = do
  drag <- getWindowResize ctx
  case drag of
    Just wrd
      | inputMouseDown inp -> do
          let (nw, nh, nx, ny) = resizeFromEdge wrd (inputMousePos inp) winW winH
              key = intKey (wrdWidget wrd)
          modifyStore ctx (insertSlot fieldPoint (slotKey SlotWinSize key) (nw, nh) . insertSlot fieldPoint key (nx, ny))
          relayoutWindow ctx winW winH (wrdWidget wrd) nw nh
          damageWidget ctx (wrdWidget wrd) (DamageInflated haloDamageSlop)
          markDirty ctx
          pure True
      | otherwise -> do
          modifyInteraction ctx (\s -> s {isWindowResize = Nothing})
          pure False
    Nothing
      | inputMousePressed inp -> tryStartWindowResize ctx (inputMousePos inp)
      | otherwise -> pure False

relayoutWindow :: Context -> Float -> Float -> WidgetId -> Float -> Float -> IO ()
relayoutWindow ctx winW winH wid nw nh = do
  withWidgetNode ctx wid () $ \idx -> do
    mpos <- lookupWindowPos ctx wid
    (x, y, _, _) <- getRect (ctxNodeArena ctx) idx
    placeWindowNode (ctxNodeArena ctx) (contextMeasurers ctx) winW winH idx nw nh (const (fromMaybe (x, y) mpos))

-- | Resize edge under @mouse@ for the topmost window whose halo holds it,
-- unless the halo is blocked or the pointer is on one of the window's
-- controls. The top handle reaches over the title bar, which drags elsewhere.
resizeEdgeTarget :: Context -> V2 -> IO (Maybe (NodeIdx, Rect, WindowResizeEdge))
resizeEdgeTarget ctx mouse = runMaybeT $ do
  idx <- MaybeT (topmostWindowAtResizeHalo ctx mouse)
  rect <- liftIO (getNodeRect (ctxNodeArena ctx) idx)
  -- The halo covers the window interior, so find the edge first and run the
  -- hover probe and node scans only when there is one.
  edge <- MaybeT (windowResizeEdgeFor ctx idx rect mouse)
  guard . not =<< liftIO (resizeHaloBlocked ctx mouse idx)
  guard . not =<< liftIO (windowControlAt ctx idx mouse)
  pure (idx, rect, edge)

tryStartWindowResize :: Context -> V2 -> IO Bool
tryStartWindowResize ctx mouse@(V2 mx my) = do
  mTarget <- resizeEdgeTarget ctx mouse
  case mTarget of
    Nothing -> pure False
    Just (idx, Rect x y w h, edge) -> do
      wid <- getWidgetId (ctxNodeArena ctx) idx
      (minW, minH, maxW, maxH) <- getMinMax (ctxNodeArena ctx) idx
      modifyInteraction ctx $ \s ->
        s
          { isWindowResize =
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
          }
      markDirty ctx
      pure True

-- | Cursor for the held resize edge or an unblocked hovered edge. 'Nothing'
-- leaves cursor selection to other controls.
windowResizeCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
windowResizeCursorKind ctx inp = do
  mDrag <- getWindowResize ctx
  case mDrag of
    Just wrd
      | inputMouseDown inp -> pure (Just (cursorForResizeEdge (wrdEdge wrd)))
      | otherwise -> pure Nothing
    Nothing -> fmap (\(_, _, edge) -> cursorForResizeEdge edge) <$> resizeEdgeTarget ctx (inputMousePos inp)

-- Halo must not steal hits from page widgets or another window's interior.
resizeHaloBlocked :: Context -> V2 -> NodeIdx -> IO Bool
resizeHaloBlocked ctx mouse winIdx = do
  mInside <- topmostOverlayAtMouse ctx mouse
  case mInside of
    Just other | other /= winIdx -> pure True
    _ -> do
      hot <- probeHotId ctx mouse
      withWidgetNode ctx hot False $ \hotIdx -> not <$> nodeInSubtree ctx hotIdx winIdx

tryStartWindowDrag :: Context -> V2 -> IO Bool
tryStartWindowDrag ctx mouse@(V2 mx my) = do
  mTop <- topmostOverlayAtMouse ctx mouse
  case mTop of
    Nothing -> pure False
    Just idx -> do
      nt <- getNodeType (ctxNodeArena ctx) idx
      mTitle <- if nt == NodeWindow then windowTitleRect ctx idx else pure Nothing
      case mTitle of
        Just title | rectContains title mouse -> do
          overClose <- windowControlAt ctx idx mouse
          if overClose
            then pure False
            else do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              (wx, wy, _, _) <- getRect (ctxNodeArena ctx) idx
              modifyInteraction ctx (\s -> s {isWindowDrag = Just (wid, mx - wx, my - wy)})
              markDirty ctx
              pure True
        _ -> pure False

-- | Title bar: the window's topmost child, stretched up to the window top.
windowTitleRect :: Context -> NodeIdx -> IO (Maybe Rect)
windowTitleRect ctx idx = do
  (_, wy, _, _) <- getRect (ctxNodeArena ctx) idx
  fc <- getFirstChild (ctxNodeArena ctx) idx
  mBest <- go fc Nothing
  pure $ mBest <&> \(Rect cx cy cw ch) ->
    let topY = min wy cy
     in Rect cx topY cw ((cy - topY) + ch)
  where
    go ci best
      | ci < 0 = pure best
      | otherwise = do
          (x, y, w, h) <- getRect (ctxNodeArena ctx) ci
          ns <- getNextSibling (ctxNodeArena ctx) ci
          let here = Rect x y w h
          go ns $ case best of
            Just b@(Rect _ by _ _) | y >= by -> Just b
            _ -> Just here

windowControlAt :: Context -> NodeIdx -> V2 -> IO Bool
windowControlAt ctx idx mouse =
  maybe (pure False) (widgetIdInSubtree ctx idx) =<< findTopWidgetUnderMouse ctx mouse isInteractiveNode

-- | How the context measures text and custom widgets, for the solve and for
-- placing floating nodes after it.
contextMeasurers :: Context -> Measurers
contextMeasurers ctx =
  Measurers
    { msFm = ctxFontMetrics ctx
    , msMonoFm = ctxMonoFontMetrics ctx
    , msMeasure = ctxMeasureText ctx
    , msResolveFont = \sz weight style var -> do
        (fm, _) <- ctxResolveFont ctx sz weight style var
        pure (fm, ctxResolveMeasure ctx sz weight style var)
    , msLookupMeasure = lookupCustomMeasure ctx
    }
