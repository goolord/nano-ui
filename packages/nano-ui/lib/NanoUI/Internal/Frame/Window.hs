-- | Floating window input: dragging by the title bar, resizing by the edges
-- and margins, the resize cursor, and persisting window placement.
module NanoUI.Internal.Frame.Window
  ( contextMeasurers
  , lookupWindowPos
  , lookupWindowSize
  , persistWindowPositions
  , updateWindowDrag
  , updateWindowResize
  , windowResizeCursorKind
  ) where

import Control.Monad (guard, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Functor ((<&>))
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, isNothing)
import NanoUI.Internal.Context
  ( Context (..)
  , cachedWrapText
  , WindowResizeDrag (..)
  , WindowResizeEdge (..)
  , damageWidget
  , getStore
  , getsInteraction
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
import NanoUI.Internal.Frame.Hit
  ( nodeInSubtree
  , topmostFloating
  , topmostOverlayAtMouse
  , widgetIdInSubtree
  , withWidgetNode
  )
import NanoUI.Internal.Frame.Input (findTopWidgetUnderMouse)
import NanoUI.Internal.Frame.Redraw (probeHotId)
import NanoUI.Internal.Frame.Scroll.Geometry (scrollChromeLane)
import NanoUI.Internal.Id (WidgetId (..))
import NanoUI.Internal.Input (Input (..), UiCursorKind (..), inputMouseDown, inputMousePos, inputMousePressed)
import NanoUI.Internal.Layout.Arena
  ( AxisSizing (..)
  , NodeClass (FloatingNodes)
  , NodeIdx
  , NodeType (..)
  , floatingNodeCount
  , foldClassNodesM
  , getDirection
  , getFirstChild
  , getHeightSizing
  , getNextSibling
  , getNodeRect
  , getNodeType
  , getNodeValue
  , getPadding
  , getRect
  , getWidgetId
  , getWidthSizing
  , isWidgetNode
  )
import NanoUI.Internal.Layout.Solve (Measurers (..), placeWindowNode, windowBodyScroller)
import NanoUI.Internal.Monad ((<&&>))
import NanoUI.Internal.Store (fieldPoint, insertSlot, lookupSlot)
import NanoUI.Internal.Style (Padding (..))
import NanoUI.Internal.Types (DamageBounds (..), Rect (..), V2 (..), clamp, haloDamageSlop, rectContains, rectInflate, rectNonEmpty)

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
  store1 <- foldClassNodesM na FloatingNodes record store0
  when (store1 /= store0) $ setStore ctx store1
 where
  na = ctxNodeArena ctx

-- | Start or continue a title-bar drag and save its position. Returns 'True'
-- while a drag starts or is held; releases clear it. Resize gestures take priority.
updateWindowDrag :: Context -> Input -> IO Bool
updateWindowDrag ctx inp =
  (isNothing <$> getsInteraction ctx isWindowResize) <&&> do
    let drag (wid, gx, gy) = do
          let V2 mx my = inputMousePos inp
          modifyStore ctx (insertSlot fieldPoint (intKey wid) (mx - gx, my - gy))
          pure wid
    windowGesture ctx inp isWindowDrag (\s -> s {isWindowDrag = Nothing}) drag tryStartWindowDrag

-- | Continue the window gesture @held@ reads while the button is down, with
-- @step@, which moves its window, and end it once the button is up. With none
-- held, a press may start one. Returns 'True' while one starts or is held.
windowGesture ::
  Context -> Input -> (InteractionState -> Maybe g) -> (InteractionState -> InteractionState)
  -> (g -> IO WidgetId) -> (Context -> V2 -> IO Bool) -> IO Bool
windowGesture ctx inp held release step start =
  getsInteraction ctx held >>= \case
    Just g
      | inputMouseDown inp -> do
          wid <- step g
          damageWidget ctx wid (DamageInflated haloDamageSlop)
          markDirty ctx
          pure True
      | otherwise -> False <$ modifyInteraction ctx release
    Nothing
      | inputMousePressed inp -> start ctx (inputMousePos inp)
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
windowResizeEdgeAt pad (Rect x y w h) (V2 mx my)
  | onSide || onEnd = find ((== (side west east, side north south)) . edgeSides) allEdges
  | otherwise = Nothing
  where
    s = windowResizeHandleFor
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
    side lo hi = if lo then -1 else if hi then 1 else 0
    allEdges = [ResizeN, ResizeS, ResizeE, ResizeW, ResizeNE, ResizeNW, ResizeSE, ResizeSW]

-- | Which side of each axis an edge moves: -1 the left or top, 1 the right or
-- bottom, 0 neither.
edgeSides :: WindowResizeEdge -> (Int, Int)
edgeSides = \case
  ResizeN -> (0, -1)
  ResizeS -> (0, 1)
  ResizeE -> (1, 0)
  ResizeW -> (-1, 0)
  ResizeNE -> (1, -1)
  ResizeNW -> (-1, -1)
  ResizeSE -> (1, 1)
  ResizeSW -> (-1, 1)

cursorForResizeEdge :: WindowResizeEdge -> UiCursorKind
cursorForResizeEdge edge = case edgeSides edge of
  (0, _) -> UiCursorNsResize
  (_, 0) -> UiCursorEwResize
  (sx, sy)
    | sx == sy -> UiCursorNwseResize
    | otherwise -> UiCursorNeswResize

-- | Width, height, x and y of the held resize's window with the pointer at
-- @mouse@, in a logical window of @winW@ by @winH@.
resizeFromEdge :: WindowResizeDrag -> V2 -> Float -> Float -> (Float, Float, Float, Float)
resizeFromEdge wrd (V2 mx my) winW winH = (w, h, x, y)
  where
    (sx, sy) = edgeSides (wrdEdge wrd)
    (x, w) = axis sx (wrdStartX wrd) (wrdStartW wrd) (mx - wrdGrabX wrd) (wrdMinW wrd) (wrdMaxW wrd) winW
    (y, h) = axis sy (wrdStartY wrd) (wrdStartH wrd) (my - wrdGrabY wrd) (wrdMinH wrd) (wrdMaxH wrd) winH
    -- The moving side follows the pointer within the size limits, the other
    -- stays put, and the window stays inside the logical window.
    axis :: Int -> Float -> Float -> Float -> Float -> Float -> Float -> (Float, Float)
    axis side start len d lo hi limit =
      let !len' = clamp (max lo 1) (min hi limit) (len + fromIntegral side * d)
          !pos = if side < 0 then start + len - len' else start
       in (clamp 0 (max 0 (limit - len')) pos, len')

-- | Start or continue a resize within logical window width/height. Updates
-- stored bounds and places the window again. Returns 'True' while starting or held.
updateWindowResize :: Context -> Input -> Float -> Float -> IO Bool
updateWindowResize ctx inp winW winH =
  windowGesture ctx inp isWindowResize (\s -> s {isWindowResize = Nothing}) resize tryStartWindowResize
  where
    resize wrd = do
      let (nw, nh, nx, ny) = resizeFromEdge wrd (inputMousePos inp) winW winH
          wid = wrdWidget wrd
          key = intKey wid
      modifyStore ctx (insertSlot fieldPoint (slotKey SlotWinSize key) (nw, nh) . insertSlot fieldPoint key (nx, ny))
      withWidgetNode ctx wid () $ \idx -> do
        mpos <- lookupWindowPos ctx wid
        (x, y, _, _) <- getRect (ctxNodeArena ctx) idx
        let ms = contextMeasurers ctx
        placeWindowNode (ctxNodeArena ctx) ms winW winH idx nw nh (const (fromMaybe (x, y) mpos))
      pure wid

-- | Resize edge under @mouse@ for the topmost window whose halo holds it,
-- unless the halo is blocked or the pointer is on one of the window's
-- controls. The top handle reaches over the title bar, which drags elsewhere.
resizeEdgeTarget :: Context -> V2 -> IO (Maybe (NodeIdx, Rect, WindowResizeEdge))
resizeEdgeTarget ctx mouse = runMaybeT $ do
  let na = ctxNodeArena ctx
      inHalo r = rectNonEmpty r && rectContains (rectInflate windowResizeHandleFor r) mouse
  idx <- MaybeT (topmostFloating ctx (== NodeWindow) inHalo)
  rect <- liftIO (getNodeRect na idx)
  -- The halo covers the window interior, so find the edge first and run the
  -- hover probe and node scans only when there is one.
  pad <- liftIO (getPadding na idx)
  edge <- MaybeT (pure (windowResizeEdgeAt pad rect mouse))
  -- The body's scrollbar, while its content overflows, scrolls instead.
  when (rectContains rect mouse) $ do
    onLane <- liftIO $ windowBodyScroller na idx >>= \case
      Nothing -> pure False
      Just ci -> do
        (x, y, w, h) <- getRect na ci
        bodyPad <- getPadding na ci
        contentSize <- getNodeValue na ci
        dir <- getDirection na ci
        pure $
          contentSize > h - padT bodyPad - padB bodyPad
            && rectContains (scrollChromeLane ScrollBarWindow dir x y w h bodyPad) mouse
    guard (not onLane)
  -- The halo must not steal hits from another window's interior or from page
  -- widgets.
  inside <- liftIO (topmostOverlayAtMouse ctx mouse)
  guard (maybe True (== idx) inside)
  hot <- liftIO (probeHotId ctx mouse)
  guard =<< liftIO (withWidgetNode ctx hot True (\hotIdx -> nodeInSubtree ctx hotIdx idx))
  guard . not =<< liftIO (windowControlAt ctx idx mouse)
  pure (idx, rect, edge)

tryStartWindowResize :: Context -> V2 -> IO Bool
tryStartWindowResize ctx mouse@(V2 mx my) = fmap isJust . runMaybeT $ do
  (idx, Rect x y w h, edge) <- MaybeT (resizeEdgeTarget ctx mouse)
  liftIO $ do
    let na = ctxNodeArena ctx
    wid <- getWidgetId na idx
    AxisSizing _ _ minW maxW <- getWidthSizing na idx
    AxisSizing _ _ minH maxH <- getHeightSizing na idx
    let drag = WindowResizeDrag wid edge mx my x y w h minW minH maxW maxH
    modifyInteraction ctx (\s -> s {isWindowResize = Just drag})
    markDirty ctx

-- | Cursor for the held resize edge or an unblocked hovered edge. 'Nothing'
-- leaves cursor selection to other controls.
windowResizeCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
windowResizeCursorKind ctx inp =
  getsInteraction ctx isWindowResize >>= \case
    Just wrd
      | inputMouseDown inp -> pure (Just (cursorForResizeEdge (wrdEdge wrd)))
      | otherwise -> pure Nothing
    Nothing -> fmap (\(_, _, edge) -> cursorForResizeEdge edge) <$> resizeEdgeTarget ctx (inputMousePos inp)

tryStartWindowDrag :: Context -> V2 -> IO Bool
tryStartWindowDrag ctx mouse@(V2 mx my) = fmap isJust . runMaybeT $ do
  idx <- MaybeT (topmostOverlayAtMouse ctx mouse)
  nt <- liftIO (getNodeType (ctxNodeArena ctx) idx)
  guard (nt == NodeWindow)
  title <- MaybeT (windowTitleRect ctx idx)
  guard (rectContains title mouse)
  guard . not =<< liftIO (windowControlAt ctx idx mouse)
  liftIO $ do
    wid <- getWidgetId (ctxNodeArena ctx) idx
    Rect wx wy _ _ <- getNodeRect (ctxNodeArena ctx) idx
    modifyInteraction ctx (\s -> s {isWindowDrag = Just (wid, mx - wx, my - wy)})
    markDirty ctx

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
          here@(Rect _ y _ _) <- getNodeRect (ctxNodeArena ctx) ci
          ns <- getNextSibling (ctxNodeArena ctx) ci
          go ns $ case best of
            Just b@(Rect _ by _ _) | y >= by -> Just b
            _ -> Just here

windowControlAt :: Context -> NodeIdx -> V2 -> IO Bool
windowControlAt ctx idx mouse =
  maybe (pure False) (widgetIdInSubtree ctx idx) =<< findTopWidgetUnderMouse ctx mouse isWidgetNode

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
    , msWrap = cachedWrapText ctx
    }
