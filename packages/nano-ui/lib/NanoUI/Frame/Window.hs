{-# LANGUAGE DataKinds #-}

-- | Floating window input: dragging by the title bar, edge and inner-east
-- resizing, the resize cursor, and persisting window placement.
module NanoUI.Frame.Window
  ( lookupWindowPos
  , lookupWindowSize
  , persistWindowPositions
  , updateWindowDrag
  , updateWindowResize
  , WindowResizeEdge (..)
  , windowResizeCursorKind
  ) where

import Control.Monad (when)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe, isJust)
import NanoUI.Context
  ( Context (..)
  , WidgetStore (..)
  , WindowResizeDrag (..)
  , WindowResizeEdge (..)
  , damageWidget
  , getStore
  , getWindowDrag
  , getWindowResize
  , intKey
  , markDirty
  , setStore
  , slotKey
  , Slot (..)
  , InteractionState (..)
  , modifyInteraction
  )
import NanoUI.Font (ScrollBarSlot (..))
import NanoUI.Frame.Hit (findNodeByWidgetId, nodeInSubtree, topmostOverlayAtMouse)
import NanoUI.Frame.Input (findTopWidgetUnderMouse, isInteractiveNode)
import NanoUI.Frame.Redraw (probeHotId)
import NanoUI.Frame.Scroll.Geometry (scrollChromeLane)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), UiCursorKind (..), inputMouseDown, inputMousePos, inputMousePressed)
import NanoUI.Layout.Arena
  ( NodeIdx
  , NodeType (..)
  , findChildM
  , findNodeRevM
  , foldNodesM
  , getDirection
  , getFirstChild
  , getMinMax
  , getNextSibling
  , getNodeType
  , getNodeValue
  , getPadding
  , getRect
  , getWidgetId
  )
import NanoUI.Layout.Solve (placeWindowNode, scrollBarSlotOf)
import NanoUI.Style (Padding (..))
import NanoUI.Types (DamageBounds (..), Rect (..), V2 (..), haloDamageSlop, rectContains, rectInflate)

topmostWindowAtResizeHalo :: Context -> V2 -> IO (Maybe NodeIdx)
topmostWindowAtResizeHalo ctx mouse =
  findNodeRevM (ctxNodeArena ctx) $ \idx -> do
    nt <- getNodeType (ctxNodeArena ctx) idx
    if nt /= NodeWindow
      then pure False
      else do
        (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
        if w <= 0 || h <= 0
          then pure False
          else do
            let rect = Rect x y w h
            if rectContains (rectInflate windowResizeHandleFor rect) mouse
              then pure True
              else windowInnerEastResizeHit ctx idx rect mouse

windowInnerEastResizeHit :: Context -> NodeIdx -> Rect -> V2 -> IO Bool
windowInnerEastResizeHit ctx winIdx (Rect x _ w _) mouse@(V2 mx _) = do
  pad <- getPadding (ctxNodeArena ctx) winIdx
  if mx < x + w - padR pad || mx > x + w
    then pure False
    else do
      mLane <- windowBodyScrollLane ctx winIdx
      pure (not (maybe False (`rectContains` mouse) mLane))

lookupWindowPos :: Context -> WidgetId -> IO (Maybe (Float, Float))
lookupWindowPos ctx wid = do
  store <- getStore ctx
  pure (IM.lookup (intKey wid) (storePoint store))

lookupWindowSize :: Context -> WidgetId -> IO (Maybe (Float, Float))
lookupWindowSize ctx wid = do
  store <- getStore ctx
  pure (IM.lookup (slotKey SlotWinSize (intKey wid)) (storePoint store))

persistWindowPositions :: Context -> IO ()
persistWindowPositions ctx = do
  store0 <- getStore ctx
  let na = ctxNodeArena ctx
      record acc idx = do
        nt <- getNodeType na idx
        if nt /= NodeWindow
          then pure acc
          else do
            wid <- getWidgetId na idx
            (x, y, w, h) <- getRect na idx
            let k = intKey wid
                sizeKey = slotKey SlotWinSize k
                points = storePoint acc
            -- Keep an unchanged map as is, so the store comparison below
            -- short-circuits on pointer equality.
            pure $
              if IM.lookup k points == Just (x, y) && IM.lookup sizeKey points == Just (w, h)
                then acc
                else acc {storePoint = IM.insert k (x, y) (IM.insert sizeKey (w, h) points)}
  store1 <- foldNodesM na record store0
  when (store1 /= store0) $ setStore ctx store1

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
              store <- getStore ctx
              setStore ctx (store {storePoint = IM.insert (intKey wid) (mx - gx, my - gy) (storePoint store)})
              damageWidget ctx wid (DamageInflated haloDamageSlop)
              markDirty ctx
              pure True
          | otherwise -> do
              modifyInteraction ctx (\s -> s {isWindowDrag = Nothing})
              pure False
        Nothing
          | inputMousePressed inp -> tryStartWindowDrag ctx (inputMousePos inp)
          | otherwise -> pure False

windowResizeHandleFor :: Float
windowResizeHandleFor = 12

-- Handles sit outside the window. The right pad strip also resizes beside the bar.
windowResizeEdgeAt :: Rect -> V2 -> Maybe WindowResizeEdge
windowResizeEdgeAt (Rect x y w h) (V2 mx my) =
  let s = windowResizeHandleFor
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

innerEastCornerEdge :: Padding -> Rect -> Float -> WindowResizeEdge
innerEastCornerEdge pad (Rect _ y _ h) my =
  let s = windowResizeHandleFor
      minBand = 6
      topBand = max minBand (min s (padT pad))
      botBand = max minBand (min s (padB pad))
   in if my >= y && my < y + topBand
        then ResizeNE
        else if my > y + h - botBand && my <= y + h then ResizeSE else ResizeE

-- | Lane of the window body's scrollbar while its content overflows.
windowBodyScrollLane :: Context -> NodeIdx -> IO (Maybe Rect)
windowBodyScrollLane ctx winIdx = do
  let na = ctxNodeArena ctx
  mBody <-
    findChildM na winIdx $ \ci -> do
      nt <- getNodeType na ci
      if nt /= NodeScrollContainer
        then pure False
        else do
          slot <- scrollBarSlotOf na ci
          if slot /= ScrollBarWindow
            then pure False
            else do
              (_, _, _, h) <- getRect na ci
              pad <- getPadding na ci
              contentSize <- getNodeValue na ci
              pure (contentSize > h - padT pad - padB pad)
  traverse
    ( \ci -> do
        (x, y, w, h) <- getRect na ci
        pad <- getPadding na ci
        dir <- getDirection na ci
        pure (scrollChromeLane ScrollBarWindow dir x y w h pad)
    )
    mBody

windowInnerResizeEdgeAt :: Context -> NodeIdx -> Rect -> V2 -> IO (Maybe WindowResizeEdge)
windowInnerResizeEdgeAt ctx winIdx winRect@(Rect x y w h) mouse@(V2 mx my) = do
  hit <- windowInnerEastResizeHit ctx winIdx winRect mouse
  if hit
    then do
      pad <- getPadding (ctxNodeArena ctx) winIdx
      pure (Just (innerEastCornerEdge pad winRect my))
    else do
      let cornerW = min 16 (w / 3)
          cornerH = min 16 (h / 3)
          botH = min 6 (h / 3)
          inBotRightCorner = mx >= x + w - cornerW && mx <= x + w && my >= y + h - cornerH && my <= y + h
          inBotEdge = mx >= x && mx <= x + w && my >= y + h - botH && my <= y + h
      pure $
        if inBotRightCorner
          then Just ResizeSE
          else if inBotEdge then Just ResizeS else Nothing

windowResizeEdgeFor :: Context -> NodeIdx -> Rect -> V2 -> IO (Maybe WindowResizeEdge)
windowResizeEdgeFor ctx winIdx winRect mouse =
  case windowResizeEdgeAt winRect mouse of
    Just edge -> pure (Just edge)
    Nothing -> windowInnerResizeEdgeAt ctx winIdx winRect mouse

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
      !w = max minW (min maxW w0)
      !h = max minH (min maxH h0)
      !x0 = if fromW then right0 - w else wrdStartX wrd
      !y0 = if fromN then bottom0 - h else wrdStartY wrd
      !x = max 0 (min x0 (max 0 (winW - w)))
      !y = max 0 (min y0 (max 0 (winH - h)))
   in (w, h, x, y)

updateWindowResize :: Context -> Input -> Float -> Float -> IO Bool
updateWindowResize ctx inp winW winH = do
  drag <- getWindowResize ctx
  case drag of
    Just wrd
      | inputMouseDown inp -> do
          let (nw, nh, nx, ny) = resizeFromEdge wrd (inputMousePos inp) winW winH
              key = intKey (wrdWidget wrd)
          store <- getStore ctx
          setStore ctx (store {storePoint = IM.insert (slotKey SlotWinSize key) (nw, nh) (IM.insert key (nx, ny) (storePoint store))})
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
  mIdx <- findNodeByWidgetId ctx wid
  case mIdx of
    Nothing -> pure ()
    Just idx -> do
      mpos <- lookupWindowPos ctx wid
      (x, y, _, _) <- getRect (ctxNodeArena ctx) idx
      placeWindowNode (ctxNodeArena ctx) (ctxFontMetrics ctx) winW winH idx nw nh (const (fromMaybe (x, y) mpos))

-- | Resize edge under @mouse@ for the topmost window whose halo holds it,
-- unless the halo is blocked or the pointer is on the title bar or one of its
-- controls.
resizeEdgeTarget :: Context -> V2 -> IO (Maybe (NodeIdx, Rect, WindowResizeEdge))
resizeEdgeTarget ctx mouse = do
  mWin <- topmostWindowAtResizeHalo ctx mouse
  case mWin of
    Nothing -> pure Nothing
    Just idx -> do
      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
      let rect = Rect x y w h
      -- The halo covers the window interior, so find the edge first and run
      -- the hover probe and node scans only when there is one.
      mEdge <- windowResizeEdgeFor ctx idx rect mouse
      case mEdge of
        Nothing -> pure Nothing
        Just edge -> do
          mTitle <- windowTitleRect ctx idx
          if maybe False (`rectContains` mouse) mTitle
            then pure Nothing
            else do
              blocked <- resizeHaloBlocked ctx mouse idx
              overControl <- if blocked then pure False else windowTitleHasInteractive ctx idx mouse
              pure (if blocked || overControl then Nothing else Just (idx, rect, edge))

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
      if hashWidgetId hot == 0
        then pure False
        else do
          mHot <- findNodeByWidgetId ctx hot
          case mHot of
            Nothing -> pure False
            Just hotIdx -> not <$> nodeInSubtree ctx hotIdx winIdx

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
          overClose <- windowTitleHasInteractive ctx idx mouse
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
  pure $ case mBest of
    Nothing -> Nothing
    Just (Rect cx cy cw ch) ->
      let topY = min wy cy
       in Just (Rect cx topY cw ((cy - topY) + ch))
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

windowTitleHasInteractive :: Context -> NodeIdx -> V2 -> IO Bool
windowTitleHasInteractive ctx idx mouse = do
  mWid <- findTopWidgetUnderMouse ctx mouse isInteractiveNode
  case mWid of
    Nothing -> pure False
    Just wid -> do
      mNode <- findNodeByWidgetId ctx wid
      maybe (pure False) (\wi -> nodeInSubtree ctx wi idx) mNode
