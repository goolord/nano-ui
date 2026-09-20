{-# LANGUAGE DataKinds #-}

-- | Layout hit testing for modals, windows, and overlay stacking.
module NanoUI.Frame.Hit
  ( findNodeByWidgetId
  , withWidgetNode
  , findNodeByKey
  , modalTreeOpen
  , nodeInSubtree
  , widgetIdInSubtree
  , overlayHitAllowed
  , topmostOverlayAtMouse
  , topmostModalAtMouse
  , widgetOverlayAllowed
  , nodeOwnsPointer
  , scrollHitRect
  , nodePointVisible
  , nodeClippedHit
  , nodeInteractionHit
  ) where

import Data.Maybe (isJust)
import NanoUI.Context (Context (..), PointerRoute (..), getPointerRoute, getPrevClipRect, getPrevRect, intKey, modalActive)
import NanoUI.Id (WidgetId)
import NanoUI.Layout.Arena
  ( NodeIdx
  , NodeType (NodeModal, NodePopup, NodeScrollContainer, NodeWindow)
  , findNodeRevM
  , getClipRect
  , getNodeRect
  , getNodeType
  , getParent
  , getRect
  , getWidgetId
  , isFloatingNode
  , lookupNodeByKey
  , lookupNodeByWidgetId
  , topModalNode
  , walkAncestors
  )
import NanoUI.Monad ((<&&>))
import NanoUI.Types (Rect (..), V2 (..), rectContains, rectH, rectHit, rectW)

findNodeByWidgetId :: Context -> WidgetId -> IO (Maybe NodeIdx)
findNodeByWidgetId ctx wid = lookupNodeByWidgetId (ctxNodeArena ctx) wid

-- | Run @k@ on the node a widget id is on this frame, or give @def@ when it
-- is on none.
{-# INLINE withWidgetNode #-}
withWidgetNode :: Context -> WidgetId -> a -> (NodeIdx -> IO a) -> IO a
withWidgetNode ctx wid def k = findNodeByWidgetId ctx wid >>= maybe (pure def) k

findNodeByKey :: Context -> Int -> IO (Maybe NodeIdx)
findNodeByKey ctx k = lookupNodeByKey (ctxNodeArena ctx) k

modalTreeOpen :: Context -> IO Bool
modalTreeOpen ctx = do
  top <- topModalNode (ctxNodeArena ctx)
  pure (isJust top)

nodeInSubtree :: Context -> NodeIdx -> NodeIdx -> IO Bool
nodeInSubtree ctx idx top =
  isJust <$> walkAncestors (ctxNodeArena ctx) idx (\i -> pure (if i == top then Just () else Nothing))

-- | Membership predicate for an already-resolved subtree root. Callers
-- filtering many widgets can resolve the root once for the whole operation.
widgetIdInSubtree :: Context -> NodeIdx -> WidgetId -> IO Bool
widgetIdInSubtree ctx root wid = do
  node <- findNodeByWidgetId ctx wid
  maybe (pure False) (\idx -> nodeInSubtree ctx idx root) node

overlayHitAllowed :: Context -> NodeIdx -> V2 -> IO Bool
overlayHitAllowed ctx idx mouse = do
  mModal <- topModalNode (ctxNodeArena ctx)
  case mModal of
    Just top -> nodeInSubtree ctx idx top
    Nothing -> do
      mTop <- topmostOverlayAtMouse ctx mouse
      case mTop of
        Nothing -> pure True
        Just tidx -> nodeInSubtree ctx idx tidx

topmostOverlayAtMouse :: Context -> V2 -> IO (Maybe NodeIdx)
topmostOverlayAtMouse ctx mouse =
  topmostFloatingAtMouse ctx mouse (\nt -> nt == NodeWindow || nt == NodePopup)

topmostModalAtMouse :: Context -> V2 -> IO (Maybe NodeIdx)
topmostModalAtMouse ctx mouse =
  topmostFloatingAtMouse ctx mouse (== NodeModal)

topmostFloatingAtMouse :: Context -> V2 -> (NodeType -> Bool) -> IO (Maybe NodeIdx)
topmostFloatingAtMouse ctx mouse wanted =
  findNodeRevM (ctxNodeArena ctx) $ \idx ->
    (wanted <$> getNodeType (ctxNodeArena ctx) idx) <&&> ((`rectHit` mouse) <$> getNodeRect (ctxNodeArena ctx) idx)

-- | Whether the view saw the pointer where node @idx@ was declared: the frame
-- routed it to the node's layer (its nearest floating ancestor, or the page),
-- and the modal on top, if one is up or was last frame, has the node inside it.
nodeOwnsPointer :: Context -> NodeIdx -> IO Bool
nodeOwnsPointer ctx idx =
  getPointerRoute ctx >>= \case
    RouteLayer routed -> do
      layer <- layerOf idx
      if layer /= routed
        then pure False
        else maybe (not <$> modalActive ctx) (nodeInSubtree ctx idx) =<< topModalNode na
    _ -> pure False
  where
    na = ctxNodeArena ctx
    layerOf i
      | i < 0 = pure 0
      | otherwise = do
          nt <- getNodeType na i
          if isFloatingNode nt
            then intKey <$> getWidgetId na i
            else layerOf =<< getParent na i

widgetOverlayAllowed :: Context -> WidgetId -> IO Bool
widgetOverlayAllowed ctx wid = do
  top <- topModalNode (ctxNodeArena ctx)
  case top of
    Nothing -> pure True
    Just modal -> widgetIdInSubtree ctx modal wid

-- Prev rects are visual space (snapshot after applyScrollOffsets).
scrollHitRect :: Context -> WidgetId -> IO (Maybe Rect)
scrollHitRect = getPrevRect

{-# INLINE nodePointVisible #-}
nodePointVisible :: Context -> NodeIdx -> V2 -> IO Bool
nodePointVisible ctx idx mouse = do
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  let vis = Rect x y w h
  if not (w > 0 && h > 0 && rectContains vis mouse)
    then pure False
    else do
      mClip <- getClipRect (ctxNodeArena ctx) idx
      pure (maybe True (`rectContains` mouse) mClip)

{-# INLINE nodeClippedHit #-}
nodeClippedHit :: Context -> NodeIdx -> Rect -> V2 -> IO Bool
nodeClippedHit ctx idx rect mouse = do
  if not (rectW rect > 0 && rectH rect > 0 && rectContains rect mouse)
    then pure False
    else do
      na <- pure (ctxNodeArena ctx)
      mLive <- getClipRect na idx
      mClip <-
        case mLive of
          Just r -> pure (Just r)
          Nothing -> do
            wid <- getWidgetId na idx
            getPrevClipRect ctx wid
      pure (maybe True (`rectContains` mouse) mClip)

-- | Hit test during UI build (before applyScrollOffsets). Uses prev rects and
-- scroll viewport clips only, not per-node live clips.
{-# INLINE nodeInteractionHit #-}
nodeInteractionHit :: Context -> NodeIdx -> Rect -> V2 -> IO Bool
nodeInteractionHit ctx idx rect mouse = do
  if not (rectW rect > 0 && rectH rect > 0 && rectContains rect mouse)
    then pure False
    else scrollViewportHit ctx idx mouse

scrollViewportHit :: Context -> NodeIdx -> V2 -> IO Bool
scrollViewportHit ctx idx mouse = go idx
  where
    go i
      | i <= 0 = pure True
      | otherwise = do
          p <- getParent (ctxNodeArena ctx) i
          if p < 0
            then pure True
            else do
              nt <- getNodeType (ctxNodeArena ctx) p
              if nt == NodeScrollContainer
                then do
                  wid <- getWidgetId (ctxNodeArena ctx) p
                  mClip <- getPrevClipRect ctx wid
                  case mClip of
                    Nothing -> go p
                    Just clip ->
                      if rectContains clip mouse then go p else pure False
                else go p
