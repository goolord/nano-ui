{-# LANGUAGE DataKinds #-}

-- | Layout hit testing for modals, windows, and overlay stacking.
module NanoUI.Frame.Hit
  ( findNodeByWidgetId
  , findNodeByKey
  , modalTreeOpen
  , topmostModalIdx
  , nodeInTopmostModal
  , nodeInSubtree
  , modalHitAllowed
  , overlayHitAllowed
  , topmostOverlayAtMouse
  , topmostModalAtMouse
  , topmostWindowAtMouse
  , widgetOverlayAllowed
  , widgetIdInModal
  , ancestorScrollShift
  , scrollHitRect
  , nodePointVisible
  , nodeClippedHit
  , nodeInteractionHit
  ) where

import Data.IORef (readIORef)
import Data.Maybe (isJust)
import qualified Data.HashTable.IO as HT
import NanoUI.Context (Context (..), getPrevRect, getScrollOffset, getPrevClipRect)
import NanoUI.Host (HostProfile, isCellHost)
import NanoUI.Id (WidgetId)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (NodeModal, NodePopup, NodeScrollContainer, NodeWindow)
  , findNodeRevM
  , getDirection
  , getClipRect
  , getNodeType
  , getParent
  , getRect
  , getWidgetId
  , isFloatingNode
  , isScrollNode
  , lookupNodeByKey
  , naIndex
  )
import NanoUI.Types (Rect (..), V2 (..), rectContains, rectH, rectW)

findNodeByWidgetId :: Context -> WidgetId -> IO (Maybe NodeIdx)
findNodeByWidgetId ctx wid = do
  table <- readIORef (naIndex (ctxNodeArena ctx))
  HT.lookup table wid

findNodeByKey :: Context -> Int -> IO (Maybe NodeIdx)
findNodeByKey ctx k = lookupNodeByKey (ctxNodeArena ctx) k

modalTreeOpen :: Context -> IO Bool
modalTreeOpen ctx = do
  top <- topmostModalIdx ctx
  pure (isJust top)

topmostModalIdx :: Context -> IO (Maybe NodeIdx)
topmostModalIdx ctx =
  findNodeRevM (ctxNodeArena ctx) $ \i ->
    (== NodeModal) <$> getNodeType (ctxNodeArena ctx) i

nodeInTopmostModal :: Context -> NodeIdx -> IO Bool
nodeInTopmostModal ctx idx = do
  mTop <- topmostModalIdx ctx
  case mTop of
    Nothing -> pure False
    Just top -> nodeInSubtree ctx idx top

nodeInSubtree :: Context -> NodeIdx -> NodeIdx -> IO Bool
nodeInSubtree ctx idx top = go idx
  where
    go i
      | i < 0 = pure False
      | i == top = pure True
      | otherwise = do
          parent <- getParent (ctxNodeArena ctx) i
          go parent

modalHitAllowed :: Context -> NodeIdx -> IO Bool
modalHitAllowed ctx idx = do
  mTop <- topmostModalIdx ctx
  case mTop of
    Nothing -> pure True
    Just top -> nodeInSubtree ctx idx top

overlayHitAllowed :: Context -> NodeIdx -> V2 -> IO Bool
overlayHitAllowed ctx idx mouse = do
  mModal <- topmostModalIdx ctx
  case mModal of
    Just _ -> modalHitAllowed ctx idx
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

topmostWindowAtMouse :: Context -> V2 -> IO (Maybe NodeIdx)
topmostWindowAtMouse ctx mouse =
  topmostFloatingAtMouse ctx mouse (== NodeWindow)

topmostFloatingAtMouse :: Context -> V2 -> (NodeType -> Bool) -> IO (Maybe NodeIdx)
topmostFloatingAtMouse ctx mouse wanted =
  findNodeRevM (ctxNodeArena ctx) $ \idx -> do
    nt <- getNodeType (ctxNodeArena ctx) idx
    if not (wanted nt)
      then pure False
      else do
        (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
        pure (w > 0 && h > 0 && rectContains (Rect x y w h) mouse)

widgetOverlayAllowed :: Context -> WidgetId -> IO Bool
widgetOverlayAllowed ctx wid = do
  open <- modalTreeOpen ctx
  if not open then pure True else widgetIdInModal ctx wid

widgetIdInModal :: Context -> WidgetId -> IO Bool
widgetIdInModal ctx wid = do
  mIdx <- findNodeByWidgetId ctx wid
  case mIdx of
    Nothing -> pure False
    Just idx -> nodeInTopmostModal ctx idx

-- Prev rects are layout space. Floating nodes are window space after placePopups.
ancestorScrollShift :: Context -> NodeIdx -> IO (Float, Float)
ancestorScrollShift ctx idx = go idx (0, 0)
  where
    go i (sx, sy)
      | i <= 0 = pure (sx, sy)
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) i
          if isFloatingNode nt
            then pure (sx, sy)
            else do
              p <- getParent (ctxNodeArena ctx) i
              if p < 0
                then pure (sx, sy)
                else do
                  (sx', sy') <- parentScrollShift ctx p (sx, sy)
                  go p (sx', sy')

-- Prev rects are visual space; during UI build add live scroll delta since snapshot.
scrollHitRect :: Context -> WidgetId -> IO (Maybe Rect)
scrollHitRect ctx wid = do
  mIdx <- findNodeByWidgetId ctx wid
  mprev <- getPrevRect ctx wid
  case (mIdx, mprev) of
    (Just idx, Just (Rect x y w h)) -> do
      (dx, dy) <- ancestorScrollShift ctx idx
      pure (Just (Rect (x + dx) (y + dy) w h))
    _ -> pure mprev

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
    host = ctxHostProfile ctx
    go i
      | i <= 0 = pure True
      | otherwise = do
          p <- getParent (ctxNodeArena ctx) i
          if p < 0
            then pure True
            else do
              nt <- getNodeType (ctxNodeArena ctx) p
              if scrollViewportGate host nt
                then do
                  wid <- getWidgetId (ctxNodeArena ctx) p
                  mClip <- getPrevClipRect ctx wid
                  case mClip of
                    Nothing -> go p
                    Just clip ->
                      if rectContains clip mouse then go p else pure False
                else go p

-- Desktop modals are not scroll containers; only cell-host modals scroll.
scrollViewportGate :: HostProfile -> NodeType -> Bool
scrollViewportGate host nt =
  case nt of
    NodeScrollContainer -> True
    NodeModal -> isCellHost host
    _ -> False

parentScrollShift :: Context -> NodeIdx -> (Float, Float) -> IO (Float, Float)
parentScrollShift ctx p (sx, sy) = do
  nt <- getNodeType (ctxNodeArena ctx) p
  if isScrollNode nt && not (isCellHost (ctxHostProfile ctx) && nt == NodeModal)
    then do
      wid <- getWidgetId (ctxNodeArena ctx) p
      off <- getScrollOffset ctx wid
      dir <- getDirection (ctxNodeArena ctx) p
      pure $
        case dir of
          DirColumn -> (sx, sy - off)
          DirRow -> (sx - off, sy)
    else pure (sx, sy)
