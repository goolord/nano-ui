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
  , topmostWindowAtMouse
  , widgetOverlayAllowed
  , widgetIdInModal
  , ancestorScrollShift
  , scrollHitRect
  ) where

import Data.IORef (readIORef)
import Data.Maybe (isJust)
import qualified Data.HashTable.IO as HT
import NanoUI.Context (Context (..), getPrevRect, getScrollOffset)
import NanoUI.Id (WidgetId)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (NodeModal, NodeWindow)
  , findNodeRevM
  , getDirection
  , getNodeType
  , getParent
  , getRect
  , getWidgetId
  , isScrollNode
  , lookupNodeByKey
  , naIndex
  )
import NanoUI.Types (Rect (..), V2 (..), rectContains)

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
      mWin <- topmostWindowAtMouse ctx mouse
      case mWin of
        Nothing -> pure True
        Just widx -> nodeInSubtree ctx idx widx

topmostWindowAtMouse :: Context -> V2 -> IO (Maybe NodeIdx)
topmostWindowAtMouse ctx mouse =
  findNodeRevM (ctxNodeArena ctx) $ \idx -> do
    nt <- getNodeType (ctxNodeArena ctx) idx
    if nt /= NodeWindow
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

ancestorScrollShift :: Context -> NodeIdx -> IO (Float, Float)
ancestorScrollShift ctx idx = go idx (0, 0)
  where
    go i (sx, sy)
      | i <= 0 = pure (sx, sy)
      | otherwise = do
          p <- getParent (ctxNodeArena ctx) i
          if p < 0
            then pure (sx, sy)
            else do
              (sx', sy') <- parentScrollShift ctx p (sx, sy)
              go p (sx', sy')

-- Prev rects are stored in layout space. Shift by live scroll before hit tests.
scrollHitRect :: Context -> WidgetId -> IO (Maybe Rect)
scrollHitRect ctx wid = do
  mIdx <- findNodeByWidgetId ctx wid
  mprev <- getPrevRect ctx wid
  case (mIdx, mprev) of
    (Just idx, Just (Rect x y w h)) -> do
      (dx, dy) <- ancestorScrollShift ctx idx
      pure (Just (Rect (x + dx) (y + dy) w h))
    _ -> pure mprev

parentScrollShift :: Context -> NodeIdx -> (Float, Float) -> IO (Float, Float)
parentScrollShift ctx p (sx, sy) = do
  nt <- getNodeType (ctxNodeArena ctx) p
  if isScrollNode nt
    then do
      wid <- getWidgetId (ctxNodeArena ctx) p
      off <- getScrollOffset ctx wid
      dir <- getDirection (ctxNodeArena ctx) p
      pure $
        case dir of
          DirColumn -> (sx, sy - off)
          DirRow -> (sx - off, sy)
    else pure (sx, sy)
