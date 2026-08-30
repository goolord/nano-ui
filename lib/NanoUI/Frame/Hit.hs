{-# LANGUAGE DataKinds #-}

-- | Layout hit testing for modals, windows, and overlay stacking.
module NanoUI.Frame.Hit
  ( findNodeByWidgetId
  , modalTreeOpen
  , topmostModalIdx
  , nodeInTopmostModal
  , nodeInSubtree
  , modalHitAllowed
  , overlayHitAllowed
  , topmostWindowAtMouse
  , widgetOverlayAllowed
  , widgetIdInModal
  ) where

import Data.IORef (readIORef)
import Data.Maybe (isJust)
import qualified Data.HashTable.IO as HT
import NanoUI.Context (Context (..))
import NanoUI.Id (WidgetId)
import NanoUI.Layout.Arena
  ( NodeIdx
  , NodeType (NodeModal, NodeWindow)
  , arenaCount
  , getNodeType
  , getParent
  , getRect
  , naWidgetIndex
  )
import NanoUI.Types (Rect (..), V2 (..), rectContains)

findNodeByWidgetId :: Context -> WidgetId -> IO (Maybe NodeIdx)
findNodeByWidgetId ctx wid = do
  table <- readIORef (naWidgetIndex (ctxNodeArena ctx))
  HT.lookup table wid

modalTreeOpen :: Context -> IO Bool
modalTreeOpen ctx = do
  top <- topmostModalIdx ctx
  pure (isJust top)

topmostModalIdx :: Context -> IO (Maybe NodeIdx)
topmostModalIdx ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  go (count - 1)
  where
    go idx
      | idx < 0 = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt == NodeModal then pure (Just idx) else go (idx - 1)

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
topmostWindowAtMouse ctx mouse = do
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
              if w > 0 && h > 0 && rectContains (Rect x y w h) mouse
                then pure (Just idx)
                else go (idx - 1)

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
