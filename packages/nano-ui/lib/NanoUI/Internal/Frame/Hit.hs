-- | Hit tests against the node arena: finding a widget's node, testing a point
-- against a node's rect and clip, and deciding which nodes the modals and
-- floating panels leave reachable by the pointer.
module NanoUI.Internal.Frame.Hit
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
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Maybe (isJust)
import NanoUI.Internal.Context
  ( Context (..)
  , PointerRoute (..)
  , getPointerRoute
  , getPrevClipRect
  , getPrevRect
  , intKey
  , modalActive
  )
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Layout.Arena
  ( NodeIdx
  , NodeType (NodeModal, NodePopup, NodeScrollContainer, NodeWindow)
  , findNodeRevM
  , floatingNodeCount
  , getClipRect
  , getNodeRect
  , getNodeType
  , getParent
  , getWidgetId
  , isFloatingNode
  , lookupNodeByKey
  , lookupNodeByWidgetId
  , topModalNode
  , walkAncestors
  )
import NanoUI.Internal.Monad ((<&&>))
import NanoUI.Internal.Types (Rect (..), V2 (..), rectContains, rectHit)

-- | The node that carries widget id @wid@ in this frame's arena. 'Nothing' for
-- @WidgetId 0@ and for a widget the view has not declared this frame. When
-- several nodes carry the id, this is the one most recently indexed under it.
findNodeByWidgetId :: Context -> WidgetId -> IO (Maybe NodeIdx)
findNodeByWidgetId ctx wid = lookupNodeByWidgetId (ctxNodeArena ctx) wid

-- | Run the callback on a widget's current node, or return the supplied
-- default when the id has no node.
{-# INLINE withWidgetNode #-}
withWidgetNode :: Context -> WidgetId -> a -> (NodeIdx -> IO a) -> IO a
withWidgetNode ctx wid def k = findNodeByWidgetId ctx wid >>= maybe (pure def) k

-- | 'findNodeByWidgetId' for a widget's 'Int' key ('intKey').
findNodeByKey :: Context -> Int -> IO (Maybe NodeIdx)
findNodeByKey ctx k = lookupNodeByKey (ctxNodeArena ctx) k

-- | Whether the arena holds a modal node, which it does when the view
-- declared an open modal.
modalTreeOpen :: Context -> IO Bool
modalTreeOpen ctx = isJust <$> topModalNode (ctxNodeArena ctx)

-- | Whether node @idx@ is node @top@ or one of its descendants.
nodeInSubtree :: Context -> NodeIdx -> NodeIdx -> IO Bool
nodeInSubtree ctx idx top =
  isJust
    <$> walkAncestors
      (ctxNodeArena ctx)
      idx
      (\i -> pure (if i == top then Just () else Nothing))

-- | Whether the node of widget @wid@ is node @root@ or one of its
-- descendants. 'False' when the widget has no node this frame. The root is a
-- node index, so a caller that tests many widgets looks it up once.
widgetIdInSubtree :: Context -> NodeIdx -> WidgetId -> IO Bool
widgetIdInSubtree ctx root wid = withWidgetNode ctx wid False (\idx -> nodeInSubtree ctx idx root)

-- | Whether the pointer at @mouse@ can reach node @idx@ past the floating
-- panels. While a modal is open, only the nodes inside the top modal can be
-- reached. Otherwise, when a window or popup lies under the point, only the
-- nodes inside the topmost one can. Anywhere else every node can.
overlayHitAllowed :: Context -> NodeIdx -> V2 -> IO Bool
overlayHitAllowed ctx idx mouse = do
  -- Most frames have no floating panel, and then nothing needs looking up.
  floating <- floatingNodeCount (ctxNodeArena ctx)
  top <-
    if floating <= 0
      then pure Nothing
      else
        runMaybeT $
          MaybeT (topModalNode (ctxNodeArena ctx))
            <|> MaybeT (topmostOverlayAtMouse ctx mouse)
  maybe (pure True) (nodeInSubtree ctx idx) top

-- | The window or popup on top at @mouse@: the last one in arena order whose
-- rect holds the point.
topmostOverlayAtMouse :: Context -> V2 -> IO (Maybe NodeIdx)
topmostOverlayAtMouse ctx mouse =
  topmostFloatingAtMouse ctx mouse (\nt -> nt == NodeWindow || nt == NodePopup)

-- | The modal on top at @mouse@: the last one in arena order whose rect holds
-- the point.
topmostModalAtMouse :: Context -> V2 -> IO (Maybe NodeIdx)
topmostModalAtMouse ctx mouse =
  topmostFloatingAtMouse ctx mouse (== NodeModal)

-- | The last node in arena order whose type satisfies @wanted@ and whose
-- non-empty rect holds @mouse@. The frame paints the panels of one type in
-- arena order, so among them the last one is on top.
topmostFloatingAtMouse ::
  Context -> V2 -> (NodeType -> Bool) -> IO (Maybe NodeIdx)
topmostFloatingAtMouse ctx mouse wanted =
  findNodeRevM (ctxNodeArena ctx) $ \idx ->
    (wanted <$> getNodeType (ctxNodeArena ctx) idx)
      <&&> ((`rectHit` mouse) <$> getNodeRect (ctxNodeArena ctx) idx)

-- | Whether the frame routed the pointer to node @idx@, which decides whether
-- its widget saw the pointer while the view ran. The route must be the node's
-- layer: the key of the nearest floating panel at or above the node, or 0 for
-- the page. While a modal is open, the node must also be inside the top
-- modal. With no modal in the arena, one that was open on the previous frame
-- blocks every node, as it did while the view ran
-- ('NanoUI.Internal.Context.pointerBlockedByModal').
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

-- | Whether widget @wid@ may show and use a dropdown or menu of its own. With
-- no modal open it always may. While one is open, only a widget inside the
-- top modal may.
widgetOverlayAllowed :: Context -> WidgetId -> IO Bool
widgetOverlayAllowed ctx wid = do
  top <- topModalNode (ctxNodeArena ctx)
  maybe (pure True) (\modal -> widgetIdInSubtree ctx modal wid) top

-- | The on-screen rect of widget @wid@ as 'NanoUI.Internal.Damage.updatePrevRects'
-- last recorded it: in window coordinates, with scroll offsets applied. While
-- the view runs, that is the rect from the previous frame, and it is the only
-- geometry a widget has then, because this frame's layout is not solved yet.
-- 'Nothing' when the widget had no node at that time, or an empty rect.
scrollHitRect :: Context -> WidgetId -> IO (Maybe Rect)
scrollHitRect = getPrevRect

-- | Whether @mouse@ is on the visible part of node @idx@: inside its non-empty
-- rect, and inside its clip rect when it has one. It reads this frame's
-- solved geometry, which is complete once
-- 'NanoUI.Internal.Frame.Scroll.applyScrollOffsets' has run.
{-# INLINE nodePointVisible #-}
nodePointVisible :: Context -> NodeIdx -> V2 -> IO Bool
nodePointVisible ctx idx mouse = do
  vis <- getNodeRect (ctxNodeArena ctx) idx
  if not (rectHit vis mouse)
    then pure False
    else do
      mClip <- getClipRect (ctxNodeArena ctx) idx
      pure (maybe True (`rectContains` mouse) mClip)

-- | 'nodePointVisible' with the rect supplied by the caller, for a widget
-- whose hit rect differs from its node's rect. A node with no clip rect of
-- its own, as before 'NanoUI.Internal.Frame.Scroll.applyScrollOffsets' has run in a
-- frame, is tested against the clip recorded for its widget id on the
-- previous frame.
{-# INLINE nodeClippedHit #-}
nodeClippedHit :: Context -> NodeIdx -> Rect -> V2 -> IO Bool
nodeClippedHit ctx idx rect mouse = do
  if not (rectHit rect mouse)
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

-- | The hit test widgets use while the view runs, when this frame's layout is
-- not solved. @rect@ is the widget's rect from the previous frame
-- ('scrollHitRect'). The point must be inside it, and inside the previous
-- frame's viewport of every scroll container above node @idx@, so content
-- scrolled out of view takes no input. The node's own clip rect is not read:
-- it is not set until 'NanoUI.Internal.Frame.Scroll.applyScrollOffsets' runs.
{-# INLINE nodeInteractionHit #-}
nodeInteractionHit :: Context -> NodeIdx -> Rect -> V2 -> IO Bool
nodeInteractionHit ctx idx rect mouse = do
  if not (rectHit rect mouse)
    then pure False
    else scrollViewportHit ctx idx mouse

-- | Whether @mouse@ is inside the previous frame's viewport of every scroll
-- container above node @idx@. A scroll container with no recorded viewport
-- does not constrain the point.
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
