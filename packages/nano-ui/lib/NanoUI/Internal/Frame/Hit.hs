-- | Hit tests against the node arena: finding a widget's node, testing a point
-- against a node's rect and clip, and deciding which nodes the modals and
-- floating panels leave reachable by the pointer.
module NanoUI.Internal.Frame.Hit
  ( findNodeByWidgetId
  , withWidgetNode
  , nodeInSubtree
  , widgetIdInSubtree
  , overlayHitAllowed
  , overlayHitRoot
  , topmostOverlayAtMouse
  , topmostModalAtMouse
  , topmostFloating
  , widgetOverlayAllowed
  , nodeOwnsPointer
  , nodePointVisible
  , nodeClippedHit
  , nodeInteractionHit
  , innermostHit
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Maybe (isJust)
import NanoUI.Internal.Context
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Monad ((<&&>))
import NanoUI.Internal.Types (Rect (..), V2 (..), rectContains, rectHit)
import NanoUI.Internal.WidgetText (containerFlagInert, hasFlag)

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

-- | The floating panel that the pointer at @mouse@ is confined to, for
-- 'overlayHitAllowed'. While a modal is open it is the top modal. Otherwise
-- it is the topmost window or popup under the point. 'Nothing' anywhere
-- else. A caller that tests many nodes looks it up once.
overlayHitRoot :: Context -> V2 -> IO (Maybe NodeIdx)
overlayHitRoot ctx mouse = do
  -- Most frames have no floating panel, and then nothing needs looking up.
  floating <- floatingNodeCount (ctxNodeArena ctx)
  if floating <= 0
    then pure Nothing
    else
      runMaybeT $
        MaybeT (topModalNode (ctxNodeArena ctx))
          <|> MaybeT (topmostOverlayAtMouse ctx mouse)

-- | Whether the pointer can reach node @idx@ past the floating panels: every
-- node can when the 'overlayHitRoot' is 'Nothing', otherwise only the nodes
-- inside it.
overlayHitAllowed :: Context -> Maybe NodeIdx -> NodeIdx -> IO Bool
overlayHitAllowed ctx top idx = maybe (pure True) (nodeInSubtree ctx idx) top

-- | The window or popup on top at @mouse@: the last one in arena order whose
-- rect holds the point.
topmostOverlayAtMouse :: Context -> V2 -> IO (Maybe NodeIdx)
topmostOverlayAtMouse ctx mouse =
  topmostFloating ctx (\nt -> nt == NodeWindow || nt == NodePopup) (`rectHit` mouse)

-- | The modal on top at @mouse@: the last one in arena order whose rect holds
-- the point.
topmostModalAtMouse :: Context -> V2 -> IO (Maybe NodeIdx)
topmostModalAtMouse ctx mouse = topmostFloating ctx (== NodeModal) (`rectHit` mouse)

-- | The last floating node in arena order whose type satisfies @wanted@ and
-- whose rect satisfies @at@. The frame paints the panels of one type in arena
-- order, so among them the last one is on top.
topmostFloating :: Context -> (NodeType -> Bool) -> (Rect -> Bool) -> IO (Maybe NodeIdx)
topmostFloating ctx wanted at =
  findClassNodeRevM (ctxNodeArena ctx) FloatingNodes $ \idx ->
    (wanted <$> getNodeType (ctxNodeArena ctx) idx) <&&> (at <$> getNodeRect (ctxNodeArena ctx) idx)

-- | Whether the frame routed the pointer to node @idx@, which decides whether
-- its widget saw the pointer while the view ran. The route must be the node's
-- layer: the key of the nearest floating panel at or above the node, or 0 for
-- the page. While a modal is open, the node must also be inside the top
-- modal. With no modal in the arena, one that was open on the previous frame
-- blocks every node, as it did while the view ran
-- ('NanoUI.Internal.Context.pointerBlockedByModal').
nodeOwnsPointer :: Context -> NodeIdx -> IO Bool
nodeOwnsPointer ctx@Context {ctxNodeArena = na} idx =
  getsInteraction ctx isPointerRoute >>= \case
    RouteLayer routed -> do
      layer <- layerOf idx
      pure (layer == routed)
        <&&> (maybe (not <$> modalActive ctx) (nodeInSubtree ctx idx) =<< topModalNode na)
    _ -> pure False
 where
  layerOf i = maybe 0 intKey <$> walkFloatingAncestors na i (\j _ -> Just <$> getWidgetId na j)

-- | Whether widget @wid@ may show and use a dropdown or menu of its own. With
-- no modal open it always may. While one is open, only a widget inside the
-- top modal may.
widgetOverlayAllowed :: Context -> WidgetId -> IO Bool
widgetOverlayAllowed ctx wid = do
  top <- topModalNode (ctxNodeArena ctx)
  maybe (pure True) (\modal -> widgetIdInSubtree ctx modal wid) top

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
nodeClippedHit ctx@Context {ctxNodeArena = na} idx rect mouse =
  pure (rectHit rect mouse) <&&> do
    mLive <- getClipRect na idx
    mClip <- case mLive of
      Just _ -> pure mLive
      Nothing -> getPrevClipRect ctx =<< getWidgetId na idx
    pure (maybe True (`rectContains` mouse) mClip)

-- | The hit test widgets use while the view runs, when this frame's layout is
-- not solved. @rect@ is the widget's rect from the previous frame
-- ('NanoUI.Internal.Context.getPrevRect'). The point must be inside it, and inside the previous
-- frame's viewport of every scroll container above node @idx@, so content
-- scrolled out of view takes no input; a scroll container with no recorded
-- viewport does not constrain the point. A widget drawn inside another widget
-- takes no input outside that widget's previous rect either. The node's own
-- clip rect is not read: it is not set until
-- 'NanoUI.Internal.Frame.Scroll.applyScrollOffsets' runs.
{-# INLINE nodeInteractionHit #-}
nodeInteractionHit :: Context -> NodeIdx -> Rect -> V2 -> IO Bool
nodeInteractionHit ctx@Context {ctxNodeArena = na} idx rect mouse
  | not (rectHit rect mouse) = pure False
  | idx <= 0 = pure True
  | otherwise = getParent na idx >>= inside True
 where
  -- Whether the mouse is inside the recorded viewport of every scroll
  -- container from node @i@ up, and, while @byWidgets@, inside the previous
  -- rect of every widget. A floating panel escapes the widget it is declared
  -- in (the root can be one), so widgets above one do not bound it.
  inside byWidgets i
    | i < 0 = pure True
    | otherwise = do
        nt <- getNodeType na i
        bounds <-
          if nt == NodeScrollContainer
            then getPrevClipRect ctx =<< getWidgetId na i
            else
              if byWidgets && isWidgetNode nt
                then getPrevRect ctx =<< getWidgetId na i
                else pure Nothing
        case bounds of
          Just r | not (rectContains r mouse) -> pure False
          _ -> getParent na i >>= inside (byWidgets && not (isFloatingNode nt))

-- | The widget a pointer hit on widget @idx@ lands on: its first enabled
-- descendant widget that @hits@, painted over it, and so on inward, skipping
-- inert containers ('containerFlagInert').
innermostHit :: Context -> (NodeIdx -> IO Bool) -> NodeIdx -> IO NodeIdx
innermostHit ctx@Context {ctxNodeArena = na} hits idx =
  maybe (pure idx) (innermostHit ctx hits) =<< firstHitIn idx
 where
  -- Depth first in declaration order, past widgets that miss.
  firstHitIn i = flowChildrenInOrder na i >>= firstJust
  firstJust [] = pure Nothing
  firstJust (d : ds) = do
    nt <- getNodeType na d
    si <- getStyleIdx na d
    found <-
      if nt == NodeContainer && hasFlag containerFlagInert si
        then pure Nothing
        else do
          here <-
            pure (isWidgetNode nt)
              <&&> hits d
              <&&> (not <$> (isDisabled ctx =<< getWidgetId na d))
          if here then pure (Just d) else firstHitIn d
    maybe (firstJust ds) (pure . Just) found
