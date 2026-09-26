-- | The layout overlay ('NanoUI.Internal.Context.setExplainLayout'): a
-- one-pixel outline just inside every layout node, coloured by how deep the
-- node is, and a tint over the node under the pointer with its content box
-- outlined. Each layer's outlines are drawn over that layer, the page's over
-- the page and a floating panel's over the panel, so a window hides the
-- outlines of what it covers as it hides those nodes. Where the view marked
-- parts of itself ('esScopes'), only the nodes of those parts are outlined
-- and explained. The overlay only paints: layout and hit tests never see it,
-- and a frame with it off runs none of this.
module NanoUI.Internal.Frame.Explain
  ( explainFrame
  , paintExplainPage
  , paintExplainLayer
  , paintExplainHover
  ) where

import Control.Monad (forM_, when)
import Data.IORef (modifyIORef', readIORef)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import NanoUI.Internal.Context
import NanoUI.Internal.Draw (DrawArena, Layer (..), beginLayer, pushRect)
import NanoUI.Internal.Frame.Hit (topmostFloating)
import NanoUI.Internal.Frame.Node (childPaintClip)
import NanoUI.Internal.Input (Input (..))
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Id (hashWidgetId)
import NanoUI.Internal.Style (Direction (..), Flow (..), Padding (..), Theme, fadeAlpha, themeSeries)
import NanoUI.Internal.Types (Color, Rect (..), Size (..), V2 (..), rectContains, rectHit, rectIntersect)

-- | Work out what the overlay draws this frame, and repaint where that
-- changed: the outlines of every layer, and the highlight under the pointer.
-- The frame's own rect diffs follow widgets only, and a row or column moves
-- without one, so the outlines keep their own. A new node under the pointer
-- also asks for a frame, whose view sees it ('getExplainedNode'). Runs after
-- layout and before the frame's damage is written.
explainFrame :: Context -> Input -> IO ()
explainFrame ctx@Context {ctxNodeArena = na} inp = do
  scopes <- esScopes <$> readIORef (ctxExplain ctx)
  let mouse = inputMousePos inp
      Size w h = inputWindowSize inp
      window = Rect 0 0 w h
      -- Whether node @idx@ is one the overlay shows: in a scope, if the view
      -- marked any.
      inScope idx = null scopes || any (\(from, below) -> idx >= from && idx < below) scopes
      -- Node @idx@'s outline and those inside it, in declaration order, ahead
      -- of @rest@. The children, pinned ones included, are visited last
      -- declared first.
      outlines !depth !clip idx rest = do
        !rect <- getNodeRect na idx
        inner <- childClip ctx idx clip rect
        below <- case inner of
          Nothing -> pure rest
          Just c -> foldPlacedChildrenM na idx (\acc ci -> outlines (depth + 1) c ci acc) rest
        pure (if inScope idx then (rect, clip, depth) : below else below)
      -- The innermost node under the pointer from @idx@ down: the child
      -- drawn on top with a node under it ('firstChildOnTopJustM': an
      -- earlier sibling over a later one, but a later layer over an earlier
      -- one, and a pinned child over the rest), or else @idx@ itself. A child
      -- can paint outside a row or column, so each is searched.
      nodeAt !depth clip idx = do
        rect <- getNodeRect na idx
        inner <- childClip ctx idx clip rect
        deeper <- maybe (pure Nothing) (firstChildOnTopJustM na idx . nodeAt (depth + 1)) inner
        case deeper of
          Just _ -> pure deeper
          Nothing
            | inScope idx && rectHit rect mouse && rectContains clip mouse ->
                Just . (,clip) <$> describeNode na depth idx
            | otherwise -> pure Nothing
  count <- arenaCount na
  -- The layers in the order the frame paints them: the page, then the
  -- windows, the modals and the popups, each kind in arena order. The page's
  -- root is node 0, unless the view has no page and that is a panel.
  floating <-
    foldClassNodeRevM na FloatingNodes (\acc idx -> (: acc) . (,idx) <$> getNodeType na idx) []
  page <- if count > 0 then not . isFloatingNode <$> getNodeType na 0 else pure False
  let roots =
        [(pageLayer, 0) | page]
          ++ [(idx, idx) | nt <- [NodeWindow, NodeModal, NodePopup], (t, idx) <- floating, t == nt]
  layers <- mapM (\(key, root) -> (key,) <$> outlines (0 :: Int) window root []) roots
  route <- getsInteraction ctx isPointerRoute
  hover <- case route of
    -- A dropdown or the text-edit menu is over the pointer, and no node is.
    RouteLayer _ | count > 0 -> do
      let panelAt nt rest = topmostFloating ctx (== nt) (`rectHit` mouse) >>= maybe rest pure
      nodeAt (0 :: Int) window =<< foldr panelAt (pure 0) [NodePopup, NodeModal, NodeWindow]
    _ -> pure Nothing
  ExplainState {esLayers = prevLayers, esHover = prevHover} <- readIORef (ctxExplain ctx)
  let changed (a : as) (b : bs)
        | a == b = changed as bs
        | otherwise = a : b : changed as bs
      changed as bs = as ++ bs
      shown (rect, clip, _) = rectIntersect clip rect
      highlight (node, clip) = rectIntersect clip (explainedRect node)
  mapM_ (damageRect ctx) (mapMaybe shown (changed (concatMap snd prevLayers) (concatMap snd layers)))
  when (hover /= prevHover) $ do
    mapM_ (damageRect ctx) (prevHover >>= highlight)
    mapM_ (damageRect ctx) (hover >>= highlight)
    markDirtyCovered ctx
  modifyIORef' (ctxExplain ctx) (\es -> es {esOn = True, esLayers = layers, esHover = hover})

-- | The key 'esLayers' files the page's outlines under. No node has it, and
-- node 0 can be a floating panel's root, whose outlines go over the panel.
pageLayer :: Int
pageLayer = -1

-- | Draw the outlines 'explainFrame' found for the page.
paintExplainPage :: Context -> IO ()
paintExplainPage ctx = paintExplainLayer ctx pageLayer

-- | Draw the outlines 'explainFrame' found for the floating panel at node
-- @root@.
paintExplainLayer :: Context -> NodeIdx -> IO ()
paintExplainLayer ctx@Context {ctxDrawArena = da} root = do
  colour <- depthColor <$> getTheme ctx
  layers <- esLayers <$> readIORef (ctxExplain ctx)
  forM_ (lookup root layers) $ mapM_ $ \(rect, clip, depth) -> outline da clip rect (colour depth)

-- | Tint the node under the pointer and outline its content box, over
-- everything else the frame draws.
paintExplainHover :: Context -> IO ()
paintExplainHover ctx@Context {ctxDrawArena = da} = do
  hover <- esHover <$> readIORef (ctxExplain ctx)
  forM_ hover $ \(ExplainedNode {explainedDepth = depth, explainedRect = rect@(Rect x y w h), explainedPadding = Padding l r t b}, clip) -> do
    col <- (`depthColor` depth) <$> getTheme ctx
    beginLayer da LayerChrome
    forM_ (rectIntersect clip rect) $ \tint -> pushRect da tint (fadeAlpha col 0x40)
    when (l > 0 || r > 0 || t > 0 || b > 0) $
      outline da clip (Rect (x + l) (y + t) (w - l - r) (h - t - b)) col

-- | The clip node @idx@'s children paint in, when it paints in @clip@ at
-- @rect@ ('childPaintClip'). 'Nothing' when nothing inside shows.
childClip :: Context -> NodeIdx -> Rect -> Rect -> IO (Maybe Rect)
childClip ctx idx clip rect = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  maybe (Just clip) (rectIntersect clip) <$> childPaintClip ctx idx nt rect

-- | What the overlay says of node @idx@, @depth@ deep in its layer.
describeNode :: NodeArena -> Int -> NodeIdx -> IO ExplainedNode
describeNode na depth idx = do
  a <- arenaArrays na
  nt <- readTagEnum a idx TagNodeType
  wid <- getWidgetId na idx
  rect <- getNodeRect na idx
  pad <- getPadding na idx
  wAx <- readAxisSizing a idx True
  hAx <- readAxisSizing a idx False
  gap <- readStyle a idx StyleGap
  dir <- readTagEnum a idx TagDirection
  flow <- readTagEnum a idx TagFlow
  pinned <- readTagEnum a idx TagPinned
  pin <- if pinned then Just <$> (V2 <$> readStyle a idx StylePinX <*> readStyle a idx StylePinY) else pure Nothing
  mode <- readTagEnum a idx TagPointer
  let direction = case dir of DirRow -> Row; DirColumn -> Column
  pure
    ExplainedNode
      { explainedKind = nodeKind nt direction flow
      , explainedWidget = if hashWidgetId wid == 0 then Nothing else Just wid
      , explainedDepth = depth
      , explainedRect = rect
      , explainedPadding = pad
      , explainedWidth = axisSizing wAx
      , explainedHeight = axisSizing hAx
      , explainedMin = V2 (axMin wAx) (axMin hAx)
      , explainedMax = V2 (axMax wAx) (axMax hAx)
      , explainedGap = gap
      , explainedDirection = direction
      , explainedFlow = flow
      , explainedPin = pin
      , explainedPointer = mode
      }

-- | A node's type without its @Node@ prefix, and how a container lays out
-- its children: its direction, @layered@ for layers, and @wrap@ after the
-- direction of a container that wraps.
nodeKind :: NodeType -> Direction -> Flow -> T.Text
nodeKind nt dir flow
  | not (isContainerNode nt) = kind
  | otherwise = kind <> ", " <> case flow of
      Layered -> "layered"
      Wrap -> direction <> ", wrap"
      Line -> direction
  where
    kind = T.pack (drop 4 (show nt))
    direction = T.toLower (T.pack (show dir))

-- | The outline colour at a depth: the theme's series colours in turn.
depthColor :: Theme -> Int -> Color
depthColor theme = \depth -> series !! (depth `rem` length series)
  where
    series = themeSeries theme

-- | A one-pixel border just inside @rect@, cut to @clip@.
outline :: DrawArena -> Rect -> Rect -> Color -> IO ()
outline da clip (Rect x y w h) col =
  forM_ edges $ \edge -> forM_ (rectIntersect clip edge) $ \r -> pushRect da r col
  where
    edges =
      [ Rect x y w 1
      , Rect x (y + h - 1) w 1
      , Rect x (y + 1) 1 (h - 2)
      , Rect (x + w - 1) (y + 1) 1 (h - 2)
      ]
