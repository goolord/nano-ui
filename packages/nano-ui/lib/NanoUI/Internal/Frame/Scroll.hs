-- | Scroll input: offsets baked into the arena, wheel routing, and scrollbar
-- thumb drags and track jumps.
module NanoUI.Internal.Frame.Scroll
  ( applyScrollOffsets
  , updateScrollWheel
  , updateScrollDrag
  , refreshScrollBarHover
  , probeScrollBarHover
  , scrollBarsFor
  , scrollBarLayout
  , ScrollBarLayout (..)
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM_, join, unless, when)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust)
import NanoUI.Internal.Context
import NanoUI.Internal.Frame.Hit (overlayHitAllowed, overlayHitRoot, topmostFloating, topmostOverlayAtMouse)
import NanoUI.Internal.Frame.Node (readScrollNode)
import NanoUI.Internal.Frame.Scroll.Geometry
import NanoUI.Internal.Frame.TextArea (TextAreaBars (..), textAreaBarLayouts, textAreaScrollGeom)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Monad ((<&&>))
import NanoUI.Internal.Input
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Style (Flow (..), Padding (..), PointerMode (..), themePanel)
import NanoUI.Internal.Types (Rect (..), Size (..), V2 (..), rectContains, rectHit, rectInflate, rectIntersect, rectUnion)

-- | Move every node by the offsets of the scroll containers around it, and
-- give each its clip. The root is clipped to the window, as paint clips it:
-- a root smaller than its content still draws the overflow.
applyScrollOffsets :: Context -> Size -> IO ()
applyScrollOffsets ctx (Size w h) = do
  beginScrollMetrics ctx
  -- A frame that added no widgets has no root to walk.
  count <- arenaCount (ctxNodeArena ctx)
  when (count > 0) $ transformSubtree ctx 0 0 0 (Rect 0 0 w h)

transformSubtree :: Context -> NodeIdx -> Float -> Float -> Rect -> IO ()
transformSubtree ctx@Context {ctxNodeArena = na} idx scrollX scrollY parentClip = do
  nt <- getNodeType na idx
  Rect lx ly vw vh <- getNodeRect na idx
  let
    floating = isFloatingNode nt
    (sx, sy) = if floating then (0, 0) else (scrollX, scrollY)
    !vx = lx + sx
    !vy = ly + sy
    -- A rect outside the clip around it leaves nothing: its clip is empty,
    -- not the clip around it.
    within r = fromMaybe (Rect (rectX r) (rectY r) 0 0) (rectIntersect parentClip r)
  -- With no offset on either axis the placed rect equals the laid-out one, so
  -- the write is a no-op; a floating node always takes that path.
  unless (sx == 0 && sy == 0) $ setRect na idx vx vy vw vh
  -- Recursing from inside each branch keeps the child transform in registers;
  -- returning it as a tuple allocated one box per node per frame.
  let descend !cx !cy !clip = forChildNodes_ na idx $ \ci -> transformSubtree ctx ci cx cy clip
  if isScrollNode nt
    then do
      (axes, viewport, range) <- scrollNodeGeometry ctx idx (Rect vx vy vw vh)
      wid <- getWidgetId na idx
      -- The only pass that sees a scroller's placed geometry. Everything
      -- that scrolls one between frames reads it back from here.
      cacheScrollMetrics ctx wid axes viewport range
      -- An offset set outright before this layout, or left over from
      -- content that has since shrunk, is held to the range the content
      -- has now, on the axes this scroller owns.
      cur@(V2 cx cy) <- getScrollOffsetIn ctx wid axes
      let
        V2 hx hy = clampScrollOffset range cur
        held@(V2 dx dy) = case axes of
          ScrollAxisY -> V2 cx hy
          ScrollAxisX -> V2 hx cy
          ScrollAxisXY -> V2 hx hy
        clip = within viewport
      when (held /= cur) $ setScrollOffsetIn ctx wid axes held
      setClipRect na idx clip
      descend (sx - dx) (sy - dy) clip
    else do
      clip <-
        case nt of
          NodePanel -> do
            theme <- nodeTheme ctx idx
            pure (within (borderContentClip (themePanel theme) (Rect vx vy vw vh)))
          _ -> pure $! if floating then Rect vx vy vw vh else parentClip
      setClipRect na idx clip
      -- A widget's children, its adornments or content, are clipped to the
      -- widget, as paint clips them, so none takes the pointer outside it.
      kids <- if isWidgetNode nt then getFirstChild na idx else pure (-1)
      descend sx sy (if kids < 0 then clip else within (Rect vx vy vw vh))

-- | Axes, content viewport and reachable offset range of the scroll container
-- at @idx@ placed at @rect@, in window axes. The wheel, the programmatic
-- commands and the transform pass all size a scroll off this, so a scroller
-- cannot disagree with itself about how far it reaches.
scrollNodeGeometry :: Context -> NodeIdx -> Rect -> IO (ScrollAxes, Rect, V2)
scrollNodeGeometry ctx idx (Rect x y w h) = do
  sn@ScrollNode {snPad = pad, snContentMain = contentMain} <-
    readScrollNode (ctxNodeArena ctx) idx
  let
    viewport = scrollNodeViewport sn x y w h
    rangeW content = scrollAxisRange content (rectW viewport) (padR pad)
    rangeH = scrollAxisRange contentMain (rectH viewport) (padB pad)
  pure $
    if sn2D sn
      then (ScrollAxisXY, viewport, V2 (rangeW (snContentW sn)) rangeH)
      else case snDir sn of
        DirColumn -> (ScrollAxisY, viewport, V2 0 rangeH)
        DirRow -> (ScrollAxisX, viewport, V2 (rangeW contentMain) 0)

updateScrollWheel :: Context -> Input -> IO ()
updateScrollWheel ctx inp = do
  let
    scroll@(V2 wheelX wheelY) = inputScroll inp
  -- A wheel over an open dropdown or the text-edit menu never gets here: the
  -- combo widget scrolls its own list, and the scroller under it stays put.
  when (wheelY /= 0 || wheelX /= 0) $ do
    mNode <- findScrollNodeUnderMouse ctx (inputMousePos inp)
    forM_ mNode $ \idx -> do
      wid <- getWidgetId (ctxNodeArena ctx) idx
      applyScrollWheelDelta ctx wid scroll
      applyCrossAxisScroll ctx idx scroll

-- Nested 2D: apply the unused axis to a paired scroller that runs the other
-- way: the nearest one above in the same panel, else the first one inside.
-- Do not walk past panel/window/modal into the page scroller.
applyCrossAxisScroll :: Context -> NodeIdx -> V2 -> IO ()
applyCrossAxisScroll ctx@Context {ctxNodeArena = na} idx scroll = do
  dir <- getDirection na idx
  let
    crossWid i = do
      nt <- getNodeType na i
      hit <- pure (isScrollNode nt) <&&> ((/= dir) <$> getDirection na i)
      if hit then Just <$> getWidgetId na i else pure Nothing
    above i = do
      nt <- getNodeType na i
      if nt == NodePanel || nt == NodeWindow || nt == NodeModal
        then pure (Just Nothing)
        else fmap Just <$> crossWid i
    inside i = firstChildJustM na i $ \ci -> crossWid ci >>= maybe (inside ci) (pure . Just)
  parent <- getParent na idx
  up <- join <$> walkAncestors na parent above
  target <- maybe (inside idx) (pure . Just) up
  forM_ target $ \wid -> applyScrollWheelDelta ctx wid scroll

-- | Node owning scroller @wid@: its text area, or the first scroll container
-- with that id that the predicate does not rule out (table slave panes share
-- an id with their master). Thumb drags use the chrome predicate, since a
-- hidden bar has no lane to grab; the wheel uses the wider one, since a
-- hidden bar still scrolls.
scrollOwnerNode ::
  (ScrollConfig -> Bool -> DirTag -> Bool)
  -> Context
  -> WidgetId
  -> IO (Maybe NodeIdx)
scrollOwnerNode suppressed Context {ctxNodeArena = na} wid =
  findClassNodeM na PointerNodes $ \idx -> do
    nt <- getNodeType na idx
    pure (nt == NodeTextArea || isScrollNode nt)
      <&&> ((== wid) <$> getWidgetId na idx)
      <&&> if nt == NodeTextArea
        then pure True
        else (\sn -> not (suppressed (snConfig sn) (sn2D sn) (snDir sn))) <$> readScrollNode na idx

applyScrollWheelDelta :: Context -> WidgetId -> V2 -> IO ()
applyScrollWheelDelta ctx@Context {ctxNodeArena = na} wid (V2 wheelX wheelY) = do
  mIdx <- scrollOwnerNode scrollWheelSuppressed ctx wid
  forM_ mIdx $ \idx -> do
    nt <- getNodeType na idx
    (axes, range) <-
      if nt == NodeTextArea
        then (ScrollAxisXY,) . tabRange . snd <$> textAreaScrollGeom ctx idx
        else do
          rect <- getNodeRect na idx
          (axes, _, range) <- scrollNodeGeometry ctx idx rect
          pure (axes, range)
    step <- resolveScrollStep ctx wid
    cur <- getScrollOffsetIn ctx wid axes
    -- Notches land on where the scroller is headed, not on where it is, so
    -- a flick mid-glide adds to the throw instead of restarting it.
    base@(V2 baseX baseY) <- scrollTargetOffset ctx wid cur
    let
      next = clampScrollOffset range (V2 (baseX + wheelX * step) (baseY + wheelY * step))
    unless (next == base && next == cur) $
      applyScrollTarget ctx wid axes next ScrollSmooth

findScrollNodeUnderMouse :: Context -> V2 -> IO (Maybe NodeIdx)
findScrollNodeUnderMouse ctx mouse = do
  count <- arenaCount (ctxNodeArena ctx)
  if count <= 0
    then pure Nothing
    else do
      -- The modal on top at the pointer, else the window or popup there.
      top <-
        runMaybeT $
          MaybeT (topmostFloating ctx (== NodeModal) (`rectHit` mouse))
            <|> MaybeT (topmostOverlayAtMouse ctx mouse)
      let start = fromMaybe 0 top
      rect <- getNodeRect (ctxNodeArena ctx) start
      layered <- (> 0) <$> layeredNodeCount (ctxNodeArena ctx)
      queryScrollTarget ctx layered mouse rect start <&> \case
        WheelTo idx -> Just idx
        _ -> Nothing

-- | What a subtree does with the wheel at the pointer.
data WheelHit
  = WheelMiss
  -- ^ Nothing in it takes the wheel there.
  | WheelBlocked
  -- ^ A node given 'PointerBlock' is on top there with no scroller of its
  -- own: nothing drawn beneath it takes the wheel, though a scroller it is
  -- inside still does.
  | WheelTo !NodeIdx
  -- ^ This scroller takes it.

-- | What the subtree at @idx@ does with the wheel at @mouse@: the answer of
-- the child drawn on top there, else @idx@ itself if it is a scroller under
-- the pointer. Where layers or a pinned node draw one child over another
-- (@layered@, and this node layered or above a pinned node), the children
-- are asked in the order paint draws them, the one on top first
-- ('firstChildOnTopJustM'), so a scroller pinned over another takes the
-- wheel; elsewhere children do not overlap, and are asked in the arena's
-- sibling order.
queryScrollTarget :: Context -> Bool -> V2 -> Rect -> NodeIdx -> IO WheelHit
queryScrollTarget ctx@Context {ctxNodeArena = na} layered mouse parentClip idx = do
  nt <- getNodeType na idx
  scrollHitClip ctx idx nt parentClip >>= \case
    Nothing -> pure WheelMiss
    Just clip -> do
      overlapping <-
        pure layered <&&> ((||) <$> ((== Layered) <$> getFlow na idx) <*> hasPinnedBelow na idx)
      let answered c =
            queryScrollTarget ctx layered mouse clip c <&> \case
              WheelMiss -> Nothing
              hit -> Just hit
      inner <-
        fromMaybe WheelMiss <$> (if overlapping then firstChildOnTopJustM else firstChildJustM) na idx answered
      case inner of
        WheelTo _ -> pure inner
        _ ->
          scrollHitSelf ctx idx nt mouse clip >>= \case
            Just self -> pure (WheelTo self)
            Nothing -> do
              blocks <-
                pure layered
                  <&&> ((== PointerBlock) <$> getPointerMode na idx)
                  <&&> (rectHit <$> getNodeRect na idx <*> pure mouse)
                  <&&> pure (rectHit clip mouse)
              pure (if blocks then WheelBlocked else inner)

-- | Node @idx@, when it is a scroller that takes the wheel at @mouse@: a
-- scroll container whose viewport or bar lanes hold it, or a text area with
-- something to scroll. Not one that lets the pointer through ('PointerPass').
scrollHitSelf ::
  Context -> NodeIdx -> NodeType -> V2 -> Rect -> IO (Maybe NodeIdx)
scrollHitSelf ctx idx nt mouse clip
  | not (nt == NodeTextArea || isScrollNode nt) = pure Nothing
  | otherwise =
      getPointerMode (ctxNodeArena ctx) idx >>= \case
        PointerPass -> pure Nothing
        _ -> scrollerHit ctx idx nt mouse clip

scrollerHit ::
  Context -> NodeIdx -> NodeType -> V2 -> Rect -> IO (Maybe NodeIdx)
scrollerHit ctx idx nt mouse clip
  | nt == NodeTextArea = do
      (field, bars) <- textAreaScrollGeom ctx idx
      let hit = rectHit clip mouse && rectHit field mouse && (tabVertical bars || tabHorizontal bars)
      pure (if hit then Just idx else Nothing)
  | isScrollNode nt && rectHit clip mouse = pure (Just idx)
  | otherwise = pure Nothing

-- Same clip stack as the span walk: scroll viewport (plus its bar lanes),
-- then panel bounds.
scrollHitClip :: Context -> NodeIdx -> NodeType -> Rect -> IO (Maybe Rect)
scrollHitClip Context {ctxNodeArena = na} idx nt parentClip
  | isScrollNode nt = do
      Rect x y w h <- getNodeRect na idx
      sn <- readScrollNode na idx
      let
        lane d = scrollChromeLane (snSlot sn) d x y w h (snPad sn)
        viewport = scrollNodeViewport sn x y w h
        hit
          | sn2D sn = rectUnion viewport (rectUnion (lane DirColumn) (lane DirRow))
          | otherwise = rectUnion viewport (lane (snDir sn))
      pure (rectIntersect parentClip hit)
  | nt == NodePanel = do
      rect <- getNodeRect na idx
      pure (rectIntersect parentClip rect)
  | otherwise = pure (Just parentClip)

-- | Scrollbar layouts of the scroller at @idx@ (id @wid@), each paired with a
-- setter for that axis's offset that skips unchanged values. Covers text
-- areas and native 2D and 1D scroll containers; a 1D scroller with suppressed
-- chrome has none.
scrollBarsFor ::
  Context -> NodeIdx -> WidgetId -> IO [(DirTag, ScrollBarLayout, Float -> IO ())]
scrollBarsFor ctx@Context {ctxNodeArena = na} idx wid = do
  nt <- getNodeType na idx
  -- A 1D scroller's offset is its stored y, whichever way it runs.
  V2 curX curY <- getScrollOffset2D ctx wid
  let
    setMain new = when (new /= curY) (setScrollOffset ctx wid new)
    setX new = when (new /= curX) (setScrollOffset2D ctx wid (V2 new curY))
    bar dir mLayout set = [(dir, layout, set) | Just layout <- [mLayout]]
    bars twoD (mV, mH) = bar DirColumn mV setMain ++ bar DirRow mH (if twoD then setX else setMain)
  if nt == NodeTextArea
    then (\(field, tab) -> bars True (textAreaBarLayouts field tab curX curY)) <$> textAreaScrollGeom ctx idx
    else do
      Rect x y w h <- getNodeRect na idx
      sn <- readScrollNode na idx
      pure (bars (sn2D sn) (scrollNodeBars sn x y w h curX curY))

-- | Bars of scroller @wid@ a thumb drag can grab. A hidden bar has no lane
-- to grab.
grabbableBars :: Context -> WidgetId -> IO [(DirTag, ScrollBarLayout, Float -> IO ())]
grabbableBars ctx wid =
  maybe (pure []) (\idx -> scrollBarsFor ctx idx wid)
    =<< scrollOwnerNode (\cfg _ dir -> scrollChromeSuppressed cfg dir) ctx wid

updateScrollDrag :: Context -> Input -> IO ()
updateScrollDrag ctx inp
  | buttonReleased MouseLeft inp =
      modifyInteraction ctx (\s -> s {isScrollDrag = Nothing})
  | otherwise = do
      mDrag <- getsInteraction ctx isScrollDrag
      case mDrag of
        Just (wid, dragDir, grabOff)
          | buttonHeld MouseLeft inp -> do
              bars <- grabbableBars ctx wid
              forM_ bars $ \(dir, layout, setOffset) ->
                when (dir == dragDir) $
                  setOffset (scrollOffsetFromThumb dir layout grabOff (inputMousePos inp))
        Nothing | buttonPressed MouseLeft inp -> tryStartScrollDrag ctx inp
        _ -> pure ()

-- | Grab a thumb, or jump the thumb's center to a track press and keep
-- dragging from there.
tryStartScrollDrag :: Context -> Input -> IO ()
tryStartScrollDrag ctx inp = do
  let
    mouse = inputMousePos inp
  mIdx <- findScrollNodeUnderMouse ctx mouse
  forM_ mIdx $ \hitIdx -> do
    wid <- getWidgetId (ctxNodeArena ctx) hitIdx
    bars <- grabbableBars ctx wid
    forM_ (find (\(_, l, _) -> onScrollBar mouse l) bars) $ \(dir, layout, setOffset) -> do
      let
        Rect tx ty tw th = sbThumb layout
        along (V2 mx my) = if dir == DirColumn then my else mx
        onThumb = rectContains (sbThumb layout) mouse
        grab = if onThumb then along mouse - along (V2 tx ty) else along (V2 tw th) / 2
      unless onThumb $ setOffset (scrollOffsetFromThumb dir layout grab mouse)
      modifyInteraction ctx (\s -> s {isScrollDrag = Just (wid, dir, grab)})

-- | The scrollbar whose thumb is being dragged, else the one under the
-- pointer, as 'isScrollHover' holds it. While the button is down for any
-- other gesture there is none. The search visits only the pointer nodes, as
-- the cursor's thumb test does, so a frame pays no walk of the whole tree.
probeScrollBarHover :: Context -> Input -> IO (Maybe (WidgetId, DirTag, Rect))
probeScrollBarHover ctx@Context {ctxNodeArena = na} inp = do
  mDrag <- getsInteraction ctx isScrollDrag
  case mDrag of
    Just (wid, dir, _) -> barWhere wid (\(d, _, _) -> d == dir) <$> grabbableBars ctx wid
    Nothing
      | buttonHeld MouseLeft inp || not (rectContains (Rect 0 0 winW winH) mouse) -> pure Nothing
      | otherwise -> do
          top <- overlayHitRoot ctx mouse
          let candidate idx = do
                nt <- getNodeType na idx
                pure (nt == NodeTextArea || isScrollNode nt)
                  <&&> (rectHit <$> getNodeRect na idx <*> pure mouse)
                  <&&> do
                    -- A scroller's own clip is its viewport, short of its
                    -- bars; the clip around it is its parent's.
                    owner <- if isScrollNode nt then getParent na idx else pure idx
                    if owner < 0 then pure True else maybe True (`rectContains` mouse) <$> getClipBounds na owner
                  <&&> (isJust <$> barAt idx)
                  <&&> overlayHitAllowed ctx top idx
              barAt idx = do
                wid <- getWidgetId na idx
                barWhere wid (\(_, l, _) -> onScrollBar mouse l) <$> scrollBarsFor ctx idx wid
          findClassNodeM na PointerNodes candidate >>= maybe (pure Nothing) barAt
  where
    mouse = inputMousePos inp
    Size winW winH = inputWindowSize inp
    barWhere wid p bars = (\(dir, l, _) -> (wid, dir, sbTrack l)) <$> find p bars

-- | Record the scrollbar the pointer is on ('probeScrollBarHover') and
-- repaint the bars it left and entered. Runs after layout.
refreshScrollBarHover :: Context -> Input -> IO ()
refreshScrollBarHover ctx inp = do
  old <- getsInteraction ctx isScrollHover
  new <- probeScrollBarHover ctx inp
  unless (new == old) $ do
    let bar (wid, dir, _) = (wid, dir)
        -- The track's antialiased edge reaches a pixel past it.
        damageBar (_, _, track) = damageRect ctx (rectInflate 1 track)
    when (fmap bar old /= fmap bar new) $ mapM_ damageBar old >> mapM_ damageBar new
    modifyInteraction ctx (\s -> s {isScrollHover = new})
