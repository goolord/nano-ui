-- | Scroll input: offsets baked into the arena, wheel routing, and scrollbar
-- thumb drags and track jumps.
module NanoUI.Internal.Frame.Scroll
  ( applyScrollOffsets
  , updateScrollWheel
  , updateScrollDrag
  , scrollBarsFor
  , scrollBarLayout
  , ScrollBarLayout (..)
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM_, join, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import NanoUI.Internal.Context
  ( Context (..)
  , InteractionState (..)
  , ScrollAxes (..)
  , ScrollBehavior (..)
  , applyScrollTarget
  , beginScrollMetrics
  , cacheScrollMetrics
  , clampScrollOffset
  , getsInteraction
  , getScrollOffset2D
  , getScrollOffsetIn
  , modifyInteraction
  , nodeTheme
  , resolveScrollStep
  , scrollTargetOffset
  , setScrollOffset
  , setScrollOffset2D
  , setScrollOffsetIn
  )
import NanoUI.Internal.Frame.Hit (topmostModalAtMouse, topmostOverlayAtMouse)
import NanoUI.Internal.Frame.Node (ScrollNode (..), readScrollNode, scrollNodeBars, scrollNodeViewport)
import NanoUI.Internal.Frame.Scroll.Geometry
  ( ScrollBarLayout (..)
  , ScrollConfig
  , borderContentClip
  , scrollAxisRange
  , scrollBarLayout
  , scrollChromeLane
  , scrollChromeSuppressed
  , scrollOffsetFromThumb
  , scrollWheelSuppressed
  )
import NanoUI.Internal.Frame.TextArea (TextAreaBars (..), textAreaBarLayouts, textAreaScrollGeom)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Monad ((<&&>))
import NanoUI.Internal.Input
  ( Input (..)
  , inputMouseDown
  , inputMousePos
  , inputMousePressed
  , inputMouseReleased
  , inputScroll
  )
import NanoUI.Internal.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
  , arenaCount
  , NodeClass (PointerNodes)
  , findClassNodeM
  , firstChildJustM
  , forChildNodes_
  , getDirection
  , getNodeRect
  , getNodeType
  , getParent
  , getRect
  , getWidgetId
  , isFloatingNode
  , isScrollNode
  , setClipRect
  , setRect
  , walkAncestors
  )
import NanoUI.Internal.Style (Padding (..), themePanel)
import NanoUI.Internal.Types (Rect (..), V2 (..), rectContains, rectHit, rectIntersect, rectUnion)

applyScrollOffsets :: Context -> IO ()
applyScrollOffsets ctx = do
  beginScrollMetrics ctx
  -- A frame that added no widgets has no root to walk.
  count <- arenaCount (ctxNodeArena ctx)
  when (count > 0) $ do
    rect <- getNodeRect (ctxNodeArena ctx) 0
    transformSubtree ctx 0 0 0 rect

transformSubtree :: Context -> NodeIdx -> Float -> Float -> Rect -> IO ()
transformSubtree ctx idx scrollX scrollY parentClip = do
  let
    na = ctxNodeArena ctx
  nt <- getNodeType na idx
  (lx, ly, vw, vh) <- getRect na idx
  let
    floating = isFloatingNode nt
    (sx, sy) = if floating then (0, 0) else (scrollX, scrollY)
    !vx = lx + sx
    !vy = ly + sy
    within r = fromMaybe parentClip (rectIntersect parentClip r)
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
      descend sx sy clip

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
applyCrossAxisScroll ctx idx scroll = do
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
 where
  na = ctxNodeArena ctx

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
scrollOwnerNode suppressed ctx wid =
  findClassNodeM na PointerNodes $ \idx -> do
    nt <- getNodeType na idx
    pure (nt == NodeTextArea || isScrollNode nt)
      <&&> ((== wid) <$> getWidgetId na idx)
      <&&> if nt == NodeTextArea
        then pure True
        else (\sn -> not (suppressed (snConfig sn) (sn2D sn) (snDir sn))) <$> readScrollNode na idx
 where
  na = ctxNodeArena ctx

applyScrollWheelDelta :: Context -> WidgetId -> V2 -> IO ()
applyScrollWheelDelta ctx wid (V2 wheelX wheelY) = do
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
 where
  na = ctxNodeArena ctx

findScrollNodeUnderMouse :: Context -> V2 -> IO (Maybe NodeIdx)
findScrollNodeUnderMouse ctx mouse = do
  count <- arenaCount (ctxNodeArena ctx)
  if count <= 0
    then pure Nothing
    else do
      top <-
        runMaybeT $
          MaybeT (topmostModalAtMouse ctx mouse)
            <|> MaybeT (topmostOverlayAtMouse ctx mouse)
      let
        start = fromMaybe 0 top
      rect <- getNodeRect (ctxNodeArena ctx) start
      queryScrollTarget ctx mouse rect start

-- | The scroller under @mouse@ in the subtree at @idx@: its first child's,
-- else @idx@ itself.
queryScrollTarget :: Context -> V2 -> Rect -> NodeIdx -> IO (Maybe NodeIdx)
queryScrollTarget ctx mouse parentClip idx = runMaybeT $ do
  nt <- liftIO $ getNodeType (ctxNodeArena ctx) idx
  clip <- MaybeT $ scrollHitClip ctx idx nt parentClip
  MaybeT (firstChildJustM (ctxNodeArena ctx) idx (queryScrollTarget ctx mouse clip))
    <|> MaybeT (scrollHitSelf ctx idx nt mouse clip)

scrollHitSelf ::
  Context -> NodeIdx -> NodeType -> V2 -> Rect -> IO (Maybe NodeIdx)
scrollHitSelf ctx idx nt mouse clip
  | nt == NodeTextArea = do
      (field, bars) <- textAreaScrollGeom ctx idx
      let hit = rectHit clip mouse && rectHit field mouse && (tabVertical bars || tabHorizontal bars)
      pure (if hit then Just idx else Nothing)
  | isScrollNode nt && rectHit clip mouse = pure (Just idx)
  | otherwise = pure Nothing

-- Same clip stack as the span walk: scroll viewport (plus its bar lanes),
-- then panel bounds.
scrollHitClip :: Context -> NodeIdx -> NodeType -> Rect -> IO (Maybe Rect)
scrollHitClip ctx idx nt parentClip
  | isScrollNode nt = do
      (x, y, w, h) <- getRect na idx
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
 where
  na = ctxNodeArena ctx

-- | Scrollbar layouts of the scroller at @idx@ (id @wid@), each paired with a
-- setter for that axis's offset that skips unchanged values. Covers text
-- areas and native 2D and 1D scroll containers; a 1D scroller with suppressed
-- chrome has none.
scrollBarsFor ::
  Context -> NodeIdx -> WidgetId -> IO [(DirTag, ScrollBarLayout, Float -> IO ())]
scrollBarsFor ctx idx wid = do
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
      (x, y, w, h) <- getRect na idx
      sn <- readScrollNode na idx
      pure (bars (sn2D sn) (scrollNodeBars sn x y w h curX curY))
 where
  na = ctxNodeArena ctx

-- | Bars of scroller @wid@ a thumb drag can grab. A hidden bar has no lane
-- to grab.
grabbableBars :: Context -> WidgetId -> IO [(DirTag, ScrollBarLayout, Float -> IO ())]
grabbableBars ctx wid =
  maybe (pure []) (\idx -> scrollBarsFor ctx idx wid)
    =<< scrollOwnerNode (\cfg _ dir -> scrollChromeSuppressed cfg dir) ctx wid

updateScrollDrag :: Context -> Input -> IO ()
updateScrollDrag ctx inp
  | inputMouseReleased inp =
      modifyInteraction ctx (\s -> s {isScrollDrag = Nothing})
  | otherwise = do
      mDrag <- getsInteraction ctx isScrollDrag
      case mDrag of
        Just (wid, dragDir, grabOff)
          | inputMouseDown inp -> do
              bars <- grabbableBars ctx wid
              forM_ bars $ \(dir, layout, setOffset) ->
                when (dir == dragDir) $
                  setOffset (scrollOffsetFromThumb dir layout grabOff (inputMousePos inp))
        Nothing | inputMousePressed inp -> tryStartScrollDrag ctx inp
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
    let
      onBar (_, l, _) = rectContains (sbThumb l) mouse || rectContains (sbTrack l) mouse
    forM_ (find onBar bars) $ \(dir, layout, setOffset) -> do
      let
        Rect tx ty tw th = sbThumb layout
        along (V2 mx my) = if dir == DirColumn then my else mx
        onThumb = rectContains (sbThumb layout) mouse
        grab = if onThumb then along mouse - along (V2 tx ty) else along (V2 tw th) / 2
      unless onThumb $ setOffset (scrollOffsetFromThumb dir layout grab mouse)
      modifyInteraction ctx (\s -> s {isScrollDrag = Just (wid, dir, grab)})
