{-# LANGUAGE DataKinds #-}

-- | Scroll input: offsets baked into the arena, wheel routing, and scrollbar
-- thumb drags and track jumps.
module NanoUI.Frame.Scroll
  ( applyScrollOffsets
  , updateScrollWheel
  , updateScrollDrag
  , scrollBarsFor
  , scrollBarLayout
  , ScrollBarLayout (..)
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM_, void, when)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import NanoUI.Context
  ( Context (..)
  , ScrollAxes (..)
  , ScrollBehavior (..)
  , applyScrollTarget
  , beginScrollMetrics
  , cacheScrollMetrics
  , clampScrollOffset
  , getScrollDrag
  , getScrollOffset
  , getScrollOffset2D
  , getScrollOffsetIn
  , resolveScrollStep
  , scrollTargetOffset
  , setScrollOffset
  , setScrollOffset2D
  , setScrollOffsetIn
  , nodeTheme
  , InteractionState (..)
  , modifyInteraction
  )
import NanoUI.Frame.Hit (topmostModalAtMouse, topmostOverlayAtMouse)
import NanoUI.Frame.Node (ScrollNode (..), readScrollNode, scrollNodeViewport)
import NanoUI.Frame.Scroll.Geometry
  ( ScrollBarLayout (..)
  , ScrollConfig
  , borderContentClip
  , decodeScrollConfig
  , isScrollStyle2D
  , scrollAxisRange
  , scrollBarLayout
  , scrollBarLayouts2D
  , scrollChromeLane
  , scrollChromeSuppressed
  , scrollOffsetFromThumb
  , scrollWheelSuppressed
  )
import NanoUI.Frame.TextArea.Content (textAreaContentGeom)
import NanoUI.Frame.TextArea.Geometry (TextAreaBars (..), TextAreaScrollBarLayouts (..), textAreaBars, textAreaScrollBarLayouts)
import NanoUI.Id (WidgetId)
import NanoUI.Input (Input (..), inputMouseDown, inputMousePos, inputMousePressed, inputMouseReleased, inputScroll)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
  , arenaCount
  , findNodeM
  , forChildNodes_
  , getDirection
  , getFirstChild
  , getLayoutRect
  , getNextSibling
  , getNodeRect
  , getNodeType
  , getParent
  , getRect
  , getStyleIdx
  , getWidgetId
  , isFloatingNode
  , isScrollNode
  , setClipRect
  , setRect
  , snapshotLayoutRects
  )
import NanoUI.Style (Padding (..), themePanel)
import NanoUI.Types (Rect (..), V2 (..), rectContains, rectIntersect, rectUnion)

applyScrollOffsets :: Context -> IO ()
applyScrollOffsets ctx = do
  beginScrollMetrics ctx
  snapshotLayoutRects (ctxNodeArena ctx)
  -- A frame that added no widgets has no root to walk.
  count <- arenaCount (ctxNodeArena ctx)
  when (count > 0) $ do
    rect <- getNodeRect (ctxNodeArena ctx) 0
    transformSubtree ctx 0 0 0 rect

transformSubtree :: Context -> NodeIdx -> Float -> Float -> Rect -> IO ()
transformSubtree ctx idx scrollX scrollY parentClip = do
  let na = ctxNodeArena ctx
  nt <- getNodeType na idx
  (lx, ly, lw, lh) <- getLayoutRect na idx
  let floating = isFloatingNode nt
      (sx, sy) = if floating then (0, 0) else (scrollX, scrollY)
      within r = fromMaybe parentClip (rectIntersect parentClip r)
  (vx, vy, vw, vh) <-
    if floating
      then getRect na idx
      else pure (lx + sx, ly + sy, lw, lh)
  when (not floating) $ setRect na idx vx vy vw vh
  (!childScrollX, !childScrollY, !childClip) <-
    if isScrollNode nt
      then do
        (axes, viewport, range) <- scrollNodeGeometry ctx idx (Rect vx vy lw lh)
        wid <- getWidgetId na idx
        -- The only pass that sees a scroller's placed geometry. Everything
        -- that scrolls one between frames reads it back from here.
        cacheScrollMetrics ctx wid axes viewport range
        -- An offset set outright before this layout, or left over from
        -- content that has since shrunk, is held to the range the content
        -- has now, on the axes this scroller owns.
        cur@(V2 cx cy) <- getScrollOffsetIn ctx wid axes
        let V2 hx hy = clampScrollOffset range cur
            held = case axes of
              ScrollAxisY -> V2 cx hy
              ScrollAxisX -> V2 hx cy
              ScrollAxisXY -> V2 hx hy
        when (held /= cur) $ setScrollOffsetIn ctx wid axes held
        let V2 dx dy = held
        let clip = within viewport
        setClipRect na idx clip
        pure (sx - dx, sy - dy, clip)
      else do
        clip <-
          case nt of
            NodePanel -> do
              theme <- nodeTheme ctx idx
              pure (within (borderContentClip (themePanel theme) (Rect vx vy vw vh)))
            _ -> pure $! if floating then Rect vx vy vw vh else parentClip
        setClipRect na idx clip
        pure (sx, sy, clip)
  forChildNodes_ na idx $ \ci ->
    transformSubtree ctx ci childScrollX childScrollY childClip

-- | Axes, content viewport and reachable offset range of the scroll container
-- at @idx@ placed at @rect@, in window axes. The wheel, the programmatic
-- commands and the transform pass all size a scroll off this, so a scroller
-- cannot disagree with itself about how far it reaches.
scrollNodeGeometry :: Context -> NodeIdx -> Rect -> IO (ScrollAxes, Rect, V2)
scrollNodeGeometry ctx idx (Rect x y w h) = do
  sn@ScrollNode {snPad = pad, snContentMain = contentMain} <- readScrollNode (ctxNodeArena ctx) idx
  let viewport = scrollNodeViewport sn x y w h
      rangeH = scrollAxisRange contentMain (rectH viewport) (padB pad)
  pure $
    if sn2D sn
      then (ScrollAxisXY, viewport, V2 (scrollAxisRange (snContentW sn) (rectW viewport) (padR pad)) rangeH)
      else case snDir sn of
        DirColumn -> (ScrollAxisY, viewport, V2 0 rangeH)
        DirRow -> (ScrollAxisX, viewport, V2 (scrollAxisRange contentMain (rectW viewport) (padR pad)) 0)

updateScrollWheel :: Context -> Input -> IO ()
updateScrollWheel ctx inp = do
  let scroll@(V2 wheelX wheelY) = inputScroll inp
  -- A wheel over an open dropdown or the text-edit menu never gets here: the
  -- combo widget scrolls its own list, and the scroller under it stays put.
  when (wheelY /= 0 || wheelX /= 0) $ do
    mNode <- findScrollNodeUnderMouse ctx (inputMousePos inp)
    forM_ mNode $ \idx -> do
      wid <- getWidgetId (ctxNodeArena ctx) idx
      void (tryApplyScrollWheelDelta ctx wid scroll)
      applyCrossAxisScroll ctx idx scroll

-- Nested 2D: apply the unused axis to a paired scroller in the same panel.
-- Do not walk past panel/window/modal into the page scroller.
applyCrossAxisScroll :: Context -> NodeIdx -> V2 -> IO ()
applyCrossAxisScroll ctx idx scroll = do
  dir <- getDirection (ctxNodeArena ctx) idx
  mAnc <- walkOppositeAncestor ctx idx dir
  case mAnc of
    Just pwid -> void (tryApplyScrollWheelDelta ctx pwid scroll)
    Nothing -> do
      mDesc <- findOppositeScrollDescendant ctx idx dir
      forM_ mDesc $ \dwid -> tryApplyScrollWheelDelta ctx dwid scroll

scrollCrossAxisStop :: NodeType -> Bool
scrollCrossAxisStop nt =
  nt == NodePanel || nt == NodeWindow || nt == NodeModal

walkOppositeAncestor :: Context -> NodeIdx -> DirTag -> IO (Maybe WidgetId)
walkOppositeAncestor ctx idx childDir = do
  p <- getParent (ctxNodeArena ctx) idx
  if p < 0
    then pure Nothing
    else do
      nt <- getNodeType (ctxNodeArena ctx) p
      if scrollCrossAxisStop nt
        then pure Nothing
        else
          if not (isScrollNode nt)
            then walkOppositeAncestor ctx p childDir
            else do
              pdir <- getDirection (ctxNodeArena ctx) p
              if pdir == childDir
                then walkOppositeAncestor ctx p childDir
                else Just <$> getWidgetId (ctxNodeArena ctx) p

findOppositeScrollDescendant :: Context -> NodeIdx -> DirTag -> IO (Maybe WidgetId)
findOppositeScrollDescendant ctx idx childDir = goChildren idx
  where
    want = if childDir == DirColumn then DirRow else DirColumn
    goChildren parent = getFirstChild (ctxNodeArena ctx) parent >>= go
    go ci
      | ci < 0 = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) ci
          found <-
            if isScrollNode nt
              then do
                d <- getDirection (ctxNodeArena ctx) ci
                if d == want
                  then Just <$> getWidgetId (ctxNodeArena ctx) ci
                  else goChildren ci
              else goChildren ci
          case found of
            Just w -> pure (Just w)
            Nothing -> getNextSibling (ctxNodeArena ctx) ci >>= go

-- | Node owning scroller @wid@: its text area, or the first scroll container
-- with that id that the predicate does not rule out (table slave panes share
-- an id with their master). Thumb drags use the chrome predicate, since a
-- hidden bar has no lane to grab; the wheel uses the wider one, since a
-- hidden bar still scrolls.
scrollOwnerNode :: (ScrollConfig -> Bool -> DirTag -> Bool) -> Context -> WidgetId -> IO (Maybe NodeIdx)
scrollOwnerNode suppressed ctx wid =
  findNodeM na $ \idx -> do
    nt <- getNodeType na idx
    if nt /= NodeTextArea && not (isScrollNode nt)
      then pure False
      else do
        owner <- getWidgetId na idx
        if owner /= wid
          then pure False
          else
            if nt == NodeTextArea
              then pure True
              else do
                si <- getStyleIdx na idx
                dir <- getDirection na idx
                pure (not (suppressed (decodeScrollConfig si) (isScrollStyle2D si) dir))
  where
    na = ctxNodeArena ctx

tryApplyScrollWheelDelta :: Context -> WidgetId -> V2 -> IO Bool
tryApplyScrollWheelDelta ctx wid (V2 wheelX wheelY) = do
  mIdx <- scrollOwnerNode scrollWheelSuppressed ctx wid
  case mIdx of
    Nothing -> pure False
    Just idx -> do
      nt <- getNodeType na idx
      (axes, range) <-
        if nt == NodeTextArea
          then do
            (fm, field, contentW, contentH) <- textAreaContentGeom ctx idx
            let bars = textAreaBars fm field contentW contentH
            pure
              ( ScrollAxisXY
              , V2 (max 0 (contentW - tabViewW bars)) (max 0 (contentH - tabViewH bars))
              )
          else do
            rect <- getNodeRect na idx
            (axes, _, range) <- scrollNodeGeometry ctx idx rect
            pure (axes, range)
      step <- resolveScrollStep ctx wid
      cur <- getScrollOffsetIn ctx wid axes
      -- Notches land on where the scroller is headed, not on where it is, so
      -- a flick mid-glide adds to the throw instead of restarting it.
      base@(V2 baseX baseY) <- scrollTargetOffset ctx wid cur
      let next = clampScrollOffset range (V2 (baseX + wheelX * step) (baseY + wheelY * step))
      if next == base && next == cur
        then pure False
        else True <$ applyScrollTarget ctx wid axes next ScrollSmooth
  where
    na = ctxNodeArena ctx

findScrollNodeUnderMouse :: Context -> V2 -> IO (Maybe NodeIdx)
findScrollNodeUnderMouse ctx mouse = do
  count <- arenaCount (ctxNodeArena ctx)
  if count <= 0
    then pure Nothing
    else do
      mModal <- topmostModalAtMouse ctx mouse
      mTop <- topmostOverlayAtMouse ctx mouse
      let start = fromMaybe 0 (mModal <|> mTop)
      rect <- getNodeRect (ctxNodeArena ctx) start
      queryScrollTarget ctx start mouse rect

queryScrollTarget :: Context -> NodeIdx -> V2 -> Rect -> IO (Maybe NodeIdx)
queryScrollTarget ctx idx mouse parentClip = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  mClipHere <- scrollHitClip ctx idx nt parentClip
  case mClipHere of
    Nothing -> pure Nothing
    Just clip -> do
      childHit <- walkScrollSiblings ctx idx mouse clip
      case childHit of
        Just hit -> pure (Just hit)
        Nothing -> scrollHitSelf ctx idx nt mouse clip

walkScrollSiblings :: Context -> NodeIdx -> V2 -> Rect -> IO (Maybe NodeIdx)
walkScrollSiblings ctx parent mouse clip = getFirstChild (ctxNodeArena ctx) parent >>= go
  where
    go ci
      | ci < 0 = pure Nothing
      | otherwise = do
          hit <- queryScrollTarget ctx ci mouse clip
          case hit of
            Just found -> pure (Just found)
            Nothing -> getNextSibling (ctxNodeArena ctx) ci >>= go

scrollHitSelf :: Context -> NodeIdx -> NodeType -> V2 -> Rect -> IO (Maybe NodeIdx)
scrollHitSelf ctx idx nt mouse clip
  | nt == NodeTextArea = do
      (fm, field, contentW, contentH) <- textAreaContentGeom ctx idx
      let bars = textAreaBars fm field contentW contentH
      pure $ case rectIntersect clip field of
        Just fclip
          | visibleHit fclip && (tabVertical bars || tabHorizontal bars) -> Just idx
        _ -> Nothing
  | isScrollNode nt && visibleHit clip = pure (Just idx)
  | otherwise = pure Nothing
  where
    visibleHit r@(Rect _ _ rw rh) = rw > 0 && rh > 0 && rectContains r mouse

-- Same clip stack as the span walk: scroll viewport (plus its bar lanes),
-- then panel bounds.
scrollHitClip :: Context -> NodeIdx -> NodeType -> Rect -> IO (Maybe Rect)
scrollHitClip ctx idx nt parentClip
  | isScrollNode nt = do
      (x, y, w, h) <- getRect na idx
      sn <- readScrollNode na idx
      let lane d = scrollChromeLane (snSlot sn) d x y w h (snPad sn)
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
scrollBarsFor :: Context -> NodeIdx -> WidgetId -> IO [(DirTag, ScrollBarLayout, Float -> IO ())]
scrollBarsFor ctx idx wid = do
  nt <- getNodeType na idx
  if nt == NodeTextArea
    then do
      (fm, field, contentW, contentH) <- textAreaContentGeom ctx idx
      cur@(V2 curX curY) <- getScrollOffset2D ctx wid
      let layouts = textAreaScrollBarLayouts fm field contentW contentH curX curY
      pure (axes2D cur (tasbVertical layouts) (tasbHorizontal layouts))
    else do
      (x, y, w, h) <- getRect na idx
      ScrollNode slot cfg native2D dir pad contentMain contentW <- readScrollNode na idx
      if native2D
        then do
          cur@(V2 offX offY) <- getScrollOffset2D ctx wid
          let (mV, mH) = scrollBarLayouts2D slot cfg x y w h pad contentW contentMain offX offY
          pure (axes2D cur mV mH)
        else
          if scrollChromeSuppressed cfg dir
            then pure []
            else do
              off <- getScrollOffset ctx wid
              pure
                [ (dir, layout, \new -> when (new /= off) (setScrollOffset ctx wid new))
                | Just layout <- [scrollBarLayout slot dir x y w h pad contentMain off]
                ]
  where
    na = ctxNodeArena ctx
    axes2D (V2 curX curY) mV mH =
      [(DirColumn, layout, \new -> when (new /= curY) (setScrollOffset2D ctx wid (V2 curX new))) | Just layout <- [mV]]
        ++ [(DirRow, layout, \new -> when (new /= curX) (setScrollOffset2D ctx wid (V2 new curY))) | Just layout <- [mH]]

updateScrollDrag :: Context -> Input -> IO ()
updateScrollDrag ctx inp
  | inputMouseReleased inp = modifyInteraction ctx (\s -> s {isScrollDrag = Nothing})
  | otherwise = do
      mDrag <- getScrollDrag ctx
      case mDrag of
        Just (wid, dragDir, grabOff)
          | inputMouseDown inp -> do
              -- A hidden bar has no lane to grab.
              bars <- maybe (pure []) (\idx -> scrollBarsFor ctx idx wid) =<< scrollOwnerNode (\cfg _ dir -> scrollChromeSuppressed cfg dir) ctx wid
              forM_ bars $ \(dir, layout, setOffset) ->
                when (dir == dragDir) $
                  setOffset (scrollOffsetFromThumb dir layout grabOff (inputMousePos inp))
        Nothing | inputMousePressed inp -> tryStartScrollDrag ctx inp
        _ -> pure ()

-- | Grab a thumb, or jump the thumb's center to a track press and keep
-- dragging from there.
tryStartScrollDrag :: Context -> Input -> IO ()
tryStartScrollDrag ctx inp = do
  let mouse = inputMousePos inp
  mIdx <- findScrollNodeUnderMouse ctx mouse
  forM_ mIdx $ \hitIdx -> do
    wid <- getWidgetId (ctxNodeArena ctx) hitIdx
    bars <- maybe (pure []) (\idx -> scrollBarsFor ctx idx wid) =<< scrollOwnerNode (\cfg _ dir -> scrollChromeSuppressed cfg dir) ctx wid
    forM_ (find (\(_, l, _) -> rectContains (sbThumb l) mouse || rectContains (sbTrack l) mouse) bars) $
        \(dir, layout, setOffset) -> do
          let thumb = sbThumb layout
              along (V2 mx my) = if dir == DirColumn then my else mx
              Rect tx ty tw th = thumb
          if rectContains thumb mouse
            then modifyInteraction ctx (\s -> s {isScrollDrag = Just (wid, dir, along mouse - along (V2 tx ty))})
            else do
              let half = along (V2 tw th) / 2
              setOffset (scrollOffsetFromThumb dir layout half mouse)
              modifyInteraction ctx (\s -> s {isScrollDrag = Just (wid, dir, half)})
