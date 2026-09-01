{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.Scroll
  ( applyScrollOffsets
  , updateScrollWheel
  , updateScrollDrag
  , tryStartScrollDrag
  , scrollBarLayout
  , ScrollBarLayout (..)
  , paintScrollChrome
  ) where


import Control.Monad (void, when)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import NanoUI.Context
  ( Context (..)
  , getScrollOffset
  , getScrollOffset2D
  , setScrollOffset
  , setScrollOffset2D
  )
import NanoUI.Draw (DrawArena, Layer (..), beginLayer, currentLayer, pushRect, pushRoundedRect)
import NanoUI.Font (ScrollBarSlot (..))
import NanoUI.Host (HostProfile, isCellHost)
import NanoUI.Id (WidgetId, hashWidgetId)
import NanoUI.Input (Input (..), inputMouseDown, inputMousePos, inputMousePressed, inputMouseReleased, inputScroll)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
  , arenaCount
  , getDirection
  , getFirstChild
  , getAspect
  , getLayoutRect
  , getNextSibling
  , getNodeValue
  , getPadding
  , getParent
  , getNodeType
  , getRect
  , getStyleIdx
  , getWidgetId
  , isFloatingNode
  , isScrollNode
  , setClipRect
  , setRect
  , snapshotLayoutRects
  )
import NanoUI.Layout.Solve (scrollBarSlotOf)
import NanoUI.Style (Padding (..), Theme (..), scrollBarThumbColor, scrollBarTrackColor, themeFloatingWindow, themePanel)
import NanoUI.Types (Rect (..), V2 (..), rectContains, rectH, rectIntersect, rectUnion, rectW, rectX, rectY, v2X, v2Y)
import NanoUI.Frame.Clip (borderContentClip)
import NanoUI.Frame.Scroll.Geometry
  ( ScrollBarLayout (..)
  , scrollBarLayout
  , scrollChromeLane
  , scrollContentClip
  , scrollOffsetFromThumb
  , scrollViewportClip2D
  , scrollChromeSuppressed
  , scrollShowsChrome
  , decodeScrollConfig
  , isScrollStyle2D
  , ScrollConfig (..)
  , ScrollPolicy (..)
  )
import NanoUI.Frame.Hit (ancestorScrollShift, findNodeByWidgetId, topmostModalAtMouse, topmostOverlayAtMouse)

scrollLineFor :: HostProfile -> Float
scrollLineFor host = if isCellHost host then 1 else scrollLine

scrollLine :: Float
scrollLine = 20

applyScrollOffsets :: Context -> IO ()
applyScrollOffsets ctx = do
  snapshotLayoutRects (ctxNodeArena ctx)
  (wx, wy, ww, wh) <- getRect (ctxNodeArena ctx) 0
  let rootClip = Rect wx wy ww wh
  transformSubtree ctx 0 0 0 rootClip

transformSubtree :: Context -> NodeIdx -> Float -> Float -> Rect -> IO ()
transformSubtree ctx idx scrollX scrollY parentClip = do
  let na = ctxNodeArena ctx
  nt <- getNodeType na idx
  (lx, ly, lw, lh) <- getLayoutRect na idx
  let floating = isFloatingNode nt
      (sx, sy) =
        if floating
          then (0, 0)
          else (scrollX, scrollY)
  (vx, vy, vw, vh) <-
    if floating
      then getRect na idx
      else pure (lx + sx, ly + sy, lw, lh)
  when (not floating) $ setRect na idx vx vy vw vh
  let nodeRect = Rect vx vy vw vh
  (childScrollX, childScrollY, childClip) <-
    if isScrollNode nt
      then do
        let skipModal = isCellHost (ctxHostProfile ctx) && nt == NodeModal
        if skipModal
          then do
            setClipRect na idx nodeRect
            pure (sx, sy, nodeRect)
          else do
            pad <- getPadding na idx
            dir <- getDirection na idx
            slot <- scrollBarSlotOf na idx
            let fm = ctxFontMetrics ctx
            wid <- getWidgetId na idx
            si <- getStyleIdx na idx
            if isScrollStyle2D si
              then do
                contentH <- getNodeValue na idx
                contentW <- getAspect na idx
                let cfg = decodeScrollConfig si
                    viewport2d =
                      scrollViewportClip2D
                        (ctxHostProfile ctx)
                        fm
                        slot
                        cfg
                        vx
                        vy
                        lw
                        lh
                        pad
                        contentW
                        contentH
                    clip2d = fromMaybe parentClip (rectIntersect parentClip viewport2d)
                V2 offX offY <- getScrollOffset2D ctx wid
                setClipRect na idx clip2d
                pure (sx - offX, sy - offY, clip2d)
              else do
                contentSize <- getNodeValue na idx
                let cfg = decodeScrollConfig si
                    viewport1d =
                      scrollContentClip (ctxHostProfile ctx) fm slot cfg dir vx vy lw lh pad contentSize
                    clip1d = fromMaybe parentClip (rectIntersect parentClip viewport1d)
                off <- getScrollOffset ctx wid
                let (nsx, nsy) =
                      case dir of
                        DirColumn -> (sx, sy - off)
                        DirRow -> (sx - off, sy)
                setClipRect na idx clip1d
                pure (nsx, nsy, clip1d)
      else do
        case nt of
          NodePanel -> do
            let style = themePanel (ctxTheme ctx)
                inner = borderContentClip style nodeRect
                clipHere = fromMaybe parentClip (rectIntersect parentClip inner)
            setClipRect na idx clipHere
            pure (sx, sy, clipHere)
          _ -> do
            let clipHere = if floating then nodeRect else parentClip
            setClipRect na idx clipHere
            pure (sx, sy, clipHere)
  fc <- getFirstChild na idx
  let go ci
        | ci < 0 = pure ()
        | otherwise = do
            transformSubtree ctx ci childScrollX childScrollY childClip
            ns <- getNextSibling na ci
            go ns
  go fc

updateScrollWheel :: Context -> Input -> IO ()
updateScrollWheel ctx inp = do
  let scroll = inputScroll inp
  when (v2Y scroll /= 0 || v2X scroll /= 0) $ do
    mNode <- findScrollNodeUnderMouse ctx (inputMousePos inp)
    case mNode of
      Just idx -> do
        wid <- getWidgetId (ctxNodeArena ctx) idx
        void (tryApplyScrollWheelDelta ctx wid scroll)
        applyCrossAxisScroll ctx idx scroll
      Nothing -> do
        focus <- readIORef (ctxFocusId ctx)
        if hashWidgetId focus == 0
          then pure ()
          else do
            mWid <- findScrollOwningWidget ctx focus
            case mWid of
              Just wid -> void (tryApplyScrollWheelDelta ctx wid scroll)
              Nothing -> pure ()

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
      case mDesc of
        Just dwid -> void (tryApplyScrollWheelDelta ctx dwid scroll)
        Nothing -> pure ()

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
    goChildren parent = do
      fc <- getFirstChild (ctxNodeArena ctx) parent
      go fc
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
            Nothing -> do
              ns <- getNextSibling (ctxNodeArena ctx) ci
              go ns

-- Nested scrollers take the wheel only while hovered or while they own focus.
-- No leftover chain to the parent at a limit.
findScrollOwningWidget :: Context -> WidgetId -> IO (Maybe WidgetId)
findScrollOwningWidget ctx wid = do
  mIdx <- findNodeByWidgetId ctx wid
  case mIdx of
    Nothing -> pure Nothing
    Just idx -> walkUp idx
  where
    walkUp i
      | i < 0 = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) i
          if isScrollNode nt
            then Just <$> getWidgetId (ctxNodeArena ctx) i
            else do
              p <- getParent (ctxNodeArena ctx) i
              walkUp p

tryApplyScrollWheelDelta :: Context -> WidgetId -> V2 -> IO Bool
tryApplyScrollWheelDelta ctx wid scroll = do
  mGeom <- scrollContainerGeom ctx wid
  case mGeom of
    Nothing -> pure False
    Just (idx, dir, _x, _y, w, h, pad, contentSize) -> do
      si <- getStyleIdx (ctxNodeArena ctx) idx
      let step = scrollLineFor (ctxHostProfile ctx)
          innerW = w - padL pad - padR pad
          innerH = h - padT pad - padB pad
      if isScrollStyle2D si
        then do
          contentW <- getAspect (ctxNodeArena ctx) idx
          contentH <- getNodeValue (ctxNodeArena ctx) idx
          V2 curX curY <- getScrollOffset2D ctx wid
          let maxX = max 0 (contentW - innerW)
              maxY = max 0 (contentH - innerH)
              newX = max 0 (min maxX (curX + v2X scroll * step))
              newY = max 0 (min maxY (curY + v2Y scroll * step))
          if newX == curX && newY == curY
            then pure False
            else do
              setScrollOffset2D ctx wid (V2 newX newY)
              pure True
        else do
          cur <- getScrollOffset ctx wid
          let cfg = decodeScrollConfig si
          case dir of
            DirColumn
              | scrollPolicyY cfg == ScrollNone -> pure False
              | otherwise -> applyAxis cur innerH contentSize (v2Y scroll * step)
            DirRow
              | scrollPolicyX cfg == ScrollNone -> pure False
              | otherwise -> applyAxis cur innerW contentSize (v2X scroll * step)
  where
    applyAxis cur inner contentSize delta = do
      let maxOff = max 0 (contentSize - inner)
          newOff = max 0 (min maxOff (cur + delta))
      if newOff == cur
        then pure False
        else do
          setScrollOffset ctx wid newOff
          pure True

findScrollTargetUnderMouse :: Context -> V2 -> IO (Maybe WidgetId)
findScrollTargetUnderMouse ctx mouse = do
  mIdx <- findScrollNodeUnderMouse ctx mouse
  case mIdx of
    Nothing -> pure Nothing
    Just idx -> Just <$> getWidgetId (ctxNodeArena ctx) idx

findScrollNodeUnderMouse :: Context -> V2 -> IO (Maybe NodeIdx)
findScrollNodeUnderMouse ctx mouse = do
  mModal <- topmostModalAtMouse ctx mouse
  mTop <- topmostOverlayAtMouse ctx mouse
  let mStart =
        case mModal of
          Just idx -> Just idx
          Nothing -> mTop
  case mStart of
    Just idx -> do
      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
      queryScrollTarget ctx idx mouse (Rect x y w h)
    Nothing -> do
      (x, y, w, h) <- getRect (ctxNodeArena ctx) 0
      queryScrollTarget ctx 0 mouse (Rect x y w h)

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
        Nothing -> scrollHitSelf ctx idx mouse clip

walkScrollSiblings :: Context -> NodeIdx -> V2 -> Rect -> IO (Maybe NodeIdx)
walkScrollSiblings ctx parent mouse clip = do
  fc <- getFirstChild (ctxNodeArena ctx) parent
  go fc
  where
    go ci
      | ci < 0 = pure Nothing
      | otherwise = do
          hit <- queryScrollTarget ctx ci mouse clip
          case hit of
            Just found -> pure (Just found)
            Nothing -> do
              ns <- getNextSibling (ctxNodeArena ctx) ci
              go ns

scrollHitSelf :: Context -> NodeIdx -> V2 -> Rect -> IO (Maybe NodeIdx)
scrollHitSelf ctx idx mouse clip = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  let
    cellModalShell = isCellHost (ctxHostProfile ctx) && nt == NodeModal
  if not (isScrollNode nt) || cellModalShell
    then pure Nothing
    else
      if rectW clip > 0 && rectH clip > 0 && rectContains clip mouse
        then pure (Just idx)
        else pure Nothing

-- Same clip stack as collectClippedSpans': scroll viewport, then panel bounds.
scrollHitClip :: Context -> NodeIdx -> NodeType -> Rect -> IO (Maybe Rect)
scrollHitClip ctx idx nt parentClip = do
  pad <- getPadding (ctxNodeArena ctx) idx
  let fm = ctxFontMetrics ctx
  if isScrollNode nt
    then do
      (x, y, w, h) <- getScrollVisualRect ctx idx
      dir <- getDirection (ctxNodeArena ctx) idx
      slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
      si <- getStyleIdx (ctxNodeArena ctx) idx
      let cfg = decodeScrollConfig si
      localClip <-
        if isScrollStyle2D si
          then do
            contentH <- getNodeValue (ctxNodeArena ctx) idx
            contentW <- getAspect (ctxNodeArena ctx) idx
            pure $
              scrollViewportClip2D (ctxHostProfile ctx) fm slot cfg x y w h pad contentW contentH
          else do
            contentSize <- getNodeValue (ctxNodeArena ctx) idx
            pure $ scrollContentClip (ctxHostProfile ctx) fm slot cfg dir x y w h pad contentSize
      let laneDir = scrollChromeLane (ctxHostProfile ctx) fm slot dir x y w h pad
          laneCol = scrollChromeLane (ctxHostProfile ctx) fm slot DirColumn x y w h pad
          laneRow = scrollChromeLane (ctxHostProfile ctx) fm slot DirRow x y w h pad
          hit =
            if isScrollStyle2D si
              then rectUnion localClip (rectUnion laneCol laneRow)
              else rectUnion localClip laneDir
      pure (rectIntersect parentClip hit)
    else
      if nt == NodePanel
        then do
          (x, y, w, h) <- getScrollVisualRect ctx idx
          pure (rectIntersect parentClip (Rect x y w h))
        else pure (Just parentClip)

-- Layout position plus ancestor scroll shifts (before applyScrollOffsets runs).
getScrollVisualRect :: Context -> NodeIdx -> IO (Float, Float, Float, Float)
getScrollVisualRect ctx idx = do
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  (dx, dy) <- ancestorScrollShift ctx idx
  pure (x + dx, y + dy, w, h)

updateScrollDrag :: Context -> Input -> IO ()
updateScrollDrag ctx inp = do
  gesture <- readIORef (ctxMenuPointerGesture ctx)
  if gesture
    then when (inputMouseReleased inp) $ writeIORef (ctxScrollDrag ctx) Nothing
    else do
      mDrag <- readIORef (ctxScrollDrag ctx)
      if inputMouseReleased inp
        then writeIORef (ctxScrollDrag ctx) Nothing
        else
          case mDrag of
            Just (wid, grabOff) | inputMouseDown inp -> do
              mGeom <- scrollContainerGeom ctx wid
              case mGeom of
                Nothing -> pure ()
                Just (idx, dir, x, y, w, h, pad, contentSize) -> do
                  off <- getScrollOffset ctx wid
                  let fm = ctxFontMetrics ctx
                  slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
                  case scrollBarLayout (ctxHostProfile ctx) fm slot dir x y w h pad contentSize off of
                    Nothing -> pure ()
                    Just layout -> do
                      let newOff = scrollOffsetFromThumb dir layout grabOff (inputMousePos inp)
                      when (newOff /= off) $ setScrollOffset ctx wid newOff
            Nothing | inputMousePressed inp -> tryStartScrollDrag ctx inp
            _ -> pure ()

scrollContainerGeom ::
  Context -> WidgetId -> IO (Maybe (NodeIdx, DirTag, Float, Float, Float, Float, Padding, Float))
scrollContainerGeom ctx wid = do
  count <- arenaCount (ctxNodeArena ctx)
  let go idx
        | idx >= count = pure Nothing
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            if not (isScrollNode nt)
              then go (idx + 1)
              else do
                w' <- getWidgetId (ctxNodeArena ctx) idx
                if w' /= wid
                  then go (idx + 1)
                  else
                    if isCellHost (ctxHostProfile ctx) && nt == NodeModal
                      then go (idx + 1)
                      else do
                        dir <- getDirection (ctxNodeArena ctx) idx
                        si <- getStyleIdx (ctxNodeArena ctx) idx
                        let cfg = decodeScrollConfig si
                        if scrollChromeSuppressed cfg (isScrollStyle2D si) dir
                          then go (idx + 1)
                          else do
                            pad <- getPadding (ctxNodeArena ctx) idx
                            contentSize <- getNodeValue (ctxNodeArena ctx) idx
                            (x, y, w, h) <- getScrollVisualRect ctx idx
                            pure (Just (idx, dir, x, y, w, h, pad, contentSize))
  go 0

tryStartScrollDrag :: Context -> Input -> IO ()
tryStartScrollDrag ctx inp =
  when (inputMousePressed inp) $ do
    let mouse = inputMousePos inp
    mTarget <- findScrollTargetUnderMouse ctx mouse
    case mTarget of
      Nothing -> pure ()
      Just wid -> tryStartScrollDragOn ctx wid mouse

tryStartScrollDragOn :: Context -> WidgetId -> V2 -> IO ()
tryStartScrollDragOn ctx wid mouse = do
  mGeom <- scrollContainerGeom ctx wid
  case mGeom of
    Nothing -> pure ()
    Just (idx, dir, x, y, w, h, pad, contentSize) -> do
      off <- getScrollOffset ctx wid
      let fm = ctxFontMetrics ctx
      slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
      case scrollBarLayout (ctxHostProfile ctx) fm slot dir x y w h pad contentSize off of
        Nothing -> pure ()
        Just layout -> do
          let thumb = sbThumb layout
              track = sbTrack layout
          if rectContains thumb mouse
            then do
              let grabOff =
                    case dir of
                      DirColumn -> v2Y mouse - rectY thumb
                      DirRow -> v2X mouse - rectX thumb
              writeIORef (ctxScrollDrag ctx) (Just (wid, grabOff))
            else
              when (rectContains track mouse) $ do
                let maxOff = sbMaxOff layout
                    thumbH = rectH thumb
                    thumbW = rectW thumb
                    newOff =
                      case dir of
                        DirColumn ->
                          let trackY = rectY track
                              trackH = rectH track
                              ratio =
                                (v2Y mouse - trackY - thumbH / 2)
                                  / max 1 (trackH - thumbH)
                           in max 0 (min maxOff (ratio * maxOff))
                        DirRow ->
                          let trackX = rectX track
                              trackW = rectW track
                              ratio =
                                (v2X mouse - trackX - thumbW / 2)
                                  / max 1 (trackW - thumbW)
                           in max 0 (min maxOff (ratio * maxOff))
                setScrollOffset ctx wid newOff
                let grabOff =
                      case dir of
                        DirColumn -> thumbH / 2
                        DirRow -> thumbW / 2
                writeIORef (ctxScrollDrag ctx) (Just (wid, grabOff))

paintScrollChrome ::
  Context ->
  DrawArena ->
  NodeIdx ->
  WidgetId ->
  Float ->
  Float ->
  Float ->
  Float ->
  Padding ->
  Theme ->
  Bool ->
  IO ()
paintScrollChrome ctx da idx wid x y w h pad theme terminal = do
  layer <- currentLayer da
  let barLayer =
        case layer of
          LayerOverlay -> LayerChrome
          _ -> LayerContent
  beginLayer da barLayer
  drawScrollBar ctx da idx wid x y w h pad theme terminal
  beginLayer da layer

drawScrollBar ::
  Context ->
  DrawArena ->
  NodeIdx ->
  WidgetId ->
  Float ->
  Float ->
  Float ->
  Float ->
  Padding ->
  Theme ->
  Bool ->
  IO ()
drawScrollBar ctx da idx wid x y w h pad theme terminal = do
  si <- getStyleIdx (ctxNodeArena ctx) idx
  dir <- getDirection (ctxNodeArena ctx) idx
  off <- getScrollOffset ctx wid
  let fm = ctxFontMetrics ctx
  slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
  let base =
        case slot of
          ScrollBarWindow -> themeFloatingWindow theme
          _ -> themePanel theme
      trackBg = scrollBarTrackColor base theme terminal
      thumbCol = scrollBarThumbColor base theme terminal
      drawLayout layout =
        let track = sbTrack layout
            thumb = sbThumb layout
         in if terminal
              then do
                pushRect da track trackBg
                pushRect da thumb thumbCol
              else do
                let trackR = min 4 (min (rectW track) (rectH track) / 2)
                    thumbR = min 4 (min (rectW thumb) (rectH thumb) / 2)
                pushRoundedRect da track trackR trackBg
                pushRoundedRect da thumb thumbR thumbCol
      drawAxis axis contentSize axisOff =
        case scrollBarLayout (ctxHostProfile ctx) fm slot axis x y w h pad contentSize axisOff of
          Nothing -> pure ()
          Just layout -> drawLayout layout
  if isScrollStyle2D si
    then do
      let cfg = decodeScrollConfig si
      contentH <- getNodeValue (ctxNodeArena ctx) idx
      contentW <- getAspect (ctxNodeArena ctx) idx
      V2 offX offY <- getScrollOffset2D ctx wid
      when (scrollShowsChrome cfg True DirColumn) $
        drawAxis DirColumn contentH offY
      when (scrollShowsChrome cfg True DirRow) $
        drawAxis DirRow contentW offX
    else do
      let cfg = decodeScrollConfig si
      contentSize <- getNodeValue (ctxNodeArena ctx) idx
      when (scrollShowsChrome cfg False dir) $
        drawAxis dir contentSize off

