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
import Data.IORef (readIORef)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe)
import NanoUI.Context
  ( Context (..)
  , getMenuPointerGesture
  , getScrollDrag
  , getScrollOffset
  , getScrollOffset2D
  , getStore
  , intKey
  , setScrollDrag
  , setScrollOffset
  , setScrollOffset2D
  )
import NanoUI.Draw (DrawArena, Layer (..), beginLayer, currentLayer, pushRect, pushRoundedRect)
import NanoUI.Font (ScrollBarSlot (..), textDisplayWidth, widgetContentInset)
import NanoUI.Frame.TextEdit
  ( TextAreaGeom (..)
  , TextAreaScrollBarLayouts (..)
  , textAreaBarLanes
  , textAreaGeom
  , textAreaScrollBarLayouts
  )
import NanoUI.Store (storeText)
import NanoUI.Types (HostProfile, isCellHost)
import qualified NanoUI.Widgets.TextBuffer as TB
import NanoUI.Id (WidgetId)
import NanoUI.Input (Input (..), inputMouseDown, inputMousePos, inputMousePressed, inputMouseReleased, inputScroll)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
  , arenaCount
  , getDirection
  , getFirstChild
  , getScrollContentW
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
import NanoUI.Frame.Scroll.Geometry
  ( ScrollBarLayout (..)
  , borderContentClip
  , scrollBarLayout
  , scrollAxisRange
  , scrollChromeLane
  , scrollContentClip
  , scrollOffsetFromThumb
  , scrollViewportClip2D
  , scrollChromeActive
  , scrollChromeSuppressed
  , decodeScrollConfig
  , padContentClip
  , isScrollStyle2D
  , ScrollConfig (..)
  , ScrollPolicy (..)
  )
import NanoUI.Frame.Hit (findNodeByWidgetId, topmostModalAtMouse, topmostOverlayAtMouse)

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
                contentW <- getScrollContentW na idx
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
                V2 offCross offMain <- getScrollOffset2D ctx wid
                let (nsx, nsy) =
                      case dir of
                        DirColumn -> (sx - offCross, sy - offMain)
                        DirRow -> (sx - offMain, sy - offCross)
                setClipRect na idx clip1d
                pure (nsx, nsy, clip1d)
      else do
        case nt of
          NodePanel -> do
            theme <- readIORef (ctxTheme ctx)
            let style = themePanel theme
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

tryApplyScrollWheelDelta :: Context -> WidgetId -> V2 -> IO Bool
tryApplyScrollWheelDelta ctx wid scroll = do
  mGeom <- scrollContainerGeom ctx wid
  case mGeom of
    Nothing -> pure False
    Just (idx, dir, _x, _y, w, h, pad, contentSize) -> do
      nt <- getNodeType (ctxNodeArena ctx) idx
      let step = scrollLineFor (ctxHostProfile ctx)
          innerW = w - padL pad - padR pad
          innerH = h - padT pad - padB pad
      if nt == NodeTextArea
        then do
          store <- getStore ctx
          let key = intKey wid
              text = IM.findWithDefault "" key (storeText store)
              buf = TB.fromText text
              lineTexts = TB.toLines buf
              host = ctxHostProfile ctx
              fm = ctxFontMetrics ctx
              contentW = maximum (0 : [textDisplayWidth host fm l | l <- lineTexts])
              contentH = contentSize
              (barLaneW, barLaneH) = textAreaBarLanes host fm
              hasV0 = contentH > innerH
              hasH0 = contentW > innerW
              hasV = contentH > (if hasH0 then max 0 (innerH - barLaneH) else innerH)
              hasH = contentW > (if hasV0 then max 0 (innerW - barLaneW) else innerW)
              availW = if hasV then max 0 (innerW - barLaneW) else innerW
              availH = if hasH then max 0 (innerH - barLaneH) else innerH
          V2 curX curY <- getScrollOffset2D ctx wid
          let maxX = max 0 (contentW - availW)
              maxY = max 0 (contentH - availH)
              newX = max 0 (min maxX (curX + v2X scroll * step))
              newY = max 0 (min maxY (curY + v2Y scroll * step))
          if newX == curX && newY == curY
            then pure False
            else do
              setScrollOffset2D ctx wid (V2 newX newY)
              pure True
        else do
          si <- getStyleIdx (ctxNodeArena ctx) idx
          if isScrollStyle2D si
            then do
              contentW <- getScrollContentW (ctxNodeArena ctx) idx
              contentH <- getNodeValue (ctxNodeArena ctx) idx
              V2 curX curY <- getScrollOffset2D ctx wid
              let maxX = scrollAxisRange contentW innerW (padR pad)
                  maxY = scrollAxisRange contentH innerH (padB pad)
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
                  | otherwise -> applyAxis cur innerH contentSize (padB pad) (v2Y scroll * step)
                DirRow
                  | scrollPolicyX cfg == ScrollNone -> pure False
                  | otherwise -> applyAxis cur innerW contentSize (padR pad) (v2X scroll * step)
  where
    applyAxis cur inner contentSize trailingPad delta = do
      let maxOff = scrollAxisRange contentSize inner trailingPad
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
  if nt == NodeTextArea
    then do
      (x, y, w, h) <- getScrollVisualRect ctx idx
      let fm = ctxFontMetrics ctx
          geom = textAreaGeom (ctxHostProfile ctx) fm x y w h
          field = tagFieldRect geom
      case rectIntersect clip field of
        Nothing -> pure Nothing
        Just fclip ->
          if rectW fclip > 0 && rectH fclip > 0 && rectContains fclip mouse
            then do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              store <- getStore ctx
              let key = intKey wid
                  text = IM.findWithDefault "" key (storeText store)
                  buf = TB.fromText text
                  lineTexts = TB.toLines buf
                  lineCount = max 1 (length lineTexts)
                  lineH = tagLineHeight geom
                  contentH = fromIntegral lineCount * lineH
                  host = ctxHostProfile ctx
                  contentW = maximum (0 : [textDisplayWidth host fm l | l <- lineTexts])
                  (ix, iy) = widgetContentInset host fm
                  innerW = rectW field - 2 * ix
                  innerH = rectH field - 2 * iy
              if contentH > innerH || contentW > innerW
                then pure (Just idx)
                else pure Nothing
            else pure Nothing
    else if not (isScrollNode nt)
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
            contentW <- getScrollContentW (ctxNodeArena ctx) idx
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

-- | Node rect in screen space, for wheel / thumb / drag hit-testing. Arena
-- rects are already baked to visual coordinates by applyScrollOffsets at the
-- end of every frame; re-applying ancestor scroll shifts here double-counts
-- them and makes hit-testing drift by the scroll offset after any scroll.
getScrollVisualRect :: Context -> NodeIdx -> IO (Float, Float, Float, Float)
getScrollVisualRect ctx idx = getRect (ctxNodeArena ctx) idx

updateScrollDrag :: Context -> Input -> IO ()
updateScrollDrag ctx inp = do
  gesture <- getMenuPointerGesture ctx
  if gesture
    then when (inputMouseReleased inp) $ setScrollDrag ctx Nothing
    else do
      mDrag <- getScrollDrag ctx
      if inputMouseReleased inp
        then setScrollDrag ctx Nothing
        else
          case mDrag of
            Just (wid, dragDir, grabOff) | inputMouseDown inp -> do
              mIdx <- findNodeByWidgetId ctx wid
              case mIdx of
                Nothing -> pure ()
                Just idx -> do
                  nt <- getNodeType (ctxNodeArena ctx) idx
                  if nt == NodeTextArea
                    then do
                      (x, y, w, h) <- getScrollVisualRect ctx idx
                      let host = ctxHostProfile ctx
                          fm = ctxFontMetrics ctx
                          geom = textAreaGeom host fm x y w h
                          field = tagFieldRect geom
                      store <- getStore ctx
                      let key = intKey wid
                          text = IM.findWithDefault "" key (storeText store)
                          buf = TB.fromText text
                          lineTexts = TB.toLines buf
                          lineCount = max 1 (length lineTexts)
                          lineH = tagLineHeight geom
                          contentH = fromIntegral lineCount * lineH
                          contentW = maximum (0 : [textDisplayWidth host fm l | l <- lineTexts])
                      V2 curX curY <- getScrollOffset2D ctx wid
                      let layouts = textAreaScrollBarLayouts host fm field contentW contentH curX curY
                      case dragDir of
                        DirColumn ->
                          case tasbVertical layouts of
                            Nothing -> pure ()
                            Just layout -> do
                              let newY = scrollOffsetFromThumb DirColumn layout grabOff (inputMousePos inp)
                              when (newY /= curY) $ setScrollOffset2D ctx wid (V2 curX newY)
                        DirRow ->
                          case tasbHorizontal layouts of
                            Nothing -> pure ()
                            Just layout -> do
                              let newX = scrollOffsetFromThumb DirRow layout grabOff (inputMousePos inp)
                              when (newX /= curX) $ setScrollOffset2D ctx wid (V2 newX curY)
                     else do
                       mGeom <- scrollContainerGeom ctx wid
                       case mGeom of
                         Nothing -> pure ()
                         Just (idx', dir, x, y, w, h, pad, contentSize) -> do
                           si <- getStyleIdx (ctxNodeArena ctx) idx'
                           if isScrollStyle2D si
                             then do
                               contentW <- getScrollContentW (ctxNodeArena ctx) idx'
                               V2 offX offY <- getScrollOffset2D ctx wid
                               (mV, mH) <-
                                 scrollBarLayouts2D
                                   ctx
                                   idx'
                                   ScrollBars2DGeom
                                     { sb2X = x
                                     , sb2Y = y
                                     , sb2W = w
                                     , sb2H = h
                                     , sb2Pad = pad
                                     , sb2ContentW = contentW
                                     , sb2ContentH = contentSize
                                     , sb2OffX = offX
                                     , sb2OffY = offY
                                     }
                               case dragDir of
                                 DirColumn ->
                                   case mV of
                                     Nothing -> pure ()
                                     Just layout -> do
                                       let newY = scrollOffsetFromThumb DirColumn layout grabOff (inputMousePos inp)
                                       when (newY /= offY) $ setScrollOffset2D ctx wid (V2 offX newY)
                                 DirRow ->
                                   case mH of
                                     Nothing -> pure ()
                                     Just layout -> do
                                       let newX = scrollOffsetFromThumb DirRow layout grabOff (inputMousePos inp)
                                       when (newX /= offX) $ setScrollOffset2D ctx wid (V2 newX offY)
                             else do
                               off <- getScrollOffset ctx wid
                               let fm = ctxFontMetrics ctx
                               slot <- scrollBarSlotOf (ctxNodeArena ctx) idx'
                               case scrollBarLayout (ctxHostProfile ctx) fm slot dir x y w h pad contentSize off of
                                 Nothing -> pure ()
                                 Just layout -> do
                                   let newOff = scrollOffsetFromThumb dir layout grabOff (inputMousePos inp)
                                   when (newOff /= off) $ setScrollOffset ctx wid newOff
            Nothing | inputMousePressed inp -> tryStartScrollDrag ctx inp
            _ -> pure ()

-- | Named geometry for a native 2D scroll container's two scrollbar layouts.
-- The four trailing Floats of the positional form (content and offset per
-- axis) transpose silently, so keep them named.
data ScrollBars2DGeom = ScrollBars2DGeom
  { sb2X :: !Float
  , sb2Y :: !Float
  , sb2W :: !Float
  , sb2H :: !Float
  , sb2Pad :: Padding
  , sb2ContentW :: !Float
  , sb2ContentH :: !Float
  , sb2OffX :: !Float
  , sb2OffY :: !Float
  }

-- | Both-axis scrollbar layouts for a native 2D scroll container: (vertical,
-- horizontal). Nothing per axis when that axis does not overflow.
scrollBarLayouts2D :: Context -> NodeIdx -> ScrollBars2DGeom -> IO (Maybe ScrollBarLayout, Maybe ScrollBarLayout)
scrollBarLayouts2D ctx idx g = do
  let host = ctxHostProfile ctx
      fm = ctxFontMetrics ctx
  slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
  let v = scrollBarLayout host fm slot DirColumn (sb2X g) (sb2Y g) (sb2W g) (sb2H g) (sb2Pad g) (sb2ContentH g) (sb2OffY g)
      hr = scrollBarLayout host fm slot DirRow (sb2X g) (sb2Y g) (sb2W g) (sb2H g) (sb2Pad g) (sb2ContentW g) (sb2OffX g)
  pure (v, hr)

scrollContainerGeom ::
  Context -> WidgetId -> IO (Maybe (NodeIdx, DirTag, Float, Float, Float, Float, Padding, Float))
scrollContainerGeom ctx wid = do
  count <- arenaCount (ctxNodeArena ctx)
  let go idx
        | idx >= count = pure Nothing
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            if nt == NodeTextArea
              then do
                w' <- getWidgetId (ctxNodeArena ctx) idx
                if w' /= wid
                  then go (idx + 1)
                  else do
                    (x, y, w, h) <- getScrollVisualRect ctx idx
                    let fm = ctxFontMetrics ctx
                        host = ctxHostProfile ctx
                        geom = textAreaGeom host fm x y w h
                        field = tagFieldRect geom
                        (ix, iy) = widgetContentInset host fm
                        pad = Padding ix ix iy iy
                    store <- getStore ctx
                    let key = intKey wid
                        text = IM.findWithDefault "" key (storeText store)
                        buf = TB.fromText text
                        lineCount = max 1 (TB.getLineCount buf)
                        lineH = tagLineHeight geom
                        contentSize = fromIntegral lineCount * lineH
                    pure (Just (idx, DirColumn, rectX field, rectY field, rectW field, rectH field, pad, contentSize))
              else if not (isScrollNode nt)
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
  mIdx <- findNodeByWidgetId ctx wid
  case mIdx of
    Nothing -> pure ()
    Just idx -> do
      nt <- getNodeType (ctxNodeArena ctx) idx
      if nt == NodeTextArea
        then do
          (x, y, w, h) <- getScrollVisualRect ctx idx
          let host = ctxHostProfile ctx
              fm = ctxFontMetrics ctx
              geom = textAreaGeom host fm x y w h
              field = tagFieldRect geom
          store <- getStore ctx
          let key = intKey wid
              text = IM.findWithDefault "" key (storeText store)
              buf = TB.fromText text
              lineTexts = TB.toLines buf
              lineCount = max 1 (length lineTexts)
              lineH = tagLineHeight geom
              contentH = fromIntegral lineCount * lineH
              contentW = maximum (0 : [textDisplayWidth host fm l | l <- lineTexts])
          V2 curX curY <- getScrollOffset2D ctx wid
          let layouts = textAreaScrollBarLayouts host fm field contentW contentH curX curY
          case tasbVertical layouts of
            Just layout | rectContains (sbThumb layout) mouse -> do
              let grabOff = v2Y mouse - rectY (sbThumb layout)
              setScrollDrag ctx (Just (wid, DirColumn, grabOff))
            Just layout | rectContains (sbTrack layout) mouse -> do
              let maxOff = sbMaxOff layout
                  thumb = sbThumb layout
                  track = sbTrack layout
                  thumbH = rectH thumb
                  trackY = rectY track
                  trackH = rectH track
                  ratio = (v2Y mouse - trackY - thumbH / 2) / max 1 (trackH - thumbH)
                  newOff = max 0 (min maxOff (ratio * maxOff))
              setScrollOffset2D ctx wid (V2 curX newOff)
              setScrollDrag ctx (Just (wid, DirColumn, thumbH / 2))
            _ ->
              case tasbHorizontal layouts of
                Just layout | rectContains (sbThumb layout) mouse -> do
                  let grabOff = v2X mouse - rectX (sbThumb layout)
                  setScrollDrag ctx (Just (wid, DirRow, grabOff))
                Just layout | rectContains (sbTrack layout) mouse -> do
                  let maxOff = sbMaxOff layout
                      thumb = sbThumb layout
                      track = sbTrack layout
                      thumbW = rectW thumb
                      trackX = rectX track
                      trackW = rectW track
                      ratio = (v2X mouse - trackX - thumbW / 2) / max 1 (trackW - thumbW)
                      newOff = max 0 (min maxOff (ratio * maxOff))
                  setScrollOffset2D ctx wid (V2 newOff curY)
                  setScrollDrag ctx (Just (wid, DirRow, thumbW / 2))
                _ -> pure ()
        else do
          mGeom <- scrollContainerGeom ctx wid
          case mGeom of
            Nothing -> pure ()
            Just (idx', dir, x, y, w, h, pad, contentSize) -> do
              si <- getStyleIdx (ctxNodeArena ctx) idx'
              if isScrollStyle2D si
                then do
                  contentW <- getScrollContentW (ctxNodeArena ctx) idx'
                  V2 curX curY <- getScrollOffset2D ctx wid
                  (mV, mH) <-
                    scrollBarLayouts2D
                      ctx
                      idx'
                      ScrollBars2DGeom
                        { sb2X = x
                        , sb2Y = y
                        , sb2W = w
                        , sb2H = h
                        , sb2Pad = pad
                        , sb2ContentW = contentW
                        , sb2ContentH = contentSize
                        , sb2OffX = curX
                        , sb2OffY = curY
                        }
                  case tryAxis DirColumn mV (curX, curY) of
                    Just start -> start
                    Nothing ->
                      case tryAxis DirRow mH (curX, curY) of
                        Just start -> start
                        Nothing -> pure ()
                else do
                  off <- getScrollOffset ctx wid
                  let fm = ctxFontMetrics ctx
                  slot <- scrollBarSlotOf (ctxNodeArena ctx) idx'
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
                          setScrollDrag ctx (Just (wid, dir, grabOff))
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
                            setScrollDrag ctx (Just (wid, dir, grabOff))
   where
    -- Thumb grab, else track jump-and-drag, for one axis of a 2D scroller.
    -- The other axis component of the shared 2D offset is preserved.
    tryAxis axis mLayout (curX, curY) = case mLayout of
      Nothing -> Nothing
      Just layout
        | rectContains (sbThumb layout) mouse ->
            Just
              ( setScrollDrag
                  ctx
                  ( Just
                      ( wid
                      , axis
                      , case axis of
                          DirColumn -> v2Y mouse - rectY (sbThumb layout)
                          DirRow -> v2X mouse - rectX (sbThumb layout)
                      )
                  )
              )
        | rectContains (sbTrack layout) mouse ->
            let maxOff = sbMaxOff layout
                thumb = sbThumb layout
                track = sbTrack layout
                thumbMain = case axis of DirColumn -> rectH thumb; DirRow -> rectW thumb
                trackPos = case axis of DirColumn -> rectY track; DirRow -> rectX track
                trackMain = case axis of DirColumn -> rectH track; DirRow -> rectW track
                mouseMain = case axis of DirColumn -> v2Y mouse; DirRow -> v2X mouse
                ratio = (mouseMain - trackPos - thumbMain / 2) / max 1 (trackMain - thumbMain)
                newOff = max 0 (min maxOff (ratio * maxOff))
                jump =
                  case axis of
                    DirColumn -> setScrollOffset2D ctx wid (V2 curX newOff)
                    DirRow -> setScrollOffset2D ctx wid (V2 newOff curY)
             in Just
                  ( do
                      jump
                      setScrollDrag ctx (Just (wid, axis, thumbMain / 2))
                  )
        | otherwise -> Nothing

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
  let padClip = padContentClip (ctxHostProfile ctx) fm x y w h pad
      innerW = rectW padClip
      innerH = rectH padClip
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
      contentW <- getScrollContentW (ctxNodeArena ctx) idx
      V2 offX offY <- getScrollOffset2D ctx wid
      when (scrollChromeActive cfg True DirColumn contentH innerH) $
        drawAxis DirColumn contentH offY
      when (scrollChromeActive cfg True DirRow contentW innerW) $
        drawAxis DirRow contentW offX
    else do
      let cfg = decodeScrollConfig si
      contentSize <- getNodeValue (ctxNodeArena ctx) idx
      let innerMain =
            case dir of
              DirColumn -> innerH
              DirRow -> innerW
      when (scrollChromeActive cfg False dir contentSize innerMain) $
        drawAxis dir contentSize off

