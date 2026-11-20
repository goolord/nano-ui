-- | Position pass, container/scroll/grid/wrap placement, and floating panels.
module NanoUI.Internal.Layout.Solve.Position
  ( positionNodeA
  , placeFloatingNodes
  , computePopupPosition
  , placeWindowNode
  , scrollBarSlotOf
  ) where

import Control.Monad (foldM, foldM_, join, unless, when, zipWithM_)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe, isJust)
import Data.Primitive.PrimArray (copyMutablePrimArray, readPrimArray, writePrimArray)
import NanoUI.Internal.Font
import NanoUI.Internal.Frame.Scroll.Geometry
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Layout.Solve.Flex
import NanoUI.Internal.Layout.Solve.Measure
import NanoUI.Internal.Style (AlignX (..), AlignY (..), Flow (..), Padding (..), lineAlignFraction, windowMargin)
import NanoUI.Internal.Types (PopupAnchor (..), PopupPlacement (..), Rect (..), V2 (..), clamp, foldUpTo, forUpTo_)
import NanoUI.Internal.WidgetText

-- | Place a widget's adornment rows ('adornRows'), each centred on its
-- height: a field's at its content insets, a button's either side of its
-- label in its centred group ('adornedButtonGroup').
positionAdornments :: SolveEnv -> Int -> NodeIdx -> NodeType -> Rect -> IO ()
positionAdornments env@SolveEnv {seArena = na, seArrays = a} depth idx nt (Rect x y w h) = do
  rows@(AdornRows li lw ti tw _) <- adornRows na idx
  si <- getStyleIdx na idx
  measurer <- nodeMeasurer env idx nt si
  (x0, x1) <- case nt of
    NodeTextInput -> do
      let (ix, _) = widgetContentInset (tmMetrics measurer)
      pure (x + ix, x + w - ix)
    _ -> do
      gap <- readStyle a idx StyleGap
      txt <- getText na idx
      (_, groupW) <- adornedButtonGroup measurer txt gap rows
      let x0
            | nt == NodeButton && tabLabelAtStart si = x + tabHeaderPadX
            | otherwise = alignX AlignCenter x w groupW
      pure (x0, x0 + groupW)
  let place ci cx cw = when (ci >= 0) $ do
        ch <- readGeom a ci GeomH
        positionNodeA env (depth + 1) ci (Rect cx (y + (h - ch) / 2) cw ch)
  place li x0 lw
  place ti (x1 - tw) tw

-- | Place node @idx@ and its subtree in the rect offered it. In the solve's
-- position pass, record the rect, or put the subtree back from the cache
-- when the captured solve placed it as this one would ('reusePositions').
positionNodeA ::
  SolveEnv ->
  Int ->
  NodeIdx ->
  Rect ->
  IO ()
positionNodeA env !depth !idx rect@(Rect x y w h) = case sePositions env of
  Nothing -> positionNode env depth idx rect
  Just pl@PositionLog {plOffered} -> do
    let !o = idx * 4
    offeredBefore <- not . isNaN <$> readPrimArray plOffered o
    if offeredBefore
      -- A second offer reads what the first placed, so neither is put back.
      then writePrimArray plOffered (o + 2) (0 / 0) >> positionNode env depth idx rect
      else do
        reused <- case seCache env of
          Just lc -> reusePositions env lc pl idx rect
          Nothing -> pure False
        unless reused $ do
          writePrimArray plOffered o x
          writePrimArray plOffered (o + 1) y
          writePrimArray plOffered (o + 2) w
          writePrimArray plOffered (o + 3) h
          positionNode env depth idx rect

-- | Put back what the captured solve's position pass left in @idx@'s subtree
-- (its unsnapped rects, the rects offered its nodes and its scroll extents),
-- when the measure pass found that solve placed the subtree as this one
-- would ('plSpans') and it offered the node @rect@ too. 'False' when not.
reusePositions :: SolveEnv -> LayoutCache -> PositionLog -> NodeIdx -> Rect -> IO Bool
reusePositions SolveEnv {seArena = na, seArrays = a} lc PositionLog {plOffered, plSpans} idx (Rect x y w h) = do
  end <- readPrimArray plSpans idx
  let prev = lcOffered lc
      !o = idx * 4
  same <-
    if end < 0 || end > lcCount lc
      then pure False
      else do
        px <- readPrimArray prev o
        py <- readPrimArray prev (o + 1)
        pw <- readPrimArray prev (o + 2)
        ph <- readPrimArray prev (o + 3)
        pure (px == x && py == y && pw == w && ph == h)
  when same $ do
    copyMutablePrimArray plOffered o prev o ((end - idx) * 4)
    let copyNode !i = when (i < end) $ do
          let at k = readPrimArray (lcUnsnapped lc) (i * 4 + k)
          join (setRect na i <$> at 0 <*> at 1 <*> at 2 <*> at 3)
          nt <- readTagEnum a i TagNodeType
          when (isScrollNode nt) (restoreScrollExtents a lc i)
          copyNode (i + 1)
    copyNode idx
  pure same

positionNode ::
  SolveEnv ->
  Int ->
  NodeIdx ->
  Rect ->
  IO ()
positionNode env@SolveEnv {seArena = na, seArrays = a} !depth !idx (Rect x y availW availH) = do
  wAx <- readAxisSizing a idx True
  hAx@(AxisSizing hTag _ minH maxH) <- readAxisSizing a idx False
  intrinsicW <- readGeom a idx GeomW
  intrinsicH <- readGeom a idx GeomH
  nt <- readTagEnum a idx TagNodeType
  let !w = resolveSize wAx intrinsicW availW
      !resolvedH = resolveSize hAx intrinsicH availH
  ratio <- if hTag == SizingFit then readStyle a idx StyleAspect else pure 0
  h <- case nt of
    -- A fit height with an aspect ratio follows the assigned width.
    _ | ratio > 0 -> pure (clamp minH maxH (w / ratio))
    -- Text outside a row wraps at the assigned width.
    NodeText | hTag /= SizingFixed -> do
      isRowChild <- parentIsRow na idx
      if isRowChild then pure resolvedH else textHeightAt env idx hAx w (axTag wAx /= SizingFit) resolvedH
    _ | (nt == NodeContainer || nt == NodePanel) && hTag == SizingFit -> pure (clamp minH maxH (max intrinsicH availH))
    -- A measured drawing laid out at a new width recomputes its height.
    NodeDrawing | hTag == SizingFit && w /= intrinsicW -> drawingHeightAt env idx w hAx resolvedH
    _ -> pure resolvedH
  setRect na idx x y w h
  if isContainerNode nt
    then do
      (pad, gap, dir) <- containerFlow a idx
      (if isScrollNode nt then positionScrollChildren else positionChildren)
        env depth idx dir gap pad (Rect x y w h)
    else do
      kids <- readTree a idx TreeFirstChild
      when (kids >= 0) $ positionAdornments env depth idx nt (Rect x y w h)
  when (hTag == SizingFit && ratio <= 0 && isContainerNode nt && not (isScrollNode nt)) $
    adjustFitHeight na idx minH maxH x y w

adjustFitHeight :: NodeArena -> NodeIdx -> Float -> Float -> Float -> Float -> Float -> IO ()
adjustFitHeight na idx minH maxH x y w = do
  fc <- getFirstChild na idx
  when (fc >= 0) $ do
    pad <- getPadding na idx
    let step maxB ci = do
          Rect _ subY _ subH <- getNodeRect na ci
          pure (max maxB (subY + subH))
    maxB <- foldFlowChildrenM na idx step y
    let fitH = clamp minH maxH (maxB + padB pad - y)
    Rect _ _ _ curH <- getNodeRect na idx
    -- Children sit at raw positions, so only float error separates their
    -- bottom from the measured height; the epsilon keeps that from growing
    -- every nested content-sized level.
    when (fitH > curH + 1.0e-3) $
      setRect na idx x y w fitH

positionScrollChildren ::
  SolveEnv ->
  Int ->
  NodeIdx ->
  DirTag ->
  Float ->
  Padding ->
  Rect ->
  IO ()
positionScrollChildren env@SolveEnv {seArena = na} depth idx dir gap pad (Rect px py pw ph) = do
  si <- getStyleIdx na idx
  contentSize <- getNodeValue na idx
  slot <- scrollBarSlotOf na idx
  let Rect cx cy innerW innerH = padContentClip px py pw ph pad
      cfg = decodeScrollConfig si
  if isScrollStyle2D si
    then do
      contentW <- getScrollContentW na idx
      let (gutterW, gutterH) = scrollGutters2D slot cfg pad contentW contentSize innerW innerH
          viewW = max 0 (innerW - gutterW)
          viewH = max 0 (innerH - gutterH)
          -- Keep measured content. Shrinking to the clip wraps table columns.
          layoutW = max contentW viewW
          layoutH = max contentSize viewH
          -- cx/cy and the layout box are already inside the padding.
          box = Rect cx cy layoutW layoutH
      -- The children line up along the scroller's direction, as they
      -- measured, and as in a 1D scroller a grid layout makes no grid.
      case dir of
        DirRow -> positionRowFromParent env depth idx gap box
        DirColumn -> positionColumn env depth idx gap False Nothing cx layoutW box
      positionLayered env depth idx False box
    else do
      let gutterCol = scrollAxisGutter (scrollPolicyY cfg) slot (padR pad) contentSize innerH
          gutterRow = scrollAxisGutter (scrollPolicyX cfg) slot (padB pad) contentSize innerW
      box <- case dir of
        DirRow -> do
          wTag <- axTag <$> getWidthSizing na idx
          let rowMain = if wTag == SizingGrow then max contentSize innerW else contentSize
              b = Rect cx cy rowMain (innerH - gutterRow)
          b <$ positionRowFromParent env depth idx gap b
        DirColumn -> do
          let viewW = innerW - gutterCol
              b = Rect cx cy viewW innerH
          b <$ positionColumn env depth idx gap False (Just contentSize) cx viewW b
      positionLayered env depth idx False box
  fc <- getFirstChild na idx
  when (fc >= 0) $ do
    let step (FlowAcc count maxB maxR) ci = do
          Rect subX subY subW subH <- getNodeRect na ci
          pure (FlowAcc (count + 1) (max maxB (subY + subH)) (max maxR (subX + subW)))
    -- Pinned children count too, so the scroller reaches them.
    FlowAcc _ maxB maxR <- foldPlacedChildrenM na idx step (FlowAcc 0 cy cx)
    -- Content size is measured from the content origin (px+padL, py+padT) so
    -- it compares against the padded viewport (innerW/innerH) on the same
    -- scale. Measuring from the padding-box origin double-counts the leading
    -- padding and makes a child that exactly fills the viewport look
    -- padX/padY bigger, surfacing a phantom scrollbar on padded scrollers.
    -- The trailing padding is excluded here too (so it cannot
    -- surface a bar by itself); scrollAxisRange adds it back into the
    -- reachable range once an axis genuinely overflows, so scrolling to the
    -- end still reveals it.
    let actualContentH = maxB - py - padT pad
        actualContentW = maxR - px - padL pad
        raise get set v = get na idx >>= \old -> set na idx (max old v)
    if isScrollStyle2D si
      then do
        raise getNodeValue setSolvedValue actualContentH
        raise getScrollContentW setScrollContentW actualContentW
      else raise getNodeValue setSolvedValue $
        case dir of DirColumn -> actualContentH; DirRow -> actualContentW

-- | Left edge of column child @ci@ in a column of width @cw@ at @cx@, and the
-- width the column offers it: all of @cw@, or a percentage's share of it.
-- Grow children already take the full width; alignment is for content
-- narrower than the column, not for shifting a full-width box past it. A
-- child is aligned at its resolved width, so text measured wider than the
-- column wraps to it instead of being pushed past the edge.
{-# INLINE columnChildX #-}
columnChildX :: NodeArena -> NodeIdx -> Float -> Float -> IO (Float, Float)
columnChildX na ci cx cw = do
  wAx <- getWidthSizing na ci
  if axTag wAx == SizingGrow
    then pure (cx, cw)
    else do
      Rect _ _ iw _ <- getNodeRect na ci
      ax <- getAlignX na ci
      let !offer = percentOr wAx cw cw
          !x = alignX ax cx cw (resolveSize wAx iw offer)
      pure (x, offer)

-- | The size a node takes along an axis where @avail@ is offered, from its
-- sizing, its measured size @intrinsic@ and its limits.
{-# INLINE resolveSize #-}
resolveSize :: AxisSizing -> Float -> Float -> Float
resolveSize (AxisSizing tag val lo hi) intrinsic avail =
  clamp lo hi $ case tag of
    SizingFixed -> val
    SizingGrow -> min avail hi
    SizingPercent -> min avail hi
    _ -> min intrinsic avail

positionChildren ::
  SolveEnv ->
  Int ->
  NodeIdx ->
  DirTag ->
  Float ->
  Padding ->
  Rect ->
  IO ()
positionChildren env@SolveEnv {seArrays = a} depth idx dir gap pad (Rect px py pw ph) = do
  nt <- readTagEnum a idx TagNodeType
  gCols <- readTree a idx TreeGridCols
  minColW <- readStyle a idx StyleGridMinColW
  flow <- readTagEnum a idx TagFlow
  let chrome = isChromeColumn nt dir
      cbox = padContentClip px py pw ph pad
  if gCols > 0 || minColW > 0
    then positionGrid env depth idx gCols minColW gap cbox
    else case flow of
      -- Layered children are placed with the pinned ones, below.
      Layered -> pure ()
      Wrap -> positionWrap env depth idx dir gap cbox
      Line -> case dir of
        DirRow -> positionRowFromParent env depth idx gap cbox
        DirColumn -> positionColumn env depth idx gap chrome Nothing px pw cbox
  positionLayered env depth idx (flow == Layered) cbox

childRowCrossSize :: NodeArena -> NodeIdx -> Float -> IO Float
childRowCrossSize na ci availCross = do
  hAx@(AxisSizing hTag _ minH _) <- getHeightSizing na ci
  Rect _ _ _ intrinsic <- getNodeRect na ci
  pure $
    if hTag == SizingFit || hTag == SizingShrink
      -- Fit/Shrink keep the measured box. Do not use the wrap-line
      -- or row slot as availH: that stretches every child when leftover
      -- leaks into scratch `fh`.
      then max minH intrinsic
      else resolveSize hAx intrinsic (percentOr hAx availCross availCross)

-- | Copy the first @n@ scratch child indices, and beside them the scratch
-- array @sizes@ picks, to the snapshot for nesting depth @depth@, and run
-- @act@ on the copies. Laying out a child reuses the scratch, so a container
-- walks its children from the snapshot.
{-# INLINE withSnapshot #-}
withSnapshot ::
  NodeArena -> Int -> Int -> (FlexScratch -> IOArr Float) ->
  (IOArr Int -> IOArr Float -> IO a) -> IO a
withSnapshot na depth n sizes act = do
  s <- readIORef (naScratch na)
  AxisSnapshot idxSnap sizeSnap <- ensureAxisSnapshot na depth n
  copyMutablePrimArray idxSnap 0 (fsIdx s) 0 n
  copyMutablePrimArray sizeSnap 0 (sizes s) 0 n
  act idxSnap sizeSnap

positionRowFromParent ::
  SolveEnv ->
  Int ->
  NodeIdx ->
  Float ->
  Rect ->
  IO ()
positionRowFromParent env@SolveEnv {seArena = na} depth parent gap (Rect cx cy cw ch) = do
  n <- loadChildrenScratch na parent (flowChildSize env False cw ch)
  distributeScratch na n cw (gap * fromIntegral (max 0 (n - 1))) True
  withSnapshot na depth n fsOut $ \idxSnap outSnap ->
    placeRowLine env depth idxSnap outSnap n gap cx cy ch

-- | Place the first @k@ snapshot children left to right from @x0@, each as
-- wide as its @outSnap@ share, in a line @cross@ tall at @top@.
-- Baseline-aligned children share the lowest of their baselines, so the
-- child with the tallest ascent touches the top.
placeRowLine :: SolveEnv -> Int -> IOArr Int -> IOArr Float -> Int -> Float -> Float -> Float -> Float -> IO ()
placeRowLine env@SolveEnv {seArena = na} !depth idxSnap outSnap !k !gap !x0 !top !cross = do
  let lowestBaseline acc i = do
        ci <- readPrimArray idxSnap i
        ay <- getAlignY na ci
        if ay /= AlignBaseline
          then pure acc
          else max acc <$> (childRowCrossSize na ci cross >>= childBaseline env ci)
  base <- foldUpTo k lowestBaseline 0
  -- The cursor stays in raw floats, never snapped: rounding it re-compounds
  -- error every child (1.667 -> 2.0 -> ...) so a shrink row overruns its
  -- fixed width. Each child's far edge is the next one's raw origin, so
  -- quantizeResultsA, which snaps edges, puts both on the same pixel.
  let go !i !x = when (i < k) $ do
          ci <- readPrimArray idxSnap i
          fw <- readPrimArray outSnap i
          -- Fit/fixed children keep content height. Only Grow/Percent eat `cross`.
          crossH <- childRowCrossSize na ci cross
          ay <- getAlignY na ci
          fy <-
            if ay == AlignBaseline
              then (\b -> top + base - b) <$> childBaseline env ci crossH
              else pure (alignY ay top cross crossH)
          positionNodeA env (depth + 1) ci (Rect x fy fw crossH)
          -- The next child starts at this one's far edge, or at its share's
          -- when it came out wider (a minimum width past its share), so
          -- the row keeps its length. A max width already caps its share
          -- ('distributeScratch').
          placedW <- readGeom (seArrays env) ci GeomW
          go (i + 1) (x + min fw placedW + gap)
  go 0 x0

positionGrid ::
  SolveEnv ->
  Int ->
  NodeIdx ->
  Int ->
  Float ->
  Float ->
  Rect ->
  IO ()
positionGrid env@SolveEnv {seArena = na} depth parent gCols minColW gap (Rect cx cy cw ch) = do
  let cols = gridColumnCount gCols minColW cw gap
      colW = max 0 ((cw - gap * fromIntegral (cols - 1)) / fromIntegral cols)
  -- Refit at the column width before freezing row heights. The enclosing
  -- column's height query uses the same per-node, per-width memo.
  n <- loadChildrenScratch na parent (flowChildSize env True colW ch)
  when (n > 0) $ do
    let numRows = (n + cols - 1) `quot` cols
    -- Freeze child indices and their measured cross sizes before recursing.
    -- Children reuse the working scratch while this grid iterates rows and
    -- columns, so the live arrays would be clobbered by the first child.
    withSnapshot na depth n fsH $ \idxArr hArr -> do
      let goRows !r !curY = when (r < numRows) $ do
            rowH <- gridRowHeight hArr n cols r
            forUpTo_ (min cols (n - r * cols)) $ \j -> do
              ci <- readPrimArray idxArr (r * cols + j)
              wAx <- getWidthSizing na ci
              hAx <- getHeightSizing na ci
              Rect _ _ iw _ <- getNodeRect na ci
              fh <- readPrimArray hArr (r * cols + j)
              -- A cell is offered its own height, refit at the column width,
              -- where its alignment puts it, as a row places its children
              -- ('placeRowLine'). Offered the whole row there, a fit
              -- container would take all of it and overrun the row.
              let offerW = percentOr wAx colW colW
                  childW = resolveSize wAx iw offerW
                  childH =
                    if axTag hAx == SizingPercent
                      then percentOr hAx rowH rowH
                      else resolveSize hAx fh rowH
                  itemX = cx + fromIntegral j * (colW + gap)
              ax <- getAlignX na ci
              ay <- getAlignY na ci
              let fx = alignX ax itemX colW childW
                  fy = alignY ay curY rowH childH
              positionNodeA env (depth + 1) ci (Rect fx fy offerW childH)
            goRows (r + 1) (curY + rowH + gap)
      goRows 0 cy

-- | Lay out a wrapping container's flow children in lines along @dir@
-- within the content box ('wrapLines'), separated by the line gap.
-- Within a line, children share its spare or missing length as in a row or
-- column ('distributeScratch'), and sit across it by alignment or fill it
-- if they grow. A line with room left over shifts by the container's line
-- alignment ('TagLineAlign'). A line is as thick as its thickest child
-- ('wrapLineCross'), or whatever a placed child grew to.
positionWrap :: SolveEnv -> Int -> NodeIdx -> DirTag -> Float -> Rect -> IO ()
positionWrap env@SolveEnv {seArena = na, seArrays = a} depth parent dir gap (Rect cx cy cw ch) = do
  lineGap <- readStyle a parent StyleLineGap
  align <- lineAlignFraction <$> readTagEnum a parent TagLineAlign
  let row = dir == DirRow
      limit = if row then cw else ch
      along (_, (w, h)) = if row then w else h
      gaps k = gap * fromIntegral (k - 1)
  kids <- mapM (\ci -> (ci,) <$> flowChildSize env False cw ch ci) =<< flowChildrenInOrder na parent
  let placeLine !lineAt line = do
        let k = length line
        FlexScratch {fsIdx = idxArr, fsW = wArr, fsH = hArr, fsOut = outArr} <- ensureScratchCapacity na k
        zipWithM_ (\j (ci, (w, h)) -> writePrimArray idxArr j ci >> writePrimArray wArr j w >> writePrimArray hArr j h) [0 ..] line
        distributeScratch na k limit (gaps k) row
        lineCross <- wrapLineCross env dir line
        -- Shift the line by its alignment's share of the leftover room.
        shift <-
          if align <= 0
            then pure 0
            else (\used -> max 0 (limit - used) * align) <$> foldUpTo k (\t i -> (t +) <$> readPrimArray outArr i) (gaps k)
        withSnapshot na depth k fsOut $ \idxSnap outSnap ->
          if row
            then placeRowLine env depth idxSnap outSnap k gap (cx + shift) lineAt lineCross
            else placeColumnLine env depth idxSnap outSnap k gap False Nothing 0 0 (Rect lineAt (cy + shift) lineCross ch)
        -- The next line starts past this one, or past any child that grew.
        let reach m (ci, _) = do
              Rect x y w h <- getNodeRect na ci
              pure (max m (if row then y + h else x + w))
        (+ lineGap) <$> foldM reach (lineAt + lineCross) line
  foldM_ placeLine (if row then cy else cx) (wrapLines limit gap along kids)

-- | Place the children of node @idx@ that draw over the rest, in its content
-- box: every child of a layered container (@allLayered@), plus pinned
-- children. Each sits where its alignment puts it; pinned ones then move by
-- their offsets, so an offset from the end edge anchors to that edge.
--
-- A grow child fills the box (from a pin's offset to its unaligned edge), a
-- percentage is of the box, and anything else keeps its measured size, capped
-- at the box width for layers. A fit height is recomputed at the new width.
positionLayered :: SolveEnv -> Int -> NodeIdx -> Bool -> Rect -> IO ()
positionLayered env@SolveEnv {seArena = na, seArrays = a} depth idx allLayered (Rect cx cy cw ch) = do
  pinnedBelow <- readTagEnum a idx TagPinnedBelow
  when (allLayered || pinnedBelow) $ forChildNodes_ na idx $ \ci -> do
    floating <- isFloatingNode <$> readTagEnum a ci TagNodeType
    pinned <- readTagEnum a ci TagPinned
    when ((allLayered || pinned) && not floating) $ do
      -- Only a pinned node has an offset.
      ox <- readStyle a ci StylePinX
      oy <- readStyle a ci StylePinY
      ax <- readTagEnum a ci TagAlignX
      ay <- readTagEnum a ci TagAlignY
      wAx <- readAxisSizing a ci True
      hAx@(AxisSizing hTag _ _ _) <- readAxisSizing a ci False
      iw <- readGeom a ci GeomW
      ih <- readGeom a ci GeomH
      let -- Room a grow child gets beside its offset: past it at the start,
          -- short of it at the end, inset on both sides when centred.
          room (AxisSizing tag val _ _) atStart atEnd box offset other = case tag of
            SizingGrow
              | atStart -> max 0 (box - offset)
              | atEnd -> max 0 (box + offset)
              | otherwise -> max 0 (box - 2 * abs offset)
            SizingPercent -> box * val / 100
            _ -> other
          w = resolveSize wAx iw (room wAx (ax == AlignStart) (ax == AlignEnd) cw ox (if pinned then iw else cw))
      fitH <-
        if (hTag == SizingFit || hTag == SizingShrink) && w /= iw
          then recomputeFitHeightAtWidth env ci w
          else pure ih
      let h = resolveSize hAx fitH (room hAx (ay /= AlignMiddle && ay /= AlignBottom) (ay == AlignBottom) ch oy fitH)
      positionNodeA env (depth + 1) ci (Rect (alignX ax cx cw w + ox) (alignY ay cy ch h + oy) w h)

-- | Place a column's flow children top to bottom in the box @cx cy cw ch@. A
-- scroll column shares out its content height (@scrollContent@) and gives a
-- scroll child no more than the part of the viewport below its top; any other
-- column shares out @ch@. In a window's or modal's column (@chrome@) a
-- separator spans the padding box (@px@, @pw@) and takes no gap before it.
positionColumn ::
  SolveEnv ->
  Int ->
  NodeIdx ->
  Float ->
  Bool ->
  Maybe Float ->
  Float ->
  Float ->
  Rect ->
  IO ()
positionColumn env@SolveEnv {seArena = na} !depth !parent !gap chrome scrollContent !px !pw box@(Rect _ _ cw ch) = do
  n <- loadChildrenScratch na parent (flowChildSize env True cw ch)
  -- The gaps come out of the height shared, as in a row, or grow children
  -- overflow the column by them.
  gapSum <-
    if chrome then columnGapSumScratch na n gap else pure (gap * fromIntegral (max 0 (n - 1)))
  distributeScratch na n (fromMaybe ch scrollContent) gapSum False
  withSnapshot na depth n fsOut $ \idxSnap outSnap ->
    placeColumnLine env depth idxSnap outSnap n gap chrome scrollContent px pw box

-- | Place the first @k@ snapshot children top to bottom in the box, each as
-- tall as its @outSnap@ share. Arguments after @gap@ are as for
-- 'positionColumn'.
placeColumnLine ::
  SolveEnv -> Int -> IOArr Int -> IOArr Float -> Int -> Float ->
  Bool -> Maybe Float -> Float -> Float -> Rect -> IO ()
placeColumnLine env@SolveEnv {seArena = na} !depth idxSnap outSnap !k !gap chrome scrollContent !px !pw (Rect cx cy cw ch) =
  go 0 cy
  where
    go !i !y = when (i < k) $ do
      ci <- readPrimArray idxSnap i
      fh <- readPrimArray outSnap i
      nt <- getNodeType na ci
      (fx, nodeW) <-
        if chrome && nt == NodeSeparator
          then pure (px, pw)
          else columnChildX na ci cx cw
      let childH = case scrollContent of
            Just _ | isScrollNode nt -> min fh (max 0 (ch - (y - cy)))
            _ -> fh
      positionNodeA env (depth + 1) ci (Rect fx y nodeW childH)
      Rect _ _ _ placedH <- getNodeRect na ci
      gapAfter <-
        if i + 1 >= k
          then pure 0
          else readPrimArray idxSnap (i + 1) >>= \b -> pairColumnGap na chrome b gap
      go (i + 1) (y + placedH + gapAfter)

-- | Place the measured modals, windows and popups within a window @winW@
-- by @winH@ logical pixels, and lay out their children. A modal is centred,
-- leaving the standard window margin where space permits. A window takes its
-- saved position and size (@lookupPos@, @lookupSize@), by default its
-- measured size near the top-right corner. A popup goes where its registered
-- anchor, side and gap put it ('computePopupPosition'), by default at the
-- origin with automatic placement.
--
-- When @mPlaced@ is provided, a floating node whose target rect matches its
-- cached 'PlacedFloat' restores its cached subtree geometry instead of
-- re-running layout, and newly placed subtrees are recorded there. Nodes are
-- visited in arena order, so a nested floating node is placed or restored
-- after its enclosing floating node.
placeFloatingNodes ::
  NodeArena ->
  Measurers ->
  Float ->
  Float ->
  (WidgetId -> IO (Maybe (Float, Float))) ->
  (WidgetId -> IO (Maybe (Float, Float))) ->
  (WidgetId -> IO (Maybe (PopupAnchor, PopupPlacement, Float))) ->
  Maybe (IORef (IntMap PlacedFloat)) ->
  IO ()
placeFloatingNodes na ms winW winH lookupPos lookupSize lookupAnchor mPlaced = do
  env <- solveEnv na ms Nothing
  let place placed idx = do
        rect <- floatingRect idx
        case IM.lookup idx placed of
          Just p | pfRect p == rect -> placed <$ restorePlaced na idx p
          _ -> do
            positionFloating env idx rect
            maybe (IM.delete idx placed) (\p -> IM.insert idx p placed) <$> recordPlaced env idx rect
  case mPlaced of
    Nothing -> forClassNodes_ na FloatingNodes $ \idx -> positionFloating env idx =<< floatingRect idx
    Just placedRef -> writeIORef placedRef =<< foldClassNodesM na FloatingNodes place =<< readIORef placedRef
  where
    floatingRect idx = do
      nt <- getNodeType na idx
      wid <- getWidgetId na idx
      Rect _ _ iw ih <- getNodeRect na idx
      case nt of
        NodeModal -> do
          let w = min iw (max 0 (winW - 2 * windowMargin))
              h = min ih (max 0 (winH - 2 * windowMargin))
          pure (Rect (max 0 ((winW - w) / 2)) (max 0 ((winH - h) / 2)) w h)
        NodeWindow -> do
          (w0, h0) <- fromMaybe (min iw winW, min ih winH) <$> lookupSize wid
          mpos <- lookupPos wid
          windowRect na winW winH idx w0 h0 $ \w -> fromMaybe (winW - w - windowMargin, windowMargin) mpos
        _ -> do
          mcfg <- lookupAnchor wid
          let (anchor, placement, offset) = fromMaybe (AnchorPoint (V2 0 0), PlacementAuto, 4) mcfg
              (x, y) = computePopupPosition winW winH windowMargin iw ih anchor placement offset
          pure (Rect x y iw ih)

-- | Lay out floating node @idx@ and its subtree at @rect@: a window at
-- exactly that rect, a modal or popup as the solve places a node offered it.
positionFloating :: SolveEnv -> NodeIdx -> Rect -> IO ()
positionFloating env@SolveEnv {seArena = na} idx rect@(Rect x y w h) = do
  nt <- getNodeType na idx
  if nt == NodeWindow
    then do
      setRect na idx x y w h
      (pad, gap, dir) <- containerFlow (seArrays env) idx
      positionChildren env 0 idx dir gap pad rect
    else positionNodeA env 0 idx rect

-- | Snapshot the placed subtree of floating node @idx@ at @rect@
-- ('snapshotPlaced'), or return 'Nothing' when its subtree is not a contiguous
-- index range or contains a drawing with a custom measure (whose height at
-- the placed width is not re-verified by the layout cache).
recordPlaced :: SolveEnv -> NodeIdx -> Rect -> IO (Maybe PlacedFloat)
recordPlaced SolveEnv {seArena = na, seMs = ms} idx rect =
  subtreeSpan na idx >>= \case
    Nothing -> pure Nothing
    Just n -> do
      let measured i = do
            nt <- getNodeType na i
            if nt == NodeDrawing then isJust <$> (msLookupMeasure ms =<< getWidgetId na i) else pure False
      anyMeasured <- foldUpTo n (\acc i -> if acc then pure True else measured (idx + i)) False
      if anyMeasured then pure Nothing else Just <$> snapshotPlaced na idx n rect

-- | Lay out window @idx@ at size @w0 h0@ ('windowRect').
placeWindowNode :: NodeArena -> Measurers -> Float -> Float -> NodeIdx -> Float -> Float -> (Float -> (Float, Float)) -> IO ()
placeWindowNode na ms winW winH idx w0 h0 originFor = do
  env <- solveEnv na ms Nothing
  positionFloating env idx =<< windowRect na winW winH idx w0 h0 originFor

-- | Window @idx@'s rect at size @w0 h0@, clamped to its min and max size and
-- the screen, with its origin, given that size, clamped on screen. The
-- caller picks @w0 h0@: 'placeFloatingNodes' passes the saved size, or else
-- the measured size capped at the screen.
windowRect :: NodeArena -> Float -> Float -> NodeIdx -> Float -> Float -> (Float -> (Float, Float)) -> IO Rect
windowRect na winW winH idx w0 h0 originFor = do
  AxisSizing _ _ minW maxW <- getWidthSizing na idx
  AxisSizing _ _ minH maxH <- getHeightSizing na idx
  let w = clamp minW (min maxW winW) w0
      h = clamp minH (min maxH winH) h0
      (x0, y0) = originFor w
  pure (Rect (clamp 0 (max 0 (winW - w)) x0) (clamp 0 (max 0 (winH - h)) y0) w h)

-- | Horizontal placement for a widget-anchored popup. Aligns the popup's left
-- edge with the anchor even when the anchor sits inside the window margin (a
-- menu bar flush to the left, say); the margin is only there to keep the popup
-- clear of the right edge.
clampPopupX :: Float -> Float -> Float -> Float -> Float
clampPopupX margin winW iw x0
  | x0 < margin && x0 + iw <= winW = max 0 x0
  | otherwise = clamp margin (winW - iw - margin) x0

-- | Popup origin from window width/height, margin, popup width/height, anchor,
-- preferred placement, and gap. All coordinates use logical pixels.
-- Flips or clamps placement to the available window space.
computePopupPosition ::
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  PopupAnchor ->
  PopupPlacement ->
  Float ->
  (Float, Float)
computePopupPosition winW winH margin iw ih anchor placement offset =
  case anchor of
    AnchorPoint (V2 px py) ->
      -- On each axis the popup goes before or after the point, as beside an
      -- anchor span of no length, or at it, flipping to the other side if it
      -- would overflow and fits there. Either way it keeps within the margins.
      let onAxis p size lim isBefore isAfter
            | isBefore = before p 0 lim size
            | isAfter = after p 0 lim size
            | p + size > lim - margin && p - size - offset >= margin = p - size - offset
            | otherwise = p
       in ( clamp margin (winW - iw - margin) (onAxis px iw winW (placement == PlacementLeft) (placement == PlacementRight))
          , clampY (onAxis py ih winH (placement == PlacementAbove) (placement == PlacementBelow))
          )
    AnchorRect (Rect rx ry rw rh) ->
      case placement of
        PlacementBelow -> (clampPopupX margin winW iw rx, clampY (after ry rh winH ih))
        PlacementAbove -> (clampPopupX margin winW iw rx, clampY (before ry rh winH ih))
        PlacementRight -> (clampPopupX margin winW iw (after rx rw winW iw), clampY ry)
        PlacementLeft -> (clampPopupX margin winW iw (before rx rw winW iw), clampY ry)
        PlacementAuto ->
          let spaceBelow = winH - margin - (ry + rh + offset)
              spaceAbove = ry - offset - margin
              y = if spaceBelow >= ih || spaceBelow >= spaceAbove
                    then ry + rh + offset
                    else ry - ih - offset
              x = clampPopupX margin winW iw rx
           in (x, clampY y)
        PlacementAtCursor ->
          (clampPopupX margin winW iw rx, clampY (ry + rh + offset))
  where
    -- Keep the popup's top edge within the window margins.
    clampY y = clamp margin (winH - ih - margin) y
    -- A popup of @size@ after (or before) the anchor span @lo@..@lo + len@ on
    -- an axis of length @lim@, taking the other side when it would overflow
    -- and the other side fits.
    after lo len lim size
      | lo + len + offset + size > lim - margin && lo - size - offset >= margin = lo - size - offset
      | otherwise = lo + len + offset
    before lo len lim size
      | lo - size - offset < margin && lo + len + offset + size <= lim - margin = lo + len + offset
      | otherwise = lo - size - offset
