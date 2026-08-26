module NanoUI.Layout.Solve
  (   solveLayout
  , placeModals
  , placeWindows
  ) where

import Control.Monad (forM, forM_, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Font
  ( FontMetrics (..)
  , checkboxBoxSize
  , checkboxLeading
  , fmLineHeight
  , isTerminalFont
  , measureTextWrapped
  , measureTextWrappedIO
  , labelContentInset
  , scrollOverflowGutter
  , widgetPadding
  , buttonPadding
  , layoutLineHeight
  )
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeArena
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , getAlignX
  , getAlignY
  , getDirection
  , getFirstChild
  , getGap
  , getHeightSizing
  , getMinMax
  , getNextSibling
  ,   getNodeType
  , getPadding
  , getRect
  , getText
  , getWidgetId
  , getWidthSizing
  , getWrap
  , isFloatingNode
  , setRect
  , getNodeValue
  , setNodeValue
  )
import NanoUI.Id (WidgetId)
import NanoUI.Style (AlignX (..), AlignY (..), Padding (..))
import NanoUI.WidgetText
  ( checkboxLabelText
  , selectDisplayText
  , selectChevronReserve
  , selectParseOptions
  , sliderLabelText
  , sliderParseRange
  , textInputFieldHeight
  , textInputLabelGap
  , textInputMinWidth
  , textInputPlaceholder
  , sliderValueText
  )
solveLayout :: NodeArena -> FontMetrics -> (Text -> IO (Float, Float)) -> Float -> Float -> IO ()
solveLayout na fm measure rootW rootH = do
  count <- arenaCount na
  whenPositive count $ do
    -- Wrap rows and wrapping labels need a known width. First pass sizes Grow
    -- rows unconstrained, position assigns widths, second pass remasures wrap
    -- height, then we position again so siblings sit below the wrapped content.
    measurePass na fm measure False
    positionNode na fm 0 0 0 rootW rootH
    needsRemeasure <- anyNeedsRemeasure na count
    when needsRemeasure $ do
      measurePass na fm measure True
      positionNode na fm 0 0 0 rootW rootH

measurePass ::
  NodeArena ->
  FontMetrics ->
  (Text -> IO (Float, Float)) ->
  Bool ->
  IO ()
measurePass na fm measure useAssignedWidth = do
  count <- arenaCount na
  forM_ (reverse [0 .. count - 1]) $ \idx ->
    measureNode na fm measure useAssignedWidth idx

whenPositive :: Int -> IO () -> IO ()
whenPositive n act = if n > 0 then act else pure ()

-- Flex wrap and body text both need a second measure after widths are known.
anyNeedsRemeasure :: NodeArena -> Int -> IO Bool
anyNeedsRemeasure na count = go 0
  where
    go idx
      | idx >= count = pure False
      | otherwise = do
          wrapped <- getWrap na idx
          nt <- getNodeType na idx
          if wrapped || nt == NodeText
            then pure True
            else go (idx + 1)

measureNode ::
  NodeArena ->
  FontMetrics ->
  (Text -> IO (Float, Float)) ->
  Bool ->
  NodeIdx ->
  IO ()
measureNode na fm measure useAssignedWidth idx = do
  nt <- getNodeType na idx
  case nt of
    NodeText -> measureTextNode na fm measure useAssignedWidth idx
    NodeSpacer -> measureSpacer na idx
    NodeSeparator -> measureSeparator na idx
    NodeContainer -> measureContainer na useAssignedWidth idx
    NodePanel -> measureContainer na useAssignedWidth idx
    NodeScrollContainer -> measureScrollContainer na idx
    NodeModal -> measureScrollContainer na idx
    NodeWindow -> measureScrollContainer na idx
    NodeImage -> measureImage na idx
    _ -> measureWidget na fm measure idx

measureTextNode ::
  NodeArena ->
  FontMetrics ->
  (Text -> IO (Float, Float)) ->
  Bool ->
  NodeIdx ->
  IO ()
measureTextNode na fm measure useAssignedWidth idx = do
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, _) <- getWidthSizing na idx
  (_, _, assignedW, _) <- getRect na idx
  let needsAssignedWrap = useAssignedWidth && wTag == SizingGrow && assignedW > 0
  if useAssignedWidth && maxW >= 1e8 && not needsAssignedWrap
    then pure ()
    else do
      txt <- getText na idx
      (tw0, th0) <- measure txt
      let (ix, _) = labelContentInset fm
          wrapCap
            | maxW < 1e8 = max 0 maxW
            | needsAssignedWrap = assignedW
            | otherwise = maxW
          wrapW = max 0 (wrapCap - ix)
      (tw, th) <-
        if wrapCap < 1e8 && wrapCap + 0.5 < tw0
          then
            if isTerminalFont fm
              then pure (measureTextWrapped fm txt wrapW)
              else measureTextWrappedIO (\t -> fmap fst (measure t)) fm txt wrapW
          else pure (tw0, th0)
      setRect na idx 0 0 (clamp tw minW maxW) (clamp (max (layoutLineHeight fm) th) minH maxH)

measureImage :: NodeArena -> NodeIdx -> IO ()
measureImage na idx = do
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  let w =
        case wTag of
          SizingFixed -> wVal
          _ -> if minW > 0 then minW else 32
      h =
        case hTag of
          SizingFixed -> hVal
          _ -> if minH > 0 then minH else 32
  setRect na idx 0 0 (clamp w minW maxW) (clamp h minH maxH)

measureSpacer :: NodeArena -> NodeIdx -> IO ()
measureSpacer na idx = do
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  let w = case wTag of SizingFixed -> wVal; _ -> 8
      h = case hTag of SizingFixed -> hVal; _ -> 8
  setRect na idx 0 0 w h

measureSeparator :: NodeArena -> NodeIdx -> IO ()
measureSeparator na idx = do
  dir <- getDirection na idx
  case dir of
    DirRow -> setRect na idx 0 0 1 20
    DirColumn -> setRect na idx 0 0 20 1

measureWidget :: NodeArena -> FontMetrics -> (Text -> IO (Float, Float)) -> NodeIdx -> IO ()
measureWidget na fm measure idx = do
  nt <- getNodeType na idx
  txt <- getText na idx
  (minW, minH, maxW, maxH) <- getMinMax na idx
  let (padX, padY) =
        case nt of
          NodeButton -> buttonPadding fm
          NodeSelect -> buttonPadding fm
          _ -> widgetPadding fm
  (tw, th, extraW, extraH) <-
    case nt of
      NodeSlider -> do
        let lbl =
              if T.null txt
                then " "
                else sliderLabelText txt
            (_, minV, maxV) = sliderParseRange txt
        (lw, lh) <- measure lbl
        (vwMin, _) <- measure (sliderValueText minV)
        (vwMax, _) <- measure (sliderValueText maxV)
        let vw = max vwMin vwMax
        let trackExtra =
              if isTerminalFont fm
                then fmLineHeight fm * 0.35
                else 22
        pure (max lw vw, lh, 0, trackExtra)
      NodeCheckbox -> do
        let body =
              if T.null txt
                then " "
                else checkboxLabelText txt
        (mw, mh) <- measure body
        let box = checkboxBoxSize fm
        pure (mw, max mh box, checkboxLeading fm, 0)
      NodeSelect -> do
        let (lbl, opts) = selectParseOptions txt
            choices = if null opts then [""] else opts
        dims <- mapM (measure . selectDisplayText lbl) choices
        let (mw, mh) =
              case dims of
                [] -> (0, 0)
                ds -> (maximum (map fst ds), maximum (map snd ds))
        pure (mw, mh, selectChevronReserve, 0)
      NodeTextInput -> do
        let lbl = if T.null txt then " " else txt
        (lw, lh) <- measure lbl
        if isTerminalFont fm
          then do
            (vw, vh) <- measure (textInputPlaceholder lbl)
            pure (max lw vw, max lh vh, 0, 0)
          else do
            (pw, _) <- measure (textInputPlaceholder lbl)
            let fieldH = textInputFieldHeight fm
                gap = textInputLabelGap fm
                contentW = max textInputMinWidth (max lw pw)
            pure (contentW, lh + gap + fieldH, 0, lh + gap)
      _ -> do
        let body =
              if T.null txt
                then " "
                else txt
        (mw, mh) <- measure body
        pure (mw, mh, 0, 0)
  setRect na idx 0 0 (clamp (tw + padX + extraW) minW maxW) (clamp (th + padY + extraH) minH maxH)

measureContainer :: NodeArena -> Bool -> NodeIdx -> IO ()
measureContainer na useAssignedWidth idx = do
  pad <- getPadding na idx
  gap <- getGap na idx
  dir <- getDirection na idx
  wrap <- getWrap na idx
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  (_, _, assignedW, _) <- getRect na idx
  childDims <- collectChildDims na idx
  let padX = padL pad + padR pad
      innerMaxW =
        case wTag of
          SizingFixed -> max 0 (wVal - padX)
          _
            | useAssignedWidth && assignedW > 0 ->
                max 0 (assignedW - padX)
            | otherwise -> max 0 (maxW - padX)
      (contentW, contentH) =
        if wrap && dir == DirRow && innerMaxW > 0
          then foldWrappedRow childDims innerMaxW gap
          else foldChildren dir gap childDims
      w =
        case wTag of
          SizingFixed -> clamp wVal minW maxW
          _ -> clamp (contentW + padL pad + padR pad) minW maxW
      h =
        case hTag of
          SizingFixed -> clamp hVal minH maxH
          _ -> clamp (contentH + padT pad + padB pad) minH maxH
  setRect na idx 0 0 w h

measureScrollContainer :: NodeArena -> NodeIdx -> IO ()
measureScrollContainer na idx = do
  pad <- getPadding na idx
  gap <- getGap na idx
  dir <- getDirection na idx
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  childDims <- collectChildDims na idx
  let (contentW, contentH) = foldChildren dir gap childDims
      fullW = contentW + padL pad + padR pad
      fullH = contentH + padT pad + padB pad
  -- Overflow compares against the inner viewport, so store content only.
  -- Including pad made every padded Fit modal/list think it could scroll.
  setNodeValue na idx (case dir of DirColumn -> contentH; DirRow -> contentW)
  let viewportW =
        case wTag of
          SizingFixed -> wVal
          _ -> fullW
      viewportH =
        case hTag of
          SizingFixed -> hVal
          _ -> fullH
  setRect na idx 0 0 (clamp viewportW minW maxW) (clamp viewportH minH maxH)

collectChildDims :: NodeArena -> NodeIdx -> IO [(Float, Float)]
collectChildDims na idx = do
  fc <- getFirstChild na idx
  go fc []
  where
    go ci acc =
      if ci < 0
        then pure acc
        else do
          nt <- getNodeType na ci
          ns <- getNextSibling na ci
          if isFloatingNode nt
            then go ns acc
            else do
              (_, _, w, h) <- getRect na ci
              go ns ((w, h) : acc)

foldChildren :: DirTag -> Float -> [(Float, Float)] -> (Float, Float)
foldChildren _ _ [] = (0, 0)
foldChildren DirRow gap dims =
  let ws = map fst dims
      hs = map snd dims
      totalW = sum ws + gap * fromIntegral (max 0 (length ws - 1))
      maxH = if null hs then 0 else maximum hs
   in (totalW, maxH)
foldChildren DirColumn gap dims =
  let ws = map fst dims
      hs = map snd dims
      maxW = if null ws then 0 else maximum ws
      totalH = sum hs + gap * fromIntegral (max 0 (length hs - 1))
   in (maxW, totalH)

foldWrappedRow :: [(Float, Float)] -> Float -> Float -> (Float, Float)
foldWrappedRow dims avail gap =
  let rows = packDimLines dims avail gap
      heights = map lineDimCross rows
      widths = map (lineDimMain gap) rows
   in ( if null widths then 0 else maximum widths
      , if null heights then 0 else sum heights + gap * fromIntegral (max 0 (length heights - 1))
      )

packDimLines :: [(Float, Float)] -> Float -> Float -> [[(Float, Float)]]
packDimLines dims avail gap = reverse (go 0 [] 0 [])
  where
    n = length dims
    go i curLine curW acc
      | i >= n = finalize curLine acc
      | otherwise =
          let item = dims !! i
              (w, _) = item
              need = if null curLine then w else w + gap
           in if null curLine || curW + need <= avail + 0.001
                then go (i + 1) (item : curLine) (curW + need) acc
                -- Oversized children get a row alone; parent may clip or scroll.
                else go i [] 0 (curLine : acc)
    finalize [] acc = acc
    finalize cur acc = cur : acc

lineDimMain :: Float -> [(Float, Float)] -> Float
lineDimMain gap line =
  let ws = map fst line
   in sum ws + gap * fromIntegral (max 0 (length ws - 1))

lineDimCross :: [(Float, Float)] -> Float
lineDimCross line =
  if null line then 0 else maximum (map snd line)

packRowLines :: [(NodeIdx, Float, Float)] -> Float -> Float -> [[(NodeIdx, Float, Float)]]
packRowLines dims avail gap = reverse (go 0 [] 0 [])
  where
    n = length dims
    go i curLine curW acc
      | i >= n = finalize curLine acc
      | otherwise =
          let item = dims !! i
              (w, _) = (snd3 item, thd3 item)
              need = if null curLine then w else w + gap
           in if null curLine || curW + need <= avail + 0.001
                then go (i + 1) (item : curLine) (curW + need) acc
                -- Oversized children get a row alone; parent may clip or scroll.
                else go i [] 0 (curLine : acc)
    finalize [] acc = acc
    finalize cur acc = cur : acc

lineCrossSize :: [(NodeIdx, Float, Float)] -> Float
lineCrossSize line =
  if null line then 0 else maximum (map thd3 line)

positionNode :: NodeArena -> FontMetrics -> NodeIdx -> Float -> Float -> Float -> Float -> IO ()
positionNode na fm idx x y availW availH = do
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  (_, _, intrinsicW, intrinsicH) <- getRect na idx
  let w = clamp (resolveSize wTag wVal intrinsicW availW minW maxW) minW maxW
      h = clamp (resolveSize hTag hVal intrinsicH availH minH maxH) minH maxH
  setRect na idx x y w h
  pad <- getPadding na idx
  gap <- getGap na idx
  dir <- getDirection na idx
  nt <- getNodeType na idx
  case nt of
    NodeContainer -> positionChildren na fm idx dir gap pad x y w h
    NodePanel -> positionChildren na fm idx dir gap pad x y w h
    NodeScrollContainer -> positionScrollChildren na fm idx dir gap pad x y w h
    NodeModal -> positionScrollChildren na fm idx dir gap pad x y w h
    NodeWindow -> positionScrollChildren na fm idx dir gap pad x y w h
    _ -> pure ()

positionScrollChildren ::
  NodeArena ->
  FontMetrics ->
  NodeIdx ->
  DirTag ->
  Float ->
  Padding ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
positionScrollChildren na fm idx dir gap pad px py pw ph = do
  contentSize <- getNodeValue na idx
  let cx = px + padL pad
      cy = py + padT pad
      innerW = pw - padL pad - padR pad
      innerH = ph - padT pad - padB pad
      gutterCol = scrollOverflowGutter fm contentSize innerH
      gutterRow = scrollOverflowGutter fm contentSize innerW
  children <- collectChildren na idx
  case dir of
    DirRow -> positionRow na fm children gap cx cy contentSize (innerH - gutterRow)
    DirColumn -> positionColumn na fm children gap cx cy (innerW - gutterCol) contentSize

resolveSize :: SizingTag -> Float -> Float -> Float -> Float -> Float -> Float
resolveSize SizingFixed v _ _ _ _ = v
resolveSize SizingFit _ intrinsic avail minS maxS = clamp (min intrinsic avail) minS maxS
resolveSize SizingShrink _ intrinsic avail minS maxS = clamp (min intrinsic avail) minS maxS
resolveSize SizingGrow _ _ avail _ maxS = min avail maxS
resolveSize SizingPercent p _ avail _ maxS = min (avail * p / 100) maxS

positionChildren ::
  NodeArena ->
  FontMetrics ->
  NodeIdx ->
  DirTag ->
  Float ->
  Padding ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
positionChildren na fm idx dir gap pad px py pw ph = do
  wrap <- getWrap na idx
  let cx = px + padL pad
      cy = py + padT pad
      cw = pw - padL pad - padR pad
      ch = ph - padT pad - padB pad
  children <- collectChildren na idx
  case dir of
    DirRow
      | wrap -> positionRowWrap na fm children gap cx cy cw ch
      | otherwise -> positionRow na fm children gap cx cy cw ch
    DirColumn -> positionColumn na fm children gap cx cy cw ch

-- Children are linked newest-first, so prepending while walking restores
-- declaration order.
collectChildren :: NodeArena -> NodeIdx -> IO [NodeIdx]
collectChildren na idx = do
  fc <- getFirstChild na idx
  go fc []
  where
    go ci acc =
      if ci < 0
        then pure acc
        else do
          nt <- getNodeType na ci
          ns <- getNextSibling na ci
          if isFloatingNode nt
            then go ns acc
            else go ns (ci : acc)

positionRow :: NodeArena -> FontMetrics -> [NodeIdx] -> Float -> Float -> Float -> Float -> Float -> IO ()
positionRow na fm children gap cx cy cw ch = do
  childInfos <- loadChildInfos na children
  sizes <- distributeMainAxis na childInfos cw gap True
  ox <- newIORef cx
  forM_ (zip childInfos sizes) $ \((ci, _, _), (fw, _)) -> do
    (_, _, _, ih) <- getRect na ci
    curX <- readIORef ox
    ay <- getAlignY na ci
    let fy = alignY ay cy ch ih
    positionNode na fm ci curX fy fw ch
    writeIORef ox (curX + fw + gap)

positionRowWrap :: NodeArena -> FontMetrics -> [NodeIdx] -> Float -> Float -> Float -> Float -> Float -> IO ()
positionRowWrap na fm children gap cx cy cw _ch = do
  childInfos <- loadChildInfos na children
  let lineGroups = packRowLines childInfos cw gap
  go cy lineGroups
  where
    go _ [] = pure ()
    go oy (rowItems : rest) = do
      let rowH = lineCrossSize rowItems
      sizes <- distributeMainAxis na rowItems cw gap True
      ox <- newIORef cx
      forM_ (zip rowItems sizes) $ \((ci, _, _), (fw, _)) -> do
        (_, _, _, ih) <- getRect na ci
        curX <- readIORef ox
        ay <- getAlignY na ci
        let fy = alignY ay oy rowH ih
        positionNode na fm ci curX fy fw rowH
        writeIORef ox (curX + fw + gap)
      go (oy + rowH + gap) rest

positionColumn :: NodeArena -> FontMetrics -> [NodeIdx] -> Float -> Float -> Float -> Float -> Float -> IO ()
positionColumn na fm children gap cx cy cw ch = do
  childInfos <- loadChildInfos na children
  sizes <- distributeMainAxis na childInfos ch gap False
  oy <- newIORef cy
  forM_ (zip childInfos sizes) $ \((ci, _, _), (_, fh)) -> do
    (_, _, iw, _) <- getRect na ci
    curY <- readIORef oy
    ax <- getAlignX na ci
    let fx = alignX ax cx cw iw
    positionNode na fm ci fx curY cw fh
    writeIORef oy (curY + fh + gap)

loadChildInfos :: NodeArena -> [NodeIdx] -> IO [(NodeIdx, Float, Float)]
loadChildInfos na children =
  forM children $ \ci -> do
    (_, _, w, h) <- getRect na ci
    pure (ci, w, h)

-- Distribute grow/shrink slack on the container main axis (row = width).
distributeMainAxis ::
  NodeArena ->
  [(NodeIdx, Float, Float)] ->
  Float ->
  Float ->
  Bool ->
  IO [(Float, Float)]
distributeMainAxis na childInfos avail gap horizontal = do
  let n = length childInfos
      totalIntrinsic =
        sum (map mainSize childInfos) + gap * fromIntegral (max 0 n - 1)
      slack = avail - totalIntrinsic
  if slack > 0.001
    then growSizes childInfos slack
    else
      if slack < -0.001
        then shrinkSizes childInfos (negate slack)
        else pure (map (\(_, w, h) -> (w, h)) childInfos)
  where
    mainSize (_, w, h) = if horizontal then w else h

    growSizes infos slack =
      do
        growTotal <- sumFactors na infos horizontal getGrowFactor
        if growTotal <= 0
          then pure (map (\(_, w, h) -> (w, h)) infos)
          else
            forM infos $ \(ci, iw, ih) -> do
              gf <- getGrowFactor na ci horizontal
              let extra = slack * gf / growTotal
              pure (if horizontal then (iw + extra, ih) else (iw, ih + extra))

    shrinkSizes infos overflow =
      do
        shrinkTotal <- sumFactors na infos horizontal getShrinkFactor
        if shrinkTotal <= 0
          then pure (map (\(_, w, h) -> (w, h)) infos)
          else
            forM infos $ \(ci, iw, ih) -> do
              (minW, minH, _, _) <- getMinMax na ci
              sf <- getShrinkFactor na ci horizontal
              let main = if horizontal then iw else ih
                  minMain = if horizontal then minW else minH
                  delta = overflow * sf / shrinkTotal
                  shrunk = max minMain (main - delta)
              pure (if horizontal then (shrunk, ih) else (iw, shrunk))

sumFactors ::
  NodeArena ->
  [(NodeIdx, Float, Float)] ->
  Bool ->
  (NodeArena -> NodeIdx -> Bool -> IO Float) ->
  IO Float
sumFactors na childInfos horizontal k = do
  fs <- forM childInfos $ \(ci, _, _) -> k na ci horizontal
  pure (sum fs)

getGrowFactor :: NodeArena -> NodeIdx -> Bool -> IO Float
getGrowFactor na idx horizontal = do
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  case if horizontal then (wTag, wVal) else (hTag, hVal) of
    (SizingGrow, g) -> pure g
    _ -> pure 0

getShrinkFactor :: NodeArena -> NodeIdx -> Bool -> IO Float
getShrinkFactor na idx horizontal = do
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  let (tag, val) = if horizontal then (wTag, wVal) else (hTag, hVal)
  case tag of
    SizingShrink -> pure val
    -- Grow also gives space back when the window is smaller than content.
    SizingGrow -> pure (if val > 0 then val else 1)
    -- Fit stays content-sized. A pinned header must not squash when a Grow
    -- sibling (page scroll) is taller than the window.
    SizingFit -> pure 0
    _ -> pure 0

alignX :: AlignX -> Float -> Float -> Float -> Float
alignX AlignStart cx _ _ = cx
alignX AlignCenter cx cw iw = cx + (cw - iw) / 2
alignX AlignEnd cx cw iw = cx + cw - iw

alignY :: AlignY -> Float -> Float -> Float -> Float
alignY AlignTop cy _ _ = cy
alignY AlignMiddle cy ch ih = cy + (ch - ih) / 2
alignY AlignBottom cy ch ih = cy + ch - ih

clamp :: Float -> Float -> Float -> Float
clamp v lo hi = max lo (min hi v)

placeModals :: NodeArena -> FontMetrics -> Float -> Float -> IO ()
placeModals na fm winW winH = do
  count <- arenaCount na
  forM_ [0 .. count - 1] $ \idx -> do
    nt <- getNodeType na idx
    when (nt == NodeModal) $ do
      (_, _, iw, ih) <- getRect na idx
      let w = min iw winW
          h = min ih winH
          x = max 0 ((winW - w) / 2)
          y = max 0 ((winH - h) / 2)
      positionNode na fm idx x y w h

windowMargin :: Float
windowMargin = 24

placeWindows ::
  NodeArena ->
  FontMetrics ->
  Float ->
  Float ->
  (WidgetId -> IO (Maybe (Float, Float))) ->
  IO ()
placeWindows na fm winW winH lookupPos = do
  count <- arenaCount na
  forM_ [0 .. count - 1] $ \idx -> do
    nt <- getNodeType na idx
    when (nt == NodeWindow) $ do
      wid <- getWidgetId na idx
      (_, _, iw, ih) <- getRect na idx
      let w = min iw winW
          h = min ih winH
      mpos <- lookupPos wid
      let (x0, y0) = maybe (max 0 (winW - w - windowMargin), windowMargin) id mpos
          x = clamp x0 0 (max 0 (winW - w))
          y = clamp y0 0 (max 0 (winH - h))
      positionNode na fm idx x y w h

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c
