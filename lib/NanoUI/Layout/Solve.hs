module NanoUI.Layout.Solve
  ( solveLayout
  ) where

import Control.Monad (forM, forM_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Font
  ( FontMetrics (..)
  , checkboxLeading
  , fmLineHeight
  , isTerminalFont
  , measureTextWrapped
  , measureTextWrappedIO
  , widgetPadding
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
  , getNodeType
  , getPadding
  , getRect
  , getText
  , getWidthSizing
  , getWrap
  , setRect
  , getNodeValue
  , setNodeValue
  )
import NanoUI.Style (AlignX (..), AlignY (..), Padding (..))
import NanoUI.WidgetText
  ( checkboxLabelText
  , selectDisplayText
  , selectChevronReserve
  , selectParseOptions
  , sliderLabelText
  , sliderParseRange
  , sliderValueText
  )
solveLayout :: NodeArena -> FontMetrics -> (Text -> IO (Float, Float)) -> Float -> Float -> IO ()
solveLayout na fm measure rootW rootH = do
  count <- arenaCount na
  whenPositive count $ do
    forM_ (reverse [0 .. count - 1]) $ \idx ->
      measureNode na fm measure idx
    positionNode na 0 0 0 rootW rootH

whenPositive :: Int -> IO () -> IO ()
whenPositive n act = if n > 0 then act else pure ()

measureNode :: NodeArena -> FontMetrics -> (Text -> IO (Float, Float)) -> NodeIdx -> IO ()
measureNode na fm measure idx = do
  nt <- getNodeType na idx
  case nt of
    NodeText -> measureTextNode na fm measure idx
    NodeSpacer -> measureSpacer na idx
    NodeSeparator -> measureSeparator na idx
    NodeContainer -> measureContainer na idx
    NodeScrollContainer -> measureScrollContainer na idx
    _ -> measureWidget na fm measure idx

measureTextNode :: NodeArena -> FontMetrics -> (Text -> IO (Float, Float)) -> NodeIdx -> IO ()
measureTextNode na fm measure idx = do
  txt <- getText na idx
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (tw, th) <-
    if maxW < 1e8
      then do
        let wrapW = max 0 maxW
        if isTerminalFont fm
          then pure (measureTextWrapped fm txt wrapW)
          else measureTextWrappedIO (\t -> fmap fst (measure t)) fm txt wrapW
      else measure txt
  setRect na idx 0 0 (clamp tw minW maxW) (clamp (max (fmLineHeight fm) th) minH maxH)

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
  let (padX, padY) = widgetPadding fm
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
        pure (max lw vw, lh, 0, fmLineHeight fm * 0.35)
      NodeCheckbox -> do
        let body =
              if T.null txt
                then " "
                else checkboxLabelText txt
        (mw, mh) <- measure body
        pure (mw, mh, checkboxLeading fm, 0)
      NodeSelect -> do
        let (lbl, opts) = selectParseOptions txt
            choices = if null opts then [""] else opts
        dims <- mapM (measure . selectDisplayText lbl) choices
        let (mw, mh) =
              case dims of
                [] -> (0, 0)
                ds -> (maximum (map fst ds), maximum (map snd ds))
        pure (mw, mh, selectChevronReserve, 0)
      _ -> do
        let body =
              if T.null txt
                then " "
                else txt
        (mw, mh) <- measure body
        pure (mw, mh, 0, 0)
  setRect na idx 0 0 (clamp (tw + padX + extraW) minW maxW) (clamp (th + padY + extraH) minH maxH)

measureContainer :: NodeArena -> NodeIdx -> IO ()
measureContainer na idx = do
  pad <- getPadding na idx
  gap <- getGap na idx
  dir <- getDirection na idx
  wrap <- getWrap na idx
  (minW, minH, maxW, maxH) <- getMinMax na idx
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  childDims <- collectChildDims na idx
  let innerMaxW =
        case wTag of
          SizingFixed -> max 0 (wVal - padL pad - padR pad)
          _ -> max 0 (maxW - padL pad - padR pad)
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
  setNodeValue na idx (case dir of DirColumn -> fullH; DirRow -> fullW)
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
          (_, _, w, h) <- getRect na ci
          ns <- getNextSibling na ci
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

positionNode :: NodeArena -> NodeIdx -> Float -> Float -> Float -> Float -> IO ()
positionNode na idx x y availW availH = do
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
    NodeContainer -> positionChildren na idx dir gap pad x y w h
    NodeScrollContainer -> positionScrollChildren na idx dir gap pad x y w h
    _ -> pure ()

positionScrollChildren :: NodeArena -> NodeIdx -> DirTag -> Float -> Padding -> Float -> Float -> Float -> Float -> IO ()
positionScrollChildren na idx dir gap pad px py pw ph = do
  contentSize <- getNodeValue na idx
  let cx = px + padL pad
      cy = py + padT pad
      cw = pw - padL pad - padR pad
      innerH = ph - padT pad - padB pad
  children <- collectChildren na idx
  case dir of
    DirRow -> positionRow na children gap cx cy contentSize innerH
    DirColumn -> positionColumn na children gap cx cy cw contentSize

resolveSize :: SizingTag -> Float -> Float -> Float -> Float -> Float -> Float
resolveSize SizingFixed v _ _ _ _ = v
resolveSize SizingFit _ intrinsic avail minS maxS = clamp (min intrinsic avail) minS maxS
resolveSize SizingShrink _ intrinsic avail minS maxS = clamp (min intrinsic avail) minS maxS
resolveSize SizingGrow _ _ avail _ maxS = min avail maxS
resolveSize SizingPercent p _ avail _ maxS = min (avail * p / 100) maxS

positionChildren :: NodeArena -> NodeIdx -> DirTag -> Float -> Padding -> Float -> Float -> Float -> Float -> IO ()
positionChildren na idx dir gap pad px py pw ph = do
  wrap <- getWrap na idx
  let cx = px + padL pad
      cy = py + padT pad
      cw = pw - padL pad - padR pad
      ch = ph - padT pad - padB pad
  children <- collectChildren na idx
  case dir of
    DirRow
      | wrap -> positionRowWrap na children gap cx cy cw ch
      | otherwise -> positionRow na children gap cx cy cw ch
    DirColumn -> positionColumn na children gap cx cy cw ch

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
          ns <- getNextSibling na ci
          go ns (ci : acc)

positionRow :: NodeArena -> [NodeIdx] -> Float -> Float -> Float -> Float -> Float -> IO ()
positionRow na children gap cx cy cw ch = do
  childInfos <- loadChildInfos na children
  sizes <- distributeMainAxis na childInfos cw gap True
  ox <- newIORef cx
  forM_ (zip childInfos sizes) $ \((ci, _, _), (fw, _)) -> do
    (_, _, _, ih) <- getRect na ci
    curX <- readIORef ox
    ay <- getAlignY na ci
    let fy = alignY ay cy ch ih
    positionNode na ci curX fy fw ch
    writeIORef ox (curX + fw + gap)

positionRowWrap :: NodeArena -> [NodeIdx] -> Float -> Float -> Float -> Float -> Float -> IO ()
positionRowWrap na children gap cx cy cw _ch = do
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
        positionNode na ci curX fy fw rowH
        writeIORef ox (curX + fw + gap)
      go (oy + rowH + gap) rest

positionColumn :: NodeArena -> [NodeIdx] -> Float -> Float -> Float -> Float -> Float -> IO ()
positionColumn na children gap cx cy cw ch = do
  childInfos <- loadChildInfos na children
  sizes <- distributeMainAxis na childInfos ch gap False
  oy <- newIORef cy
  forM_ (zip childInfos sizes) $ \((ci, _, _), (_, fh)) -> do
    (_, _, iw, _) <- getRect na ci
    curY <- readIORef oy
    ax <- getAlignX na ci
    let fx = alignX ax cx cw iw
    positionNode na ci fx curY cw fh
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
    SizingFit -> pure 1
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

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c
