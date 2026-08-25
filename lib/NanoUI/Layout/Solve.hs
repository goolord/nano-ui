module NanoUI.Layout.Solve
  ( solveLayout
  ) where

import Control.Monad (forM, forM_)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import NanoUI.Font (FontMetrics (..), measureText, widgetPadding)
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
  , setRect
  )
import NanoUI.Style (AlignX (..), AlignY (..), Padding (..))

solveLayout :: NodeArena -> FontMetrics -> Float -> Float -> IO ()
solveLayout na fm rootW rootH = do
  count <- arenaCount na
  whenPositive count $ do
    forM_ (reverse [0 .. count - 1]) $ \idx ->
      measureNode na fm idx
    positionNode na 0 0 0 rootW rootH

whenPositive :: Int -> IO () -> IO ()
whenPositive n act = if n > 0 then act else pure ()

measureNode :: NodeArena -> FontMetrics -> NodeIdx -> IO ()
measureNode na fm idx = do
  nt <- getNodeType na idx
  case nt of
    NodeText -> measureTextNode na fm idx
    NodeSpacer -> measureSpacer na idx
    NodeSeparator -> measureSeparator na idx
    NodeContainer -> measureContainer na fm idx
    _ -> measureWidget na fm idx

measureTextNode :: NodeArena -> FontMetrics -> NodeIdx -> IO ()
measureTextNode na fm idx = do
  txt <- getText na idx
  (minW, minH, maxW, maxH) <- getMinMax na idx
  let (tw, th) = measureText fm txt
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

measureWidget :: NodeArena -> FontMetrics -> NodeIdx -> IO ()
measureWidget na fm idx = do
  txt <- getText na idx
  (tw, th) <-
    if T.null txt
      then pure (40, fmLineHeight fm)
      else pure (measureText fm txt)
  (minW, minH, maxW, maxH) <- getMinMax na idx
  let (padX, padY) = widgetPadding fm
  setRect na idx 0 0 (clamp (tw + padX) minW maxW) (clamp (th + padY) minH maxH)

measureContainer :: NodeArena -> FontMetrics -> NodeIdx -> IO ()
measureContainer na _ idx = do
  pad <- getPadding na idx
  gap <- getGap na idx
  dir <- getDirection na idx
  (minW, minH, maxW, maxH) <- getMinMax na idx
  childDims <- collectChildDims na idx
  let (contentW, contentH) = foldChildren dir gap childDims
      w = clamp (contentW + padL pad + padR pad) minW maxW
      h = clamp (contentH + padT pad + padB pad) minH maxH
  setRect na idx 0 0 w h

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
  if nt == NodeContainer
    then positionChildren na idx dir gap pad x y w h
    else pure ()

resolveSize :: SizingTag -> Float -> Float -> Float -> Float -> Float -> Float
resolveSize SizingFixed v _ _ _ _ = v
resolveSize SizingFit _ intrinsic _ minS maxS = clamp intrinsic minS maxS
resolveSize SizingGrow _ _ avail _ maxS = min avail maxS
resolveSize SizingPercent p _ avail _ maxS = min (avail * p / 100) maxS

positionChildren :: NodeArena -> NodeIdx -> DirTag -> Float -> Padding -> Float -> Float -> Float -> Float -> IO ()
positionChildren na idx dir gap pad px py pw ph = do
  let cx = px + padL pad
      cy = py + padT pad
      cw = pw - padL pad - padR pad
      ch = ph - padT pad - padB pad
  children <- collectChildren na idx
  case dir of
    DirRow -> positionRow na children gap cx cy cw ch
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
  childInfos <- forM children $ \ci -> do
    (_, _, w, h) <- getRect na ci
    pure (ci, w, h)
  let totalIntrinsic = sum (map snd3 childInfos) + gap * fromIntegral (max 0 (length childInfos - 1))
      slack = cw - totalIntrinsic
  growTotal <- sumGrow na children
  ox <- newIORef cx
  forM_ childInfos $ \(ci, iw, ih) -> do
    curX <- readIORef ox
    growW <-
      if growTotal > 0
        then do
          gf <- getGrowFactor na ci
          pure (slack * gf / growTotal)
        else pure 0
    let fw = iw + growW
    ay <- getAlignY na ci
    let fy = alignY ay cy ch ih
    positionNode na ci curX fy fw ch
    writeIORef ox (curX + fw + gap)

positionColumn :: NodeArena -> [NodeIdx] -> Float -> Float -> Float -> Float -> Float -> IO ()
positionColumn na children gap cx cy cw ch = do
  childInfos <- forM children $ \ci -> do
    (_, _, w, h) <- getRect na ci
    pure (ci, w, h)
  let totalIntrinsic = sum (map thd3 childInfos) + gap * fromIntegral (max 0 (length childInfos - 1))
      slack = ch - totalIntrinsic
  growTotal <- sumGrow na children
  oy <- newIORef cy
  forM_ childInfos $ \(ci, iw, ih) -> do
    curY <- readIORef oy
    growH <-
      if growTotal > 0
        then do
          gf <- getGrowFactor na ci
          pure (slack * gf / growTotal)
        else pure 0
    let fh = ih + growH
    ax <- getAlignX na ci
    let fx = alignX ax cx cw iw
    positionNode na ci fx curY cw fh
    writeIORef oy (curY + fh + gap)

sumGrow :: NodeArena -> [NodeIdx] -> IO Float
sumGrow na children = do
  factors <- forM children (getGrowFactor na)
  pure (sum factors)

getGrowFactor :: NodeArena -> NodeIdx -> IO Float
getGrowFactor na idx = do
  (wTag, wVal) <- getWidthSizing na idx
  (hTag, hVal) <- getHeightSizing na idx
  case (wTag, hTag) of
    (SizingGrow, _) -> pure wVal
    (_, SizingGrow) -> pure hVal
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
