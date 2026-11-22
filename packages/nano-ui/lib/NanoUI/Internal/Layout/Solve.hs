-- | The layout solver: measures and places the node arena's flow tree, then
-- positions modals, windows and popups.
module NanoUI.Internal.Layout.Solve
  ( solveLayout
  , runCustomMeasure
  , measuredHeightAt
  , Measurers (..)
  , placeFloatingNodes
  , computePopupPosition
  , placeWindowNode
  , scrollBarSlotOf
  , windowBodyScroller
  , findAncestorMaxW
  , textWrapCap
  ) where

import Control.Monad (unless, when)
import Data.IORef (IORef, newIORef, readIORef)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Primitive.PrimArray (newPrimArray, readPrimArray, setPrimArray, writePrimArray)
import Data.Word (Word8)
import NanoUI.Internal.Font (FontMetrics (..))
import NanoUI.Internal.Frame.Scroll.Geometry (isScrollStyle2D)
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Layout.Solve.Measure
import NanoUI.Internal.Layout.Solve.Position hiding (placeFloatingNodes)
import NanoUI.Internal.Layout.Solve.Position qualified as Position
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Types (PopupAnchor, PopupPlacement, Rect (..), forUpTo_, gridSpan, onGrid)

-- | Measure nodes and place the page within the supplied logical width/height.
-- The view must have finished adding nodes, and 'computeSubtreeHashes' must
-- have run for this frame. Place floating nodes separately with
-- 'placeFloatingNodes', then apply scrolling.
-- When a layout cache captured under the same font metrics is supplied, a
-- node whose inputs, ancestors' inputs and subtree are unchanged since that
-- solve, and whose children all came out their captured sizes, takes its
-- captured measured size instead of measuring again ('restoreMeasured'). The
-- position pass then puts back, instead of placing again, each such subtree
-- that holds no drawing and is offered the rect the captured solve offered
-- it ('reusePositions'). Quantization runs over every node, so the
-- result is what a full solve computes. Returns each custom-measured node's
-- 'CustomMeasureRecord', for the layout cache.
solveLayout :: NodeArena -> Measurers -> Float -> Float -> Maybe LayoutCache -> IO (IntMap CustomMeasureRecord)
solveLayout na ms rootW rootH mCache = do
  count <- arenaCount na
  if count <= 0
    then pure IM.empty
    else do
      measureLog <- newIORef IM.empty
      env0 <- solveEnv na ms mCache
      (offered, unsnapped, spans) <- positionArrays na
      setPrimArray offered 0 (count * 4) (0 / 0)
      let env = env0 {seMeasureLog = Just measureLog, sePositions = Just (PositionLog offered spans)}
      measurePass env count
      positionNodeA env 0 0 (Rect 0 0 rootW rootH)
      forUpTo_ count $ \i -> do
        let copyCol col k = readGeom (seArrays env) i col >>= writePrimArray unsnapped (i * 4 + k)
        copyCol GeomX 0 >> copyCol GeomY 1 >> copyCol GeomW 2 >> copyCol GeomH 3
      floatingCount <- floatingNodeCount na
      quantizeResultsA na (seArrays env) count floatingCount (fmSnapScale (msFm ms))
      readIORef measureLog

-- | Snap the solved geometry of the @count@ nodes outside floating subtrees
-- to the device pixel grid of scale @s@. @floatingCount@ is the arena's
-- floating node count.
--
-- A floating node (modal, window, popup) and everything inside it is laid
-- out by placement after the solve, which sizes the subtree from these
-- measured sizes. Rounding them here would size a dialog and its
-- content-sized parts off their content, so the subtree keeps them, and
-- 'placeFloatingNodes' snaps it once placed ('snapFloatingSubtrees').
quantizeResultsA :: NodeArena -> NodeArenaArrays -> Int -> Int -> Float -> IO ()
quantizeResultsA na a count floatingCount s
  | s <= 0 = pure ()
  -- Nothing floats, so every node snaps and none needs marking.
  | floatingCount <= 0 = forUpTo_ count (snapNode na a s)
  | otherwise = forFloatingMarks a count $ \i inFloating -> unless inFloating (snapNode na a s i)

-- | Snap every node inside a floating subtree, after placement has laid it
-- out, as 'quantizeResultsA' snaps the page.
snapFloatingSubtrees :: NodeArena -> Float -> IO ()
snapFloatingSubtrees na s = do
  count <- arenaCount na
  floatingCount <- floatingNodeCount na
  when (s > 0 && count > 0 && floatingCount > 0) $ do
    a <- arenaArrays na
    forFloatingMarks a count $ \i inFloating -> when inFloating (snapNode na a s i)

-- | Visit the @count@ nodes in arena order with whether each is a floating
-- node or inside one. A parent always precedes its children, so one pass
-- marks each node from its parent.
{-# INLINE forFloatingMarks #-}
forFloatingMarks :: NodeArenaArrays -> Int -> (Int -> Bool -> IO ()) -> IO ()
forFloatingMarks a count k = do
  floating <- newPrimArray count :: IO (IOArr Word8)
  forUpTo_ count $ \i -> do
    nt <- readTagEnum a i TagNodeType
    parent <- readTree a i TreeParent
    inFloating <-
      if isFloatingNode nt
        then pure True
        else if parent >= 0 then (/= 0) <$> readPrimArray floating parent else pure False
    writePrimArray floating i (if inFloating then 1 else 0)
    k i inFloating

-- | Snap node @i@ to the device pixel grid of scale @s@.
snapNode :: NodeArena -> NodeArenaArrays -> Float -> Int -> IO ()
snapNode na a s i = do
  x <- readGeom a i GeomX
  y <- readGeom a i GeomY
  w <- readGeom a i GeomW
  h <- readGeom a i GeomH
  -- Snap both edges and take the size between them. Rounding the size on
  -- its own can push a node's far edge a pixel past the snapped origin of
  -- the sibling that starts there, and the node then paints over it (a
  -- table cell over the column rule beside it).
  writeGeom a i GeomX (onGrid s x)
  writeGeom a i GeomY (onGrid s y)
  writeGeom a i GeomW (max 0 (gridSpan s x (x + w)))
  writeGeom a i GeomH (max 0 (gridSpan s y (y + h)))
  nt <- readTagEnum a i TagNodeType
  when (nt == NodeScrollContainer) $ do
    si <- readTree a i TreeStyleIdx
    dir <- readTagEnum a i TagDirection
    padL <- readStyle a i StylePadL
    padT <- readStyle a i StylePadT
    val <- getNodeValue na i
    if isScrollStyle2D si
      then do
        cw <- getScrollContentW na i
        setScrollContentW na i (max 0 (gridSpan s (x + padL) (x + padL + cw)))
        setSolvedValue na i (max 0 (gridSpan s (y + padT) (y + padT + val)))
      else do
        let start = if dir == DirRow then x + padL else y + padT
        setSolvedValue na i (max 0 (gridSpan s start (start + val)))

-- | Place floating nodes ('Position.placeFloatingNodes'), then snap their
-- subtrees to the device pixel grid.
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
  Position.placeFloatingNodes na ms winW winH lookupPos lookupSize lookupAnchor mPlaced
  snapFloatingSubtrees na (fmSnapScale (msFm ms))
