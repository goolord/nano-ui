{-# LANGUAGE RecordWildCards #-}

module NanoUI.Layout.Arena
  ( NodeIdx
  , NodeType (..)
  , NodeArenaArrays (..)
  , isWidgetNode
  , isContainerNode
  , isScrollNode
  , isFloatingNode
  , SizingTag (..)
  , DirTag (..)
  , NodeArena (..)
  , FlexScratch (..)
  , WidthMemo
  , newNodeArena
  , resetNodeArena
  , arenaCount
  , arenaArrays
  , withArenaArraysSnap
  , geomStride
  , geomX
  , geomY
  , geomW
  , geomH
  , geomLayoutX
  , geomLayoutY
  , geomClipX
  , geomClipY
  , geomClipW
  , geomClipH
  , styleStride
  , styleWVal
  , styleHVal
  , stylePadL
  , stylePadR
  , stylePadT
  , stylePadB
  , styleGap
  , styleMinW
  , styleMinH
  , styleMaxW
  , styleMaxH
  , styleGrow
  , styleScrollContentW
  , styleNodeValue
  , styleGridMinColW
  , styleFontSize
  , tagStride
  , tagNodeType
  , tagDirection
  , tagWSizing
  , tagHSizing
  , tagAlignX
  , tagAlignY
  , treeStride
  , treeParent
  , treeFirstChild
  , treeNextSibling
  , treeChildCount
  , treeWidgetId
  , treeStyleIdx
  , treeTextIdx
  , treeGridCols
  , readGeom
  , writeGeom
  , readStyle
  , writeStyle
  , readTagEnum
  , writeTagEnum
  , readTree
  , writeTree
  , addNode
  , addNodeFromLayout
  , rootAttachParent
  , setNodeText
  , getParent
  , getFirstChild
  , getNextSibling
  , getChildCount
  , getNodeType
  , getDirection
  , getGridCols
  , setGridCols
  , getGridMinColW
  , setGridMinColW
  , getScrollContentW
  , setScrollContentW
  , getWidthSizing
  , getHeightSizing
  , getPadding
  , getGap
  , getMinMax
  , parentIsRow
  , getAlignX
  , getAlignY
  , getRect
  , setRect
  , getLayoutRect
  , getClipRect
  , setClipRect
  , snapshotLayoutRects
  , getText
  , getOptions
  , setOptions
  , getWidgetId
  , setWidgetId
  , lookupNodeByWidgetId
  , lookupNodeByKey
  , getStyleIdx
  , setStyleIdx
  , getNodeValue
  , setNodeValue
  , getNodeFontSize
  , setNodeFontSize
  , getNodeFontColor
  , setNodeFontColor
  , ensureScratchCapacity
  , AxisSnapshot (..)
  , ensureAxisSnapshot
  , memoizeWidth
  , forNodes_
  , forChildNodes_
  , foldFlowChildrenM
  , findNodeRevM
  , foldNodeRevM
  , findNodeM
  , foldNodesM
  , findChildM
  , LayoutCache (..)
  , newLayoutCache
  , captureLayoutCache
  , layoutCacheEligible
  , layoutInputsMatch
  , restoreLayoutCache
  ) where

import Control.Exception (bracket_)
import Control.Monad (forM_, when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.HashTable.IO (BasicHashTable)
import qualified Data.HashTable.IO as HT
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Primitive.Array (MutableArray, copyMutableArray, newArray, readArray, sizeofMutableArray, writeArray)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , copyMutablePrimArray
  , newPrimArray
  , readPrimArray
  , setPrimArray
  , writePrimArray
  )
import Data.Primitive.Types (Prim)
import GHC.Exts (RealWorld)
import Data.Text (Text)
import Data.Word (Word8, Word32, Word64)
import qualified Data.Text as T
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Style (AlignX, AlignY, Direction (..), Layout (..), Padding (..), Sizing (..))
import NanoUI.Types (Color (..), Rect (..))

type NodeIdx = Int

data NodeType
  = NodeContainer
  | NodeText
  | NodeSpacer
  | NodeSeparator
  | NodeWidget
  | NodeButton
  | NodeCheckbox
  | NodeSlider
  | NodeTextInput
  | NodeTextArea
  | NodeScrollContainer
  | NodeSelect
  | NodeModal
  | NodeImage
  | NodePanel
  | NodeWindow
  -- Appended last: stored as Word8 in the arena. Update every exhaustive
  -- NodeType case when adding variants.
  | NodeBox
  | NodeRadio
  | NodeColorPicker
  | NodeTree
  | NodePopup
  | NodeDrawing
  deriving (Eq, Show, Enum, Bounded)

isWidgetNode :: NodeType -> Bool
isWidgetNode nt =
  case nt of
    NodeWidget -> True
    NodeButton -> True
    NodeCheckbox -> True
    NodeRadio -> True
    NodeSlider -> True
    NodeTextInput -> True
    NodeTextArea -> True
    NodeSelect -> True
    NodeColorPicker -> True
    NodeTree -> True
    NodeDrawing -> True
    _ -> False

isContainerNode :: NodeType -> Bool
isContainerNode nt =
  case nt of
    NodeContainer -> True
    NodeScrollContainer -> True
    NodeModal -> True
    NodePanel -> True
    NodeWindow -> True
    NodePopup -> True
    _ -> False

isScrollNode :: NodeType -> Bool
isScrollNode nt = nt == NodeScrollContainer

isFloatingNode :: NodeType -> Bool
isFloatingNode nt = nt == NodeModal || nt == NodeWindow || nt == NodePopup

data SizingTag
  = SizingFixed
  | SizingFit
  | SizingGrow
  | SizingShrink
  | SizingPercent
  deriving (Eq, Show, Enum, Bounded)

data DirTag = DirRow | DirColumn
  deriving (Eq, Show, Enum, Bounded)

-- | Node columns. Each array holds one row of @*Stride@ slots per node; the
-- column constants below name the slots.
data NodeArenaArrays = NodeArenaArrays
  { naArrGeom :: !(MutablePrimArray RealWorld Float)
  , naArrStyle :: !(MutablePrimArray RealWorld Float)
  , naArrTags :: !(MutablePrimArray RealWorld Word8)
  , naArrTree :: !(MutablePrimArray RealWorld Int)
  , naArrTextStore :: !(MutableArray RealWorld Text)
  , naArrOptionsStore :: !(MutableArray RealWorld [Text])
  , naArrFontColor :: !(MutablePrimArray RealWorld Int)
  }

data NodeArena = NodeArena
  { naCount :: IORef Int
  , naCapacity :: IORef Int
  , naArrays :: IORef NodeArenaArrays
  , naArraysSnap :: IORef (Maybe NodeArenaArrays)
  , naScratch :: IORef FlexScratch
  -- Per-depth copies of the axis scratch while the position pass recurses.
  -- Children reuse the working scratch, so a container's child list must be
  -- snapshotted at its own depth to survive recursive positioning.
  , naSnapCap :: IORef Int
  , naSnapLevels :: IORef (MutableArray RealWorld (Maybe AxisSnapshot))
  -- Per-frame memos keyed by (node, quantized width): wrapped text sizes and
  -- fit heights. Text and style are fixed per node within a frame, so the
  -- frame tag is all that is needed to invalidate across frames.
  , naFrameTag :: IORef Word32
  , naWrapMemo :: IORef WidthMemo
  , naFitMemo :: IORef WidthMemo
  , naEpoch :: IORef Word32
  , naIndex :: IORef (BasicHashTable WidgetId Word64)
  }

-- | Flex solver scratch: child node indices, their measured widths and
-- heights, and the distributed output sizes.
data FlexScratch = FlexScratch
  { fsCap :: !Int
  , fsIdx :: !(MutablePrimArray RealWorld Int)
  , fsW :: !(MutablePrimArray RealWorld Float)
  , fsH :: !(MutablePrimArray RealWorld Float)
  , fsOutW :: !(MutablePrimArray RealWorld Float)
  , fsOutH :: !(MutablePrimArray RealWorld Float)
  }

-- | A per-frame memo of two floats per node keyed by a width. Slots hold
-- @(key, a, b)@ per node; an entry is live only while its tag equals the
-- arena's frame tag.
data WidthMemo = WidthMemo
  { wmTags :: !(MutablePrimArray RealWorld Word32)
  , wmSlots :: !(MutablePrimArray RealWorld Float)
  }

-- | Initial number of per-depth layout snapshot levels. The level array grows
-- on demand (see 'ensureSnapLevelsArr'), so this is not a depth limit.
maxSnapDepth :: Int
maxSnapDepth = 256

-- | One depth level's frozen child indices and distributed main-axis sizes.
data AxisSnapshot = AxisSnapshot
  { asIdx :: !(MutablePrimArray RealWorld Int)
  , asOut :: !(MutablePrimArray RealWorld Float)
  }

initialCapacity :: Int
initialCapacity = 256

-- | Geometry columns: solved rect, the position snapshot taken by
-- 'snapshotLayoutRects', and the clip rect.
geomStride, geomX, geomY, geomW, geomH, geomLayoutX, geomLayoutY :: Int
geomStride = 10
geomX = 0
geomY = 1
geomW = 2
geomH = 3
geomLayoutX = 4
geomLayoutY = 5

geomClipX, geomClipY, geomClipW, geomClipH :: Int
geomClipX = 6
geomClipY = 7
geomClipW = 8
geomClipH = 9

-- | Style columns: sizing values, padding, gap, min/max, grow, and per-node
-- values that are not layout inputs (scroll extent, node value, font size).
styleStride, styleWVal, styleHVal, stylePadL, stylePadR, stylePadT, stylePadB :: Int
styleStride = 16
styleWVal = 0
styleHVal = 1
stylePadL = 2
stylePadR = 3
stylePadT = 4
stylePadB = 5

styleGap, styleMinW, styleMinH, styleMaxW, styleMaxH, styleGrow :: Int
styleGap = 6
styleMinW = 7
styleMinH = 8
styleMaxW = 9
styleMaxH = 10
styleGrow = 11

styleScrollContentW, styleNodeValue, styleGridMinColW, styleFontSize :: Int
styleScrollContentW = 12
styleNodeValue = 13
styleGridMinColW = 14
styleFontSize = 15

-- | Tag columns (enum values as 'Word8'). Columns 4 and 7 are unused.
tagStride, tagNodeType, tagDirection, tagWSizing, tagHSizing, tagAlignX, tagAlignY :: Int
tagStride = 8
tagNodeType = 0
tagDirection = 1
tagWSizing = 2
tagHSizing = 3
tagAlignX = 5
tagAlignY = 6

-- | Tree columns: links, widget id, style index, text index (-1 for no text),
-- and the grid column count (containers only).
treeStride, treeParent, treeFirstChild, treeNextSibling, treeChildCount :: Int
treeStride = 8
treeParent = 0
treeFirstChild = 1
treeNextSibling = 2
treeChildCount = 3

treeWidgetId, treeStyleIdx, treeTextIdx, treeGridCols :: Int
treeWidgetId = 4
treeStyleIdx = 5
treeTextIdx = 6
treeGridCols = 7

{-# INLINE readGeom #-}
readGeom :: NodeArenaArrays -> NodeIdx -> Int -> IO Float
readGeom a idx col = readPrimArray (naArrGeom a) (idx * geomStride + col)

{-# INLINE writeGeom #-}
writeGeom :: NodeArenaArrays -> NodeIdx -> Int -> Float -> IO ()
writeGeom a idx col = writePrimArray (naArrGeom a) (idx * geomStride + col)

{-# INLINE readStyle #-}
readStyle :: NodeArenaArrays -> NodeIdx -> Int -> IO Float
readStyle a idx col = readPrimArray (naArrStyle a) (idx * styleStride + col)

{-# INLINE writeStyle #-}
writeStyle :: NodeArenaArrays -> NodeIdx -> Int -> Float -> IO ()
writeStyle a idx col = writePrimArray (naArrStyle a) (idx * styleStride + col)

{-# INLINE readTagEnum #-}
readTagEnum :: Enum e => NodeArenaArrays -> NodeIdx -> Int -> IO e
readTagEnum a idx col = do
  t <- readPrimArray (naArrTags a) (idx * tagStride + col)
  pure $! toEnum (fromIntegral t)

{-# INLINE writeTagEnum #-}
writeTagEnum :: Enum e => NodeArenaArrays -> NodeIdx -> Int -> e -> IO ()
writeTagEnum a idx col v = writePrimArray (naArrTags a) (idx * tagStride + col) (fromIntegral (fromEnum v))

{-# INLINE readTree #-}
readTree :: NodeArenaArrays -> NodeIdx -> Int -> IO Int
readTree a idx col = readPrimArray (naArrTree a) (idx * treeStride + col)

{-# INLINE writeTree #-}
writeTree :: NodeArenaArrays -> NodeIdx -> Int -> Int -> IO ()
writeTree a idx col = writePrimArray (naArrTree a) (idx * treeStride + col)

newNodeArenaArrays :: Int -> IO NodeArenaArrays
newNodeArenaArrays cap = do
  naArrGeom <- newPrimArray (cap * geomStride)
  naArrStyle <- newPrimArray (cap * styleStride)
  naArrTags <- newPrimArray (cap * tagStride)
  naArrTree <- newPrimArray (cap * treeStride)
  naArrTextStore <- newArray cap T.empty
  naArrOptionsStore <- newArray cap []
  naArrFontColor <- newPrimArray cap
  pure NodeArenaArrays {..}

newFlexScratch :: Int -> IO FlexScratch
newFlexScratch fsCap = do
  fsIdx <- newPrimArray fsCap
  fsW <- newPrimArray fsCap
  fsH <- newPrimArray fsCap
  fsOutW <- newPrimArray fsCap
  fsOutH <- newPrimArray fsCap
  pure FlexScratch {..}

-- | Memo slots per node: key and two values.
memoStride :: Int
memoStride = 3

-- Tags start zeroed: frame tags are never 0, so fresh entries always miss.
newWidthMemo :: Int -> IO WidthMemo
newWidthMemo cap = do
  wmTags <- newPrimArray cap
  setPrimArray wmTags 0 cap 0
  wmSlots <- newPrimArray (cap * memoStride)
  pure WidthMemo {..}

newNodeArena :: IO NodeArena
newNodeArena = do
  let cap = initialCapacity
      scratchCap = 64
  naCount <- newIORef 0
  naCapacity <- newIORef cap
  naArrays <- newIORef =<< newNodeArenaArrays cap
  naArraysSnap <- newIORef Nothing
  naScratch <- newIORef =<< newFlexScratch scratchCap
  naSnapCap <- newIORef scratchCap
  naSnapLevels <- newIORef =<< newArray maxSnapDepth Nothing
  naFrameTag <- newIORef 1
  naWrapMemo <- newIORef =<< newWidthMemo cap
  naFitMemo <- newIORef =<< newWidthMemo cap
  naEpoch <- newIORef 1
  naIndex <- newIORef =<< HT.new
  pure NodeArena {..}

resetNodeArena :: NodeArena -> IO ()
resetNodeArena na = do
  writeIORef (naCount na) 0
  !ft <- readIORef (naFrameTag na)
  writeIORef (naFrameTag na) (if ft == maxBound then 1 else ft + 1)
  !ep <- readIORef (naEpoch na)
  let !ep' = ep + 1
  if ep' == 0 || (ep' .&. 0x7F == 0)
    then do
      let !nextEp = if ep' == 0 then 1 else ep'
      writeIORef (naEpoch na) nextEp
      writeIORef (naIndex na) =<< HT.new
    else writeIORef (naEpoch na) ep'

{-# INLINE arenaCount #-}
arenaCount :: NodeArena -> IO Int
arenaCount na = readIORef (naCount na)

{-# INLINE arenaArrays #-}
arenaArrays :: NodeArena -> IO NodeArenaArrays
arenaArrays na = do
  m <- readIORef (naArraysSnap na)
  case m of
    Just a -> pure a
    Nothing -> readIORef (naArrays na)

-- | Pin arena column arrays for a layout pass so field reads skip naArrays IORef.
withArenaArraysSnap :: NodeArena -> IO a -> IO a
withArenaArraysSnap na act =
  bracket_
    (readIORef (naArrays na) >>= writeIORef (naArraysSnap na) . Just)
    (writeIORef (naArraysSnap na) Nothing)
    act

{-# NOINLINE ensureCapacity #-}
ensureCapacity :: NodeArena -> Int -> IO ()
ensureCapacity na needed = do
  cap <- readIORef (naCapacity na)
  if needed < cap
    then pure ()
    else do
      let newCap = cap * 2
      a <- readIORef (naArrays na)
      naArrGeom <- growPrimArrayCopy (naArrGeom a) (cap * geomStride) (newCap * geomStride) 0
      naArrStyle <- growPrimArrayCopy (naArrStyle a) (cap * styleStride) (newCap * styleStride) 0
      naArrTags <- growPrimArrayCopy (naArrTags a) (cap * tagStride) (newCap * tagStride) 0
      naArrTree <- growPrimArrayCopy (naArrTree a) (cap * treeStride) (newCap * treeStride) 0
      naArrTextStore <- growBoxedStoreCopy T.empty (naArrTextStore a) cap newCap
      naArrOptionsStore <- growBoxedStoreCopy [] (naArrOptionsStore a) cap newCap
      naArrFontColor <- growPrimArrayCopy (naArrFontColor a) cap newCap 0
      growWidthMemo (naWrapMemo na) cap newCap
      growWidthMemo (naFitMemo na) cap newCap
      let newA = NodeArenaArrays {..}
      writeIORef (naArrays na) newA
      m <- readIORef (naArraysSnap na)
      case m of
        Just{} -> writeIORef (naArraysSnap na) (Just newA)
        Nothing -> pure ()
      writeIORef (naCapacity na) newCap

{-# NOINLINE growPrimArrayCopy #-}
growPrimArrayCopy :: Prim a => MutablePrimArray RealWorld a -> Int -> Int -> a -> IO (MutablePrimArray RealWorld a)
growPrimArrayCopy oldArr cap newCap defVal = do
  newArr <- newPrimArray newCap
  copyMutablePrimArray newArr 0 oldArr 0 cap
  setPrimArray newArr cap (newCap - cap) defVal
  pure newArr

growWidthMemo :: IORef WidthMemo -> Int -> Int -> IO ()
growWidthMemo ref cap newCap = do
  WidthMemo tags slots <- readIORef ref
  wmTags <- growPrimArrayCopy tags cap newCap 0
  wmSlots <- growPrimArrayCopy slots (cap * memoStride) (newCap * memoStride) 0
  writeIORef ref WidthMemo {..}

{-# NOINLINE growBoxedStoreCopy #-}
growBoxedStoreCopy :: a -> MutableArray RealWorld a -> Int -> Int -> IO (MutableArray RealWorld a)
growBoxedStoreCopy emptyVal arr oldCap newCap = do
  newArr <- newArray newCap emptyVal
  copyMutableArray newArr 0 arr 0 oldCap
  pure newArr

{-# INLINE sizingTag #-}
sizingTag :: Sizing -> (SizingTag, Float)
sizingTag (Fixed v) = (SizingFixed, v)
sizingTag Fit = (SizingFit, 0)
sizingTag (Grow g) = (SizingGrow, g)
sizingTag (Shrink s) = (SizingShrink, s)
sizingTag (Percent p) = (SizingPercent, p)

-- Empty stack attaches to node 0 so walks from the page root still reach
-- windows/modals/popups built as UI siblings.
rootAttachParent :: NodeArena -> Int -> IO Int
rootAttachParent na parent
  | parent >= 0 = pure parent
  | otherwise = do
      n <- arenaCount na
      pure (if n > 0 then 0 else -1)

{-# INLINE addNode #-}
addNode ::
  NodeArena ->
  NodeType ->
  Int ->
  Direction ->
  Sizing ->
  Sizing ->
  Padding ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  AlignX ->
  AlignY ->
  IO NodeIdx
addNode na nt parent dir wSiz hSiz pad gap minW minH maxW maxH grow ax ay = do
  idx <- readIORef (naCount na)
  ensureCapacity na (idx + 1)
  let (wTag, wVal) = sizingTag wSiz
      (hTag, hVal) = sizingTag hSiz
  a <- arenaArrays na

  setPrimArray (naArrGeom a) (idx * geomStride) geomStride 0

  writeStyle a idx styleWVal wVal
  writeStyle a idx styleHVal hVal
  writeStyle a idx stylePadL (padL pad)
  writeStyle a idx stylePadR (padR pad)
  writeStyle a idx stylePadT (padT pad)
  writeStyle a idx stylePadB (padB pad)
  writeStyle a idx styleGap gap
  writeStyle a idx styleMinW minW
  writeStyle a idx styleMinH minH
  writeStyle a idx styleMaxW maxW
  writeStyle a idx styleMaxH maxH
  writeStyle a idx styleGrow grow
  setPrimArray (naArrStyle a) (idx * styleStride + styleScrollContentW) (styleStride - styleScrollContentW) 0

  setPrimArray (naArrTags a) (idx * tagStride) tagStride 0
  writeTagEnum a idx tagNodeType nt
  writeTagEnum a idx tagDirection $ case dir of
    Row -> DirRow
    Column -> DirColumn
  writeTagEnum a idx tagWSizing wTag
  writeTagEnum a idx tagHSizing hTag
  writeTagEnum a idx tagAlignX ax
  writeTagEnum a idx tagAlignY ay

  setPrimArray (naArrTree a) (idx * treeStride) treeStride 0
  writeTree a idx treeParent parent
  writeTree a idx treeFirstChild (-1)
  writeTree a idx treeNextSibling (-1)
  writeTree a idx treeTextIdx (-1)
  writePrimArray (naArrFontColor a) idx 0
  writeArray (naArrOptionsStore a) idx []

  when (parent >= 0) $ do
    fc <- readTree a parent treeFirstChild
    writeTree a idx treeNextSibling fc
    writeTree a parent treeFirstChild idx
    cc <- readTree a parent treeChildCount
    writeTree a parent treeChildCount (cc + 1)
  writeIORef (naCount na) (idx + 1)
  pure idx

addNodeFromLayout :: NodeArena -> NodeType -> Int -> Layout -> IO NodeIdx
addNodeFromLayout na nt parent l = do
  idx <-
    addNode
      na
      nt
      parent
      (layoutDirection l)
      (layoutWidth l)
      (layoutHeight l)
      (layoutPadding l)
      (layoutGap l)
      (layoutMinW l)
      (layoutMinH l)
      (layoutMaxW l)
      (layoutMaxH l)
      0
      (layoutAlignX l)
      (layoutAlignY l)
  setGridCols na idx (layoutGridCols l)
  setGridMinColW na idx (layoutGridMinColW l)
  setNodeFontSize na idx (layoutFontSize l)
  setNodeFontColor na idx (layoutFontColor l)
  pure idx

{-# INLINE setNodeText #-}
setNodeText :: NodeArena -> NodeIdx -> Text -> IO ()
setNodeText na idx txt = do
  a <- arenaArrays na
  writeArray (naArrTextStore a) idx txt
  writeTree a idx treeTextIdx idx

{-# INLINE getParent #-}
getParent :: NodeArena -> NodeIdx -> IO NodeIdx
getParent na idx = arenaArrays na >>= \a -> readTree a idx treeParent

{-# INLINE getFirstChild #-}
getFirstChild :: NodeArena -> NodeIdx -> IO NodeIdx
getFirstChild na idx = arenaArrays na >>= \a -> readTree a idx treeFirstChild

{-# INLINE getNextSibling #-}
getNextSibling :: NodeArena -> NodeIdx -> IO NodeIdx
getNextSibling na idx = arenaArrays na >>= \a -> readTree a idx treeNextSibling

{-# INLINE getChildCount #-}
getChildCount :: NodeArena -> NodeIdx -> IO Int
getChildCount na idx = arenaArrays na >>= \a -> readTree a idx treeChildCount

{-# INLINE getNodeType #-}
getNodeType :: NodeArena -> NodeIdx -> IO NodeType
getNodeType na idx = arenaArrays na >>= \a -> readTagEnum a idx tagNodeType

{-# INLINE getDirection #-}
getDirection :: NodeArena -> NodeIdx -> IO DirTag
getDirection na idx = arenaArrays na >>= \a -> readTagEnum a idx tagDirection

{-# INLINE getGridCols #-}
getGridCols :: NodeArena -> NodeIdx -> IO Int
getGridCols na idx = arenaArrays na >>= \a -> readTree a idx treeGridCols

{-# INLINE setGridCols #-}
setGridCols :: NodeArena -> NodeIdx -> Int -> IO ()
setGridCols na idx c = arenaArrays na >>= \a -> writeTree a idx treeGridCols c

{-# INLINE getWidthSizing #-}
getWidthSizing :: NodeArena -> NodeIdx -> IO (SizingTag, Float)
getWidthSizing na idx = arenaArrays na >>= \a -> (,) <$> readTagEnum a idx tagWSizing <*> readStyle a idx styleWVal

{-# INLINE getHeightSizing #-}
getHeightSizing :: NodeArena -> NodeIdx -> IO (SizingTag, Float)
getHeightSizing na idx = arenaArrays na >>= \a -> (,) <$> readTagEnum a idx tagHSizing <*> readStyle a idx styleHVal

{-# INLINE getPadding #-}
getPadding :: NodeArena -> NodeIdx -> IO Padding
getPadding na idx = do
  a <- arenaArrays na
  Padding <$> readStyle a idx stylePadL <*> readStyle a idx stylePadR <*> readStyle a idx stylePadT <*> readStyle a idx stylePadB

{-# INLINE getGap #-}
getGap :: NodeArena -> NodeIdx -> IO Float
getGap na idx = arenaArrays na >>= \a -> readStyle a idx styleGap

{-# INLINE getMinMax #-}
getMinMax :: NodeArena -> NodeIdx -> IO (Float, Float, Float, Float)
getMinMax na idx = do
  a <- arenaArrays na
  (,,,) <$> readStyle a idx styleMinW <*> readStyle a idx styleMinH <*> readStyle a idx styleMaxW <*> readStyle a idx styleMaxH

{-# INLINE getScrollContentW #-}
getScrollContentW :: NodeArena -> NodeIdx -> IO Float
getScrollContentW na idx = arenaArrays na >>= \a -> readStyle a idx styleScrollContentW

{-# INLINE setScrollContentW #-}
setScrollContentW :: NodeArena -> NodeIdx -> Float -> IO ()
setScrollContentW na idx v = arenaArrays na >>= \a -> writeStyle a idx styleScrollContentW v

{-# INLINE getGridMinColW #-}
getGridMinColW :: NodeArena -> NodeIdx -> IO Float
getGridMinColW na idx = arenaArrays na >>= \a -> readStyle a idx styleGridMinColW

{-# INLINE setGridMinColW #-}
setGridMinColW :: NodeArena -> NodeIdx -> Float -> IO ()
setGridMinColW na idx v = arenaArrays na >>= \a -> writeStyle a idx styleGridMinColW v

{-# INLINE parentIsRow #-}
parentIsRow :: NodeArena -> NodeIdx -> IO Bool
parentIsRow na idx = do
  p <- getParent na idx
  if p < 0
    then pure False
    else do
      dir <- getDirection na p
      pure (dir == DirRow)

{-# INLINE getAlignX #-}
getAlignX :: NodeArena -> NodeIdx -> IO AlignX
getAlignX na idx = arenaArrays na >>= \a -> readTagEnum a idx tagAlignX

{-# INLINE getAlignY #-}
getAlignY :: NodeArena -> NodeIdx -> IO AlignY
getAlignY na idx = arenaArrays na >>= \a -> readTagEnum a idx tagAlignY

{-# INLINE getRect #-}
getRect :: NodeArena -> NodeIdx -> IO (Float, Float, Float, Float)
getRect na idx = do
  a <- arenaArrays na
  (,,,) <$> readGeom a idx geomX <*> readGeom a idx geomY <*> readGeom a idx geomW <*> readGeom a idx geomH

{-# INLINE setRect #-}
setRect :: NodeArena -> NodeIdx -> Float -> Float -> Float -> Float -> IO ()
setRect na idx x y w h = do
  a <- arenaArrays na
  writeGeom a idx geomX x
  writeGeom a idx geomY y
  writeGeom a idx geomW w
  writeGeom a idx geomH h

{-# INLINE getLayoutRect #-}
getLayoutRect :: NodeArena -> NodeIdx -> IO (Float, Float, Float, Float)
getLayoutRect na idx = do
  a <- arenaArrays na
  (,,,) <$> readGeom a idx geomLayoutX <*> readGeom a idx geomLayoutY <*> readGeom a idx geomW <*> readGeom a idx geomH

{-# INLINE getClipRect #-}
getClipRect :: NodeArena -> NodeIdx -> IO (Maybe Rect)
getClipRect na idx = do
  a <- arenaArrays na
  x <- readGeom a idx geomClipX
  y <- readGeom a idx geomClipY
  w <- readGeom a idx geomClipW
  h <- readGeom a idx geomClipH
  let r = Rect x y w h
  pure (if w > 0 && h > 0 then Just r else Nothing)

{-# INLINE setClipRect #-}
setClipRect :: NodeArena -> NodeIdx -> Rect -> IO ()
setClipRect na idx (Rect x y w h) = do
  a <- arenaArrays na
  writeGeom a idx geomClipX x
  writeGeom a idx geomClipY y
  writeGeom a idx geomClipW w
  writeGeom a idx geomClipH h

{-# INLINE snapshotLayoutRects #-}
snapshotLayoutRects :: NodeArena -> IO ()
snapshotLayoutRects na = do
  a <- arenaArrays na
  forNodes_ na $ \i -> do
    readGeom a i geomX >>= writeGeom a i geomLayoutX
    readGeom a i geomY >>= writeGeom a i geomLayoutY

-- | Cached layout signature and solved geometry for whole-layout reuse. The
-- backing arrays are reused; only cache misses capture a new solved frame.
data LayoutCache = LayoutCache
  { lcCap :: !Int
  , lcCount :: !Int
  , lcGeom :: !(MutablePrimArray RealWorld Float)
  , lcStyle :: !(MutablePrimArray RealWorld Float)
  , lcTags :: !(MutablePrimArray RealWorld Word8)
  , lcTree :: !(MutablePrimArray RealWorld Int)
  , lcText :: !(MutableArray RealWorld Text)
  , lcOptions :: !(MutableArray RealWorld [Text])
  }

newLayoutCache :: Int -> IO LayoutCache
newLayoutCache cap0 = do
  let !cap = max 16 cap0
  lcGeom <- newPrimArray (cap * geomStride)
  lcStyle <- newPrimArray (cap * styleStride)
  lcTags <- newPrimArray (cap * tagStride)
  lcTree <- newPrimArray (cap * treeStride)
  lcText <- newArray cap T.empty
  lcOptions <- newArray cap []
  pure LayoutCache {lcCap = cap, lcCount = 0, ..}

growLayoutCache :: LayoutCache -> Int -> IO LayoutCache
growLayoutCache lc needed
  | needed <= lcCap lc = pure lc
  | otherwise = do
      let !oldCap = lcCap lc
          !newCap = max needed (oldCap * 2)
      lcGeom <- growPrimArrayCopy (lcGeom lc) (oldCap * geomStride) (newCap * geomStride) 0
      lcStyle <- growPrimArrayCopy (lcStyle lc) (oldCap * styleStride) (newCap * styleStride) 0
      lcTags <- growPrimArrayCopy (lcTags lc) (oldCap * tagStride) (newCap * tagStride) 0
      lcTree <- growPrimArrayCopy (lcTree lc) (oldCap * treeStride) (newCap * treeStride) 0
      lcText <- growBoxedStoreCopy T.empty (lcText lc) oldCap newCap
      lcOptions <- growBoxedStoreCopy [] (lcOptions lc) oldCap newCap
      pure LayoutCache {lcCap = newCap, lcCount = lcCount lc, ..}

-- | Snapshot the current (post-solve) arena form, constraints and rects.
captureLayoutCache :: NodeArena -> LayoutCache -> IO LayoutCache
captureLayoutCache na lc0 = do
  n <- arenaCount na
  lc <- growLayoutCache lc0 n
  a <- arenaArrays na
  copyMutablePrimArray (lcGeom lc) 0 (naArrGeom a) 0 (n * geomStride)
  copyMutablePrimArray (lcStyle lc) 0 (naArrStyle a) 0 (n * styleStride)
  copyMutablePrimArray (lcTags lc) 0 (naArrTags a) 0 (n * tagStride)
  copyMutablePrimArray (lcTree lc) 0 (naArrTree a) 0 (n * treeStride)
  copyMutableArray (lcText lc) 0 (naArrTextStore a) 0 n
  copyMutableArray (lcOptions lc) 0 (naArrOptionsStore a) 0 n
  pure lc {lcCount = n}

-- | Floating placement depends on state outside the arena descriptor. Custom
-- measurement is checked separately by Frame, which owns its registration.
layoutCacheEligible :: NodeArena -> IO Bool
layoutCacheEligible na = do
  n <- arenaCount na
  a <- arenaArrays na
  if n <= 0
    then pure False
    else allRangeM 0 n $ \i -> not . isFloatingNode <$> readTagEnum a i tagNodeType

-- | Compare layout inputs, stopping at the first mismatch. Node values are
-- paint state except on scroll containers, where they are solver outputs.
-- Neither belongs in the layout-input signature.
layoutInputsMatch :: NodeArena -> LayoutCache -> IO Bool
layoutInputsMatch na lc = do
  n <- arenaCount na
  if n <= 0 || n /= lcCount lc
    then pure False
    else do
      a <- arenaArrays na
      eligible <- layoutCacheEligible na
      if not eligible
        then pure False
        else do
          andThen (styleMatch (naArrStyle a) (lcStyle lc) n) $
            andThen (allRangeM 0 (n * tagStride) (primEqAt (naArrTags a) (lcTags lc))) $
              andThen (treeMatch a (lcTree lc) n) $
                andThen (allRangeM 0 n (boxedEqAt (naArrTextStore a) (lcText lc))) $
                  allRangeM 0 n (boxedEqAt (naArrOptionsStore a) (lcOptions lc))

{-# INLINE andThen #-}
andThen :: IO Bool -> IO Bool -> IO Bool
andThen check next = do
  ok <- check
  if ok then next else pure False

-- | Whether @p@ holds at every index in @[lo, hi)@, stopping at the first miss.
{-# INLINE allRangeM #-}
allRangeM :: Int -> Int -> (Int -> IO Bool) -> IO Bool
allRangeM lo hi p = go lo
  where
    go !i
      | i >= hi = pure True
      | otherwise = do
          ok <- p i
          if ok then go (i + 1) else pure False

{-# INLINE primEqAt #-}
primEqAt :: (Prim a, Eq a) => MutablePrimArray RealWorld a -> MutablePrimArray RealWorld a -> Int -> IO Bool
primEqAt x y i = (==) <$> readPrimArray x i <*> readPrimArray y i

{-# INLINE boxedEqAt #-}
boxedEqAt :: Eq a => MutableArray RealWorld a -> MutableArray RealWorld a -> Int -> IO Bool
boxedEqAt x y i = (==) <$> readArray x i <*> readArray y i

-- The scroll-extent and node-value columns hold solver outputs or paint-only
-- values, so they are skipped.
styleMatch :: MutablePrimArray RealWorld Float -> MutablePrimArray RealWorld Float -> Int -> IO Bool
styleMatch x y n =
  allRangeM 0 n $ \i ->
    let !base = i * styleStride
     in andThen (allRangeM base (base + styleScrollContentW) (primEqAt x y)) $
          allRangeM (base + styleGridMinColW) (base + styleStride) (primEqAt x y)

-- Box/image/drawing style IDs are paint data; their intrinsic dimensions come
-- from sizing constraints. The grid column count only matters to containers.
treeMatch :: NodeArenaArrays -> MutablePrimArray RealWorld Int -> Int -> IO Bool
treeMatch a cached n =
  allRangeM 0 n $ \i -> do
    nt <- readTagEnum a i tagNodeType
    let paintStyle = nt == NodeBox || nt == NodeImage || nt == NodeDrawing
        !base = i * treeStride
    allRangeM 0 treeStride $ \j ->
      if (j == treeStyleIdx && paintStyle) || (j == treeGridCols && not (isContainerNode nt))
        then pure True
        else (==) <$> readTree a i j <*> readPrimArray cached (base + j)

-- | Restore only solver outputs. Rebuilt paint values/colors must survive a
-- cache hit; copying the entire cached style array would revert them.
restoreLayoutCache :: NodeArena -> LayoutCache -> IO ()
restoreLayoutCache na lc = do
  a <- arenaArrays na
  let !n = lcCount lc
  copyMutablePrimArray (naArrGeom a) 0 (lcGeom lc) 0 (n * geomStride)
  let go !i
        | i >= n = pure ()
        | otherwise = do
            nt <- readTagEnum a i tagNodeType
            -- Scroll content width and node value (the content height).
            when (isScrollNode nt) $
              let !off = i * styleStride + styleScrollContentW
               in copyMutablePrimArray (naArrStyle a) off (lcStyle lc) off 2
            go (i + 1)
  go 0

{-# INLINE getText #-}
getText :: NodeArena -> NodeIdx -> IO Text
getText na idx = do
  a <- arenaArrays na
  ti <- readTree a idx treeTextIdx
  if ti < 0
    then pure T.empty
    else readArray (naArrTextStore a) ti

{-# INLINE getOptions #-}
getOptions :: NodeArena -> NodeIdx -> IO [Text]
getOptions na idx = do
  a <- arenaArrays na
  readArray (naArrOptionsStore a) idx

{-# INLINE setOptions #-}
setOptions :: NodeArena -> NodeIdx -> [Text] -> IO ()
setOptions na idx opts = do
  a <- arenaArrays na
  writeArray (naArrOptionsStore a) idx opts

{-# INLINE getWidgetId #-}
getWidgetId :: NodeArena -> NodeIdx -> IO WidgetId
getWidgetId na idx = arenaArrays na >>= \a -> WidgetId . fromIntegral <$> readTree a idx treeWidgetId

{-# INLINE packEpochNode #-}
packEpochNode :: Word32 -> NodeIdx -> Word64
packEpochNode !epoch !idx = (fromIntegral epoch `shiftL` 32) .|. (fromIntegral idx .&. 0xFFFFFFFF)

{-# INLINE unpackEpochNode #-}
unpackEpochNode :: Word64 -> (Word32, NodeIdx)
unpackEpochNode !w = (fromIntegral (w `shiftR` 32), fromIntegral (w .&. 0xFFFFFFFF))

{-# INLINE setWidgetId #-}
setWidgetId :: NodeArena -> NodeIdx -> WidgetId -> IO ()
setWidgetId na idx wid = do
  a <- arenaArrays na
  let WidgetId w = wid
  writeTree a idx treeWidgetId (fromIntegral w)
  when (hashWidgetId wid /= 0) $ do
    !ep <- readIORef (naEpoch na)
    table <- readIORef (naIndex na)
    HT.insert table wid (packEpochNode ep idx)

{-# INLINE lookupNodeByWidgetId #-}
lookupNodeByWidgetId :: NodeArena -> WidgetId -> IO (Maybe NodeIdx)
lookupNodeByWidgetId na wid
  | hashWidgetId wid == 0 = pure Nothing
  | otherwise = do
      table <- readIORef (naIndex na)
      mVal <- HT.lookup table wid
      case mVal of
        Nothing -> pure Nothing
        Just val -> do
          !ep <- readIORef (naEpoch na)
          let (!entryEp, !idx) = unpackEpochNode val
          pure (if entryEp == ep then Just idx else Nothing)

{-# INLINE lookupNodeByKey #-}
lookupNodeByKey :: NodeArena -> Int -> IO (Maybe NodeIdx)
lookupNodeByKey na key = lookupNodeByWidgetId na (WidgetId (fromIntegral key))

{-# INLINE getNodeValue #-}
getNodeValue :: NodeArena -> NodeIdx -> IO Float
getNodeValue na idx = arenaArrays na >>= \a -> readStyle a idx styleNodeValue

{-# INLINE setNodeValue #-}
setNodeValue :: NodeArena -> NodeIdx -> Float -> IO ()
setNodeValue na idx v = arenaArrays na >>= \a -> writeStyle a idx styleNodeValue v

{-# INLINE getNodeFontSize #-}
getNodeFontSize :: NodeArena -> NodeIdx -> IO Float
getNodeFontSize na idx = arenaArrays na >>= \a -> readStyle a idx styleFontSize

{-# INLINE setNodeFontSize #-}
setNodeFontSize :: NodeArena -> NodeIdx -> Float -> IO ()
setNodeFontSize na idx v = arenaArrays na >>= \a -> writeStyle a idx styleFontSize v

-- | Per-node font color (paint-only, intentionally kept out of @naArrTree@
-- where 'treeGridCols' holds the grid column count for containers).
{-# INLINE getNodeFontColor #-}
getNodeFontColor :: NodeArena -> NodeIdx -> IO (Maybe Color)
getNodeFontColor na idx = do
  a <- arenaArrays na
  val <- readPrimArray (naArrFontColor a) idx
  if (val .&. 0x100000000) /= 0
    then pure (Just (Color (fromIntegral (val .&. 0xFFFFFFFF))))
    else pure Nothing

{-# INLINE setNodeFontColor #-}
setNodeFontColor :: NodeArena -> NodeIdx -> Maybe Color -> IO ()
setNodeFontColor na idx mCol = do
  a <- arenaArrays na
  let val = case mCol of
        Nothing -> 0
        Just (Color w) -> 0x100000000 .|. fromIntegral w
  writePrimArray (naArrFontColor a) idx val

{-# INLINE getStyleIdx #-}
getStyleIdx :: NodeArena -> NodeIdx -> IO Int
getStyleIdx na idx = arenaArrays na >>= \a -> readTree a idx treeStyleIdx

{-# INLINE setStyleIdx #-}
setStyleIdx :: NodeArena -> NodeIdx -> Int -> IO ()
setStyleIdx na idx v = arenaArrays na >>= \a -> writeTree a idx treeStyleIdx v

-- | Get the snapshot buffers for a recursion depth, grown to hold at least
-- @needed@ entries. Buffers are reused across frames; nothing is allocated in
-- steady state once capacity is warm.
{-# NOINLINE ensureAxisSnapshot #-}
ensureAxisSnapshot :: NodeArena -> Int -> Int -> IO AxisSnapshot
ensureAxisSnapshot na depth needed = do
  arr0 <- readIORef (naSnapLevels na)
  let !d = max 0 depth
  arr <- ensureSnapLevelsArr na arr0 (d + 1)
  cap <- readIORef (naSnapCap na)
  if needed <= cap
    then getLevel arr d cap
    else do
      let !newCap = max needed (cap * 2)
          !levels = sizeofMutableArray arr
      forM_ [0 .. levels - 1] $ \i -> do
        m <- readArray arr i
        case m of
          Nothing -> pure ()
          Just (AxisSnapshot idx out) -> do
            idx' <- growPrimArrayCopy idx cap newCap 0
            out' <- growPrimArrayCopy out cap newCap 0
            writeArray arr i (Just (AxisSnapshot idx' out'))
      writeIORef (naSnapCap na) newCap
      getLevel arr d newCap
  where
    getLevel arr d currentCap = do
      m <- readArray arr d
      case m of
        Just s -> pure s
        Nothing -> do
          asIdx <- newPrimArray currentCap
          asOut <- newPrimArray currentCap
          let s = AxisSnapshot asIdx asOut
          writeArray arr d (Just s)
          pure s

-- | Grow the per-depth snapshot-level array to hold at least @need@ levels.
-- Replaces the old fixed depth clamp so arbitrarily deep nesting is safe.
ensureSnapLevelsArr :: NodeArena -> MutableArray RealWorld (Maybe AxisSnapshot) -> Int -> IO (MutableArray RealWorld (Maybe AxisSnapshot))
ensureSnapLevelsArr na arr need = do
  let !sz = sizeofMutableArray arr
  if need <= sz
    then pure arr
    else do
      let !newSz = max need (sz * 2)
      arr' <- newArray newSz Nothing
      copyMutableArray arr' 0 arr 0 sz
      writeIORef (naSnapLevels na) arr'
      pure arr'

-- | Memoize @compute@ for node @idx@ at width @key@ in one of the arena's
-- per-frame memos. Widths within 0.25 px share an entry so near-identical
-- reflows still hit.
{-# INLINE memoizeWidth #-}
memoizeWidth :: NodeArena -> IORef WidthMemo -> NodeIdx -> Float -> IO (Float, Float) -> IO (Float, Float)
memoizeWidth na ref idx key compute = do
  ft <- readIORef (naFrameTag na)
  WidthMemo tags slots <- readIORef ref
  tag <- readPrimArray tags idx
  let !base = idx * memoStride
  hit <-
    if tag /= ft
      then pure False
      else do
        k <- readPrimArray slots base
        pure (abs (k - key) <= 0.25)
  if hit
    then (,) <$> readPrimArray slots (base + 1) <*> readPrimArray slots (base + 2)
    else do
      r@(x, y) <- compute
      WidthMemo tags' slots' <- readIORef ref
      writePrimArray tags' idx ft
      writePrimArray slots' base key
      writePrimArray slots' (base + 1) x
      writePrimArray slots' (base + 2) y
      pure r

-- | The flex scratch, grown to hold at least @needed@ entries.
{-# INLINE ensureScratchCapacity #-}
ensureScratchCapacity :: NodeArena -> Int -> IO FlexScratch
ensureScratchCapacity na needed = do
  s <- readIORef (naScratch na)
  if needed <= fsCap s then pure s else growScratch na s needed

{-# NOINLINE growScratch #-}
growScratch :: NodeArena -> FlexScratch -> Int -> IO FlexScratch
growScratch na s needed = do
  let !cap = fsCap s
      !newCap = max needed (cap * 2)
  fsIdx <- growPrimArrayCopy (fsIdx s) cap newCap (-1)
  fsW <- growPrimArrayCopy (fsW s) cap newCap 0
  fsH <- growPrimArrayCopy (fsH s) cap newCap 0
  fsOutW <- growPrimArrayCopy (fsOutW s) cap newCap 0
  fsOutH <- growPrimArrayCopy (fsOutH s) cap newCap 0
  let s' = FlexScratch {fsCap = newCap, ..}
  writeIORef (naScratch na) s'
  pure s'

{-# INLINE forNodes_ #-}
forNodes_ :: NodeArena -> (NodeIdx -> IO ()) -> IO ()
forNodes_ na f = do
  n <- arenaCount na
  let go !i
        | i >= n = pure ()
        | otherwise = f i >> go (i + 1)
  go 0

{-# INLINE forChildNodes_ #-}
forChildNodes_ :: NodeArena -> NodeIdx -> (NodeIdx -> IO ()) -> IO ()
forChildNodes_ na parentIdx f = do
  fc <- getFirstChild na parentIdx
  let go !ci
        | ci < 0 = pure ()
        | otherwise = do
            f ci
            ns <- getNextSibling na ci
            go ns
  go fc

-- | Fold over a node's children in sibling order, skipping floating
-- (modal, window, popup) children, which are placed outside the flow.
{-# INLINE foldFlowChildrenM #-}
foldFlowChildrenM :: NodeArena -> NodeIdx -> (acc -> NodeIdx -> IO acc) -> acc -> IO acc
foldFlowChildrenM na parentIdx f z = do
  fc <- getFirstChild na parentIdx
  let go !ci !acc
        | ci < 0 = pure acc
        | otherwise = do
            nt <- getNodeType na ci
            ns <- getNextSibling na ci
            if isFloatingNode nt
              then go ns acc
              else f acc ci >>= go ns
  go fc z

{-# INLINE findNodeRevM #-}
findNodeRevM :: NodeArena -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findNodeRevM na p = do
  n <- arenaCount na
  let go !i
        | i < 0 = pure Nothing
        | otherwise = do
            ok <- p i
            if ok then pure (Just i) else go (i - 1)
  go (n - 1)


{-# INLINE foldNodeRevM #-}
foldNodeRevM :: NodeArena -> (a -> NodeIdx -> IO a) -> a -> IO a
foldNodeRevM na f z = do
  n <- arenaCount na
  let go !i !acc
        | i < 0 = pure acc
        | otherwise = do
            acc' <- f acc i
            go (i - 1) acc'
  go (n - 1) z

-- ---------------------------------------------------------------------------
-- Frame traversal helpers: forward node scans and child searches, shaped like
-- 'forNodes_' and 'findNodeRevM'.
-- ---------------------------------------------------------------------------

-- | First node, in arena order, satisfying the predicate.
{-# INLINE findNodeM #-}
findNodeM :: NodeArena -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findNodeM na p = do
  n <- arenaCount na
  let go !i
        | i >= n = pure Nothing
        | otherwise = do
            ok <- p i
            if ok then pure (Just i) else go (i + 1)
  go 0

-- | Left fold over every node in arena order.
{-# INLINE foldNodesM #-}
foldNodesM :: NodeArena -> (a -> NodeIdx -> IO a) -> a -> IO a
foldNodesM na f z = do
  n <- arenaCount na
  let go !i !acc
        | i >= n = pure acc
        | otherwise = f acc i >>= go (i + 1)
  go 0 z

-- | First direct child of @parentIdx@ satisfying the predicate.
{-# INLINE findChildM #-}
findChildM :: NodeArena -> NodeIdx -> (NodeIdx -> IO Bool) -> IO (Maybe NodeIdx)
findChildM na parentIdx p = do
  fc <- getFirstChild na parentIdx
  let go !ci
        | ci < 0 = pure Nothing
        | otherwise = do
            ok <- p ci
            if ok then pure (Just ci) else getNextSibling na ci >>= go
  go fc
