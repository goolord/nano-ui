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
  , newNodeArena
  , resetNodeArena
  , arenaCount
  , arenaArrays
  , withArenaArraysSnap
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
  , getGrow
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
  , lookupWrapMemo
  , storeWrapMemo
  , forNodes_
  , forChildNodes_
  , findNodeRevM
  , foldNodeRevM
  ) where

import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.HashTable.IO (BasicHashTable)
import qualified Data.HashTable.IO as HT
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Primitive.Array (MutableArray, newArray, readArray, writeArray)
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
import NanoUI.Style (AlignX (..), AlignY (..), Direction (..), Layout (..), Padding (..), Sizing (..))
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
    NodeImage -> False
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

data NodeArenaArrays = NodeArenaArrays
  { naArrGeom :: !(MutablePrimArray RealWorld Float)
  , naArrStyle :: !(MutablePrimArray RealWorld Float)
  , naArrTags :: !(MutablePrimArray RealWorld Word8)
  , naArrTree :: !(MutablePrimArray RealWorld Int)
  , naArrTextStore :: !(MutableArray RealWorld Text)
  , naArrOptionsStore :: !(MutableArray RealWorld [Text])
  }

data NodeArena = NodeArena
  { naCount :: IORef Int
  , naCapacity :: IORef Int
  , naArrays :: IORef NodeArenaArrays
  , naArraysSnap :: IORef (Maybe NodeArenaArrays)
  -- Flex solver scratch: child node indices + main/cross sizes + distributed outs.
  , naScratchCap :: IORef Int
  , naScratchCount :: IORef Int
  , naScratchIdx :: IORef (MutablePrimArray RealWorld Int)
  , naScratchMain :: IORef (MutablePrimArray RealWorld Float)
  , naScratchCross :: IORef (MutablePrimArray RealWorld Float)
  , naScratchOutMain :: IORef (MutablePrimArray RealWorld Float)
  , naScratchOutCross :: IORef (MutablePrimArray RealWorld Float)
  -- Per-depth copies of the axis scratch while the position pass recurses.
  -- Children reuse the working scratch, so a container's child list must be
  -- snapshotted at its own depth to survive recursive positioning.
  , naSnapCap :: IORef Int
  , naSnapLevels :: IORef (IM.IntMap AxisSnapshot)
  -- Per-frame memo of wrapped text sizes keyed by (node, quantized wrap
  -- width). Text and style are fixed per node within a frame, so the frame
  -- tag is all that is needed to invalidate across frames.
  , naFrameTag :: IORef Word32
  , naWrapTag :: IORef (MutablePrimArray RealWorld Word32)
  , naWrapKey :: IORef (MutablePrimArray RealWorld Float)
  , naWrapW :: IORef (MutablePrimArray RealWorld Float)
  , naWrapH :: IORef (MutablePrimArray RealWorld Float)
  , naEpoch :: IORef Word32
  , naIndex :: IORef (BasicHashTable WidgetId Word64)
  }

-- | One depth level's frozen child indices and distributed main-axis sizes.
data AxisSnapshot = AxisSnapshot
  { asIdx :: !(MutablePrimArray RealWorld Int)
  , asOut :: !(MutablePrimArray RealWorld Float)
  }

initialCapacity :: Int
initialCapacity = 256

newNodeArenaArrays :: Int -> IO NodeArenaArrays
newNodeArenaArrays cap = do
  naArrGeom <- newPrimArray (cap * 10)
  naArrStyle <- newPrimArray (cap * 16)
  naArrTags <- newPrimArray (cap * 8)
  naArrTree <- newPrimArray (cap * 8)
  naArrTextStore <- newArray cap T.empty
  naArrOptionsStore <- newArray cap []
  pure NodeArenaArrays {..}

{-# INLINE newNodeArena #-}
newNodeArena :: IO NodeArena
newNodeArena = do
  let cap = initialCapacity
  naCount <- newIORef 0
  naCapacity <- newIORef cap
  naArrays <- newIORef =<< newNodeArenaArrays cap
  naArraysSnap <- newIORef Nothing
  let scratchCap = 64
  naScratchCap <- newIORef scratchCap
  naScratchCount <- newIORef 0
  naScratchIdx <- newIORef =<< newPrimArray scratchCap
  naScratchMain <- newIORef =<< newPrimArray scratchCap
  naScratchCross <- newIORef =<< newPrimArray scratchCap
  naScratchOutMain <- newIORef =<< newPrimArray scratchCap
  naScratchOutCross <- newIORef =<< newPrimArray scratchCap
  naSnapCap <- newIORef scratchCap
  naSnapLevels <- newIORef IM.empty
  naFrameTag <- newIORef 1
  naWrapTag <- newIORef =<< newPrimArray cap
  naWrapKey <- newIORef =<< newPrimArray cap
  naWrapW <- newIORef =<< newPrimArray cap
  naWrapH <- newIORef =<< newPrimArray cap
  naEpoch <- newIORef 1
  naIndex <- newIORef =<< HT.new
  pure
    NodeArena
      { naCount
      , naCapacity
      , naArrays
      , naArraysSnap
      , naScratchCap
      , naScratchCount
      , naScratchIdx
      , naScratchMain
      , naScratchCross
      , naScratchOutMain
      , naScratchOutCross
      , naSnapCap
      , naSnapLevels
      , naFrameTag
      , naWrapTag
      , naWrapKey
      , naWrapW
      , naWrapH
      , naEpoch
      , naIndex
      }

{-# INLINE resetNodeArena #-}
resetNodeArena :: NodeArena -> IO ()
resetNodeArena na = do
  writeIORef (naCount na) 0
  !ft <- readIORef (naFrameTag na)
  writeIORef (naFrameTag na) (if ft == maxBound then 1 else ft + 1)
  !ep <- readIORef (naEpoch na)
  let !ep' = ep + 1
  if ep' == 0
    then do
      writeIORef (naEpoch na) 1
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
  bracket
    ( do
        a <- readIORef (naArrays na)
        writeIORef (naArraysSnap na) (Just a)
    )
    (\_ -> writeIORef (naArraysSnap na) Nothing)
    (\_ -> act)

{-# NOINLINE ensureCapacity #-}
ensureCapacity :: NodeArena -> Int -> IO ()
ensureCapacity na needed = do
  cap <- readIORef (naCapacity na)
  if needed < cap
    then pure ()
    else do
      let newCap = cap * 2
      a <- readIORef (naArrays na)
      naArrGeom <- growPrimArrayCopy (naArrGeom a) (cap * 10) (newCap * 10) 0
      naArrStyle <- growPrimArrayCopy (naArrStyle a) (cap * 16) (newCap * 16) 0
      naArrTags <- growPrimArrayCopy (naArrTags a) (cap * 8) (newCap * 8) 0
      naArrTree <- growPrimArrayCopy (naArrTree a) (cap * 8) (newCap * 8) 0
      naArrTextStore <- growBoxedStoreCopy T.empty (naArrTextStore a) cap newCap
      naArrOptionsStore <- growBoxedStoreCopy [] (naArrOptionsStore a) cap newCap
      growPrimArray (naWrapTag na) cap newCap 0
      growPrimArray (naWrapKey na) cap newCap 0
      growPrimArray (naWrapW na) cap newCap 0
      growPrimArray (naWrapH na) cap newCap 0
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

{-# NOINLINE growPrimArray #-}
growPrimArray :: Prim a => IORef (MutablePrimArray RealWorld a) -> Int -> Int -> a -> IO ()
growPrimArray ref cap newCap defVal = do
  oldArr <- readIORef ref
  newArr <- growPrimArrayCopy oldArr cap newCap defVal
  writeIORef ref newArr

{-# NOINLINE growBoxedStoreCopy #-}
growBoxedStoreCopy :: a -> MutableArray RealWorld a -> Int -> Int -> IO (MutableArray RealWorld a)
growBoxedStoreCopy emptyVal arr oldCap newCap = do
  newArr <- newArray newCap emptyVal
  forM_ [0 .. oldCap - 1] $ \i ->
    readArray arr i >>= writeArray newArr i
  forM_ [oldCap .. newCap - 1] $ \i ->
    writeArray newArr i emptyVal
  pure newArr

{-# INLINE sizingTag #-}
sizingTag :: Sizing -> (SizingTag, Float)
sizingTag (Fixed v) = (SizingFixed, v)
sizingTag Fit = (SizingFit, 0)
sizingTag (Grow g) = (SizingGrow, g)
sizingTag (Shrink s) = (SizingShrink, s)
sizingTag (Percent p) = (SizingPercent, p)

{-# INLINE dirTag #-}
dirTag :: Direction -> DirTag
dirTag Row = DirRow
dirTag Column = DirColumn

{-# INLINE alignXTag #-}
alignXTag :: AlignX -> Word8
alignXTag AlignStart = 0
alignXTag AlignCenter = 1
alignXTag AlignEnd = 2

{-# INLINE alignYTag #-}
alignYTag :: AlignY -> Word8
alignYTag AlignTop = 0
alignYTag AlignMiddle = 1
alignYTag AlignBottom = 2

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

  let gBase = idx * 10
  writePrimArray (naArrGeom a) (gBase + 0) 0
  writePrimArray (naArrGeom a) (gBase + 1) 0
  writePrimArray (naArrGeom a) (gBase + 2) 0
  writePrimArray (naArrGeom a) (gBase + 3) 0
  writePrimArray (naArrGeom a) (gBase + 4) 0
  writePrimArray (naArrGeom a) (gBase + 5) 0
  writePrimArray (naArrGeom a) (gBase + 6) 0
  writePrimArray (naArrGeom a) (gBase + 7) 0
  writePrimArray (naArrGeom a) (gBase + 8) 0
  writePrimArray (naArrGeom a) (gBase + 9) 0

  let sBase = idx * 16
  writePrimArray (naArrStyle a) (sBase + 0) wVal
  writePrimArray (naArrStyle a) (sBase + 1) hVal
  writePrimArray (naArrStyle a) (sBase + 2) (padL pad)
  writePrimArray (naArrStyle a) (sBase + 3) (padR pad)
  writePrimArray (naArrStyle a) (sBase + 4) (padT pad)
  writePrimArray (naArrStyle a) (sBase + 5) (padB pad)
  writePrimArray (naArrStyle a) (sBase + 6) gap
  writePrimArray (naArrStyle a) (sBase + 7) minW
  writePrimArray (naArrStyle a) (sBase + 8) minH
  writePrimArray (naArrStyle a) (sBase + 9) maxW
  writePrimArray (naArrStyle a) (sBase + 10) maxH
  writePrimArray (naArrStyle a) (sBase + 11) grow
  writePrimArray (naArrStyle a) (sBase + 12) 0
  writePrimArray (naArrStyle a) (sBase + 13) 0
  writePrimArray (naArrStyle a) (sBase + 14) 0
  writePrimArray (naArrStyle a) (sBase + 15) 0

  let tagBase = idx * 8
  writePrimArray (naArrTags a) (tagBase + 0) (fromIntegral (fromEnum nt))
  writePrimArray (naArrTags a) (tagBase + 1) (fromIntegral (fromEnum (dirTag dir)))
  writePrimArray (naArrTags a) (tagBase + 2) (fromIntegral (fromEnum wTag))
  writePrimArray (naArrTags a) (tagBase + 3) (fromIntegral (fromEnum hTag))
  writePrimArray (naArrTags a) (tagBase + 4) 0
  writePrimArray (naArrTags a) (tagBase + 5) (alignXTag ax)
  writePrimArray (naArrTags a) (tagBase + 6) (alignYTag ay)
  writePrimArray (naArrTags a) (tagBase + 7) 0

  let tBase = idx * 8
  writePrimArray (naArrTree a) (tBase + 0) parent
  writePrimArray (naArrTree a) (tBase + 1) (-1)
  writePrimArray (naArrTree a) (tBase + 2) (-1)
  writePrimArray (naArrTree a) (tBase + 3) 0
  writePrimArray (naArrTree a) (tBase + 4) 0
  writePrimArray (naArrTree a) (tBase + 5) 0
  writePrimArray (naArrTree a) (tBase + 6) (-1)
  writePrimArray (naArrTree a) (tBase + 7) 0
  writeArray (naArrOptionsStore a) idx []

  if parent >= 0
    then do
      let pBase = parent * 8
      fc <- readPrimArray (naArrTree a) (pBase + 1)
      writePrimArray (naArrTree a) (tBase + 2) fc
      writePrimArray (naArrTree a) (pBase + 1) idx
      cc <- readPrimArray (naArrTree a) (pBase + 3)
      writePrimArray (naArrTree a) (pBase + 3) (cc + 1)
    else pure ()
  writeIORef (naCount na) (idx + 1)
  pure idx

{-# INLINE addNodeFromLayout #-}
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
  writePrimArray (naArrTree a) (idx * 8 + 6) idx

{-# INLINE getParent #-}
getParent :: NodeArena -> NodeIdx -> IO NodeIdx
getParent na idx = arenaArrays na >>= \a -> readPrimArray (naArrTree a) (idx * 8)

{-# INLINE getFirstChild #-}
getFirstChild :: NodeArena -> NodeIdx -> IO NodeIdx
getFirstChild na idx = arenaArrays na >>= \a -> readPrimArray (naArrTree a) (idx * 8 + 1)

{-# INLINE getNextSibling #-}
getNextSibling :: NodeArena -> NodeIdx -> IO NodeIdx
getNextSibling na idx = arenaArrays na >>= \a -> readPrimArray (naArrTree a) (idx * 8 + 2)

{-# INLINE getChildCount #-}
getChildCount :: NodeArena -> NodeIdx -> IO Int
getChildCount na idx = arenaArrays na >>= \a -> readPrimArray (naArrTree a) (idx * 8 + 3)

{-# INLINE getNodeType #-}
getNodeType :: NodeArena -> NodeIdx -> IO NodeType
getNodeType na idx = arenaArrays na >>= \a -> readPrimArray (naArrTags a) (idx * 8) >>= pure . toEnum . fromIntegral

{-# INLINE getDirection #-}
getDirection :: NodeArena -> NodeIdx -> IO DirTag
getDirection na idx = arenaArrays na >>= \a -> readPrimArray (naArrTags a) (idx * 8 + 1) >>= pure . toEnum . fromIntegral

{-# INLINE getGridCols #-}
getGridCols :: NodeArena -> NodeIdx -> IO Int
getGridCols na idx = arenaArrays na >>= \a -> readPrimArray (naArrTree a) (idx * 8 + 7)

{-# INLINE setGridCols #-}
setGridCols :: NodeArena -> NodeIdx -> Int -> IO ()
setGridCols na idx c = arenaArrays na >>= \a -> writePrimArray (naArrTree a) (idx * 8 + 7) c

{-# INLINE getWidthSizing #-}
getWidthSizing :: NodeArena -> NodeIdx -> IO (SizingTag, Float)
getWidthSizing na idx = do
  a <- arenaArrays na
  tag <- readPrimArray (naArrTags a) (idx * 8 + 2)
  val <- readPrimArray (naArrStyle a) (idx * 16)
  pure (toEnum (fromIntegral tag), val)

{-# INLINE getHeightSizing #-}
getHeightSizing :: NodeArena -> NodeIdx -> IO (SizingTag, Float)
getHeightSizing na idx = do
  a <- arenaArrays na
  tag <- readPrimArray (naArrTags a) (idx * 8 + 3)
  val <- readPrimArray (naArrStyle a) (idx * 16 + 1)
  pure (toEnum (fromIntegral tag), val)

{-# INLINE getPadding #-}
getPadding :: NodeArena -> NodeIdx -> IO Padding
getPadding na idx = do
  a <- arenaArrays na
  let base = idx * 16
  l <- readPrimArray (naArrStyle a) (base + 2)
  r <- readPrimArray (naArrStyle a) (base + 3)
  t <- readPrimArray (naArrStyle a) (base + 4)
  b <- readPrimArray (naArrStyle a) (base + 5)
  pure (Padding l r t b)

{-# INLINE getGap #-}
getGap :: NodeArena -> NodeIdx -> IO Float
getGap na idx = arenaArrays na >>= \a -> readPrimArray (naArrStyle a) (idx * 16 + 6)

{-# INLINE getMinMax #-}
getMinMax :: NodeArena -> NodeIdx -> IO (Float, Float, Float, Float)
getMinMax na idx = do
  a <- arenaArrays na
  let base = idx * 16
  minW <- readPrimArray (naArrStyle a) (base + 7)
  minH <- readPrimArray (naArrStyle a) (base + 8)
  maxW <- readPrimArray (naArrStyle a) (base + 9)
  maxH <- readPrimArray (naArrStyle a) (base + 10)
  pure (minW, minH, maxW, maxH)

{-# INLINE getGrow #-}
getGrow :: NodeArena -> NodeIdx -> IO Float
getGrow na idx = arenaArrays na >>= \a -> readPrimArray (naArrStyle a) (idx * 16 + 11)

{-# INLINE getScrollContentW #-}
getScrollContentW :: NodeArena -> NodeIdx -> IO Float
getScrollContentW na idx = arenaArrays na >>= \a -> readPrimArray (naArrStyle a) (idx * 16 + 12)

{-# INLINE setScrollContentW #-}
setScrollContentW :: NodeArena -> NodeIdx -> Float -> IO ()
setScrollContentW na idx v = arenaArrays na >>= \a -> writePrimArray (naArrStyle a) (idx * 16 + 12) v

{-# INLINE getGridMinColW #-}
getGridMinColW :: NodeArena -> NodeIdx -> IO Float
getGridMinColW na idx = arenaArrays na >>= \a -> readPrimArray (naArrStyle a) (idx * 16 + 14)

{-# INLINE setGridMinColW #-}
setGridMinColW :: NodeArena -> NodeIdx -> Float -> IO ()
setGridMinColW na idx v = arenaArrays na >>= \a -> writePrimArray (naArrStyle a) (idx * 16 + 14) v

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
getAlignX na idx = do
  a <- arenaArrays na
  w <- readPrimArray (naArrTags a) (idx * 8 + 5)
  pure $
    case w of
      0 -> AlignStart
      1 -> AlignCenter
      2 -> AlignEnd
      _ -> AlignStart

{-# INLINE getAlignY #-}
getAlignY :: NodeArena -> NodeIdx -> IO AlignY
getAlignY na idx = do
  a <- arenaArrays na
  w <- readPrimArray (naArrTags a) (idx * 8 + 6)
  pure $
    case w of
      0 -> AlignTop
      1 -> AlignMiddle
      2 -> AlignBottom
      _ -> AlignTop

{-# INLINE getRect #-}
getRect :: NodeArena -> NodeIdx -> IO (Float, Float, Float, Float)
getRect na idx = do
  a <- arenaArrays na
  let base = idx * 10
  x <- readPrimArray (naArrGeom a) (base + 0)
  y <- readPrimArray (naArrGeom a) (base + 1)
  w <- readPrimArray (naArrGeom a) (base + 2)
  h <- readPrimArray (naArrGeom a) (base + 3)
  pure (x, y, w, h)

{-# INLINE setRect #-}
setRect :: NodeArena -> NodeIdx -> Float -> Float -> Float -> Float -> IO ()
setRect na idx x y w h = do
  a <- arenaArrays na
  let base = idx * 10
  writePrimArray (naArrGeom a) (base + 0) x
  writePrimArray (naArrGeom a) (base + 1) y
  writePrimArray (naArrGeom a) (base + 2) w
  writePrimArray (naArrGeom a) (base + 3) h

{-# INLINE getLayoutRect #-}
getLayoutRect :: NodeArena -> NodeIdx -> IO (Float, Float, Float, Float)
getLayoutRect na idx = do
  a <- arenaArrays na
  let base = idx * 10
  x <- readPrimArray (naArrGeom a) (base + 4)
  y <- readPrimArray (naArrGeom a) (base + 5)
  w <- readPrimArray (naArrGeom a) (base + 2)
  h <- readPrimArray (naArrGeom a) (base + 3)
  pure (x, y, w, h)

{-# INLINE getClipRect #-}
getClipRect :: NodeArena -> NodeIdx -> IO (Maybe Rect)
getClipRect na idx = do
  a <- arenaArrays na
  let base = idx * 10
  x <- readPrimArray (naArrGeom a) (base + 6)
  y <- readPrimArray (naArrGeom a) (base + 7)
  w <- readPrimArray (naArrGeom a) (base + 8)
  h <- readPrimArray (naArrGeom a) (base + 9)
  let r = Rect x y w h
  pure (if w > 0 && h > 0 then Just r else Nothing)

{-# INLINE setClipRect #-}
setClipRect :: NodeArena -> NodeIdx -> Rect -> IO ()
setClipRect na idx (Rect x y w h) = do
  a <- arenaArrays na
  let base = idx * 10
  writePrimArray (naArrGeom a) (base + 6) x
  writePrimArray (naArrGeom a) (base + 7) y
  writePrimArray (naArrGeom a) (base + 8) w
  writePrimArray (naArrGeom a) (base + 9) h

{-# INLINE snapshotLayoutRects #-}
snapshotLayoutRects :: NodeArena -> IO ()
snapshotLayoutRects na = do
  n <- arenaCount na
  a <- arenaArrays na
  let geom = naArrGeom a
      go !i
        | i >= n = pure ()
        | otherwise = do
            let base = i * 10
            x <- readPrimArray geom (base + 0)
            y <- readPrimArray geom (base + 1)
            writePrimArray geom (base + 4) x
            writePrimArray geom (base + 5) y
            go (i + 1)
  go 0

{-# INLINE getText #-}
getText :: NodeArena -> NodeIdx -> IO Text
getText na idx = do
  a <- arenaArrays na
  ti <- readPrimArray (naArrTree a) (idx * 8 + 6)
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
getWidgetId na idx = do
  a <- arenaArrays na
  w <- readPrimArray (naArrTree a) (idx * 8 + 4)
  pure (WidgetId (fromIntegral w))

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
  writePrimArray (naArrTree a) (idx * 8 + 4) (fromIntegral w)
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
getNodeValue na idx = arenaArrays na >>= \a -> readPrimArray (naArrStyle a) (idx * 16 + 13)

{-# INLINE setNodeValue #-}
setNodeValue :: NodeArena -> NodeIdx -> Float -> IO ()
setNodeValue na idx v = arenaArrays na >>= \a -> writePrimArray (naArrStyle a) (idx * 16 + 13) v

{-# INLINE getNodeFontSize #-}
getNodeFontSize :: NodeArena -> NodeIdx -> IO Float
getNodeFontSize na idx = arenaArrays na >>= \a -> readPrimArray (naArrStyle a) (idx * 16 + 15)

{-# INLINE setNodeFontSize #-}
setNodeFontSize :: NodeArena -> NodeIdx -> Float -> IO ()
setNodeFontSize na idx v = arenaArrays na >>= \a -> writePrimArray (naArrStyle a) (idx * 16 + 15) v

{-# INLINE getNodeFontColor #-}
getNodeFontColor :: NodeArena -> NodeIdx -> IO (Maybe Color)
getNodeFontColor na idx = do
  a <- arenaArrays na
  val <- readPrimArray (naArrTree a) (idx * 8 + 7)
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
  writePrimArray (naArrTree a) (idx * 8 + 7) val

{-# INLINE getStyleIdx #-}
getStyleIdx :: NodeArena -> NodeIdx -> IO Int
getStyleIdx na idx = arenaArrays na >>= \a -> readPrimArray (naArrTree a) (idx * 8 + 5)

{-# INLINE setStyleIdx #-}
setStyleIdx :: NodeArena -> NodeIdx -> Int -> IO ()
setStyleIdx na idx v = arenaArrays na >>= \a -> writePrimArray (naArrTree a) (idx * 8 + 5) v

-- | Get the snapshot buffers for a recursion depth, grown to hold at least
-- @needed@ entries. Buffers are reused across frames; nothing is allocated in
-- steady state once capacity is warm.
{-# NOINLINE ensureAxisSnapshot #-}
ensureAxisSnapshot :: NodeArena -> Int -> Int -> IO AxisSnapshot
ensureAxisSnapshot na depth needed = do
  cap <- readIORef (naSnapCap na)
  if needed <= cap
    then getLevel depth
    else do
      let newCap = max needed (cap * 2)
      levels <- readIORef (naSnapLevels na)
      levels' <- traverse (growSnap cap newCap) levels
      writeIORef (naSnapLevels na) levels'
      writeIORef (naSnapCap na) newCap
      getLevel depth
  where
    getLevel d = do
      levels <- readIORef (naSnapLevels na)
      case IM.lookup d levels of
        Just s -> pure s
        Nothing -> do
          cap' <- readIORef (naSnapCap na)
          asIdx <- newPrimArray cap'
          asOut <- newPrimArray cap'
          let s = AxisSnapshot {..}
          writeIORef (naSnapLevels na) (IM.insert d s levels)
          pure s

    growSnap cap newCap (AxisSnapshot idx out) = do
      idx' <- growPrimArrayCopy idx cap newCap 0
      out' <- growPrimArrayCopy out cap newCap 0
      pure AxisSnapshot {asIdx = idx', asOut = out'}

-- | Look up a wrapped text size memoized for this frame at @(node, wrapW)@.
-- Wrap widths are quantized to 0.25 px so near-identical reflows still hit.
{-# INLINE lookupWrapMemo #-}
lookupWrapMemo :: NodeArena -> NodeIdx -> Float -> IO (Maybe (Float, Float))
lookupWrapMemo na idx wrapW = do
  ft <- readIORef (naFrameTag na)
  tagArr <- readIORef (naWrapTag na)
  tag <- readPrimArray tagArr idx
  if tag /= ft
    then pure Nothing
    else do
      keyArr <- readIORef (naWrapKey na)
      key <- readPrimArray keyArr idx
      if abs (key - wrapW) <= 0.25
        then do
          wArr <- readIORef (naWrapW na)
          hArr <- readIORef (naWrapH na)
          w <- readPrimArray wArr idx
          h <- readPrimArray hArr idx
          pure (Just (w, h))
        else pure Nothing

{-# INLINE storeWrapMemo #-}
storeWrapMemo :: NodeArena -> NodeIdx -> Float -> Float -> Float -> IO ()
storeWrapMemo na idx wrapW w h = do
  ft <- readIORef (naFrameTag na)
  tagArr <- readIORef (naWrapTag na)
  keyArr <- readIORef (naWrapKey na)
  wArr <- readIORef (naWrapW na)
  hArr <- readIORef (naWrapH na)
  writePrimArray tagArr idx ft
  writePrimArray keyArr idx wrapW
  writePrimArray wArr idx w
  writePrimArray hArr idx h

{-# NOINLINE ensureScratchCapacity #-}
ensureScratchCapacity :: NodeArena -> Int -> IO ()
ensureScratchCapacity na needed = do
  cap <- readIORef (naScratchCap na)
  if needed <= cap
    then pure ()
    else do
      let newCap = max needed (cap * 2)
      growPrimArray (naScratchIdx na) cap newCap (-1)
      growPrimArray (naScratchMain na) cap newCap 0
      growPrimArray (naScratchCross na) cap newCap 0
      growPrimArray (naScratchOutMain na) cap newCap 0
      growPrimArray (naScratchOutCross na) cap newCap 0
      writeIORef (naScratchCap na) newCap

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
