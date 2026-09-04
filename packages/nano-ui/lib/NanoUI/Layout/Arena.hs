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
  , getWidthSizing
  , getHeightSizing
  , getPadding
  , getGap
  , getMinMax
  , getGrow
  , getAspect
  , setAspect
  , getWrap
  , parentIsNonWrapRow
  , getAlignX
  , getAlignY
  , getRect
  , setRect
  , getLayoutRect
  , getClipRect
  , setClipRect
  , snapshotLayoutRects
  , getText
  , getWidgetId
  , setWidgetId
  , lookupNodeByKey
  , getStyleIdx
  , setStyleIdx
  , getNodeValue
  , setNodeValue
  , ensureScratchCapacity
  , forNodes_
  , forChildNodes_
  , findNodeRevM
  , foldChildNodesM
  , foldNodeRevM
  ) where

import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Data.HashTable.IO (BasicHashTable)
import qualified Data.HashTable.IO as HT
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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
import Data.Word (Word8)
import qualified Data.Text as T
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Style (AlignX (..), AlignY (..), Direction (..), Layout (..), Padding (..), Sizing (..))
import NanoUI.Types (Rect (..))

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
    NodeImage -> True
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
isScrollNode nt =
  case nt of
    NodeScrollContainer -> True
    NodeModal -> True
    _ -> False

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
  { naArrParent :: !(MutablePrimArray RealWorld Int)
  , naArrFirstChild :: !(MutablePrimArray RealWorld Int)
  , naArrNextSibling :: !(MutablePrimArray RealWorld Int)
  , naArrChildCount :: !(MutablePrimArray RealWorld Int)
  , naArrNodeType :: !(MutablePrimArray RealWorld Word8)
  , naArrDirection :: !(MutablePrimArray RealWorld Word8)
  , naArrWidthSizing :: !(MutablePrimArray RealWorld Word8)
  , naArrHeightSizing :: !(MutablePrimArray RealWorld Word8)
  , naArrWidthValue :: !(MutablePrimArray RealWorld Float)
  , naArrHeightValue :: !(MutablePrimArray RealWorld Float)
  , naArrPadL :: !(MutablePrimArray RealWorld Float)
  , naArrPadR :: !(MutablePrimArray RealWorld Float)
  , naArrPadT :: !(MutablePrimArray RealWorld Float)
  , naArrPadB :: !(MutablePrimArray RealWorld Float)
  , naArrGap :: !(MutablePrimArray RealWorld Float)
  , naArrMinW :: !(MutablePrimArray RealWorld Float)
  , naArrMinH :: !(MutablePrimArray RealWorld Float)
  , naArrMaxW :: !(MutablePrimArray RealWorld Float)
  , naArrMaxH :: !(MutablePrimArray RealWorld Float)
  , naArrGrow :: !(MutablePrimArray RealWorld Float)
  , naArrAspect :: !(MutablePrimArray RealWorld Float)
  , naArrWrap :: !(MutablePrimArray RealWorld Word8)
  , naArrAlignX :: !(MutablePrimArray RealWorld Word8)
  , naArrAlignY :: !(MutablePrimArray RealWorld Word8)
  , naArrX :: !(MutablePrimArray RealWorld Float)
  , naArrY :: !(MutablePrimArray RealWorld Float)
  , naArrLayoutX :: !(MutablePrimArray RealWorld Float)
  , naArrLayoutY :: !(MutablePrimArray RealWorld Float)
  , naArrClipX :: !(MutablePrimArray RealWorld Float)
  , naArrClipY :: !(MutablePrimArray RealWorld Float)
  , naArrClipW :: !(MutablePrimArray RealWorld Float)
  , naArrClipH :: !(MutablePrimArray RealWorld Float)
  , naArrW :: !(MutablePrimArray RealWorld Float)
  , naArrH :: !(MutablePrimArray RealWorld Float)
  , naArrWidgetId :: !(MutablePrimArray RealWorld WidgetId)
  , naArrValue :: !(MutablePrimArray RealWorld Float)
  , naArrStyleIdx :: !(MutablePrimArray RealWorld Int)
  , naArrTextIdx :: !(MutablePrimArray RealWorld Int)
  , naArrGridCols :: !(MutablePrimArray RealWorld Int)
  , naArrTextStore :: !(MutableArray RealWorld Text)
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
  , naIndex :: IORef (BasicHashTable WidgetId NodeIdx)
  }

initialCapacity :: Int
initialCapacity = 256

newNodeArenaArrays :: Int -> IO NodeArenaArrays
newNodeArenaArrays cap = do
  naArrParent <- newPrimArray cap
  naArrFirstChild <- newPrimArray cap
  naArrNextSibling <- newPrimArray cap
  naArrChildCount <- newPrimArray cap
  naArrNodeType <- newPrimArray cap
  naArrDirection <- newPrimArray cap
  naArrWidthSizing <- newPrimArray cap
  naArrHeightSizing <- newPrimArray cap
  naArrWidthValue <- newPrimArray cap
  naArrHeightValue <- newPrimArray cap
  naArrPadL <- newPrimArray cap
  naArrPadR <- newPrimArray cap
  naArrPadT <- newPrimArray cap
  naArrPadB <- newPrimArray cap
  naArrGap <- newPrimArray cap
  naArrMinW <- newPrimArray cap
  naArrMinH <- newPrimArray cap
  naArrMaxW <- newPrimArray cap
  naArrMaxH <- newPrimArray cap
  naArrGrow <- newPrimArray cap
  naArrAspect <- newPrimArray cap
  naArrWrap <- newPrimArray cap
  naArrAlignX <- newPrimArray cap
  naArrAlignY <- newPrimArray cap
  naArrX <- newPrimArray cap
  naArrY <- newPrimArray cap
  naArrLayoutX <- newPrimArray cap
  naArrLayoutY <- newPrimArray cap
  naArrClipX <- newPrimArray cap
  naArrClipY <- newPrimArray cap
  naArrClipW <- newPrimArray cap
  naArrClipH <- newPrimArray cap
  naArrW <- newPrimArray cap
  naArrH <- newPrimArray cap
  naArrWidgetId <- newPrimArray cap
  naArrValue <- newPrimArray cap
  naArrStyleIdx <- newPrimArray cap
  naArrTextIdx <- newPrimArray cap
  naArrGridCols <- newPrimArray cap
  setPrimArray naArrGridCols 0 cap 0
  naArrTextStore <- newArray cap T.empty
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
      , naIndex
      }

{-# INLINE resetNodeArena #-}
resetNodeArena :: NodeArena -> IO ()
resetNodeArena na = do
  table <- readIORef (naIndex na)
  n <- readIORef (naCount na)
  writeIORef (naCount na) 0
  clearWidgetIndex na table n

clearWidgetIndex :: NodeArena -> BasicHashTable WidgetId NodeIdx -> Int -> IO ()
clearWidgetIndex na table n = do
  a <- arenaArrays na
  let go !i
        | i >= n = pure ()
        | otherwise = do
            wid <- readPrimArray (naArrWidgetId a) i
            when (hashWidgetId wid /= 0) $ HT.delete table wid
            go (i + 1)
  go 0

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
      naArrParent <- growPrimArrayCopy (naArrParent a) cap newCap (-1)
      naArrFirstChild <- growPrimArrayCopy (naArrFirstChild a) cap newCap (-1)
      naArrNextSibling <- growPrimArrayCopy (naArrNextSibling a) cap newCap (-1)
      naArrChildCount <- growPrimArrayCopy (naArrChildCount a) cap newCap 0
      naArrNodeType <- growPrimArrayCopy (naArrNodeType a) cap newCap 0
      naArrDirection <- growPrimArrayCopy (naArrDirection a) cap newCap 0
      naArrWidthSizing <- growPrimArrayCopy (naArrWidthSizing a) cap newCap 0
      naArrHeightSizing <- growPrimArrayCopy (naArrHeightSizing a) cap newCap 0
      naArrWidthValue <- growPrimArrayCopy (naArrWidthValue a) cap newCap 0
      naArrHeightValue <- growPrimArrayCopy (naArrHeightValue a) cap newCap 0
      naArrPadL <- growPrimArrayCopy (naArrPadL a) cap newCap 0
      naArrPadR <- growPrimArrayCopy (naArrPadR a) cap newCap 0
      naArrPadT <- growPrimArrayCopy (naArrPadT a) cap newCap 0
      naArrPadB <- growPrimArrayCopy (naArrPadB a) cap newCap 0
      naArrGap <- growPrimArrayCopy (naArrGap a) cap newCap 0
      naArrMinW <- growPrimArrayCopy (naArrMinW a) cap newCap 0
      naArrMinH <- growPrimArrayCopy (naArrMinH a) cap newCap 0
      naArrMaxW <- growPrimArrayCopy (naArrMaxW a) cap newCap 1e9
      naArrMaxH <- growPrimArrayCopy (naArrMaxH a) cap newCap 1e9
      naArrGrow <- growPrimArrayCopy (naArrGrow a) cap newCap 0
      naArrAspect <- growPrimArrayCopy (naArrAspect a) cap newCap 0
      naArrWrap <- growPrimArrayCopy (naArrWrap a) cap newCap 0
      naArrAlignX <- growPrimArrayCopy (naArrAlignX a) cap newCap 0
      naArrAlignY <- growPrimArrayCopy (naArrAlignY a) cap newCap 0
      naArrX <- growPrimArrayCopy (naArrX a) cap newCap 0
      naArrY <- growPrimArrayCopy (naArrY a) cap newCap 0
      naArrLayoutX <- growPrimArrayCopy (naArrLayoutX a) cap newCap 0
      naArrLayoutY <- growPrimArrayCopy (naArrLayoutY a) cap newCap 0
      naArrClipX <- growPrimArrayCopy (naArrClipX a) cap newCap 0
      naArrClipY <- growPrimArrayCopy (naArrClipY a) cap newCap 0
      naArrClipW <- growPrimArrayCopy (naArrClipW a) cap newCap 0
      naArrClipH <- growPrimArrayCopy (naArrClipH a) cap newCap 0
      naArrW <- growPrimArrayCopy (naArrW a) cap newCap 0
      naArrH <- growPrimArrayCopy (naArrH a) cap newCap 0
      naArrWidgetId <- growPrimArrayCopy (naArrWidgetId a) cap newCap (WidgetId 0)
      naArrValue <- growPrimArrayCopy (naArrValue a) cap newCap 0
      naArrStyleIdx <- growPrimArrayCopy (naArrStyleIdx a) cap newCap 0
      naArrTextIdx <- growPrimArrayCopy (naArrTextIdx a) cap newCap (-1)
      naArrGridCols <- growPrimArrayCopy (naArrGridCols a) cap newCap 0
      naArrTextStore <- growTextStoreCopy (naArrTextStore a) cap newCap
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

{-# NOINLINE growTextStoreCopy #-}
growTextStoreCopy :: MutableArray RealWorld Text -> Int -> Int -> IO (MutableArray RealWorld Text)
growTextStoreCopy arr oldCap newCap = do
  newArr <- newArray newCap T.empty
  forM_ [0 .. oldCap - 1] $ \i ->
    readArray arr i >>= writeArray newArr i
  forM_ [oldCap .. newCap - 1] $ \i ->
    writeArray newArr i T.empty
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
  Bool ->
  IO NodeIdx
addNode na nt parent dir wSiz hSiz pad gap minW minH maxW maxH grow ax ay wrap = do
  idx <- readIORef (naCount na)
  ensureCapacity na (idx + 1)
  let (wTag, wVal) = sizingTag wSiz
      (hTag, hVal) = sizingTag hSiz
  a <- arenaArrays na
  writePrimArray (naArrParent a) idx parent
  writePrimArray (naArrNodeType a) idx (fromIntegral (fromEnum nt))
  writePrimArray (naArrDirection a) idx (fromIntegral (fromEnum (dirTag dir)))
  writePrimArray (naArrWidthSizing a) idx (fromIntegral (fromEnum wTag))
  writePrimArray (naArrHeightSizing a) idx (fromIntegral (fromEnum hTag))
  writePrimArray (naArrWidthValue a) idx wVal
  writePrimArray (naArrHeightValue a) idx hVal
  writePrimArray (naArrPadL a) idx (padL pad)
  writePrimArray (naArrPadR a) idx (padR pad)
  writePrimArray (naArrPadT a) idx (padT pad)
  writePrimArray (naArrPadB a) idx (padB pad)
  writePrimArray (naArrGap a) idx gap
  writePrimArray (naArrMinW a) idx minW
  writePrimArray (naArrMinH a) idx minH
  writePrimArray (naArrMaxW a) idx maxW
  writePrimArray (naArrMaxH a) idx maxH
  writePrimArray (naArrGrow a) idx grow
  writePrimArray (naArrAspect a) idx 0
  writePrimArray (naArrWrap a) idx (if wrap then 1 else 0)
  writePrimArray (naArrAlignX a) idx (alignXTag ax)
  writePrimArray (naArrAlignY a) idx (alignYTag ay)
  writePrimArray (naArrX a) idx 0
  writePrimArray (naArrY a) idx 0
  writePrimArray (naArrLayoutX a) idx 0
  writePrimArray (naArrLayoutY a) idx 0
  writePrimArray (naArrClipX a) idx 0
  writePrimArray (naArrClipY a) idx 0
  writePrimArray (naArrClipW a) idx 0
  writePrimArray (naArrClipH a) idx 0
  writePrimArray (naArrW a) idx 0
  writePrimArray (naArrH a) idx 0
  writePrimArray (naArrWidgetId a) idx (WidgetId 0)
  writePrimArray (naArrValue a) idx 0
  writePrimArray (naArrStyleIdx a) idx 0
  writePrimArray (naArrTextIdx a) idx (-1)
  writePrimArray (naArrGridCols a) idx 0
  writePrimArray (naArrFirstChild a) idx (-1)
  writePrimArray (naArrNextSibling a) idx (-1)
  writePrimArray (naArrChildCount a) idx 0
  if parent >= 0
    then do
      fc <- readPrimArray (naArrFirstChild a) parent
      writePrimArray (naArrNextSibling a) idx fc
      writePrimArray (naArrFirstChild a) parent idx
      cc <- readPrimArray (naArrChildCount a) parent
      writePrimArray (naArrChildCount a) parent (cc + 1)
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
      (layoutWrap l)
  setAspect na idx (layoutAspect l)
  setGridCols na idx (layoutGridCols l)
  pure idx

{-# INLINE setNodeText #-}
setNodeText :: NodeArena -> NodeIdx -> Text -> IO ()
setNodeText na idx txt = do
  a <- arenaArrays na
  writeArray (naArrTextStore a) idx txt
  writePrimArray (naArrTextIdx a) idx idx

{-# INLINE getParent #-}
getParent :: NodeArena -> NodeIdx -> IO NodeIdx
getParent na idx = arenaArrays na >>= \a -> readPrimArray (naArrParent a) idx

{-# INLINE getFirstChild #-}
getFirstChild :: NodeArena -> NodeIdx -> IO NodeIdx
getFirstChild na idx = arenaArrays na >>= \a -> readPrimArray (naArrFirstChild a) idx

{-# INLINE getNextSibling #-}
getNextSibling :: NodeArena -> NodeIdx -> IO NodeIdx
getNextSibling na idx = arenaArrays na >>= \a -> readPrimArray (naArrNextSibling a) idx

{-# INLINE getChildCount #-}
getChildCount :: NodeArena -> NodeIdx -> IO Int
getChildCount na idx = arenaArrays na >>= \a -> readPrimArray (naArrChildCount a) idx

{-# INLINE getNodeType #-}
getNodeType :: NodeArena -> NodeIdx -> IO NodeType
getNodeType na idx = arenaArrays na >>= \a -> readPrimArray (naArrNodeType a) idx >>= pure . toEnum . fromIntegral

{-# INLINE getDirection #-}
getDirection :: NodeArena -> NodeIdx -> IO DirTag
getDirection na idx = arenaArrays na >>= \a -> readPrimArray (naArrDirection a) idx >>= pure . toEnum . fromIntegral

{-# INLINE getGridCols #-}
getGridCols :: NodeArena -> NodeIdx -> IO Int
getGridCols na idx = arenaArrays na >>= \a -> readPrimArray (naArrGridCols a) idx

{-# INLINE setGridCols #-}
setGridCols :: NodeArena -> NodeIdx -> Int -> IO ()
setGridCols na idx c = arenaArrays na >>= \a -> writePrimArray (naArrGridCols a) idx c

{-# INLINE getWidthSizing #-}
getWidthSizing :: NodeArena -> NodeIdx -> IO (SizingTag, Float)
getWidthSizing na idx = do
  a <- arenaArrays na
  tag <- readPrimArray (naArrWidthSizing a) idx
  val <- readPrimArray (naArrWidthValue a) idx
  pure (toEnum (fromIntegral tag), val)

{-# INLINE getHeightSizing #-}
getHeightSizing :: NodeArena -> NodeIdx -> IO (SizingTag, Float)
getHeightSizing na idx = do
  a <- arenaArrays na
  tag <- readPrimArray (naArrHeightSizing a) idx
  val <- readPrimArray (naArrHeightValue a) idx
  pure (toEnum (fromIntegral tag), val)

{-# INLINE getPadding #-}
getPadding :: NodeArena -> NodeIdx -> IO Padding
getPadding na idx = do
  a <- arenaArrays na
  l <- readPrimArray (naArrPadL a) idx
  r <- readPrimArray (naArrPadR a) idx
  t <- readPrimArray (naArrPadT a) idx
  b <- readPrimArray (naArrPadB a) idx
  pure (Padding l r t b)

{-# INLINE getGap #-}
getGap :: NodeArena -> NodeIdx -> IO Float
getGap na idx = arenaArrays na >>= \a -> readPrimArray (naArrGap a) idx

{-# INLINE getMinMax #-}
getMinMax :: NodeArena -> NodeIdx -> IO (Float, Float, Float, Float)
getMinMax na idx = do
  a <- arenaArrays na
  minW <- readPrimArray (naArrMinW a) idx
  minH <- readPrimArray (naArrMinH a) idx
  maxW <- readPrimArray (naArrMaxW a) idx
  maxH <- readPrimArray (naArrMaxH a) idx
  pure (minW, minH, maxW, maxH)

{-# INLINE getGrow #-}
getGrow :: NodeArena -> NodeIdx -> IO Float
getGrow na idx = arenaArrays na >>= \a -> readPrimArray (naArrGrow a) idx

{-# INLINE getAspect #-}
getAspect :: NodeArena -> NodeIdx -> IO Float
getAspect na idx = arenaArrays na >>= \a -> readPrimArray (naArrAspect a) idx

{-# INLINE setAspect #-}
setAspect :: NodeArena -> NodeIdx -> Float -> IO ()
setAspect na idx v = arenaArrays na >>= \a -> writePrimArray (naArrAspect a) idx v

{-# INLINE getWrap #-}
getWrap :: NodeArena -> NodeIdx -> IO Bool
getWrap na idx = arenaArrays na >>= \a -> readPrimArray (naArrWrap a) idx >>= pure . (/= 0)

{-# INLINE parentIsNonWrapRow #-}
parentIsNonWrapRow :: NodeArena -> NodeIdx -> IO Bool
parentIsNonWrapRow na idx = do
  p <- getParent na idx
  if p < 0
    then pure False
    else do
      dir <- getDirection na p
      w <- getWrap na p
      pure (dir == DirRow && not w)

{-# INLINE getAlignX #-}
getAlignX :: NodeArena -> NodeIdx -> IO AlignX
getAlignX na idx = do
  a <- arenaArrays na
  w <- readPrimArray (naArrAlignX a) idx
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
  w <- readPrimArray (naArrAlignY a) idx
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
  x <- readPrimArray (naArrX a) idx
  y <- readPrimArray (naArrY a) idx
  w <- readPrimArray (naArrW a) idx
  h <- readPrimArray (naArrH a) idx
  pure (x, y, w, h)

{-# INLINE setRect #-}
setRect :: NodeArena -> NodeIdx -> Float -> Float -> Float -> Float -> IO ()
setRect na idx x y w h = do
  a <- arenaArrays na
  writePrimArray (naArrX a) idx x
  writePrimArray (naArrY a) idx y
  writePrimArray (naArrW a) idx w
  writePrimArray (naArrH a) idx h

{-# INLINE getLayoutRect #-}
getLayoutRect :: NodeArena -> NodeIdx -> IO (Float, Float, Float, Float)
getLayoutRect na idx = do
  a <- arenaArrays na
  x <- readPrimArray (naArrLayoutX a) idx
  y <- readPrimArray (naArrLayoutY a) idx
  w <- readPrimArray (naArrW a) idx
  h <- readPrimArray (naArrH a) idx
  pure (x, y, w, h)

{-# INLINE getClipRect #-}
getClipRect :: NodeArena -> NodeIdx -> IO (Maybe Rect)
getClipRect na idx = do
  a <- arenaArrays na
  x <- readPrimArray (naArrClipX a) idx
  y <- readPrimArray (naArrClipY a) idx
  w <- readPrimArray (naArrClipW a) idx
  h <- readPrimArray (naArrClipH a) idx
  let r = Rect x y w h
  pure (if w > 0 && h > 0 then Just r else Nothing)

{-# INLINE setClipRect #-}
setClipRect :: NodeArena -> NodeIdx -> Rect -> IO ()
setClipRect na idx (Rect x y w h) = do
  a <- arenaArrays na
  writePrimArray (naArrClipX a) idx x
  writePrimArray (naArrClipY a) idx y
  writePrimArray (naArrClipW a) idx w
  writePrimArray (naArrClipH a) idx h

{-# INLINE snapshotLayoutRects #-}
snapshotLayoutRects :: NodeArena -> IO ()
snapshotLayoutRects na = do
  n <- arenaCount na
  a <- arenaArrays na
  let go !i
        | i >= n = pure ()
        | otherwise = do
            x <- readPrimArray (naArrX a) i
            y <- readPrimArray (naArrY a) i
            writePrimArray (naArrLayoutX a) i x
            writePrimArray (naArrLayoutY a) i y
            go (i + 1)
  go 0

{-# INLINE getText #-}
getText :: NodeArena -> NodeIdx -> IO Text
getText na idx = do
  a <- arenaArrays na
  ti <- readPrimArray (naArrTextIdx a) idx
  if ti < 0
    then pure T.empty
    else readArray (naArrTextStore a) ti

{-# INLINE getWidgetId #-}
getWidgetId :: NodeArena -> NodeIdx -> IO WidgetId
getWidgetId na idx = arenaArrays na >>= \a -> readPrimArray (naArrWidgetId a) idx

{-# INLINE setWidgetId #-}
setWidgetId :: NodeArena -> NodeIdx -> WidgetId -> IO ()
setWidgetId na idx wid = do
  a <- arenaArrays na
  writePrimArray (naArrWidgetId a) idx wid
  when (hashWidgetId wid /= 0) $ do
    table <- readIORef (naIndex na)
    HT.insert table wid idx

{-# INLINE lookupNodeByKey #-}
lookupNodeByKey :: NodeArena -> Int -> IO (Maybe NodeIdx)
lookupNodeByKey na key
  | key == 0 = pure Nothing
  | otherwise = do
      table <- readIORef (naIndex na)
      HT.lookup table (WidgetId (fromIntegral key))

{-# INLINE getNodeValue #-}
getNodeValue :: NodeArena -> NodeIdx -> IO Float
getNodeValue na idx = arenaArrays na >>= \a -> readPrimArray (naArrValue a) idx

{-# INLINE setNodeValue #-}
setNodeValue :: NodeArena -> NodeIdx -> Float -> IO ()
setNodeValue na idx v = arenaArrays na >>= \a -> writePrimArray (naArrValue a) idx v

{-# INLINE getStyleIdx #-}
getStyleIdx :: NodeArena -> NodeIdx -> IO Int
getStyleIdx na idx = arenaArrays na >>= \a -> readPrimArray (naArrStyleIdx a) idx

{-# INLINE setStyleIdx #-}
setStyleIdx :: NodeArena -> NodeIdx -> Int -> IO ()
setStyleIdx na idx v = arenaArrays na >>= \a -> writePrimArray (naArrStyleIdx a) idx v

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

{-# INLINE foldChildNodesM #-}
foldChildNodesM :: NodeArena -> NodeIdx -> (a -> NodeIdx -> IO a) -> a -> IO a
foldChildNodesM na parentIdx f z = do
  fc <- getFirstChild na parentIdx
  let go !ci !acc
        | ci < 0 = pure acc
        | otherwise = do
            acc' <- f acc ci
            ns <- getNextSibling na ci
            go ns acc'
  go fc z

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
