module NanoUI.Layout.Arena
  ( NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , DirTag (..)
  , NodeArena (..)
  , newNodeArena
  , resetNodeArena
  , arenaCount
  , addNode
  , setNodeText
  , getParent
  , getFirstChild
  , getNextSibling
  , getChildCount
  , getNodeType
  , getDirection
  , getWidthSizing
  , getHeightSizing
  , getPadding
  , getGap
  , getMinMax
  , getGrow
  , getAlignX
  , getAlignY
  , getRect
  , setRect
  , getText
  , getWidgetId
  , setWidgetId
  , getStyleIdx
  , setStyleIdx
  ) where

import Control.Monad (forM_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Primitive.Array (MutableArray, newArray, readArray, writeArray)
import Data.Primitive.PrimArray (MutablePrimArray, newPrimArray, readPrimArray, writePrimArray)
import GHC.Exts (RealWorld)
import Data.Text (Text)
import Data.Word (Word8, Word64)
import qualified Data.Text as T
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Style (AlignX (..), AlignY (..), Direction (..), Padding (..), Sizing (..))

type NodeIdx = Int

data NodeType
  = NodeContainer
  | NodeText
  | NodeSpacer
  | NodeSeparator
  | NodeWidget
  deriving (Eq, Show, Enum, Bounded)

data SizingTag
  = SizingFixed
  | SizingFit
  | SizingGrow
  | SizingPercent
  deriving (Eq, Show, Enum, Bounded)

data DirTag = DirRow | DirColumn
  deriving (Eq, Show, Enum, Bounded)

data NodeArena = NodeArena
  { naCount :: IORef Int
  , naCapacity :: IORef Int
  , naParent :: IORef (MutablePrimArray RealWorld Int)
  , naFirstChild :: IORef (MutablePrimArray RealWorld Int)
  , naNextSibling :: IORef (MutablePrimArray RealWorld Int)
  , naChildCount :: IORef (MutablePrimArray RealWorld Int)
  , naNodeType :: IORef (MutablePrimArray RealWorld Word8)
  , naDirection :: IORef (MutablePrimArray RealWorld Word8)
  , naWidthSizing :: IORef (MutablePrimArray RealWorld Word8)
  , naHeightSizing :: IORef (MutablePrimArray RealWorld Word8)
  , naWidthValue :: IORef (MutablePrimArray RealWorld Float)
  , naHeightValue :: IORef (MutablePrimArray RealWorld Float)
  , naPadL :: IORef (MutablePrimArray RealWorld Float)
  , naPadR :: IORef (MutablePrimArray RealWorld Float)
  , naPadT :: IORef (MutablePrimArray RealWorld Float)
  , naPadB :: IORef (MutablePrimArray RealWorld Float)
  , naGap :: IORef (MutablePrimArray RealWorld Float)
  , naMinW :: IORef (MutablePrimArray RealWorld Float)
  , naMinH :: IORef (MutablePrimArray RealWorld Float)
  , naMaxW :: IORef (MutablePrimArray RealWorld Float)
  , naMaxH :: IORef (MutablePrimArray RealWorld Float)
  , naGrow :: IORef (MutablePrimArray RealWorld Float)
  , naAlignX :: IORef (MutablePrimArray RealWorld Word8)
  , naAlignY :: IORef (MutablePrimArray RealWorld Word8)
  , naX :: IORef (MutablePrimArray RealWorld Float)
  , naY :: IORef (MutablePrimArray RealWorld Float)
  , naW :: IORef (MutablePrimArray RealWorld Float)
  , naH :: IORef (MutablePrimArray RealWorld Float)
  , naWidgetId :: IORef (MutablePrimArray RealWorld Word64)
  , naStyleIdx :: IORef (MutablePrimArray RealWorld Int)
  , naTextStore :: IORef (MutableArray RealWorld Text)
  , naTextIdx :: IORef (MutablePrimArray RealWorld Int)
  }

initialCapacity :: Int
initialCapacity = 256

{-# INLINE newNodeArena #-}
newNodeArena :: IO NodeArena
newNodeArena = do
  let cap = initialCapacity
  naCount <- newIORef 0
  naCapacity <- newIORef cap
  naParent <- newIORef =<< newPrimArray cap
  naFirstChild <- newIORef =<< newPrimArray cap
  naNextSibling <- newIORef =<< newPrimArray cap
  naChildCount <- newIORef =<< newPrimArray cap
  naNodeType <- newIORef =<< newPrimArray cap
  naDirection <- newIORef =<< newPrimArray cap
  naWidthSizing <- newIORef =<< newPrimArray cap
  naHeightSizing <- newIORef =<< newPrimArray cap
  naWidthValue <- newIORef =<< newPrimArray cap
  naHeightValue <- newIORef =<< newPrimArray cap
  naPadL <- newIORef =<< newPrimArray cap
  naPadR <- newIORef =<< newPrimArray cap
  naPadT <- newIORef =<< newPrimArray cap
  naPadB <- newIORef =<< newPrimArray cap
  naGap <- newIORef =<< newPrimArray cap
  naMinW <- newIORef =<< newPrimArray cap
  naMinH <- newIORef =<< newPrimArray cap
  naMaxW <- newIORef =<< newPrimArray cap
  naMaxH <- newIORef =<< newPrimArray cap
  naGrow <- newIORef =<< newPrimArray cap
  naAlignX <- newIORef =<< newPrimArray cap
  naAlignY <- newIORef =<< newPrimArray cap
  naX <- newIORef =<< newPrimArray cap
  naY <- newIORef =<< newPrimArray cap
  naW <- newIORef =<< newPrimArray cap
  naH <- newIORef =<< newPrimArray cap
  naWidgetId <- newIORef =<< newPrimArray cap
  naStyleIdx <- newIORef =<< newPrimArray cap
  naTextStore <- newIORef =<< newArray cap T.empty
  naTextIdx <- newIORef =<< newPrimArray cap
  pure
    NodeArena
      { naCount
      , naCapacity
      , naParent
      , naFirstChild
      , naNextSibling
      , naChildCount
      , naNodeType
      , naDirection
      , naWidthSizing
      , naHeightSizing
      , naWidthValue
      , naHeightValue
      , naPadL
      , naPadR
      , naPadT
      , naPadB
      , naGap
      , naMinW
      , naMinH
      , naMaxW
      , naMaxH
      , naGrow
      , naAlignX
      , naAlignY
      , naX
      , naY
      , naW
      , naH
      , naWidgetId
      , naStyleIdx
      , naTextStore
      , naTextIdx
      }

{-# INLINE resetNodeArena #-}
resetNodeArena :: NodeArena -> IO ()
resetNodeArena na = writeIORef (naCount na) 0

{-# INLINE arenaCount #-}
arenaCount :: NodeArena -> IO Int
arenaCount na = readIORef (naCount na)

writeInt :: IORef (MutablePrimArray RealWorld Int) -> Int -> Int -> IO ()
writeInt mv i v = readIORef mv >>= \arr -> writePrimArray arr i v

readInt :: IORef (MutablePrimArray RealWorld Int) -> Int -> IO Int
readInt mv i = readIORef mv >>= \arr -> readPrimArray arr i

writeWord8 :: IORef (MutablePrimArray RealWorld Word8) -> Int -> Word8 -> IO ()
writeWord8 mv i v = readIORef mv >>= \arr -> writePrimArray arr i v

readWord8 :: IORef (MutablePrimArray RealWorld Word8) -> Int -> IO Word8
readWord8 mv i = readIORef mv >>= \arr -> readPrimArray arr i

writeFloat :: IORef (MutablePrimArray RealWorld Float) -> Int -> Float -> IO ()
writeFloat mv i v = readIORef mv >>= \arr -> writePrimArray arr i v

readFloat :: IORef (MutablePrimArray RealWorld Float) -> Int -> IO Float
readFloat mv i = readIORef mv >>= \arr -> readPrimArray arr i

writeWord64 :: IORef (MutablePrimArray RealWorld Word64) -> Int -> Word64 -> IO ()
writeWord64 mv i v = readIORef mv >>= \arr -> writePrimArray arr i v

readWord64 :: IORef (MutablePrimArray RealWorld Word64) -> Int -> IO Word64
readWord64 mv i = readIORef mv >>= \arr -> readPrimArray arr i

{-# INLINE ensureCapacity #-}
ensureCapacity :: NodeArena -> Int -> IO ()
ensureCapacity na needed = do
  cap <- readIORef (naCapacity na)
  if needed < cap
    then pure ()
    else
      let newCap = cap * 2
       in do
        growInt (naParent na) cap newCap (-1)
        growInt (naFirstChild na) cap newCap (-1)
        growInt (naNextSibling na) cap newCap (-1)
        growInt (naChildCount na) cap newCap 0
        growWord8 (naNodeType na) cap newCap 0
        growWord8 (naDirection na) cap newCap 0
        growWord8 (naWidthSizing na) cap newCap 0
        growWord8 (naHeightSizing na) cap newCap 0
        growFloat (naWidthValue na) cap newCap 0
        growFloat (naHeightValue na) cap newCap 0
        growFloat (naPadL na) cap newCap 0
        growFloat (naPadR na) cap newCap 0
        growFloat (naPadT na) cap newCap 0
        growFloat (naPadB na) cap newCap 0
        growFloat (naGap na) cap newCap 0
        growFloat (naMinW na) cap newCap 0
        growFloat (naMinH na) cap newCap 0
        growFloat (naMaxW na) cap newCap 1e9
        growFloat (naMaxH na) cap newCap 1e9
        growFloat (naGrow na) cap newCap 0
        growWord8 (naAlignX na) cap newCap 0
        growWord8 (naAlignY na) cap newCap 0
        growFloat (naX na) cap newCap 0
        growFloat (naY na) cap newCap 0
        growFloat (naW na) cap newCap 0
        growFloat (naH na) cap newCap 0
        growWord64 (naWidgetId na) cap newCap 0
        growInt (naStyleIdx na) cap newCap 0
        growInt (naTextIdx na) cap newCap (-1)
        growTextStore (naTextStore na) cap newCap
        writeIORef (naCapacity na) newCap

growInt :: IORef (MutablePrimArray RealWorld Int) -> Int -> Int -> Int -> IO ()
growInt mv oldCap newCap fill = do
  arr <- readIORef mv
  newArr <- newPrimArray newCap
  forM_ [0 .. oldCap - 1] $ \i ->
    readPrimArray arr i >>= writePrimArray newArr i
  forM_ [oldCap .. newCap - 1] $ \i ->
    writePrimArray newArr i fill
  writeIORef mv newArr

growWord8 :: IORef (MutablePrimArray RealWorld Word8) -> Int -> Int -> Word8 -> IO ()
growWord8 mv oldCap newCap fill = do
  arr <- readIORef mv
  newArr <- newPrimArray newCap
  forM_ [0 .. oldCap - 1] $ \i ->
    readPrimArray arr i >>= writePrimArray newArr i
  forM_ [oldCap .. newCap - 1] $ \i ->
    writePrimArray newArr i fill
  writeIORef mv newArr

growFloat :: IORef (MutablePrimArray RealWorld Float) -> Int -> Int -> Float -> IO ()
growFloat mv oldCap newCap fill = do
  arr <- readIORef mv
  newArr <- newPrimArray newCap
  forM_ [0 .. oldCap - 1] $ \i ->
    readPrimArray arr i >>= writePrimArray newArr i
  forM_ [oldCap .. newCap - 1] $ \i ->
    writePrimArray newArr i fill
  writeIORef mv newArr

growWord64 :: IORef (MutablePrimArray RealWorld Word64) -> Int -> Int -> Word64 -> IO ()
growWord64 mv oldCap newCap fill = do
  arr <- readIORef mv
  newArr <- newPrimArray newCap
  forM_ [0 .. oldCap - 1] $ \i ->
    readPrimArray arr i >>= writePrimArray newArr i
  forM_ [oldCap .. newCap - 1] $ \i ->
    writePrimArray newArr i fill
  writeIORef mv newArr

growTextStore :: IORef (MutableArray RealWorld Text) -> Int -> Int -> IO ()
growTextStore mv oldCap newCap = do
  arr <- readIORef mv
  newArr <- newArray newCap T.empty
  forM_ [0 .. oldCap - 1] $ \i ->
    readArray arr i >>= writeArray newArr i
  forM_ [oldCap .. newCap - 1] $ \i ->
    writeArray newArr i T.empty
  writeIORef mv newArr

{-# INLINE sizingTag #-}
sizingTag :: Sizing -> (SizingTag, Float)
sizingTag (Fixed v) = (SizingFixed, v)
sizingTag Fit = (SizingFit, 0)
sizingTag (Grow g) = (SizingGrow, g)
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
  writeInt (naParent na) idx parent
  writeWord8 (naNodeType na) idx (fromIntegral (fromEnum nt))
  writeWord8 (naDirection na) idx (fromIntegral (fromEnum (dirTag dir)))
  writeWord8 (naWidthSizing na) idx (fromIntegral (fromEnum wTag))
  writeWord8 (naHeightSizing na) idx (fromIntegral (fromEnum hTag))
  writeFloat (naWidthValue na) idx wVal
  writeFloat (naHeightValue na) idx hVal
  writeFloat (naPadL na) idx (padL pad)
  writeFloat (naPadR na) idx (padR pad)
  writeFloat (naPadT na) idx (padT pad)
  writeFloat (naPadB na) idx (padB pad)
  writeFloat (naGap na) idx gap
  writeFloat (naMinW na) idx minW
  writeFloat (naMinH na) idx minH
  writeFloat (naMaxW na) idx maxW
  writeFloat (naMaxH na) idx maxH
  writeFloat (naGrow na) idx grow
  writeWord8 (naAlignX na) idx (alignXTag ax)
  writeWord8 (naAlignY na) idx (alignYTag ay)
  writeFloat (naX na) idx 0
  writeFloat (naY na) idx 0
  writeFloat (naW na) idx 0
  writeFloat (naH na) idx 0
  writeWord64 (naWidgetId na) idx 0
  writeInt (naStyleIdx na) idx 0
  writeInt (naTextIdx na) idx (-1)
  writeInt (naFirstChild na) idx (-1)
  writeInt (naNextSibling na) idx (-1)
  writeInt (naChildCount na) idx 0
  if parent >= 0
    then do
      fc <- readInt (naFirstChild na) parent
      writeInt (naNextSibling na) idx fc
      writeInt (naFirstChild na) parent idx
      cc <- readInt (naChildCount na) parent
      writeInt (naChildCount na) parent (cc + 1)
    else pure ()
  writeIORef (naCount na) (idx + 1)
  pure idx

{-# INLINE setNodeText #-}
setNodeText :: NodeArena -> NodeIdx -> Text -> IO ()
setNodeText na idx txt = do
  textStore <- readIORef (naTextStore na)
  writeArray textStore idx txt
  writeInt (naTextIdx na) idx idx

{-# INLINE getParent #-}
getParent na idx = readInt (naParent na) idx

{-# INLINE getFirstChild #-}
getFirstChild na idx = readInt (naFirstChild na) idx

{-# INLINE getNextSibling #-}
getNextSibling na idx = readInt (naNextSibling na) idx

{-# INLINE getChildCount #-}
getChildCount na idx = readInt (naChildCount na) idx

{-# INLINE getNodeType #-}
getNodeType na idx = readWord8 (naNodeType na) idx >>= pure . toEnum . fromIntegral

{-# INLINE getDirection #-}
getDirection na idx = readWord8 (naDirection na) idx >>= pure . toEnum . fromIntegral

{-# INLINE getWidthSizing #-}
getWidthSizing na idx = do
  tag <- readWord8 (naWidthSizing na) idx
  val <- readFloat (naWidthValue na) idx
  pure (toEnum (fromIntegral tag), val)

{-# INLINE getHeightSizing #-}
getHeightSizing na idx = do
  tag <- readWord8 (naHeightSizing na) idx
  val <- readFloat (naHeightValue na) idx
  pure (toEnum (fromIntegral tag), val)

{-# INLINE getPadding #-}
getPadding na idx = do
  l <- readFloat (naPadL na) idx
  r <- readFloat (naPadR na) idx
  t <- readFloat (naPadT na) idx
  b <- readFloat (naPadB na) idx
  pure (Padding l r t b)

{-# INLINE getGap #-}
getGap na idx = readFloat (naGap na) idx

{-# INLINE getMinMax #-}
getMinMax na idx = do
  minW <- readFloat (naMinW na) idx
  minH <- readFloat (naMinH na) idx
  maxW <- readFloat (naMaxW na) idx
  maxH <- readFloat (naMaxH na) idx
  pure (minW, minH, maxW, maxH)

{-# INLINE getGrow #-}
getGrow na idx = readFloat (naGrow na) idx

{-# INLINE getAlignX #-}
getAlignX na idx = do
  w <- readWord8 (naAlignX na) idx
  pure $
    case w of
      0 -> AlignStart
      1 -> AlignCenter
      2 -> AlignEnd
      _ -> AlignStart

{-# INLINE getAlignY #-}
getAlignY na idx = do
  w <- readWord8 (naAlignY na) idx
  pure $
    case w of
      0 -> AlignTop
      1 -> AlignMiddle
      2 -> AlignBottom
      _ -> AlignTop

{-# INLINE getRect #-}
getRect na idx = do
  x <- readFloat (naX na) idx
  y <- readFloat (naY na) idx
  w <- readFloat (naW na) idx
  h <- readFloat (naH na) idx
  pure (x, y, w, h)

{-# INLINE setRect #-}
setRect na idx x y w h = do
  writeFloat (naX na) idx x
  writeFloat (naY na) idx y
  writeFloat (naW na) idx w
  writeFloat (naH na) idx h

{-# INLINE getText #-}
getText na idx = do
  ti <- readInt (naTextIdx na) idx
  if ti < 0
    then pure T.empty
    else readIORef (naTextStore na) >>= \arr -> readArray arr ti

{-# INLINE getWidgetId #-}
getWidgetId na idx = readWord64 (naWidgetId na) idx >>= pure . WidgetId

{-# INLINE setWidgetId #-}
setWidgetId na idx wid = writeWord64 (naWidgetId na) idx (hashWidgetId wid)

{-# INLINE getStyleIdx #-}
getStyleIdx na idx = readInt (naStyleIdx na) idx

{-# INLINE setStyleIdx #-}
setStyleIdx na idx si = writeInt (naStyleIdx na) idx si
