{-# LANGUAGE RecordWildCards #-}

-- | Flat span buffer: parallel prim arrays for geometry/colors, boxed texts.
module NanoUI.Frame.SpanArena
  ( SpanArena (..)
  , newSpanArena
  , resetSpanArena
  , pushSpan
  , spanArenaCount
  , spanArenaToList
  , spanArenaToListOccluded
  , foldSpanArena
  ) where

import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.Primitive.Array (MutableArray, copyMutableArray, newArray, readArray, writeArray)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , copyMutablePrimArray
  , newPrimArray
  , readPrimArray
  , writePrimArray
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import GHC.Exts (RealWorld)
import NanoUI.Types (Color (..), Rect (..), colorToWord32, rectFullyInside, rectIntersect)

data SpanArena = SpanArena
  { saCount :: IORef Int
  , saCap :: IORef Int
  , saX :: IORef (MutablePrimArray RealWorld Float)
  , saY :: IORef (MutablePrimArray RealWorld Float)
  , saW :: IORef (MutablePrimArray RealWorld Float)
  , saH :: IORef (MutablePrimArray RealWorld Float)
  , saClipX :: IORef (MutablePrimArray RealWorld Float)
  , saClipY :: IORef (MutablePrimArray RealWorld Float)
  , saClipW :: IORef (MutablePrimArray RealWorld Float)
  , saClipH :: IORef (MutablePrimArray RealWorld Float)
  , saFg :: IORef (MutablePrimArray RealWorld Word32)
  , saBg :: IORef (MutablePrimArray RealWorld Word32)
  , saText :: IORef (MutableArray RealWorld Text)
  }

newSpanArena :: Int -> IO SpanArena
newSpanArena cap0 = do
  let cap = max 16 cap0
  saCount <- newIORef 0
  saCap <- newIORef cap
  saX <- newIORef =<< newPrimArray cap
  saY <- newIORef =<< newPrimArray cap
  saW <- newIORef =<< newPrimArray cap
  saH <- newIORef =<< newPrimArray cap
  saClipX <- newIORef =<< newPrimArray cap
  saClipY <- newIORef =<< newPrimArray cap
  saClipW <- newIORef =<< newPrimArray cap
  saClipH <- newIORef =<< newPrimArray cap
  saFg <- newIORef =<< newPrimArray cap
  saBg <- newIORef =<< newPrimArray cap
  saText <- newIORef =<< newArray cap T.empty
  pure SpanArena {..}

resetSpanArena :: SpanArena -> IO ()
resetSpanArena sa = writeIORef (saCount sa) 0

spanArenaCount :: SpanArena -> IO Int
spanArenaCount sa = readIORef (saCount sa)

ensureSpanCap :: SpanArena -> Int -> IO ()
ensureSpanCap sa needed = do
  cap <- readIORef (saCap sa)
  when (needed > cap) $ do
    let newCap = max needed (cap * 2)
    growF (saX sa) cap newCap
    growF (saY sa) cap newCap
    growF (saW sa) cap newCap
    growF (saH sa) cap newCap
    growF (saClipX sa) cap newCap
    growF (saClipY sa) cap newCap
    growF (saClipW sa) cap newCap
    growF (saClipH sa) cap newCap
    growW (saFg sa) cap newCap
    growW (saBg sa) cap newCap
    growT (saText sa) cap newCap
    writeIORef (saCap sa) newCap

growF :: IORef (MutablePrimArray RealWorld Float) -> Int -> Int -> IO ()
growF ref oldCap newCap = do
  arr <- readIORef ref
  newArr <- newPrimArray newCap
  copyMutablePrimArray newArr 0 arr 0 oldCap
  writeIORef ref newArr

growW :: IORef (MutablePrimArray RealWorld Word32) -> Int -> Int -> IO ()
growW ref oldCap newCap = do
  arr <- readIORef ref
  newArr <- newPrimArray newCap
  copyMutablePrimArray newArr 0 arr 0 oldCap
  writeIORef ref newArr

growT :: IORef (MutableArray RealWorld Text) -> Int -> Int -> IO ()
growT ref oldCap newCap = do
  arr <- readIORef ref
  newArr <- newArray newCap T.empty
  copyMutableArray newArr 0 arr 0 oldCap
  writeIORef ref newArr

{-# INLINE pushSpan #-}
pushSpan :: SpanArena -> Rect -> Text -> Color -> Color -> Rect -> IO ()
pushSpan sa (Rect x y w h) txt fg bg (Rect cx cy cw ch) = do
  i <- readIORef (saCount sa)
  ensureSpanCap sa (i + 1)
  readIORef (saX sa) >>= \a -> writePrimArray a i x
  readIORef (saY sa) >>= \a -> writePrimArray a i y
  readIORef (saW sa) >>= \a -> writePrimArray a i w
  readIORef (saH sa) >>= \a -> writePrimArray a i h
  readIORef (saClipX sa) >>= \a -> writePrimArray a i cx
  readIORef (saClipY sa) >>= \a -> writePrimArray a i cy
  readIORef (saClipW sa) >>= \a -> writePrimArray a i cw
  readIORef (saClipH sa) >>= \a -> writePrimArray a i ch
  readIORef (saFg sa) >>= \a -> writePrimArray a i (colorToWord32 fg)
  readIORef (saBg sa) >>= \a -> writePrimArray a i (colorToWord32 bg)
  readIORef (saText sa) >>= \a -> writeArray a i txt
  writeIORef (saCount sa) (i + 1)

spanArenaToList :: SpanArena -> IO [(Rect, Text, Color, Color, Rect)]
spanArenaToList sa = spanArenaToListOccluded IM.empty sa

spanArenaToListOccluded :: IM.IntMap Rect -> SpanArena -> IO [(Rect, Text, Color, Color, Rect)]
spanArenaToListOccluded panels sa = do
  n <- readIORef (saCount sa)
  if n <= 0
    then pure []
    else do
      let panelRects
            | IM.null panels = []
            | otherwise = IM.elems panels
      go (n - 1) [] panelRects
  where
    go !i acc panelRects
      | i < 0 = pure acc
      | otherwise = do
          (Rect x y w h, txt, fg, bg, clip) <- readSpanAt sa i
          acc' <-
            if null panelRects || not (spanOccluded panelRects (Rect x y w h) clip)
              then pure ((Rect x y w h, txt, fg, bg, clip) : acc)
              else pure acc
          go (i - 1) acc' panelRects

readSpanAt :: SpanArena -> Int -> IO (Rect, Text, Color, Color, Rect)
readSpanAt sa i = do
  x <- readIORef (saX sa) >>= \a -> readPrimArray a i
  y <- readIORef (saY sa) >>= \a -> readPrimArray a i
  w <- readIORef (saW sa) >>= \a -> readPrimArray a i
  h <- readIORef (saH sa) >>= \a -> readPrimArray a i
  cx <- readIORef (saClipX sa) >>= \a -> readPrimArray a i
  cy <- readIORef (saClipY sa) >>= \a -> readPrimArray a i
  cw <- readIORef (saClipW sa) >>= \a -> readPrimArray a i
  ch <- readIORef (saClipH sa) >>= \a -> readPrimArray a i
  fg <- readIORef (saFg sa) >>= \a -> readPrimArray a i
  bg <- readIORef (saBg sa) >>= \a -> readPrimArray a i
  txt <- readIORef (saText sa) >>= \a -> readArray a i
  pure (Rect x y w h, txt, Color fg, Color bg, Rect cx cy cw ch)

spanOccluded :: [Rect] -> Rect -> Rect -> Bool
spanOccluded panelRects rect clip =
  case rectIntersect rect clip of
    Nothing -> True
    Just visible -> any (rectFullyInside visible) panelRects

foldSpanArena :: SpanArena -> (Rect -> Text -> Color -> Color -> Rect -> IO ()) -> IO ()
foldSpanArena sa f = do
  n <- readIORef (saCount sa)
  let go !i
        | i >= n = pure ()
        | otherwise = do
            (Rect x y w h, txt, fg, bg, clip) <- readSpanAt sa i
            f (Rect x y w h) txt fg bg clip
            go (i + 1)
  go 0
