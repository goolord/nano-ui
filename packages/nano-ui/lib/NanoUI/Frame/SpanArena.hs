{-# LANGUAGE RecordWildCards #-}

-- | Flat span buffer: parallel prim arrays for geometry/colors, boxed texts.
module NanoUI.Frame.SpanArena
  ( SpanArena (..)
  , SpanArenaArrays (..)
  , newSpanArena
  , resetSpanArena
  , pushSpan
  , spanArenaCount
  , spanArenaArrays
  , withSpanArenaSnap
  , spanArenaToList
  , spanArenaToListOccluded
  , foldSpanArena
  , foldSpanArenaOccluded
  ) where

import Control.Monad (unless, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.Primitive (Prim)
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
import Control.Exception (bracket_)

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
  , saSnap :: IORef (Maybe SpanArenaArrays)
  }

-- | Snapshot of the column arrays so batch pushes/reads skip one IORef
-- dereference per column access.
data SpanArenaArrays = SpanArenaArrays
  { saaX :: MutablePrimArray RealWorld Float
  , saaY :: MutablePrimArray RealWorld Float
  , saaW :: MutablePrimArray RealWorld Float
  , saaH :: MutablePrimArray RealWorld Float
  , saaClipX :: MutablePrimArray RealWorld Float
  , saaClipY :: MutablePrimArray RealWorld Float
  , saaClipW :: MutablePrimArray RealWorld Float
  , saaClipH :: MutablePrimArray RealWorld Float
  , saaFg :: MutablePrimArray RealWorld Word32
  , saaBg :: MutablePrimArray RealWorld Word32
  , saaText :: MutableArray RealWorld Text
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
  saSnap <- newIORef Nothing
  pure SpanArena {..}

resetSpanArena :: SpanArena -> IO ()
resetSpanArena sa = writeIORef (saCount sa) 0

spanArenaCount :: SpanArena -> IO Int
spanArenaCount sa = readIORef (saCount sa)

-- | Current column arrays, preferring the active snapshot (see
-- 'withSpanArenaSnap').
{-# INLINE spanArenaArrays #-}
spanArenaArrays :: SpanArena -> IO SpanArenaArrays
spanArenaArrays sa = do
  m <- readIORef (saSnap sa)
  case m of
    Just a -> pure a
    Nothing -> readSpanArenaArrays sa

readSpanArenaArrays :: SpanArena -> IO SpanArenaArrays
readSpanArenaArrays SpanArena {..} =
  SpanArenaArrays
    <$> readIORef saX
    <*> readIORef saY
    <*> readIORef saW
    <*> readIORef saH
    <*> readIORef saClipX
    <*> readIORef saClipY
    <*> readIORef saClipW
    <*> readIORef saClipH
    <*> readIORef saFg
    <*> readIORef saBg
    <*> readIORef saText

-- | Pin the column arrays for a batch of pushes/reads so per-span access
-- skips the column IORefs. Growth during the snapshot refreshes it.
withSpanArenaSnap :: SpanArena -> IO a -> IO a
withSpanArenaSnap sa act =
  bracket_
    (readSpanArenaArrays sa >>= writeIORef (saSnap sa) . Just)
    (writeIORef (saSnap sa) Nothing)
    act

ensureSpanCap :: SpanArena -> Int -> IO ()
ensureSpanCap sa needed = do
  cap <- readIORef (saCap sa)
  when (needed > cap) $ do
    let newCap = max needed (cap * 2)
    growP (saX sa) cap newCap
    growP (saY sa) cap newCap
    growP (saW sa) cap newCap
    growP (saH sa) cap newCap
    growP (saClipX sa) cap newCap
    growP (saClipY sa) cap newCap
    growP (saClipW sa) cap newCap
    growP (saClipH sa) cap newCap
    growP (saFg sa) cap newCap
    growP (saBg sa) cap newCap
    growT (saText sa) cap newCap
    writeIORef (saCap sa) newCap
    -- Keep an active snapshot pointing at the fresh columns.
    m <- readIORef (saSnap sa)
    case m of
      Just _ -> readSpanArenaArrays sa >>= writeIORef (saSnap sa) . Just
      Nothing -> pure ()

growP :: Prim a => IORef (MutablePrimArray RealWorld a) -> Int -> Int -> IO ()
growP ref oldCap newCap = do
  arr <- readIORef ref
  newArr <- newPrimArray newCap
  copyMutablePrimArray newArr 0 arr 0 oldCap
  writeIORef ref newArr
{-# SPECIALIZE growP :: IORef (MutablePrimArray RealWorld Float) -> Int -> Int -> IO () #-}
{-# SPECIALIZE growP :: IORef (MutablePrimArray RealWorld Word32) -> Int -> Int -> IO () #-}

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
  SpanArenaArrays {..} <- spanArenaArrays sa
  writePrimArray saaX i x
  writePrimArray saaY i y
  writePrimArray saaW i w
  writePrimArray saaH i h
  writePrimArray saaClipX i cx
  writePrimArray saaClipY i cy
  writePrimArray saaClipW i cw
  writePrimArray saaClipH i ch
  writePrimArray saaFg i (colorToWord32 fg)
  writePrimArray saaBg i (colorToWord32 bg)
  writeArray saaText i txt
  writeIORef (saCount sa) (i + 1)

spanArenaToList :: SpanArena -> IO [(Rect, Text, Color, Color, Rect)]
spanArenaToList sa = spanArenaToListOccluded IM.empty sa

spanArenaToListOccluded :: IM.IntMap Rect -> SpanArena -> IO [(Rect, Text, Color, Color, Rect)]
spanArenaToListOccluded panels sa = do
  accRef <- newIORef []
  foldSpanArenaOccluded panels sa $ \r t fg bg c ->
    modifyIORef' accRef ((r, t, fg, bg, c) :)
  reverse <$> readIORef accRef

foldSpanArena :: SpanArena -> (Rect -> Text -> Color -> Color -> Rect -> IO ()) -> IO ()
foldSpanArena = foldSpanArenaOccluded IM.empty

{-# INLINE foldSpanArenaOccluded #-}
foldSpanArenaOccluded ::
  IM.IntMap Rect ->
  SpanArena ->
  (Rect -> Text -> Color -> Color -> Rect -> IO ()) ->
  IO ()
foldSpanArenaOccluded panels sa f = do
  n <- readIORef (saCount sa)
  SpanArenaArrays {..} <- spanArenaArrays sa
  let panelRects
        | IM.null panels = []
        | otherwise = IM.elems panels
      go !i
        | i >= n = pure ()
        | otherwise = do
            x <- readPrimArray saaX i
            y <- readPrimArray saaY i
            w <- readPrimArray saaW i
            h <- readPrimArray saaH i
            cx <- readPrimArray saaClipX i
            cy <- readPrimArray saaClipY i
            cw <- readPrimArray saaClipW i
            ch <- readPrimArray saaClipH i
            fg <- readPrimArray saaFg i
            bg <- readPrimArray saaBg i
            txt <- readArray saaText i
            let rect = Rect x y w h
                clip = Rect cx cy cw ch
            unless (not (null panelRects) && spanOccluded panelRects rect clip) $
              f rect txt (Color fg) (Color bg) clip
            go (i + 1)
  go 0

spanOccluded :: [Rect] -> Rect -> Rect -> Bool
spanOccluded panelRects rect clip =
  case rectIntersect rect clip of
    Nothing -> True
    Just visible -> any (rectFullyInside visible) panelRects
