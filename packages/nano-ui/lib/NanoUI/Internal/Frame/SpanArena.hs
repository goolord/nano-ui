{-# LANGUAGE RecordWildCards #-}

-- | Flat span buffer: strided prim arrays for geometry and colors, boxed texts.
module NanoUI.Internal.Frame.SpanArena
  ( SpanArena
  , newSpanArena
  , resetSpanArena
  , pushSpan
  , spanArenaCount
  , spanArenaToList
  , spanArenaToListOccluded
  , foldSpanArena
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.Primitive.Array (MutableArray, copyMutableArray, newArray, readArray, sizeofMutableArray, writeArray)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , newPrimArray
  , readPrimArray
  , resizeMutablePrimArray
  , writePrimArray
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import GHC.Exts (RealWorld)
import NanoUI.Internal.Types (Color (..), Rect (..), colorToWord32, rectFullyInside, rectIntersect)

-- | Reusable mutable text-span storage for one frame. Entries contain logical
-- bounds/clip, text, and foreground/background colours.
data SpanArena = SpanArena
  { saCount :: IORef Int
  , saArrays :: IORef SpanArenaArrays
  }

-- | Span columns. @saRects@ holds 'rectStride' floats per span (the span rect,
-- then its clip), @saColors@ the foreground and background, and @saTexts@ one
-- text per span; its size is the capacity.
data SpanArenaArrays = SpanArenaArrays
  { saRects :: !(MutablePrimArray RealWorld Float)
  , saColors :: !(MutablePrimArray RealWorld Word32)
  , saTexts :: !(MutableArray RealWorld Text)
  }

rectStride :: Int
rectStride = 8

-- | Empty span storage with at least 16 slots, growing as needed.
newSpanArena :: Int -> IO SpanArena
newSpanArena cap0 = do
  let cap = max 16 cap0
  saCount <- newIORef 0
  saRects <- newPrimArray (cap * rectStride)
  saColors <- newPrimArray (cap * 2)
  saTexts <- newArray cap T.empty
  saArrays <- newIORef SpanArenaArrays {..}
  pure SpanArena {..}

-- | Clear the live count while retaining capacity for subsequent spans.
resetSpanArena :: SpanArena -> IO ()
resetSpanArena sa = writeIORef (saCount sa) 0

-- | Live span count, independent of allocated capacity.
spanArenaCount :: SpanArena -> IO Int
spanArenaCount sa = readIORef (saCount sa)

{-# NOINLINE growSpanArena #-}
growSpanArena :: SpanArena -> SpanArenaArrays -> Int -> IO SpanArenaArrays
growSpanArena sa SpanArenaArrays {saRects = rects, saColors = colors, saTexts = texts} needed = do
  let cap = sizeofMutableArray texts
      newCap = max needed (cap * 2)
  saRects <- resizeMutablePrimArray rects (newCap * rectStride)
  saColors <- resizeMutablePrimArray colors (newCap * 2)
  saTexts <- newArray newCap T.empty
  copyMutableArray saTexts 0 texts 0 cap
  let a = SpanArenaArrays {..}
  writeIORef (saArrays sa) a
  pure a

-- | Append bounds, text, foreground, background, and clip in paint order.
{-# INLINE pushSpan #-}
pushSpan :: SpanArena -> Rect -> Text -> Color -> Color -> Rect -> IO ()
pushSpan sa (Rect x y w h) txt fg bg (Rect cx cy cw ch) = do
  i <- readIORef (saCount sa)
  a0 <- readIORef (saArrays sa)
  SpanArenaArrays {..} <-
    if i < sizeofMutableArray (saTexts a0) then pure a0 else growSpanArena sa a0 (i + 1)
  let !r = i * rectStride
  writePrimArray saRects r x
  writePrimArray saRects (r + 1) y
  writePrimArray saRects (r + 2) w
  writePrimArray saRects (r + 3) h
  writePrimArray saRects (r + 4) cx
  writePrimArray saRects (r + 5) cy
  writePrimArray saRects (r + 6) cw
  writePrimArray saRects (r + 7) ch
  writePrimArray saColors (2 * i) (colorToWord32 fg)
  writePrimArray saColors (2 * i + 1) (colorToWord32 bg)
  writeArray saTexts i txt
  writeIORef (saCount sa) (i + 1)

-- | Copy visible clipped spans into a list in insertion order.
spanArenaToList :: SpanArena -> IO [(Rect, Text, Color, Color, Rect)]
spanArenaToList = spanArenaToListOccluded IM.empty

-- | Spans in push order, dropping those hidden behind @panels@.
spanArenaToListOccluded :: IM.IntMap Rect -> SpanArena -> IO [(Rect, Text, Color, Color, Rect)]
spanArenaToListOccluded panels sa =
  foldSpans panels sa True (\acc r t fg bg c -> pure ((r, t, fg, bg, c) : acc)) []

-- | Visit clipped spans in insertion order without constructing a list.
foldSpanArena :: SpanArena -> (Rect -> Text -> Color -> Color -> Rect -> IO ()) -> IO ()
foldSpanArena sa f = foldSpans IM.empty sa False (\_ r t fg bg c -> f r t fg bg c) ()

-- | Fold over the spans not hidden behind @panels@, first to last, or last to
-- first when @backwards@ (so a consing fold builds a list in push order).
{-# INLINE foldSpans #-}
foldSpans ::
  IM.IntMap Rect ->
  SpanArena ->
  Bool ->
  (acc -> Rect -> Text -> Color -> Color -> Rect -> IO acc) ->
  acc ->
  IO acc
foldSpans panels sa backwards f z = do
  n <- readIORef (saCount sa)
  SpanArenaArrays {..} <- readIORef (saArrays sa)
  let panelRects = IM.elems panels
      go !i !acc
        | i < 0 || i >= n = pure acc
        | otherwise = do
            let !r = i * rectStride
            x <- readPrimArray saRects r
            y <- readPrimArray saRects (r + 1)
            w <- readPrimArray saRects (r + 2)
            h <- readPrimArray saRects (r + 3)
            cx <- readPrimArray saRects (r + 4)
            cy <- readPrimArray saRects (r + 5)
            cw <- readPrimArray saRects (r + 6)
            ch <- readPrimArray saRects (r + 7)
            fg <- readPrimArray saColors (2 * i)
            bg <- readPrimArray saColors (2 * i + 1)
            txt <- readArray saTexts i
            let rect = Rect x y w h
                clip = Rect cx cy cw ch
            acc' <-
              if not (null panelRects) && spanOccluded panelRects rect clip
                then pure acc
                else f acc rect txt (Color fg) (Color bg) clip
            go (if backwards then i - 1 else i + 1) acc'
  go (if backwards then n - 1 else 0) z

spanOccluded :: [Rect] -> Rect -> Rect -> Bool
spanOccluded panelRects rect clip =
  case rectIntersect rect clip of
    Nothing -> True
    Just visible -> any (rectFullyInside visible) panelRects
