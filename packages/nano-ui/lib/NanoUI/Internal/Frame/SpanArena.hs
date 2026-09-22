-- | Reusable text-span storage for one frame.
module NanoUI.Internal.Frame.SpanArena
  ( SpanArena
  , newSpanArena
  , resetSpanArena
  , pushSpans
  , spanArenaCount
  , spanArenaToList
  , foldSpanArena
  ) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import NanoUI.Internal.Types (Color, Rect, rectFullyInside, rectIntersect)

-- | One frame's text spans, newest first. Each holds logical bounds, text,
-- foreground and background colours, and clip.
newtype SpanArena = SpanArena (IORef [(Rect, Text, Color, Color, Rect)])

newSpanArena :: IO SpanArena
newSpanArena = SpanArena <$> newIORef []

-- | Drop every span.
resetSpanArena :: SpanArena -> IO ()
resetSpanArena (SpanArena ref) = writeIORef ref []

-- | Append bounds, text, foreground, background, and clip in paint order.
pushSpans :: SpanArena -> [(Rect, Text, Color, Color, Rect)] -> IO ()
pushSpans (SpanArena ref) spans = modifyIORef' ref (\acc -> foldl' (flip (:)) acc spans)

-- | Live span count.
spanArenaCount :: SpanArena -> IO Int
spanArenaCount (SpanArena ref) = length <$> readIORef ref

-- | Spans in push order, dropping those hidden behind @panels@.
spanArenaToList :: IM.IntMap Rect -> SpanArena -> IO [(Rect, Text, Color, Color, Rect)]
spanArenaToList panels (SpanArena ref) = foldl' keep [] <$> readIORef ref
  where
    panelRects = IM.elems panels
    keep acc s@(rect, _, _, _, clip)
      | not (null panelRects) && spanOccluded panelRects rect clip = acc
      | otherwise = s : acc

-- | Visit spans in push order.
foldSpanArena :: SpanArena -> (Rect -> Text -> Color -> Color -> Rect -> IO ()) -> IO ()
foldSpanArena (SpanArena ref) f =
  readIORef ref >>= mapM_ (\(r, t, fg, bg, c) -> f r t fg bg c) . reverse

spanOccluded :: [Rect] -> Rect -> Rect -> Bool
spanOccluded panelRects rect clip =
  case rectIntersect rect clip of
    Nothing -> True
    Just visible -> any (rectFullyInside visible) panelRects
