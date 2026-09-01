{-# LANGUAGE DataKinds #-}

-- | Scroll and text clipping rects for paint and span tagging.
module NanoUI.Frame.Clip
  ( tagClippedSpans
  , padTextClipRect
  , padContentClip
  , terminalModalOuterClip
  , scrollContentClip
  , scrollChromeLane
  , borderContentClip
  ) where

import qualified Data.Text as T
import NanoUI.Font (FontMetrics)
import NanoUI.Host (HostProfile)
import NanoUI.Frame.Scroll.Geometry
  ( padContentClip
  , scrollChromeLane
  , scrollContentClip
  )
import NanoUI.Style (Padding (..), Style (..), styleBorderWidth)
import NanoUI.Types (Color, Rect (..), rectIntersect)

tagClippedSpans :: Rect -> [(Rect, T.Text, Color, Color)] -> [(Rect, T.Text, Color, Color, Rect)]
tagClippedSpans clip =
  concatMap
    ( \(rect, txt, fg, bg) ->
        case rectIntersect clip (padTextClipRect rect) of
          Nothing -> []
          Just clipHere -> [(rect, txt, fg, bg, clipHere)]
    )

textClipSlop :: Float
textClipSlop = 4

padTextClipRect :: Rect -> Rect
padTextClipRect (Rect x y w h) = Rect x y (w + textClipSlop) h

-- TUI modal: title and separator stay fixed; modal/2 wraps body in scroll.
-- Outer clip is the padded panel. Inner NodeScrollContainer clips overflow.
terminalModalOuterClip :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> Padding -> Rect
terminalModalOuterClip = padContentClip

borderContentClip :: Style -> Rect -> Rect
borderContentClip style (Rect x y w h) =
  if styleBorderWidth style <= 0
    then Rect x y w h
    else
      let bw = max 1 (styleBorderWidth style)
       in Rect (x + bw) (y + bw) (max 0 (w - 2 * bw)) (max 0 (h - 2 * bw))
