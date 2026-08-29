{-# LANGUAGE DataKinds #-}

-- | Scroll and text clipping rects for paint and span tagging.
module NanoUI.Frame.Clip
  ( tagClippedSpans
  , padTextClipRect
  , padContentClip
  , terminalModalOuterClip
  , scrollContentClip
  , scrollChromeLane
  ) where

import qualified Data.Text as T
import NanoUI.Font
  ( FontMetrics
  , ScrollBarSlot (..)
  , resolveLayoutPadding
  , scrollBarGeomFor
  , scrollBarOuterGap
  , scrollLayoutGutter
  )
import NanoUI.Host (HostProfile)
import NanoUI.Layout.Arena (DirTag (..))
import NanoUI.Style (Padding (..))
import NanoUI.Types (Color, Rect (..), rectH, rectIntersect, rectW, rectX, rectY)

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

padContentClip :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> Padding -> Rect
padContentClip host fm x y w h pad0 =
  let pad = resolveLayoutPadding host fm pad0
   in Rect
        (x + padL pad)
        (y + padT pad)
        (max 0 (w - padL pad - padR pad))
        (max 0 (h - padT pad - padB pad))

-- TUI modal: title and separator stay fixed; modal/2 wraps body in scroll.
-- Outer clip is the padded panel. Inner NodeScrollContainer clips overflow.
terminalModalOuterClip :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> Padding -> Rect
terminalModalOuterClip = padContentClip

scrollContentClip ::
  HostProfile ->
  FontMetrics ->
  ScrollBarSlot ->
  DirTag ->
  Float ->
  Float ->
  Float ->
  Float ->
  Padding ->
  Float ->
  Rect
scrollContentClip host fm slot dir x y w h pad contentSize =
  let base = padContentClip host fm x y w h pad
      innerMain =
        case dir of
          DirColumn -> rectH base
          DirRow -> rectW base
      gutter = scrollLayoutGutter host fm slot contentSize innerMain
   in case dir of
        DirColumn -> Rect (rectX base) (rectY base) (max 0 (rectW base - gutter)) (rectH base)
        DirRow -> Rect (rectX base) (rectY base) (rectW base) (max 0 (rectH base - gutter))

-- List/page bars sit in the scroll rect. Window body hangs into the parent pad.
scrollChromeLane ::
  HostProfile -> FontMetrics -> ScrollBarSlot -> DirTag -> Float -> Float -> Float -> Float -> Padding -> Rect
scrollChromeLane host fm slot dir x y w h pad =
  let (barW, _) = scrollBarGeomFor host fm slot
      outer = scrollBarOuterGap host fm slot
      hang = slot == ScrollBarWindow
   in case dir of
        DirColumn ->
          let laneX =
                if hang
                  then x + w + outer
                  else max x (x + w - outer - barW)
           in Rect laneX (y + padT pad) barW (max 0 (h - padT pad - padB pad))
        DirRow ->
          let laneY =
                if hang
                  then y + h + outer
                  else max y (y + h - outer - barW)
           in Rect (x + padL pad) laneY (max 0 (w - padL pad - padR pad)) barW
