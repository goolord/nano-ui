-- | Scrollbar geometry: gutters, viewport clips, and track and thumb layout.
module NanoUI.Internal.Frame.Scroll.Geometry
  ( ScrollPolicy (..)
  , ScrollConfig (..)
  , defaultScrollConfig
  , ScrollBarLayout (..)
  , scrollContentClip
  , scrollViewportClip2D
  , scrollChromeLane
  , scrollBarLayout
  , scrollBarLayouts2D
  , scrollAxisRange
  , scrollOffsetFromThumb
  , padContentClip
  , encodeScrollConfig
  , decodeScrollConfig
  , scrollConfigNative2D
  , scrollDefault1D
  , scrollVerticalAuto
  , scrollVerticalHidden
  , scrollHorizontalHidden
  , scrollAxisGutter
  , scrollGutters2D
  , scrollChromeSuppressed
  , scrollWheelSuppressed
  , scrollChromeActive
  , isScrollStyle2D
  , tagClippedSpans
  , padTextClipRect
  , borderContentClip
  ) where

import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import NanoUI.Internal.Font
import NanoUI.Internal.Types (Color, Rect (..), V2 (..), clamp, rectH, rectIntersect, rectW, rectX, rectY, v2X, v2Y)
import NanoUI.Internal.Layout.Arena (DirTag (..))
import NanoUI.Internal.Style (Direction (..), Padding (..), Style (..), styleBorderWidth, windowPad)

-- | Axis scrollbar visibility and interaction policy.
data ScrollPolicy
  = ScrollAuto
  | ScrollAlways
  | ScrollNone
  | ScrollHidden
  deriving (Eq, Show, Enum, Bounded)

-- | 2D scroll configuration (policy per axis).
data ScrollConfig = ScrollConfig
  { scrollPolicyX :: !ScrollPolicy
  , scrollPolicyY :: !ScrollPolicy
  -- | Paint only clipped children, without a background or border. Scrollbar
  -- policies still apply; combine with 'ScrollHidden' to omit all chrome.
  , scrollBare :: !Bool
  }
  deriving (Eq, Show)

-- | Automatic bars on both axes and a painted background.
defaultScrollConfig :: ScrollConfig
defaultScrollConfig = ScrollConfig ScrollAuto ScrollAuto False

scrollConfigNative2D :: ScrollConfig -> Bool
scrollConfigNative2D cfg = scrollPolicyX cfg /= ScrollNone && scrollPolicyY cfg /= ScrollNone

-- | A scroll node's style index. Bit 4 is always set, so that no scroll
-- node's style index is 0.
encodeScrollConfig :: ScrollConfig -> Int
encodeScrollConfig (ScrollConfig px py bare) =
  16 .|. fromEnum px .|. shiftL (fromEnum py) 2 .|. (if bare then 32 else 0)

decodeScrollConfig :: Int -> ScrollConfig
decodeScrollConfig bits = ScrollConfig (toEnum (bits .&. 3)) (toEnum (shiftR bits 2 .&. 3)) (testBit bits 5)

scrollDefault1D :: Direction -> ScrollConfig
scrollDefault1D Column = scrollVerticalAuto
scrollDefault1D Row = ScrollConfig ScrollAuto ScrollNone False

scrollVerticalAuto :: ScrollConfig
scrollVerticalAuto = ScrollConfig ScrollNone ScrollAuto False

scrollVerticalHidden :: ScrollConfig
scrollVerticalHidden = ScrollConfig ScrollNone ScrollHidden False

scrollHorizontalHidden :: ScrollConfig
scrollHorizontalHidden = ScrollConfig ScrollHidden ScrollNone False

-- | Cross-axis gutter for one bar. @trailPad@ is the scroller's padding on
-- the bar's side (right for the vertical bar, bottom for the horizontal one).
scrollAxisGutter ::
  ScrollPolicy ->
  ScrollBarSlot ->
  Float ->
  Float ->
  Float ->
  Float
scrollAxisGutter policy slot trailPad contentSize innerMain =
  case policy of
    ScrollNone -> 0
    ScrollHidden -> 0
    ScrollAuto -> scrollLayoutGutter slot trailPad contentSize innerMain
    ScrollAlways -> scrollBarGutter slot trailPad

-- Vertical bar takes width. Horizontal bar takes height. Second pass
-- covers the corner case where one bar makes the other axis overflow.
scrollGutters2D ::
  ScrollBarSlot ->
  ScrollConfig ->
  Padding ->
  Float ->
  Float ->
  Float ->
  Float ->
  (Float, Float)
scrollGutters2D slot cfg pad contentW contentH innerW innerH =
  let gVert inner = scrollAxisGutter (scrollPolicyY cfg) slot (padR pad) contentH inner
      gHorz inner = scrollAxisGutter (scrollPolicyX cfg) slot (padB pad) contentW inner
      gW0 = gVert innerH
      gH0 = gHorz innerW
      gW = gVert (innerH - gH0)
      gH = gHorz (innerW - gW0)
   in (gW, gH)

isScrollStyle2D :: Int -> Bool
isScrollStyle2D si = si /= 0 && scrollConfigNative2D (decodeScrollConfig si)

-- | The policy of the axis a scroller laid out along @dir@ scrolls on.
{-# INLINE scrollPolicyFor #-}
scrollPolicyFor :: ScrollConfig -> DirTag -> ScrollPolicy
scrollPolicyFor cfg = \case
  DirColumn -> scrollPolicyY cfg
  DirRow -> scrollPolicyX cfg

-- | Whether the bar along @dir@ is never painted or grabbed.
scrollChromeSuppressed :: ScrollConfig -> DirTag -> Bool
scrollChromeSuppressed cfg dir =
  case scrollPolicyFor cfg dir of
    ScrollAuto -> False
    ScrollAlways -> False
    _ -> True

-- | Wheel eligibility is wider than chrome eligibility: a hidden bar never
-- paints or drags, but it still scrolls. Only a dead axis ('ScrollNone')
-- ignores the wheel outright. Native 2D scrollers always keep both axes
-- live by construction.
scrollWheelSuppressed :: ScrollConfig -> Bool -> DirTag -> Bool
scrollWheelSuppressed cfg native2D dir = not native2D && scrollPolicyFor cfg dir == ScrollNone

-- | Scroll range along one axis. Content that fits (modulo the trailing
-- padding, which must not surface a bar by itself) does not scroll; genuine
-- overflow extends the range past the last child by the trailing padding so
-- scrolling to the end still reveals it. Stored content sizes exclude the
-- trailing padding (see positionScrollChildren); this is where it is added
-- back into the reachable range.
scrollAxisRange :: Float -> Float -> Float -> Float
scrollAxisRange contentSize innerMain trailingPad
  | contentSize > innerMain + 0.5 = max 0 (contentSize + trailingPad - innerMain)
  | otherwise = 0

-- | Whether the bar along @dir@ shows for content of @contentSize@ in
-- @innerMain@.
scrollChromeActive :: ScrollConfig -> DirTag -> Float -> Float -> Bool
scrollChromeActive cfg dir contentSize innerMain =
  case scrollPolicyFor cfg dir of
    ScrollAlways -> True
    ScrollAuto -> contentSize > innerMain + 0.5
    _ -> False

-- | Logical window-space track/thumb bounds and the maximum scroll offset.
data ScrollBarLayout = ScrollBarLayout
  { sbTrack :: Rect
  , sbThumb :: Rect
  , sbMaxOff :: Float
  }
  deriving (Eq, Show)

padContentClip :: Float -> Float -> Float -> Float -> Padding -> Rect
padContentClip x y w h pad =
  Rect
    (x + padL pad)
    (y + padT pad)
    (max 0 (w - padL pad - padR pad))
    (max 0 (h - padT pad - padB pad))

scrollContentClip ::
  ScrollBarSlot ->
  ScrollConfig ->
  DirTag ->
  Float ->
  Float ->
  Float ->
  Float ->
  Padding ->
  Float ->
  Rect
scrollContentClip slot cfg dir x y w h pad contentSize =
  let base = padContentClip x y w h pad
      innerMain =
        case dir of
          DirColumn -> rectH base
          DirRow -> rectW base
      trailPad =
        case dir of
          DirColumn -> padR pad
          DirRow -> padB pad
      gutter = scrollAxisGutter (scrollPolicyFor cfg dir) slot trailPad contentSize innerMain
   in case dir of
        DirColumn -> Rect (rectX base) (rectY base) (max 0 (rectW base - gutter)) (rectH base)
        DirRow -> Rect (rectX base) (rectY base) (rectW base) (max 0 (rectH base - gutter))

scrollViewportClip2D ::
  ScrollBarSlot ->
  ScrollConfig ->
  Float ->
  Float ->
  Float ->
  Float ->
  Padding ->
  Float ->
  Float ->
  Rect
scrollViewportClip2D slot cfg x y w h pad contentW contentH =
  let base = padContentClip x y w h pad
      innerW = rectW base
      innerH = rectH base
      (gutterW, gutterH) = scrollGutters2D slot cfg pad contentW contentH innerW innerH
   in Rect (rectX base) (rectY base) (max 0 (innerW - gutterW)) (max 0 (innerH - gutterH))

-- | The strip a bar sits in. A list bar sits one gap (see 'scrollBarGap')
-- inside its well's edge. A page bar sits a side gap inside the page's edge,
-- and a window body's bar a side gap inside the window's edge, out in the
-- window's padding. The gutter keeps the content one gap before each of them.
scrollChromeLane ::
  ScrollBarSlot -> DirTag -> Float -> Float -> Float -> Float -> Padding -> Rect
scrollChromeLane slot dir x y w h pad =
  let (barW, _) = scrollBarGeomFor slot
      -- From the scroller's edge in to the bar's far side. Window and modal
      -- bodies only scroll vertically, so the window's side padding is the
      -- one that places their bar.
      inset trailPad = case slot of
        ScrollBarList -> scrollBarGap trailPad
        ScrollBarPage -> scrollBarSideGap
        ScrollBarWindow -> scrollBarSideGap - padR windowPad
   in case dir of
        DirColumn ->
          Rect (max x (x + w - inset (padR pad) - barW)) (y + padT pad) barW (max 0 (h - padT pad - padB pad))
        DirRow ->
          Rect (x + padL pad) (max y (y + h - inset (padB pad) - barW)) (max 0 (w - padL pad - padR pad)) barW

-- | Track and thumb from slot, axis, x/y/width/height, padding, content extent,
-- and current offset. Lengths use logical pixels; 'Nothing' means no usable
-- scrollbar is needed or fits. Use 'scrollBarLayoutIn' for a reduced viewport.
scrollBarLayout ::
  ScrollBarSlot ->
  DirTag ->
  Float ->
  Float ->
  Float ->
  Float ->
  Padding ->
  Float ->
  Float ->
  Maybe ScrollBarLayout
scrollBarLayout slot dir x y w h pad =
  scrollBarLayoutIn slot dir x y w h pad $ case dir of
    DirColumn -> h - padT pad - padB pad
    DirRow -> w - padL pad - padR pad

-- | 'scrollBarLayout' with an explicit visible main extent. A native 2D
-- scroller passes the padding box minus the cross-axis lane (see
-- 'scrollGutters2D'), so its reachable range and thumb reflect the viewport
-- that is actually visible rather than the lane-underlapped padding box. On a
-- one-dimensional scroller @viewMain@ is just the padding box on that axis.
scrollBarLayoutIn ::
  ScrollBarSlot
  -> DirTag
  -> Float
  -> Float
  -> Float
  -> Float
  -> Padding
  -> Float
  -> Float
  -> Float
  -> Maybe ScrollBarLayout
scrollBarLayoutIn slot dir x y w h pad viewMain contentSize off =
  let
    (barW, barMargin) = scrollBarGeomFor slot
    minThumb = 16
    lane = scrollChromeLane slot dir x y w h pad
    (origin, trailing) = case dir of
      DirColumn -> (y + padT pad, padB pad)
      DirRow -> (x + padL pad, padR pad)
    maxOff = scrollAxisRange contentSize viewMain trailing
    trackStart = origin + barMargin
    trackSize = max 0 (viewMain - 2 * barMargin)
    thumbSize = max minThumb (trackSize * viewMain / (contentSize + trailing))
    thumbStart = trackStart + (off / maxOff) * (trackSize - thumbSize)
    band start size = case dir of
      DirColumn -> Rect (rectX lane) start barW size
      DirRow -> Rect start (rectY lane) size barW
   in
    if maxOff <= 0
      then Nothing
      else
        Just
          (ScrollBarLayout (band trackStart trackSize) (band thumbStart thumbSize) maxOff)

-- | Both-axis layouts for a native 2D scroller: (vertical, horizontal). Each
-- axis's visible main extent is its side of the viewport, which the other
-- axis's live gutter has narrowed, so the range and thumb are computed
-- against the viewport minus the opposite scrollbar lane.
scrollBarLayouts2D ::
  ScrollBarSlot ->
  ScrollConfig ->
  Float ->
  Float ->
  Float ->
  Float ->
  Padding ->
  Float ->
  Float ->
  Float ->
  Float ->
  (Maybe ScrollBarLayout, Maybe ScrollBarLayout)
scrollBarLayouts2D slot cfg x y w h pad contentW contentH offX offY =
  let Rect _ _ viewW viewH = scrollViewportClip2D slot cfg x y w h pad contentW contentH
   in ( scrollBarLayoutIn slot DirColumn x y w h pad viewH contentH offY
      , scrollBarLayoutIn slot DirRow x y w h pad viewW contentW offX
      )

scrollOffsetFromThumb :: DirTag -> ScrollBarLayout -> Float -> V2 -> Float
scrollOffsetFromThumb dir layout grabOff mouse =
  let
    maxOff = sbMaxOff layout
    track = sbTrack layout
    thumb = sbThumb layout
    (trackStart, trackSize, thumbSize, pointer) = case dir of
      DirColumn -> (rectY track, rectH track, rectH thumb, v2Y mouse)
      DirRow -> (rectX track, rectW track, rectW thumb, v2X mouse)
    ratio = (pointer - grabOff - trackStart) / max 1 (trackSize - thumbSize)
   in
    clamp 0 maxOff (ratio * maxOff)

textClipSlop :: Float
textClipSlop = 4

tagClippedSpans :: Rect -> [(Rect, Text, Color, Color)] -> [(Rect, Text, Color, Color, Rect)]
tagClippedSpans clip =
  mapMaybe (\(rect, txt, fg, bg) -> (rect, txt, fg, bg,) <$> rectIntersect clip (padTextClipRect rect))

padTextClipRect :: Rect -> Rect
padTextClipRect (Rect x y w h) = Rect x y (w + textClipSlop) h

borderContentClip :: Style -> Rect -> Rect
borderContentClip style (Rect x y w h) =
  if styleBorderWidth style <= 0
    then Rect x y w h
    else
      let bw = max 1 (styleBorderWidth style)
       in Rect (x + bw) (y + bw) (max 0 (w - 2 * bw)) (max 0 (h - 2 * bw))
