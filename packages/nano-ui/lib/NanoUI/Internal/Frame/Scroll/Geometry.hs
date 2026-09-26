-- | Scrollbar geometry: gutters, viewport clips, and track and thumb layout.
module NanoUI.Internal.Frame.Scroll.Geometry
  ( ScrollConfig (..)
  , defaultScrollConfig
  , ScrollBarLayout (..)
  , ScrollNode (..)
  , scrollNodeViewport
  , scrollNodeBars
  , scrollChromeLane
  , scrollBarLayout
  , scrollAxisRange
  , scrollOffsetFromThumb
  , onScrollBar
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
  , isScrollStyle2D
  , tagClippedSpans
  , padTextClipRect
  , borderContentClip
  ) where

import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import NanoUI.Internal.Font
import NanoUI.Internal.Types (Color, Rect (..), V2 (..), clamp, rectContains, rectH, rectIntersect, rectW, rectX, rectY, v2X, v2Y)
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
scrollDefault1D Stack = scrollVerticalAuto

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
    ScrollAuto -> if scrollOverflows contentSize innerMain then scrollBarGutter slot trailPad else 0
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
  | scrollOverflows contentSize innerMain = max 0 (contentSize + trailingPad - innerMain)
  | otherwise = 0

-- | Whether content of @contentSize@ overflows @innerMain@ by more than half
-- a pixel, the rounding a fractional layout leaves. The range and the gutter
-- both ask this, so an auto bar never reserves a lane it does not paint.
{-# INLINE scrollOverflows #-}
scrollOverflows :: Float -> Float -> Bool
scrollOverflows contentSize innerMain = contentSize > innerMain + 0.5

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

-- | What the scroll passes read off a scroll container: its bar slot, scroll
-- config, whether it scrolls natively in 2D, direction, padding, the content
-- extent along its main axis (the content height for 2D) and, for 2D, the
-- content width.
data ScrollNode = ScrollNode
  { snSlot :: !ScrollBarSlot
  , snConfig :: !ScrollConfig
  , sn2D :: !Bool
  , snDir :: !DirTag
  , snPad :: {-# UNPACK #-} !Padding
  , snContentMain :: {-# UNPACK #-} !Float
  , snContentW :: {-# UNPACK #-} !Float
  }

-- | Content viewport of a scroll node placed at @x y w h@: its padding box
-- minus the live scrollbar gutters.
scrollNodeViewport :: ScrollNode -> Float -> Float -> Float -> Float -> Rect
scrollNodeViewport (ScrollNode slot cfg native2D dir pad contentMain contentW) x y w h
  | native2D = Rect bx by (max 0 (innerW - gutterW)) (max 0 (innerH - gutterH))
  | dir == DirColumn = Rect bx by (max 0 (innerW - gutter (padR pad) innerH)) innerH
  | otherwise = Rect bx by innerW (max 0 (innerH - gutter (padB pad) innerW))
  where
    Rect bx by innerW innerH = padContentClip x y w h pad
    (gutterW, gutterH) = scrollGutters2D slot cfg pad contentW contentMain innerW innerH
    -- Strict, so its two calls pass unboxed floats (measured: boxing them
    -- cost the pointer scene 100 kB).
    gutter !trailPad !innerMain = scrollAxisGutter (scrollPolicyFor cfg dir) slot trailPad contentMain innerMain

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
  scrollBarLayoutIn slot dir (Rect x y w h) pad $ case dir of
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
  -> Rect
  -> Padding
  -> Float
  -> Float
  -> Float
  -> Maybe ScrollBarLayout
scrollBarLayoutIn slot dir (Rect x y w h) pad viewMain contentSize off =
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

-- | The vertical and the horizontal bar a scroll node placed at @x y w h@
-- shows at offsets @offX offY@ (a 1D scroller's offset is @offY@, whichever
-- way it runs). An axis whose chrome is suppressed shows none. A native 2D
-- scroller's axes each take their visible main extent from the viewport, which
-- the other axis's live gutter has narrowed, so the range and thumb are
-- computed against the viewport minus the opposite scrollbar lane.
scrollNodeBars ::
  ScrollNode -> Float -> Float -> Float -> Float -> Float -> Float -> (Maybe ScrollBarLayout, Maybe ScrollBarLayout)
scrollNodeBars sn@(ScrollNode slot cfg native2D dir pad contentMain contentW) x y w h offX offY
  | native2D =
      let Rect _ _ viewW viewH = scrollNodeViewport sn x y w h
       in ( shown DirColumn (scrollBarLayoutIn slot DirColumn (Rect x y w h) pad viewH contentMain offY)
          , shown DirRow (scrollBarLayoutIn slot DirRow (Rect x y w h) pad viewW contentW offX)
          )
  | dir == DirColumn = (shown dir bar, Nothing)
  | otherwise = (Nothing, shown dir bar)
  where
    bar = scrollBarLayout slot dir x y w h pad contentMain offY
    shown d l = if scrollChromeSuppressed cfg d then Nothing else l

-- | Whether @mouse@ is on a bar's thumb or track.
onScrollBar :: V2 -> ScrollBarLayout -> Bool
onScrollBar mouse l = rectContains (sbThumb l) mouse || rectContains (sbTrack l) mouse

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
