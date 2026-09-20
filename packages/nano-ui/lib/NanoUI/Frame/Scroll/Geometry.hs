{-# LANGUAGE DataKinds #-}

-- | Scrollbar geometry: gutters, viewport clips, and track and thumb layout.
module NanoUI.Frame.Scroll.Geometry
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
  , scrollLineFor
  , scrollAxisOverflows
  , scrollChromeActive
  , isScrollStyle2D
  , tagClippedSpans
  , padTextClipRect
  , borderContentClip
  ) where

import Data.Bits ((.&.), shiftL, shiftR)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import NanoUI.Font
  ( ScrollBarSlot (..)
  , scrollBarGap
  , scrollBarGeomFor
  , scrollBarGutter
  , scrollBarSideGap
  , scrollLayoutGutter
  )
import NanoUI.Types (Color, Rect (..), V2 (..), rectH, rectIntersect, rectW, rectX, rectY, v2X, v2Y)
import NanoUI.Layout.Arena (DirTag (..))
import NanoUI.Style (Direction (..), Padding (..), Style (..), styleBorderWidth, windowPad)

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
  , scrollClamp :: !Bool
  -- | A bare scroller paints no well of its own: no input background, no
  -- border, no window fill. Only the clipped children render, so a strip that
  -- borrows the scroller for its offset and clip (tab headers) looks exactly
  -- like it did before it started scrolling. Chrome policies still apply on
  -- top: 'ScrollHidden' plus bare is the fully chrome-less scroller.
  , scrollBare :: !Bool
  }
  deriving (Eq, Show)

defaultScrollConfig :: ScrollConfig
defaultScrollConfig =
  ScrollConfig
    { scrollPolicyX = ScrollAuto
    , scrollPolicyY = ScrollAuto
    , scrollClamp = True
    , scrollBare = False
    }

scrollConfigNative2D :: ScrollConfig -> Bool
scrollConfigNative2D cfg =
  scrollAxisActive (scrollPolicyX cfg) && scrollAxisActive (scrollPolicyY cfg)
  where
    scrollAxisActive = \case
      ScrollNone -> False
      _ -> True

encodeScrollConfig :: ScrollConfig -> Int
encodeScrollConfig cfg =
  policyBits (scrollPolicyX cfg)
    + shiftL (policyBits (scrollPolicyY cfg)) 2
    + (if scrollClamp cfg then 16 else 0)
    + (if scrollBare cfg then 32 else 0)
  where
    policyBits = \case
      ScrollAuto -> 0
      ScrollAlways -> 1
      ScrollNone -> 2
      ScrollHidden -> 3

decodeScrollConfig :: Int -> ScrollConfig
decodeScrollConfig bits =
  ScrollConfig
    { scrollPolicyX = decodePolicy (bits .&. 3)
    , scrollPolicyY = decodePolicy (shiftR bits 2 .&. 3)
    , scrollClamp = bits .&. 16 /= 0
    , scrollBare = bits .&. 32 /= 0
    }
  where
    decodePolicy 1 = ScrollAlways
    decodePolicy 2 = ScrollNone
    decodePolicy 3 = ScrollHidden
    decodePolicy _ = ScrollAuto

scrollDefault1D :: Direction -> ScrollConfig
scrollDefault1D Column = scrollVerticalAuto
scrollDefault1D Row = scrollHorizontalAuto

scrollVerticalAuto :: ScrollConfig
scrollVerticalAuto = ScrollConfig ScrollNone ScrollAuto True False

scrollHorizontalAuto :: ScrollConfig
scrollHorizontalAuto = ScrollConfig ScrollAuto ScrollNone True False

scrollVerticalHidden :: ScrollConfig
scrollVerticalHidden = ScrollConfig ScrollNone ScrollHidden True False

scrollHorizontalHidden :: ScrollConfig
scrollHorizontalHidden = ScrollConfig ScrollHidden ScrollNone True False

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

scrollShowsChrome :: ScrollConfig -> DirTag -> Bool
scrollShowsChrome cfg dir =
  case scrollPolicyFor cfg dir of
    ScrollAuto -> True
    ScrollAlways -> True
    _ -> False

scrollChromeSuppressed :: ScrollConfig -> DirTag -> Bool
scrollChromeSuppressed cfg dir = not (scrollShowsChrome cfg dir)

-- | Distance one wheel notch scrolls along a live axis. Window hosts step a
-- text line. Widgets that map wheel notches onto a scroller's offset share
-- this so the step cannot drift per caller.
scrollLineFor :: Float
scrollLineFor = 20

-- | Wheel eligibility is wider than chrome eligibility: a hidden bar never
-- paints or drags, but it still scrolls. Only a dead axis ('ScrollNone')
-- ignores the wheel outright. Native 2D scrollers always keep both axes
-- live by construction.
scrollWheelSuppressed :: ScrollConfig -> Bool -> DirTag -> Bool
scrollWheelSuppressed cfg native2D dir = not native2D && scrollPolicyFor cfg dir == ScrollNone

scrollAxisOverflows :: ScrollPolicy -> Float -> Float -> Bool
scrollAxisOverflows policy contentSize innerMain =
  case policy of
    ScrollNone -> False
    ScrollHidden -> False
    ScrollAlways -> True
    ScrollAuto -> contentSize > innerMain + 0.5

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

scrollChromeActive :: ScrollConfig -> DirTag -> Float -> Float -> Bool
scrollChromeActive cfg dir contentSize innerMain =
  scrollShowsChrome cfg dir && scrollAxisOverflows (scrollPolicyFor cfg dir) contentSize innerMain

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
scrollBarLayout slot dir x y w h pad contentSize off =
  let innerW = w - padL pad - padR pad
      innerH = h - padT pad - padB pad
      viewMain = case dir of
        DirColumn -> innerH
        DirRow -> innerW
   in scrollBarLayoutIn slot dir x y w h pad viewMain contentSize off

-- | 'scrollBarLayout' with an explicit visible main extent. A native 2D
-- scroller passes the padding box minus the cross-axis lane (see
-- 'scrollGutters2D'), so its reachable range and thumb reflect the viewport
-- that is actually visible rather than the lane-underlapped padding box. On a
-- one-dimensional scroller @viewMain@ is just the padding box on that axis.
scrollBarLayoutIn ::
  ScrollBarSlot ->
  DirTag ->
  Float ->
  Float ->
  Float ->
  Float ->
  Padding ->
  Float ->
  Float ->
  Float ->
  Maybe ScrollBarLayout
scrollBarLayoutIn slot dir x y w h pad viewMain contentSize off =
  let (barW, barMargin) = scrollBarGeomFor slot
      minThumb = 16
   in case dir of
        DirColumn ->
          let trailH = padB pad
              extentH = contentSize + trailH
              maxOff = scrollAxisRange contentSize viewMain trailH
           in if maxOff <= 0
                then Nothing
                else
                  let lane = scrollChromeLane slot DirColumn x y w h pad
                      trackX = rectX lane
                      trackY = y + padT pad + barMargin
                      trackH = max 0 (viewMain - 2 * barMargin)
                      thumbH = max minThumb (trackH * viewMain / extentH)
                      ratio = off / maxOff
                      thumbY = trackY + ratio * (trackH - thumbH)
                   in
                    Just
                      ScrollBarLayout
                        { sbTrack = Rect trackX trackY barW trackH
                        , sbThumb = Rect trackX thumbY barW thumbH
                        , sbMaxOff = maxOff
                        }
        DirRow ->
          let trailW = padR pad
              extentW = contentSize + trailW
              maxOff = scrollAxisRange contentSize viewMain trailW
           in if maxOff <= 0
                then Nothing
                else
                  let lane = scrollChromeLane slot DirRow x y w h pad
                      trackY = rectY lane
                      trackX = x + padL pad + barMargin
                      trackW = max 0 (viewMain - 2 * barMargin)
                      thumbW = max minThumb (trackW * viewMain / extentW)
                      ratio = off / maxOff
                      thumbX = trackX + ratio * (trackW - thumbW)
                   in
                    Just
                      ScrollBarLayout
                        { sbTrack = Rect trackX trackY trackW barW
                        , sbThumb = Rect thumbX trackY thumbW barW
                        , sbMaxOff = maxOff
                        }

-- | Both-axis layouts for a native 2D scroller: (vertical, horizontal). Each
-- axis's visible main extent is reduced by the other axis's live gutter, so
-- the range and thumb are computed against the viewport minus the opposite
-- scrollbar lane.
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
  let innerW = w - padL pad - padR pad
      innerH = h - padT pad - padB pad
      (gutterW, gutterH) = scrollGutters2D slot cfg pad contentW contentH innerW innerH
      viewW = max 0 (innerW - gutterW)
      viewH = max 0 (innerH - gutterH)
      v = scrollBarLayoutIn slot DirColumn x y w h pad viewH contentH offY
      hr = scrollBarLayoutIn slot DirRow x y w h pad viewW contentW offX
   in (v, hr)

scrollOffsetFromThumb :: DirTag -> ScrollBarLayout -> Float -> V2 -> Float
scrollOffsetFromThumb dir layout grabOff mouse =
  let maxOff = sbMaxOff layout
      track = sbTrack layout
      thumb = sbThumb layout
   in case dir of
        DirColumn ->
          let trackY = rectY track
              trackH = rectH track
              thumbH = rectH thumb
              thumbTop = v2Y mouse - grabOff
              ratio = (thumbTop - trackY) / max 1 (trackH - thumbH)
           in max 0 (min maxOff (ratio * maxOff))
        DirRow ->
          let trackX = rectX track
              trackW = rectW track
              thumbW = rectW thumb
              thumbLeft = v2X mouse - grabOff
              ratio = (thumbLeft - trackX) / max 1 (trackW - thumbW)
           in max 0 (min maxOff (ratio * maxOff))

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
