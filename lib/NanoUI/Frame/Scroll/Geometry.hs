{-# LANGUAGE DataKinds #-}

-- | Unified scrollbar geometry: gutters, viewport clips, track/thumb layout.
module NanoUI.Frame.Scroll.Geometry
  ( ScrollPolicy (..)
  , ScrollConfig (..)
  , defaultScrollConfig
  , ScrollBarLayout (..)
  , scrollContentClip
  , scrollViewportClip2D
  , scrollChromeLane
  , scrollBarLayout
  , scrollOffsetFromThumb
  , padContentClip
  , encodeScrollConfig
  , decodeScrollConfig
  , scrollConfigNative2D
  , scrollDefault1D
  , scrollVerticalAuto
  , scrollHorizontalAuto
  , scrollVerticalHidden
  , scrollHorizontalHidden
  , scrollAxisGutter
  , scrollShowsChrome
  , scrollChromeSuppressed
  ) where

import Data.Bits ((.&.), shiftL, shiftR)
import NanoUI.Font
  ( FontMetrics
  , ScrollBarSlot (..)
  , resolveLayoutPadding
  , scrollBarGeomFor
  , scrollBarGutter
  , scrollBarListExtra
  , scrollBarOuterGap
  , scrollBarPageExtra
  , scrollLayoutGutter
  )
import NanoUI.Host (HostProfile, isCellHost)
import NanoUI.Layout.Arena (DirTag (..))
import NanoUI.Style (Direction (..), Padding (..))
import NanoUI.Types (Rect (..), V2 (..), rectH, rectW, rectX, rectY, v2X, v2Y)

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
  }
  deriving (Eq, Show)

defaultScrollConfig :: ScrollConfig
defaultScrollConfig =
  ScrollConfig
    { scrollPolicyX = ScrollAuto
    , scrollPolicyY = ScrollAuto
    , scrollClamp = True
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
    + if scrollClamp cfg then 16 else 0
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
scrollVerticalAuto = ScrollConfig ScrollNone ScrollAuto True

scrollHorizontalAuto :: ScrollConfig
scrollHorizontalAuto = ScrollConfig ScrollAuto ScrollNone True

scrollVerticalHidden :: ScrollConfig
scrollVerticalHidden = ScrollConfig ScrollNone ScrollHidden True

scrollHorizontalHidden :: ScrollConfig
scrollHorizontalHidden = ScrollConfig ScrollHidden ScrollNone True

scrollAxisGutter ::
  ScrollPolicy ->
  HostProfile ->
  FontMetrics ->
  ScrollBarSlot ->
  Float ->
  Float ->
  Float
scrollAxisGutter policy host fm slot contentSize innerMain =
  case policy of
    ScrollNone -> 0
    ScrollHidden -> 0
    ScrollAuto -> scrollLayoutGutter host fm slot contentSize innerMain
    ScrollAlways ->
      case slot of
        ScrollBarWindow -> 0
        ScrollBarList -> scrollBarGutter host fm + scrollBarListExtra
        ScrollBarPage -> scrollBarGutter host fm + scrollBarPageExtra

scrollShowsChrome :: ScrollConfig -> Bool -> DirTag -> Bool
scrollShowsChrome cfg native2D dir =
  if native2D
    then axisShows (scrollPolicyX cfg) || axisShows (scrollPolicyY cfg)
    else
      case dir of
        DirColumn -> axisShows (scrollPolicyY cfg)
        DirRow -> axisShows (scrollPolicyX cfg)
  where
    axisShows = \case
      ScrollAuto -> True
      ScrollAlways -> True
      _ -> False

scrollChromeSuppressed :: ScrollConfig -> Bool -> DirTag -> Bool
scrollChromeSuppressed cfg native2D dir = not (scrollShowsChrome cfg native2D dir)

data ScrollBarLayout = ScrollBarLayout
  { sbTrack :: Rect
  , sbThumb :: Rect
  , sbMaxOff :: Float
  }
  deriving (Eq, Show)

padContentClip :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> Padding -> Rect
padContentClip host fm x y w h pad0 =
  let pad = resolveLayoutPadding host fm pad0
   in Rect
        (x + padL pad)
        (y + padT pad)
        (max 0 (w - padL pad - padR pad))
        (max 0 (h - padT pad - padB pad))

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

scrollViewportClip2D ::
  HostProfile ->
  FontMetrics ->
  ScrollBarSlot ->
  Float ->
  Float ->
  Float ->
  Float ->
  Padding ->
  Float ->
  Float ->
  Rect
scrollViewportClip2D host fm slot x y w h pad contentW contentH =
  let base = padContentClip host fm x y w h pad
      innerW = rectW base
      innerH = rectH base
      gutterX = scrollLayoutGutter host fm slot contentW innerW
      gutterY = scrollLayoutGutter host fm slot contentH innerH
   in Rect (rectX base) (rectY base) (max 0 (innerW - gutterX)) (max 0 (innerH - gutterY))

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

scrollBarLayout ::
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
  Float ->
  Maybe ScrollBarLayout
scrollBarLayout host fm slot dir x y w h pad contentSize off =
  let (barW, barMargin) = scrollBarGeomFor host fm slot
      minThumb = if isCellHost host then barW else 16
   in case dir of
        DirColumn ->
          let innerH = h - padT pad - padB pad
              maxOff = max 0 (contentSize - innerH)
           in if maxOff <= 0
                then Nothing
                else
                  let lane = scrollChromeLane host fm slot DirColumn x y w h pad
                      trackX = rectX lane
                      trackY = y + padT pad + barMargin
                      trackH = max 0 (innerH - 2 * barMargin)
                      thumbH = max minThumb (trackH * innerH / contentSize)
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
          let innerW = w - padL pad - padR pad
              maxOff = max 0 (contentSize - innerW)
           in if maxOff <= 0
                then Nothing
                else
                  let lane = scrollChromeLane host fm slot DirRow x y w h pad
                      trackY = rectY lane
                      trackX = x + padL pad + barMargin
                      trackW = max 0 (innerW - 2 * barMargin)
                      thumbW = max minThumb (trackW * innerW / contentSize)
                      ratio = off / maxOff
                      thumbX = trackX + ratio * (trackW - thumbW)
                   in
                    Just
                      ScrollBarLayout
                        { sbTrack = Rect trackX trackY trackW barW
                        , sbThumb = Rect thumbX trackY thumbW barW
                        , sbMaxOff = maxOff
                        }

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
