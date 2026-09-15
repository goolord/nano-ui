-- | Pure text-area geometry: the field box, content clip, and which scrollbars
-- show where for a given content extent.
module NanoUI.Frame.TextArea.Geometry
  ( TextAreaGeom (..)
  , textAreaGeom
  , textAreaFieldClip
  , textAreaBarLanes
  , TextAreaBars (..)
  , textAreaBars
  , TextAreaScrollBarLayouts (..)
  , textAreaScrollBarLayouts
  , textAreaScrollBarLayout
  , textAreaHScrollBarLayout
  , isMouseOnTextAreaScrollBar
  ) where

import NanoUI.Font (FontMetrics (..), ScrollBarSlot (..), scrollBarGeomFor, scrollBarSideGap, widgetContentInset)
import NanoUI.Frame.Scroll.Geometry (ScrollBarLayout (..), scrollBarLayout, scrollChromeLane)
import NanoUI.Layout.Arena (DirTag (..))
import NanoUI.Style (Padding (..))
import NanoUI.Types (Rect (..), V2, onGrid, rectContains)

data TextAreaGeom = TextAreaGeom
  { tagFieldRect :: !Rect
  , tagLineHeight :: !Float
  }
  deriving (Eq, Show)

-- | A caption-less text area fills its whole node rect.
textAreaGeom :: FontMetrics -> Float -> Float -> Float -> Float -> TextAreaGeom
textAreaGeom fm x y w h =
  TextAreaGeom
    { tagFieldRect = Rect x y w h
    , tagLineHeight = onGrid (fmSnapScale fm) (fmLineHeight fm)
    }

textAreaFieldClip :: TextAreaGeom -> FontMetrics -> Rect
textAreaFieldClip geom fm =
  let s = fmSnapScale fm
      Rect fx fy fw fh = tagFieldRect geom
      (ix, iy) = widgetContentInset fm
   in Rect (fx + onGrid s ix) (fy + onGrid s iy) (max 0 (fw - 2 * ix)) (max 0 (fh - 2 * iy))

-- | Width of the vertical and height of the horizontal scrollbar lane.
textAreaBarLanes :: FontMetrics -> (Float, Float)
textAreaBarLanes _fm =
  let lane = fst (scrollBarGeomFor ScrollBarList) + scrollBarSideGap
   in (lane, lane)

-- | Which scrollbars a text area shows for its content extent, the text
-- viewport they leave, and the paddings that place each bar's lane.
data TextAreaBars = TextAreaBars
  { tabVertical :: !Bool
  , tabHorizontal :: !Bool
  , tabViewW :: !Float
  , tabViewH :: !Float
  , tabPadV :: !Padding
  , tabPadH :: !Padding
  }

textAreaBars :: FontMetrics -> Rect -> Float -> Float -> TextAreaBars
textAreaBars fm (Rect _ _ fw fh) contentW contentH =
  let (ix, iy) = widgetContentInset fm
      innerW = max 0 (fw - 2 * ix)
      innerH = max 0 (fh - 2 * iy)
      (laneW, laneH) = textAreaBarLanes fm
      -- Either bar's lane can push the other axis into overflow.
      hasV = contentH > (if contentW > innerW then max 0 (innerH - laneH) else innerH)
      hasH = contentW > (if contentH > innerH then max 0 (innerW - laneW) else innerW)
   in TextAreaBars
        { tabVertical = hasV
        , tabHorizontal = hasH
        , tabViewW = if hasV then max 0 (innerW - laneW) else innerW
        , tabViewH = if hasH then max 0 (innerH - laneH) else innerH
        , tabPadV = Padding 0 0 iy (if hasH then iy + laneH else iy)
        , tabPadH = Padding ix (if hasV then ix + laneW else ix) 0 0
        }

data TextAreaScrollBarLayouts = TextAreaScrollBarLayouts
  { tasbVertical :: !(Maybe ScrollBarLayout)
  , tasbHorizontal :: !(Maybe ScrollBarLayout)
  }
  deriving (Eq, Show)

textAreaScrollBarLayouts :: FontMetrics -> Rect -> Float -> Float -> Float -> Float -> TextAreaScrollBarLayouts
textAreaScrollBarLayouts fm field@(Rect x y w h) contentW contentH scrollX scrollY =
  let bars = textAreaBars fm field contentW contentH
      layout shown dir pad content off
        | shown = scrollBarLayout fm ScrollBarList dir x y w h pad content off
        | otherwise = Nothing
   in TextAreaScrollBarLayouts
        { tasbVertical = layout (tabVertical bars) DirColumn (tabPadV bars) contentH scrollY
        , tasbHorizontal = layout (tabHorizontal bars) DirRow (tabPadH bars) contentW scrollX
        }

textAreaScrollBarLayout :: FontMetrics -> Rect -> Float -> Float -> Maybe ScrollBarLayout
textAreaScrollBarLayout fm field contentH scrollY =
  tasbVertical (textAreaScrollBarLayouts fm field 0 contentH 0 scrollY)

textAreaHScrollBarLayout :: FontMetrics -> Rect -> Float -> Float -> Maybe ScrollBarLayout
textAreaHScrollBarLayout fm field contentW scrollX =
  tasbHorizontal (textAreaScrollBarLayouts fm field contentW 0 scrollX 0)

-- | Whether @mouse@ is over a shown bar's lane or track.
isMouseOnTextAreaScrollBar :: FontMetrics -> Rect -> Float -> Float -> Float -> Float -> V2 -> Bool
isMouseOnTextAreaScrollBar fm field@(Rect x y w h) contentW contentH scrollX scrollY mouse =
  let bars = textAreaBars fm field contentW contentH
      layouts = textAreaScrollBarLayouts fm field contentW contentH scrollX scrollY
      onBar dir pad =
        maybe False $ \layout ->
          rectContains (scrollChromeLane ScrollBarList dir x y w h pad) mouse
            || rectContains (sbTrack layout) mouse
   in onBar DirColumn (tabPadV bars) (tasbVertical layouts)
        || onBar DirRow (tabPadH bars) (tasbHorizontal layouts)
