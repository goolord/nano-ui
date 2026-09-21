-- | Pure text-area geometry: the field box, content clip, and which scrollbars
-- show where for a given content extent.
module NanoUI.Internal.Frame.TextArea.Geometry
  ( textAreaLineHeight
  , textAreaFieldClip
  , textAreaBarLane
  , TextAreaBars (..)
  , textAreaBars
  , TextAreaScrollBarLayouts (..)
  , textAreaScrollBarLayouts
  , textAreaScrollBarLayout
  , textAreaHScrollBarLayout
  , isMouseOnTextAreaScrollBar
  ) where

import NanoUI.Internal.Font (FontMetrics (..), ScrollBarSlot (..), scrollBarGeomFor, scrollBarSideGap, widgetContentInset)
import NanoUI.Internal.Frame.Scroll.Geometry (ScrollBarLayout (..), scrollBarLayout, scrollChromeLane)
import NanoUI.Internal.Layout.Arena (DirTag (..))
import NanoUI.Internal.Style (Padding (..))
import NanoUI.Internal.Types (Rect (..), V2, onGrid, rectContains)

-- | Text area row height, snapped to the device pixel grid.
textAreaLineHeight :: FontMetrics -> Float
textAreaLineHeight fm = onGrid (fmSnapScale fm) (fmLineHeight fm)

-- | Text clip of a text area field. A caption-less text area's field is its
-- whole node rect.
textAreaFieldClip :: FontMetrics -> Rect -> Rect
textAreaFieldClip fm (Rect fx fy fw fh) =
  let s = fmSnapScale fm
      (ix, iy) = widgetContentInset fm
   in Rect (fx + onGrid s ix) (fy + onGrid s iy) (max 0 (fw - 2 * ix)) (max 0 (fh - 2 * iy))

-- | Width of the vertical and height of the horizontal scrollbar lane.
textAreaBarLane :: Float
textAreaBarLane = fst (scrollBarGeomFor ScrollBarList) + scrollBarSideGap

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
      laneW = textAreaBarLane
      laneH = textAreaBarLane
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

-- | Optional vertical and horizontal bars after accounting for their shared corner.
data TextAreaScrollBarLayouts = TextAreaScrollBarLayouts
  { tasbVertical :: !(Maybe ScrollBarLayout)
  , tasbHorizontal :: !(Maybe ScrollBarLayout)
  }
  deriving (Eq, Show)

-- | Compute both bars from field bounds, content width/height, and x/y offsets,
-- all in logical pixels. Absent bars have 'Nothing' layouts.
textAreaScrollBarLayouts :: FontMetrics -> Rect -> Float -> Float -> Float -> Float -> TextAreaScrollBarLayouts
textAreaScrollBarLayouts fm field@(Rect x y w h) contentW contentH scrollX scrollY =
  let bars = textAreaBars fm field contentW contentH
      layout shown dir pad content off
        | shown = scrollBarLayout ScrollBarList dir x y w h pad content off
        | otherwise = Nothing
   in TextAreaScrollBarLayouts
        { tasbVertical = layout (tabVertical bars) DirColumn (tabPadV bars) contentH scrollY
        , tasbHorizontal = layout (tabHorizontal bars) DirRow (tabPadH bars) contentW scrollX
        }

-- | Vertical bar from content height and y offset, assuming no horizontal overflow.
textAreaScrollBarLayout :: FontMetrics -> Rect -> Float -> Float -> Maybe ScrollBarLayout
textAreaScrollBarLayout fm field contentH scrollY =
  tasbVertical (textAreaScrollBarLayouts fm field 0 contentH 0 scrollY)

-- | Horizontal bar from content width and x offset, assuming no vertical overflow.
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
