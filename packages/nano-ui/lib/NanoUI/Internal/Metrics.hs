-- | Shared UI layout constants and control geometry derived from 'FontMetrics'.
module NanoUI.Internal.Metrics
  ( tableCellInset
  , widgetContentInset
  , widgetPadding
  , buttonPadding
  , selectPadding
  , menuOuterPad
  , menuItemPadX
  , menuItemRowH
  , menuSepH
  , menuMinW
  , checkboxBoxSize
  , checkboxLeading
  , treeItemPadding
  , treeRowLeading
  , treeChevronRect
  , sliderHandleDiameter
  , sliderTrackBounds
  , sliderHitBounds
  , scrollBarWidth
  , scrollBarSideGap
  , scrollBarGeomFor
  , ScrollBarSlot (..)
  , classifyScrollBar
  , scrollBarGap
  , scrollBarGutter
  ) where

import NanoUI.Internal.Font.Types (FontMetrics (..))
import NanoUI.Internal.Types (Rect (..), clamp)

-- | Horizontal text inset of a table cell. Zebra and header fills use the full cell rect.
tableCellInset :: Float
tableCellInset = 6

-- | Per-side horizontal/vertical content inset: 1.25 space advances on each axis.
{-# INLINE widgetContentInset #-}
widgetContentInset :: FontMetrics -> (Float, Float)
widgetContentInset fm =
  let pad = fmAdvance fm ' ' * 1.25
   in (pad, pad)

-- | Total horizontal and vertical content padding, twice 'widgetContentInset'.
{-# INLINE widgetPadding #-}
widgetPadding :: FontMetrics -> (Float, Float)
widgetPadding fm =
  let (cx, cy) = widgetContentInset fm
   in (2 * cx, 2 * cy)

-- | Total horizontal/vertical padding inside a button label box.
{-# INLINE buttonPadding #-}
buttonPadding :: FontMetrics -> (Float, Float)
buttonPadding fm = (fmAdvance fm ' ' * 2.0, fmLineHeight fm * 0.30)

-- | Total horizontal/vertical padding inside a select button.
{-# INLINE selectPadding #-}
selectPadding :: FontMetrics -> (Float, Float)
selectPadding fm = (fmAdvance fm ' ' * 2.0, fmLineHeight fm * 0.50)

-- | Blank border between the menu panel edge and its rows.
menuOuterPad :: Float
menuOuterPad = 6

-- | Extra horizontal inset of a menu row's label past 'menuOuterPad'.
menuItemPadX :: Float
menuItemPadX = 10

-- | Fixed height of one menu row.
menuItemRowH :: Float
menuItemRowH = 28

-- | Height of a separator band inside a menu.
menuSepH :: Float
menuSepH = 9

-- | Floor for the menu panel width.
menuMinW :: Float
menuMinW = 148

-- | Side length of a checkbox's box, sized from its label's font.
{-# INLINE checkboxBoxSize #-}
checkboxBoxSize :: FontMetrics -> Float
checkboxBoxSize fm = clamp 18 22 (fmLineHeight fm * 1.15)

-- | Horizontal offset from a checkbox/radio node's left edge to its label.
{-# INLINE checkboxLeading #-}
checkboxLeading :: FontMetrics -> Float
checkboxLeading fm = checkboxBoxSize fm + 8

-- | Total tree-row x/y padding: zero horizontally and at least 8 logical pixels vertically.
{-# INLINE treeItemPadding #-}
treeItemPadding :: FontMetrics -> (Float, Float)
treeItemPadding fm = (0, max 8 (fromIntegral (round (fmLineHeight fm * 0.40) :: Int)))

-- | Per-depth horizontal indent for tree rows.
{-# INLINE treeIndentStep #-}
treeIndentStep :: FontMetrics -> Float
treeIndentStep fm = max 12 (fmLineHeight fm * 0.85)

-- | Width of the disclosure chevron slot beside a tree label.
{-# INLINE treeChevronLeading #-}
treeChevronLeading :: FontMetrics -> Float
treeChevronLeading fm = checkboxBoxSize fm + 6

-- | Leading width before a tree row's label at nesting depth @depth@.
{-# INLINE treeRowLeading #-}
treeRowLeading :: FontMetrics -> Int -> Float
treeRowLeading fm depth =
  treeIndentStep fm * fromIntegral (max 0 depth) + treeChevronLeading fm

-- | Bounding box of a tree row's disclosure chevron at @depth@.
{-# INLINE treeChevronRect #-}
treeChevronRect :: FontMetrics -> Float -> Float -> Float -> Int -> Rect
treeChevronRect fm x y h depth =
  let indent = treeIndentStep fm * fromIntegral (max 0 depth)
      lead = max 1 (treeChevronLeading fm)
   in Rect (x + indent) y lead h

-- | Visual height of a slider track.
sliderTrackHeight :: Float
sliderTrackHeight = 10

-- | Diameter of a slider thumb handle.
sliderHandleDiameter :: Float
sliderHandleDiameter = 18

-- | Half the difference between slider handle diameter and track height.
sliderHandleSlack :: Float
sliderHandleSlack = (sliderHandleDiameter - sliderTrackHeight) / 2

-- | A 10-pixel-high track centred vertically within x/y/width/height bounds.
{-# INLINE sliderTrackBounds #-}
sliderTrackBounds :: Float -> Float -> Float -> Float -> Rect
sliderTrackBounds x y w h =
  let trackY = y + max 0 ((h - sliderTrackHeight) / 2)
   in Rect x trackY (max 0 w) sliderTrackHeight

-- | The track grown by the handle's overhang: where a drag or a hover on the
-- handle counts. The painter and the cursor must agree on this.
{-# INLINE sliderHitBounds #-}
sliderHitBounds :: Float -> Float -> Float -> Float -> Rect
sliderHitBounds x y w h =
  let Rect tx ty tw th = sliderTrackBounds x y w h
   in Rect tx (ty - sliderHandleSlack) tw (th + 2 * sliderHandleSlack)

-- | Thickness of a list or page scrollbar.
scrollBarWidth :: Float
scrollBarWidth = 8

-- | Thickness of a window body's scrollbar.
scrollBarSlimWidth :: Float
scrollBarSlimWidth = 4

-- | Inset between scrollbar ends and container bounds.
scrollBarMargin :: Float
scrollBarMargin = 3

-- | The sliver between a page or window bar and the outer edge, and the
-- smallest gap on either side of a list bar.
scrollBarSideGap :: Float
scrollBarSideGap = 3

-- | Bar width and end margin for a slot.
scrollBarGeomFor :: ScrollBarSlot -> (Float, Float)
scrollBarGeomFor ScrollBarWindow = (scrollBarSlimWidth, 0)
scrollBarGeomFor _ = (scrollBarWidth, scrollBarMargin)

-- | The layout arena stores a scroller's slot as its 'Enum' value, and every
-- other node reads a zero there, so 'ScrollBarList' comes first.
data ScrollBarSlot = ScrollBarList | ScrollBarPage | ScrollBarWindow
  deriving (Eq, Ord, Show, Enum)

-- | Choose the 'ScrollBarSlot' for a scroller from window-body and page flags.
classifyScrollBar :: Bool -> Bool -> ScrollBarSlot
classifyScrollBar isWindowBody isPageGrow
  | isWindowBody = ScrollBarWindow
  | isPageGrow = ScrollBarPage
  | otherwise = ScrollBarList

-- | Gap between the content and a bar, given the padding @trailPad@ on the
-- bar's side.
scrollBarGap :: Float -> Float
scrollBarGap trailPad = max scrollBarSideGap trailPad

-- | Space an overflowing scroller takes from its content beside @trailPad@.
scrollBarGutter :: ScrollBarSlot -> Float -> Float
scrollBarGutter slot trailPad =
  let (barW, _) = scrollBarGeomFor slot
      gap = scrollBarGap trailPad
   in case slot of
        ScrollBarList -> barW + 2 * gap - trailPad
        ScrollBarPage -> barW + scrollBarSideGap + gap - trailPad
        ScrollBarWindow -> barW + scrollBarSideGap
