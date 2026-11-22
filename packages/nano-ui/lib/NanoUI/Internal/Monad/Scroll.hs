
-- | Scroll queries and commands from inside a view.
module NanoUI.Internal.Monad.Scroll
  ( getScrollMetricsUi
  , setScrollOffsetUi
  , scrollToUi
  , scrollByUi
  , scrollPagesUi
  , scrollRectIntoViewUi
  , setScrollStepUi
  )
where

import NanoUI.Internal.Context (getScrollMetrics, readScrollMetrics, scrollBy, scrollPages, scrollRectIntoView, scrollTo, setScrollOffsetIn, setScrollOffsetPending, setScrollStep, ScrollAlign, ScrollBehavior, ScrollMetrics (scrollAxes))
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Monad.Core (NanoUI, withContext)
import NanoUI.Internal.Types (Rect, V2)

-- | The scroller's geometry from the previous layout (viewport, range, and
-- offset), or 'Nothing' before its first layout. The id is returned by
-- 'NanoUI.scrollArea'; to query or command a scroller before it is declared,
-- read its id with 'currentId' first:
--
-- > sid <- currentId
-- > metrics <- getScrollMetricsUi sid
-- > (_, rows) <- scrollArea (fillW . fillH) (visibleRows metrics)
--
-- If this frame's layout changes the metrics, a follow-up frame is scheduled.
getScrollMetricsUi :: WidgetId -> NanoUI (Maybe ScrollMetrics)
getScrollMetricsUi wid = withContext (`getScrollMetrics` wid)

-- | Put a scroller at an offset in window axes, cancelling a glide; a 1D
-- scroller ignores the axis it does not scroll on, and unlike
-- 'NanoUI.setScrollOffset2D' a row scroller takes its offset in x. Unlike
-- 'scrollToUi' the offset is not held to the range the last layout found,
-- so it can place content this frame is about to lay out; the layout holds
-- it to the content's real range.
setScrollOffsetUi :: WidgetId -> V2 -> NanoUI ()
setScrollOffsetUi wid off = withContext $ \ctx ->
  readScrollMetrics ctx wid >>= \case
    Just m -> setScrollOffsetIn ctx wid (scrollAxes m) off
    Nothing -> setScrollOffsetPending ctx wid off

-- | Scroll to an offset, held to the scroller's range.
scrollToUi :: WidgetId -> V2 -> ScrollBehavior -> NanoUI ()
scrollToUi wid off behavior = withContext (\ctx -> scrollTo ctx wid off behavior)

-- | Scroll by a delta in pixels.
scrollByUi :: WidgetId -> V2 -> ScrollBehavior -> NanoUI ()
scrollByUi wid delta behavior = withContext (\ctx -> scrollBy ctx wid delta behavior)

-- | Scroll by whole viewports: @V2 0 0.5@ is half a page down.
scrollPagesUi :: WidgetId -> V2 -> ScrollBehavior -> NanoUI ()
scrollPagesUi wid pages behavior = withContext (\ctx -> scrollPages ctx wid pages behavior)

-- | Scroll a rectangle of the content, in content coordinates, into view:
-- the row a keyboard selection moved to in a list that builds only the rows
-- it shows.
scrollRectIntoViewUi :: WidgetId -> Rect -> ScrollAlign -> ScrollBehavior -> NanoUI ()
scrollRectIntoViewUi wid r align behavior = withContext (\ctx -> scrollRectIntoView ctx wid r align behavior)

-- | Give a scroller its own wheel step, in pixels a notch; @0@ puts it back on
-- the app's.
setScrollStepUi :: WidgetId -> Float -> NanoUI ()
setScrollStepUi wid px = withContext (\ctx -> setScrollStep ctx wid px)

