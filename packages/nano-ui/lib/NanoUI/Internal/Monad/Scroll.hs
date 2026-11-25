-- | Scroll queries, tuning, and commands from inside a view.
module NanoUI.Internal.Monad.Scroll
  ( getScrollTuning
  , setScrollTuning
  , getScrollMetrics
  , setScrollOffset
  , scrollTo
  , scrollBy
  , scrollPages
  , scrollToStart
  , scrollToEnd
  , scrollIntoView
  , scrollRectIntoView
  , setScrollStep
  )
where

import NanoUI.Internal.Context qualified as Ctx
import NanoUI.Internal.Context (ScrollAlign, ScrollBehavior, ScrollMetrics (scrollAxes), ScrollTuning)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Monad.Core (NanoUI, withContext)
import NanoUI.Internal.Types (Rect, V2)

-- | Read the session's global wheel-step and smooth-scroll tuning.
getScrollTuning :: NanoUI ScrollTuning
getScrollTuning = withContext Ctx.getScrollTuning

-- | Configure wheel-step distance and smooth-scroll duration for the session.
setScrollTuning :: ScrollTuning -> NanoUI ()
setScrollTuning tuning = withContext (`Ctx.setScrollTuning` tuning)

-- | The scroller's geometry from the previous layout (viewport, range, and
-- offset), or 'Nothing' before its first layout. The id comes from
-- 'NanoUI.scrollWith'', 'NanoUI.scroll2DWith'', or 'NanoUI.scrollConfigured'';
-- to query or command a scroller before it is declared, read its id with
-- 'NanoUI.currentId' first:
--
-- > sid <- currentId
-- > metrics <- getScrollMetrics sid
-- > (rows, _) <- scrollWith' (fillW . fillH) (visibleRows metrics)
--
-- If this frame's layout changes the metrics, a follow-up frame is scheduled.
getScrollMetrics :: WidgetId -> NanoUI (Maybe ScrollMetrics)
getScrollMetrics wid = withContext (`Ctx.getScrollMetrics` wid)

-- | Put a scroller at an offset in window axes, cancelling a glide; a 1D
-- scroller ignores the axis it does not scroll on, and a row scroller takes
-- its offset in x. Unlike 'scrollTo', the offset is not held to the range
-- the last layout found, so it can place content this frame is about to lay
-- out; the layout holds it to the content's real range.
setScrollOffset :: WidgetId -> V2 -> NanoUI ()
setScrollOffset wid off = withContext $ \ctx ->
  Ctx.readScrollMetrics ctx wid >>= \case
    Just m -> Ctx.setScrollOffsetIn ctx wid (scrollAxes m) off
    Nothing -> Ctx.setScrollOffsetPending ctx wid off

-- | Scroll to an offset, held to the scroller's range.
scrollTo :: WidgetId -> V2 -> ScrollBehavior -> NanoUI ()
scrollTo wid off behavior = withContext (\ctx -> Ctx.scrollTo ctx wid off behavior)

-- | Scroll by a delta in pixels.
scrollBy :: WidgetId -> V2 -> ScrollBehavior -> NanoUI ()
scrollBy wid delta behavior = withContext (\ctx -> Ctx.scrollBy ctx wid delta behavior)

-- | Scroll by whole viewports: @V2 0 0.5@ is half a page down.
scrollPages :: WidgetId -> V2 -> ScrollBehavior -> NanoUI ()
scrollPages wid pages behavior = withContext (\ctx -> Ctx.scrollPages ctx wid pages behavior)

-- | Scroll back to the top (and left).
scrollToStart :: WidgetId -> ScrollBehavior -> NanoUI ()
scrollToStart wid behavior = withContext (\ctx -> Ctx.scrollToStart ctx wid behavior)

-- | Scroll to the end of the content.
scrollToEnd :: WidgetId -> ScrollBehavior -> NanoUI ()
scrollToEnd wid behavior = withContext (\ctx -> Ctx.scrollToEnd ctx wid behavior)

-- | Scroll widget @target@ into the viewport of scroller @wid@ it lives in,
-- using last frame's layout. For virtualized lists that omit off-screen rows,
-- use 'scrollRectIntoView' with the row's content rectangle instead.
scrollIntoView :: WidgetId -> WidgetId -> ScrollAlign -> ScrollBehavior -> NanoUI ()
scrollIntoView wid target align behavior = withContext (\ctx -> Ctx.scrollIntoView ctx wid target align behavior)

-- | Scroll a rectangle of the content, in content coordinates, into view:
-- the row a keyboard selection moved to in a list that builds only the rows
-- it shows.
scrollRectIntoView :: WidgetId -> Rect -> ScrollAlign -> ScrollBehavior -> NanoUI ()
scrollRectIntoView wid r align behavior = withContext (\ctx -> Ctx.scrollRectIntoView ctx wid r align behavior)

-- | Give a scroller its own wheel step, in pixels per notch; @0@ restores the
-- session's default.
setScrollStep :: WidgetId -> Float -> NanoUI ()
setScrollStep wid px = withContext (\ctx -> Ctx.setScrollStep ctx wid px)
