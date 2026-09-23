-- | Scroll offsets and links kept in the widget store, the
-- wheel and glide tuning kept in the context, and the commands that move a
-- scroller: to an offset, by a delta or page, or onto a widget.
module NanoUI.Internal.Context.Scroll
  ( getScrollOffset
  , setScrollOffset
  , getScrollOffset2D
  , setScrollOffset2D
  , linkScrollAxes
    -- * Tuning
  , ScrollTuning (..)
  , defaultScrollTuning
  , getScrollTuning
  , setScrollTuning
  , getScrollStep
  , setScrollStep
  , resolveScrollStep
    -- * Geometry
  , ScrollAxes (..)
  , ScrollMetrics (..)
  , getScrollMetrics
  , cacheScrollMetrics
  , beginScrollMetrics
  , getScrollOffsetIn
  , setScrollOffsetIn
    -- * Commands
  , ScrollBehavior (..)
  , ScrollAlign (..)
  , scrollTo
  , scrollBy
  , scrollPages
  , scrollToStart
  , scrollToEnd
  , scrollIntoView
  , scrollRectIntoView
    -- * Glide
  , applyScrollTarget
  , scrollTargetOffset
  , scrollGliding
  , clampScrollOffset
  , stepScrollGlides
  ) where

import Control.Monad (forM_, unless, when)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS

import NanoUI.Internal.Context.Core (damageWidget, getPrevRect, getStore, setStore, writeSlots)
import NanoUI.Internal.Context.Types
import NanoUI.Internal.Draw qualified as Draw
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Store (Slot (..), SlotWrites (..), WidgetStore, fieldFloat, fieldInt, fieldPoint, findSlot, insertSlot, lookupSlot, slotKey, slotWrite, slotWriteOr)
import NanoUI.Internal.Types (DamageBounds (..), Rect (..), V2 (..), clamp, onGrid, v2Add, v2X, v2Y)

{-# INLINE snapScrollOffset #-}
snapScrollOffset :: Context -> Float -> IO Float
snapScrollOffset ctx v = do
  s <- Draw.getDrawSnapScale (ctxDrawArena ctx)
  pure (onGrid s v)

-- | A scroller's stored offset. A text area keeps both axes in its own point
-- slot. A scroll container keeps its main axis (the vertical one when it
-- scrolls both) under its key and the other under 'SlotScrollCross', so a 1D
-- scroller's offset reads @(cross, main)@ and a 2D one's @(x, y)@. Floats keep
-- a scroll frame a 'storeFloat'-only change, which the damage pass clips.
{-# INLINE storedScrollOffset #-}
storedScrollOffset :: Int -> WidgetStore -> V2
storedScrollOffset key s =
  maybe
    (V2 (findSlot fieldFloat 0 (slotKey SlotScrollCross key) s) (findSlot fieldFloat 0 key s))
    (uncurry V2)
    (lookupSlot fieldPoint (slotKey SlotTextAreaScroll key) s)

-- | Pixel-snapped main-axis offset of a 1D scroller, or vertical offset of a
-- text area/2D scroller. Defaults to zero before state exists.
getScrollOffset :: Context -> WidgetId -> IO Float
getScrollOffset ctx wid = snapScrollOffset ctx . v2Y . storedScrollOffset (intKey wid) =<< getStore ctx

-- | Move a scroller to an offset along its main axis. Cancels a glide in
-- flight: whoever sets an offset outright owns it.
setScrollOffset :: Context -> WidgetId -> Float -> IO ()
setScrollOffset ctx wid off = do
  cancelScrollGlide ctx wid
  writeScrollOffset ctx wid off

writeScrollOffset :: Context -> WidgetId -> Float -> IO ()
writeScrollOffset ctx wid off = do
  V2 x _ <- storedScrollOffset (intKey wid) <$> getStore ctx
  writeScrollOffset2D ctx wid (V2 x off)

-- | Pixel-snapped x/y offset for text areas and 2D scrollers. For a 1D
-- scroller, the fallback stores cross-axis in x and main-axis in y.
getScrollOffset2D :: Context -> WidgetId -> IO V2
getScrollOffset2D ctx wid = do
  V2 x y <- storedScrollOffset (intKey wid) <$> getStore ctx
  V2 <$> snapScrollOffset ctx x <*> snapScrollOffset ctx y

-- | Move a scroller to an offset on both axes. Cancels a glide in flight.
setScrollOffset2D :: Context -> WidgetId -> V2 -> IO ()
setScrollOffset2D ctx wid off = do
  cancelScrollGlide ctx wid
  writeScrollOffset2D ctx wid off

writeScrollOffset2D :: Context -> WidgetId -> V2 -> IO ()
writeScrollOffset2D ctx wid (V2 x y) = do
  store <- getStore ctx
  let key = intKey wid
      taKey = slotKey SlotTextAreaScroll key
      -- A linked scroller ('linkScrollAxes') follows along: the header's
      -- main axis is the body's cross axis.
      mirror link to v =
        let k = findSlot fieldInt 0 (slotKey link key) store
         in if k == 0 then SlotWrites (const True) id else slotWriteOr fieldFloat 0 (to k) v
  -- Text areas only reach the first branch because `textAreaWith` seeds this
  -- slot at init; without the seed a freshly mounted editor falls through to
  -- the container slots below and its offsets are never rendered. The scroll
  -- offset damage only knows scroll nodes, so a text area damages itself.
  case lookupSlot fieldPoint taKey store of
    Just cur ->
      when (cur /= (x, y)) $ do
        setStore ctx (insertSlot fieldPoint taKey (x, y) store)
        damageWidget ctx wid DamageSelf
    Nothing ->
      writeSlots ctx $
        slotWriteOr fieldFloat 0 key y
          <> slotWriteOr fieldFloat 0 (slotKey SlotScrollCross key) x
          <> mirror SlotScrollLinkX id x
          <> mirror SlotScrollLinkY (slotKey SlotScrollCross) y

-- | Link a two-axis body with a separate horizontal scroller, such as a table
-- header. Arguments are body id then horizontal id; synchronises their x offsets.
linkScrollAxes :: Context -> WidgetId -> WidgetId -> IO ()
linkScrollAxes ctx yWid xWid = do
  let yKey = intKey yWid
      xKey = intKey xWid
  writeSlots ctx $
    slotWrite fieldInt (slotKey SlotScrollLinkX yKey) xKey
      <> slotWrite fieldInt (slotKey SlotScrollLinkY xKey) yKey
  store <- getStore ctx
  let V2 x2 y = storedScrollOffset yKey store
      x1 = findSlot fieldFloat 0 xKey store
      x = if x2 == 0 && x1 /= 0 then x1 else x2
  when (x /= x2 || x /= x1) $
    setScrollOffset2D ctx yWid (V2 x y)

-- =============================================================================
-- Tuning
-- =============================================================================

-- | Wheel step and glide time for every scroller in this context.
getScrollTuning :: Context -> IO ScrollTuning
getScrollTuning ctx = ssTuning <$> readIORef (ctxScrollState ctx)

-- | Set the wheel step and glide time. Raising 'scrollWheelStep' makes the
-- wheel cover more ground per notch; a nonzero 'scrollSmoothTime' turns every
-- wheel notch and every 'ScrollSmooth' command into a glide.
setScrollTuning :: Context -> ScrollTuning -> IO ()
setScrollTuning ctx tuning =
  modifyIORef' (ctxScrollState ctx) $ \st -> st {ssTuning = tuning}

-- | This scroller's own wheel step, or @0@ when it follows the context's.
getScrollStep :: Context -> WidgetId -> IO Float
getScrollStep ctx wid = findSlot fieldFloat 0 (slotKey SlotScrollStep (intKey wid)) <$> getStore ctx

-- | Give one scroller its own wheel step, in pixels per notch. @0@ puts it
-- back on the context's step. A list whose rows are a fixed height reads best
-- at a whole number of rows per notch.
setScrollStep :: Context -> WidgetId -> Float -> IO ()
setScrollStep ctx wid px = writeSlots ctx (slotWriteOr fieldFloat 0 (slotKey SlotScrollStep (intKey wid)) px)

-- | Pixels one wheel notch scrolls this scroller.
resolveScrollStep :: Context -> WidgetId -> IO Float
resolveScrollStep ctx wid = do
  own <- getScrollStep ctx wid
  if own > 0
    then pure own
    else max 1 . scrollWheelStep <$> getScrollTuning ctx

-- =============================================================================
-- Geometry
-- =============================================================================

-- | What a scroller looked like on the frame it was last laid out on.
-- Offsets and ranges are in window axes: @x@ rightwards, @y@ downwards,
-- whichever way the scroller itself is built.
data ScrollMetrics = ScrollMetrics
  { scrollViewport :: !Rect
  -- ^ The visible content, in window coordinates, inside padding and clear of
  -- the scrollbars.
  , scrollRange :: !V2
  -- ^ Largest offset each axis reaches. @0@ on an axis that does not scroll.
  , scrollOffset :: !V2
  -- ^ Where the scroller is now.
  , scrollAxes :: !ScrollAxes
  }
  deriving (Eq, Show)

-- | Geometry of the scroller @wid@, or 'Nothing' before it has been laid out.
-- Reads the last frame's layout, so it is safe to call while building the
-- next one.
getScrollMetrics :: Context -> WidgetId -> IO (Maybe ScrollMetrics)
getScrollMetrics ctx wid = do
  s <- getStore ctx
  let key = intKey wid
      point slot = lookupSlot fieldPoint (slotKey slot key) s
  case (point SlotScrollViewPos, point SlotScrollViewSize, point SlotScrollRange) of
    (Just (vx, vy), Just (vw, vh), Just (mx, my)) -> do
      let axes = toEnum (findSlot fieldInt 0 (slotKey SlotScrollAxes key) s)
      off <- getScrollOffsetIn ctx wid axes
      pure $
        Just
          ScrollMetrics
            { scrollViewport = Rect vx vy vw vh
            , scrollRange = V2 mx my
            , scrollOffset = off
            , scrollAxes = axes
            }
    _ -> pure Nothing

-- | Start a frame's geometry pass: the first scroll node to publish under a
-- widget id wins for that frame.
beginScrollMetrics :: Context -> IO ()
beginScrollMetrics ctx =
  modifyIORef' (ctxScrollState ctx) $ \st ->
    if IS.null (ssCached st) then st else st {ssCached = IS.empty}

-- | Record what the scroll pass measured, so the commands and the app can
-- read it between frames. Writes nothing when nothing moved, and nothing at
-- all for a second node sharing this one's widget id. A table's frozen pane
-- and its body share theirs, and letting both publish would rewrite the store
-- every frame and hand the commands a viewport that alternates between panes.
cacheScrollMetrics :: Context -> WidgetId -> ScrollAxes -> Rect -> V2 -> IO ()
cacheScrollMetrics ctx wid axes (Rect vx vy vw vh) range@(V2 mx my) = do
  let key = intKey wid
  st <- readIORef (ctxScrollState ctx)
  unless (IS.member key (ssCached st)) $ do
    writeIORef (ctxScrollState ctx) $! st {ssCached = IS.insert key (ssCached st)}
    -- A range that just shrank (a filtered list, a narrower window) would
    -- leave a glide heading past the new end.
    clampScrollGlide ctx wid range
    writeSlots ctx $
      slotWrite fieldPoint (slotKey SlotScrollViewPos key) (vx, vy)
        <> slotWrite fieldPoint (slotKey SlotScrollViewSize key) (vw, vh)
        <> slotWrite fieldPoint (slotKey SlotScrollRange key) (mx, my)
        <> slotWrite fieldInt (slotKey SlotScrollAxes key) (fromEnum axes)

-- | A 1D row scroller keeps its offset in the main-axis slot, so window and
-- stored axes are swapped for it and identical for everything else. The swap
-- is its own inverse.
{-# INLINE swapAxes #-}
swapAxes :: ScrollAxes -> V2 -> V2
swapAxes ScrollAxisX (V2 x y) = V2 y x
swapAxes _ v = v

-- | This scroller's offset in window axes.
getScrollOffsetIn :: Context -> WidgetId -> ScrollAxes -> IO V2
getScrollOffsetIn ctx wid axes = swapAxes axes <$> getScrollOffset2D ctx wid

-- | Move a scroller to an offset in window axes, cancelling any glide. A 1D
-- scroller ignores the axis it does not scroll on.
setScrollOffsetIn :: Context -> WidgetId -> ScrollAxes -> V2 -> IO ()
setScrollOffsetIn ctx wid axes off = do
  cancelScrollGlide ctx wid
  writeScrollOffsetIn ctx wid axes off

writeScrollOffsetIn :: Context -> WidgetId -> ScrollAxes -> V2 -> IO ()
writeScrollOffsetIn ctx wid axes off
  | axes == ScrollAxisXY = writeScrollOffset2D ctx wid off
  | otherwise = writeScrollOffset ctx wid (v2Y (swapAxes axes off))

-- =============================================================================
-- Commands
-- =============================================================================

-- | Whether a scroll lands on its target at once or glides onto it.
-- 'ScrollSmooth' still lands at once when the context's 'scrollSmoothTime' is
-- @0@, so one setting turns smooth scrolling on for the whole app.
data ScrollBehavior = ScrollInstant | ScrollSmooth
  deriving (Eq, Show)

-- | Where a widget ends up in the viewport once it is scrolled into view.
data ScrollAlign
  = -- | Move as little as possible: nothing at all when it is already whole.
    ScrollNearest
  | -- | Against the leading edge, at the top or left.
    ScrollStart
  | ScrollCenter
  | -- | Against the trailing edge, at the bottom or right.
    ScrollEnd
  deriving (Eq, Show)

-- | Scroll to an absolute offset, clamped to the scroller's range.
scrollTo :: Context -> WidgetId -> V2 -> ScrollBehavior -> IO ()
scrollTo ctx wid off = scrollToward ctx wid (\_ -> pure off)

-- | Scroll by a delta in pixels. Deltas accumulate onto a glide already in
-- flight, so repeated calls keep up rather than fighting each other.
scrollBy :: Context -> WidgetId -> V2 -> ScrollBehavior -> IO ()
scrollBy ctx wid delta = scrollToward ctx wid (\m -> headedBy ctx wid m delta)

-- | Scroll by whole viewports: @V2 0 1@ is one page down, @V2 0 (-0.5)@ half
-- a page up.
scrollPages :: Context -> WidgetId -> V2 -> ScrollBehavior -> IO ()
scrollPages ctx wid (V2 px py) = scrollToward ctx wid $ \m ->
  let Rect _ _ vw vh = scrollViewport m in headedBy ctx wid m (V2 (px * vw) (py * vh))

-- | Where the scroller is headed ('scrollTargetOffset'), moved by @delta@.
headedBy :: Context -> WidgetId -> ScrollMetrics -> V2 -> IO V2
headedBy ctx wid m delta = (`v2Add` delta) <$> scrollTargetOffset ctx wid (scrollOffset m)

-- | Scroll back to the top (and left).
scrollToStart :: Context -> WidgetId -> ScrollBehavior -> IO ()
scrollToStart ctx wid = scrollTo ctx wid (V2 0 0)

-- | Scroll to the end of the content.
scrollToEnd :: Context -> WidgetId -> ScrollBehavior -> IO ()
scrollToEnd ctx wid = scrollTo ctx wid (V2 (1 / 0) (1 / 0)) -- clamped to the range

-- | Scroll @target@ into the viewport of the scroller @wid@ it is built
-- inside. Both widgets are read from the last frame's layout, so a widget
-- that was not built then, such as a row a virtualized list left out, cannot
-- be found; scroll to its content rectangle with 'scrollRectIntoView' instead.
scrollIntoView :: Context -> WidgetId -> WidgetId -> ScrollAlign -> ScrollBehavior -> IO ()
scrollIntoView ctx wid target align behavior = do
  mMetrics <- getScrollMetrics ctx wid
  mRect <- getPrevRect ctx target
  case (mMetrics, mRect) of
    (Just m, Just (Rect rx ry rw rh)) -> do
      let Rect vx vy _ _ = scrollViewport m
          V2 ox oy = scrollOffset m
      scrollRectIntoView ctx wid (Rect (rx - vx + ox) (ry - vy + oy) rw rh) align behavior
    _ -> pure ()

-- | Scroll a rectangle of the content into view. The rectangle is in content
-- coordinates: the origin is where the content starts, which is where the
-- viewport shows it at offset @0@.
scrollRectIntoView :: Context -> WidgetId -> Rect -> ScrollAlign -> ScrollBehavior -> IO ()
scrollRectIntoView ctx wid (Rect rx ry rw rh) align = scrollToward ctx wid $ \m -> do
  let Rect _ _ vw vh = scrollViewport m
      V2 ox oy = scrollOffset m
  pure (V2 (alignAxis align vw rx rw ox) (alignAxis align vh ry rh oy))

-- | Offset that puts a span of the content where @align@ asks for it.
alignAxis :: ScrollAlign -> Float -> Float -> Float -> Float -> Float
alignAxis align viewSize start size cur =
  case align of
    ScrollStart -> start
    ScrollEnd -> start + size - viewSize
    ScrollCenter -> start + (size - viewSize) / 2
    ScrollNearest
      | start < cur -> start
      | start + size > cur + viewSize -> min start (start + size - viewSize)
      | otherwise -> cur

-- | Hold an offset inside @0@ and the scroller's range on each axis.
{-# INLINE clampScrollOffset #-}
clampScrollOffset :: V2 -> V2 -> V2
clampScrollOffset (V2 mx my) (V2 x y) = V2 (clamp 0 mx x) (clamp 0 my y)

-- | Drop the axis a 1D scroller does not move on. A table's paired panes link
-- their cross offsets, so a vertical scroller can carry a horizontal offset it
-- does not own; a glide that watched it would never settle.
{-# INLINE projectAxes #-}
projectAxes :: ScrollAxes -> V2 -> V2
projectAxes axes (V2 x y) =
  case axes of
    ScrollAxisY -> V2 0 y
    ScrollAxisX -> V2 x 0
    ScrollAxisXY -> V2 x y

-- | Send the scroller to the offset @pick@ chooses from its metrics, clamped
-- to its range. Nothing happens before it has been laid out.
scrollToward :: Context -> WidgetId -> (ScrollMetrics -> IO V2) -> ScrollBehavior -> IO ()
scrollToward ctx wid pick behavior = do
  mMetrics <- getScrollMetrics ctx wid
  forM_ mMetrics $ \m -> do
    target <- pick m
    applyScrollTarget ctx wid (scrollAxes m) (clampScrollOffset (scrollRange m) target) behavior

-- =============================================================================
-- Glide
-- =============================================================================

-- | Send a scroller to an offset in window axes, gliding if the caller asked
-- for it and the context is tuned for it. The target must already be clamped
-- to the scroller's range.
applyScrollTarget :: Context -> WidgetId -> ScrollAxes -> V2 -> ScrollBehavior -> IO ()
applyScrollTarget ctx wid axes target0 behavior = do
  st <- readIORef (ctxScrollState ctx)
  cur <- getScrollOffsetIn ctx wid axes
  let target = projectAxes axes target0
      instant = behavior == ScrollInstant || scrollSmoothTime (ssTuning st) <= 0
  if instant || nearOffset (projectAxes axes cur) target
    then setScrollOffsetIn ctx wid axes target
    else
      writeIORef (ctxScrollState ctx) $!
        st {ssGlides = IM.insert (intKey wid) (ScrollGlide wid target axes) (ssGlides st)}

-- | Where the scroller is headed: the glide's target if one is in flight, and
-- @fallback@ (normally the current offset) if not. Deltas add onto this so
-- that notches arriving mid-glide are not swallowed.
scrollTargetOffset :: Context -> WidgetId -> V2 -> IO V2
scrollTargetOffset ctx wid fallback = do
  st <- readIORef (ctxScrollState ctx)
  pure (maybe fallback sgTarget (IM.lookup (intKey wid) (ssGlides st)))

-- | Whether a smooth-scroll target is pending for this widget.
scrollGliding :: Context -> WidgetId -> IO Bool
scrollGliding ctx wid =
  IM.member (intKey wid) . ssGlides <$> readIORef (ctxScrollState ctx)

-- | Hold a glide in flight inside a range that has just been measured again.
-- Without this a list filtered down mid-glide coasts past its new end and
-- stops there, showing nothing, until something else scrolls it.
clampScrollGlide :: Context -> WidgetId -> V2 -> IO ()
clampScrollGlide ctx wid range = modifyGlides ctx (IM.adjust clampGlide (intKey wid))
  where
    clampGlide g = g {sgTarget = projectAxes (sgAxes g) (clampScrollOffset range (sgTarget g))}

-- | Remove the pending glide, leaving the current offset unchanged.
cancelScrollGlide :: Context -> WidgetId -> IO ()
cancelScrollGlide ctx wid = modifyGlides ctx (IM.delete (intKey wid))

-- | Edit the glides in flight, leaving the state untouched when there are none.
modifyGlides :: Context -> (IM.IntMap ScrollGlide -> IM.IntMap ScrollGlide) -> IO ()
modifyGlides ctx f =
  modifyIORef' (ctxScrollState ctx) $ \st ->
    if IM.null (ssGlides st) then st else st {ssGlides = f (ssGlides st)}

-- | Advance every glide by @dt@ seconds. Each one covers the same fraction of
-- what is left every second, so a long throw starts fast and eases in, and at
-- least a pixel a frame so a glide cannot stall on the pixel grid the offsets
-- snap to.
stepScrollGlides :: Context -> Float -> IO ()
stepScrollGlides ctx dt = do
  st <- readIORef (ctxScrollState ctx)
  unless (IM.null (ssGlides st)) $ do
    let alpha = glideAlpha (scrollSmoothTime (ssTuning st)) dt
    live <- IM.traverseMaybeWithKey (const (stepGlide ctx alpha)) (ssGlides st)
    modifyIORef' (ctxScrollState ctx) $ \s -> s {ssGlides = live}

-- | Fraction of the remaining distance a glide covers in @dt@ seconds.
-- 'scrollSmoothTime' is the time to cover all but a twentieth of it.
glideAlpha :: Float -> Float -> Float
glideAlpha smooth dt
  | smooth <= 0 || dt <= 0 = 1
  | otherwise = clamp 0 1 (1 - exp (negate (3 * dt / smooth)))

-- | Move a glide one step, and drop it once it lands.
stepGlide :: Context -> Float -> ScrollGlide -> IO (Maybe ScrollGlide)
stepGlide ctx alpha g@(ScrollGlide wid target axes) = do
  cur <- projectAxes axes <$> getScrollOffsetIn ctx wid axes
  let next = V2 (stepAxis (v2X cur) (v2X target)) (stepAxis (v2Y cur) (v2Y target))
  writeScrollOffsetIn ctx wid axes next
  pure (if nearOffset next target then Nothing else Just g)
  where
    stepAxis c t
      | abs (t - c) <= 1 = t
      | otherwise =
          let moved = c + (t - c) * alpha
           in if abs (moved - c) < 1
                then c + signum (t - c)
                else moved

nearOffset :: V2 -> V2 -> Bool
nearOffset (V2 ax ay) (V2 bx by) = abs (ax - bx) <= 0.01 && abs (ay - by) <= 0.01
