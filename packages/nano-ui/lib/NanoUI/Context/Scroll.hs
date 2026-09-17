-- | Scroll offsets, links and configuration kept in the widget store, the
-- wheel and glide tuning kept in the context, and the commands that move a
-- scroller: to an offset, by a delta or page, or onto a widget.
module NanoUI.Context.Scroll
  ( getScrollOffset
  , setScrollOffset
  , getScrollOffset2D
  , setScrollOffset2D
  , setScrollConfig
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
  , cancelScrollGlide
  , stepScrollGlides
  ) where

import Control.Monad (unless, when)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS

import NanoUI.Context.Core (damageWidget, getPrevRect, getStore, setStore)
import NanoUI.Context.Types
  ( Context (..)
  , ScrollAxes (..)
  , ScrollGlide (..)
  , ScrollState (..)
  , ScrollTuning (..)
  , defaultScrollTuning
  , intKey
  )
import NanoUI.Draw qualified as Draw
import NanoUI.Frame.Scroll.Geometry
  ( ScrollConfig
  , decodeScrollConfig
  , defaultScrollConfig
  , encodeScrollConfig
  , scrollConfigNative2D
  )
import NanoUI.Id (WidgetId)
import NanoUI.Store
  ( WidgetStore (..)
  , slotKey
  , Slot (..)
  )
import NanoUI.Types (DamageBounds (..), Rect (..), V2 (..), clamp, onGrid, v2X, v2Y)

{-# INLINE snapScrollOffset #-}
snapScrollOffset :: Context -> Float -> IO Float
snapScrollOffset ctx v = do
  s <- Draw.getDrawSnapScale (ctxDrawArena ctx)
  pure (onGrid s v)

getScrollOffset :: Context -> WidgetId -> IO Float
getScrollOffset ctx wid = do
  s <- getStore ctx
  let key = intKey wid
      points = storePoint s
      cfgBits = IM.findWithDefault (encodeScrollConfig defaultScrollConfig) (slotKey SlotScrollCfg key) (storeInt s)
      -- Text areas keep both axes in their own slot; native 2D scrollers keep
      -- them in the offset slot, falling back to the main-axis float as
      -- 'getScrollOffset2D' does.
      off = case IM.lookup (slotKey SlotTextAreaScroll key) points of
        Just (_, sy) -> sy
        Nothing
          | scrollConfigNative2D (decodeScrollConfig cfgBits)
          , Just (_, y) <- IM.lookup (slotKey SlotScrollOff key) points ->
              y
          | otherwise -> IM.findWithDefault 0 key (storeFloat s)
  snapScrollOffset ctx off

-- | Move a scroller to an offset along its main axis. Cancels a glide in
-- flight: whoever sets an offset outright owns it.
setScrollOffset :: Context -> WidgetId -> Float -> IO ()
setScrollOffset ctx wid off = do
  cancelScrollGlide ctx wid
  writeScrollOffset ctx wid off

writeScrollOffset :: Context -> WidgetId -> Float -> IO ()
writeScrollOffset ctx wid off = do
  store <- getStore ctx
  let key = intKey wid
      sKey = slotKey SlotTextAreaScroll key
  case IM.lookup sKey (storePoint store) of
    Just (sx, sy) ->
      when (sy /= off) $ do
        setStore ctx (store {storePoint = IM.insert sKey (sx, off) (storePoint store)})
        damageWidget ctx wid DamageSelf
    Nothing -> do
      cfg <- getScrollConfig ctx wid
      if scrollConfigNative2D cfg
        then do
          cur <- getScrollOffset2D ctx wid
          writeScrollOffset2D ctx wid (V2 (v2X cur) off)
        else do
          let prev = IM.findWithDefault 0 key (storeFloat store)
          when (prev /= off) $ do
            let floats0 = IM.insert key off (storeFloat store)
                yKey = IM.findWithDefault 0 (slotKey SlotScrollLinkY key) (storeInt store)
            if yKey == 0
              then setStore ctx (store {storeFloat = floats0})
              else do
                let offKey = slotKey SlotScrollOff yKey
                    crossKey = slotKey SlotScrollCross yKey
                    prevY = IM.findWithDefault 0 yKey floats0
                    floats1 = IM.insert yKey prevY $ IM.insert crossKey off floats0
                    points = IM.insert offKey (off, prevY) (storePoint store)
                setStore ctx (store {storeFloat = floats1, storePoint = points})

getScrollOffset2D :: Context -> WidgetId -> IO V2
getScrollOffset2D ctx wid = do
  s <- getStore ctx
  let widKey = intKey wid
      sKey = slotKey SlotTextAreaScroll widKey
  v <-
    case IM.lookup sKey (storePoint s) of
      Just (sx, sy) -> pure (V2 sx sy)
      Nothing -> do
        let offKey = slotKey SlotScrollOff widKey
            crossKey = slotKey SlotScrollCross widKey
        case IM.lookup offKey (storePoint s) of
          Just (x, y) -> pure (V2 x y)
          Nothing ->
            pure
              ( V2
                  (IM.findWithDefault 0 crossKey (storeFloat s))
                  (IM.findWithDefault 0 widKey (storeFloat s))
              )
  sx <- snapScrollOffset ctx (v2X v)
  sy <- snapScrollOffset ctx (v2Y v)
  pure (V2 sx sy)

-- | Move a scroller to an offset on both axes. Cancels a glide in flight.
setScrollOffset2D :: Context -> WidgetId -> V2 -> IO ()
setScrollOffset2D ctx wid off = do
  cancelScrollGlide ctx wid
  writeScrollOffset2D ctx wid off

writeScrollOffset2D :: Context -> WidgetId -> V2 -> IO ()
writeScrollOffset2D ctx wid off = do
  store <- getStore ctx
  let widKey = intKey wid
      sKey = slotKey SlotTextAreaScroll widKey
  -- Text areas only reach the first branch because `textAreaWith` seeds this
  -- slot at init; without the seed a freshly mounted editor falls through to
  -- the container slots below and its offsets are never rendered.
  case IM.lookup sKey (storePoint store) of
    Just (sx, sy) -> do
      let sx' = v2X off
          sy' = v2Y off
      when (sx /= sx' || sy /= sy') $ do
        setStore ctx (store {storePoint = IM.insert sKey (sx', sy') (storePoint store)})
        damageWidget ctx wid DamageSelf
    Nothing -> do
      let offKey = slotKey SlotScrollOff widKey
          crossKey = slotKey SlotScrollCross widKey
          prev = IM.lookup offKey (storePoint store)
          next = (v2X off, v2Y off)
          prevY = IM.findWithDefault 0 widKey (storeFloat store)
          prevX = IM.findWithDefault 0 crossKey (storeFloat store)
          xLink = IM.findWithDefault 0 (slotKey SlotScrollLinkX widKey) (storeInt store)
      when (prev /= Just next || prevY /= v2Y off || prevX /= v2X off) $ do
        let floats0 =
              IM.insert widKey (v2Y off) $
                IM.insert crossKey (v2X off) (storeFloat store)
            floats1 =
              if xLink == 0 then floats0 else IM.insert xLink (v2X off) floats0
        setStore ctx
          ( store
              { storePoint = IM.insert offKey next (storePoint store)
              , storeFloat = floats1
              }
          )

linkScrollAxes :: Context -> WidgetId -> WidgetId -> IO ()
linkScrollAxes ctx yWid xWid = do
  store <- getStore ctx
  let yKey = intKey yWid
      xKey = intKey xWid
      ints =
        IM.insert (slotKey SlotScrollLinkX yKey) xKey $
          IM.insert (slotKey SlotScrollLinkY xKey) yKey (storeInt store)
  setStore ctx (store {storeInt = ints})
  V2 x2 y <- getScrollOffset2D ctx yWid
  x1 <- do
    s <- getStore ctx
    pure (IM.findWithDefault 0 xKey (storeFloat s))
  let x = if x2 == 0 && x1 /= 0 then x1 else x2
  when (x /= x2 || x /= x1) $
    setScrollOffset2D ctx yWid (V2 x y)

getScrollConfig :: Context -> WidgetId -> IO ScrollConfig
getScrollConfig ctx wid = do
  s <- getStore ctx
  let cfgKey = slotKey SlotScrollCfg (intKey wid)
      bits = IM.findWithDefault (encodeScrollConfig defaultScrollConfig) cfgKey (storeInt s)
  pure (decodeScrollConfig bits)

setScrollConfig :: Context -> WidgetId -> ScrollConfig -> IO ()
setScrollConfig ctx wid cfg = do
  store <- getStore ctx
  let cfgKey = slotKey SlotScrollCfg (intKey wid)
      bits = encodeScrollConfig cfg
      prev = IM.findWithDefault (encodeScrollConfig defaultScrollConfig) cfgKey (storeInt store)
  when (prev /= bits) $
    setStore ctx (store {storeInt = IM.insert cfgKey bits (storeInt store)})

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
getScrollStep ctx wid = do
  s <- getStore ctx
  pure (IM.findWithDefault 0 (slotKey SlotScrollStep (intKey wid)) (storeFloat s))

-- | Give one scroller its own wheel step, in pixels per notch. @0@ puts it
-- back on the context's step. A list whose rows are a fixed height reads best
-- at a whole number of rows per notch.
setScrollStep :: Context -> WidgetId -> Float -> IO ()
setScrollStep ctx wid px = do
  store <- getStore ctx
  let key = slotKey SlotScrollStep (intKey wid)
      prev = IM.findWithDefault 0 key (storeFloat store)
  when (prev /= px) $
    setStore ctx (store {storeFloat = IM.insert key px (storeFloat store)})

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
      point slot = IM.lookup (slotKey slot key) (storePoint s)
  case (point SlotScrollViewPos, point SlotScrollViewSize, point SlotScrollRange) of
    (Just (vx, vy), Just (vw, vh), Just (mx, my)) -> do
      let axes = decodeScrollAxes (IM.findWithDefault 0 (slotKey SlotScrollAxes key) (storeInt s))
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
cacheScrollMetrics ctx wid axes viewport range = do
  taken <- claimScrollMetrics ctx (intKey wid)
  unless taken (writeScrollMetrics ctx wid axes viewport range)

-- | Whether this widget id has already published geometry this frame; marks
-- it published if not.
claimScrollMetrics :: Context -> Int -> IO Bool
claimScrollMetrics ctx key = do
  st <- readIORef (ctxScrollState ctx)
  if IS.member key (ssCached st)
    then pure True
    else do
      writeIORef (ctxScrollState ctx) $! st {ssCached = IS.insert key (ssCached st)}
      pure False

writeScrollMetrics :: Context -> WidgetId -> ScrollAxes -> Rect -> V2 -> IO ()
writeScrollMetrics ctx wid axes (Rect vx vy vw vh) range@(V2 mx my) = do
  -- A range that just shrank (a filtered list, a narrower window) would leave
  -- a glide heading past the new end.
  clampScrollGlide ctx wid range
  store <- getStore ctx
  let key = intKey wid
      axesKey = slotKey SlotScrollAxes key
      posKey = slotKey SlotScrollViewPos key
      sizeKey = slotKey SlotScrollViewSize key
      rangeKey = slotKey SlotScrollRange key
      code = encodeScrollAxes axes
      points = storePoint store
      ints = storeInt store
      samePoint k v = IM.lookup k points == Just v
  unless
    ( samePoint posKey (vx, vy)
        && samePoint sizeKey (vw, vh)
        && samePoint rangeKey (mx, my)
        && IM.lookup axesKey ints == Just code
    )
    $ setStore ctx
      ( store
          { storePoint =
              IM.insert posKey (vx, vy) $
                IM.insert sizeKey (vw, vh) $
                  IM.insert rangeKey (mx, my) points
          , storeInt = IM.insert axesKey code ints
          }
      )

encodeScrollAxes :: ScrollAxes -> Int
encodeScrollAxes = \case
  ScrollAxisY -> 0
  ScrollAxisX -> 1
  ScrollAxisXY -> 2

decodeScrollAxes :: Int -> ScrollAxes
decodeScrollAxes = \case
  1 -> ScrollAxisX
  2 -> ScrollAxisXY
  _ -> ScrollAxisY

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
writeScrollOffsetIn ctx wid axes off =
  case axes of
    ScrollAxisXY -> writeScrollOffset2D ctx wid off
    ScrollAxisY -> writeScrollOffset ctx wid (v2Y off)
    ScrollAxisX -> writeScrollOffset ctx wid (v2X off)

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
scrollTo ctx wid off behavior =
  withScrollMetrics ctx wid $ \m ->
    applyScrollTarget ctx wid (scrollAxes m) (clampScrollOffset (scrollRange m) off) behavior

-- | Scroll by a delta in pixels. Deltas accumulate onto a glide already in
-- flight, so repeated calls keep up rather than fighting each other.
scrollBy :: Context -> WidgetId -> V2 -> ScrollBehavior -> IO ()
scrollBy ctx wid delta behavior =
  withScrollMetrics ctx wid $ \m -> scrollMetricsBy ctx wid m delta behavior

-- | Scroll by whole viewports: @V2 0 1@ is one page down, @V2 0 (-0.5)@ half
-- a page up.
scrollPages :: Context -> WidgetId -> V2 -> ScrollBehavior -> IO ()
scrollPages ctx wid (V2 px py) behavior =
  withScrollMetrics ctx wid $ \m -> do
    let Rect _ _ vw vh = scrollViewport m
    scrollMetricsBy ctx wid m (V2 (px * vw) (py * vh)) behavior

scrollMetricsBy :: Context -> WidgetId -> ScrollMetrics -> V2 -> ScrollBehavior -> IO ()
scrollMetricsBy ctx wid m (V2 dx dy) behavior = do
  V2 bx by <- scrollTargetOffset ctx wid (scrollOffset m)
  applyScrollTarget ctx wid (scrollAxes m) (clampScrollOffset (scrollRange m) (V2 (bx + dx) (by + dy))) behavior

-- | Scroll back to the top (and left).
scrollToStart :: Context -> WidgetId -> ScrollBehavior -> IO ()
scrollToStart ctx wid = scrollTo ctx wid (V2 0 0)

-- | Scroll to the end of the content.
scrollToEnd :: Context -> WidgetId -> ScrollBehavior -> IO ()
scrollToEnd ctx wid behavior =
  withScrollMetrics ctx wid $ \m ->
    applyScrollTarget ctx wid (scrollAxes m) (scrollRange m) behavior

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
scrollRectIntoView ctx wid (Rect rx ry rw rh) align behavior =
  withScrollMetrics ctx wid $ \m -> do
    let Rect _ _ vw vh = scrollViewport m
        V2 ox oy = scrollOffset m
        V2 mx my = scrollRange m
        target =
          V2
            (clamp 0 mx (alignAxis align vw rx rw ox))
            (clamp 0 my (alignAxis align vh ry rh oy))
    applyScrollTarget ctx wid (scrollAxes m) target behavior

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

withScrollMetrics :: Context -> WidgetId -> (ScrollMetrics -> IO ()) -> IO ()
withScrollMetrics ctx wid act = getScrollMetrics ctx wid >>= mapM_ act

-- =============================================================================
-- Glide
-- =============================================================================

-- | Send a scroller to an offset in window axes, gliding if the caller asked
-- for it and the context is tuned for it. The target must already be clamped
-- to the scroller's range.
applyScrollTarget :: Context -> WidgetId -> ScrollAxes -> V2 -> ScrollBehavior -> IO ()
applyScrollTarget ctx wid axes target0 behavior = do
  st <- readIORef (ctxScrollState ctx)
  let smooth = scrollSmoothTime (ssTuning st)
      target = projectAxes axes target0
  if behavior == ScrollInstant || smooth <= 0
    then setScrollOffsetIn ctx wid axes target
    else do
      cur <- getScrollOffsetIn ctx wid axes
      if nearOffset (projectAxes axes cur) target
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

scrollGliding :: Context -> WidgetId -> IO Bool
scrollGliding ctx wid =
  IM.member (intKey wid) . ssGlides <$> readIORef (ctxScrollState ctx)

-- | Hold a glide in flight inside a range that has just been measured again.
-- Without this a list filtered down mid-glide coasts past its new end and
-- stops there, showing nothing, until something else scrolls it.
clampScrollGlide :: Context -> WidgetId -> V2 -> IO ()
clampScrollGlide ctx wid range =
  modifyIORef' (ctxScrollState ctx) $ \st ->
    if IM.null (ssGlides st)
      then st
      else st {ssGlides = IM.adjust clampGlide (intKey wid) (ssGlides st)}
  where
    clampGlide g = g {sgTarget = projectAxes (sgAxes g) (clampScrollOffset range (sgTarget g))}

cancelScrollGlide :: Context -> WidgetId -> IO ()
cancelScrollGlide ctx wid =
  modifyIORef' (ctxScrollState ctx) $ \st ->
    if IM.null (ssGlides st)
      then st
      else st {ssGlides = IM.delete (intKey wid) (ssGlides st)}

-- | Advance every glide by @dt@ seconds. Each one covers the same fraction of
-- what is left every second, so a long throw starts fast and eases in, and at
-- least a pixel a frame so a glide cannot stall on the pixel grid the offsets
-- snap to.
stepScrollGlides :: Context -> Float -> IO ()
stepScrollGlides ctx dt = do
  st <- readIORef (ctxScrollState ctx)
  unless (IM.null (ssGlides st)) $ do
    let alpha = glideAlpha (scrollSmoothTime (ssTuning st)) dt
    done <- mapM (stepGlide ctx alpha) (IM.toList (ssGlides st))
    let settled = [k | (k, True) <- done]
    unless (null settled) $
      modifyIORef' (ctxScrollState ctx) $ \s ->
        s {ssGlides = foldr IM.delete (ssGlides s) settled}

-- | Fraction of the remaining distance a glide covers in @dt@ seconds.
-- 'scrollSmoothTime' is the time to cover all but a twentieth of it.
glideAlpha :: Float -> Float -> Float
glideAlpha smooth dt
  | smooth <= 0 || dt <= 0 = 1
  | otherwise = clamp 0 1 (1 - exp (negate (3 * dt / smooth)))

stepGlide :: Context -> Float -> (Int, ScrollGlide) -> IO (Int, Bool)
stepGlide ctx alpha (key, ScrollGlide wid target axes) = do
  cur <- projectAxes axes <$> getScrollOffsetIn ctx wid axes
  let next = V2 (stepAxis (v2X cur) (v2X target)) (stepAxis (v2Y cur) (v2Y target))
  writeScrollOffsetIn ctx wid axes next
  pure (key, nearOffset next target)
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
