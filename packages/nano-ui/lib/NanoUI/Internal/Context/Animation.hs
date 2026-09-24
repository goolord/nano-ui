-- | Per-widget animations: starting, ticking, settling and reading values.
module NanoUI.Internal.Context.Animation
  ( anyAnimating
  , getLiveAnimations
  , takeAnimSettled
  , lookupAnimation
  , startAnimation
  , startAnimationEase
  , startAnimationEaseDelay
  , startSpring
  , keepAnimationAlive
  , repaintIfOrphan
  , setAnimationValue
  , tickAnimations
  , getAnimationValue
  ) where

import Control.Monad (unless, when)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS

import NanoUI.Internal.Animation
import NanoUI.Internal.Context.Core (damageFull, damageKey, getsDamage, markDirtyCovered)
import NanoUI.Internal.Context.Types (AnimationState (..), Context (..), DamageState (..), ScrollState (..), intKey)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Layout.Arena (getNodeRect, lookupNodeByKey)
import NanoUI.Internal.Types (DamageBounds (..), Rect, defaultDamageSlop, rectNonEmpty)

-- | Read a projection of running and settled animations.
{-# INLINE getsAnimation #-}
getsAnimation :: Context -> (AnimationState -> a) -> IO a
getsAnimation ctx f = f <$> readIORef (ctxAnimationState ctx)

-- | Whether the frame loop has to keep drawing: an animation is running, or a
-- scroller is still gliding onto its target.
{-# INLINE anyAnimating #-}
anyAnimating :: Context -> IO Bool
anyAnimating ctx = do
  anim <- not . IM.null <$> getLiveAnimations ctx
  if anim
    then pure True
    else not . IM.null . ssGlides <$> readIORef (ctxScrollState ctx)

-- | The running animations. Every entry is in progress: the start functions
-- settle a request that would not move, and 'tickAnimations' moves finished
-- entries to the resting values.
{-# INLINE getLiveAnimations #-}
getLiveAnimations :: Context -> IO (IntMap Animation)
getLiveAnimations ctx = getsAnimation ctx asAnimations

-- | Read and clear the flag indicating an animation settled during the last tick.
takeAnimSettled :: Context -> IO Bool
takeAnimSettled ctx = do
  as <- readIORef (ctxAnimationState ctx)
  if asAnimSettled as
    then do
      writeIORef (ctxAnimationState ctx) $! as {asAnimSettled = False}
      pure True
    else pure False

-- | Running animation for an id, or 'Nothing'. Settled values are stored separately.
{-# INLINE lookupAnimation #-}
lookupAnimation :: Context -> WidgetId -> IO (Maybe Animation)
lookupAnimation ctx wid = IM.lookup (intKey wid) . asAnimations <$> readIORef (ctxAnimationState ctx)

-- | Linear tween from start to end over a duration in seconds. A matching
-- request preserves progress; non-positive duration settles immediately.
{-# INLINE startAnimation #-}
startAnimation :: Context -> WidgetId -> Float -> Float -> Float -> IO ()
startAnimation ctx wid start end dur = startAnimationEase ctx wid start end dur EaseLinear

-- | 'startAnimation' with a chosen easing curve and no delay.
{-# INLINE startAnimationEase #-}
startAnimationEase :: Context -> WidgetId -> Float -> Float -> Float -> Ease -> IO ()
startAnimationEase ctx wid start end dur ease = startAnimationEaseDelay ctx wid start end dur ease 0

-- | Tween with start, end, duration, curve, and delay. Times use seconds;
-- negative delay is treated as zero. Matching requests do not restart it.
startAnimationEaseDelay :: Context -> WidgetId -> Float -> Float -> Float -> Ease -> Float -> IO ()
startAnimationEaseDelay ctx wid start end dur ease delay
  | dur <= 0 || approxEq start end = settleKey ctx key end
  | otherwise = do
      as <- readIORef (ctxAnimationState ctx)
      let req = max 0 delay
      case IM.lookup key (asAnimations as) of
        Just a@(EaseAnim aStart _ _ _ _ _ _) | approxEq aStart start && easeSameSpec a ease dur req end -> pure ()
        _ ->
          writeIORef (ctxAnimationState ctx) $! insertRunning key (EaseAnim start end dur 0 ease req req) as
      repaintIfOrphan ctx key
  where
    key = intKey wid

-- | Move toward a target, preserving an existing spring's position/velocity.
-- With no running animation, starts at the settled value or zero.
startSpring :: Context -> WidgetId -> SpringParams -> Float -> IO ()
startSpring ctx wid params target = do
  let key = intKey wid
  as <- readIORef (ctxAnimationState ctx)
  case IM.lookup key (asAnimations as) of
    Just (SpringAnim _ _ t p) | t == target && p == params -> repaintIfOrphan ctx key
    running -> do
      let (pos, vel) = case running of
            Just (SpringAnim p v _ _) -> (p, v)
            Just a -> (animationValue a, 0)
            Nothing -> (IM.findWithDefault 0 key (asAnimRest as), 0)
      if abs (pos - target) <= springEps && abs vel <= springEps
        then settleKey ctx key target
        else do
          writeIORef (ctxAnimationState ctx) $! insertRunning key (SpringAnim pos vel target params) as
          repaintIfOrphan ctx key

-- | Run @anim@ at @key@ in place of the key's resting value, if any.
{-# INLINE insertRunning #-}
insertRunning :: Int -> Animation -> AnimationState -> AnimationState
insertRunning key anim as =
  as
    { asAnimRest = IM.delete key (asAnimRest as)
    , asAnimations = IM.insert key anim (asAnimations as)
    }

-- | How long a 'keepAnimationAlive' animation would run by itself: in effect
-- forever. The lease below ends it.
keepAliveSec :: Float
keepAliveSec = 1e9

-- | Request continuous frames while the widget calls this on every view pass.
-- 'tickAnimations' removes the request after a frame without renewal.
keepAnimationAlive :: Context -> WidgetId -> IO ()
keepAnimationAlive ctx wid = do
  startAnimation ctx wid 0 1 keepAliveSec
  let key = intKey wid
  as <- readIORef (ctxAnimationState ctx)
  unless (IS.member key (asKeepTouched as)) $
    writeIORef (ctxAnimationState ctx) $! as {asKeepTouched = IS.insert key (asKeepTouched as)}

-- | Stop an animation at a value and damage its widget if the value changed.
{-# INLINE setAnimationValue #-}
setAnimationValue :: Context -> WidgetId -> Float -> IO ()
setAnimationValue ctx wid val = settleKey ctx (intKey wid) val

-- | Advance animations by elapsed seconds, retain settled values, and expire
-- keep-alive requests that were not renewed in the view and resting values
-- that nothing held.
tickAnimations :: Context -> Float -> IO ()
tickAnimations ctx dt = do
  rects <- getsDamage ctx dsPrevRects
  modifyIORef' (ctxAnimationState ctx) $ \as0 ->
    let as = lapseRest rects (lapseKeepAlive as0)
     in if IM.null (asAnimations as)
          then if asAnimSettled as then as {asAnimSettled = False} else as
          else
            let stepped = IM.map (stepAnim dt) (asAnimations as)
                (live, done) = IM.partition animInProgress stepped
                rest' =
                  IM.foldlWithKey' (\r k a -> restAt k (animationValue a) r) (asAnimRest as) done
             in as
                  { asAnimations = live
                  , asAnimRest = rest'
                  , asRestHeld = if IM.null done then asRestHeld as else asRestHeld as <> IM.keysSet done
                  , asAnimSettled = not (IM.null done)
                  }

-- | Frames between sweeps of the resting values. A value unused for a whole
-- period goes at the sweep that ends it, so within two periods of its last
-- use; one used at least once a period stays.
restLeaseFrames :: Int
restLeaseFrames = 300

-- | Count a frame toward the next sweep of the resting values and, once a
-- period is up, sweep: keep the values held since the last sweep and those
-- whose widget has a rect, drop the rest. A widget's own value (its hover)
-- stays while the widget is laid out, since a clip frame need not paint it
-- and read it. Frames without resting values do not count; they only drop
-- leases left over from values that went back to zero.
lapseRest :: IntMap Rect -> AnimationState -> AnimationState
lapseRest rects as
  | IM.null (asAnimRest as) =
      if IS.null (asRestHeld as) then as else as {asRestHeld = IS.empty, asRestFrames = 0}
  | asRestFrames as + 1 < restLeaseFrames = as {asRestFrames = asRestFrames as + 1}
  | otherwise =
      as
        { asAnimRest = IM.filterWithKey (\k _ -> IS.member k held || IM.member k rects) (asAnimRest as)
        , asRestHeld = IS.empty
        , asRestFrames = 0
        }
  where
    held = asRestHeld as

-- | End the perpetual animations whose widget did not renew its lease this
-- frame, and start the next frame's lease. A lapsed key leaves no resting
-- value and does not count as settled: the widget is gone, and the rect it
-- leaves behind is damaged like any other removed node.
lapseKeepAlive :: AnimationState -> AnimationState
lapseKeepAlive as
  | IS.null (asKeepAlive as) && IS.null (asKeepTouched as) = as
  | otherwise =
      let lapsed = asKeepAlive as `IS.difference` asKeepTouched as
          perpetual a = case a of
            EaseAnim _ _ dur _ _ _ _ -> dur >= keepAliveSec
            SpringAnim {} -> False
          anims
            | IS.null lapsed = asAnimations as
            | otherwise =
                IM.filterWithKey (\k a -> not (IS.member k lapsed && perpetual a)) (asAnimations as)
       in as {asAnimations = anims, asKeepAlive = asKeepTouched as, asKeepTouched = IS.empty}

-- | Repaint the whole frame for a running animation whose key names no
-- widget ('NanoUI.Internal.Widgets.Animate.animate' and 'animateTo' key a
-- fresh id): no rect says what its value moves, so the frame that draws the
-- new value repaints whole. The running animation keeps frames coming at the
-- paced rate; marking the context dirty would schedule them unpaced.
repaintIfOrphan :: Context -> Int -> IO ()
repaintIfOrphan ctx key = do
  hadRect <- IM.member key <$> getsDamage ctx dsPrevRects
  hasNow <- nodeHasKey ctx key
  unless (hadRect || hasNow) (damageFull ctx)

nodeHasKey :: Context -> Int -> IO Bool
nodeHasKey ctx key =
  maybe (pure False) (fmap rectNonEmpty . getNodeRect (ctxNodeArena ctx))
    =<< lookupNodeByKey (ctxNodeArena ctx) key

settleKey :: Context -> Int -> Float -> IO ()
settleKey ctx key val = do
  as <- readIORef (ctxAnimationState ctx)
  let rest = asAnimRest as
      prevRest = IM.findWithDefault 0 key rest
      prevLive = IM.lookup key (asAnimations as)
      restChanged
        | approxEq val 0 = IM.member key rest
        | otherwise = prevRest /= val
      rest' = if restChanged then restAt key val rest else rest
      -- A value set is in use, like one read.
      hold = not (approxEq val 0) && IS.notMember key (asRestHeld as)
      held' = if hold then IS.insert key (asRestHeld as) else asRestHeld as
  -- A spring at rest settles every frame; write only what changes.
  case prevLive of
    Just _ ->
      writeIORef (ctxAnimationState ctx) $!
        as {asAnimations = IM.delete key (asAnimations as), asAnimRest = rest', asRestHeld = held'}
    Nothing ->
      when (restChanged || hold) $
        writeIORef (ctxAnimationState ctx) $! as {asAnimRest = rest', asRestHeld = held'}
  when (maybe (not (approxEq prevRest val)) (not . approxEq val . animationValue) prevLive) $ do
    -- Covered: the key's widget paints from this value and its rect is
    -- damaged. 'animate' and 'animateTo' key a fresh id with no node, and
    -- those repaint whole through 'repaintIfOrphan' while they run.
    damageKey ctx key (DamageInflated defaultDamageSlop)
    markDirtyCovered ctx

-- | Record a settled value; one near zero is kept as no entry, which reads as zero.
restAt :: Int -> Float -> IntMap Float -> IntMap Float
restAt key v = if approxEq v 0 then IM.delete key else IM.insert key v

-- | Current animated or settled value; zero when the id has neither. Reading
-- a settled value holds it through the next sweep ('lapseRest').
getAnimationValue :: Context -> WidgetId -> IO Float
getAnimationValue ctx wid = do
  let key = intKey wid
  as <- readIORef (ctxAnimationState ctx)
  case IM.lookup key (asAnimations as) of
    Just a -> pure $! animationValue a
    Nothing -> do
      -- 'restAt' keeps no zero, so a nonzero value is a stored one.
      let v = IM.findWithDefault 0 key (asAnimRest as)
      when (v /= 0 && IS.notMember key (asRestHeld as)) $
        writeIORef (ctxAnimationState ctx) $! as {asRestHeld = IS.insert key (asRestHeld as)}
      pure v
