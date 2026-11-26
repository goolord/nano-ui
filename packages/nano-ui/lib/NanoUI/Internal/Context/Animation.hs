-- | Per-widget animations: starting, ticking, settling and reading values.
module NanoUI.Internal.Context.Animation
  ( anyAnimating
  , liveAnimationKeys
  , isAnimatingKey
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
  , getViewAnimationValue
  ) where

import Control.Monad (unless, when)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS

import NanoUI.Internal.Animation
import NanoUI.Internal.Context.Core (damageFull, damageKey, getsDamage, markDirty)
import NanoUI.Internal.Context.Types (AnimationState (..), Context (..), DamageState (..), PrevFrame (..), ScrollState (..), intKey)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Layout.Arena (getNodeRect, lookupNodeByKey)
import NanoUI.Internal.Types (DamageBounds (..), Rect, defaultDamageSlop, finite, rectNonEmpty)

-- | Whether the frame loop has to keep drawing: an animation is running, a
-- keep-alive lease is active, or a scroller is still gliding onto its target.
{-# INLINE anyAnimating #-}
anyAnimating :: Context -> IO Bool
anyAnimating ctx = do
  as <- readIORef (ctxAnimationState ctx)
  if not (IM.null (asAnimations as)) || not (IS.null (asKeepAlive as)) || not (IS.null (asKeepTouched as))
    then pure True
    else not . IM.null . ssGlides <$> readIORef (ctxScrollState ctx)

-- | Keys with a running animation or an active keep-alive lease.
{-# INLINE liveAnimationKeys #-}
liveAnimationKeys :: Context -> IO IntSet
liveAnimationKeys ctx = do
  as <- readIORef (ctxAnimationState ctx)
  pure (IM.keysSet (asAnimations as) <> asKeepAlive as <> asKeepTouched as)

-- | Whether @wid@ has a running animation or an active keep-alive lease.
{-# INLINE isAnimatingKey #-}
isAnimatingKey :: Context -> WidgetId -> IO Bool
isAnimatingKey ctx wid = do
  let k = intKey wid
  as <- readIORef (ctxAnimationState ctx)
  pure (IM.member k (asAnimations as) || IS.member k (asKeepAlive as) || IS.member k (asKeepTouched as))

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
  -- A NaN or infinite end/duration/delay would restart or run forever and
  -- never let the loop idle; ignore non-finite targets/durations.
  | not (finite end) || not (finite dur) || not (finite delay) = pure ()
  | not (finite start) || dur <= 0 || approxEq start end = settleKey ctx key end
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

-- | Advance a spring toward a target. Keeps the position and velocity of a
-- running spring; otherwise starts from the settled value or zero.
startSpring :: Context -> WidgetId -> SpringParams -> Float -> IO ()
startSpring ctx wid params target
  -- A non-finite target would leave the spring NaN for good.
  | not (finite target) = pure ()
  -- Non-positive or non-finite stiffness/damping would oscillate or diverge
  -- forever and prevent idling; settle at target immediately.
  | not (finite (springStiffness params))
      || not (finite (springDamping params))
      || springStiffness params <= 0
      || springDamping params <= 0 =
      settleKey ctx (intKey wid) target
  | otherwise = do
  let key = intKey wid
  as <- readIORef (ctxAnimationState ctx)
  case IM.lookup key (asAnimations as) of
    Just (SpringAnim _ _ t p) | t == target && p == params -> repaintIfOrphan ctx key
    running -> do
      let (pos0, vel0) = case running of
            Just (SpringAnim d v t _) -> (t + d, v)
            Just a -> (animationValue a, 0)
            Nothing -> (IM.findWithDefault 0 key (asAnimRest as), 0)
          -- A value already poisoned starts again at the target.
          (pos, vel) = if finite pos0 && finite vel0 then (pos0, vel0) else (target, 0)

      if abs (pos - target) <= springEps && abs vel <= springEps
        then settleKey ctx key target
        else do
          writeIORef (ctxAnimationState ctx) $! insertRunning key (SpringAnim (pos - target) vel target params) as
          repaintIfOrphan ctx key

-- | Run @anim@ at @key@ in place of the key's resting value, if any.
{-# INLINE insertRunning #-}
insertRunning :: Int -> Animation -> AnimationState -> AnimationState
insertRunning key anim as =
  as
    { asAnimRest = IM.delete key (asAnimRest as)
    , asAnimations = IM.insert key anim (asAnimations as)
    }

-- | Request continuous frames while the widget calls this on every view pass.
-- 'tickAnimations' removes the request after a frame without renewal.
keepAnimationAlive :: Context -> WidgetId -> IO ()
keepAnimationAlive ctx wid = do
  let key = intKey wid
  as <- readIORef (ctxAnimationState ctx)
  let live = IM.member key (asAnimations as) || IS.member key (asKeepAlive as) || IS.member key (asKeepTouched as)
  unless (IS.member key (asKeepTouched as)) $
    writeIORef (ctxAnimationState ctx) $! as {asKeepTouched = IS.insert key (asKeepTouched as)}
  unless live (repaintIfOrphan ctx key)

-- | Stop an animation at a value and damage its widget if the value changed.
{-# INLINE setAnimationValue #-}
setAnimationValue :: Context -> WidgetId -> Float -> IO ()
setAnimationValue ctx wid val = settleKey ctx (intKey wid) val

-- | Advance animations by elapsed seconds, retain settled values, and expire
-- keep-alive requests and view hooks' animations that were not renewed in the
-- view and resting values that nothing held.
tickAnimations :: Context -> Float -> IO ()
tickAnimations ctx dt = do
  rects <- getsDamage ctx (pfRects . dsPrev)
  modifyIORef' (ctxAnimationState ctx) $ \as0 ->
    let as = lapseViewAnims (lapseRest rects (lapseKeepAlive as0))
     in if IM.null (asAnimations as)
          then if asAnimSettled as then as {asAnimSettled = False} else as
          else
            let stepped = IM.map (stepAnim dt) (asAnimations as)
                (live, done) = IM.partition animInProgress stepped
             in as
                  { asAnimations = live
                  , asAnimRest = restAll (asAnimRest as) done
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

-- | Advance the keep-alive lease window by one frame: only keys renewed
-- since the previous tick stay live.
lapseKeepAlive :: AnimationState -> AnimationState
lapseKeepAlive as
  | IS.null (asKeepAlive as) && IS.null (asKeepTouched as) = as
  | otherwise = as {asKeepAlive = asKeepTouched as, asKeepTouched = IS.empty}

-- | Rest, at their current values, the view hooks' animations that the view
-- left out this frame ('getViewAnimationValue'), and start the next frame's
-- lease.
lapseViewAnims :: AnimationState -> AnimationState
lapseViewAnims as
  | IS.null (asViewAnims as) && IS.null (asViewTouched as) = as
  | otherwise =
      let lapsed = asViewAnims as `IS.difference` asViewTouched as
          gone = IM.restrictKeys (asAnimations as) lapsed
       in as
            { asAnimations = if IM.null gone then asAnimations as else IM.withoutKeys (asAnimations as) lapsed
            , asAnimRest = restAll (asAnimRest as) gone
            , asViewAnims = asViewTouched as
            , asViewTouched = IS.empty
            }

-- | Repaint the whole frame for a running animation whose key has no layout
-- node ('NanoUI.Internal.Widgets.Animate.animate' and 'animateTo' allocate a
-- fresh id): without a widget rect to bound its damage, the frame must repaint
-- in full. The running animation drives frames at the paced rate; marking the
-- context dirty would schedule them unpaced.
repaintIfOrphan :: Context -> Int -> IO ()
repaintIfOrphan ctx key = do
  hadRect <- getsDamage ctx (IM.member key . pfRects . dsPrev)
  unless hadRect $ do
    let na = ctxNodeArena ctx
    hasNow <- maybe (pure False) (fmap rectNonEmpty . getNodeRect na) =<< lookupNodeByKey na key
    unless hasNow (damageFull ctx)

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
    -- A zero-length tween on such a key settles here without running, so
    -- this is the frame that must repaint whole.
    repaintIfOrphan ctx key
    markDirty ctx

-- | Record a settled value; one near zero is kept as no entry, which reads as zero.
restAt :: Int -> Float -> IntMap Float -> IntMap Float
restAt key v = if approxEq v 0 then IM.delete key else IM.insert key v

restAll :: IntMap Float -> IntMap Animation -> IntMap Float
restAll = IM.foldlWithKey' (\r k a -> restAt k (animationValue a) r)

-- | Current animated or settled value; zero when the id has neither. Reading
-- a settled value holds it through the next sweep ('lapseRest').
getAnimationValue :: Context -> WidgetId -> IO Float
getAnimationValue = readAnimValue False

-- | 'getAnimationValue' for a key a view hook owns ('animate', 'animateTo').
-- An animation running there lasts while the view reads it: on the first
-- frame without a read, 'tickAnimations' rests it at its value.
getViewAnimationValue :: Context -> WidgetId -> IO Float
getViewAnimationValue = readAnimValue True

readAnimValue :: Bool -> Context -> WidgetId -> IO Float
readAnimValue touchView ctx wid = do
  let key = intKey wid
  as <- readIORef (ctxAnimationState ctx)
  case IM.lookup key (asAnimations as) of
    Just a -> do
      when (touchView && IS.notMember key (asViewTouched as)) $
        writeIORef (ctxAnimationState ctx) $! as {asViewTouched = IS.insert key (asViewTouched as)}
      pure $! animationValue a
    Nothing -> do
      -- 'restAt' keeps no zero, so a nonzero value is a stored one.
      let v = IM.findWithDefault 0 key (asAnimRest as)
      when (v /= 0 && IS.notMember key (asRestHeld as)) $
        writeIORef (ctxAnimationState ctx) $! as {asRestHeld = IS.insert key (asRestHeld as)}
      pure v
