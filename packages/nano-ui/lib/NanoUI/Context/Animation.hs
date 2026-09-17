-- | Per-widget animations: starting, ticking, settling and reading values.
module NanoUI.Context.Animation
  ( anyAnimating
  , getLiveAnimations
  , isAnimatingKey
  , takeAnimSettled
  , lookupAnimation
  , getAnimRectless
  , setAnimRectless
  , startAnimation
  , startAnimationEase
  , startAnimationEaseDelay
  , startSpring
  , setAnimationValue
  , tickAnimations
  , getAnimationValue
  , getAnimRest
  , pruneAnimRest
  ) where

import Control.Monad (unless, when)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM

import NanoUI.Animation
  ( Animation (..)
  , Ease (..)
  , SpringParams
  , animInProgress
  , animationValue
  , approxEq
  , easeSameSpec
  , springEps
  , stepAnim
  , writeRest
  )
import NanoUI.Context.Core (damageKey, getsDamage, markDirty)
import NanoUI.Context.Types (AnimationState (..), Context (..), DamageState (..), ScrollState (..), intKey)
import NanoUI.Id (WidgetId)
import NanoUI.Layout.Arena (getRect, lookupNodeByKey)
import NanoUI.Types (DamageBounds (..), defaultDamageSlop)

-- | Whether the frame loop has to keep drawing: an animation is running, or a
-- scroller is still gliding onto its target.
{-# INLINE anyAnimating #-}
anyAnimating :: Context -> IO Bool
anyAnimating ctx = do
  anim <- asAnyAnimating <$> readIORef (ctxAnimationState ctx)
  if anim
    then pure True
    else not . IM.null . ssGlides <$> readIORef (ctxScrollState ctx)

{-# INLINE getLiveAnimations #-}
getLiveAnimations :: Context -> IO (IntMap Animation)
getLiveAnimations ctx = IM.filter animInProgress . asAnimations <$> readIORef (ctxAnimationState ctx)

-- | Whether the widget key has an animation in progress. Unlike
-- 'getLiveAnimations' this does not rebuild the animation map.
{-# INLINE isAnimatingKey #-}
isAnimatingKey :: Context -> Int -> IO Bool
isAnimatingKey ctx key =
  maybe False animInProgress . IM.lookup key . asAnimations <$> readIORef (ctxAnimationState ctx)

-- Consecutive frames each live animation has had no nonzero widget rect in the
-- arena. Maintained by 'NanoUI.Damage.updatePrevRects'; used by 'writeDamage'
-- to bound the DamageFull escalation for rect-less animations so a perpetual
-- animation whose widget left the arena (e.g. `keepAnimating` on a widget
-- hidden by a tab switch) stops repainting the whole window after a frame or
-- two, instead of forever.
{-# INLINE getAnimRectless #-}
getAnimRectless :: Context -> IO (IntMap Int)
getAnimRectless ctx = asRectless <$> readIORef (ctxAnimationState ctx)

{-# INLINE setAnimRectless #-}
setAnimRectless :: Context -> IntMap Int -> IO ()
setAnimRectless ctx m =
  modifyIORef' (ctxAnimationState ctx) $ \as -> as {asRectless = m}

takeAnimSettled :: Context -> IO Bool
takeAnimSettled ctx = do
  as <- readIORef (ctxAnimationState ctx)
  if asAnimSettled as
    then do
      writeIORef (ctxAnimationState ctx) $! as {asAnimSettled = False}
      pure True
    else pure False

{-# INLINE lookupAnimation #-}
lookupAnimation :: Context -> WidgetId -> IO (Maybe Animation)
lookupAnimation ctx wid = IM.lookup (intKey wid) . asAnimations <$> readIORef (ctxAnimationState ctx)

{-# INLINE startAnimation #-}
startAnimation :: Context -> WidgetId -> Float -> Float -> Float -> IO ()
startAnimation ctx wid start end dur = startAnimationEase ctx wid start end dur EaseLinear

{-# INLINE startAnimationEase #-}
startAnimationEase :: Context -> WidgetId -> Float -> Float -> Float -> Ease -> IO ()
startAnimationEase ctx wid start end dur ease = startAnimationEaseDelay ctx wid start end dur ease 0

startAnimationEaseDelay :: Context -> WidgetId -> Float -> Float -> Float -> Ease -> Float -> IO ()
startAnimationEaseDelay ctx wid start end dur ease delay
  | dur <= 0 || approxEq start end = settleKey ctx key end
  | otherwise = do
      as <- readIORef (ctxAnimationState ctx)
      let req = max 0 delay
      case IM.lookup key (asAnimations as) of
        Just a@(EaseAnim aStart _ _ _ _ _ _) | approxEq aStart start && easeSameSpec a ease dur req end -> pure ()
        _ ->
          writeIORef (ctxAnimationState ctx) $!
            as
              { asAnimRest = IM.delete key (asAnimRest as)
              , asAnimations = IM.insert key (EaseAnim start end dur 0 ease req req) (asAnimations as)
              , asAnyAnimating = True
              }
      markDirtyIfOrphan ctx key
  where
    key = intKey wid

startSpring :: Context -> WidgetId -> SpringParams -> Float -> IO ()
startSpring ctx wid params target = do
  let key = intKey wid
  as <- readIORef (ctxAnimationState ctx)
  case IM.lookup key (asAnimations as) of
    Just (SpringAnim _ _ t p) | t == target && p == params -> markDirtyIfOrphan ctx key
    running -> do
      let (pos, vel) = case running of
            Just (SpringAnim p v _ _) -> (p, v)
            Just a -> (animationValue a, 0)
            Nothing -> (IM.findWithDefault 0 key (asAnimRest as), 0)
      if abs (pos - target) <= springEps && abs vel <= springEps
        then settleKey ctx key target
        else do
          writeIORef (ctxAnimationState ctx) $!
            as
              { asAnimRest = IM.delete key (asAnimRest as)
              , asAnimations = IM.insert key (SpringAnim pos vel target params) (asAnimations as)
              , asAnyAnimating = True
              }
          markDirtyIfOrphan ctx key

{-# INLINE setAnimationValue #-}
setAnimationValue :: Context -> WidgetId -> Float -> IO ()
setAnimationValue ctx wid val = settleKey ctx (intKey wid) val

tickAnimations :: Context -> Float -> IO ()
tickAnimations ctx dt =
  modifyIORef' (ctxAnimationState ctx) $ \as ->
    if IM.null (asAnimations as)
      then as {asAnyAnimating = False, asAnimSettled = False}
      else
        let stepped = IM.map (stepAnim dt) (asAnimations as)
            (live, done) = IM.partition animInProgress stepped
            rest' = IM.foldlWithKey' writeRest (asAnimRest as) done
         in as
              { asAnimations = live
              , asAnimRest = rest'
              , asAnyAnimating = not (IM.null live)
              , asAnimSettled = not (IM.null done)
              }

markDirtyIfOrphan :: Context -> Int -> IO ()
markDirtyIfOrphan ctx key = do
  hadRect <- IM.member key <$> getsDamage ctx dsPrevRects
  hasNow <- nodeHasKey ctx key
  unless (hadRect || hasNow) (markDirty ctx)

nodeHasKey :: Context -> Int -> IO Bool
nodeHasKey ctx key = do
  mIdx <- lookupNodeByKey (ctxNodeArena ctx) key
  case mIdx of
    Nothing -> pure False
    Just idx -> do
      (_, _, w, h) <- getRect (ctxNodeArena ctx) idx
      pure (w > 0 && h > 0)

settleKey :: Context -> Int -> Float -> IO ()
settleKey ctx key val = do
  as <- readIORef (ctxAnimationState ctx)
  let rest = asAnimRest as
      prevRest = IM.findWithDefault 0 key rest
      prevLive = IM.lookup key (asAnimations as)
      restChanged
        | approxEq val 0 = IM.member key rest
        | otherwise = prevRest /= val
      rest'
        | not restChanged = rest
        | approxEq val 0 = IM.delete key rest
        | otherwise = IM.insert key val rest
  -- A spring at rest settles every frame; write only what changes.
  case prevLive of
    Just _ -> do
      let anims' = IM.delete key (asAnimations as)
      writeIORef (ctxAnimationState ctx) $!
        as {asAnimations = anims', asAnimRest = rest', asAnyAnimating = not (IM.null anims')}
    Nothing -> when restChanged $ writeIORef (ctxAnimationState ctx) $! as {asAnimRest = rest'}
  when (maybe (not (approxEq prevRest val)) (not . approxEq val . animationValue) prevLive) $ do
    damageKey ctx key (DamageInflated defaultDamageSlop)
    markDirty ctx

getAnimationValue :: Context -> WidgetId -> IO Float
getAnimationValue ctx wid = do
  let key = intKey wid
  as <- readIORef (ctxAnimationState ctx)
  case IM.lookup key (asAnimations as) of
    Just a -> pure $! animationValue a
    Nothing -> pure $! IM.findWithDefault 0 key (asAnimRest as)

{-# INLINE getAnimRest #-}
getAnimRest :: Context -> IO (IntMap Float)
getAnimRest ctx = asAnimRest <$> readIORef (ctxAnimationState ctx)

{-# INLINE pruneAnimRest #-}
pruneAnimRest :: Context -> (Int -> Bool) -> IO ()
pruneAnimRest ctx shouldKeep =
  modifyIORef' (ctxAnimationState ctx) $ \as ->
    as {asAnimRest = IM.filterWithKey (\k _ -> shouldKeep k) (asAnimRest as)}
