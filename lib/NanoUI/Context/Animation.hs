module NanoUI.Context.Animation
  ( anyAnimating
  , startAnimation
  , startAnimationEase
  , startAnimationEaseDelay
  , startSpring
  , setAnimationValue
  , tickAnimations
  , getAnimationValue
  ) where

import Control.Monad (when)
import Data.Maybe (isNothing)
import qualified Data.IntMap.Strict as IM
import NanoUI.Animation
  ( Animation (..)
  , Ease (..)
  , animInProgress
  , animationValue
  , approxEq
  , stepAnim
  , writeRest
  )
import NanoUI.Context.Internal
  ( Context (..)
  , intKey
  , markDirty
  )
import NanoUI.Context.PrevRects (getPrevRectByKey)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import Data.IORef (readIORef, writeIORef)
import NanoUI.Layout.Arena (arenaCount, getRect, getWidgetId)
import NanoUI.Spring (SpringParams, springEps)

refreshAnimating :: Context -> IO ()
refreshAnimating ctx = do
  anims <- readIORef (ctxAnimations ctx)
  writeIORef (ctxAnyAnimating ctx) (any animInProgress anims)

{-# INLINE anyAnimating #-}
anyAnimating :: Context -> IO Bool
anyAnimating ctx = readIORef (ctxAnyAnimating ctx)

{-# INLINE startAnimation #-}
startAnimation :: Context -> WidgetId -> Float -> Float -> Float -> IO ()
startAnimation ctx wid start end dur = startAnimationEase ctx wid start end dur EaseLinear

{-# INLINE startAnimationEase #-}
startAnimationEase :: Context -> WidgetId -> Float -> Float -> Float -> Ease -> IO ()
startAnimationEase ctx wid start end dur ease =
  startAnimationEaseDelay ctx wid start end dur ease 0

{-# INLINE startAnimationEaseDelay #-}
startAnimationEaseDelay :: Context -> WidgetId -> Float -> Float -> Float -> Ease -> Float -> IO ()
startAnimationEaseDelay ctx wid start end dur ease delay = do
  let key = intKey wid
  anims <- readIORef (ctxAnimations ctx)
  if dur <= 0 || approxEq start end
    then settleKey ctx key end
    else do
      let req = max 0 delay
          (elapsed, delayLeft) =
            case IM.lookup key anims of
              Just (EaseAnim aStart aEnd aDur aElapsed aEase aDelay aDelayReq)
                | approxEq aStart start
                    && approxEq aEnd end
                    && aEase == ease
                    && approxEq aDur dur
                    && approxEq req aDelayReq ->
                    (aElapsed, aDelay)
              _ -> (0, req)
      rest <- readIORef (ctxAnimRest ctx)
      writeIORef (ctxAnimRest ctx) (IM.delete key rest)
      writeIORef
        (ctxAnimations ctx)
        (IM.insert key (EaseAnim start end dur elapsed ease delayLeft req) anims)
      writeIORef (ctxAnyAnimating ctx) True
      markDirtyIfOrphan ctx key

-- Spring toward target. An existing spring keeps position and velocity.
startSpring :: Context -> WidgetId -> SpringParams -> Float -> IO ()
startSpring ctx wid params target = do
  let key = intKey wid
  anims <- readIORef (ctxAnimations ctx)
  cur <- case IM.lookup key anims of
    Just a -> pure (animationValue a)
    Nothing -> do
      rest <- readIORef (ctxAnimRest ctx)
      pure (IM.findWithDefault 0 key rest)
  let (pos, vel) =
        case IM.lookup key anims of
          Just (SpringAnim p v _ _) -> (p, v)
          _ -> (cur, 0)
  if abs (pos - target) <= springEps && abs vel <= springEps
    then settleKey ctx key target
    else do
      rest <- readIORef (ctxAnimRest ctx)
      writeIORef (ctxAnimRest ctx) (IM.delete key rest)
      writeIORef
        (ctxAnimations ctx)
        (IM.insert key (SpringAnim pos vel target params) anims)
      writeIORef (ctxAnyAnimating ctx) True
      markDirtyIfOrphan ctx key

{-# INLINE setAnimationValue #-}
setAnimationValue :: Context -> WidgetId -> Float -> IO ()
setAnimationValue ctx wid val = settleKey ctx (intKey wid) val

{-# INLINE tickAnimations #-}
tickAnimations :: Context -> Float -> IO ()
tickAnimations ctx dt = do
  anims <- readIORef (ctxAnimations ctx)
  if IM.null anims
    then do
      writeIORef (ctxAnyAnimating ctx) False
      writeIORef (ctxAnimSettled ctx) False
    else do
      let stepped = IM.map (stepAnim dt) anims
          (live, done) = IM.partition animInProgress stepped
      writeIORef (ctxAnimations ctx) live
      rest <- readIORef (ctxAnimRest ctx)
      writeIORef (ctxAnimRest ctx) (IM.foldlWithKey' writeRest rest done)
      writeIORef (ctxAnyAnimating ctx) (not (IM.null live))
      writeIORef (ctxAnimSettled ctx) (not (IM.null done))

markDirtyIfOrphan :: Context -> Int -> IO ()
markDirtyIfOrphan ctx key = do
  mprev <- getPrevRectByKey ctx key
  hasNow <- nodeHasKey ctx key
  when (isNothing mprev && not hasNow) (markDirty ctx)

-- Call-site tweens have no node. Hover ids exist in the arena this frame
-- before prev rects are written; those must not force Full.
nodeHasKey :: Context -> Int -> IO Bool
nodeHasKey ctx key = do
  n <- arenaCount (ctxNodeArena ctx)
  let go i
        | i >= n = pure False
        | otherwise = do
            wid <- getWidgetId (ctxNodeArena ctx) i
            if intKey wid == key && hashWidgetId wid /= 0
              then do
                (_, _, w, h) <- getRect (ctxNodeArena ctx) i
                if w > 0 && h > 0 then pure True else go (i + 1)
              else go (i + 1)
  go 0

settleKey :: Context -> Int -> Float -> IO ()
settleKey ctx key val = do
  anims <- readIORef (ctxAnimations ctx)
  rest <- readIORef (ctxAnimRest ctx)
  let prevRest = IM.findWithDefault 0 key rest
      prevLive = fmap animationValue (IM.lookup key anims)
      changed =
        case prevLive of
          Just v -> not (approxEq v val)
          Nothing -> not (approxEq prevRest val)
  writeIORef (ctxAnimations ctx) (IM.delete key anims)
  writeIORef
    (ctxAnimRest ctx)
    ( if approxEq val 0
        then IM.delete key rest
        else IM.insert key val rest
    )
  when changed (markDirty ctx)
  refreshAnimating ctx

{-# INLINE getAnimationValue #-}
getAnimationValue :: Context -> WidgetId -> IO Float
getAnimationValue ctx wid = do
  let key = intKey wid
  anims <- readIORef (ctxAnimations ctx)
  case IM.lookup key anims of
    Just a -> pure (animationValue a)
    Nothing -> do
      rest <- readIORef (ctxAnimRest ctx)
      pure (IM.findWithDefault 0 key rest)
