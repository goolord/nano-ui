module NanoUI.Animation
  ( Ease (..)
  , Animation (..)
  , applyEase
  , approxEq
  , animInProgress
  , animationValue
  , easeSameSpec
  , stepAnim
  , writeRest
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import NanoUI.Ease (evaluateBezier)
import NanoUI.Spring (SpringParams, springEps, stepSpring)

data Ease
  = EaseLinear
  | EaseInQuad
  | EaseOutQuad
  | EaseInOutQuad
  | EaseInCubic
  | EaseOutCubic
  | EaseInOutCubic
  | EaseOutBack
  | EaseCubicBezier
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
  deriving (Eq, Show)

-- EaseAnim start end duration elapsed ease delay delayReq
-- SpringAnim pos vel target params
data Animation
  = EaseAnim
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !Ease
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
  | SpringAnim
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !SpringParams
  deriving (Eq, Show)

-- True when this ease slot matches the call-site spec and target.
easeSameSpec :: Animation -> Ease -> Float -> Float -> Float -> Bool
easeSameSpec (EaseAnim _ end dur _ ease _ delayReq) wantEase wantDur delay target =
  ease == wantEase
    && approxEq dur wantDur
    && approxEq delay delayReq
    && approxEq end target
easeSameSpec _ _ _ _ _ = False

-- Map unit progress through an easing curve. Input is clamped to [0, 1].
-- EaseOutBack may return a value outside that range (overshoot).
applyEase :: Ease -> Float -> Float
applyEase ease t0 =
  let t = max 0 (min 1 t0)
   in case ease of
        EaseLinear -> t
        EaseInQuad -> t * t
        EaseOutQuad -> t * (2 - t)
        EaseInOutQuad
          | t < 0.5 -> 2 * t * t
          | otherwise -> -1 + (4 - 2 * t) * t
        EaseInCubic -> t * t * t
        EaseOutCubic ->
          let u = 1 - t
           in 1 - u * u * u
        EaseInOutCubic
          | t < 0.5 -> 4 * t * t * t
          | otherwise ->
              let u = -2 * t + 2
               in 1 - (u * u * u) / 2
        EaseOutBack ->
          let c1 = 1.70158
              c3 = c1 + 1
              u = t - 1
           in 1 + c3 * u * u * u + c1 * u * u
        EaseCubicBezier x1 y1 x2 y2 -> evaluateBezier x1 y1 x2 y2 t

approxEq :: Float -> Float -> Bool
approxEq a b = abs (a - b) <= 1e-4

{-# INLINE animInProgress #-}
animInProgress :: Animation -> Bool
animInProgress (EaseAnim start end dur elapsed _ delay _) =
  not (approxEq start end)
    && dur > 0
    && (delay > 0 || elapsed < dur)
animInProgress (SpringAnim pos vel target _) =
  abs (pos - target) > springEps || abs vel > springEps

{-# INLINE animationValue #-}
animationValue :: Animation -> Float
animationValue a@(EaseAnim start end dur elapsed ease delay _)
  | not (animInProgress a) = end
  | delay > 0 = start
  | otherwise =
      let t = min 1 (elapsed / max 0.001 dur)
       in start + (end - start) * applyEase ease t
animationValue (SpringAnim pos _ _ _) = pos

stepAnim :: Float -> Animation -> Animation
stepAnim dt a@(EaseAnim start end dur elapsed ease delay delayReq)
  | not (animInProgress a) = a
  | delay > 0 =
      let remain = delay - dt
       in if remain > 0
            then EaseAnim start end dur elapsed ease remain delayReq
            else stepAnim (negate remain) (EaseAnim start end dur elapsed ease 0 delayReq)
  | otherwise =
      let next = elapsed + dt
       in if next >= dur
            then EaseAnim end end 0 0 ease 0 0
            else EaseAnim start end dur next ease 0 delayReq
stepAnim dt (SpringAnim pos vel target params) =
  let (pos', vel') = stepSpring params pos vel target dt
   in if abs (pos' - target) <= springEps && abs vel' <= springEps
        then SpringAnim target 0 target params
        else SpringAnim pos' vel' target params

writeRest :: IntMap Float -> Int -> Animation -> IntMap Float
writeRest rest key a =
  let end = case a of
        EaseAnim _ e _ _ _ _ _ -> e
        SpringAnim _ _ t _ -> t
   in if approxEq end 0
        then IM.delete key rest
        else IM.insert key end rest
