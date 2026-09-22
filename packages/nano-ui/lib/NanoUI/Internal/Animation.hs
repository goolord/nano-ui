-- | Scalar easing and spring integration. Durations and elapsed time use seconds.
module NanoUI.Internal.Animation
  ( Ease (..)
  , Animation (..)
  , SpringParams (..)
  , presetBouncy
  , presetSmooth
  , presetStiff
  , springEps
  , applyEase
  , approxEq
  , animInProgress
  , animationValue
  , easeSameSpec
  , stepAnim
  ) where

import NanoUI.Internal.Types (clamp01)

-- Cubic Bezier easing. X control points are clamped to [0, 1] (CSS-style).
-- t=0 and t=1 return the endpoints so Newton cannot pop the first/last frame.
evaluateBezier :: Float -> Float -> Float -> Float -> Float -> Float
evaluateBezier x1 y1 x2 y2 t0
  | t0 <= 0 = 0
  | t0 >= 1 = 1
  | otherwise =
      let p1 = clamp01 x1
          p2 = clamp01 x2
          tau = solveBezierX p1 p2 t0 0.5 0
       in sampleBezier y1 y2 tau

sampleBezier :: Float -> Float -> Float -> Float
sampleBezier p1 p2 u =
  let one = 1 - u
   in 3 * one * one * u * p1 + 3 * one * u * u * p2 + u * u * u

bezierDeriv :: Float -> Float -> Float -> Float
bezierDeriv p1 p2 u =
  let one = 1 - u
   in 3 * one * one * p1 + 6 * one * u * (p2 - p1) + 3 * u * u * (1 - p2)

solveBezierX :: Float -> Float -> Float -> Float -> Int -> Float
solveBezierX p1 p2 targetT estimate iter
  | iter >= 8 = estimate
  | otherwise =
      let currentX = sampleBezier p1 p2 estimate
          errorVal = currentX - targetT
       in if abs errorVal < 1e-4
            then estimate
            else
              let deriv = bezierDeriv p1 p2 estimate
                  safeDeriv =
                    if abs deriv < 1e-6
                      then if deriv >= 0 then 1e-6 else -1e-6
                      else deriv
                  nextEst = clamp01 (estimate - errorVal / safeDeriv)
               in solveBezierX p1 p2 targetT nextEst (iter + 1)

-- | Spring stiffness, damping, and mass. Time uses seconds. Integration clamps
-- stiffness/damping to non-negative values and mass to at least 1e-6.
data SpringParams = SpringParams
  { springStiffness :: {-# UNPACK #-} !Float
  , springDamping :: {-# UNPACK #-} !Float
  , springMass :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

-- | Lightly damped spring: stiffness 180, damping 12, mass 1.
presetBouncy :: SpringParams
presetBouncy = SpringParams {springStiffness = 180, springDamping = 12, springMass = 1}

-- | Near-critically damped spring: stiffness 120, damping 20, mass 1.
presetSmooth :: SpringParams
presetSmooth = SpringParams {springStiffness = 120, springDamping = 20, springMass = 1}

-- | Faster spring: stiffness 300, damping 30, mass 1.
presetStiff :: SpringParams
presetStiff = SpringParams {springStiffness = 300, springDamping = 30, springMass = 1}

springEps :: Float
springEps = 1e-3

maxSubstep :: Float
maxSubstep = 1 / 30

maxSubsteps :: Int
maxSubsteps = 32

-- | Advance position and velocity toward a target by elapsed seconds.
-- Non-positive time leaves both unchanged; at most 32 substeps of 1/30 s are taken.
stepSpring :: SpringParams -> Float -> Float -> Float -> Float -> (Float, Float)
stepSpring params x v target dt
  | dt <= 0 = (x, v)
  | otherwise = go x v dt 0
  where
    go !pos !vel remain n
      | remain <= 1e-8 || n >= maxSubsteps = (pos, vel)
      | otherwise =
          let h = min maxSubstep remain
              (pos', vel') = rk4 params pos vel target h
           in go pos' vel' (remain - h) (n + 1)

rk4 :: SpringParams -> Float -> Float -> Float -> Float -> (Float, Float)
rk4 params x v xTarget dt =
  let k1v = accel x v
      k1x = v
      k2v = accel (x + 0.5 * dt * k1x) (v + 0.5 * dt * k1v)
      k2x = v + 0.5 * dt * k1v
      k3v = accel (x + 0.5 * dt * k2x) (v + 0.5 * dt * k2v)
      k3x = v + 0.5 * dt * k2v
      k4v = accel (x + dt * k3x) (v + dt * k3v)
      k4x = v + dt * k3v
      xNext = x + (dt / 6) * (k1x + 2 * k2x + 2 * k3x + k4x)
      vNext = v + (dt / 6) * (k1v + 2 * k2v + 2 * k3v + k4v)
   in (xNext, vNext)
  where
    k = max 0 (springStiffness params)
    c = max 0 (springDamping params)
    m = max 1e-6 (springMass params)
    accel pos vel = (-k * (pos - xTarget) - c * vel) / m

-- | Progress curves. Cubic Bezier arguments are @x1 y1 x2 y2@ for the two
-- control points between endpoints (0,0) and (1,1).
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

-- | Running scalar animation. 'EaseAnim' stores start, end, duration, elapsed
-- time, curve, remaining delay, and requested delay (times in seconds).
-- 'SpringAnim' stores position, velocity, target, and spring parameters.
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

-- | Whether a tween matches curve, duration, requested delay, and target.
-- Numeric comparisons use 'approxEq'; springs never match.
easeSameSpec :: Animation -> Ease -> Float -> Float -> Float -> Bool
easeSameSpec (EaseAnim _ end dur _ ease _ delayReq) wantEase wantDur delay target =
  ease == wantEase
    && approxEq dur wantDur
    && approxEq delay delayReq
    && approxEq end target
easeSameSpec _ _ _ _ _ = False

-- | Map unit progress through an easing curve. Input is clamped to [0, 1].
-- EaseOutBack may return a value outside that range (overshoot).
applyEase :: Ease -> Float -> Float
applyEase ease t0 =
  let t = clamp01 t0
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

-- | Absolute difference no greater than 1e-4.
approxEq :: Float -> Float -> Bool
approxEq a b = abs (a - b) <= 1e-4

-- | Whether a tween has time remaining or a spring has position/velocity
-- error greater than 1e-3. Delayed nontrivial tweens count as running.
{-# INLINE animInProgress #-}
animInProgress :: Animation -> Bool
animInProgress (EaseAnim start end dur elapsed _ delay _) =
  not (approxEq start end)
    && dur > 0
    && (delay > 0 || elapsed < dur)
animInProgress (SpringAnim pos vel target _) =
  abs (pos - target) > springEps || abs vel > springEps

-- | Current interpolated value, or the target for a completed tween.
{-# INLINE animationValue #-}
animationValue :: Animation -> Float
animationValue a@(EaseAnim start end dur elapsed ease delay _)
  | not (animInProgress a) = end
  | delay > 0 = start
  | otherwise =
      let t = min 1 (elapsed / max 0.001 dur)
       in start + (end - start) * applyEase ease t
animationValue (SpringAnim pos _ _ _) = pos

-- | Advance an animation by elapsed seconds, including its delay.
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
