module NanoUI.Spring
  ( SpringParams (..)
  , presetBouncy
  , presetSmooth
  , presetStiff
  , springEps
  , stepSpring
  ) where

-- Mass-spring-damper params. k, c, m in the standard second-order form.
data SpringParams = SpringParams
  { springStiffness :: {-# UNPACK #-} !Float
  , springDamping :: {-# UNPACK #-} !Float
  , springMass :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

presetBouncy :: SpringParams
presetBouncy = SpringParams {springStiffness = 180, springDamping = 12, springMass = 1}

presetSmooth :: SpringParams
presetSmooth = SpringParams {springStiffness = 120, springDamping = 20, springMass = 1}

presetStiff :: SpringParams
presetStiff = SpringParams {springStiffness = 300, springDamping = 30, springMass = 1}

-- Position and velocity must both fall under this to snap and settle.
springEps :: Float
springEps = 1e-3

-- Cap a hitch so a stiff spring does not explode.
maxSubstep :: Float
maxSubstep = 1 / 30

maxSubsteps :: Int
maxSubsteps = 32

-- Advance one scalar with RK4. Large dt is split into maxSubstep chunks.
-- After maxSubsteps, leftover time is dropped.
stepSpring :: SpringParams -> Float -> Float -> Float -> Float -> (Float, Float)
stepSpring params x v target dt
  | dt <= 0 = (x, v)
  | otherwise = go x v dt 0
  where
    go pos vel remain n
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
      k4x = v + dt * k3x
      xNext = x + (dt / 6) * (k1x + 2 * k2x + 2 * k3x + k4x)
      vNext = v + (dt / 6) * (k1v + 2 * k2v + 2 * k3v + k4v)
   in (xNext, vNext)
  where
    k = max 0 (springStiffness params)
    c = max 0 (springDamping params)
    m = max 1e-6 (springMass params)
    accel pos vel = (-k * (pos - xTarget) - c * vel) / m
