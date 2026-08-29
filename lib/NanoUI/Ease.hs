module NanoUI.Ease
  ( evaluateBezier
  ) where

-- Cubic Bezier easing. X control points are clamped to [0, 1] (CSS-style).
-- t=0 and t=1 return the endpoints so Newton cannot pop the first/last frame.
evaluateBezier :: Float -> Float -> Float -> Float -> Float -> Float
evaluateBezier x1 y1 x2 y2 t0
  | t0 <= 0 = 0
  | t0 >= 1 = 1
  | otherwise =
      let p1 = max 0 (min 1 x1)
          p2 = max 0 (min 1 x2)
          tau = solveBezierX p1 p2 t0 0.5 0
       in sampleBezier y1 y2 tau

sampleBezier :: Float -> Float -> Float -> Float
sampleBezier p1 p2 u =
  let one = 1 - u
   in 3 * one * one * u * p1 + 3 * one * u * u * p2 + u * u * u

-- d/du of the unit-interval cubic with P0=0, P3=1.
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
                  nextEst = max 0 (min 1 (estimate - errorVal / safeDeriv))
               in solveBezierX p1 p2 targetT nextEst (iter + 1)
