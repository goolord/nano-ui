module Cases.ShrinkDamage (tests) where

import Spec
import Data.Primitive.SmallArray (smallArrayFromList)

tests :: [Spec]
tests =
  [ spec "drawing-shrink-damage" runDrawingShrinkDamageTest
  , spec "hovered-button-shrink-damage" runHoveredButtonShrinkDamageTest
  ]

-- | A drawing whose rect shrinks repaints the strip it vacated: the second
-- frame's damage covers its old rect as well as its new one. The pointer
-- rests at the origin, over the drawing, so its hover fade is running and the
-- frame is clipped rather than repainted whole. The drawing is the root, whose
-- viewport is its own rect, so clipping the old rect by the new viewport
-- would leave the vacated strip out.
runDrawingShrinkDamageTest :: Context -> IORef Int -> IO ()
runDrawingShrinkDamageTest ctx failed = do
  let inp = withInput 200 100
      ui w = void (drawing (fixedWH w 20) (\r -> smallArrayFromList [FillRect r (colorRGBA 255 0 0 255)]))
  _ <- runFrame ctx inp (ui 40)
  assertEq failed [Rect 0 0 40 20] =<< arenaRects ctx
  _ <- takeDamage ctx
  _ <- runFrame ctx inp (ui 20)
  assertEq failed [Rect 0 0 20 20] =<< arenaRects ctx
  dmg <- takeDamage ctx
  assert failed (damageCovers dmg (Rect 0 0 40 20))

-- | The same holds for any node under a shrinking root: a hovered button in a
-- column as wide as the button repaints where the button was once both
-- shrink.
runHoveredButtonShrinkDamageTest :: Context -> IORef Int -> IO ()
runHoveredButtonShrinkDamageTest ctx failed = do
  let inp = (withInput 200 100) {inputMousePos = V2 5 5}
      ui w = column (buttonWith (fixedWH w 20) "")
      old = Rect 3 3 40 20
  _ <- warmup2 ctx inp (ui 40)
  assert failed . elem old =<< arenaRects ctx
  _ <- takeDamage ctx
  _ <- runFrame ctx inp (ui 20)
  assert failed . elem (Rect 3 3 20 20) =<< arenaRects ctx
  dmg <- takeDamage ctx
  assert failed (damageCovers dmg old)

damageCovers :: Damage -> Rect -> Bool
damageCovers DamageFull _ = True
damageCovers (DamageClip clip) r = covers clip r
