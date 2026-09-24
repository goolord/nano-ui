module Cases.ShrinkDamage (tests) where

import Spec
import Data.Primitive.SmallArray (smallArrayFromList)

tests :: [Spec]
tests =
  [ spec "drawing-shrink-damage" runDrawingShrinkDamageTest
  , spec "hovered-button-shrink-damage" runHoveredButtonShrinkDamageTest
  , spec "root-overflow-grow-damage" runRootOverflowGrowDamageTest
  , spec "root-overflow-shrink-damage" runRootOverflowShrinkDamageTest
  ]

-- | A drawing whose rect shrinks repaints the strip it vacated: the second
-- frame's damage covers its old rect as well as its new one. The pointer
-- rests at the origin, over the drawing, so its hover fade is running and the
-- frame is clipped rather than repainted whole. The drawing is the root, so
-- its old rect must not be clipped to its own new rect.
runDrawingShrinkDamageTest :: Context -> IORef Int -> IO ()
runDrawingShrinkDamageTest ctx failed = do
  let inp = withInput 200 100
      ui w = redDrawing w 20
  _ <- runFrame ctx inp (ui 40)
  assertEq failed [Rect 0 0 40 20] =<< arenaRects ctx
  _ <- takeDamage ctx
  _ <- runFrame ctx inp (ui 20)
  assertEq failed [Rect 0 0 20 20] =<< arenaRects ctx
  dmg <- takeDamage ctx
  assert failed (damageCovers dmg (Rect 0 0 40 20))

-- | The same holds for any hovered node whose parent shrinks with it: a
-- button in a column as wide as the button repaints where the button was once
-- both shrink.
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

-- | A root smaller than its content still paints the overflow, so a drawing
-- that overflowed a short root repaints all it drew once it shrinks, even as
-- the root grows. The root is clipped to the window, as paint clips it, not
-- to its own rect.
runRootOverflowGrowDamageTest :: Context -> IORef Int -> IO ()
runRootOverflowGrowDamageTest ctx failed = do
  let inp = (withInput 200 100) {inputMousePos = V2 1 1}
      ui h dh = columnWith (fixedWH 40 h . padAll 0) (redDrawing 40 dh)
  _ <- warmup2 ctx inp (ui 20 40)
  _ <- takeDamage ctx
  _ <- runFrame ctx inp (ui 40 20)
  dmg <- takeDamage ctx
  assert failed (damageCovers dmg (Rect 0 0 40 40))

-- | The same with the root's size unchanged.
runRootOverflowShrinkDamageTest :: Context -> IORef Int -> IO ()
runRootOverflowShrinkDamageTest ctx failed = do
  let inp = (withInput 200 100) {inputMousePos = V2 1 1}
      ui dh = columnWith (fixedWH 40 20 . padAll 0) (redDrawing 40 dh)
  _ <- warmup2 ctx inp (ui 40)
  _ <- takeDamage ctx
  _ <- runFrame ctx inp (ui 30)
  dmg <- takeDamage ctx
  assert failed (damageCovers dmg (Rect 0 0 40 40))

redDrawing :: Float -> Float -> NanoUI ()
redDrawing w h = void (drawing (fixedWH w h) (\r -> smallArrayFromList [FillRect r (colorRGBA 255 0 0 255)]))

damageCovers :: Damage -> Rect -> Bool
damageCovers DamageFull _ = True
damageCovers (DamageClip clip) r = covers clip r
