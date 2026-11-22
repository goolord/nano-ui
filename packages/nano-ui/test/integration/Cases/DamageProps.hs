{-# OPTIONS_GHC -O0 #-}
-- | Damage soundness over random interaction scripts: whatever a frame paints
-- differently from the frame before lies inside that frame's damage.
--
-- A headless context paints every frame whole ('ctxPaintFull' is on by
-- default) and still reports damage, so two consecutive frames can be
-- compared directly. A frame's picture is the multiset of its draw
-- triangles (vertices, texture and the clip of the command that draws them,
-- hashed) and of its text spans (rect, text, colours and clip), which the backend
-- rasterises itself. A triangle or span in one frame and not the other is
-- a change; where it shows is its bounding box cut to its clip. Every change
-- with any area must lie inside the union of the frame's damage pieces (or
-- its damage clip when there are no pieces), inflated by a pixel for
-- antialiased edges. 'DamageFull' covers everything.
--
-- Draw order is not compared: a frame that drew the same primitives in
-- another order counts as unchanged. A primitive hidden under an opaque one
-- still counts, so the check can only be stricter than the pixels need.
module Cases.DamageProps (tests) where

import Spec
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import NanoUI.Testing.Sim qualified as Sim
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, counterexample, elements, frequency, ioProperty, vectorOf)

tests :: [Spec]
tests =
  [propSpec "damage-props" 3 60 damageSound]

-- | One user action, with points in a 400 x 300 window.
data Act
  = AMove V2
  | AClick V2
  | ADrag V2 V2
  | AWheel V2 Float
  | AType T.Text
  | APress Key
  | AWait Float
  deriving (Show)

-- | A point, most often in the top left where the views put their widgets.
genPoint :: Gen V2
genPoint = frequency [(1, within 400 300), (3, within 260 180)]
  where
    within w h = V2 <$> (fromIntegral <$> choose (0 :: Int, w)) <*> (fromIntegral <$> choose (0 :: Int, h))

instance Arbitrary Act where
  arbitrary =
    frequency
      [ (3, AMove <$> genPoint)
      , (4, AClick <$> genPoint)
      , (1, ADrag <$> genPoint <*> genPoint)
      , (1, AWheel <$> genPoint <*> elements [-3, -1, 1, 3])
      , (1, AType <$> elements ["a", "xy", " ", "\x732B"])
      , (2, APress <$> elements [KeyTab, KeyEscape, KeyLeft, KeyRight, KeyUp, KeyDown, KeyEnter, KeyBackspace])
      , (2, AWait <$> elements [0.05, 0.2, 1])
      ]

-- | Which view a script drives, and the script.
data Script = Script Int [Act]
  deriving (Show)

instance Arbitrary Script where
  arbitrary = Script <$> choose (0, length views - 1) <*> (choose (4, 25) >>= (`vectorOf` arbitrary))
  shrink (Script v acts) = [Script v (take i acts <> drop (i + 1) acts) | i <- [0 .. length acts - 1]]

-- | The views, each built fresh for a script: buttons with a counter, a form
-- of text input, checkbox and slider, a scroll area, a select above tabs,
-- and a list that grows.
views :: [IO (NanoUI ())]
views =
  [ do
      n <- newVar (0 :: Int)
      pure . columnWith (padAll 10 . gap 6) $ do
        k <- readVar n
        label (T.pack ("count " <> show k))
        rowWith (gap 4) $ do
          plus <- button "plus"
          when plus (modifyVar n (+ 1))
          minus <- button "minus"
          when minus (modifyVar n (subtract 1))
        void (button "idle")
  , do
      t <- newVar ("text" :: T.Text)
      c <- newVar False
      s <- newVar (30 :: Float)
      pure . columnWith (padAll 10 . gap 6) $ do
        textInput t
        checkbox "check" c
        sliderWith fillW 0 100 s
        v <- readVar s
        label (T.pack (show (round v :: Int)))
  , pure . scrollWith (fixedWH 300 200) $
      forM_ [1 .. 30 :: Int] $ \i -> row (label (T.pack ("row " <> show i)) >> void (button "go"))
  , do
      sel <- newVar (0 :: Int)
      active <- newVar (0 :: Int)
      pure . columnWith (padAll 10 . gap 6) $ do
        selectWith (fixedW 160) ["one", "two", "three"] sel
        tabs active [tab k (T.pack ("tab " <> show k)) (label (T.pack ("body " <> show k))) | k <- [0 .. 2 :: Int]]
  , do
      items <- newVar (2 :: Int)
      pure . columnWith (padAll 10 . gap 4) $ do
        add <- button "add"
        when add (modifyVar items (+ 1))
        remove <- button "remove"
        when remove (modifyVar items (max 0 . subtract 1))
        k <- readVar items
        forM_ [1 .. k] $ \i -> label (T.pack ("item " <> show i))
  ]

-- | A drawn primitive's identity: a triangle's hash ('trianglePicture'), or
-- a text span's layer, rect, clip, colours and text.
data Prim = Tri Int | Span [Float] T.Text
  deriving (Eq, Ord, Show)

-- | The frame's picture: each primitive with its count and visible rect.
picture :: Context -> Input -> DrawData -> IO (M.Map Prim (Int, Rect))
picture ctx inp dd = do
  tris <- M.mapKeysMonotonic Tri <$> trianglePicture dd
  (base, over) <- collectRasterSpans ctx inp
  let texts = [textPrim layer s | (layer, ss) <- [(0, base), (1, over)], s <- ss]
  pure (M.unionWith count tris (M.fromListWith count [(k, (1, r)) | (k, r) <- texts]))
  where
    count (a, r) (b, _) = (a + b, r)
    rectKey (Rect x y w h) = [x, y, w, h]
    textPrim :: Float -> (Rect, T.Text, Color, Color, Rect) -> (Prim, Rect)
    textPrim layer (r, t, fg, bg, clip) =
      (Span (layer : rectKey r <> rectKey clip <> colorKey fg <> colorKey bg) t, fromMaybe (Rect 0 0 0 0) (rectIntersect r clip))
    colorKey c = [fromIntegral (colorR c), fromIntegral (colorG c), fromIntegral (colorB c), fromIntegral (colorA c)]

-- | The parts of @r@ outside @cut@: up to four rects.
subtractRect :: Rect -> Rect -> [Rect]
subtractRect r@(Rect x y w h) cut = case rectIntersect r cut of
  Nothing -> [r]
  Just (Rect cx cy cw ch) ->
    filter (\(Rect _ _ a b) -> a > 1e-3 && b > 1e-3)
      [ Rect x y w (cy - y)
      , Rect x (cy + ch) w (y + h - cy - ch)
      , Rect x cy (cx - x) ch
      , Rect (cx + cw) cy (x + w - cx - cw) ch
      ]

-- | Whether the union of @pieces@ covers @r@.
coveredBy :: [Rect] -> Rect -> Bool
coveredBy pieces r = null (foldl' (\rs p -> concatMap (`subtractRect` p) rs) [r] pieces)

-- | Run a frame headlessly, then compare its picture with the last frame's
-- against its damage, appending what it found uncovered to @found@.
checkedFrame :: IORef (Maybe (M.Map Prim (Int, Rect))) -> IORef [String] -> Context -> Input -> NanoUI a -> IO (a, Bool)
checkedFrame lastRef found ctx inp view = do
  (a, dd, again) <- runFrame ctx inp view
  now <- picture ctx inp dd
  dmg <- takeDamage ctx
  pieces <- takeDamagePieces ctx
  previous <- readIORef lastRef
  writeIORef lastRef (Just now)
  case (previous, dmg) of
    (Nothing, _) -> pure ()
    (_, DamageFull) -> pure ()
    (Just before, DamageClip clip) -> do
      let regions = map (rectInflate 1) (if null pieces then [clip] else pieces)
          changed = M.elems (M.differenceWith gone before now) <> M.elems (M.differenceWith gone now before)
          gone (n, r) (m, _) = if n > m then Just (n - m, r) else Nothing
          uncovered = [r | (_, r) <- changed, rectW r > 1e-3, rectH r > 1e-3, not (coveredBy regions r)]
      unless (null uncovered) $
        modifyIORef' found (<> ["changes at " <> show (take 3 uncovered) <> " outside damage " <> show dmg <> " pieces " <> show pieces])
  pure (a, again)

runAct :: Act -> Sim.Sim () ()
runAct = \case
  AMove p -> void (Sim.moveTo (Sim.At p))
  AClick p -> void (Sim.click (Sim.At p))
  ADrag p q -> void (Sim.drag (Sim.At p) (Sim.At q) 3)
  AWheel p dy -> void (Sim.wheel (Sim.At p) (V2 0 dy))
  AType t -> void (Sim.typeText t)
  APress k -> void (Sim.pressKey k)
  AWait dt -> void (Sim.advance dt)

damageSound :: Script -> Property
damageSound (Script which acts) = ioProperty $ do
  view <- views !! which
  ctx <- newContext
  quiet <- newIORef 0
  lastRef <- newIORef Nothing
  found <- newIORef []
  let opts = Sim.defaultSimOptions {Sim.simFrameRunner = checkedFrame lastRef found}
  Sim.runSimWith opts ctx quiet (withInput 400 300) view (mapM_ runAct acts >> void (Sim.advance 2) >> void Sim.settle)
  problems <- readIORef found
  pure (counterexample (unlines (take 5 problems)) (null problems))
