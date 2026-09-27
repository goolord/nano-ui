-- | A solve that puts back what the last one placed lays out as solving
-- from scratch does.
module Cases.LayoutReuse (tests) where

import Spec
import Data.Bits (shiftR, xor)
import Data.Text qualified as T
import Data.Word (Word64)
import NanoUI.Adornment (affix, leading, trailing)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (arenaCount, getNodeRect, getNodeType, getNodeValue, getScrollContentW, isScrollNode)

tests :: [Spec]
tests =
  [ spec "position-reuse-cases" runPositionReuseCasesTest
  , spec "position-reuse-random" runPositionReuseRandomTest
  ]

-- | Every node's rect and each scroll container's content extents.
layoutOf :: Context -> IO [(Rect, Maybe (Float, Float))]
layoutOf ctx = do
  let na = ctxNodeArena ctx
      scrollOf i = getNodeType na i >>= \nt ->
        if isScrollNode nt then Just <$> ((,) <$> getNodeValue na i <*> getScrollContentW na i) else pure Nothing
  n <- arenaCount na
  forM [0 .. n - 1] $ \i -> (,) <$> getNodeRect na i <*> scrollOf i

-- | Run each frame on a context that keeps its layout cache and on one that
-- solves every frame from scratch, and check they lay out the same.
sameAsFullSolves :: IORef Int -> [(Input, NanoUI ())] -> IO ()
sameAsFullSolves failed frames = do
  reusing <- newContext
  fresh <- newContext
  forM_ frames $ \(inp, ui) -> do
    _ <- runFrame reusing inp ui
    writeIORef (ctxLayoutCache fresh) Nothing
    _ <- runFrame fresh inp ui
    expected <- layoutOf fresh
    assertEq failed expected =<< layoutOf reusing

-- | A ticking label beside rows that stay put, a label that gains and loses
-- a line so everything below it moves, and a window that resizes.
runPositionReuseCasesTest :: Context -> IORef Int -> IO ()
runPositionReuseCasesTest _ failed = do
  let inp = withInput 640 400
      rows = forM_ [1 .. 30 :: Int] $ \i -> row $ do
        label (T.pack ("row " <> show i))
        void (button "b")
        labelWith fillW (T.replicate (i `mod` 5) "wrapping words ")
      ticking f = column $ do
        label (T.pack ("tick " <> show f))
        rows
      shifting f = column $ do
        label (if even f then "one line" else "two\nlines")
        void (scroll rows)
  sameAsFullSolves failed [(inp, ticking f) | f <- [0 .. 5 :: Int]]
  sameAsFullSolves failed [(inp, shifting f) | f <- [0 .. 5 :: Int]]
  sameAsFullSolves failed [(withInput w 400, ticking (0 :: Int)) | w <- [640, 640, 500, 500, 640]]

-- | Randomised trees of every container kind, with texts that change at
-- different rates and a window that resizes now and then.
runPositionReuseRandomTest :: Context -> IORef Int -> IO ()
runPositionReuseRandomTest _ failed =
  forM_ [1 .. 16 :: Word64] $ \seed -> do
    let t = gen (mix seed) 0
        size f = if (f `div` 13) `mod` 2 == 0 then withInput 900 700 else withInput 640 700
    sameAsFullSolves failed [(size f, columnWith (fillW . fillH) (render f False t)) | f <- [0 .. 29]]

data Kind = KCol | KRow | KGrid Int | KScroll | KScroll2D | KLayers | KWrapRow | KWrapCol | KWindow

data Leaf = LLabel | LLabelFill | LLabelLines | LButton | LButtonAdorned | LSpacer | LSep | LCheck | LMeasured

data Tree = Node Kind Word64 [Tree] | Leaf Leaf Word64

mix :: Word64 -> Word64
mix z0 =
  let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
   in z2 `xor` (z2 `shiftR` 31)

hash2 :: Word64 -> Word64 -> Word64
hash2 a b = mix (a * 0x9e3779b97f4a7c15 + b)

pick :: Word64 -> Int -> Int
pick h n = fromIntegral (h `mod` fromIntegral n)

gen :: Word64 -> Int -> Tree
gen h depth
  | depth >= 4 || (depth > 1 && pick (hash2 h 1) 4 == 0) =
      Leaf ([LLabel, LLabelFill, LLabelLines, LButton, LButtonAdorned, LSpacer, LSep, LCheck, LMeasured] !! pick (hash2 h 6) 9) (hash2 h 7)
  | otherwise =
      let kind = case pick (hash2 h 2) 20 of
            0 -> KGrid (1 + pick (hash2 h 3) 4)
            1 -> KScroll
            2 -> KScroll2D
            3 -> KLayers
            4 -> KWrapRow
            5 -> KWrapCol
            6 | depth > 0 -> KWindow
            n | n < 13 -> KCol
            _ -> KRow
          kids = 1 + pick (hash2 h 4) (if depth == 0 then 8 else 5)
       in Node kind (hash2 h 5) [gen (hash2 h (100 + fromIntegral i)) (depth + 1) | i <- [1 .. kids]]

containerMods :: Word64 -> Bool -> Layout -> Layout
containerMods m inLayers =
  let f i = pick (hash2 m i)
      width = case f 1 10 of
        0 -> fillW
        1 -> \l -> l {layoutWidth = Percent (fromIntegral (30 + f 2 60))}
        2 -> \l -> l {layoutWidth = Fixed (fromIntegral (80 + f 3 300))}
        3 -> \l -> l {layoutWidth = Shrink 1}
        4 -> \l -> l {layoutWidth = Grow 2}
        _ -> id
      height = case f 4 10 of
        0 -> fillH
        1 -> \l -> l {layoutHeight = Fixed (fromIntegral (40 + f 5 200))}
        _ -> id
      spaced = if f 6 3 == 0 then gap (fromIntegral (f 7 12)) else id
      packed = if f 8 2 == 0 then tight else id
      lo = if f 9 6 == 0 then minW (fromIntegral (20 + f 10 100)) else id
      hi = if f 11 6 == 0 then maxW (fromIntegral (100 + f 12 300)) else id
      aligned = case f 13 8 of
        0 -> alignMid
        1 -> alignEnd
        2 -> alignBaseline
        _ -> id
      pinned = if not inLayers && f 14 12 == 0 then pinAt (fromIntegral (f 15 20)) (fromIntegral (f 16 20)) else id
      ratio = if f 17 25 == 0 then aspect 1.5 else id
      padded = if f 18 5 == 0 then padAll (fromIntegral (f 19 10)) else id
   in padded . ratio . pinned . aligned . hi . lo . packed . spaced . height . width

-- | Leaf @i@'s content version at frame @f@: it changes every frame, every
-- third, every seventh, or never.
version :: Word64 -> Int -> Word64
version i f = hash2 i (fromIntegral (f `div` ([1, 3, 7, 1000] !! pick (hash2 i 1) 4)))

words' :: Word64 -> Bool -> T.Text
words' v lines' =
  let n = 1 + pick v 12
      ws = [["alpha", "be", "gamma", "d", "epsilonic"] !! pick (hash2 v (fromIntegral j)) 5 | j <- [1 .. n]]
   in T.intercalate (if lines' && pick v 3 == 0 then "\n" else " ") ws

render :: Int -> Bool -> Tree -> NanoUI ()
render f inLayers = \case
  Node kind m kids -> do
    let mods = containerMods m inLayers
        body inner = mapM_ (render f inner) kids
    case kind of
      KCol -> columnWith mods (body False)
      KRow -> rowWith mods (body False)
      KGrid n -> gridWith n mods (body False)
      KScroll -> scrollWith mods (body False)
      KScroll2D -> scroll2DWith mods (body False)
      KLayers -> layersWith mods (body True)
      KWrapRow -> rowWith (wrap . mods) (body False)
      KWrapCol -> columnWith (wrap . mods) (body False)
      KWindow -> void (window True (T.pack ("W" <> show (m `mod` 1000))) (columnWith mods (body False)))
  Leaf kind i -> do
    let v = version i f
    case kind of
      LLabel -> label (words' v False)
      LLabelFill -> labelWith (fillW . maxW (fromIntegral (120 + pick v 200))) (words' v False)
      LLabelLines -> labelWith fillW (words' v True)
      LButton -> void (button (words' v False))
      LButtonAdorned -> void (buttonConfigured defaultButtonConfig {bcAdornments = leading (affix "$") <> trailing (affix (T.pack (show (pick v 1000))))} (words' v False))
      LSpacer -> spacer (Fixed (fromIntegral (5 + pick v 60))) (Fixed (fromIntegral (5 + pick (hash2 v 9) 30)))
      LSep -> separator
      LCheck -> void (checkbox (words' v False) (pick v 2 == 0))
      LMeasured ->
        void $ customWidget defaultCustomWidgetSpec
          { widgetMeasure = Just (\_ (w, _) -> (min w 90, fromIntegral (12 + pick v 20)))
          , widgetLayout = fillW defaultLayout
          }
