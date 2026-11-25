{-# OPTIONS_GHC -O0 #-}
-- | Properties of the layout solve over random container trees: children
-- stay inside their parents, siblings in a line do not overlap, grow
-- children share the leftover space by factor, and every edge lands on the
-- device pixel grid.
module Cases.LayoutProps (tests) where

import Spec
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena
  ( AxisSizing (..)
  , SizingTag (..)
  , StyleCol (..)
  , TreeCol (..)
  , arenaArrays
  , arenaCount
  , getDirection
  , getFirstChild
  , getFlow
  , getHeightSizing
  , getNextSibling
  , getNodeRect
  , getNodeType
  , getPadding
  , getParent
  , getWidthSizing
  , isFloatingNode
  , isScrollNode
  , readStyle
  , readTree
  )
import NanoUI.Testing.Sim qualified as Sim
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, counterexample, elements, frequency, ioProperty, sized, vectorOf)

tests :: [Spec]
tests =
  [ propSpec "layout-props" 7 400 layoutProperties
  , sceneSpec "wrap-row-height" wrapRowScene
  , sceneSpec "grid-cell-height" gridCellScene
  , sceneSpec "fill-label-height" fillLabelScene
  , sceneSpec "wrap-percent-height" wrapPercentScene
  , sceneSpec "wrap-row-in-row-alignment" wrapInRowScene
  ]

-- Scenes whose heights depend on width, measured at one width and laid out
-- at another. Each broke a property before the solve refit heights at the
-- width a node is laid out at.
--
-- A fit column in a 51 px row measures its wrapped row at 51 px (two lines,
-- 72 px tall) but lays it out at the 138 px its content asks for (one line,
-- 42 px of content).
--
-- A 72 px column holds a two-column grid with a filling label, which lays
-- out a line taller in its 25 px cell than the column measured it.
--
-- A fit column in a 148 px wrapped row holds a filling label in a grow
-- column, which lays out fewer lines at the column's width than it measured
-- at the row's.
--
-- A 235 px wrapped row holds a 44 % column, a checkbox and a button: the
-- percentage of the line breaks it into two lines.
--
-- A grow wrapped row in a row is 40 px wide, where its filling label takes
-- four lines, and is centred in the row at that height.

-- | Check one scene at snap scale 1, printing what it breaks.
sceneSpec :: String -> Scene -> Spec
sceneSpec name scene = spec name $ \_ failed -> do
  problems <- violations 1 <$> sceneNodes 1 scene
  unless (null problems) (mapM_ putStrLn problems >> modifyIORef' failed (+ 1))

wrapInRowScene :: Scene
wrapInRowScene =
  Scene 900 700 $
    Box KRow (Mods SFit SFit 10 3 2)
      [ Box KCol (Mods SFit SFit 0 3 2) [Leaf LCheck, Leaf LCheck, Box KRow (Mods SFit SFit 4 0 1) [Leaf LSep]]
      , Box KRow (Mods SFit SFill 4 0 0)
          [ Box KRow (Mods (SPercent 87) SFit 4 0 3) []
          , Box KWrap (Mods (SGrow 3) SFit 0 8 1) [Leaf (LGrow 2), Leaf (LGrow 4), Leaf (LLabelFill 27)]
          ]
      ]

gridCellScene :: Scene
gridCellScene =
  Scene 1600 1200 $
    Box KCol (Mods (SGrow 2) (SGrow 3) 4 8 0)
      [ Box KWrap (Mods (SFixed 293) SFill 0 8 1) []
      , Box KCol (Mods (SFixed 72) SFit 10 8 2) [Box (KGrid 2) (Mods SFit SFit 0 3 1) [Leaf (LLabelFill 91)]]
      ]

fillLabelScene :: Scene
fillLabelScene =
  Scene 640 480 $
    Box KWrap (Mods (SFixed 148) SFill 0 3 0)
      [ Box KCol (Mods SFit SFill 4 8 1)
          [ Box KCol (Mods (SGrow 3) SFit 0 3 3) [Leaf (LLabelFill 17)]
          , Leaf (LGrow 3)
          , Box KRow (Mods SFit SFit 0 0 2) [Leaf (LLabel 29)]
          ]
      ]

wrapPercentScene :: Scene
wrapPercentScene =
  Scene 900 700 $
    Box KCol (Mods SFit SFill 4 0 1)
      [ Leaf (LGrow 2)
      , Box KWrap (Mods (SFixed 235) SFit 0 8 0) [Box KCol (Mods (SPercent 44) SFit 0 3 2) [], Leaf LCheck, Leaf (LButton 36)]
      ]

wrapRowScene :: Scene
wrapRowScene =
  Scene 1600 1200 $
    Box KRow (Mods (SFixed 51) SFit 0 0 0)
      [ Box KCol (Mods SFit SFill 4 3 0)
          [ Box KWrap (Mods SFill SFit 10 8 3) [Leaf (LSpacer 7 20), Leaf (LMeasured 94 26)]
          , Box KRow (Mods SFit (SGrow 1) 4 0 0) [Leaf (LButton 4)]
          ]
      ]

-- | A random container tree. Leaves are labels, buttons, spacers, checkboxes,
-- separators and measured drawings, all small enough that a tree fits a
-- large window, so most containers have room for their children.
data Box = Box Kind Mods [Box] | Leaf Leaf
  deriving (Show)

data Kind = KRow | KCol | KGrid Int | KWrap | KScroll
  deriving (Show)

data Leaf
  = LLabel Int
  | LLabelFill Int
  | LButton Int
  | LSpacer Int Int
  | -- | A spacer that grows by this factor along its parent's main axis.
    LGrow Int
  | LCheck
  | LMeasured Int Int
  | LSep
  deriving (Show)

data Sz = SFit | SFill | SGrow Int | SPercent Int | SFixed Int
  deriving (Show)

data Mods = Mods {mW :: Sz, mH :: Sz, mGap :: Int, mPad :: Int, mAlign :: Int}
  deriving (Show)

-- | A tree and the window it lays out in.
data Scene = Scene Float Float Box
  deriving (Show)

instance Arbitrary Scene where
  arbitrary = do
    (w, h) <- elements [(1600, 1200), (900, 700), (640, 480)]
    Scene w h <$> sized (\n -> genBox (min 3 (1 + n `div` 30)))
  shrink (Scene w h b) = [Scene w h b' | b' <- shrinkBox b]

shrinkBox :: Box -> [Box]
shrinkBox = \case
  Leaf _ -> []
  Box k m kids ->
    kids
      <> [Box k m (take i kids <> drop (i + 1) kids) | i <- [0 .. length kids - 1]]
      <> [Box k m (take i kids <> [kid'] <> drop (i + 1) kids) | (i, kid) <- zip [0 ..] kids, kid' <- shrinkBox kid]
      <> [Box k (m {mH = SFit}) kids | not (isFit (mH m))]
      <> [Box k (m {mW = SFit}) kids | not (isFit (mW m))]
  where
    isFit SFit = True
    isFit _ = False

genBox :: Int -> Gen Box
genBox depth
  | depth <= 0 = Leaf <$> genLeaf
  | otherwise =
      frequency
        [ (1, Leaf <$> genLeaf)
        , (3, genKind >>= \k -> Box k <$> genMods <*> (choose (1, 5) >>= (`vectorOf` genChild k)))
        ]

  where
    genChild = \case
      KGrid _ -> Leaf <$> genLeaf
      KWrap -> Leaf <$> genLeaf
      _ -> genBox (depth - 1)

genKind :: Gen Kind
genKind = frequency [(4, pure KRow), (4, pure KCol), (1, KGrid <$> choose (1, 4)), (1, pure KWrap), (1, pure KScroll)]

genLeaf :: Gen Leaf
genLeaf =
  frequency
    [ (3, LLabel <$> choose (0, 99))
    , (1, LLabelFill <$> choose (0, 99))
    , (2, LButton <$> choose (0, 99))
    , (2, LSpacer <$> choose (0, 60) <*> choose (0, 40))
    , (3, LGrow <$> choose (1, 4))
    , (1, pure LCheck)
    , (1, LMeasured <$> choose (10, 120) <*> choose (8, 40))
    , (1, pure LSep)
    ]

-- | A container's sizes and spacing.
genMods :: Gen Mods
genMods =
  Mods
    <$> genWidth
    <*> frequency [(5, pure SFit), (1, pure SFill), (1, SGrow <$> choose (1, 3)), (1, SFixed <$> choose (30, 300))]
    <*> elements [0, 0, 4, 10]
    <*> elements [0, 0, 3, 8]
    <*> choose (0, 7)
  where
    genWidth = frequency [(4, pure SFit), (2, pure SFill), (2, SGrow <$> choose (1, 3)), (1, SPercent <$> choose (20, 100)), (1, SFixed <$> choose (40, 400))]

-- | Short words, so a label is one line unless its width is a share.
textOf :: Int -> T.Text
textOf i = T.unwords (take (1 + i `mod` 3) (drop (i `mod` 5) (cycle ["alpha", "be", "gamma", "d", "eps"])))

data Axis = AxisRow | AxisCol | AxisNone

renderBox :: Axis -> Box -> NanoUI ()
renderBox axis = \case
  Box kind m kids -> do
    let lay = mods m
        inner a = mapM_ (renderBox a) kids
    case kind of
      KRow -> rowWith lay (inner AxisRow)
      KCol -> columnWith lay (inner AxisCol)
      KGrid n -> gridWith n lay (inner AxisNone)
      KWrap -> rowWith (wrap . lay) (inner AxisNone)
      KScroll -> scrollWith lay (inner AxisCol)
  Leaf leaf -> case leaf of
    LLabel i -> label (textOf i)
    LLabelFill i -> labelWith fillW (textOf i)
    LButton i -> void (button (textOf i))
    LSpacer w h -> spacer (Fixed (fromIntegral w)) (Fixed (fromIntegral h))
    LGrow f -> case axis of
      AxisRow -> spacer (Grow (fromIntegral f)) (Fixed 10)
      AxisCol -> spacer (Fixed 10) (Grow (fromIntegral f))
      AxisNone -> spacer (Fixed 10) (Fixed 10)
    LCheck -> void (withValue False (checkbox "check"))
    LMeasured w h ->
      void $ customWidget defaultCustomWidgetSpec
        { widgetMeasure = Just (\_ (avail, _) -> (min avail (fromIntegral w), fromIntegral h))
        , widgetLayout = fillW defaultLayout
        }
    LSep -> separator
  where
    mods m =
      align (mAlign m) . gap (fromIntegral (mGap m)) . padAll (fromIntegral (mPad m)) . width (mW m) . height (mH m)
    width = \case
      SFit -> id
      SFill -> fillW
      SGrow f -> \l -> l {layoutWidth = Grow (fromIntegral f)}
      SPercent p -> \l -> l {layoutWidth = Percent (fromIntegral p)}
      SFixed px -> \l -> l {layoutWidth = Fixed (fromIntegral px)}
    height = \case
      SFit -> id
      SFill -> fillH
      SGrow f -> \l -> l {layoutHeight = Grow (fromIntegral f)}
      SPercent _ -> id
      SFixed px -> \l -> l {layoutHeight = Fixed (fromIntegral px)}
    align = \case
      1 -> alignMid
      2 -> alignEnd
      4 -> itemsCenter
      5 -> itemsEnd
      6 -> itemsBaseline
      7 -> itemsCenter . alignMid
      _ -> id

-- | What the checks read of one node.
data Node = Node
  { nType :: NodeType
  , nRect :: Rect
  , nKids :: [Int]
  -- ^ Children in declaration order.
  , nRow :: Bool
  , nLine :: Bool
  -- ^ A single row or column: not wrapped, layered or a grid.
  , nPad :: Padding
  , nGap :: Float
  , nCols :: Int
  -- ^ A grid's column count, else 0.
  , nMain :: AxisSizing
  -- ^ The sizing along the parent's main axis.
  , nFloating :: Bool
  -- ^ Floating, or inside a floating node.
  }

readNodes :: Context -> IO [(Int, Node)]
readNodes ctx = do
  let na = ctxNodeArena ctx
  a <- arenaArrays na
  n <- arenaCount na
  let children i = getFirstChild na i >>= collect []
      collect acc c = if c < 0 then pure acc else getNextSibling na c >>= collect (c : acc)
  nodes <- forM [0 .. n - 1] $ \i -> do
    nt <- getNodeType na i
    parent <- getParent na i
    rowParent <- if parent < 0 then pure True else (== DirRow) <$> getDirection na parent
    dir <- getDirection na i
    flow <- getFlow na i
    cols <- readTree a i TreeGridCols
    node <-
      Node nt
        <$> getNodeRect na i
        <*> children i
        <*> pure (dir == DirRow)
        <*> pure (flow == Line && cols <= 0 && nt == NodeContainer)
        <*> getPadding na i
        <*> readStyle a i StyleGap
        <*> pure (max 0 cols)
        <*> (if rowParent then getWidthSizing na i else getHeightSizing na i)
        <*> pure (isFloatingNode nt)
    pure (i, node, parent)
  -- A parent precedes its children, so one pass marks floating subtrees.
  let marked = foldl' mark [] nodes
      mark acc (i, node, parent) =
        let inherited = maybe False nFloating (lookup parent acc)
         in acc <> [(i, node {nFloating = nFloating node || inherited})]
  pure marked

-- | The broken properties of the solved arena, described.
violations :: Float -> [(Int, Node)] -> [String]
violations scale nodes = concatMap check nodes
  where
    eps = 1e-3
    get i = lookup i nodes
    check (i, p)
      | nFloating p || isScrollNode (nType p) = []
      | otherwise =
          let kids = [(c, k) | c <- nKids p, Just k <- [get c], not (nFloating k)]
           in snapped i p <> contained i p kids <> lined i p kids <> grown i p kids
    -- Every edge times the scale is a whole device pixel.
    snapped i p =
      let Rect x y w h = nRect p
          off v = abs (v * scale - fromIntegral (round (v * scale) :: Int)) > eps
       in ["node " <> show i <> " off the grid at scale " <> show scale <> ": " <> show (nRect p) | scale > 0, any off [x, y, x + w, y + h]]
    -- Children lie inside the parent along each axis on which they fit: the
    -- children's sizes along a line plus its gaps, or else their extent,
    -- plus padding, are at most the parent's size. A grid child also has
    -- to fit its column: one wider overhangs its cell as its alignment
    -- says, which may cross the grid's edge.
    contained i p kids
      | null kids = []
      | otherwise =
          let Rect px py pw ph = nRect p
              Padding pl pr pt pb = nPad p
              rects = map (nRect . snd) kids
              gaps = nGap p * fromIntegral (length kids - 1)
              extent lo hi = maximum (map hi rects) - minimum (map lo rects)
              needW
                | nLine p && nRow p = sum (map rectW rects) + gaps
                | nLine p = maximum (map rectW rects)
                | otherwise = extent rectX (\r -> rectX r + rectW r)
              needH
                | nLine p && not (nRow p) = sum (map rectH rects) + gaps
                | nLine p = maximum (map rectH rects)
                | otherwise = extent rectY (\r -> rectY r + rectH r)
              fitsW = needW + pl + pr <= pw + eps
              fitsH = needH + pt + pb <= ph + eps
              colW = (pw - pl - pr - nGap p * fromIntegral (nCols p - 1)) / fromIntegral (nCols p)
              inCell (Rect _ _ w _) = nCols p <= 0 || w <= colW + eps
              outW r@(Rect x _ w _) = inCell r && (x < px - eps || x + w > px + pw + eps)
              outH (Rect _ y _ h) = y < py - eps || y + h > py + ph + eps
           in [ "child " <> show c <> " " <> show (nRect k) <> " outside parent " <> show i <> " " <> show (nRect p)
              | (c, k) <- kids
              , (fitsW && outW (nRect k)) || (fitsH && outH (nRect k))
              ]
    -- In a row or column that fits, each child starts at or after the end
    -- of the one declared before it.
    lined i p kids
      | not (nLine p) = []
      | otherwise =
          let lo r = if nRow p then rectX r else rectY r
              len r = if nRow p then rectW r else rectH r
              rects = map (nRect . snd) kids
              Padding pl pr pt pb = nPad p
              room = (if nRow p then rectW (nRect p) - pl - pr else rectH (nRect p) - pt - pb) - nGap p * fromIntegral (length kids - 1)
           in [ "siblings overlap in " <> show i <> ": " <> show a <> " then " <> show b
              | sum (map len rects) <= room + eps
              , (a, b) <- zip rects (drop 1 rects)
              , lo b < lo a + len a - eps
              ]
    -- Grow children without a maximum take the leftover space by factor,
    -- none below its content size. A grow spacer's content is 8 px, so a
    -- spacer longer than that takes exactly its factor's share, no more per
    -- unit of factor than any other grow child; a spacer near 8 px had a
    -- share no larger. Each snapped length is within a pixel of the exact
    -- one, so two shares per unit of factor differ by at most @1 / f + 1 /
    -- g@. With room to spare, the grow children fill it, to within a pixel
    -- per child and one for the parent: padding need not be whole device
    -- pixels, so each edge rounds on its own.
    grown i p kids
      | not (nLine p) = []
      | otherwise =
          let len r = if nRow p then rectW r else rectH r
              isGrow k = axTag (nMain k) == SizingGrow && axVal (nMain k) > 0
              growing = [(k, axVal (nMain k)) | (_, k) <- kids, isGrow k]
              others = [len (nRect k) | (_, k) <- kids, not (isGrow k)]
              Padding pl pr pt pb = nPad p
              inner = if nRow p then rectW (nRect p) - pl - pr else rectH (nRect p) - pt - pb
              leftover = inner - sum others - nGap p * fromIntegral (length kids - 1)
              perUnit (k, f) = len (nRect k) / f
              spacers = [a | a@(k, _) <- growing, nType k == NodeSpacer]
              unlocked = [a | a@(k, _) <- spacers, len (nRect k) > spacerContent + tol]
              locked = [a | a@(k, _) <- spacers, len (nRect k) <= spacerContent + tol]
              tol = 1 + eps
              share a b = "grow spacer in " <> show i <> " has " <> show (perUnit a) <> " per unit of factor, another grow child " <> show (perUnit b)
           in if null growing || any ((< 1e8) . axMax . nMain . fst) growing || leftover <= tol
                then []
                else
                  [share a b | a@(_, f) <- unlocked, b@(_, g) <- growing, perUnit a > perUnit b + 1 / f + 1 / g + eps]
                    <> [share a b | a@(_, f) <- unlocked, b@(_, g) <- unlocked, perUnit a < perUnit b - 1 / f - 1 / g - eps]
                    <> [share a b | a@(k, f) <- locked, b@(_, g) <- unlocked, f * perUnit b > len (nRect k) + tol + f / g]
                    <> [ "grow children of " <> show i <> " take " <> show taken <> " of leftover " <> show leftover
                       | let taken = sum [len (nRect k) | (k, _) <- growing]
                       , abs (taken - leftover) > tol * fromIntegral (length kids + 1)
                       ]
    spacerContent = 8

-- | The scene's nodes after it settles at snap scale @s@.
sceneNodes :: Float -> Scene -> IO [(Int, Node)]
sceneNodes s (Scene w h root) = do
  ctx <- (`withFontMetrics` (monospaceMetrics 12) {fmSnapScale = s}) <$> newContext
  quiet <- newIORef 0
  Sim.runSim ctx quiet (withInput w h) (columnWith (fillW . fillH) (renderBox AxisCol root)) (pure ())
  readNodes ctx

-- | Lay a scene out at each snap scale and check every property.
layoutProperties :: Scene -> Property
layoutProperties scene = ioProperty $ do
  found <- forM [1, 1.25, 1.5, 1.75] $ \s -> violations s <$> sceneNodes s scene
  let problems = concat found
  pure (counterexample (unlines (take 12 problems)) (null problems))
