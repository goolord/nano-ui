{-# OPTIONS_GHC -O0 #-}
module Cases.Explain (tests) where

import Spec
import Data.List (findIndex)
import Data.Maybe (isJust, isNothing, listToMaybe)
import NanoUI.Internal.Context (Context (..), ExplainState (..), setDrawSquareGeometry)
import NanoUI.Internal.Layout.Arena (arenaCount, getNodeRect, getNodeType, getParent, isFloatingNode)

tests :: [Spec]
tests =
  [ spec "explain-outlines" runExplainOutlinesTest
  , spec "explain-damage" runExplainDamageTest
  , spec "explain-hover" runExplainHoverTest
  , spec "explain-window-layer" runExplainWindowLayerTest
  , spec "explain-layers-and-pin" runExplainLayersAndPinTest
  , spec "explain-scope" runExplainScopeTest
  ]

inp :: Input
inp = withInputOff 480 360

-- | Every node's rect and depth in its layer, from 0 at the page root or a floating panel.
layerDepths :: Context -> IO [(Rect, Int)]
layerDepths ctx = do
  let na = ctxNodeArena ctx
      depthOf idx = do
        nt <- getNodeType na idx
        p <- getParent na idx
        if isFloatingNode nt || p < 0 then pure 0 else (+ 1) <$> depthOf p
  n <- arenaCount na
  forM [0 .. n - 1] $ \i -> (,) <$> getNodeRect na i <*> depthOf i

-- | The overlay's colour at a depth.
depthColor :: Theme -> Int -> Color
depthColor theme depth = themeSeries theme !! (depth `rem` length (themeSeries theme))

-- | The top edge of a node's outline at a depth, in that depth's colour.
edge :: Theme -> Int -> Rect -> (Rect, Color)
edge theme d (Rect x y w _) = (Rect x y w 1, depthColor theme d)

-- | Quad equality, tolerating float rounding in the decoded vertices.
sameQuad :: (Rect, Color) -> (Rect, Color) -> Bool
sameQuad (Rect x y w h, c) (Rect x' y' w' h', c') =
  c == c' && all ((< 0.01) . abs) [x - x', y - y', w - w', h - h']

-- | Draw-order index of the first quad matching this one.
drawnIndex :: [(Rect, Color)] -> (Rect, Color) -> Maybe Int
drawnIndex quads q = findIndex (sameQuad q) quads

drawnAt :: [(Rect, Color)] -> (Rect, Color) -> Bool
drawnAt quads = isJust . drawnIndex quads

-- | Draw-order index of the first floating-window background quad.
windowFill :: Theme -> [(Rect, Color)] -> Maybe Int
windowFill theme = findIndex ((== styleBg (themeFloatingWindow theme)) . snd)

-- | Draw plain quads with the overlay on, and return the theme.
outlining :: Context -> IO Theme
outlining ctx = setDrawSquareGeometry ctx True >> setExplainLayout ctx True >> getTheme ctx

-- | Run two frames; return the second frame's result and quads.
quadsOf :: Context -> Input -> NanoUI a -> IO (a, [(Rect, Color)])
quadsOf ctx i ui = warmupDraw ctx i ui >>= traverse drawQuads

damageOf :: Context -> Input -> NanoUI a -> IO Damage
damageOf ctx i ui = runFrame ctx i ui >> takeDamage ctx

-- | The node explained after a frame with the pointer at @p@.
explainedAt :: Context -> NanoUI a -> V2 -> IO (Maybe ExplainedNode)
explainedAt ctx view p = runFrame ctx inp {inputMousePos = p} view >> getExplainedNode ctx

-- | The overlay outlines every node in its depth's colour, clipped like the node,
-- without changing layout or hit testing.
runExplainOutlinesTest :: Context -> IORef Int -> IO ()
runExplainOutlinesTest ctx failed = do
  setDrawSquareGeometry ctx True
  theme <- getTheme ctx
  let ui = columnWith (padAll 8 . gap 4) (label "top" >> rowWith (padAll 4 . gap 4) (button' "a" <* label "b"))
  (_, quadsOff) <- quadsOf ctx inp ui
  rectsOff <- arenaRects ctx
  setExplainLayout ctx True
  (a, quadsOn) <- quadsOf ctx inp ui
  arenaRects ctx >>= assertEq failed rectsOff
  edges <- map (\(r, d) -> edge theme d r) . filter (\(Rect _ _ w h, _) -> w > 0 && h > 0) <$> layerDepths ctx
  -- The root, the label, the row, the button and its label.
  assert failed (length edges >= 5)
  assert failed (all (drawnAt quadsOn) edges && not (any (drawnAt quadsOff) edges))
  runClick ctx inp ui (centerOf a) >>= assert failed . respClicked
  setExplainLayout ctx False
  (_, quadsOffAgain) <- quadsOf ctx inp ui
  assert failed (not (any (drawnAt quadsOffAgain) edges))
  setExplainLayout ctx True
  (_, quads) <- quadsOf ctx inp . columnWith (padAll 10) $
    scrollArea (fixedWH 120 50) (column (replicateM_ 10 (labelWith (fixedWH 80 20) "row")))
  nodes <- layerDepths ctx
  -- The rows, and only the rows, are at the deepest level.
  let rowDepth = maximum (map snd nodes)
      rows = [r | (r, d) <- nodes, d == rowDepth]
      rowEdge = edge theme rowDepth
  assertEq failed (length rows) 10
  assertJust failed (listToMaybe [r | (r@(Rect _ _ 120 50), _) <- nodes]) $ \scroller@(Rect _ sy _ sh) -> do
    let below = [r | r@(Rect _ y _ _) <- rows, y >= sy + sh]
    assert failed (length below >= 5 && not (any (drawnAt quads . rowEdge) below))
    assert failed (any (drawnAt quads . rowEdge) (take 1 rows))
    assert failed (all (covers scroller) [r | (r, c) <- quads, c == depthColor theme rowDepth])

-- | Toggling the overlay repaints everything, setting it again nothing; a column
-- without an id (no rect diff) repaints its old and new outline when it moves.
runExplainDamageTest :: Context -> IORef Int -> IO ()
runExplainDamageTest ctx failed = do
  writeIORef (ctxPaintFull ctx) False
  let ui = column (label "text")
      idle = damageOf ctx inp ui >>= assert failed . damageIsEmpty
  warmup2 ctx inp ui >> idle
  setExplainLayout ctx True
  needsRedraw ctx inp inp >>= assert failed
  damageOf ctx inp ui >>= assertEq failed DamageFull
  idle
  setExplainLayout ctx True
  needsRedraw ctx inp inp >>= assert failed . not
  idle
  damageOf ctx inp (explainLayout False >> ui) >>= assertEq failed DamageFull
  getExplainLayout ctx >>= assert failed . not
  damageOf ctx inp (explainLayout False >> ui) >>= assert failed . damageIsEmpty
  evalUi ctx inp (explainLayout True >> explainingLayout <* ui) >>= assert failed
  takeDamage ctx >>= assertEq failed DamageFull
  widthRef <- newIORef 40
  let moving = columnWith (padAll 10) $
        liftIO (readIORef widthRef) >>= \w -> columnWith (fixedWH w 30) (pure ())
      sized w = filter (\(Rect _ _ w' h) -> w' == w && h == 30) <$> arenaRects ctx
  warmup2 ctx inp moving
  damageOf ctx inp moving >>= assert failed . damageIsEmpty
  before <- sized 40
  writeIORef widthRef 90
  dmg <- damageOf ctx inp moving
  after <- sized 90
  assertEq failed (1, 1) (length before, length after)
  assert failed (all (clipCovers dmg) (before ++ after))

-- | The node under the pointer is tinted and reported; only moving to another repaints.
runExplainHoverTest :: Context -> IORef Int -> IO ()
runExplainHoverTest ctx failed = do
  setDrawSquareGeometry ctx True
  writeIORef (ctxPaintFull ctx) False
  theme <- getTheme ctx
  let box80 = labelWith' (fixedWH 80 20)
      ui = columnWith (padAll 10 . gap 10) ((,) <$> box80 "a" <*> box80 "b")
  (a, b) <- warmup2 ctx inp ui
  let overA = inp {inputMousePos = centerOf a}
      nudged = inp {inputMousePos = v2Add (centerOf a) (V2 2 1)}
      overB = inp {inputMousePos = centerOf b}
      tint = depthColor theme 1
      Rect ax ay aw _ = respRect a
  -- Labels ignore hover, so with the overlay off moving between them needs no frame.
  needsRedraw ctx overA overB >>= assert failed . not
  setExplainLayout ctx True
  _ <- warmup2 ctx inp ui
  getExplainedNode ctx >>= assert failed . isNothing
  needsRedraw ctx inp overA >>= assert failed
  (_, draw, again) <- runFrame ctx overA ui
  quads <- drawQuads draw
  assert failed (again && drawnAt quads (respRect a, colorRGBA (colorR tint) (colorG tint) (colorB tint) 0x40))
  assertJustM failed (getExplainedNode ctx) $ \node -> do
    assertEq failed (respRect a, "Text", 1) (explainedRect node, explainedKind node, explainedDepth node)
    -- The content box (inside the label's padding) is outlined over the tint.
    let Padding l r t _ = explainedPadding node
    assert failed (l > 0 && t > 0 && drawnAt quads (Rect (ax + l) (ay + t) (aw - l - r) 1, tint))
  evalUi ctx overA (explainedNode <* ui) >>= assertEq failed (Just (respRect a)) . fmap explainedRect
  takeDamage ctx >>= assert failed . damageIsEmpty
  forM_ [(overA, overA, False), (overA, nudged, True), (nudged, nudged, False), (nudged, overB, True)] $
    \(from, to, want) -> needsRedraw ctx from to >>= assertEq failed want
  (_, _, againNudged) <- runFrame ctx nudged ui
  takeDamage ctx >>= \dmg -> assert failed (not againNudged && damageIsEmpty dmg)
  (_, _, againB) <- runFrame ctx overB ui
  dmg <- takeDamage ctx
  assert failed (againB && clipCovers dmg (respRect a) && clipCovers dmg (respRect b))
  assertJustM failed (getExplainedNode ctx) $ assertEq failed (respRect b) . explainedRect

-- | A window's outlines draw over it, after the page's, with depths from its root.
runExplainWindowLayerTest :: Context -> IORef Int -> IO ()
runExplainWindowLayerTest ctx failed = do
  theme <- outlining ctx
  let win = window' (constVar True) "Win" (labelWith' (fixedWH 60 20) "inside")
      ui = label "page" >> snd <$> win
  assertJustM failed (warmup2 ctx inp ui) $ \inner -> do
    (_, quads) <- quadsOf ctx inp {inputMousePos = centerOf inner} ui
    nodes <- layerDepths ctx
    let pageRoot = listToMaybe nodes >>= drawnIndex quads . edge theme 0 . fst
    assertJust failed (lookup (respRect inner) nodes) $ \depth -> do
      assert failed (depth > 0)
      -- The page's outlines, then the window over them, then its outlines.
      let innerEdge = drawnIndex quads (edge theme depth (respRect inner))
      assertJust failed ((,,) <$> pageRoot <*> windowFill theme quads <*> innerEdge) $ \(p, w, i) ->
        assert failed (p < w && w < i)
      assertJustM failed (getExplainedNode ctx) $ \node ->
        assertEq failed (respRect inner, depth) (explainedRect node, explainedDepth node)
  -- With no page, node 0 is the window root, outlined once over its fill.
  (_, quads) <- quadsOf ctx inp win
  assertJustM failed (listToMaybe <$> layerDepths ctx) $ \(r, depth) -> do
    let e = edge theme depth r
    assertEq failed (0, 1) (depth, length (filter (sameQuad e) quads))
    assertJust failed ((,) <$> windowFill theme quads <*> drawnIndex quads e) $ \(f, i) -> assert failed (f < i)

-- | Pinned nodes are outlined, the topmost node is explained, containers are named,
-- and the explained node reports its layout request.
runExplainLayersAndPinTest :: Context -> IORef Int -> IO ()
runExplainLayersAndPinTest ctx failed = do
  theme <- outlining ctx
  let area l = drawing l (const mempty)
      ui = columnWith (tight . gap 0) $
        (++) <$> sequence [area (pinAt 20 20 . fixedWH 60 40), area (fixedWH 200 100)]
          <*> layersWith tight (sequence [area (fixedWH 200 100), area (fixedWH 100 50)])
      farCorner r = let Rect x y w h = respRect r in V2 (x + w - 5) (y + h - 5)
  (rs, quads) <- quadsOf ctx inp ui
  assert failed (any (drawnAt quads . edge theme 1 . respRect) (take 1 rs))
  -- The pinned area, the area it covers, and the upper and lower layers.
  forM_ (zip rs [centerOf, farCorner, farCorner, centerOf]) $ \(r, at) ->
    explainedAt ctx ui (at r) >>= assertEq failed (Just (respRect r)) . fmap explainedRect
  -- Pointing into a container's padding explains the container.
  let square = void (area (fixedWH 10 10))
      kinds = columnWith (tight . gap 0) $ do
        layersWith (padAll 20 . fixedWH 200 100) square
        rowWith (padAll 20 . fixedWH 200 100 . wrap) (square >> square)
        columnWith (padAll 20 . fixedWH 200 100) square
  warmup2 ctx inp kinds
  forM_ [(80, "Container, layered"), (180, "Container, row, wrap"), (280, "Container, column")] $ \(y, kind) ->
    explainedAt ctx kinds (V2 150 y) >>= assertEq failed (Just kind) . fmap explainedKind
  let fields = columnWith (tight . gap 0) $ do
        chips <- rowWith (wrap . gap 6 . fixedW 300 . minH 40) $ label' "chip"
        pinned <- buttonWith' (pinAt 10 50 . pointer PointerBlock . fillW . maxW 120 . fixedH 30) "pin"
        pure (chips, pinned)
  (chip, pinned) <- warmup2 ctx inp fields
  assertJustM failed (explainedAt ctx fields (centerOf pinned)) $ \n -> do
    assertEq failed (Just (respId pinned), Grow 1, Fixed 30, V2 0 0) $
      (explainedWidget n, explainedWidth n, explainedHeight n, explainedMin n)
    assertEq failed (V2 120 1e9, Just (V2 10 50), PointerBlock, Line) $
      (explainedMax n, explainedPin n, explainedPointer n, explainedFlow n)
  -- The wrapping row, hit in its padding beside the chip.
  let Rect cx cy _ ch = respRect chip
  assertJustM failed (explainedAt ctx fields (V2 (cx - 1) (cy + ch / 2))) $ \n -> do
    assertEq failed (Nothing, Fixed 300, Fit, V2 300 40, 6) $
      (explainedWidget n, explainedWidth n, explainedHeight n, explainedMin n, explainedGap n)
    assertEq failed (Row, Wrap, Nothing, PointerAuto) $
      (explainedDirection n, explainedFlow n, explainedPin n, explainedPointer n)

-- | 'explainScope' limits outlining and explaining to its body; off, it records nothing.

runExplainScopeTest :: Context -> IORef Int -> IO ()
runExplainScopeTest ctx failed = do
  theme <- outlining ctx
  let ui scoped = columnWith (padAll 8 . gap 8) $ do
        outside <- labelWith' (fixedWH 80 20) "outside"
        (a, b) <- (if scoped then explainScope else id) $
          rowWith (padAll 4) ((,) <$> labelWith' (fixedWH 60 20) "a" <*> labelWith' (fixedWH 60 20) "b")
        pure (outside, a, b)
      outlined quads r = maybe False (\d -> drawnAt quads (edge theme d r)) . lookup r <$> layerDepths ctx
  ((outside, a, b), quads) <- quadsOf ctx inp (ui True)
  mapM (outlined quads . respRect) [a, b] >>= assertEq failed [True, True]
  outlined quads (respRect outside) >>= assert failed . not
  -- The root column is not outlined either.
  assertJustM failed (listToMaybe <$> arenaRects ctx) $ \root -> outlined quads root >>= assert failed . not
  getExplainedNode ctx >>= assert failed . isNothing
  explainedAt ctx (ui True) (centerOf outside) >>= assert failed . isNothing
  explainedAt ctx (ui True) (centerOf a) >>= assertEq failed (Just (respId a)) . (>>= explainedWidget)
  (_, quadsAll) <- quadsOf ctx inp (ui False)
  outlined quadsAll (respRect outside) >>= assert failed
  setExplainLayout ctx False
  _ <- runFrame ctx inp (ui True)
  esScopes <$> readIORef (ctxExplain ctx) >>= assertEq failed []
