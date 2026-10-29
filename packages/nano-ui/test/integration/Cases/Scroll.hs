module Cases.Scroll (tests) where

import Spec
import Data.List (sort)
import Data.Maybe (isJust)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peekElemOff)
import Data.Text qualified as T
import NanoUI.Internal.Context (InteractionState (isScrollDrag), ctxNodeArena, getsInteraction, setDrawSnapScale)
import NanoUI.Internal.Layout.Arena
  ( findNodeM
  , getClipBounds
  , getClipRect
  , getNodeValue
  , getNodeType
  , getNodeRect
  , getScrollContentW
  , getWidgetId
  , lookupNodeByWidgetId
  )
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ spec "scroll-thumb" runScrollThumbTest
  , pixelSpec "scroll-bar-gutter" runScrollBarGutterTest
  , spec "scroll-bar-short-track" runScrollBarShortTrackTest
  , spec "scroll-damage" runScrollDamageTest
  , spec "page-backdrop" runPageBackdropTest
  , spec "scroll-tall-label-cull" runTallLabelCullTest
  , spec "nested-scroll" runNestedScrollTest
  , spec "scroll-hit" runScrollHitTest
  , spec "scroll-disjoint-viewport" runDisjointViewportTest
  , spec "scroll-lockstep-probe" runScrollLockstepProbeTest
  , spec "scroll-2d-layout" runScroll2DLayoutTest
  , pixelSpec "scroll-wheel-tuning" runScrollTuningTest
  , pixelSpec "scroll-commands" runScrollCommandsTest
  , spec "scroll-wheel-routing" runWheelRoutingTest
  ]

at :: Float -> Float -> Target
at x y = At (V2 x y)

-- | The cursor with the pointer moved onto the target.
cursorAt :: Target -> Sim a UiCursorKind
cursorAt t = do
  _ <- moveTo t
  ctx <- simContext
  liftIO . uiCursorKind ctx =<< getInput

-- | The widget's rect from the last frame.
widgetRect :: WidgetId -> Sim a Rect
widgetRect wid = do
  ctx <- simContext
  Just r <- liftIO (getPrevRect ctx wid)
  pure r

-- | Labels @prefix 1@ to @prefix n@.
numbered :: String -> Int -> NanoUI ()
numbered prefix n = mapM_ (\i -> label (T.pack (prefix <> show i))) [1 .. n]

-- | The thumb of a list's bar: the pointer on it shows a grab cursor and a
-- press there a grabbing one, and behind a modal, which takes the press,
-- neither. The pointer on the bar brightens the thumb, and so does dragging
-- it off the bar. Moving onto or off the bar wakes a frame that repaints the
-- bar alone; moving along it wakes none.
runScrollThumbTest :: Context -> IORef Int -> IO ()
runScrollThumbTest ctx failed = do
  dialog <- newVar False
  let off = withInputOff 200 120
      ui = do
        r <- scrollArea (fillW . fixedH 80) (column (replicateM_ 8 (label "scroll line")))
        modal dialog "Title" (label "body")
        pure r
  (sid, ()) <- warmup2 ctx off ui
  theme <- readIORef (ctxTheme ctx)
  let rest = scrollBarThumbColor (themeInput theme) theme
      hovered = scrollBarThumbHoverColor (themeInput theme) theme
      thumbIs inp want = do
        (_, dd, _) <- runFrame ctx inp ui
        colors <- map snd <$> drawQuads dd
        assert failed (want `elem` colors && notElem (if want == rest then hovered else rest) colors)
  assertJustM failed (getPrevRect ctx sid) $ \(Rect rx ry rw rh) -> do
    let thumbX = rx + rw - scrollBarGutter ScrollBarList 0 / 2
        onBar y = off {inputMousePos = V2 thumbX y}
        over = onBar (ry + rh / 2)
    assert failed =<< needsRedraw ctx off over
    thumbIs over hovered
    damage <- takeDamage ctx
    assert failed (case damage of DamageClip r -> rectW r < 30; DamageFull -> False)
    assert failed . not =<< needsRedraw ctx over (onBar (ry + rh / 2 + 4))
    assert failed =<< needsRedraw ctx over off
    thumbIs off rest
    -- A drag keeps its thumb bright wherever the pointer goes.
    thumbIs (applyMouseButton MouseLeft True over) hovered
    thumbIs (holdAt off (V2 20 (ry + rh / 2))) hovered
    thumbIs (applyMouseButton MouseLeft False off) rest
    assertJustM failed (findGrabHover ctx ui off thumbX [ry + rh * n / 8 | n <- [1 .. 7]]) $ \hover -> do
      let down = applyMouseButton MouseLeft True hover
      _ <- runFrame ctx down ui
      assert failed =<< cursorKindIs ctx down UiCursorGrabbing
      _ <- runFrame ctx (releaseAt down) ui
      writeVarIO dialog True
      assertEq failed UiCursorDefault =<< cursorOver ctx hover ui (inputMousePos hover)

-- | The scroll content's right edge stops at the bar's gutter, one gap
-- before the bar. The gap matches the scroller's right padding and is never
-- under the side gap. A list bar keeps that gap to its well's edge as well;
-- a page bar sits a side gap inside the page's edge, where it takes the
-- pointer, and the sliver past it and the gap before it stay clear.
runScrollBarGutterTest :: Context -> IORef Int -> IO ()
runScrollBarGutterTest ctx failed = do
  let listPad = padR (layoutPadding defaultLayout)
      wideThen n = labelWith' fillW "Wide" <* replicateM_ n (label "scroll line")
      page = scrollArea (padAll 12 . grow) (wideThen 20)
  forM_
    [ (withInput 200 120, scrollArea (fillW . fixedH 60) (wideThen 8), scrollBarGutter ScrollBarList listPad, listPad)
    , (withInput 240 140, scrollArea (tight . grow) (wideThen 20), scrollBarGutter ScrollBarPage 0, 0)
    , (withInput 240 140, page, scrollBarGutter ScrollBarPage 12, 12)
    , (withInput 240 140, panelWith grow (scrollArea (tight . grow) (wideThen 20)), scrollBarGutter ScrollBarList 0, 0)
    ]
    $ \(inp0, ui, gutter, endPad) -> do
      (sid, child) <- warmup2 ctx inp0 ui
      assertScrollGutterPad failed ctx sid child gutter endPad
  let inp0 = withInputOff 240 140
  (sid, _) <- warmup2 ctx inp0 page
  assertJustM failed (getPrevRect ctx sid) $ \(Rect sx sy sw sh) -> do
    let barLeft = sx + sw - scrollBarGutter ScrollBarPage 12
        grabs x = isJust <$> findGrabHover ctx page inp0 x [sy + sh * n / 8 | n <- [1 .. 7]]
    assertEq failed [True, False, False] =<< mapM grabs [barLeft + scrollBarWidth / 2, sx + sw - 1, barLeft - 6]

-- | A scroller whose track is shorter than the smallest thumb shows no bar,
-- rather than a thumb that overruns the track and moves against the offset.
runScrollBarShortTrackTest :: Context -> IORef Int -> IO ()
runScrollBarShortTrackTest _ failed = do
  let bar h = scrollBarLayout ScrollBarList DirColumn 0 0 100 h (Padding 0 0 0 0) 500
  assert failed (not (isJust (bar 20 0)))
  -- A track as long as the smallest thumb still shows one.
  assertJust failed (bar 22 100) $ \layout -> assert failed (covers (sbTrack layout) (sbThumb layout))

-- | Each change of a scroll offset damages the scroller's viewport only. A
-- text change inside a scroller repaints the bar's lane only when it changes
-- the content's size: a same-size edit repaints the text alone, and one that
-- makes the content taller moves the thumb. A hover in a page scroller
-- damages within the window.
runScrollDamageTest :: Context -> IORef Int -> IO ()
runScrollDamageTest ctx failed = do
  let inp0 = withInputOff 200 120
      linesOf :: T.Text -> T.Text -> NanoUI WidgetId
      linesOf firstLine lastLine = fmap fst . scrollArea (fillW . fixedH 60) . column $ do
        label firstLine
        replicateM_ 6 (label "scroll line")
        label lastLine
      clipOf c = takeDamage c >>= \case
        DamageClip r -> pure (Just r)
        DamageFull -> pure Nothing
      ui = linesOf "count 1" "last"
  sid <- warmup2 ctx inp0 ui
  forM_ [24, 48] $ \off -> do
    _ <- runFrame ctx inp0 (ui <* liftIO (setScrollOffset ctx sid off))
    assertJustM failed (clipOf ctx) $ \r -> assert failed (rectW r > 0 && rectH r > 0 && rectH r <= 60 + defaultDamageSlop * 2)
  setScrollOffset ctx sid 0
  _ <- warmup2 ctx inp0 ui
  assertJustM failed (getPrevRect ctx sid) $ \(Rect sx sy sw sh) -> do
    _ <- runFrame ctx inp0 (linesOf "count 2" "last")
    assertJustM failed (clipOf ctx) $ \r -> assert failed (rectW r > 0 && rectH r > 0 && not (rectContains r (V2 (sx + sw - 2) (sy + sh / 2))))
  -- On the page the lines that move repaint the window whole, so make the
  -- content taller in a floating window, which clips them to its viewport.
  let inWindow lastLine = snd <$> window' (constVar True) "Lines" (linesOf "count 1" lastLine)
      winInp = withInputOff 640 400
  mWsid <- warmup2 ctx winInp (inWindow "last")
  assertJust failed mWsid $ \wsid -> assertJustM failed (getPrevRect ctx wsid) $ \(Rect wx wy ww wh) -> do
    _ <- runFrame ctx winInp (inWindow "last\nmore\nmore")
    assertJustM failed (clipOf ctx) $ \r -> assert failed (rectContains r (V2 (wx + ww - 2) (wy + wh / 2)))
  pixel <- newPixelContext
  let controls = scrollWith (tight . grow) . columnWith (padAll 8 . gap 8 . fillW) . card $ do
        heading "Controls"
        (cb, _) <- withValue False (checkbox' "Feature")
        void (withValue 50 (slider 0 100))
        numbered "pad line " 16
        pure cb
  runSim pixel failed (withInputOff 400 160) controls $ do
    _ <- moveTo . On =<< result
    dmg <- liftIO (takeDamage pixel)
    check (case dmg of DamageFull -> True; DamageClip (Rect _ y _ h) -> y >= -1 && y + h <= 161)

-- | A page-level (grow by grow) scroller paints no well, so on a clip frame
-- the strip its scrolled content vacated has no covering command and would
-- show a ghost of an earlier offset: every clip frame fills the viewport
-- with the page colour. A full frame leaves the page surface to the backend
-- (an opaque 'pageColor' or 'panelPage' over the window clear, and nothing
-- for the default page, which is the clear already), so the scroller's own
-- backdrop is left out unless something was drawn under it.
runPageBackdropTest :: Context -> IORef Int -> IO ()
runPageBackdropTest ctx failed = do
  theme <- readIORef (ctxTheme ctx)
  let inp0 = withInputOff 300 220
      page around = fmap fst . around $ scrollArea grow (column (replicateM_ 20 (label "backdrop line")))
      near a b = abs (a - b) <= 0.6
      sameViewport (Rect rx ry rw rh) (Rect qx qy qw qh) =
        near qx rx && near qy ry && near (qx + qw) (rx + rw) && near (qy + qh) (ry + rh)
      -- The frame after @prep@ on a fresh context: its clear colour, and
      -- how many quads fill the scroller's viewport.
      frameAfter full ui prep = do
        c <- newContext
        writeIORef (ctxPaintFull c) full
        sid <- warmup2 c inp0 ui
        void (prep c sid)
        (_, draw, _) <- runFrame c inp0 ui
        quads <- drawQuads draw
        Just r <- getPrevRect c sid
        pure (drawClear draw, length [() | (q, _) <- quads, sameViewport r q])
      red = colorRGBA 1 2 3 255
  -- A clip frame after a scroll, here one that repaints a few lines: a frame
  -- with nothing damaged paints nothing at all.
  let scrolled c sid = setScrollOffset c sid 120 >> void (runFrame c inp0 (page id)) >> damageRect c (Rect 10 10 20 20)
  (_, fills) <- frameAfter False (page id) scrolled
  assertGt failed fills 0
  forM_
    [ (id, themeWindow theme, 0)
    , (themed (pageColor red theme), red, 0)
    , (themed (panelPage theme), styleBg (themePanel theme), 0)
    , -- A panel under the scroller must be repainted over.
      (panel, themeWindow theme, 1)
    ]
    $ \(around, bg, n) -> assertEq failed (bg, n) =<< frameAfter True (page around) (\_ _ -> pure ())

-- | A label far taller than its scroller draws only the lines near the
-- viewport: its quads stay few however long the text is, and every row of
-- the viewport inside its padding still shows a glyph, wherever the scroller
-- is. Monospace glyphs are boxes a line tall, so the rows they cover are the
-- lines drawn.
runTallLabelCullTest :: Context -> IORef Int -> IO ()
runTallLabelCullTest ctx failed = do
  let inp0 = withInputOff 300 220
      txt = T.intercalate "\n" [T.pack ("line " <> show i) | i <- [1 .. 1000 :: Int]]
      ui = fmap fst $ scrollArea (fillW . fixedH 100) (labelWith fillW txt)
  sid <- warmup2 ctx inp0 ui
  forM_ [0, 3333, 1000000] $ \off -> do
    setScrollOffset ctx sid off
    (_, draw, _) <- run2Frames ctx inp0 ui
    assertJustM failed (getPrevRect ctx sid) $ \(Rect rx ry rw rh) -> do
      quads <- drawQuads draw
      -- Glyph boxes, not the scroller's backdrop, border or bar.
      let glyphs = [r | (r@(Rect qx _ qw qh), _) <- quads, qw < 40, qh < 40, qx < rx + rw / 2]
          shown y = any (\(Rect _ gy _ gh) -> gy <= y && y < gy + gh) glyphs
      assert failed (length quads < 400)
      assert failed (all shown [ry + fromIntegral k | k <- [12, 17 .. floor rh - 12 :: Int]])

-- | The wheel scrolls the scroller under the pointer: the inner of two
-- nested ones and not the outer, which it leaves alone once the inner one
-- reaches its end. Focus is not enough to take the wheel. Hit rects follow
-- the offset: a wheel just above the scrolled inner viewport does not reach
-- it, and one below it scrolls the outer. An inner scroller clipped out
-- below the outer viewport takes no wheel there.
runNestedScrollTest :: Context -> IORef Int -> IO ()
runNestedScrollTest ctx failed = do
  let ui = scrollArea (fillW . fixedH 90) . column $ do
        (inner, _) <- scrollArea (fillW . fixedH 40) . column $ button' "In" >> numbered "in " 12
        numbered "out " 12
        pure inner
  runSim ctx failed (withInput 240 220) ui $ do
    (outer, inner) <- result
    let offsets = liftIO ((,) <$> getScrollOffset ctx outer <*> getScrollOffset ctx inner)
        notch = frameWith (\i -> i {inputScroll = V2 0 1})
        pump prev = do
          (_, now) <- notch >> offsets
          when (now > prev) (pump now)
    r@(Rect ix iy iw ih) <- widgetRect inner
    Rect _ oy _ oh <- widgetRect outer
    pressTab 1
    check . (/= WidgetId 0) =<< liftIO (getFocusId ctx)
    _ <- wheel (at 230 210) (V2 0 1)
    checkEq (0, 0) =<< offsets
    _ <- wheel (Within r) (V2 0 1)
    (o1, i1) <- offsets
    checkEq 0 o1
    checkGt i1 0
    pump i1
    (o2, iMax) <- offsets
    checkEq 0 o2
    _ <- wheel (at (ix + iw / 2) (iy - 6)) (V2 0 (-1))
    checkEq iMax . snd =<< offsets
    _ <- wheel (at (ix + iw / 2) (min (oy + oh - 4) (iy + ih + 8))) (V2 0 1)
    o3 <- fst <$> offsets
    checkGt o3 0
  c <- newContext
  let below = scrollArea (fillW . fixedH 80) . column $ do
        numbered "out " 10
        fst <$> scrollArea (fillW . fixedH 36) (column (numbered "in " 8))
  runSim c failed (withInput 200 400) below $ do
    (_, inner) <- result
    r <- widgetRect inner
    check (v2Y (spanCenter r) > 80 && v2Y (spanCenter r) < 400)
    _ <- wheel (Within r) (V2 0 1)
    checkEq 0 =<< liftIO (getScrollOffset c inner)

-- | A button scrolled into view inside a fixed-height or a page-level
-- scroller takes the click where it is drawn. One scrolled out of its
-- viewport keeps its last rect, but a pointer there does not hover it, show
-- its cursor or click it.
runScrollHitTest :: Context -> IORef Int -> IO ()
runScrollHitTest _ failed = do
  let target layout pads = do
        hitVar <- useVar ("" :: T.Text)
        hit <- readVar hitVar
        (sid, btn) <- scrollArea layout . column $ do
          replicateM_ pads (label "pad")
          b <- button' "Target"
          when (respClicked b) (writeVar hitVar "yes")
          pure b
        pure (sid, btn, hit)
      wheelDown n = do
        (sid, _, _) <- result
        replicateM_ n (wheel (OnWidget sid) (V2 0 1))
        liftIO . (`getScrollOffset` sid) =<< simContext
  forM_ [(newContext, withInput 240 160, fillW . fixedH 80), (newPixelContext, withInput 640 120, tight . grow)] $ \(mk, inp, layout) -> do
    ctx <- mk
    runSim ctx failed inp (target layout 6) $ do
      off <- wheelDown 8
      checkGt off 0
      (_, b, _) <- frame
      checkEq "yes" . (\(_, _, hit) -> hit) =<< click (On b)
  ctx <- newContext
  runSim ctx failed (withInput 240 160) (target (fillW . fixedH 8) 40) $ do
    _ <- wheelDown 80
    (_, b, _) <- result
    r <- widgetRect (respId b)
    checkEq UiCursorDefault =<< cursorAt (Within r)
    check . (/= respId b) =<< liftIO (getHotId ctx)
    checkEq "" . (\(_, _, hit) -> hit) =<< click (Within r)

-- | Content in a scroller outside its parent's viewport has an empty clip
-- (rather than none) and takes no pointer input, even where it has scrolled
-- into the outer viewport's rect: here layered buttons and a pinned one in
-- an inner scroller below the outer one's fold, scrolled up into it. In view,
-- the pinned button wins over the layers, and the top layer's button wins
-- beside it.
runDisjointViewportTest :: Context -> IORef Int -> IO ()
runDisjointViewportTest ctx failed = do
  let filler h = spacer (Fixed 10) (Fixed h)
      ui = fmap (\(outer, (inner, bs)) -> (outer, inner, bs)) . scrollArea (fixedH 100 . fillW) . column $ do
        filler 150
        inner <- scrollArea (fixedH 30 . fillW) . column $ do
          stacked <- layers (mapM (buttonWith' (fixedWH 100 20)) ["Under", "Over"])
          pinned <- buttonWith' (pinAt 60 0 . fixedWH 40 20) "Pin"
          filler 200
          pure (stacked ++ [pinned])
        filler 100
        pure inner
      -- A point on the top layer's button left of the pinned one, and on the pinned one.
      targets bs = [(1, V2 (rectX (respRect (bs !! 1)) + 10) (v2Y (centerOf (bs !! 1)))), (2 :: Int, centerOf (bs !! 2))]
      na = ctxNodeArena ctx
      buttonsAfter offsets = liftIO offsets >> (\(_, _, bs) -> bs) <$> settle
      hot = liftIO (getHotId ctx)
  runSim ctx failed (withInputOff 300 200) ui $ do
    (outer, inner, _) <- result
    hidden <- buttonsAfter (setScrollOffset ctx inner 100)
    forM_ hidden $ \b -> do
      Just idx <- liftIO (lookupNodeByWidgetId na (respId b))
      checkEq Nothing =<< liftIO (getClipRect na idx)
      check . maybe False (\(Rect _ _ w h) -> w == 0 && h == 0) =<< liftIO (getClipBounds na idx)
    forM_ (targets hidden) $ \(_, p) -> do
      check (v2Y p > 0 && v2Y p < 100)
      check . (`notElem` map respId hidden) =<< (moveTo (At p) >> frame >> hot)
      (_, _, pressed) <- press (At p)
      check (not (any (\r -> respHovered r || respPressed r) pressed))
      (_, _, clicked) <- release
      check (not (any respClicked clicked))
    shown <- buttonsAfter (setScrollOffset ctx inner 0 >> setScrollOffset ctx outer 140)
    forM_ (targets shown) $ \(i, p) -> do
      checkEq (respId (shown !! i)) =<< (moveTo (At p) >> frame >> hot)
      (_, _, clicked) <- click (At p)
      checkEq [j == i | j <- [0 .. 2]] (map respClicked clicked)

-- | Probe for the reported scroll stair-stepping artifact: step a scroller
-- through fractional offsets at a display scale of 2 and decode the final
-- (snapped) vertex buffer. Every row's text and the geometry beside it (the
-- separators) move by the same delta on every step.
runScrollLockstepProbeTest :: Context -> IORef Int -> IO ()
runScrollLockstepProbeTest ctx failed = do
  setDrawSnapScale ctx 2
  let inp0 = withInput 300 220
      rows =
        [ ("Feature", "Enabled")
        , ("Volume", "50")
        , ("Quality", "High")
        , ("Accent", "#3D7EFF")
        , ("Theme", "Tomorrow at Midnight Min")
        , ("Theme radio", "Theme radio value")
        , ("Name", "Ada Lovelace")
        , ("Notes", "short note")
        , ("Tree", "1 visible item")
        , ("Table sort", "Name")
        ]
      kvRow k v = rowWith (tight . gap 12 . alignMid . fillW) $ do
        labelWith (minW 88 . tight) k
        labelWith (tight . fillW . alignEnd) v
      ui = scrollArea (fillW . fixedH 200) (column (mapM_ (\(k, v) -> kvRow k v >> separator) rows))
  (sid, ()) <- warmup2 ctx inp0 ui
  yss <- forM [0.0, 0.3, 0.6, 1.0, 1.3, 1.7, 2.0, 2.4, 2.7, 3.1, 3.4, 3.8] $ \off -> do
    setScrollOffset ctx sid off
    (_, draw, _) <- run2Frames ctx inp0 ui
    ss <- collectTextSpans ctx
    quads <- decodeQuads draw
    -- Solid fills sample the atlas's white texel (a copy of
    -- 'NanoUI.Internal.Draw.Arena.whitePixel', which tests cannot import).
    let white = 1.5 / 1024
        solid u v = abs (u - white) < 1.0e-6 && abs (v - white) < 1.0e-6
        keyYs = [y | (k, _) <- rows, y <- take 1 (spanYOf k ss)]
    pure (sort keyYs, sort [y1 | (x1, y1, x2, _, u, v) <- quads, solid u v, x2 - x1 > 60, y1 > 1])
  -- The probe found rows and separators to compare at every step.
  assert failed (all (\(ks, fs) -> not (null ks) && not (null fs)) yss)
  let steady ds = and [abs (d - e) <= 1.0e-3 | (d, e) <- zip ds (drop 1 ds)]
  forM_ (zip yss (drop 1 yss)) $ \((keyA, fillA), (keyB, fillB)) -> do
    let textDs = zipWith subtract keyA keyB
        fillDs = zipWith subtract fillA fillB
        n = min (length textDs) (length fillDs)
    case (textDs, fillDs) of
      (t0 : _, f0 : _) -> assert failed (steady (take n textDs) && steady (take n fillDs) && abs (t0 - f0) <= 1.0e-3)
      _ -> pure ()

-- | The final (snapped) quads of a vertex buffer: each quad is 4 vertices
-- of 8 floats (x, y, r, g, b, a, u, v), as (x1, y1, x2, y2, u, v).
decodeQuads :: DrawData -> IO [(Float, Float, Float, Float, Float, Float)]
decodeQuads dd = withForeignPtr (drawVertices dd) $ \vp -> do
  let fptr = castPtr vp :: Ptr Float
  forM [0 .. drawVertexCount dd `div` 4 - 1] $ \q -> do
    vs <- forM [0 .. 3] $ \k -> mapM (peekElemOff fptr . ((q * 4 + k) * 8 +)) [0, 1, 6, 7]
    let col j = map (!! j) vs
    pure (minimum (col 0), minimum (col 1), maximum (col 0), maximum (col 1), col 2 !! 0, col 3 !! 0)

-- | The main extent of the scroll node @wid@ (its content width for a 2D
-- one, its content height for a 1D one) and the size inside its 6px padding.
scrollNodeState :: Context -> WidgetId -> Bool -> IO (Maybe (Float, Float))
scrollNodeState ctx wid is2D = do
  let na = ctxNodeArena ctx
  found <- findNodeM na $ \i -> do
    nt <- getNodeType na i
    if nt == NodeScrollContainer then (== wid) <$> getWidgetId na i else pure False
  forM found $ \i -> do
    contentMain <- if is2D then getScrollContentW na i else getNodeValue na i
    Rect _ _ rw rh <- getNodeRect na i
    pure (contentMain, (if is2D then rw else rh) - 12)

-- | 2D scrollers measure and place what they hold. A grow cell with its own
-- minimum width (zero included) counts as that minimum, not as its widest
-- label, while a grow wrapper with none still counts its content. A
-- scroller fitting its width to its content, its height capped, makes room
-- for the bar its overflow brings. A 2D scroller lines its children up along
-- its direction, a row sideways and a layout asking for a grid (which a
-- scroller does not make) down. Padding counts once: a fitting child
-- reports no overflow (content measured from the content origin, not the
-- padding box) and does not wheel, while an overflowing one wheels on to its
-- trailing padding.
runScroll2DLayoutTest :: Context -> IORef Int -> IO ()
runScroll2DLayoutTest _ failed = do
  let longName = T.replicate 12 "long name "
      rows cell = scrollArea2D (fillW . fixedH 120) . columnWith (fillW . tight) . replicateM_ 3 . rowWith (fillW . gap 10 . tight) $ do
        void cell
        rowWith (fixedW 80 . tight) (label "size")
      rangeX w ui = do
        c <- newContext
        (wid, ()) <- warmup2 c (withInput w 200) ui
        maybe (-1) (v2X . scrollRange) <$> getScrollMetrics c wid
      minCell m = columnWith (grow . minW m . tight) (label longName)
  -- The row's least width is 100 + 10 + 80 = 190: it fits at 400, not at 150.
  ranges <-
    sequence
      [ rangeX 400 (rows (minCell 100))
      , rangeX 150 (rows (minCell 0))
      , rangeX 150 (rows (minCell 100))
      , rangeX 400 (rows (columnWith (grow . tight) (rowWith (fixedW 600 . tight) (label "wide"))))
      ]
  assertEq failed [True, True, False, False] (map (== 0) ranges)
  assert failed (all (>= 0) ranges)
  fit <- newContext
  _ <- warmup2 fit (withInput 400 300) (columnWith tight (scrollWith (maxH 200) (mapM_ (label . T.pack . ("item " <>) . show) [10 .. 59 :: Int])))
  -- Seven glyphs of the test font, as wide as its lines are tall.
  assertEq failed (replicate 50 (7 * 12)) . map rectW . drop 2 =<< arenaRects fit
  forM_ [(\l -> l {layoutDirection = Row}, V2 300 0, map rectX, [0, 100 .. 500]), (gridCols 3, V2 0 40, map rectY, [0, 40 .. 200])] $
    \(lay, range, along, want) -> do
      c <- newContext
      (sid, ()) <-
        warmup2 c (withInput 400 300) . columnWith tight . scrollArea2D (lay . tight . gap 0 . fixedWH 300 200) $
          replicateM_ 6 (box (fixedWH 100 40) (colorRGBA 255 0 0 255))
      assertEq failed want . along . drop 2 =<< arenaRects c
      assertJustM failed (getScrollMetrics c sid) $ \m -> assertEq failed range (scrollRange m)
  padded <- newPixelContext
  let inp0 = (withInput 320 240) {inputMousePos = V2 100 100}
      overflowEps = 0.5
      fits = scrollArea2D (padAll 6 . fixedH 168 . fillW) (columnWith (tight . fillW) (mapM_ label ["alpha", "beta", "gamma"]))
  (wid, ()) <- warmup2 padded inp0 fits
  assertJustM failed (scrollNodeState padded wid True) $ \(contentW, innerW) -> assert failed (contentW <= innerW + overflowEps)
  _ <- runFrame padded inp0 {inputScroll = V2 5 5} fits
  assertEq failed (V2 0 0) =<< getScrollOffset2D padded wid
  (wid1, ()) <- warmup2 padded inp0 (scrollArea (padAll 6 . fixedH 80 . fillW) (labelWith tight "fits"))
  assertJustM failed (scrollNodeState padded wid1 False) $ \(contentH, innerH) -> assert failed (contentH <= innerH + overflowEps)
  over <- newPixelContext
  let overflows = scrollArea2D (padAll 6 . fixedH 168 . fillW) . columnWith (tight . fillW) $ do
        labelWith (tight . fixedW 500) "wide child"
        replicateM_ 30 (label "scroll line")
  (wid2, ()) <- warmup2 over inp0 overflows
  assertJustM failed (scrollNodeState over wid2 True) $ \(contentW, innerW) -> assertGt failed contentW (innerW + 40)
  assertJustM failed (scrollNodeState over wid2 False) $ \(contentH, innerH) -> do
    assertGt failed contentH (innerH + 100)
    -- Far past the end, the clamp lands on content + padB - view, where the
    -- active horizontal bar takes its lane out of the view.
    replicateM_ 40 (runFrame over inp0 {inputScroll = V2 0 50} overflows)
    V2 _ offEnd <- getScrollOffset2D over wid2
    assert failed (abs (offEnd - (contentH + 6 - (innerH - scrollBarGutter ScrollBarList 6))) < 1.5)

-- | The wheel covers the configured step per notch: the context's, and the
-- scroller's own once it has one. With a glide time set, a notch eases onto
-- its target over several frames, counted as animating until it lands
-- exactly there; a notch mid-glide adds to the throw, and an offset set
-- outright wins over it. Content that shrinks under a glide pulls the glide
-- back, rather than letting it coast to an offset the content cannot reach.
runScrollTuningTest :: Context -> IORef Int -> IO ()
runScrollTuningTest ctx failed = do
  rows <- newIORef (40 :: Int)
  let ui = do
        n <- liftIO (readIORef rows)
        fst <$> scrollArea (fillW . fixedH 80) (column (replicateM_ n (label "scroll line")))
  setScrollTuning ctx defaultScrollTuning {scrollWheelStep = 40}
  runSim ctx failed (withInput 200 120) ui $ do
    sid <- result
    let notch = frameWith (\i -> i {inputScroll = V2 0 1})
        offset = liftIO (getScrollOffset ctx sid)
        gliding = liftIO (scrollGliding ctx sid)
        io = liftIO
    _ <- moveTo (OnWidget sid)
    checkEq 40 =<< (notch >> offset)
    io (setScrollStep ctx sid 12)
    checkEq 52 =<< (notch >> offset)
    io (setScrollStep ctx sid 0)
    checkEq 92 =<< (notch >> offset)
    io (setScrollTuning ctx defaultScrollTuning {scrollWheelStep = 60, scrollSmoothTime = 0.2} >> setScrollOffset ctx sid 0)
    modifyInput (\i -> i {inputDeltaTime = 1 / 60})
    partial <- notch >> offset
    check (partial > 0 && partial < 60)
    check =<< gliding
    check =<< io (anyAnimating ctx)
    replicateM_ 30 frame
    checkEq 60 =<< offset
    check . not =<< gliding
    replicateM_ 2 notch >> replicateM_ 30 frame
    checkEq 180 =<< offset
    _ <- notch
    io (setScrollOffset ctx sid 20)
    replicateM_ 5 frame
    checkEq 20 =<< offset
    _ <- frameWith (\i -> i {inputScroll = V2 0 8})
    check =<< gliding
    io (writeIORef rows 12)
    replicateM_ 40 frame
    Just m <- io (getScrollMetrics ctx sid)
    checkGt (v2Y (scrollRange m)) 0
    check . (<= v2Y (scrollRange m) + 0.5) =<< offset

-- | The metrics a scroller publishes each frame and the commands that read
-- them: to the end, back to the start, by whole pages, and a widget or a
-- content rectangle (a virtualized row no widget was built for) brought into
-- view. A row brought into view on a frame the wheel already moved lands
-- where asked: the last frame's rects go with the offset they were placed
-- at, not the one the wheel left.
runScrollCommandsTest :: Context -> IORef Int -> IO ()
runScrollCommandsTest ctx failed = do
  setScrollTuning ctx defaultScrollTuning {scrollSmoothTime = 0}
  jump <- newIORef False
  let ui = do
        (sid, rows) <- scrollArea (fillW . fixedH 80) (column (forM [1 .. 16 :: Int] (\i -> label' (T.pack ("line " <> show i)))))
        go <- liftIO (readIORef jump)
        when go $ liftIO (scrollIntoView ctx sid (respId (rows !! 11)) ScrollStart ScrollInstant)
        pure (sid, rows)
  runSim ctx failed (withInput 200 160) ui $ do
    (sid, rows) <- result
    Just m <- liftIO (getScrollMetrics ctx sid)
    r <- widgetRect sid
    let range = v2Y (scrollRange m)
        view = scrollViewport m
        after cmd = liftIO cmd >> frame >> liftIO (getScrollOffset ctx sid)
        target = respId (rows !! 11)
        atTop wid = widgetRect wid >>= \rr -> check (abs (rectY rr - rectY view) < 1.5)
    checkEq (ScrollAxisY, V2 0 0, 0) (scrollAxes m, scrollOffset m, v2X (scrollRange m))
    checkGt range 0
    -- The viewport is the scroller's box inside its padding and bar lane.
    check (rectW view <= rectW r && rectH view <= rectH r)
    checkEq range =<< after (scrollToEnd ctx sid ScrollInstant)
    checkEq 0 =<< after (scrollToStart ctx sid ScrollInstant)
    checkEq (min range (rectH view)) =<< after (scrollPages ctx sid (V2 0 1) ScrollInstant)
    placed <- after (scrollIntoView ctx sid target ScrollStart ScrollInstant)
    atTop target
    -- Already in view: the nearest alignment leaves the offset alone.
    checkEq placed =<< after (scrollIntoView ctx sid target ScrollNearest ScrollInstant)
    -- A row above the viewport comes back to the top edge.
    _ <- after (scrollIntoView ctx sid (respId (rows !! 1)) ScrollNearest ScrollInstant)
    atTop (respId (rows !! 1))
    rowH <- rectH <$> widgetRect target
    off <- after (scrollRectIntoView ctx sid (Rect 0 (rowH * 8) 10 rowH) ScrollStart ScrollInstant)
    check (abs (off - rowH * 8) < 1.5)
    _ <- after (scrollToStart ctx sid ScrollInstant)
    _ <- moveTo (OnWidget sid)
    liftIO (writeIORef jump True)
    _ <- frameWith (\i -> i {inputScroll = V2 0 1})
    liftIO (writeIORef jump False)
    _ <- frame
    atTop target

-- | On a fresh context, hover where @aim@ points given the scrollers the
-- view returns, turn the wheel by @d@ there, and say which scrollers moved.
wheelMoves :: IORef Int -> NanoUI [WidgetId] -> ([WidgetId] -> Sim [WidgetId] Target) -> V2 -> IO [Bool]
wheelMoves failed ui aim d = do
  ctx <- newContext
  runSim ctx failed (withInputOff 400 300) ui $ do
    _ <- flip wheel d =<< aim =<< result
    liftIO . mapM (fmap (/= V2 0 0) . getScrollOffset2D ctx) =<< result

-- | Which scroller takes the wheel. The topmost under the pointer, even one
-- pinned over another but declared first; a pinned panel over the lower one
-- blocks it with 'PointerBlock' and passes it through otherwise, and a
-- blocking card inside a scroller does not stop that scroller. A scroller
-- drawn in the overflow of a root shorter than the window takes it there:
-- the root clips to the window, as in paint. A nested scroller whose content
-- fits takes none, and the page round it scrolls; so does the page where an
-- inner scroller wider than it runs on under its bar lane (where paint clips
-- the inner one away), and a press on the page's thumb there grabs it.
-- The unused axis goes on outward: a sideways wheel over a vertical page
-- with no horizontal scroller round it scrolls the first one inside, found
-- past rows that hold none, preferring one under the pointer whose content
-- fits; a 2D scroller pairs with a 1D one either way, each taking the axis
-- the one under the pointer cannot move; and a popup stops it, both ways.
runWheelRoutingTest :: Context -> IORef Int -> IO ()
runWheelRoutingTest _ failed = do
  let rows n = column (replicateM_ n (label "row"))
      across w h l = (fixedWH w h l) {layoutDirection = Row}
      cells = replicateM_ 30 (label "wide cell")
      wide = box (fixedWH 600 10) (colorRGBA 90 90 90 255)
      page body = (\(o, is) -> o : is) <$> scrollArea (fixedWH 300 200) (column body)
      on k sids = pure (OnWidget (sids !! k))
      point p _ = pure (At p)
      expectMoves want ui aim d = assertEq failed want =<< wheelMoves failed ui aim d
      overlapping mode = (\(t, u) -> [t, u]) <$> columnWith tight (do
        (top, ()) <- scrollArea (pinAt 20 20 . fixedWH 120 100) (rows 30)
        (under, ()) <- scrollArea (fixedWH 360 240) (rows 60)
        panelWith (pointer mode . pinAt 200 20 . fixedWH 100 80) (pure ())
        pure (top, under))
  forM_
    [ (PointerAuto, V2 60 60, [True, False])
    , (PointerAuto, V2 250 60, [False, True])
    , (PointerBlock, V2 250 60, [False, False])
    , (PointerBlock, V2 250 200, [False, True])
    ]
    $ \(mode, p, want) -> expectMoves want (overlapping mode) (point p) (V2 0 1)
  let carded = fmap (pure . fst) . columnWith tight $ do
        r <- scrollArea (fixedWH 300 200) . column $ do
          panelWith (pointer PointerBlock . fixedWH 200 60) (pure ())
          rows 30
        box (pinAt 350 250 . fixedWH 10 10) (colorRGBA 255 0 0 255)
        pure r
  expectMoves [True] carded (point (V2 60 30)) (V2 0 1)
  let overflowing = fmap pure . columnWith (tight . fixedH 100) $ do
        box (fixedWH 10 150) (colorRGBA 90 90 90 255)
        fst <$> scrollArea (fixedWH 300 100) (rows 30)
      belowRoot sids = widgetRect (head' sids) >>= \r -> Within r <$ checkGt (rectY r) 100
  expectMoves [True] overflowing belowRoot (V2 0 1)
  let fitting = page $ do
        inner <- fst <$> scrollArea (fillW . fixedH 100) (replicateM_ 2 (label "fits"))
        rows 30
        pure [inner]
      wider = page $ do
        inner <- fst <$> scrollArea (fixedWH 400 150) (replicateM_ 30 (label "inner row"))
        rows 30
        pure [inner]
      lane sids = (\(Rect px py pw _) -> at (px + pw - scrollBarGutter ScrollBarList 0 / 2) (py + 10)) <$> widgetRect (head' sids)
  expectMoves [True, False] fitting (on 1) (V2 0 1)
  expectMoves [True, False] wider lane (V2 0 1)
  ctx <- newContext
  runSim ctx failed (withInputOff 400 300) wider $ do
    _ <- press =<< lane =<< result
    check . isJust =<< liftIO (getsInteraction ctx isScrollDrag)
  let inside = page $ do
        replicateM_ 3 (row (label "plain" >> void (button' "b")))
        (inner, ()) <- column (scrollArea (across 200 40) cells)
        rows 20
        pure [inner]
      cornerOf sids = (\(Rect x y _ _) -> at (x + 10) (y + 5)) <$> widgetRect (head' sids)
      strips = page $ do
        (strip, ()) <- scrollArea (across 200 40) (label "fits")
        replicateM_ 3 (label "row")
        (carousel, ()) <- scrollArea (across 200 40) cells
        rows 20
        pure [strip, carousel]
  expectMoves [False, True] inside cornerOf (V2 1 0)
  expectMoves [False, False, False] strips (on 1) (V2 1 0)
  let inRow = (\(o, i) -> [o, i]) <$> scrollArea (across 300 200) (do
        (inner, ()) <- scrollArea2D (fixedWH 150 100) (column (wide >> rows 30))
        replicateM_ 30 (label "outer cell")
        pure inner)
      in2D = (\(o, l) -> [o, l]) <$> scrollArea2D (fixedWH 300 200) (column (do
        (list, ()) <- scrollArea (fixedWH 100 80) (rows 30)
        wide
        rows 30
        pure list))
  expectMoves [False, True] inRow (on 1) (V2 1 0)
  expectMoves [False, True] in2D (on 1) (V2 0 1)
  expectMoves [True, False] in2D (on 1) (V2 1 0)
  let popped = page $ do
        rows 30
        strip <- snd <$> popup' (constVar True) (defaultPopupConfig (AnchorPoint (V2 200 20))) (fst <$> scrollArea (across 150 40) cells)
        pure (maybe [] pure strip)
  assertEq failed False . (!! 0) =<< wheelMoves failed popped (on 1) (V2 0 1)
  assertEq failed False . (!! 1) =<< wheelMoves failed popped (point (V2 20 150)) (V2 1 0)
  assertEq failed True . (!! 0) =<< wheelMoves failed popped (point (V2 20 150)) (V2 0 1)
  where
    head' sids = sids !! 0
