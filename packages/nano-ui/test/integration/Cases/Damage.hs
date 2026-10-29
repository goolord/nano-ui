module Cases.Damage (tests) where

import Spec
import NanoUI.Shortcut
import Data.ByteString qualified as BS
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text qualified as T
import Data.Primitive.SmallArray (smallArrayFromList)
import NanoUI.Internal.Context (Context (..), DamageState (..), PrevFrame (..), emptyPrevFrame, getPrevRectAt, getsDamage)
import NanoUI.Internal.Layout.Arena (arenaCount, getWidgetId)

tests :: [Spec]
tests =
  [ spec "damage-rect-math" runDamageRectMathTest
  , spec "damage-explicit" runExplicitDamageTest
  , spec "damage-var-paint-change" runVarPaintChangeDamageTest
  , spec "damage-hover" runHoverDamageTest
  , spec "damage-orphan-anim-settles" runOrphanAnimationDamageSettlesTest
  , spec "damage-content-change" runContentChangeDamageTest
  , spec "image-swap-damage" runImageSwapDamageTest
  , spec "clip-frame-backdrop" runClipFrameBackdropTest
  , spec "damage-pieces-far-labels" runFarLabelsDamagePiecesTest
  , spec "resize-damage" runResizeDamageTest
  , spec "label-button-swap-damage" runLabelButtonSwapDamageTest
  , spec "prev-frame-matches-fresh-walk" runPrevFrameFreshWalkTest
  , spec "scrolled-popup-damage" runScrolledPopupDamageTest
  , spec "empty-damage-paints-nothing" runEmptyDamagePaintTest
  ]

-- | Whether a frame's damage is a clip smaller in area than @a@.
clipUnder :: Float -> Damage -> Bool
clipUnder a (DamageClip (Rect _ _ w h)) = w * h < a
clipUnder _ DamageFull = False

-- | How damage requests resolve against a rect ('DamageNone' is the identity
-- of a union, not a rect at the origin), and how a frame's rects group into
-- pieces: far ones stay apart, near ones (or ones a third rect joins) merge
-- into the bounding box, which is no pieces, and there are never more than
-- four, disjoint and covering every rect.
runDamageRectMathTest :: Context -> IORef Int -> IO ()
runDamageRectMathTest _ failed = do
  let base = Rect 10 20 100 50
      stretch (Rect x y w h) = Rect (x - 1) (y - 2) (w + 10) (h + 20)
  forM_
    [ (DamageSelf, base)
    , (DamageInflated 8, Rect 2 12 116 66)
    , (DamageExact (Rect 0 0 500 500), Rect 0 0 500 500)
    , (DamageCustom stretch, Rect 9 18 110 70)
    , (DamageNone, Rect 0 0 0 0)
    , (DamageUnion (DamageInflated 4) (DamageInflated 8), rectUnion (Rect 6 16 108 58) (Rect 2 12 116 66))
    , (DamageUnion DamageSelf DamageNone, base)
    ]
    $ \(d, want) -> assertEq failed want (resolveDamageRect d base)
  assertEq failed base (resolveDamageRect (DamageUnion DamageNone (DamageExact base)) (Rect 0 0 0 0))
  let a = Rect 0 0 20 10
  assertEq failed 2 (length (damagePieces [a, Rect 500 400 20 10]))
  forM_ [[a, Rect 30 0 20 10], [a], [], [a, Rect 40 0 20 10, Rect 20 0 20 10]] $ \rs ->
    assertEq failed [] (damagePieces rs)
  let scattered = [Rect (fromIntegral i * 100) (fromIntegral i * 80) 10 10 | i <- [0 .. 7 :: Int]]
      pieces = damagePieces scattered
  assertEq failed 4 (length pieces)
  assert failed (all (\r -> any (`covers` r) pieces) scattered)
  assert failed (and [rectIntersect p q == Nothing | (i, p) <- zip [0 :: Int ..] pieces, (j, q) <- zip [0 ..] pieces, i < j])

-- | Damage a view asks for repaints in its own frame only: a widget's rect as
-- its resolver grows it, a rect, or the whole window. A rect wholly off the
-- window adds nothing to the clip of a change elsewhere. A host that clears
-- the dirty flag before the frame still gets the repaint 'markDirty' queued,
-- since no diff covers an opaque request. A host that does not read damage
-- gets 'DamageFull' and still keeps what the next frame diffs against.
runExplicitDamageTest :: Context -> IORef Int -> IO ()
runExplicitDamageTest ctx failed = do
  let inp = withInputOff 400 300
      ui :: Int -> (Response -> NanoUI ()) -> NanoUI (Response, Response)
      ui n extra = columnWith (fillW . fillH . padAll 20) $ do
        b <- button' "First"
        extra b
        flex
        (b,) <$> rowWith fillW (flex >> labelWith' (fixedW 40) (T.pack (show n)))
      none = const (pure ())
      damageOf n extra = runFrame ctx inp (ui n extra) >> takeDamage ctx
      isEmpty n = assert failed . damageIsEmpty =<< damageOf n none
  (btn, lbl) <- warmup2 ctx inp (ui 1 none)
  dmg <- damageOf 1 (\b -> damageWidgetNow (respId b) (DamageInflated sliderDamageSlop))
  let Rect ex ey ew eh = rectInflate sliderDamageSlop (respRect btn)
  case dmg of
    DamageClip (Rect x y w h) -> unless (all ((< 0.05) . abs) [x - ex, y - ey, w - ew, h - eh]) (assertEq failed (Rect ex ey ew eh) (Rect x y w h))
    DamageFull -> assert failed False
  isEmpty 1
  assertEq failed (DamageClip (Rect 5 5 20 20)) =<< damageOf 1 (const (damageRectNow (Rect 5 5 20 20)))
  isEmpty 1
  assertEq failed DamageFull =<< damageOf 1 (const damageFullNow)
  isEmpty 1
  damageOf 2 (const (damageRectNow (Rect 100 (-200) 50 50))) >>= \case
    DamageClip clip -> assert failed (rectY clip >= rectY (respRect lbl) - 10)
    DamageFull -> assert failed False
  markDirty ctx
  clearDirty ctx
  assert failed . not =<< isDirty ctx
  assertEq failed DamageFull =<< damageOf 2 none
  isEmpty 2
  writeIORef (ctxDamageWanted ctx) False
  assertEq failed DamageFull =<< damageOf 3 none
  writeIORef (ctxDamageWanted ctx) True
  assertEq failed (DamageClip (Rect 0 0 0 0)) =<< damageOf 3 none
  changed <- damageOf 4 none
  assert failed (clipCovers changed (respRect lbl) && not (damageCovers changed (respRect btn)))

-- | A Var write that changes only a box's colour, or a same-length label and
-- elsewhere a box's colour, repaints the whole window: the Var names no
-- widget, the colour is outside the store diff, and the text diff alone
-- would clip to the label.
runVarPaintChangeDamageTest :: Context -> IORef Int -> IO ()
runVarPaintChangeDamageTest _ failed = do
  let lamp on = box (fixedWH 40 40) (if on then colorRGBA 255 0 0 255 else colorRGBA 0 0 255 255)
      flag = do
        onVar <- useVar False
        on <- readVar onVar
        resp <- button' "Toggle"
        when (respClicked resp) (writeVar onVar (not on))
        labelWith fontMono (if on then "one" else "two")
        resp <$ lamp on
      level = do
        levelVar <- useVar (0 :: Float)
        v <- readVar levelVar
        resp <- button' "Raise"
        when (respClicked resp) (writeVar levelVar (v + 1))
        resp <$ lamp (v > 0)
  forM_ [flag, level] $ \ui -> do
    ctx <- newContext
    let inp0 = withInputOff 320 240
    resp <- warmup2 ctx inp0 ui
    _ <- runClick ctx inp0 ui (centerOf resp)
    assertEq failed DamageFull =<< takeDamage ctx

-- | The first frame repaints whole. Hover enter and press repaint only the
-- button, and pointer motion inside a hovered button asks for no frame.
runHoverDamageTest :: Context -> IORef Int -> IO ()
runHoverDamageTest ctx failed = do
  let ui = column (button' "OK")
      inp0 = withInputOff 240 80
      small = clipUnder (240 * 80 * 0.5)
  _ <- runFrame ctx inp0 ui
  assertEq failed DamageFull =<< takeDamage ctx
  V2 cx cy <- centerOf <$> evalUi ctx inp0 ui
  let over = inp0 {inputMousePos = V2 cx cy}
      idle = over {inputDeltaTime = 1}
      press = applyMouseButton MouseLeft True over
  assert failed =<< needsRedraw ctx inp0 over
  _ <- runFrame ctx over ui
  assert failed . small =<< takeDamage ctx
  _ <- runFrame ctx idle ui
  assert failed . not =<< needsRedraw ctx idle over {inputMousePos = V2 (cx + 1) cy}
  assert failed =<< needsRedraw ctx idle press
  _ <- runFrame ctx press ui
  assert failed . small =<< takeDamage ctx

-- | A perpetual animation on a widget repaints a clip over it, and once the
-- widget leaves the arena (a tab switch) it stops forcing repaints. A fresh
-- animation on a widget never laid out repaints whole for its first frame.
runOrphanAnimationDamageSettlesTest :: Context -> IORef Int -> IO ()
runOrphanAnimationDamageSettlesTest ctx failed = do
  let winInp = withInput 400 300
      inp = winInp {inputDeltaTime = 0.05}
      withBar = columnWith (padAll 20) (currentId <* spacer (Fixed 40) (Fixed 20))
      withoutBar = columnWith (padAll 20) (pure ())
  wid <- evalUi ctx inp withBar
  startAnimation ctx wid 0 1 1e9
  _ <- runFrame ctx inp withBar
  takeDamage ctx >>= \case
    DamageClip r -> assert failed (rectW r > 0 && rectH r > 0)
    DamageFull -> assert failed False
  -- The first absent frame may repaint the widget's old region.
  replicateM_ 2 (runFrame ctx inp withoutBar)
  assert failed =<< anyAnimating ctx
  assert failed . damageIsEmpty =<< takeDamage ctx
  ctx2 <- newContext
  startAnimation ctx2 (WidgetId 777) 0 1 0.3
  _ <- runFrame ctx2 winInp (label "bare")
  assertEq failed DamageFull =<< takeDamage ctx2
  _ <- runFrame ctx2 winInp (label "bare")
  assert failed . (/= DamageFull) =<< takeDamage ctx2

-- | Changes no rect or text diff sees still repaint the widget, as a clip: a
-- new version of a versioned drawing (paint rebuilds its ops, so a clip frame
-- would keep the old pixels), select all in a text area (on its own frame,
-- not a follow-up), and a label beside a dragged slider shrinking from
-- "100" (only 'storeFloat' changes, as in a scroll).
runContentChangeDamageTest :: Context -> IORef Int -> IO ()
runContentChangeDamageTest _ failed = do
  ctx <- newContext
  let versioned v = column $ do
        label "Other"
        drawingVersioned v (fixedWH 80 40) (\r -> smallArrayFromList [FillRect r (colorRGBA 255 0 0 255)])
  resp <- warmup2 ctx (withInput 400 300) (versioned 1)
  _ <- runFrame ctx (withInput 400 300) (versioned 2)
  assert failed . (`clipCovers` respRect resp) =<< takeDamage ctx
  -- Frame time lets the click's hover fade finish, which would damage the area.
  areaCtx <- newContext
  let inp0 = (withInputOff 800 600) {inputDeltaTime = 0.5}
      notes = column (label "Notes" >> fst <$> withValue "hello world" (textAreaWith' (fixedWH 200 80)))
  area <- warmup2 areaCtx inp0 notes
  _ <- runClick areaCtx inp0 notes (centerOf area)
  _ <- warmup2 areaCtx inp0 notes
  _ <- runFrame areaCtx (chordInp (cmdOrCtrl <> key 'a') inp0) notes
  assert failed . (`clipCovers` respRect area) =<< takeDamage areaCtx
  sliderCtx <- newContext
  ref <- newIORef (50 :: Float)
  let inp = withInputOff 400 100
      ui = row $ do
        (s, v) <- liftIO (readIORef ref) >>= \v0 -> withValue v0 (sliderWith' (fixedW 200) 0 100)
        liftIO (writeIORef ref v)
        (s,) <$> label' (T.pack (show (round v :: Int)))
  (s, _) <- warmup2 sliderCtx inp ui
  let Rect sx sy sw sh = respRect s
      at x = V2 x (sy + sh / 2)
  runDragFrom sliderCtx inp ui (at (sx + sw / 2)) (at (sx + sw + 50))
  assertEq failed 100 =<< readIORef ref
  (_, wide) <- evalUi sliderCtx (holdAt inp (at (sx + sw / 2))) ui
  assert failed . (`clipCovers` respRect wide) =<< takeDamage sliderCtx

-- | Switching an image to another id repaints the image and only it: frames
-- swapped inside a page neither leave a stale frame nor repaint the page.
runImageSwapDamageTest :: Context -> IORef Int -> IO ()
runImageSwapDamageTest ctx failed = do
  let px a = BS.pack (concat (replicate 16 [a, 0, 0, 255]))
  assert failed . and =<< sequence [registerImage ctx (ImageId 1) 4 4 (px 60), registerImage ctx (ImageId 2) 4 4 (px 120)]
  frameRef <- newIORef (ImageId 1)
  let inp0 = withInputOff 320 200
      ui = fmap snd . scrollArea (padAll 10 . grow) . column $ do
        label "frames"
        currentId <* (image (fixedWH 40 24) =<< liftIO (readIORef frameRef))
  wid <- warmup2 ctx inp0 ui
  writeIORef frameRef (ImageId 2)
  _ <- runFrame ctx inp0 ui
  dmg <- takeDamage ctx
  assertJustM failed (getPrevRect ctx wid) $ \r -> assert failed (clipCovers dmg r && clipUnder (320 * 200 / 4) dmg)

-- | A clip frame repaints its region from the page backdrop, as a full frame
-- lays the page over the window. An idle menu-bar title has no fill, so
-- without the backdrop the hover highlight it just lost would stay.
runClipFrameBackdropTest :: Context -> IORef Int -> IO ()
runClipFrameBackdropTest ctx failed = do
  writeIORef (ctxPaintFull ctx) False
  let inp0 = withInputOff 400 300
      ui = rowWith (tight . fillW . fixedH 28) (menuButton' "File" False <* menuButton' "Edit" False)
  file <- warmup2 ctx inp0 ui
  _ <- runFrame ctx inp0 {inputMousePos = centerOf file} ui
  (_, draw, _) <- runFrame ctx inp0 ui
  dmg <- takeDamage ctx
  theme <- readIORef (ctxTheme ctx)
  quads <- drawQuads draw
  case dmg of
    DamageClip clip -> do
      assert failed (covers clip (respRect file))
      assertJust failed (listToMaybe quads) $ \(r, c) -> assert failed (covers r clip) >> assertEq failed (pageBackground theme) c
    DamageFull -> assert failed False

-- | Two labels changing in opposite corners repaint as two pieces, and each
-- draw command lies inside one of them.
runFarLabelsDamagePiecesTest :: Context -> IORef Int -> IO ()
runFarLabelsDamagePiecesTest ctx failed = do
  let inp = withInput 800 600
      ui k = columnWith (fillW . fillH) $ do
        label (T.pack (show (k :: Int)))
        flex
        rowWith fillW $ flex >> label (T.pack (show k))
  _ <- warmup2 ctx inp (ui 1)
  writeIORef (ctxPaintFull ctx) False
  (_, dd, _) <- runFrame ctx inp (ui 2)
  assert failed . (/= DamageFull) =<< takeDamage ctx
  pieces <- takeDamagePieces ctx
  assertEq failed 2 (length pieces)
  let clipOf c = Rect (cmdClipX c) (cmdClipY c) (cmdClipW c) (cmdClipH c)
      inPiece c = any ((`covers` clipOf c) . rectInflate 1) pieces
  assert failed (not (null (drawCmdElems dd)) && all inPiece (drawCmdElems dd))

-- | A node whose size changes repaints its old and new rects: a panel
-- (painted, with no widget id) with the pointer on its button, whose hover
-- fade keeps the frame clipped, or away; a scroller, well and bar lane
-- included, not just the viewport clipping its hovered content; a root
-- drawing, the strip it vacated rather than its old rect clipped to its new
-- one; and a hovered button. A drawing overflowing a short root repaints all
-- it drew (the root clips to the window, not its rect). A panel that stays
-- put adds nothing.
runResizeDamageTest :: Context -> IORef Int -> IO ()
runResizeDamageTest _ failed = do
  let at w h x y = (withInput w h) {inputMousePos = V2 x y}
      boxed w = panelWith (fixedWH w 40) (void (buttonWith (fixedWH 30 20) ""))
      scroller w = column (void (scrollWith (fixedWH w 60) (replicateM_ 6 (buttonWith (fixedWH 30 20) ""))))
      hoveredButton w = column (void (buttonWith (fixedWH w 20) ""))
      fresh inp ui = newContext >>= \ctx -> ctx <$ warmup2 ctx inp ui
  forM_
    [ (at 300 200 10 10, boxed, \w -> Rect 0 0 w 40, 100, 60)
    , (at 300 200 (-10) (-10), boxed, \w -> Rect 0 0 w 40, 100, 60)
    , (at 300 200 (-10) (-10), boxed, \w -> Rect 0 0 w 40, 60, 100)
    , (at 300 200 10 10, scroller, \w -> Rect 3 3 w 60, 100, 60)
    , (at 300 200 10 10, scroller, \w -> Rect 3 3 w 60, 60, 100)
    , (withInput 200 100, (`redDrawing` 20), \w -> Rect 0 0 w 20, 40, 20)
    , (at 200 100 5 5, hoveredButton, \w -> Rect 3 3 w 20, 40, 20)
    ]
    $ \(inp, ui, rect, from, to) -> do
      ctx <- fresh inp (ui from)
      assert failed . elem (rect from) =<< arenaRects ctx
      _ <- runFrame ctx inp (ui to)
      assert failed . elem (rect to) =<< arenaRects ctx
      assert failed . (`damageCovers` rect (max from to)) =<< takeDamage ctx
  let overflow rootH drawH = columnWith (fixedWH 40 rootH . padAll 0) (redDrawing 40 drawH)
  forM_ [(40, 20), (20, 30)] $ \(h, dh) -> do
    ctx <- fresh (at 200 100 1 1) (overflow 20 40)
    _ <- runFrame ctx (at 200 100 1 1) (overflow h dh)
    assert failed . (`damageCovers` Rect 0 0 40 40) =<< takeDamage ctx
  let steady = column (boxed 100 >> panelWith (fixedWH 80 40) (label "second"))
  ctx <- fresh (withInputOff 300 200) steady
  _ <- runFrame ctx (withInputOff 300 200) steady
  assertEq failed (DamageClip (Rect 0 0 0 0)) =<< takeDamage ctx

-- | An empty label and a button take turns at one key and rect. Each time the
-- label comes back its rect repaints: the text entry it had left with it.
runLabelButtonSwapDamageTest :: Context -> IORef Int -> IO ()
runLabelButtonSwapDamageTest ctx failed = do
  let inp = withInputOff 200 100
      ui asButton = column (if asButton then void (buttonWith (fixedWH 60 20) "") else labelWith (fixedWH 60 20) "")
  _ <- warmup2 ctx inp (ui False)
  forM_ [True, False, True, False] $ \asButton -> do
    _ <- runFrame ctx inp (ui asButton)
    dmg <- takeDamage ctx
    unless asButton $ assert failed (dmg `damageCovers` Rect 3 3 60 20)

-- | The previous-frame maps each frame updates in place equal the ones a walk
-- from empty maps builds, through label and button swaps, text changes, plain
-- and configured images and image switches, a collapsing widget, shared keys,
-- a moving panel, a resizing viewport, keyed rows reordered and removed, and
-- trailing nodes that come and go. Each widget's rect read by its node index
-- ('getPrevRectAt') is its map entry.
runPrevFrameFreshWalkTest :: Context -> IORef Int -> IO ()
runPrevFrameFreshWalkTest ctx failed = do
  ref <- newContext
  let img = ImageId 1
      img2 = ImageId 2
      inp = withInputOff 400 400
      ui :: Int -> NanoUI ()
      ui f = columnWith (fillW . fillH) $ do
        scope (if even f then labelWith (fixedWH 60 20) "" else void (buttonWith (fixedWH 60 20) ""))
        label (if f `mod` 3 == 0 then "" else "x")
        labelWith (fixedWH 40 20) (T.pack (show (f `mod` 4)))
        image (fixedWH 16 16) (if even (f `div` 2) then img else img2)
        scope $
          if f `mod` 4 < 2
            then image (fixedWH 32 32) img
            else imageConfigured defaultImageConfig {icLayout = fixedWH 32 32, icOpacity = if f `mod` 4 == 2 then 0.5 else 1} img
        void (buttonWith (if f `mod` 5 == 0 then fixedWH 0 0 else fixedWH 40 20) "z")
        rowWith tight $ forM_ [1 :: Int, 1, 2] $ \k -> withKey k (void (buttonWith (fixedWH (if odd (f `div` 3) then 30 else 50) 20) "d"))
        rowWith tight $ do
          when (f `mod` 6 < 3) (void (buttonWith (fixedWH 20 20) "p"))
          panelWith (fixedWH 80 40) (label "panel")
        void $ scrollWith (fixedWH 200 (if even (f `div` 3) then 100 else 80)) $ column $
          forM_ (if f `mod` 7 < 3 then [1 .. 6 :: Int] else reverse [2 .. 5]) $ \k -> withKey k (void (button (T.pack (show k))))
        when (f `mod` 8 < 4) $ forM_ [1 .. 3 :: Int] $ \k -> label (T.pack (show k))
      maps c = (\p -> (pfRects p, pfClips p, pfOuterClips p, pfTexts p, pfImages p)) <$> getsDamage c dsPrev
  forM_ [ctx, ref] $ \c -> forM_ [img, img2] $ \i -> assert failed =<< registerImage c i 8 8 (BS.replicate (8 * 8 * 4) 200)
  forM_ [0 .. 47] $ \f -> do
    _ <- runFrame ctx inp (ui f)
    kept <- maps ctx
    modifyIORef' (ctxDamageState ref) (\ds -> ds {dsPrev = emptyPrevFrame})
    _ <- runFrame ref inp (ui f)
    assertEq failed kept =<< maps ref
    n <- arenaCount (ctxNodeArena ctx)
    forM_ [0 .. n - 1] $ \i -> do
      wid <- getWidgetId (ctxNodeArena ctx) i
      inMap <- fromMaybe (Rect 0 0 0 0) <$> getPrevRect ctx wid
      assertEq failed inMap =<< getPrevRectAt ctx i wid

-- | A popup that follows a row of a scroller moves on a frame that only
-- scrolls. That frame repaints the popup's old shadow, which lies past its
-- rect, and only a clip, also with a scroller nested in the page, whose
-- viewport moves with it.
runScrolledPopupDamageTest :: Context -> IORef Int -> IO ()
runScrolledPopupDamageTest _ failed = forM_ [False, True] $ \nested -> do
  ctx <- newContext
  let inp = withInputOff 640 400
      rows = mapM_ (\i -> label (T.pack ("row " <> show (i :: Int))))
      ui = do
        (sc, anchor) <- scrollArea (fixedWH 200 200) . columnWith fillW $ do
          if nested then void (scrollArea (fillW . fixedH 40) (rows [1 .. 6])) else rows [1 .. 4]
          labelWith' fillW "anchor" <* rows [(if nested then 7 else 5) .. 30]
        popup (constVar True) (defaultPopupConfig (AnchorRect (respRect anchor))) {cfgPlacement = PlacementRight, cfgDismissable = False} (label "tip")
        pure sc
  sc <- warmup2 ctx inp ui
  assertJustM failed (getPrevRect ctx sc) $ \r -> do
    -- Below the nested scroller, over the page's own rows.
    let wheel = inp {inputMousePos = V2 (rectX r + 20) (rectY r + 120), inputScroll = V2 0 1}
    _ <- runFrame ctx wheel ui
    before <- IM.elems <$> floatingPanelRects ctx
    _ <- runFrame ctx wheel ui
    after <- IM.elems <$> floatingPanelRects ctx
    dmg <- takeDamage ctx
    pieces <- takeDamagePieces ctx
    assert failed (before /= after)
    forM_ before $ \(Rect x y w h) -> do
      let shadow = Rect (x + 3) (y + 3) w h
      assert failed (clipCovers dmg shadow && (null pieces || any (`covers` shadow) pieces))

-- | A clip frame with nothing to repaint paints nothing, not even what lies
-- at the window's corner, such as a page scroller's backdrop.
runEmptyDamagePaintTest :: Context -> IORef Int -> IO ()
runEmptyDamagePaintTest ctx failed = do
  let inp = withInputOff 300 200
      ui = scrollArea grow (column (label "Static" >> button "B"))
  _ <- warmup2 ctx inp ui
  writeIORef (ctxPaintFull ctx) False
  (_, dd, _) <- runFrame ctx inp ui
  assert failed . damageIsEmpty =<< takeDamage ctx
  assert failed . null =<< drawQuads dd

redDrawing :: Float -> Float -> NanoUI ()
redDrawing w h = void (drawing (fixedWH w h) (\r -> smallArrayFromList [FillRect r (colorRGBA 255 0 0 255)]))
