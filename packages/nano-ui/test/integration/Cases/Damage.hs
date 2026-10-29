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
  , spec "clip-frame-paint" runClipFramePaintTest
  , spec "resize-damage" runResizeDamageTest
  , spec "label-button-swap-damage" runLabelButtonSwapDamageTest
  , spec "prev-frame-matches-fresh-walk" runPrevFrameFreshWalkTest
  , spec "scrolled-popup-damage" runScrolledPopupDamageTest
  ]

-- | Whether a frame's damage is a clip smaller in area than @a@.
clipUnder :: Float -> Damage -> Bool
clipUnder a (DamageClip (Rect _ _ w h)) = w * h < a
clipUnder _ DamageFull = False

-- | Damage requests resolve against a rect ('DamageNone' is a union's
-- identity), and rects group into at most four disjoint covering pieces.
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

-- | Asked-for damage lasts its own frame, and damage off the window adds
-- nothing. 'clearDirty' keeps the repaint 'markDirty' queued; a host not
-- reading damage gets 'DamageFull' yet keeps what the next frame diffs.
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
    DamageClip (Rect x y w h) -> assert failed (all ((< 0.05) . abs) [x - ex, y - ey, w - ew, h - eh])
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

-- | A Var write that changes a box's colour, alone or beside a same-length
-- label, repaints whole: no diff sees the colour, and the text diff would
-- clip to the label.
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

-- | Hover enter and press repaint only the button; motion inside it asks for
-- no frame.
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

-- | A perpetual animation repaints a clip over its widget and nothing once
-- the widget is gone; on a widget never laid out, its first frame is whole.
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

-- | Changes no rect or text diff sees repaint just the widget: a drawing's
-- new version, select all in a text area (on its own frame), a label shrunk
-- by a slider drag (only 'storeFloat' changes), an image switched in a page.
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
  imgCtx <- newContext
  let px a = BS.pack (concat (replicate 16 [a, 0, 0, 255]))
  assert failed . and =<< mapM (\(i, a) -> registerImage imgCtx (ImageId i) 4 4 (px a)) [(1, 60), (2, 120)]
  frameRef <- newIORef (ImageId 1)
  let page = fmap snd . scrollArea (padAll 10 . grow) . column $ do
        label "frames"
        currentId <* (image (fixedWH 40 24) =<< liftIO (readIORef frameRef))
  wid <- warmup2 imgCtx (withInputOff 320 200) page
  writeIORef frameRef (ImageId 2)
  _ <- runFrame imgCtx (withInputOff 320 200) page
  dmg <- takeDamage imgCtx
  assertJustM failed (getPrevRect imgCtx wid) $ \r ->
    assert failed (clipCovers dmg r && clipUnder (320 * 200 / 4) dmg)

-- | A clip frame paints its region over the page backdrop (or an unfilled
-- menu title keeps its lost hover), far changes as two pieces, and with
-- nothing to repaint nothing, not even a page scroller's backdrop.
runClipFramePaintTest :: Context -> IORef Int -> IO ()
runClipFramePaintTest _ failed = do
  let clipped w h ui = do
        ctx <- newContext
        _ <- warmup2 ctx (withInputOff w h) ui
        ctx <$ writeIORef (ctxPaintFull ctx) False
  let menu = rowWith (tight . fillW . fixedH 28) (menuButton' "File" False <* menuButton' "Edit" False)
  ctx <- clipped 400 300 menu
  file <- evalUi ctx (withInputOff 400 300) menu
  _ <- runFrame ctx (withInputOff 400 300) {inputMousePos = centerOf file} menu
  (_, draw, _) <- runFrame ctx (withInputOff 400 300) menu
  theme <- readIORef (ctxTheme ctx)
  quads <- drawQuads draw
  takeDamage ctx >>= \case
    DamageClip clip -> do
      assert failed (covers clip (respRect file))
      assertJust failed (listToMaybe quads) $ \(r, c) ->
        assert failed (covers r clip) >> assertEq failed (pageBackground theme) c
    DamageFull -> assert failed False
  let corners k = columnWith (fillW . fillH) $ do
        label (T.pack (show (k :: Int)))
        flex
        rowWith fillW $ flex >> label (T.pack (show k))
  far <- clipped 800 600 (corners 1)
  (_, dd, _) <- runFrame far (withInputOff 800 600) (corners 2)
  assert failed . (/= DamageFull) =<< takeDamage far
  pieces <- takeDamagePieces far
  assertEq failed 2 (length pieces)
  let clipOf c = Rect (cmdClipX c) (cmdClipY c) (cmdClipW c) (cmdClipH c)
      inPiece c = any ((`covers` clipOf c) . rectInflate 1) pieces
  assert failed (not (null (drawCmdElems dd)) && all inPiece (drawCmdElems dd))
  let page = scrollArea grow (column (label "Static" >> button "B"))
  idle <- clipped 300 200 page
  (_, idleDraw, _) <- runFrame idle (withInputOff 300 200) page
  assert failed . damageIsEmpty =<< takeDamage idle
  assert failed . null =<< drawQuads idleDraw

-- | A resized panel (no widget id), scroller (bar lane too), root drawing or
-- hovered button repaints its old and new rects; a drawing overflowing a
-- short root all it drew. A panel that stays put adds nothing.
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

-- | An empty label taking back a button's key and rect repaints the rect.
runLabelButtonSwapDamageTest :: Context -> IORef Int -> IO ()
runLabelButtonSwapDamageTest ctx failed = do
  let inp = withInputOff 200 100
      ui asButton = column (if asButton then void (buttonWith (fixedWH 60 20) "") else labelWith (fixedWH 60 20) "")
  _ <- warmup2 ctx inp (ui False)
  forM_ [True, False, True, False] $ \asButton -> do
    _ <- runFrame ctx inp (ui asButton)
    dmg <- takeDamage ctx
    unless asButton $ assert failed (dmg `damageCovers` Rect 3 3 60 20)

-- | The previous-frame maps updated in place equal a walk from empty maps
-- through 48 frames of swaps, images, shared keys and reorders, and each
-- widget's rect by node index ('getPrevRectAt') is its map entry.
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

-- | A popup following a scrolled row repaints its old shadow, past its rect,
-- as a clip, also with a scroller nested in the page.
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

redDrawing :: Float -> Float -> NanoUI ()
redDrawing w h = void (drawing (fixedWH w h) (\r -> smallArrayFromList [FillRect r (colorRGBA 255 0 0 255)]))
